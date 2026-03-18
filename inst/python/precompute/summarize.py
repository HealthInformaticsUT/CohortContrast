"""
Pre-computation module for generating summary data without patient-level information.

Optimized version with:
- Vectorized operations (no row-by-row iteration)
- Pre-computed shared data structures
- Comprehensive logging
- Support for concept_limit=None (use all concepts)
"""

import json
import logging
import math
import time
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import warnings

import numpy as np
import pandas as pd
from scipy import stats

from clustering.clustering import build_patient_feature_matrix, cluster_patient_features

logger = logging.getLogger(__name__)

PARQUET_ENGINE = 'pyarrow'
DEFAULT_MIN_CELL_COUNT = 0
DEFAULT_CLUSTER_FEATURE_MATRIX_CELL_THRESHOLD = 50000000
FEATURES_PER_CLUSTER_CONCEPT = 3
DEFAULT_PAIRWISE_OVERLAP_MAX_CONCEPTS = 500


def setup_logging(verbose: bool = True) -> None:
    """Configure logging for the precompute module."""
    level = logging.INFO if verbose else logging.WARNING
    logging.basicConfig(
        level=level,
        format='%(asctime)s [%(levelname)s] %(message)s',
        datefmt='%H:%M:%S'
    )


class Timer:
    """Context manager for timing code blocks."""
    def __init__(self, description: str):
        self.description = description
        self.start_time = None
        self.elapsed = 0
    
    def __enter__(self):
        self.start_time = time.time()
        logger.info(f"Starting: {self.description}")
        return self
    
    def __exit__(self, *args):
        self.elapsed = time.time() - self.start_time
        logger.info(f"Completed: {self.description} ({self.elapsed:.2f}s)")


def apply_small_cell_suppression(count: int, min_cell_count: int = DEFAULT_MIN_CELL_COUNT) -> int:
    """Apply small cell suppression rule for privacy protection."""
    if min_cell_count <= 1:
        return count
    if count <= 0:
        return 0
    elif count < min_cell_count:
        return min_cell_count
    else:
        return count


def apply_suppression_to_series(series: pd.Series, min_cell_count: int) -> pd.Series:
    """Vectorized small cell suppression for a pandas Series."""
    if min_cell_count <= 1:
        return series
    result = series.copy()
    mask = (result > 0) & (result < min_cell_count)
    result.loc[mask] = min_cell_count
    return result


def deserialize_json_column_vectorized(series: pd.Series) -> pd.Series:
    """Vectorized deserialization of JSON string column to numpy arrays."""
    if series.empty:
        return series
    
    sample = series.dropna().head(5)
    if sample.empty:
        return series
    
    first_val = sample.iloc[0]
    if not isinstance(first_val, str) or not first_val.startswith('['):
        return series
    
    logger.debug(f"Deserializing JSON column with {len(series)} values")
    
    def parse_json_fast(val):
        if pd.isna(val) or val is None:
            return np.array([], dtype=float)
        if isinstance(val, str):
            if len(val) == 0 or val == '[]':
                return np.array([], dtype=float)
            try:
                return np.array(json.loads(val), dtype=float)
            except (json.JSONDecodeError, TypeError, ValueError):
                return np.array([], dtype=float)
        if isinstance(val, (list, np.ndarray)):
            return np.array(val, dtype=float)
        return np.array([val], dtype=float) if not pd.isna(val) else np.array([], dtype=float)
    
    return series.map(parse_json_fast)


def deserialize_json_columns(df: pd.DataFrame, columns: List[str] = None) -> pd.DataFrame:
    """Deserialize JSON string columns back to Python lists/arrays."""
    if df.empty:
        return df
    
    JSON_LIST_COLUMNS = ['TIME_TO_EVENT']
    if columns is None:
        columns = JSON_LIST_COLUMNS
    
    df = df.copy()
    for col in columns:
        if col in df.columns:
            df[col] = deserialize_json_column_vectorized(df[col])
            logger.debug(f"Deserialized column: {col}")
    
    return df


class PreprocessedData:
    """Container for pre-computed data structures used across multiple functions."""
    
    def __init__(self, df_target, df_exploded, person_with_ages, total_target_patients, concept_name_lookup, feature_lookup, total_control_patients=0):
        self.df_target = df_target
        self.df_exploded = df_exploded
        self.person_with_ages = person_with_ages
        self.total_target_patients = total_target_patients
        self.concept_name_lookup = concept_name_lookup
        self.feature_lookup = feature_lookup
        self.total_control_patients = total_control_patients


def preprocess_data(data_patients, data_features, data_person, data_initial) -> PreprocessedData:
    """Pre-compute all shared data structures once."""
    with Timer("Pre-processing data"):
        logger.info("Filtering target cohort...")
        df_target = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
        total_target_patients = df_target["PERSON_ID"].nunique()
        logger.info(f"Target cohort: {total_target_patients:,} patients, {len(df_target):,} rows")
        
        logger.info("Exploding TIME_TO_EVENT arrays...")
        df_exploded = df_target.explode("TIME_TO_EVENT")
        df_exploded["TIME_TO_EVENT"] = pd.to_numeric(df_exploded["TIME_TO_EVENT"], errors='coerce')
        df_exploded = df_exploded.dropna(subset=["TIME_TO_EVENT"])
        
        # Filter to only concepts that exist in data_features
        if not data_features.empty and "CONCEPT_ID" in data_features.columns:
            valid_concepts = set(data_features["CONCEPT_ID"].unique())  # Already string from loading
            # Convert and clean CONCEPT_ID (remove trailing .0 from float conversion)
            df_exploded["CONCEPT_ID"] = df_exploded["CONCEPT_ID"].astype(str).str.replace(r'\.0$', '', regex=True)
            original_count = len(df_exploded)
            df_exploded = df_exploded[df_exploded["CONCEPT_ID"].isin(valid_concepts)]
            logger.info(f"Filtered to {len(valid_concepts):,} concepts from data_features ({original_count:,} -> {len(df_exploded):,} events)")
        
        logger.info(f"Exploded data: {len(df_exploded):,} individual events")
        
        logger.info("Computing patient ages...")
        person_with_ages = pd.DataFrame()
        if data_person is not None and not data_person.empty:
            target_person_ids = df_target["PERSON_ID"].unique()
            person_with_ages = data_person[data_person["PERSON_ID"].isin(target_person_ids)].copy()
            
            if "YEAR_OF_BIRTH" in person_with_ages.columns and data_initial is not None and not data_initial.empty:
                target_initial = data_initial[data_initial["COHORT_DEFINITION_ID"] == "target"].copy()
                target_initial["COHORT_START_DATE"] = pd.to_datetime(target_initial["COHORT_START_DATE"])
                
                person_with_ages = person_with_ages.merge(
                    target_initial[["SUBJECT_ID", "COHORT_START_DATE"]],
                    left_on="PERSON_ID", right_on="SUBJECT_ID", how="inner"
                )
                
                if not person_with_ages.empty:
                    person_with_ages["AGE"] = person_with_ages["COHORT_START_DATE"].dt.year - person_with_ages["YEAR_OF_BIRTH"]
                    person_with_ages = person_with_ages[(person_with_ages["AGE"] >= 0) & (person_with_ages["AGE"] <= 150)]
            
            logger.info(f"Computed ages for {len(person_with_ages):,} patients")
        
        logger.info("Building concept lookups...")
        concept_name_lookup = {}
        if "CONCEPT_NAME" in data_features.columns:
            concept_name_lookup = dict(zip(data_features["CONCEPT_ID"].astype(str), data_features["CONCEPT_NAME"]))
        
        # Get CONCEPT_NAME and counts from data_features
        # Counts will be used for enrichment calculation with suppression applied
        # Note: HERITAGE comes from the data itself via groupby, not from feature_lookup
        feature_cols = ["CONCEPT_ID", "CONCEPT_NAME", "TARGET_SUBJECT_COUNT", "CONTROL_SUBJECT_COUNT"]
        feature_cols = [c for c in feature_cols if c in data_features.columns]
        feature_lookup = data_features[feature_cols].copy() if feature_cols else pd.DataFrame()
        if not feature_lookup.empty and "CONCEPT_ID" in feature_lookup.columns:
            feature_lookup["CONCEPT_ID"] = feature_lookup["CONCEPT_ID"].astype(str)
            # Deduplicate to one row per CONCEPT_ID (data_features may have multiple per HERITAGE)
            feature_lookup = feature_lookup.drop_duplicates(subset=["CONCEPT_ID"], keep="first")
        
        # Get total target and control patients from data_initial (consistent with R code)
        # data_initial has unique patients per cohort, so we count SUBJECT_ID
        if data_initial is not None and not data_initial.empty:
            # Convert COHORT_DEFINITION_ID to string for consistent comparison
            data_initial_copy = data_initial.copy()
            data_initial_copy["COHORT_DEFINITION_ID"] = data_initial_copy["COHORT_DEFINITION_ID"].astype(str)
            
            # Count unique subjects per cohort
            total_target_from_initial = data_initial_copy[
                data_initial_copy["COHORT_DEFINITION_ID"].isin(["target", "2"])
            ]["SUBJECT_ID"].nunique()
            total_control_patients = data_initial_copy[
                data_initial_copy["COHORT_DEFINITION_ID"].isin(["control", "1"])
            ]["SUBJECT_ID"].nunique()
            
            # Use data_initial count if available, otherwise fall back to data_patients count
            if total_target_from_initial > 0:
                total_target_patients = total_target_from_initial
            
            logger.info(f"From data_initial - Target: {total_target_patients:,}, Control: {total_control_patients:,} patients")
        else:
            # Fallback to data_patients if data_initial not available
            total_control_patients = data_patients[data_patients["COHORT_DEFINITION_ID"] == "control"]["PERSON_ID"].nunique()
            logger.info(f"Control cohort (from data_patients): {total_control_patients:,} patients")
        
        return PreprocessedData(df_target, df_exploded, person_with_ages, total_target_patients, concept_name_lookup, feature_lookup, total_control_patients)


def compute_time_stats_vectorized(times: np.ndarray) -> Dict:
    """Compute time distribution statistics."""
    if len(times) == 0:
        return {"count": 0, "min": None, "max": None, "mean": None, "median": None, "std": None,
                "q1": None, "q3": None, "iqr": None, "histogram_bins": [], "histogram_counts": [], "kde_x": [], "kde_y": []}
    
    times = times[~np.isnan(times)]
    if len(times) == 0:
        return {"count": 0, "min": None, "max": None, "mean": None, "median": None, "std": None,
                "q1": None, "q3": None, "iqr": None, "histogram_bins": [], "histogram_counts": [], "kde_x": [], "kde_y": []}
    
    q1, median, q3 = np.percentile(times, [25, 50, 75])
    result = {
        "count": int(len(times)), "min": float(np.min(times)), "max": float(np.max(times)),
        "mean": float(np.mean(times)), "median": float(median), "std": float(np.std(times)),
        "q1": float(q1), "q3": float(q3), "iqr": float(q3 - q1),
    }
    
    try:
        hist_counts, hist_bins = np.histogram(times, bins=50)
        result["histogram_bins"] = hist_bins.tolist()
        result["histogram_counts"] = hist_counts.tolist()
    except Exception:
        result["histogram_bins"] = []
        result["histogram_counts"] = []
    
    try:
        if len(times) > 3 and np.std(times) > 0:
            kde = stats.gaussian_kde(times)
            x_range = np.linspace(np.min(times), np.max(times), 100)
            result["kde_x"] = x_range.tolist()
            result["kde_y"] = kde(x_range).tolist()
        else:
            result["kde_x"] = []
            result["kde_y"] = []
    except Exception:
        result["kde_x"] = []
        result["kde_y"] = []
    
    return result


def compute_concept_summaries_vectorized(preprocessed: PreprocessedData, data_features: pd.DataFrame, min_cell_count: int = DEFAULT_MIN_CELL_COUNT) -> pd.DataFrame:
    """Compute per-concept summary statistics using fully vectorized operations."""
    with Timer("Computing concept summaries (vectorized)"):
        df_exploded = preprocessed.df_exploded
        person_with_ages = preprocessed.person_with_ages
        
        if df_exploded.empty:
            return pd.DataFrame()
    
        has_heritage = "HERITAGE" in df_exploded.columns and df_exploded["HERITAGE"].notna().any()
        group_cols = ["CONCEPT_ID", "HERITAGE"] if has_heritage else ["CONCEPT_ID"]
        
        logger.info(f"Computing summaries for {df_exploded['CONCEPT_ID'].nunique():,} concepts...")
        
        time_agg = df_exploded.groupby(group_cols)["TIME_TO_EVENT"].agg([
            ('time_count', 'count'), ('time_min', 'min'), ('time_max', 'max'),
            ('time_mean', 'mean'), ('time_median', 'median'), ('time_std', 'std'),
        ]).reset_index()
        
        quartiles = df_exploded.groupby(group_cols)["TIME_TO_EVENT"].quantile([0.25, 0.75]).unstack()
        quartiles.columns = ['time_q1', 'time_q3']
        quartiles = quartiles.reset_index()
        
        time_agg = time_agg.merge(quartiles, on=group_cols, how='left')
        time_agg['time_iqr'] = time_agg['time_q3'] - time_agg['time_q1']
        
        patient_counts = df_exploded.groupby(group_cols)["PERSON_ID"].nunique().reset_index(name='patient_count')
        summaries = time_agg.merge(patient_counts, on=group_cols, how='outer')
        
        if not preprocessed.feature_lookup.empty:
            summaries = summaries.merge(preprocessed.feature_lookup, on="CONCEPT_ID", how="left")
        
        logger.info("Computing histograms and KDE distributions...")
        histogram_data = []
        for group_key, group_df in df_exploded.groupby(group_cols):
            times = group_df["TIME_TO_EVENT"].values
            time_stats = compute_time_stats_vectorized(times)
            row_data = {col: val for col, val in zip(group_cols, [group_key] if len(group_cols) == 1 else group_key)}
            row_data["time_histogram_bins"] = json.dumps(time_stats["histogram_bins"])
            row_data["time_histogram_counts"] = json.dumps([apply_small_cell_suppression(int(c), min_cell_count) for c in time_stats["histogram_counts"]])
            row_data["time_kde_x"] = json.dumps(time_stats["kde_x"])
            row_data["time_kde_y"] = json.dumps(time_stats["kde_y"])
            histogram_data.append(row_data)
        
        histogram_df = pd.DataFrame(histogram_data)
        summaries = summaries.merge(histogram_df, on=group_cols, how='left')
        
        if not person_with_ages.empty and "AGE" in person_with_ages.columns:
            logger.info("Computing demographics per concept...")
            patient_ids_per_concept = df_exploded.groupby(group_cols)["PERSON_ID"].apply(set).reset_index()
            patient_ids_per_concept.columns = list(group_cols) + ["patient_ids"]
            
            age_stats_list = []
            for _, row in patient_ids_per_concept.iterrows():
                patient_ids = row["patient_ids"]
                patient_demo = person_with_ages[person_with_ages["PERSON_ID"].isin(patient_ids)]
                age_data = {col: row[col] for col in group_cols}
                
                if not patient_demo.empty and "AGE" in patient_demo.columns:
                    ages = patient_demo["AGE"].dropna()
                    if len(ages) > 0:
                        age_data["age_mean"] = float(ages.mean())
                        age_data["age_median"] = float(ages.median())
                        age_data["age_std"] = float(ages.std()) if len(ages) > 1 else 0.0
                        age_data["age_q1"] = float(ages.quantile(0.25))
                        age_data["age_q3"] = float(ages.quantile(0.75))
                        age_data["n_ages"] = apply_small_cell_suppression(len(ages), min_cell_count)
                
                if not patient_demo.empty and "GENDER_CONCEPT_ID" in patient_demo.columns:
                    male_count = (patient_demo["GENDER_CONCEPT_ID"] == 8507).sum()
                    if len(patient_demo) > 0:
                        age_data["male_proportion"] = float(male_count / len(patient_demo))
                
                age_stats_list.append(age_data)
            
            age_stats_df = pd.DataFrame(age_stats_list)
            summaries = summaries.merge(age_stats_df, on=group_cols, how='left')

        
        # Apply small cell suppression to all count columns FIRST
        # IMPORTANT: Suppression must happen BEFORE calculating prevalences to ensure privacy protection
        summaries["patient_count"] = apply_suppression_to_series(summaries["patient_count"], min_cell_count)
        summaries["time_count"] = apply_suppression_to_series(summaries["time_count"], min_cell_count)
        
        # Apply suppression to target/control counts BEFORE using them for prevalence calculations
        # This ensures any counts in the suppression interval are raised to min_cell_count
        if "TARGET_SUBJECT_COUNT" in summaries.columns:
            summaries["TARGET_SUBJECT_COUNT"] = apply_suppression_to_series(summaries["TARGET_SUBJECT_COUNT"], min_cell_count)
        if "CONTROL_SUBJECT_COUNT" in summaries.columns:
            summaries["CONTROL_SUBJECT_COUNT"] = apply_suppression_to_series(summaries["CONTROL_SUBJECT_COUNT"], min_cell_count)
        
        # Calculate prevalences AFTER suppression using suppressed counts
        # The suppressed counts are used here to ensure privacy-protected prevalences
        total_target_patients = preprocessed.total_target_patients
        total_control_patients = preprocessed.total_control_patients
        
        # Calculate target prevalence from suppressed TARGET_SUBJECT_COUNT
        if "TARGET_SUBJECT_COUNT" in summaries.columns and total_target_patients > 0:
            summaries["TARGET_SUBJECT_PREVALENCE"] = summaries["TARGET_SUBJECT_COUNT"] / total_target_patients
        elif total_target_patients > 0:
            # Fallback to patient_count if TARGET_SUBJECT_COUNT not available
            summaries["TARGET_SUBJECT_PREVALENCE"] = summaries["patient_count"] / total_target_patients
        else:
            summaries["TARGET_SUBJECT_PREVALENCE"] = 0.0
        
        # Calculate control prevalence from suppressed CONTROL_SUBJECT_COUNT
        if "CONTROL_SUBJECT_COUNT" in summaries.columns and total_control_patients > 0:
            summaries["CONTROL_SUBJECT_PREVALENCE"] = summaries["CONTROL_SUBJECT_COUNT"] / total_control_patients
        else:
            summaries["CONTROL_SUBJECT_PREVALENCE"] = 0.0
        
        # Calculate enrichment (PREVALENCE_DIFFERENCE_RATIO) = TARGET / CONTROL prevalence
        # Handle edge cases: 0 target → 0, 0 control → 100 (capped enrichment)
        def calc_enrichment(row):
            target_prev = row.get("TARGET_SUBJECT_PREVALENCE", 0)
            control_prev = row.get("CONTROL_SUBJECT_PREVALENCE", 0)
            if target_prev == 0:
                return 0.0
            elif control_prev == 0:
                return 100.0  # Capped enrichment when control is 0
            else:
                return target_prev / control_prev
        
        summaries["PREVALENCE_DIFFERENCE_RATIO"] = summaries.apply(calc_enrichment, axis=1)
        
        # Drop TARGET_SUBJECT_COUNT and CONTROL_SUBJECT_COUNT - they were only used for intermediate calculations
        # The prevalences are what we need, not the raw counts
        summaries = summaries.drop(columns=["TARGET_SUBJECT_COUNT", "CONTROL_SUBJECT_COUNT"], errors="ignore")
        
        logger.info(f"Generated {len(summaries):,} concept summaries")
        return summaries


def compute_ordinal_summaries_vectorized(preprocessed: PreprocessedData, data_features: pd.DataFrame, data_patients: pd.DataFrame, min_cell_count: int = DEFAULT_MIN_CELL_COUNT) -> pd.DataFrame:
    """Compute ordinal concept summaries (1st, 2nd, 3rd occurrence) using vectorized operations."""
    with Timer("Computing ordinal summaries (vectorized)"):
        df_exploded = preprocessed.df_exploded
        person_with_ages = preprocessed.person_with_ages
        total_target_patients = preprocessed.total_target_patients
        
        if df_exploded.empty:
            return pd.DataFrame()
    
        has_heritage = "HERITAGE" in df_exploded.columns and df_exploded["HERITAGE"].notna().any()
        group_cols = ["CONCEPT_ID", "HERITAGE"] if has_heritage else ["CONCEPT_ID"]
        
        logger.info("Assigning ordinals to events...")
        df_sorted = df_exploded.sort_values(["PERSON_ID"] + group_cols + ["TIME_TO_EVENT"]).copy()
        df_sorted["_rank_key"] = df_sorted.groupby(["PERSON_ID"] + group_cols)["TIME_TO_EVENT"].rank(method='dense')
        df_sorted["ORDINAL"] = df_sorted["_rank_key"].astype(int)
        df_ordinals = df_sorted.drop_duplicates(subset=["PERSON_ID"] + group_cols + ["ORDINAL"])
        
        logger.info(f"Found {df_ordinals['ORDINAL'].max():.0f} max ordinal across all concepts")
        
        ordinal_counts = df_ordinals.groupby(group_cols + ["ORDINAL"])["PERSON_ID"].nunique().reset_index(name="count")
        ordinal_pivot = ordinal_counts.pivot_table(index=group_cols, columns="ORDINAL", values="count", fill_value=0)
        
        def calc_max_ordinal(row):
            max_ord = 0
            prev_count = row.get(1, 0)
            for ordinal in range(1, 11):
                if ordinal not in row or prev_count == 0:
                    break
                next_count = row.get(ordinal + 1, 0)
                if next_count >= prev_count * 0.5:
                    max_ord = ordinal
                    prev_count = next_count
                else:
                    break
            return max_ord
        
        max_ordinals = ordinal_pivot.apply(calc_max_ordinal, axis=1).reset_index(name="max_ordinal")
        
        df_ordinals = df_ordinals.merge(max_ordinals, on=group_cols, how='left')
        df_ordinals = df_ordinals[df_ordinals["ORDINAL"] <= df_ordinals["max_ordinal"]]
        df_ordinals = df_ordinals[df_ordinals["max_ordinal"] > 0]
        
        if df_ordinals.empty:
            logger.info("No valid ordinals found (50% threshold not met)")
            return pd.DataFrame()
        
        logger.info(f"Computing stats for {df_ordinals[group_cols + ['ORDINAL']].drop_duplicates().shape[0]:,} ordinal groups")
        
        ordinal_group_cols = group_cols + ["ORDINAL"]
        
        time_agg = df_ordinals.groupby(ordinal_group_cols)["TIME_TO_EVENT"].agg([
            ('time_count', 'count'), ('time_min', 'min'), ('time_max', 'max'),
            ('time_mean', 'mean'), ('time_median', 'median'), ('time_std', 'std'),
        ]).reset_index()
        
        quartiles = df_ordinals.groupby(ordinal_group_cols)["TIME_TO_EVENT"].quantile([0.25, 0.75]).unstack()
        quartiles.columns = ['time_q1', 'time_q3']
        quartiles = quartiles.reset_index()
        
        time_agg = time_agg.merge(quartiles, on=ordinal_group_cols, how='left')
        time_agg['time_iqr'] = time_agg['time_q3'] - time_agg['time_q1']
        
        patient_counts = df_ordinals.groupby(ordinal_group_cols)["PERSON_ID"].nunique().reset_index(name='patient_count')
        ordinal_summaries = time_agg.merge(patient_counts, on=ordinal_group_cols, how='outer')
        
        # NOTE: We do NOT merge TARGET_SUBJECT_COUNT and CONTROL_SUBJECT_COUNT from feature_lookup
        # because ordinal summaries use patient_count for prevalence calculation, not these columns.
        # These columns are not needed and would require suppression if included.
        
        # Add age and gender statistics per ordinal group
        if not person_with_ages.empty and "AGE" in person_with_ages.columns:
            logger.info("Computing demographics per ordinal group...")
            patient_ids_per_ordinal = df_ordinals.groupby(ordinal_group_cols)["PERSON_ID"].apply(set).reset_index()
            patient_ids_per_ordinal.columns = list(ordinal_group_cols) + ["patient_ids"]
            
            ordinal_demo_list = []
            for _, row in patient_ids_per_ordinal.iterrows():
                patient_ids = row["patient_ids"]
                patient_demo = person_with_ages[person_with_ages["PERSON_ID"].isin(patient_ids)]
                demo_data = {col: row[col] for col in ordinal_group_cols}
                
                if not patient_demo.empty and "AGE" in patient_demo.columns:
                    ages = patient_demo["AGE"].dropna()
                    if len(ages) > 0:
                        demo_data["age_mean"] = float(ages.mean())
                        demo_data["age_median"] = float(ages.median())
                        demo_data["age_std"] = float(ages.std()) if len(ages) > 1 else 0.0
                        demo_data["age_q1"] = float(ages.quantile(0.25))
                        demo_data["age_q3"] = float(ages.quantile(0.75))
                        demo_data["n_ages"] = apply_small_cell_suppression(len(ages), min_cell_count)
                
                if not patient_demo.empty and "GENDER_CONCEPT_ID" in patient_demo.columns:
                    male_count = (patient_demo["GENDER_CONCEPT_ID"] == 8507).sum()
                    if len(patient_demo) > 0:
                        demo_data["male_proportion"] = float(male_count / len(patient_demo))
                
                ordinal_demo_list.append(demo_data)
            
            if ordinal_demo_list:
                ordinal_demo_df = pd.DataFrame(ordinal_demo_list)
                ordinal_summaries = ordinal_summaries.merge(ordinal_demo_df, on=ordinal_group_cols, how='left')
        
        ordinal_suffix_map = {1: "st", 2: "nd", 3: "rd"}
        ordinal_summaries["ordinal_name_suffix"] = ordinal_summaries["ORDINAL"].apply(lambda x: f"{x}{ordinal_suffix_map.get(x, 'th')}")
        # Save ORIGINAL_CONCEPT_ID before modifying CONCEPT_ID (clean .0 suffix for consistency)
        ordinal_summaries["ORIGINAL_CONCEPT_ID"] = ordinal_summaries["CONCEPT_ID"].astype(str).str.replace(r'\.0$', '', regex=True)
        
        # Add CONCEPT_NAME from data_features if not already present
        if "CONCEPT_NAME" not in ordinal_summaries.columns:
            if "CONCEPT_NAME" in data_features.columns:
                name_lookup = data_features[["CONCEPT_ID", "CONCEPT_NAME"]].drop_duplicates()
                ordinal_summaries = ordinal_summaries.merge(name_lookup, on="CONCEPT_ID", how="left")
            if "CONCEPT_NAME" not in ordinal_summaries.columns:
                ordinal_summaries["CONCEPT_NAME"] = "Concept"
        
        # Create unique ordinal concept ID using string concatenation for robustness
        ordinal_summaries["CONCEPT_ID"] = ordinal_summaries["CONCEPT_ID"].astype(str).str.replace(r'\.0$', '', regex=True) + "_" + ordinal_summaries["ORDINAL"].astype(str)
        ordinal_summaries["CONCEPT_NAME"] = ordinal_summaries["CONCEPT_NAME"].fillna("Concept") + " " + ordinal_summaries["ordinal_name_suffix"]
        ordinal_summaries["IS_ORDINAL"] = True
        # NOTE: TARGET_SUBJECT_PREVALENCE calculated AFTER suppression below
        
        histogram_data = []
        for group_key, group_df in df_ordinals.groupby(ordinal_group_cols):
            times = group_df["TIME_TO_EVENT"].values
            time_stats = compute_time_stats_vectorized(times)
            row_data = {col: val for col, val in zip(ordinal_group_cols, group_key)}
            row_data["time_histogram_bins"] = json.dumps(time_stats["histogram_bins"])
            row_data["time_histogram_counts"] = json.dumps([apply_small_cell_suppression(int(c), min_cell_count) for c in time_stats["histogram_counts"]])
            row_data["time_kde_x"] = json.dumps(time_stats["kde_x"])
            row_data["time_kde_y"] = json.dumps(time_stats["kde_y"])
            histogram_data.append(row_data)
        
        histogram_df = pd.DataFrame(histogram_data)
        histogram_df["CONCEPT_ID"] = histogram_df["CONCEPT_ID"].astype(str).str.replace(r'\.0$', '', regex=True) + "_" + histogram_df["ORDINAL"].astype(str)
        ordinal_summaries = ordinal_summaries.merge(
            histogram_df[["CONCEPT_ID", "time_histogram_bins", "time_histogram_counts", "time_kde_x", "time_kde_y"]],
            on="CONCEPT_ID", how="left"
        )
        
        # Calculate target and control patient counts for each ordinal occurrence
        # This is needed to calculate PREVALENCE_DIFFERENCE_RATIO correctly
        logger.info("Calculating target and control ordinal occurrence counts...")
        
        # Save unsuppressed target counts before suppression
        ordinal_summaries["TARGET_SUBJECT_COUNT"] = ordinal_summaries["patient_count"]
        
        # Process control cohort to get ordinal occurrence counts
        # For control cohort, count ALL ordinals that exist (no 50% threshold applied)
        # The 50% threshold only applies to target cohort to determine which ordinals are statistically meaningful
        total_control_patients = preprocessed.total_control_patients
        
        # Initialize CONTROL_SUBJECT_COUNT to 0 for all rows
        ordinal_summaries["CONTROL_SUBJECT_COUNT"] = 0
        
        if not data_patients.empty and total_control_patients > 0:
            # Filter control cohort
            df_control = data_patients[data_patients["COHORT_DEFINITION_ID"] == "control"].copy()
            
            if not df_control.empty:
                # Filter to only concepts that exist in ordinal summaries (use ORIGINAL_CONCEPT_ID)
                valid_concepts = set(ordinal_summaries["ORIGINAL_CONCEPT_ID"].unique())
                df_control["CONCEPT_ID"] = df_control["CONCEPT_ID"].astype(str).str.replace(r'\.0$', '', regex=True)
                df_control = df_control[df_control["CONCEPT_ID"].isin(valid_concepts)]
                
                if not df_control.empty:
                    # Explode TIME_TO_EVENT for control cohort
                    df_control_exploded = df_control.explode("TIME_TO_EVENT")
                    df_control_exploded["TIME_TO_EVENT"] = pd.to_numeric(df_control_exploded["TIME_TO_EVENT"], errors='coerce')
                    df_control_exploded = df_control_exploded.dropna(subset=["TIME_TO_EVENT"])
                    
                    if not df_control_exploded.empty:
                        logger.info("Processing control cohort ordinals (counting all ordinals, no 50% threshold)...")
                        # Assign ordinals to control events (same logic as target)
                        df_control_sorted = df_control_exploded.sort_values(["PERSON_ID"] + group_cols + ["TIME_TO_EVENT"]).copy()
                        df_control_sorted["_rank_key"] = df_control_sorted.groupby(["PERSON_ID"] + group_cols)["TIME_TO_EVENT"].rank(method='dense')
                        df_control_sorted["ORDINAL"] = df_control_sorted["_rank_key"].astype(int)
                        df_control_ordinals = df_control_sorted.drop_duplicates(subset=["PERSON_ID"] + group_cols + ["ORDINAL"])
                        
                        # For control cohort, count ALL ordinals that exist (no 50% threshold)
                        # Only limit to ordinals 1-10 to match target cohort range
                        df_control_ordinals = df_control_ordinals[df_control_ordinals["ORDINAL"] <= 10]
                        
                        if not df_control_ordinals.empty:
                            logger.info(f"Found {df_control_ordinals[group_cols + ['ORDINAL']].drop_duplicates().shape[0]:,} control ordinal groups (all ordinals counted)")
                            
                            # Count control patients per ordinal group
                            control_counts = df_control_ordinals.groupby(ordinal_group_cols)["PERSON_ID"].nunique().reset_index(name="CONTROL_SUBJECT_COUNT")
                            
                            # Match control ordinals with target ordinals for comparison
                            # Use ORIGINAL_CONCEPT_ID for matching since CONCEPT_ID in ordinal_summaries has been modified with ordinal suffix
                            # Rename CONCEPT_ID to ORIGINAL_CONCEPT_ID for merging with ordinal_summaries
                            control_counts = control_counts.rename(columns={"CONCEPT_ID": "ORIGINAL_CONCEPT_ID"})
                            # Merge with ordinal_summaries using ORIGINAL_CONCEPT_ID, ORDINAL (and HERITAGE if applicable)
                            merge_cols = ["ORIGINAL_CONCEPT_ID", "ORDINAL"] + (["HERITAGE"] if has_heritage else [])
                            ordinal_summaries = ordinal_summaries.merge(control_counts[merge_cols + ["CONTROL_SUBJECT_COUNT"]], on=merge_cols, how="left", suffixes=("", "_new"))
                            # Update CONTROL_SUBJECT_COUNT where we have matches, keep 0 where we don't
                            ordinal_summaries["CONTROL_SUBJECT_COUNT"] = ordinal_summaries["CONTROL_SUBJECT_COUNT_new"].fillna(ordinal_summaries["CONTROL_SUBJECT_COUNT"]).astype(int)
                            ordinal_summaries = ordinal_summaries.drop(columns=["CONTROL_SUBJECT_COUNT_new"], errors="ignore")
                            logger.info(f"Matched {ordinal_summaries[ordinal_summaries['CONTROL_SUBJECT_COUNT'] > 0].shape[0]:,} target ordinal groups with control data")
                        else:
                            logger.warning("No valid control ordinals found")
                    else:
                        logger.warning("Control cohort had no valid TIME_TO_EVENT after exploding")
                else:
                    logger.warning(f"Control cohort had no concepts matching ordinal summaries (valid concepts: {len(valid_concepts)})")
            else:
                logger.warning("Control cohort was empty")
        else:
            logger.warning(f"No control data available (data_patients empty: {data_patients.empty}, total_control_patients: {total_control_patients})")
        
        # Apply small cell suppression to all count columns BEFORE calculating prevalences
        # IMPORTANT: Suppression must happen BEFORE calculating prevalences to ensure privacy protection
        ordinal_summaries["patient_count"] = apply_suppression_to_series(ordinal_summaries["patient_count"], min_cell_count)
        ordinal_summaries["TARGET_SUBJECT_COUNT"] = apply_suppression_to_series(ordinal_summaries["TARGET_SUBJECT_COUNT"], min_cell_count)
        ordinal_summaries["CONTROL_SUBJECT_COUNT"] = apply_suppression_to_series(ordinal_summaries["CONTROL_SUBJECT_COUNT"], min_cell_count)
        
        # Calculate prevalence AFTER suppression using suppressed counts
        if total_target_patients > 0:
            ordinal_summaries["TARGET_SUBJECT_PREVALENCE"] = ordinal_summaries["TARGET_SUBJECT_COUNT"] / total_target_patients
        else:
            ordinal_summaries["TARGET_SUBJECT_PREVALENCE"] = 0.0
        
        # Calculate control prevalence from suppressed CONTROL_SUBJECT_COUNT
        if total_control_patients > 0:
            ordinal_summaries["CONTROL_SUBJECT_PREVALENCE"] = ordinal_summaries["CONTROL_SUBJECT_COUNT"] / total_control_patients
        else:
            ordinal_summaries["CONTROL_SUBJECT_PREVALENCE"] = 0.0
        
        # Calculate PREVALENCE_DIFFERENCE_RATIO from suppressed target and control prevalences
        # Handle edge cases: 0 target → 0, 0 control → 100 (capped enrichment)
        def calc_enrichment(row):
            target_prev = row.get("TARGET_SUBJECT_PREVALENCE", 0)
            control_prev = row.get("CONTROL_SUBJECT_PREVALENCE", 0)
            if target_prev == 0:
                return 0.0
            elif control_prev == 0:
                return 100.0  # Capped enrichment when control is 0
            else:
                return target_prev / control_prev
        
        ordinal_summaries["PREVALENCE_DIFFERENCE_RATIO"] = ordinal_summaries.apply(calc_enrichment, axis=1)
        
        # Drop TARGET_SUBJECT_COUNT and CONTROL_SUBJECT_COUNT - they should not be in ordinal summaries
        # This is a safety measure to ensure they're never saved in the parquet file
        ordinal_summaries = ordinal_summaries.drop(columns=["TARGET_SUBJECT_COUNT", "CONTROL_SUBJECT_COUNT"], errors="ignore")
        
        logger.info(f"Generated {len(ordinal_summaries):,} ordinal summaries")
        return ordinal_summaries


def compute_clustering_for_k(
    preprocessed: PreprocessedData,
    data_features: pd.DataFrame,
    data_initial,
    data_person,
    k: int,
    concept_limit: Optional[int] = 60,
    min_cell_count: int = DEFAULT_MIN_CELL_COUNT,
    minibatch_kmeans_cutoff_patients: int = 50000,
) -> Optional[Dict]:
    """Compute clustering results for a specific k value using vectorized operations."""
    df_exploded = preprocessed.df_exploded
    
    if df_exploded.empty:
        return None
    
    # Get concepts from data_features (already filtered in preprocessing)
    if not data_features.empty and "CONCEPT_ID" in data_features.columns:
        available_concepts = len(data_features)
    else:
        available_concepts = df_exploded["CONCEPT_ID"].nunique()
    
    if concept_limit is None:
        concept_limit = available_concepts
    else:
        concept_limit = min(concept_limit, available_concepts)
    
    # Always use data_features for concept selection when available (ensures consistent ordering)
    if not data_features.empty and "TARGET_SUBJECT_PREVALENCE" in data_features.columns:
        top_concepts = data_features.nlargest(concept_limit, "TARGET_SUBJECT_PREVALENCE")["CONCEPT_ID"].astype(str).tolist()
    elif not data_features.empty and "CONCEPT_ID" in data_features.columns:
        top_concepts = data_features["CONCEPT_ID"].astype(str).head(concept_limit).tolist()
    else:
        top_concepts = df_exploded.groupby("CONCEPT_ID")["PERSON_ID"].nunique().nlargest(concept_limit).index.tolist()
    
    all_patients = sorted(df_exploded["PERSON_ID"].unique())
    if len(all_patients) < k:
        return None

    # Guardrail: cap concepts so patient x feature matrix stays within a manageable size.
    max_concepts_by_matrix = max(
        2,
        DEFAULT_CLUSTER_FEATURE_MATRIX_CELL_THRESHOLD // max(1, len(all_patients) * FEATURES_PER_CLUSTER_CONCEPT),
    )
    if len(top_concepts) > max_concepts_by_matrix:
        logger.warning(
            "Capping clustering concepts from %s to %s for memory safety (%s patients).",
            len(top_concepts),
            max_concepts_by_matrix,
            len(all_patients),
        )
        top_concepts = top_concepts[:max_concepts_by_matrix]

    events_df = df_exploded[df_exploded["CONCEPT_ID"].isin(top_concepts)].copy()
    if events_df.empty:
        return None

    feature_df = build_patient_feature_matrix(events_df, all_patients, time_col="TIME_TO_EVENT")
    if feature_df.empty or len(feature_df.columns) == 0:
        return None

    feature_matrix = events_df.groupby(["PERSON_ID", "CONCEPT_ID"]).size().unstack(fill_value=0)
    feature_matrix = (feature_matrix > 0).astype(int)
    feature_matrix = feature_matrix.reindex(pd.Index(all_patients, name="PERSON_ID"), fill_value=0)

    try:
        clustering_result = cluster_patient_features(
            feature_df,
            cluster_range=(k, k),
            pca_components=20,
            k_value=k,
            minibatch_kmeans_cutoff_patients=minibatch_kmeans_cutoff_patients,
        )
    except Exception as e:
        warnings.warn(f"Clustering failed for k={k}: {e}")
        return None

    cluster_labels = clustering_result["cluster_labels"]
    silhouette = float(clustering_result["best_silhouette_score"])
    patient_assignments = pd.DataFrame({'PERSON_ID': feature_matrix.index, 'cluster': [f'C{i+1}' for i in cluster_labels]})
    raw_cluster_sizes = patient_assignments['cluster'].value_counts().to_dict()
    cluster_sizes = {k_name: apply_small_cell_suppression(v, min_cell_count) for k_name, v in raw_cluster_sizes.items()}
    
    events_with_cluster = df_exploded.merge(patient_assignments, on='PERSON_ID', how='inner')
    
    # Main concept cluster stats
    cluster_stats = events_with_cluster.groupby(["CONCEPT_ID", "cluster"]).agg({
        'PERSON_ID': 'nunique',
        'TIME_TO_EVENT': ['median', lambda x: x.quantile(0.25), lambda x: x.quantile(0.75), 'min', 'max']
    })
    cluster_stats.columns = ['patient_count', 'time_median', 'time_q1', 'time_q3', 'time_min', 'time_max']
    cluster_stats = cluster_stats.reset_index()
    
    cluster_totals = patient_assignments.groupby('cluster')['PERSON_ID'].count().to_dict()
    cluster_stats['total_cluster_patients'] = cluster_stats['cluster'].map(cluster_totals)
    # NOTE: prevalence calculated AFTER suppression below
    
    # Add CONCEPT_NAME if available in data_features
    if not data_features.empty and "CONCEPT_ID" in data_features.columns and "CONCEPT_NAME" in data_features.columns:
        name_lookup = data_features[["CONCEPT_ID", "CONCEPT_NAME"]].drop_duplicates()
        cluster_stats = cluster_stats.merge(name_lookup, on="CONCEPT_ID", how="left")
    if "CONCEPT_NAME" not in cluster_stats.columns:
        cluster_stats["CONCEPT_NAME"] = cluster_stats["CONCEPT_ID"].astype(str)
    else:
        cluster_stats["CONCEPT_NAME"] = cluster_stats["CONCEPT_NAME"].fillna(cluster_stats["CONCEPT_ID"].astype(str))
    cluster_stats["ORIGINAL_CONCEPT_ID"] = cluster_stats["CONCEPT_ID"]
    cluster_stats["ORDINAL"] = 0
    cluster_stats["IS_ORDINAL"] = False
    
    # Add age and gender stats for main concepts per cluster
    if preprocessed.person_with_ages is not None and not preprocessed.person_with_ages.empty:
        logger.debug("Computing demographics for main concept cluster stats...")
        patient_ids_by_concept_cluster = events_with_cluster.groupby(["CONCEPT_ID", "cluster"])["PERSON_ID"].apply(set).reset_index()
        patient_ids_by_concept_cluster.columns = ["CONCEPT_ID", "cluster", "patient_ids"]
        
        demo_stats_list = []
        for _, row in patient_ids_by_concept_cluster.iterrows():
            patient_ids = row["patient_ids"]
            patient_demo = preprocessed.person_with_ages[preprocessed.person_with_ages["PERSON_ID"].isin(patient_ids)]
            demo_data = {"CONCEPT_ID": row["CONCEPT_ID"], "cluster": row["cluster"]}
            if not patient_demo.empty:
                if "AGE" in patient_demo.columns:
                    ages = patient_demo["AGE"].dropna()
                    if len(ages) > 0:
                        demo_data["age_mean"] = float(ages.mean())
                        demo_data["age_std"] = float(ages.std()) if len(ages) > 1 else 0.0
                if "GENDER_CONCEPT_ID" in patient_demo.columns:
                    male_count = (patient_demo["GENDER_CONCEPT_ID"] == 8507).sum()
                    if len(patient_demo) > 0:
                        demo_data["male_proportion"] = float(male_count / len(patient_demo))
            demo_stats_list.append(demo_data)
        
        if demo_stats_list:
            demo_stats_df = pd.DataFrame(demo_stats_list)
            cluster_stats = cluster_stats.merge(demo_stats_df, on=["CONCEPT_ID", "cluster"], how="left")
    
    # Apply small cell suppression to count columns FIRST
    cluster_stats["patient_count"] = apply_suppression_to_series(cluster_stats["patient_count"], min_cell_count)
    cluster_stats["total_cluster_patients"] = apply_suppression_to_series(cluster_stats["total_cluster_patients"], min_cell_count)
    
    # Calculate prevalence AFTER suppression using suppressed counts
    cluster_stats['prevalence'] = cluster_stats['patient_count'] / cluster_stats['total_cluster_patients']
    
    # Ordinal cluster stats
    logger.debug("Computing ordinal cluster statistics...")
    events_sorted = events_with_cluster.sort_values(["PERSON_ID", "CONCEPT_ID", "TIME_TO_EVENT"]).copy()
    events_sorted["_rank"] = events_sorted.groupby(["PERSON_ID", "CONCEPT_ID"])["TIME_TO_EVENT"].rank(method='dense')
    events_sorted["ORDINAL"] = events_sorted["_rank"].astype(int)
    events_ordinal = events_sorted.drop_duplicates(subset=["PERSON_ID", "CONCEPT_ID", "ORDINAL", "cluster"])
    
    ordinal_patient_counts = events_ordinal.groupby(["CONCEPT_ID", "ORDINAL"])["PERSON_ID"].nunique().reset_index(name="count")
    ordinal_pivot = ordinal_patient_counts.pivot_table(index="CONCEPT_ID", columns="ORDINAL", values="count", fill_value=0)
    
    def calc_max_ordinal(row):
        max_ord = 0
        prev_count = row.get(1, 0)
        for ordinal in range(1, 11):
            if ordinal not in row or prev_count == 0:
                break
            next_count = row.get(ordinal + 1, 0)
            if next_count >= prev_count * 0.5:
                max_ord = ordinal
                prev_count = next_count
            else:
                break
        return max_ord
    
    max_ordinals = ordinal_pivot.apply(calc_max_ordinal, axis=1).reset_index(name="max_ordinal")
    events_ordinal = events_ordinal.merge(max_ordinals, on="CONCEPT_ID", how="left")
    events_ordinal = events_ordinal[events_ordinal["ORDINAL"] <= events_ordinal["max_ordinal"]]
    events_ordinal = events_ordinal[events_ordinal["max_ordinal"] > 0]
    
    if not events_ordinal.empty:
        ordinal_cluster_stats = events_ordinal.groupby(["CONCEPT_ID", "ORDINAL", "cluster"]).agg({
            'PERSON_ID': 'nunique',
            'TIME_TO_EVENT': ['median', lambda x: x.quantile(0.25), lambda x: x.quantile(0.75), 'min', 'max']
        })
        ordinal_cluster_stats.columns = ['patient_count', 'time_median', 'time_q1', 'time_q3', 'time_min', 'time_max']
        ordinal_cluster_stats = ordinal_cluster_stats.reset_index()
        ordinal_cluster_stats['total_cluster_patients'] = ordinal_cluster_stats['cluster'].map(cluster_totals)
        # NOTE: prevalence calculated AFTER suppression below
        
        # Add CONCEPT_NAME if available in data_features
        if not data_features.empty and "CONCEPT_ID" in data_features.columns and "CONCEPT_NAME" in data_features.columns:
            name_lookup = data_features[["CONCEPT_ID", "CONCEPT_NAME"]].drop_duplicates()
            ordinal_cluster_stats = ordinal_cluster_stats.merge(name_lookup, on="CONCEPT_ID", how="left")
        if "CONCEPT_NAME" not in ordinal_cluster_stats.columns:
            ordinal_cluster_stats["CONCEPT_NAME"] = ordinal_cluster_stats["CONCEPT_ID"].astype(str)
        else:
            ordinal_cluster_stats["CONCEPT_NAME"] = ordinal_cluster_stats["CONCEPT_NAME"].fillna(ordinal_cluster_stats["CONCEPT_ID"].astype(str))
        
        # Add age and gender stats for ordinal concepts per cluster
        if preprocessed.person_with_ages is not None and not preprocessed.person_with_ages.empty:
            logger.debug("Computing demographics for ordinal cluster stats...")
            ordinal_patient_ids = events_ordinal.groupby(["CONCEPT_ID", "ORDINAL", "cluster"])["PERSON_ID"].apply(set).reset_index()
            ordinal_patient_ids.columns = ["CONCEPT_ID", "ORDINAL", "cluster", "patient_ids"]
            
            ordinal_demo_list = []
            for _, row in ordinal_patient_ids.iterrows():
                patient_ids = row["patient_ids"]
                patient_demo = preprocessed.person_with_ages[preprocessed.person_with_ages["PERSON_ID"].isin(patient_ids)]
                demo_data = {"CONCEPT_ID": row["CONCEPT_ID"], "ORDINAL": row["ORDINAL"], "cluster": row["cluster"]}
                if not patient_demo.empty:
                    if "AGE" in patient_demo.columns:
                        ages = patient_demo["AGE"].dropna()
                        if len(ages) > 0:
                            demo_data["age_mean"] = float(ages.mean())
                            demo_data["age_std"] = float(ages.std()) if len(ages) > 1 else 0.0
                    if "GENDER_CONCEPT_ID" in patient_demo.columns:
                        male_count = (patient_demo["GENDER_CONCEPT_ID"] == 8507).sum()
                        if len(patient_demo) > 0:
                            demo_data["male_proportion"] = float(male_count / len(patient_demo))
                ordinal_demo_list.append(demo_data)
            
            if ordinal_demo_list:
                ordinal_demo_df = pd.DataFrame(ordinal_demo_list)
                ordinal_cluster_stats = ordinal_cluster_stats.merge(ordinal_demo_df, on=["CONCEPT_ID", "ORDINAL", "cluster"], how="left")
        
        ordinal_suffix_map = {1: "st", 2: "nd", 3: "rd"}
        ordinal_cluster_stats["ordinal_name_suffix"] = ordinal_cluster_stats["ORDINAL"].apply(lambda x: f"{x}{ordinal_suffix_map.get(x, 'th')}")
        ordinal_cluster_stats["ORIGINAL_CONCEPT_ID"] = ordinal_cluster_stats["CONCEPT_ID"]
        ordinal_cluster_stats["CONCEPT_ID"] = ordinal_cluster_stats["CONCEPT_ID"].astype(str).str.replace(r'\.0$', '', regex=True) + "_" + ordinal_cluster_stats["ORDINAL"].astype(str)
        ordinal_cluster_stats["CONCEPT_NAME"] = ordinal_cluster_stats["CONCEPT_NAME"] + " " + ordinal_cluster_stats["ordinal_name_suffix"]
        ordinal_cluster_stats["IS_ORDINAL"] = True
        
        # Apply small cell suppression to count columns FIRST
        ordinal_cluster_stats["patient_count"] = apply_suppression_to_series(ordinal_cluster_stats["patient_count"], min_cell_count)
        ordinal_cluster_stats["total_cluster_patients"] = apply_suppression_to_series(ordinal_cluster_stats["total_cluster_patients"], min_cell_count)
        
        # Calculate prevalence AFTER suppression using suppressed counts
        ordinal_cluster_stats['prevalence'] = ordinal_cluster_stats['patient_count'] / ordinal_cluster_stats['total_cluster_patients']
        
        ordinal_cluster_stats = ordinal_cluster_stats.drop(columns=["ordinal_name_suffix"], errors="ignore")
        
        for col in cluster_stats.columns:
            if col not in ordinal_cluster_stats.columns:
                ordinal_cluster_stats[col] = None
        for col in ordinal_cluster_stats.columns:
            if col not in cluster_stats.columns:
                cluster_stats[col] = None
        
        summary_matrix = pd.concat([cluster_stats, ordinal_cluster_stats], ignore_index=True)
        logger.debug(f"Added {len(ordinal_cluster_stats):,} ordinal cluster rows")
    else:
        summary_matrix = cluster_stats
    
    pairwise_overlap = compute_pairwise_overlap_vectorized(
        feature_matrix,
        top_concepts,
        patient_assignments,
        min_cell_count,
    )
    
    return {
        "k": k, "silhouette_score": silhouette, "cluster_sizes": cluster_sizes,
        "summary_matrix": summary_matrix, "pairwise_overlap": pairwise_overlap,
        "concepts_used": top_concepts, "total_patients": apply_small_cell_suppression(len(all_patients), min_cell_count),
        "patient_assignments": patient_assignments,
        "sampled": clustering_result.get("sampled", False),
        "algorithm": clustering_result.get("algorithm", "kmedoids"),
    }


def compute_pairwise_overlap_vectorized(
    feature_matrix,
    concept_list,
    patient_assignments,
    min_cell_count,
) -> pd.DataFrame:
    """Compute pairwise Jaccard and Phi matrices using vectorized operations.
    
    Computes overlap for:
    - "overall": all patients
    - Per-cluster: patients in each cluster (C1, C2, etc.)
    """
    if feature_matrix.shape[1] > DEFAULT_PAIRWISE_OVERLAP_MAX_CONCEPTS:
        concept_counts = feature_matrix.sum(axis=0).sort_values(ascending=False)
        selected_concepts = concept_counts.head(DEFAULT_PAIRWISE_OVERLAP_MAX_CONCEPTS).index
        logger.warning(
            "Capping pairwise overlap concepts from %s to %s to avoid quadratic memory growth.",
            feature_matrix.shape[1],
            DEFAULT_PAIRWISE_OVERLAP_MAX_CONCEPTS,
        )
        feature_matrix = feature_matrix.loc[:, selected_concepts]

    concept_patients = {cid: set(feature_matrix[feature_matrix[cid] > 0].index) for cid in feature_matrix.columns}
    n_total = len(feature_matrix)
    pairwise_rows = []
    concept_ids = list(concept_patients.keys())
    
    # Helper function to compute overlap metrics for a set of patients
    def compute_overlap_for_group(group_name, group_patient_ids, group_size):
        rows = []
        for i, cid_i in enumerate(concept_ids):
            # Get patients with concept i that are in this group
            patients_i = concept_patients[cid_i] & group_patient_ids
            count_i = len(patients_i)
            
            # Self-overlap (diagonal)
            suppressed_count_i = apply_small_cell_suppression(count_i, min_cell_count)
            rows.append({
                "concept_id_1": cid_i, "concept_id_2": cid_i, "jaccard": 1.0, "phi_correlation": 1.0,
                "prevalence": suppressed_count_i / group_size if group_size > 0 else 0,
                "patient_count": suppressed_count_i, "group": group_name
            })
            
            # Pairwise overlap with other concepts
            for j in range(i + 1, len(concept_ids)):
                cid_j = concept_ids[j]
                patients_j = concept_patients[cid_j] & group_patient_ids
                
                intersection = len(patients_i & patients_j)
                union = len(patients_i | patients_j)
                jaccard = intersection / union if union > 0 else 0
                
                # Phi correlation calculation
                n_11 = intersection
                n_10 = len(patients_i - patients_j)
                n_01 = len(patients_j - patients_i)
                n_00 = group_size - n_11 - n_10 - n_01
                denom_term = (n_11 + n_10) * (n_11 + n_01) * (n_00 + n_10) * (n_00 + n_01)
                denom = math.sqrt(float(denom_term)) if denom_term > 0 else 0.0
                phi = (n_11 * n_00 - n_10 * n_01) / denom if denom > 0 else 0
                
                rows.append({
                    "concept_id_1": cid_i, "concept_id_2": cid_j, 
                    "jaccard": float(jaccard), "phi_correlation": float(phi),
                    "co_occurrence": apply_small_cell_suppression(int(intersection), min_cell_count),
                    "union": apply_small_cell_suppression(int(union), min_cell_count), 
                    "group": group_name
                })
        return rows
    
    # Compute overall overlap
    all_patient_ids = set(feature_matrix.index)
    pairwise_rows.extend(compute_overlap_for_group("overall", all_patient_ids, n_total))
    
    # Compute per-cluster overlap
    if patient_assignments is not None and not patient_assignments.empty:
        clusters = patient_assignments["cluster"].unique()
        for cluster in clusters:
            cluster_patient_ids = set(patient_assignments[patient_assignments["cluster"] == cluster]["PERSON_ID"])
            cluster_size = len(cluster_patient_ids)
            if cluster_size > 0:
                pairwise_rows.extend(compute_overlap_for_group(cluster, cluster_patient_ids, cluster_size))
        
    return pd.DataFrame(pairwise_rows)
    

def compute_all_clusterings_parallel(
    preprocessed,
    data_features,
    data_initial,
    data_person,
    k_values,
    concept_limit,
    min_cell_count,
    max_parallel_jobs: int = 2,
    minibatch_kmeans_cutoff_patients: int = 50000,
) -> Dict[int, Dict]:
    """Compute clustering for multiple k values.
    
    Args:
        max_parallel_jobs: Maximum number of parallel jobs. Set to 1 to disable parallelism
                          (useful for memory-constrained environments). Default is 2 to balance
                          speed and memory usage.
    """
    # Sequential execution is more memory-efficient for large datasets
    # Each clustering run can use significant memory, so we limit parallelism
    if max_parallel_jobs > 1 and len(k_values) > 1:
        try:
            from joblib import Parallel, delayed
            # Limit to max_parallel_jobs to avoid OOM on servers
            n_jobs = min(max_parallel_jobs, len(k_values))
            logger.info(f"Computing clustering for k={k_values} with {n_jobs} parallel jobs...")
            results = Parallel(n_jobs=n_jobs, prefer="threads")(
                delayed(compute_clustering_for_k)(
                    preprocessed,
                    data_features,
                    data_initial,
                    data_person,
                    k,
                    concept_limit,
                    min_cell_count,
                    minibatch_kmeans_cutoff_patients,
                )
                for k in k_values
            )
            return {k: r for k, r in zip(k_values, results) if r is not None}
        except ImportError:
            logger.info("joblib not available, falling back to sequential execution")
    
    # Sequential execution (default for memory safety)
    results = {}
    for k in k_values:
        logger.info(f"Computing clustering for k={k}...")
        result = compute_clustering_for_k(
            preprocessed,
            data_features,
            data_initial,
            data_person,
            k,
            concept_limit,
            min_cell_count,
            minibatch_kmeans_cutoff_patients,
        )
        if result is not None:
            results[k] = result
    return results


def compute_cluster_demographics_for_assignments(
    person_with_ages: pd.DataFrame,
    patient_assignments: Optional[pd.DataFrame],
    min_cell_count: int = DEFAULT_MIN_CELL_COUNT
) -> Dict[str, Dict]:
    """
    Compute per-cluster demographic summaries from patient assignments.

    Returns:
        Dict keyed by cluster label (e.g., "C1"), with patient_count, age stats, n, and male_proportion.
    """
    if (
        patient_assignments is None
        or patient_assignments.empty
        or "PERSON_ID" not in patient_assignments.columns
        or "cluster" not in patient_assignments.columns
        or person_with_ages is None
        or person_with_ages.empty
        or "PERSON_ID" not in person_with_ages.columns
    ):
        return {}
    
    cluster_demographics = {}
    
    for cluster in sorted(patient_assignments["cluster"].dropna().unique()):
        cluster_patient_ids = patient_assignments[patient_assignments["cluster"] == cluster]["PERSON_ID"]
        if cluster_patient_ids.empty:
            continue
        
        cluster_persons = person_with_ages[person_with_ages["PERSON_ID"].isin(cluster_patient_ids)]
        if cluster_persons.empty:
            continue
        
        cluster_summary = {
            "patient_count": apply_small_cell_suppression(int(cluster_patient_ids.nunique()), min_cell_count)
        }
        
        if "AGE" in cluster_persons.columns:
            cluster_ages = cluster_persons["AGE"].dropna().values
            if len(cluster_ages) > 0:
                q1, q3 = np.percentile(cluster_ages, [25, 75])
                cluster_summary.update({
                    "age_mean": float(np.mean(cluster_ages)),
                    "age_median": float(np.median(cluster_ages)),
                    "age_std": float(np.std(cluster_ages)) if len(cluster_ages) > 1 else 0.0,
                    "age_q1": float(q1),
                    "age_q3": float(q3),
                    "n": apply_small_cell_suppression(int(len(cluster_ages)), min_cell_count)
                })
        
        if "GENDER_CONCEPT_ID" in cluster_persons.columns and len(cluster_persons) > 0:
            male_count = (cluster_persons["GENDER_CONCEPT_ID"] == 8507).sum()
            cluster_summary["male_proportion"] = float(male_count / len(cluster_persons))
        
        cluster_demographics[str(cluster)] = cluster_summary
    
    return cluster_demographics


def precompute_study_summary(
    study_path: str,
    output_path: Optional[str] = None,
    cluster_k_values: List[int] = [2, 3, 4, 5],
    concept_limit: Optional[int] = None,
    min_cell_count: int = DEFAULT_MIN_CELL_COUNT,
    max_parallel_jobs: int = 1,
    minibatch_kmeans_cutoff_patients: int = 50000,
) -> Dict:
    """Pre-compute all summary data for a study, removing patient-level information."""
    setup_logging(verbose=True)
    total_start = time.time()
    
    study_path = Path(study_path)
    original_study_name = study_path.name
    
    if output_path is None:
        output_path = study_path.parent / f"{original_study_name}_summary"
    else:
        output_path = Path(output_path)
    
    output_path.mkdir(parents=True, exist_ok=True)
    summary_study_name = output_path.name
    
    logger.info("=" * 60)
    logger.info(f"Pre-computing summary for: {study_path}")
    logger.info(f"Output directory: {output_path}")
    logger.info(f"Concept limit: {'all' if concept_limit is None else concept_limit}")
    logger.info(f"Min cell count: {min_cell_count}")
    logger.info(f"MiniBatchKMeans cutoff patients: {minibatch_kmeans_cutoff_patients}")
    logger.info("=" * 60)
    
    data_patients, data_features, data_initial, data_person, mapping_table = None, None, None, None, None
    
    with Timer("Loading parquet files"):
        if (study_path / "data_patients.parquet").exists():
            data_patients = pd.read_parquet(study_path / "data_patients.parquet", engine=PARQUET_ENGINE)
            data_patients = deserialize_json_columns(data_patients)
            logger.info(f"  data_patients: {len(data_patients):,} rows")
        if (study_path / "data_features.parquet").exists():
            data_features = pd.read_parquet(study_path / "data_features.parquet", engine=PARQUET_ENGINE)
            # Convert CONCEPT_ID to string for consistent merging throughout
            if "CONCEPT_ID" in data_features.columns:
                data_features["CONCEPT_ID"] = data_features["CONCEPT_ID"].astype(str).str.replace(r'\.0$', '', regex=True)
            logger.info(f"  data_features: {len(data_features):,} rows")
        if (study_path / "data_initial.parquet").exists():
            data_initial = pd.read_parquet(study_path / "data_initial.parquet", engine=PARQUET_ENGINE)
            logger.info(f"  data_initial: {len(data_initial):,} rows")
        if (study_path / "data_person.parquet").exists():
            data_person = pd.read_parquet(study_path / "data_person.parquet", engine=PARQUET_ENGINE)
            logger.info(f"  data_person: {len(data_person):,} rows")
        if (study_path / "complementaryMappingTable.parquet").exists():
            mapping_table = pd.read_parquet(study_path / "complementaryMappingTable.parquet", engine=PARQUET_ENGINE)
            logger.info(f"  complementaryMappingTable: {len(mapping_table):,} rows")
    
    if data_patients is None:
        raise ValueError(f"data_patients.parquet not found in {study_path}")
    if data_features is None:
        data_features = pd.DataFrame()
    
    generated_files = {}
    preprocessed = preprocess_data(data_patients, data_features, data_person, data_initial)
    
    if mapping_table is not None:
        mapping_table.to_parquet(output_path / "complementaryMappingTable.parquet", engine=PARQUET_ENGINE)
        generated_files["mapping_table"] = str(output_path / "complementaryMappingTable.parquet")
        logger.info("✓ Copied complementaryMappingTable.parquet")
    
    # NOTE: data_features.parquet is intentionally NOT copied to summary output
    # It contains patient-level data that is not privacy-safe.
    # All needed aggregated data is in concept_summaries.parquet with small cell suppression applied.
    
    concept_summaries = compute_concept_summaries_vectorized(preprocessed, data_features, min_cell_count)
    if not concept_summaries.empty:
        concept_summaries.to_parquet(output_path / "concept_summaries.parquet", engine=PARQUET_ENGINE)
        generated_files["concept_summaries"] = str(output_path / "concept_summaries.parquet")
        logger.info(f"✓ Generated concept_summaries.parquet: {len(concept_summaries):,} rows")
    
    ordinal_summaries = compute_ordinal_summaries_vectorized(preprocessed, data_features, data_patients, min_cell_count)
    if not ordinal_summaries.empty:
        ordinal_summaries.to_parquet(output_path / "ordinal_summaries.parquet", engine=PARQUET_ENGINE)
        generated_files["ordinal_summaries"] = str(output_path / "ordinal_summaries.parquet")
        logger.info(f"✓ Generated ordinal_summaries.parquet: {len(ordinal_summaries):,} rows")
    
    with Timer("Computing study demographics"):
        study_demographics = {"target_patients": preprocessed.total_target_patients}
        study_demographics["control_patients"] = data_patients[data_patients["COHORT_DEFINITION_ID"] == "control"]["PERSON_ID"].nunique()
        if not preprocessed.person_with_ages.empty and "AGE" in preprocessed.person_with_ages.columns:
            ages = preprocessed.person_with_ages["AGE"].dropna()
            if len(ages) > 0:
                study_demographics["age_mean"] = float(ages.mean())
                study_demographics["age_median"] = float(ages.median())
                study_demographics["age_std"] = float(ages.std())
        if "GENDER_CONCEPT_ID" in preprocessed.person_with_ages.columns:
            male_count = (preprocessed.person_with_ages["GENDER_CONCEPT_ID"] == 8507).sum()
            if len(preprocessed.person_with_ages) > 0:
                study_demographics["male_proportion"] = float(male_count / len(preprocessed.person_with_ages))
    
    with Timer(f"Computing clustering for k={cluster_k_values}"):
        clustering_results = compute_all_clusterings_parallel(
            preprocessed,
            data_features,
            data_initial,
            data_person,
            cluster_k_values,
            concept_limit,
            min_cell_count,
            max_parallel_jobs,
            minibatch_kmeans_cutoff_patients=minibatch_kmeans_cutoff_patients,
        )
        for k, result in clustering_results.items():
            # Ensure CONCEPT_ID is string to avoid mixed type issues with PyArrow
            summary_df = result["summary_matrix"].copy()
            if "CONCEPT_ID" in summary_df.columns:
                summary_df["CONCEPT_ID"] = summary_df["CONCEPT_ID"].astype(str)
            if "ORIGINAL_CONCEPT_ID" in summary_df.columns:
                summary_df["ORIGINAL_CONCEPT_ID"] = summary_df["ORIGINAL_CONCEPT_ID"].astype(str)
            summary_df.to_parquet(output_path / f"clustering_k{k}_summary.parquet", engine=PARQUET_ENGINE)
            generated_files[f"clustering_k{k}_summary"] = str(output_path / f"clustering_k{k}_summary.parquet")
            if "pairwise_overlap" in result and not result["pairwise_overlap"].empty:
                result["pairwise_overlap"].to_parquet(output_path / f"clustering_k{k}_pairwise_overlap.parquet", engine=PARQUET_ENGINE)
                generated_files[f"clustering_k{k}_pairwise_overlap"] = str(output_path / f"clustering_k{k}_pairwise_overlap.parquet")
            logger.info(f"✓ k={k}: silhouette={result['silhouette_score']:.3f}, patients={result['total_patients']}")
        clustering_metadata = {
            k: {
                "silhouette_score": r["silhouette_score"],
                "cluster_sizes": r["cluster_sizes"],
                "total_patients": r["total_patients"],
                "concepts_used_count": len(r["concepts_used"]),
                "sampled": bool(r.get("sampled", False)),
                "algorithm": str(r.get("algorithm", "kmedoids")),
            }
            for k, r in clustering_results.items()
        }
    
    demographics_data = {"overall": {}, "clusters": {}, "clusters_by_k": {}, "clusters_best_k": None}
    if preprocessed.person_with_ages is not None and not preprocessed.person_with_ages.empty:
        with Timer("Computing demographic distributions"):
            if "AGE" in preprocessed.person_with_ages.columns:
                ages = preprocessed.person_with_ages["AGE"].dropna().values
                if len(ages) > 0:
                    age_bins = list(range(0, 110, 10))
                    age_hist, _ = np.histogram(ages, bins=age_bins)
                    q1, q3 = np.percentile(ages, [25, 75])
                    demographics_data["overall"]["age_histogram"] = {
                        "bins": [f"{age_bins[i]}-{age_bins[i+1]}" for i in range(len(age_bins)-1)],
                        "counts": [apply_small_cell_suppression(int(c), min_cell_count) for c in age_hist],
                        "mean": float(np.mean(ages)), "median": float(np.median(ages)), "std": float(np.std(ages)),
                        "q1": float(q1), "q3": float(q3), "n": apply_small_cell_suppression(len(ages), min_cell_count)
                    }
            if "GENDER_CONCEPT_ID" in preprocessed.person_with_ages.columns:
                person_df = preprocessed.person_with_ages
                demographics_data["overall"]["sex_distribution"] = {
                    "male": apply_small_cell_suppression(int((person_df["GENDER_CONCEPT_ID"] == 8507).sum()), min_cell_count),
                    "female": apply_small_cell_suppression(int((person_df["GENDER_CONCEPT_ID"] == 8532).sum()), min_cell_count),
                    "total": apply_small_cell_suppression(len(person_df), min_cell_count)
                }
            if clustering_results:
                best_k = max(clustering_results.keys(), key=lambda k: clustering_results[k].get("silhouette_score", 0))
                demographics_data["clusters_best_k"] = int(best_k)
                
                for k, result in clustering_results.items():
                    patient_assignments = result.get("patient_assignments")
                    cluster_demo = compute_cluster_demographics_for_assignments(
                        preprocessed.person_with_ages,
                        patient_assignments,
                        min_cell_count
                    )
                    if cluster_demo:
                        demographics_data["clusters_by_k"][str(k)] = cluster_demo
                
                # Backward compatibility: keep top-level "clusters" as best-k demographics.
                best_k_key = str(best_k)
                if best_k_key in demographics_data["clusters_by_k"]:
                    demographics_data["clusters"] = demographics_data["clusters_by_k"][best_k_key]
    
    study_demographics["distributions"] = demographics_data
    
    # Calculate number of unique main concepts (non-ordinal)
    # concept_summaries only contains main concepts (ordinals are in ordinal_summaries)
    # Count unique CONCEPT_ID values (same concept may appear multiple times if it has multiple HERITAGE values)
    significant_concepts_count = 0
    if not concept_summaries.empty and "CONCEPT_ID" in concept_summaries.columns:
        # Count unique CONCEPT_ID values (main concepts only)
        # Note: same CONCEPT_ID may appear multiple times with different HERITAGE values
        significant_concepts_count = concept_summaries["CONCEPT_ID"].nunique()
        logger.info(f"Counted {significant_concepts_count:,} unique main concepts (significant concepts)")
    
    with Timer("Saving metadata"):
        metadata = {
            "study_name": summary_study_name, "original_study_name": original_study_name, "source_path": str(study_path),
            "mode": "summary", "demographics": study_demographics, "clustering": clustering_metadata,
            "cluster_k_values": cluster_k_values, "concept_limit": concept_limit, "min_cell_count": min_cell_count,
            "significant_concepts": significant_concepts_count,
            "minibatch_kmeans_cutoff_patients": int(minibatch_kmeans_cutoff_patients),
        }
        with open(output_path / "metadata.json", "w") as f:
            json.dump(metadata, f, indent=2)
        generated_files["metadata"] = str(output_path / "metadata.json")
        logger.info("✓ Generated metadata.json")
    
    csv_files = list(study_path.glob("*.csv"))
    if csv_files:
        for csv_file in csv_files:
            try:
                csv_df = pd.read_csv(csv_file)
                if "study" in csv_df.columns:
                    csv_df["study"] = summary_study_name
                output_csv_path = output_path / f"{summary_study_name}.csv"
                csv_df.to_csv(output_csv_path, index=False)
                generated_files["csv"] = str(output_csv_path)
                logger.info(f"✓ Created {output_csv_path.name}")
            except Exception as e:
                import shutil
                shutil.copy(csv_file, output_path / csv_file.name)
                generated_files["csv"] = str(output_path / csv_file.name)
                logger.warning(f"Copied {csv_file.name} (could not update: {e})")
    
    desc_file = study_path / "desc.txt"
    if desc_file.exists():
        import shutil
        shutil.copy(desc_file, output_path / "desc.txt")
        generated_files["desc"] = str(output_path / "desc.txt")
        logger.info("✓ Copied desc.txt")
    
    total_elapsed = time.time() - total_start
    logger.info("=" * 60)
    logger.info(f"Summary generation complete!")
    logger.info(f"Output: {output_path}")
    logger.info(f"Files generated: {len(generated_files)}")
    logger.info(f"Total time: {total_elapsed:.2f}s")
    logger.info("=" * 60)
    
    print(f"Pre-computing summary for: {study_path}")
    print(f"Output directory: {output_path}")
    
    return {"output_path": str(output_path), "files": generated_files, "metadata": metadata}


# Legacy wrappers
def compute_time_distribution_summary(times: np.ndarray, n_bins: int = 50) -> Dict:
    return compute_time_stats_vectorized(times)


def compute_concept_summaries(data_patients, data_features, data_person, data_initial=None, min_cell_count=DEFAULT_MIN_CELL_COUNT):
    data_patients = deserialize_json_columns(data_patients)
    preprocessed = preprocess_data(data_patients, data_features, data_person, data_initial)
    return compute_concept_summaries_vectorized(preprocessed, data_features, min_cell_count)


def compute_ordinal_summaries(data_patients, data_features, data_person, data_initial=None, min_cell_count=DEFAULT_MIN_CELL_COUNT):
    data_patients = deserialize_json_columns(data_patients)
    preprocessed = preprocess_data(data_patients, data_features, data_person, data_initial)
    return compute_ordinal_summaries_vectorized(preprocessed, data_features, data_patients, min_cell_count)


if __name__ == "__main__":
    import sys
    if len(sys.argv) < 2:
        print("Usage: python -m precompute.summarize <study_path> [output_path]")
        sys.exit(1)
    study_path = sys.argv[1]
    output_path = sys.argv[2] if len(sys.argv) > 2 else None
    result = precompute_study_summary(study_path, output_path)
    print(f"\nGenerated files: {list(result['files'].keys())}")
