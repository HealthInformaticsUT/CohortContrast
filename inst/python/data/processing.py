"""
Data processing functions for ContrastViewer.

This module contains functions for calculating metrics, medians,
and creating ordinal concepts from patient data.
"""

from typing import Dict, List, Optional, Tuple
import warnings

import pandas as pd
import numpy as np

from utils.helpers import normalize_concept_id, get_unique_occurrences


def calculate_median_first_occurrence(data_patients: pd.DataFrame) -> pd.DataFrame:
    """
    Calculate the median first occurrence time for each concept in the target cohort.
    
    For each concept and person in the target cohort:
    1. Find the minimum TIME_TO_EVENT (first occurrence)
    2. Calculate the median of these minimum times across all persons
    
    Groups by both CONCEPT_ID and HERITAGE since the pair is unique.
    
    Args:
        data_patients: DataFrame with columns COHORT_DEFINITION_ID, PERSON_ID, 
                      CONCEPT_ID, HERITAGE, TIME_TO_EVENT
        
    Returns:
        DataFrame with columns CONCEPT_ID, HERITAGE, and MEDIAN_FIRST_OCCURRENCE (in days)
    """
    # Filter for target cohort only
    df_target = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
    
    if df_target.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Ensure HERITAGE column exists
    has_heritage = "HERITAGE" in df_target.columns and df_target["HERITAGE"].notna().any()
    
    # Calculate first occurrence (minimum time) for each person-concept combination
    def get_min_time(time_list):
        """Get minimum time from possibly nested time values."""
        if time_list is None:
            return None
        if isinstance(time_list, np.ndarray):
            if time_list.size == 0:
                return None
            valid_times = time_list[~np.isnan(time_list)]
            return float(np.min(valid_times)) if len(valid_times) > 0 else None
        if isinstance(time_list, (list, tuple)):
            valid_times = [t for t in time_list if t is not None and not pd.isna(t)]
            return min(valid_times) if valid_times else None
        if pd.isna(time_list):
            return None
        return float(time_list)
    
    df_target["FIRST_OCCURRENCE"] = df_target["TIME_TO_EVENT"].apply(get_min_time)
    df_target = df_target[df_target["FIRST_OCCURRENCE"].notna()].copy()
    
    if df_target.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Group by both CONCEPT_ID and HERITAGE (the pair is unique)
    if has_heritage:
        median_df = df_target.groupby(["CONCEPT_ID", "HERITAGE"])["FIRST_OCCURRENCE"].median().reset_index()
        median_df.columns = ["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"]
    else:
        median_df = df_target.groupby("CONCEPT_ID")["FIRST_OCCURRENCE"].median().reset_index()
        median_df.columns = ["CONCEPT_ID", "MEDIAN_FIRST_OCCURRENCE"]
        median_df["HERITAGE"] = None
    
    return median_df


def calculate_ordinal_medians(
    data_patients: pd.DataFrame, 
    ordinal_concepts_df: pd.DataFrame
) -> pd.DataFrame:
    """
    Calculate median TIME_TO_EVENT for ordinal concepts (1st, 2nd, 3rd occurrences).
    
    Args:
        data_patients: DataFrame with TIME_TO_EVENT arrays per patient
        ordinal_concepts_df: DataFrame with ordinal concept definitions
        
    Returns:
        DataFrame with ordinal medians added
    """
    if ordinal_concepts_df.empty:
        return pd.DataFrame()
    
    # Filter to target cohort
    target_df = data_patients[data_patients['COHORT_DEFINITION_ID'] == 'target'].copy()
    
    if target_df.empty:
        return ordinal_concepts_df
    
    ordinal_medians = []
    
    for _, ordinal_row in ordinal_concepts_df.iterrows():
        original_concept_id = ordinal_row.get('ORIGINAL_CONCEPT_ID')
        ordinal_num = ordinal_row.get('ORDINAL', 1)
        
        if original_concept_id is None or ordinal_num < 1:
            continue
        
        # Get patients with this concept
        concept_patients = target_df[target_df['CONCEPT_ID'] == original_concept_id]
        
        occurrence_times = []
        for _, patient_row in concept_patients.iterrows():
            time_values = patient_row.get('TIME_TO_EVENT', [])
            
            # Handle different data types
            if time_values is None:
                continue
            if isinstance(time_values, (int, float)):
                time_values = [time_values]
            elif isinstance(time_values, np.ndarray):
                time_values = time_values.tolist()
            elif not isinstance(time_values, list):
                continue
            
            # Sort and get the nth occurrence
            time_values = [t for t in time_values if t is not None and not pd.isna(t)]
            time_values.sort()
            
            if len(time_values) >= ordinal_num:
                occurrence_times.append(time_values[ordinal_num - 1])
        
        median_time = np.median(occurrence_times) if occurrence_times else None
        
        ordinal_row_copy = ordinal_row.to_dict()
        ordinal_row_copy['MEDIAN_FIRST_OCCURRENCE'] = median_time
        ordinal_medians.append(ordinal_row_copy)
    
    return pd.DataFrame(ordinal_medians) if ordinal_medians else ordinal_concepts_df


def expand_time_to_event_column(data_patients: pd.DataFrame) -> pd.DataFrame:
    """
    Expand TIME_TO_EVENT column from lists/arrays to individual rows.
    
    Useful for detailed time analysis where each occurrence needs separate handling.
    
    Args:
        data_patients: DataFrame with TIME_TO_EVENT containing lists or arrays
        
    Returns:
        DataFrame with one row per time event
    """
    if 'TIME_TO_EVENT' not in data_patients.columns:
        return data_patients
    
    # Check if expansion is needed
    sample = data_patients['TIME_TO_EVENT'].dropna().iloc[0] if not data_patients['TIME_TO_EVENT'].dropna().empty else None
    if sample is None or not hasattr(sample, '__iter__') or isinstance(sample, str):
        return data_patients
    
    expanded = data_patients.explode('TIME_TO_EVENT')
    expanded['TIME_TO_EVENT'] = pd.to_numeric(expanded['TIME_TO_EVENT'], errors='coerce')
    
    return expanded.dropna(subset=['TIME_TO_EVENT'])


def get_concept_patient_counts(data_patients: pd.DataFrame) -> pd.DataFrame:
    """
    Count unique patients per concept in target cohort.
    
    Args:
        data_patients: Patient data DataFrame
        
    Returns:
        DataFrame with CONCEPT_ID and patient_count columns
    """
    target_df = data_patients[data_patients['COHORT_DEFINITION_ID'] == 'target']
    
    counts = target_df.groupby('CONCEPT_ID')['PERSON_ID'].nunique().reset_index()
    counts.columns = ['CONCEPT_ID', 'patient_count']
    
    return counts


def calculate_concept_metrics_from_patients(data_patients: pd.DataFrame, data_initial: pd.DataFrame, 
                                            concepts_df: pd.DataFrame) -> pd.DataFrame:
    """
    Calculate TARGET_SUBJECT_PREVALENCE and PREVALENCE_DIFFERENCE_RATIO from data_patients.
    
    Optimized version using vectorized operations instead of iterrows.
    
    For ordinal concepts, calculates based on the specific ordinal occurrence.
    For main concepts, calculates based on any occurrence.
    
    Args:
        data_patients: DataFrame with COHORT_DEFINITION_ID, PERSON_ID, CONCEPT_ID, HERITAGE, TIME_TO_EVENT, PREVALENCE
        data_initial: DataFrame with COHORT_DEFINITION_ID, SUBJECT_ID
        concepts_df: DataFrame with concepts to calculate metrics for (CONCEPT_ID, HERITAGE, ORIGINAL_CONCEPT_ID, ORDINAL, IS_ORDINAL)
        
    Returns:
        DataFrame with CONCEPT_ID, HERITAGE, TARGET_SUBJECT_PREVALENCE, PREVALENCE_DIFFERENCE_RATIO
    """
    if data_patients.empty or concepts_df.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "TARGET_SUBJECT_PREVALENCE", "PREVALENCE_DIFFERENCE_RATIO"])
    
    # Get total counts for target and control cohorts
    target_total = data_initial[data_initial["COHORT_DEFINITION_ID"] == "target"]["SUBJECT_ID"].nunique()
    control_total = data_initial[data_initial["COHORT_DEFINITION_ID"] == "control"]["SUBJECT_ID"].nunique()
    
    if target_total == 0:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "TARGET_SUBJECT_PREVALENCE", "PREVALENCE_DIFFERENCE_RATIO"])
    
    has_heritage = "HERITAGE" in data_patients.columns and "HERITAGE" in concepts_df.columns
    
    # Pre-compute occurrence counts for all patients
    data_patients = data_patients.copy()
    data_patients["NUM_OCCURRENCES"] = data_patients["TIME_TO_EVENT"].apply(
        lambda x: len(get_unique_occurrences(x))
    )
    
    # Split concepts into main and ordinal
    concepts_df = concepts_df.copy()
    if "IS_ORDINAL" not in concepts_df.columns:
        concepts_df["IS_ORDINAL"] = False
    if "ORDINAL" not in concepts_df.columns:
        concepts_df["ORDINAL"] = 0
    if "ORIGINAL_CONCEPT_ID" not in concepts_df.columns:
        concepts_df["ORIGINAL_CONCEPT_ID"] = concepts_df["CONCEPT_ID"]
    
    main_concepts = concepts_df[~concepts_df["IS_ORDINAL"]].copy()
    ordinal_concepts = concepts_df[concepts_df["IS_ORDINAL"] & (concepts_df["ORDINAL"] > 0)].copy()
    
    results = []
    
    # Process main concepts using vectorized groupby
    if not main_concepts.empty:
        # Filter data to patients with prevalence > 0
        patients_with_prevalence = data_patients[data_patients["PREVALENCE"] > 0].copy()
        
        if not patients_with_prevalence.empty:
            # Group by concept and heritage to count unique persons per cohort
            if has_heritage:
                grouped = patients_with_prevalence.groupby(
                    ["CONCEPT_ID", "HERITAGE", "COHORT_DEFINITION_ID"]
                )["PERSON_ID"].nunique().reset_index(name="patient_count")
            else:
                grouped = patients_with_prevalence.groupby(
                    ["CONCEPT_ID", "COHORT_DEFINITION_ID"]
                )["PERSON_ID"].nunique().reset_index(name="patient_count")
            
            # Pivot to get target and control counts
            if has_heritage:
                pivot = grouped.pivot_table(
                    index=["CONCEPT_ID", "HERITAGE"],
                    columns="COHORT_DEFINITION_ID",
                    values="patient_count",
                    fill_value=0
                ).reset_index()
            else:
                pivot = grouped.pivot_table(
                    index="CONCEPT_ID",
                    columns="COHORT_DEFINITION_ID",
                    values="patient_count",
                    fill_value=0
                ).reset_index()
            
            # Ensure columns exist
            if "target" not in pivot.columns:
                pivot["target"] = 0
            if "control" not in pivot.columns:
                pivot["control"] = 0
            
            # Calculate prevalences and ratio
            pivot["TARGET_SUBJECT_PREVALENCE"] = pivot["target"] / target_total
            pivot["CONTROL_PREVALENCE"] = pivot["control"] / control_total if control_total > 0 else 0
            
            # Calculate ratio with protection against division by zero
            pivot["PREVALENCE_DIFFERENCE_RATIO"] = pivot.apply(
                lambda row: row["TARGET_SUBJECT_PREVALENCE"] / row["CONTROL_PREVALENCE"] 
                if row["CONTROL_PREVALENCE"] > 0 
                else (100.0 if row["TARGET_SUBJECT_PREVALENCE"] > 0 else 0.0),
                axis=1
            )
            
            # Merge with main concepts to get result CONCEPT_IDs
            if has_heritage:
                main_metrics = main_concepts.merge(
                    pivot[["CONCEPT_ID", "HERITAGE", "TARGET_SUBJECT_PREVALENCE", "PREVALENCE_DIFFERENCE_RATIO"]],
                    left_on=["ORIGINAL_CONCEPT_ID", "HERITAGE"],
                    right_on=["CONCEPT_ID", "HERITAGE"],
                    suffixes=("", "_calc")
                )
                main_metrics = main_metrics[["CONCEPT_ID", "HERITAGE", "TARGET_SUBJECT_PREVALENCE", "PREVALENCE_DIFFERENCE_RATIO"]]
            else:
                main_metrics = main_concepts.merge(
                    pivot[["CONCEPT_ID", "TARGET_SUBJECT_PREVALENCE", "PREVALENCE_DIFFERENCE_RATIO"]],
                    left_on="ORIGINAL_CONCEPT_ID",
                    right_on="CONCEPT_ID",
                    suffixes=("", "_calc")
                )
                main_metrics["HERITAGE"] = main_concepts["HERITAGE"] if "HERITAGE" in main_concepts.columns else None
                main_metrics = main_metrics[["CONCEPT_ID", "HERITAGE", "TARGET_SUBJECT_PREVALENCE", "PREVALENCE_DIFFERENCE_RATIO"]]
            
            results.append(main_metrics)
    
    # Process ordinal concepts
    if not ordinal_concepts.empty:
        # Merge ordinal concepts with patient data
        if has_heritage:
            merged = data_patients.merge(
                ordinal_concepts[["CONCEPT_ID", "ORIGINAL_CONCEPT_ID", "ORDINAL", "HERITAGE"]],
                left_on=["CONCEPT_ID", "HERITAGE"],
                right_on=["ORIGINAL_CONCEPT_ID", "HERITAGE"],
                suffixes=("_patient", "_ordinal")
            )
            merged["RESULT_CONCEPT_ID"] = merged["CONCEPT_ID_ordinal"]
        else:
            merged = data_patients.merge(
                ordinal_concepts[["CONCEPT_ID", "ORIGINAL_CONCEPT_ID", "ORDINAL"]],
                left_on="CONCEPT_ID",
                right_on="ORIGINAL_CONCEPT_ID",
                suffixes=("_patient", "_ordinal")
            )
            merged["RESULT_CONCEPT_ID"] = merged["CONCEPT_ID_ordinal"]
            merged["HERITAGE"] = ordinal_concepts["HERITAGE"].iloc[0] if "HERITAGE" in ordinal_concepts.columns else None
        
        if not merged.empty:
            # Filter to patients with enough occurrences
            merged = merged[merged["NUM_OCCURRENCES"] >= merged["ORDINAL"]].copy()
            
            if not merged.empty:
                # Group by ordinal concept and cohort
                if has_heritage:
                    grouped = merged.groupby(
                        ["RESULT_CONCEPT_ID", "HERITAGE", "COHORT_DEFINITION_ID"]
                    )["PERSON_ID"].nunique().reset_index(name="patient_count")
                    
                    pivot = grouped.pivot_table(
                        index=["RESULT_CONCEPT_ID", "HERITAGE"],
                        columns="COHORT_DEFINITION_ID",
                        values="patient_count",
                        fill_value=0
                    ).reset_index()
                else:
                    grouped = merged.groupby(
                        ["RESULT_CONCEPT_ID", "COHORT_DEFINITION_ID"]
                    )["PERSON_ID"].nunique().reset_index(name="patient_count")
                    
                    pivot = grouped.pivot_table(
                        index="RESULT_CONCEPT_ID",
                        columns="COHORT_DEFINITION_ID",
                        values="patient_count",
                        fill_value=0
                    ).reset_index()
                
                # Ensure columns exist
                if "target" not in pivot.columns:
                    pivot["target"] = 0
                if "control" not in pivot.columns:
                    pivot["control"] = 0
                
                # Calculate prevalences and ratio
                pivot["TARGET_SUBJECT_PREVALENCE"] = pivot["target"] / target_total
                pivot["CONTROL_PREVALENCE"] = pivot["control"] / control_total if control_total > 0 else 0
                
                pivot["PREVALENCE_DIFFERENCE_RATIO"] = pivot.apply(
                    lambda row: row["TARGET_SUBJECT_PREVALENCE"] / row["CONTROL_PREVALENCE"] 
                    if row["CONTROL_PREVALENCE"] > 0 
                    else (100.0 if row["TARGET_SUBJECT_PREVALENCE"] > 0 else 0.0),
                    axis=1
                )
                
                pivot = pivot.rename(columns={"RESULT_CONCEPT_ID": "CONCEPT_ID"})
                if has_heritage:
                    ordinal_metrics = pivot[["CONCEPT_ID", "HERITAGE", "TARGET_SUBJECT_PREVALENCE", "PREVALENCE_DIFFERENCE_RATIO"]]
                else:
                    ordinal_metrics = pivot[["CONCEPT_ID", "TARGET_SUBJECT_PREVALENCE", "PREVALENCE_DIFFERENCE_RATIO"]]
                    ordinal_metrics["HERITAGE"] = None
                
                results.append(ordinal_metrics)
    
    if results:
        return pd.concat(results, ignore_index=True)
    
    return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "TARGET_SUBJECT_PREVALENCE", "PREVALENCE_DIFFERENCE_RATIO"])


def create_ordinal_concepts(data_patients: pd.DataFrame, df_features: pd.DataFrame) -> pd.DataFrame:
    """
    Create ordinal concept variants (1st, 2nd, 3rd, etc.) for concepts where people have multiple occurrences.
    
    Optimized version using vectorized operations.
    
    For each concept, checks if at least 50% of people have a 2nd occurrence, then if at least 50% of those
    have a 3rd occurrence, etc. Creates subrows for each ordinal that meets the threshold.
    
    Args:
        data_patients: DataFrame with COHORT_DEFINITION_ID, PERSON_ID, CONCEPT_ID, HERITAGE, TIME_TO_EVENT
        df_features: Original features DataFrame with CONCEPT_ID, CONCEPT_NAME, HERITAGE, etc.
        
    Returns:
        Extended DataFrame with main concepts and ordinal subrows, including linking columns
    """
    # Filter for target cohort only
    df_target = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
    
    if df_target.empty:
        df_features = df_features.copy()
        df_features["IS_ORDINAL"] = False
        df_features["ORIGINAL_CONCEPT_ID"] = df_features["CONCEPT_ID"]
        df_features["ORDINAL"] = 0
        return df_features
    
    # Extract occurrence count for each row (vectorized)
    df_target["NUM_OCCURRENCES"] = df_target["TIME_TO_EVENT"].apply(
        lambda x: len(get_unique_occurrences(x))
    )
    
    # Filter out rows with no occurrences
    df_target = df_target[df_target["NUM_OCCURRENCES"] > 0].copy()
    
    if df_target.empty:
        df_features = df_features.copy()
        df_features["IS_ORDINAL"] = False
        df_features["ORIGINAL_CONCEPT_ID"] = df_features["CONCEPT_ID"]
        df_features["ORDINAL"] = 0
        return df_features
    
    # Determine grouping columns
    has_heritage = "HERITAGE" in df_target.columns
    grouping_cols = ["CONCEPT_ID", "HERITAGE"] if has_heritage else ["CONCEPT_ID"]
    
    # Calculate occurrence statistics per concept using vectorized groupby
    # For each concept, count how many people have >= N occurrences for N = 1, 2, 3, ...
    def calculate_max_ordinal(group):
        """Calculate max ordinal for a concept group using 50% threshold."""
        num_people = len(group)
        occurrence_counts = group["NUM_OCCURRENCES"].value_counts().to_dict()
        
        max_ordinal = 0
        people_with_prev = num_people
        
        for ordinal in range(1, 11):
            # Count people with >= (ordinal + 1) occurrences
            people_with_nth_plus = sum(
                count for n, count in occurrence_counts.items() 
                if n >= (ordinal + 1)
            )
            
            threshold = people_with_prev * 0.5
            if people_with_nth_plus >= threshold:
                max_ordinal = ordinal
                people_with_prev = people_with_nth_plus
            else:
                break
        
        return max_ordinal
    
    # Get max ordinal for each concept
    ordinal_stats = df_target.groupby(grouping_cols).apply(calculate_max_ordinal).reset_index()
    ordinal_stats.columns = list(grouping_cols) + ["MAX_ORDINAL"]
    
    # Filter to concepts that need ordinal variants (max_ordinal > 1)
    concepts_with_ordinals = ordinal_stats[ordinal_stats["MAX_ORDINAL"] > 1].copy()
    
    # Build ordinal concept rows
    ordinal_concepts = []
    
    if not concepts_with_ordinals.empty:
        # Merge with df_features to get concept names and other attributes
        if has_heritage:
            features_lookup = df_features.set_index(["CONCEPT_ID", "HERITAGE"])
        else:
            features_lookup = df_features.set_index("CONCEPT_ID")
        
        for _, row in concepts_with_ordinals.iterrows():
            concept_id = row["CONCEPT_ID"]
            heritage = row["HERITAGE"] if has_heritage else None
            max_ordinal = int(row["MAX_ORDINAL"])
            
            # Get feature row
            try:
                if has_heritage:
                    main_row_dict = features_lookup.loc[(concept_id, heritage)].to_dict()
                else:
                    main_row_dict = features_lookup.loc[concept_id].to_dict()
            except KeyError:
                continue
            
            concept_name = main_row_dict.get("CONCEPT_NAME", f"Concept {concept_id}")
            
            # Create ordinal variants
            for ordinal in range(1, max_ordinal + 1):
                ordinal_suffix = {1: "st", 2: "nd", 3: "rd"}.get(ordinal, "th")
                ordinal_name = f"{concept_name} {ordinal}{ordinal_suffix}"
                
                ordinal_row = main_row_dict.copy()
                ordinal_row["CONCEPT_ID"] = int(concept_id) * 1000 + ordinal
                ordinal_row["CONCEPT_NAME"] = ordinal_name
                ordinal_row["ORIGINAL_CONCEPT_ID"] = concept_id
                ordinal_row["ORDINAL"] = ordinal
                ordinal_row["IS_ORDINAL"] = True
                if has_heritage:
                    ordinal_row["HERITAGE"] = heritage
                
                ordinal_concepts.append(ordinal_row)
    
    # Add IS_ORDINAL=False to main concepts
    df_features = df_features.copy()
    df_features["IS_ORDINAL"] = False
    df_features["ORIGINAL_CONCEPT_ID"] = df_features["CONCEPT_ID"]
    df_features["ORDINAL"] = 0
    
    # Combine main concepts with ordinal variants
    if ordinal_concepts:
        df_ordinals = pd.DataFrame(ordinal_concepts)
        # Ensure all columns match
        for col in df_features.columns:
            if col not in df_ordinals.columns:
                df_ordinals[col] = None
        
        # Reorder columns to match df_features
        df_ordinals = df_ordinals[df_features.columns]
        df_extended = pd.concat([df_features, df_ordinals], ignore_index=True)
    else:
        df_extended = df_features
    
    return df_extended
