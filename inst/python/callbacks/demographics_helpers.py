"""Helpers for demographics callbacks in summary and patient modes."""

from typing import Dict, List, Optional, Tuple

import numpy as np
import pandas as pd
import plotly.graph_objects as go
from dash import html

from data.loader import load_clustering_file
from utils.helpers import get_unique_occurrences, normalize_concept_id_series


_TARGET_COHORT_LABELS = {"target", "2"}
_CONTROL_COHORT_LABELS = {"control", "1"}


def _safe_float(value, default=None):
    """Safely convert a value to float."""
    try:
        if value is None or (isinstance(value, float) and np.isnan(value)):
            return default
        return float(value)
    except (TypeError, ValueError):
        return default


def _safe_int(value, default=None):
    """Safely convert a value to int."""
    try:
        if value is None or (isinstance(value, float) and np.isnan(value)):
            return default
        return int(value)
    except (TypeError, ValueError):
        return default


def _create_demographics_message_figure(message: str) -> go.Figure:
    """Create an empty placeholder figure with a centered message."""
    fig = go.Figure()
    fig.add_annotation(
        x=0.5,
        y=0.5,
        xref="paper",
        yref="paper",
        text=message,
        showarrow=False,
        font=dict(size=13, color="#666"),
        align="center"
    )
    fig.update_layout(
        height=320,
        margin=dict(t=30, b=30, l=30, r=30),
        plot_bgcolor="white",
        paper_bgcolor="white",
        xaxis=dict(visible=False),
        yaxis=dict(visible=False)
    )
    return fig


def _extract_active_k(metadata: Dict, clustering_results: Optional[Dict]) -> Tuple[Optional[int], Optional[float]]:
    """Get currently selected/optimal k and silhouette score."""
    k_value = None
    silhouette = None
    
    if isinstance(clustering_results, dict):
        k_value = _safe_int(clustering_results.get("optimal_cluster_count"))
        silhouette = _safe_float(clustering_results.get("best_silhouette_score"))
    
    clustering_meta = metadata.get("clustering", {}) if isinstance(metadata, dict) else {}
    if k_value is None and isinstance(clustering_meta, dict) and clustering_meta:
        best = max(
            clustering_meta.items(),
            key=lambda item: _safe_float((item[1] or {}).get("silhouette_score"), default=-1)
        )
        k_value = _safe_int(best[0])
    
    if silhouette is None and k_value is not None and isinstance(clustering_meta, dict):
        k_meta = clustering_meta.get(str(k_value), {})
        silhouette = _safe_float((k_meta or {}).get("silhouette_score"))
    
    return k_value, silhouette


def _to_dataframe(value) -> pd.DataFrame:
    """Convert list/dict dataframe payloads into a pandas DataFrame."""
    if isinstance(value, pd.DataFrame):
        return value.copy()
    if isinstance(value, list):
        return pd.DataFrame(value) if value else pd.DataFrame()
    return pd.DataFrame()


def _get_target_control_counts(parquet_data: Dict) -> Tuple[int, int]:
    """Return target/control cohort counts using data_initial when available."""
    data_initial = parquet_data.get("data_initial", pd.DataFrame())
    if isinstance(data_initial, pd.DataFrame) and not data_initial.empty:
        cohort_col = data_initial.get("COHORT_DEFINITION_ID")
        subject_col = "SUBJECT_ID" if "SUBJECT_ID" in data_initial.columns else ("PERSON_ID" if "PERSON_ID" in data_initial.columns else None)
        if cohort_col is not None and subject_col is not None:
            cohort_norm = cohort_col.astype(str).str.strip().str.lower()
            target_n = int(data_initial.loc[cohort_norm.isin(_TARGET_COHORT_LABELS), subject_col].nunique())
            control_n = int(data_initial.loc[cohort_norm.isin(_CONTROL_COHORT_LABELS), subject_col].nunique())
            return target_n, control_n
    
    data_patients = parquet_data.get("data_patients", pd.DataFrame())
    if isinstance(data_patients, pd.DataFrame) and not data_patients.empty:
        cohort_col = data_patients.get("COHORT_DEFINITION_ID")
        if cohort_col is not None and "PERSON_ID" in data_patients.columns:
            cohort_norm = cohort_col.astype(str).str.strip().str.lower()
            target_n = int(data_patients.loc[cohort_norm.isin(_TARGET_COHORT_LABELS), "PERSON_ID"].nunique())
            control_n = int(data_patients.loc[cohort_norm.isin(_CONTROL_COHORT_LABELS), "PERSON_ID"].nunique())
            return target_n, control_n
    
    return 0, 0


def _extract_target_rows(data_patients: pd.DataFrame) -> pd.DataFrame:
    """Extract target-cohort patient rows with normalized concept ids."""
    if data_patients.empty:
        return pd.DataFrame()
    if "COHORT_DEFINITION_ID" not in data_patients.columns:
        target_rows = data_patients.copy()
    else:
        cohort_norm = data_patients["COHORT_DEFINITION_ID"].astype(str).str.strip().str.lower()
        target_rows = data_patients.loc[cohort_norm.isin(_TARGET_COHORT_LABELS)].copy()
    
    if target_rows.empty:
        return pd.DataFrame()
    
    if "CONCEPT_ID" in target_rows.columns:
        target_rows["CONCEPT_ID_NORM"] = normalize_concept_id_series(target_rows["CONCEPT_ID"])
        target_rows = target_rows[target_rows["CONCEPT_ID_NORM"] != ""].copy()
    
    return target_rows


def _build_concept_name_lookup(parquet_data: Dict) -> Dict[str, str]:
    """Build concept_id -> concept_name lookup from loaded study artifacts."""
    lookup: Dict[str, str] = {}
    
    data_features = parquet_data.get("data_features", pd.DataFrame())
    if isinstance(data_features, pd.DataFrame) and not data_features.empty:
        if "CONCEPT_ID" in data_features.columns and "CONCEPT_NAME" in data_features.columns:
            tmp = data_features[["CONCEPT_ID", "CONCEPT_NAME"]].dropna(subset=["CONCEPT_ID"]).copy()
            tmp["CONCEPT_ID_NORM"] = normalize_concept_id_series(tmp["CONCEPT_ID"])
            tmp = tmp[tmp["CONCEPT_ID_NORM"] != ""].drop_duplicates("CONCEPT_ID_NORM")
            for _, row in tmp.iterrows():
                lookup[str(row["CONCEPT_ID_NORM"])] = str(row.get("CONCEPT_NAME") or row["CONCEPT_ID_NORM"])
    
    concept_summaries = parquet_data.get("concept_summaries", pd.DataFrame())
    if isinstance(concept_summaries, pd.DataFrame) and not concept_summaries.empty:
        if "CONCEPT_ID" in concept_summaries.columns and "CONCEPT_NAME" in concept_summaries.columns:
            tmp = concept_summaries[["CONCEPT_ID", "CONCEPT_NAME"]].dropna(subset=["CONCEPT_ID"]).copy()
            tmp["CONCEPT_ID_NORM"] = normalize_concept_id_series(tmp["CONCEPT_ID"])
            tmp = tmp[tmp["CONCEPT_ID_NORM"] != ""].drop_duplicates("CONCEPT_ID_NORM")
            for _, row in tmp.iterrows():
                lookup.setdefault(str(row["CONCEPT_ID_NORM"]), str(row.get("CONCEPT_NAME") or row["CONCEPT_ID_NORM"]))
    
    return lookup


def _build_patient_demographics_table(
    parquet_data: Dict,
    target_patient_ids: set
) -> pd.DataFrame:
    """Create one row per target patient with age and gender fields when available."""
    data_person = parquet_data.get("data_person", pd.DataFrame())
    if not isinstance(data_person, pd.DataFrame) or data_person.empty or "PERSON_ID" not in data_person.columns:
        return pd.DataFrame(columns=["PERSON_ID", "AGE", "GENDER_CONCEPT_ID"])
    
    person_demo = data_person[data_person["PERSON_ID"].isin(target_patient_ids)].copy()
    if person_demo.empty:
        return pd.DataFrame(columns=["PERSON_ID", "AGE", "GENDER_CONCEPT_ID"])
    
    if "GENDER_CONCEPT_ID" not in person_demo.columns:
        person_demo["GENDER_CONCEPT_ID"] = np.nan
    
    person_demo["AGE"] = np.nan
    data_initial = parquet_data.get("data_initial", pd.DataFrame())
    if isinstance(data_initial, pd.DataFrame) and not data_initial.empty:
        if "SUBJECT_ID" in data_initial.columns and "COHORT_START_DATE" in data_initial.columns:
            if "COHORT_DEFINITION_ID" in data_initial.columns:
                cohort_norm = data_initial["COHORT_DEFINITION_ID"].astype(str).str.strip().str.lower()
                target_initial = data_initial.loc[cohort_norm.isin(_TARGET_COHORT_LABELS), ["SUBJECT_ID", "COHORT_START_DATE"]].copy()
            else:
                target_initial = data_initial[["SUBJECT_ID", "COHORT_START_DATE"]].copy()
            if not target_initial.empty:
                target_initial["COHORT_START_DATE"] = pd.to_datetime(target_initial["COHORT_START_DATE"], errors="coerce")
                target_initial = target_initial.dropna(subset=["COHORT_START_DATE"])
                target_initial = target_initial.sort_values("COHORT_START_DATE").drop_duplicates("SUBJECT_ID", keep="first")
                person_demo = person_demo.merge(
                    target_initial,
                    left_on="PERSON_ID",
                    right_on="SUBJECT_ID",
                    how="left"
                )
                if "YEAR_OF_BIRTH" in person_demo.columns:
                    person_demo["AGE"] = person_demo["COHORT_START_DATE"].dt.year - pd.to_numeric(
                        person_demo["YEAR_OF_BIRTH"], errors="coerce"
                    )
                    person_demo.loc[(person_demo["AGE"] < 0) | (person_demo["AGE"] > 150), "AGE"] = np.nan
    
    out_cols = ["PERSON_ID", "AGE", "GENDER_CONCEPT_ID"]
    for col in out_cols:
        if col not in person_demo.columns:
            person_demo[col] = np.nan
    person_demo = person_demo[out_cols].drop_duplicates("PERSON_ID", keep="first")
    return person_demo


def _build_age_histogram(age_values: pd.Series, bin_width: int = 5) -> Dict:
    """Build age histogram metadata compatible with summary-mode renderers."""
    ages = pd.to_numeric(age_values, errors="coerce").dropna()
    if ages.empty:
        return {"bins": [], "counts": []}
    
    min_age = int(np.floor(ages.min() / bin_width) * bin_width)
    max_age = int(np.ceil(ages.max() / bin_width) * bin_width)
    if max_age <= min_age:
        max_age = min_age + bin_width
    
    edges = np.arange(min_age, max_age + bin_width, bin_width)
    if len(edges) < 2:
        edges = np.array([min_age, min_age + bin_width])
    
    counts, used_edges = np.histogram(ages.values, bins=edges)
    labels = [
        f"{int(used_edges[idx])}-{int(used_edges[idx + 1] - 1)}"
        for idx in range(len(counts))
    ]
    
    return {
        "bins": labels,
        "counts": counts.astype(int).tolist(),
        "mean": float(ages.mean()),
        "median": float(ages.median()),
        "q1": float(ages.quantile(0.25)),
        "q3": float(ages.quantile(0.75)),
        "min": float(ages.min()),
        "max": float(ages.max())
    }


def _ordinal_suffix(value: int) -> str:
    """Return ordinal suffix for positive integer values."""
    if 10 <= value % 100 <= 20:
        return "th"
    return {1: "st", 2: "nd", 3: "rd"}.get(value % 10, "th")


def _build_patient_main_pairs(target_rows: pd.DataFrame) -> pd.DataFrame:
    """Build unique (patient, concept) rows for target concepts."""
    if target_rows.empty or "PERSON_ID" not in target_rows.columns or "CONCEPT_ID_NORM" not in target_rows.columns:
        return pd.DataFrame(columns=["PERSON_ID", "CONCEPT_ID_NORM"])
    
    source = target_rows
    if "PREVALENCE" in source.columns:
        prevalence_num = pd.to_numeric(source["PREVALENCE"], errors="coerce").fillna(0)
        source = source.loc[prevalence_num > 0].copy()
    
    if source.empty:
        return pd.DataFrame(columns=["PERSON_ID", "CONCEPT_ID_NORM"])
    
    pairs = source[["PERSON_ID", "CONCEPT_ID_NORM"]].dropna(subset=["PERSON_ID", "CONCEPT_ID_NORM"]).copy()
    pairs = pairs.drop_duplicates(["PERSON_ID", "CONCEPT_ID_NORM"])
    return pairs


def _build_patient_ordinal_membership(target_rows: pd.DataFrame, max_ordinal: int = 10) -> pd.DataFrame:
    """Build per-patient ordinal membership rows (1st/2nd/... occurrence)."""
    required_cols = {"PERSON_ID", "CONCEPT_ID_NORM"}
    if target_rows.empty or not required_cols.issubset(set(target_rows.columns)):
        return pd.DataFrame(columns=["PERSON_ID", "ORIGINAL_CONCEPT_ID", "ORDINAL"])
    
    working = target_rows[["PERSON_ID", "CONCEPT_ID_NORM"]].copy()
    if "TIME_TO_EVENT" in target_rows.columns:
        working["occurrence_count"] = target_rows["TIME_TO_EVENT"].apply(lambda x: len(get_unique_occurrences(x)))
    elif "PREVALENCE" in target_rows.columns:
        working["occurrence_count"] = pd.to_numeric(target_rows["PREVALENCE"], errors="coerce").fillna(0).astype(int)
    else:
        working["occurrence_count"] = 1
    
    working = working.dropna(subset=["PERSON_ID", "CONCEPT_ID_NORM"])
    if working.empty:
        return pd.DataFrame(columns=["PERSON_ID", "ORIGINAL_CONCEPT_ID", "ORDINAL"])
    
    per_patient = working.groupby(["PERSON_ID", "CONCEPT_ID_NORM"], as_index=False)["occurrence_count"].max()
    per_patient["occurrence_count"] = per_patient["occurrence_count"].clip(lower=0, upper=max_ordinal).astype(int)
    per_patient = per_patient[per_patient["occurrence_count"] > 0].copy()
    if per_patient.empty:
        return pd.DataFrame(columns=["PERSON_ID", "ORIGINAL_CONCEPT_ID", "ORDINAL"])
    
    per_patient["ORDINAL"] = per_patient["occurrence_count"].apply(lambda n: list(range(1, int(n) + 1)))
    ordinal_df = per_patient.explode("ORDINAL")
    ordinal_df = ordinal_df.rename(columns={"CONCEPT_ID_NORM": "ORIGINAL_CONCEPT_ID"})
    ordinal_df["ORDINAL"] = pd.to_numeric(ordinal_df["ORDINAL"], errors="coerce")
    ordinal_df = ordinal_df.dropna(subset=["ORDINAL"])
    ordinal_df["ORDINAL"] = ordinal_df["ORDINAL"].astype(int)
    ordinal_df = ordinal_df[["PERSON_ID", "ORIGINAL_CONCEPT_ID", "ORDINAL"]].drop_duplicates()
    return ordinal_df


def _build_overall_concept_demographics(
    concept_pairs: pd.DataFrame,
    person_demo: pd.DataFrame,
    concept_name_lookup: Dict[str, str]
) -> pd.DataFrame:
    """Compute concept-level demographics from patient-level concept pairs."""
    if concept_pairs.empty:
        return pd.DataFrame()
    
    concept_demo = concept_pairs.merge(
        person_demo[["PERSON_ID", "AGE", "GENDER_CONCEPT_ID"]],
        on="PERSON_ID",
        how="left"
    )
    concept_demo["_male"] = (concept_demo["GENDER_CONCEPT_ID"] == 8507).astype(int)
    concept_demo["_has_gender"] = concept_demo["GENDER_CONCEPT_ID"].notna().astype(int)
    
    aggregated = concept_demo.groupby("CONCEPT_ID_NORM", as_index=False).agg(
        patient_count=("PERSON_ID", "nunique"),
        age_mean=("AGE", "mean"),
        male_count=("_male", "sum"),
        gender_n=("_has_gender", "sum")
    )
    aggregated["male_proportion"] = np.where(
        aggregated["gender_n"] > 0,
        aggregated["male_count"] / aggregated["gender_n"],
        np.nan
    )
    aggregated["CONCEPT_ID"] = aggregated["CONCEPT_ID_NORM"]
    aggregated["CONCEPT_NAME"] = aggregated["CONCEPT_ID_NORM"].map(concept_name_lookup).fillna(aggregated["CONCEPT_ID_NORM"])
    aggregated["ORIGINAL_CONCEPT_ID"] = aggregated["CONCEPT_ID_NORM"]
    aggregated["ORDINAL"] = 0
    aggregated["IS_ORDINAL"] = False
    out = aggregated[
        ["CONCEPT_ID", "CONCEPT_NAME", "patient_count", "age_mean", "male_proportion", "ORIGINAL_CONCEPT_ID", "ORDINAL", "IS_ORDINAL"]
    ].copy()
    return out


def _build_overall_ordinal_demographics(
    ordinal_membership: pd.DataFrame,
    person_demo: pd.DataFrame,
    concept_name_lookup: Dict[str, str]
) -> pd.DataFrame:
    """Compute ordinal concept demographics from patient-level ordinal membership."""
    if ordinal_membership.empty:
        return pd.DataFrame()
    
    ordinal_demo = ordinal_membership.merge(
        person_demo[["PERSON_ID", "AGE", "GENDER_CONCEPT_ID"]],
        on="PERSON_ID",
        how="left"
    )
    ordinal_demo["_male"] = (ordinal_demo["GENDER_CONCEPT_ID"] == 8507).astype(int)
    ordinal_demo["_has_gender"] = ordinal_demo["GENDER_CONCEPT_ID"].notna().astype(int)
    
    aggregated = ordinal_demo.groupby(["ORIGINAL_CONCEPT_ID", "ORDINAL"], as_index=False).agg(
        patient_count=("PERSON_ID", "nunique"),
        age_mean=("AGE", "mean"),
        male_count=("_male", "sum"),
        gender_n=("_has_gender", "sum")
    )
    aggregated["male_proportion"] = np.where(
        aggregated["gender_n"] > 0,
        aggregated["male_count"] / aggregated["gender_n"],
        np.nan
    )
    aggregated["ORDINAL"] = aggregated["ORDINAL"].astype(int)
    aggregated["CONCEPT_ID"] = (
        aggregated["ORIGINAL_CONCEPT_ID"].astype(str)
        + "_"
        + aggregated["ORDINAL"].astype(str)
    )
    
    base_names = aggregated["ORIGINAL_CONCEPT_ID"].astype(str).map(concept_name_lookup).fillna(
        aggregated["ORIGINAL_CONCEPT_ID"].astype(str)
    )
    suffixes = aggregated["ORDINAL"].apply(lambda n: f"{int(n)}{_ordinal_suffix(int(n))}")
    aggregated["CONCEPT_NAME"] = base_names + " " + suffixes
    aggregated["IS_ORDINAL"] = True
    
    out = aggregated[
        ["CONCEPT_ID", "CONCEPT_NAME", "ORIGINAL_CONCEPT_ID", "ORDINAL", "IS_ORDINAL", "patient_count", "age_mean", "male_proportion"]
    ].copy()
    return out


def _extract_patient_assignments(clustering_results: Optional[Dict]) -> pd.DataFrame:
    """Extract patient assignments from clustering results store payload."""
    if not isinstance(clustering_results, dict):
        return pd.DataFrame(columns=["patient_id", "cluster"])
    
    assignments = _to_dataframe(clustering_results.get("patient_assignments", []))
    if assignments.empty:
        return pd.DataFrame(columns=["patient_id", "cluster"])
    
    if "patient_id" not in assignments.columns:
        if "PERSON_ID" in assignments.columns:
            assignments = assignments.rename(columns={"PERSON_ID": "patient_id"})
    
    if "patient_id" not in assignments.columns or "cluster" not in assignments.columns:
        return pd.DataFrame(columns=["patient_id", "cluster"])
    
    assignments = assignments[["patient_id", "cluster"]].copy()
    assignments["patient_id"] = pd.to_numeric(assignments["patient_id"], errors="coerce")
    assignments = assignments.dropna(subset=["patient_id", "cluster"])
    assignments["patient_id"] = assignments["patient_id"].astype(int)
    assignments["cluster"] = assignments["cluster"].astype(str)
    assignments = assignments.drop_duplicates(["patient_id", "cluster"])
    return assignments


def _build_patient_cluster_demographics(
    assignments: pd.DataFrame,
    person_demo: pd.DataFrame
) -> Dict[str, Dict]:
    """Build cluster-level demographic stats dictionary."""
    if assignments.empty:
        return {}
    
    cluster_counts = assignments.groupby("cluster")["patient_id"].nunique().to_dict()
    clusters: Dict[str, Dict] = {}
    
    if person_demo.empty:
        for cluster_label, n_value in cluster_counts.items():
            clusters[str(cluster_label)] = {"patient_count": int(n_value), "n": int(n_value)}
        return clusters
    
    cluster_demo = assignments.merge(
        person_demo[["PERSON_ID", "AGE", "GENDER_CONCEPT_ID"]],
        left_on="patient_id",
        right_on="PERSON_ID",
        how="left"
    )
    
    for cluster_label, cluster_df in cluster_demo.groupby("cluster"):
        ages = pd.to_numeric(cluster_df.get("AGE"), errors="coerce").dropna()
        male_count = int((cluster_df.get("GENDER_CONCEPT_ID") == 8507).sum())
        female_count = int((cluster_df.get("GENDER_CONCEPT_ID") == 8532).sum())
        n_value = int(cluster_counts.get(cluster_label, cluster_df["patient_id"].nunique()))
        other_count = max(n_value - male_count - female_count, 0)
        
        row: Dict[str, object] = {
            "patient_count": n_value,
            "n": n_value,
            "male_proportion": (male_count / n_value) if n_value else None,
            "male_count": male_count,
            "female_count": female_count,
            "other_count": other_count
        }
        if not ages.empty:
            row["age_mean"] = float(ages.mean())
            row["age_median"] = float(ages.median())
            row["age_q1"] = float(ages.quantile(0.25))
            row["age_q3"] = float(ages.quantile(0.75))
        clusters[str(cluster_label)] = row
    
    return clusters


def _build_cluster_concept_demographics(
    concept_pairs: pd.DataFrame,
    ordinal_membership: pd.DataFrame,
    assignments: pd.DataFrame,
    person_demo: pd.DataFrame,
    concept_name_lookup: Dict[str, str]
) -> pd.DataFrame:
    """Build concept-level demographics by cluster for patient mode."""
    if assignments.empty:
        return pd.DataFrame()
    
    cluster_totals = assignments.groupby("cluster")["patient_id"].nunique().rename("total_cluster_patients")
    frames: List[pd.DataFrame] = []
    
    if not concept_pairs.empty:
        main_cluster = concept_pairs.merge(
            assignments,
            left_on="PERSON_ID",
            right_on="patient_id",
            how="inner"
        ).drop_duplicates(["PERSON_ID", "CONCEPT_ID_NORM", "cluster"])
        if not main_cluster.empty:
            main_cluster = main_cluster.merge(
                person_demo[["PERSON_ID", "AGE", "GENDER_CONCEPT_ID"]],
                on="PERSON_ID",
                how="left"
            )
            main_cluster["_male"] = (main_cluster["GENDER_CONCEPT_ID"] == 8507).astype(int)
            main_cluster["_has_gender"] = main_cluster["GENDER_CONCEPT_ID"].notna().astype(int)
            main_agg = main_cluster.groupby(["CONCEPT_ID_NORM", "cluster"], as_index=False).agg(
                patient_count=("PERSON_ID", "nunique"),
                age_mean=("AGE", "mean"),
                male_count=("_male", "sum"),
                gender_n=("_has_gender", "sum")
            )
            main_agg["male_proportion"] = np.where(
                main_agg["gender_n"] > 0,
                main_agg["male_count"] / main_agg["gender_n"],
                np.nan
            )
            main_agg["CONCEPT_ID"] = main_agg["CONCEPT_ID_NORM"]
            main_agg["CONCEPT_NAME"] = main_agg["CONCEPT_ID_NORM"].map(concept_name_lookup).fillna(main_agg["CONCEPT_ID_NORM"])
            main_agg["ORIGINAL_CONCEPT_ID"] = main_agg["CONCEPT_ID_NORM"]
            main_agg["ORDINAL"] = 0
            main_agg["IS_ORDINAL"] = False
            frames.append(main_agg[
                ["CONCEPT_ID", "CONCEPT_NAME", "cluster", "patient_count", "age_mean", "male_proportion", "ORIGINAL_CONCEPT_ID", "ORDINAL", "IS_ORDINAL"]
            ])
    
    if not ordinal_membership.empty:
        ordinal_cluster = ordinal_membership.merge(
            assignments,
            left_on="PERSON_ID",
            right_on="patient_id",
            how="inner"
        ).drop_duplicates(["PERSON_ID", "ORIGINAL_CONCEPT_ID", "ORDINAL", "cluster"])
        if not ordinal_cluster.empty:
            ordinal_cluster = ordinal_cluster.merge(
                person_demo[["PERSON_ID", "AGE", "GENDER_CONCEPT_ID"]],
                on="PERSON_ID",
                how="left"
            )
            ordinal_cluster["_male"] = (ordinal_cluster["GENDER_CONCEPT_ID"] == 8507).astype(int)
            ordinal_cluster["_has_gender"] = ordinal_cluster["GENDER_CONCEPT_ID"].notna().astype(int)
            ordinal_agg = ordinal_cluster.groupby(["ORIGINAL_CONCEPT_ID", "ORDINAL", "cluster"], as_index=False).agg(
                patient_count=("PERSON_ID", "nunique"),
                age_mean=("AGE", "mean"),
                male_count=("_male", "sum"),
                gender_n=("_has_gender", "sum")
            )
            ordinal_agg["male_proportion"] = np.where(
                ordinal_agg["gender_n"] > 0,
                ordinal_agg["male_count"] / ordinal_agg["gender_n"],
                np.nan
            )
            ordinal_agg["ORDINAL"] = ordinal_agg["ORDINAL"].astype(int)
            ordinal_agg["CONCEPT_ID"] = (
                ordinal_agg["ORIGINAL_CONCEPT_ID"].astype(str)
                + "_"
                + ordinal_agg["ORDINAL"].astype(str)
            )
            base_names = ordinal_agg["ORIGINAL_CONCEPT_ID"].astype(str).map(concept_name_lookup).fillna(
                ordinal_agg["ORIGINAL_CONCEPT_ID"].astype(str)
            )
            suffixes = ordinal_agg["ORDINAL"].apply(lambda n: f"{int(n)}{_ordinal_suffix(int(n))}")
            ordinal_agg["CONCEPT_NAME"] = base_names + " " + suffixes
            ordinal_agg["IS_ORDINAL"] = True
            frames.append(ordinal_agg[
                ["CONCEPT_ID", "CONCEPT_NAME", "cluster", "patient_count", "age_mean", "male_proportion", "ORIGINAL_CONCEPT_ID", "ORDINAL", "IS_ORDINAL"]
            ])
    
    if not frames:
        return pd.DataFrame()
    
    cluster_summary_df = pd.concat(frames, ignore_index=True)
    cluster_summary_df = cluster_summary_df.merge(
        cluster_totals.reset_index(),
        on="cluster",
        how="left"
    )
    return cluster_summary_df


def _prepare_patient_base_demographics_context(parquet_data: Dict) -> Dict:
    """Compute and cache patient-mode demographics artifacts independent of clustering."""
    cached = parquet_data.get("_patient_demographics_cache")
    if isinstance(cached, dict):
        return cached
    
    data_patients = parquet_data.get("data_patients", pd.DataFrame())
    if not isinstance(data_patients, pd.DataFrame):
        data_patients = pd.DataFrame()
    
    target_n, control_n = _get_target_control_counts(parquet_data)
    target_rows = _extract_target_rows(data_patients)
    target_patient_ids = set(target_rows["PERSON_ID"].dropna().unique().tolist()) if "PERSON_ID" in target_rows.columns else set()
    
    person_demo = _build_patient_demographics_table(parquet_data, target_patient_ids)
    concept_name_lookup = _build_concept_name_lookup(parquet_data)
    
    age_hist = _build_age_histogram(person_demo.get("AGE", pd.Series(dtype=float)))
    male_n = int((person_demo.get("GENDER_CONCEPT_ID") == 8507).sum()) if not person_demo.empty else 0
    female_n = int((person_demo.get("GENDER_CONCEPT_ID") == 8532).sum()) if not person_demo.empty else 0
    sex_total = int(len(person_demo)) if not person_demo.empty else int(target_n)
    other_n = max(int(sex_total) - male_n - female_n, 0)
    male_prop = (male_n / sex_total) if sex_total > 0 else None
    
    demo = {
        "target_patients": int(target_n),
        "control_patients": int(control_n),
        "age_mean": _safe_float(age_hist.get("mean")),
        "age_median": _safe_float(age_hist.get("median")),
        "male_proportion": _safe_float(male_prop)
    }
    overall = {
        "age_histogram": age_hist,
        "sex_distribution": {
            "male": male_n,
            "female": female_n,
            "other": other_n,
            "total": sex_total
        }
    }
    
    concept_pairs = _build_patient_main_pairs(target_rows)
    concept_summaries = _build_overall_concept_demographics(concept_pairs, person_demo, concept_name_lookup)
    
    ordinal_membership = _build_patient_ordinal_membership(target_rows, max_ordinal=10)
    ordinal_summaries = _build_overall_ordinal_demographics(ordinal_membership, person_demo, concept_name_lookup)
    
    cached = {
        "metadata": parquet_data.get("_metadata", {}) or parquet_data.get("metadata", {}) or {},
        "demo": demo,
        "overall": overall,
        "person_demo": person_demo,
        "concept_name_lookup": concept_name_lookup,
        "concept_pairs": concept_pairs,
        "ordinal_membership": ordinal_membership,
        "concept_summaries": concept_summaries,
        "ordinal_summaries": ordinal_summaries
    }
    parquet_data["_patient_demographics_cache"] = cached
    
    # Keep these aligned with summary-mode keys so downstream helpers can stay generic.
    parquet_data["concept_summaries"] = concept_summaries
    parquet_data["ordinal_summaries"] = ordinal_summaries
    
    return cached


def _prepare_patient_demographics_context(
    parquet_data: Dict,
    clustering_results: Optional[Dict]
) -> Dict:
    """Collect patient-mode demographic data needed for rendering."""
    base = _prepare_patient_base_demographics_context(parquet_data)
    metadata = base.get("metadata", {}) if isinstance(base, dict) else {}
    
    assignments = _extract_patient_assignments(clustering_results)
    if not assignments.empty:
        valid_patients = set(base.get("person_demo", pd.DataFrame()).get("PERSON_ID", pd.Series(dtype=int)).tolist())
        if valid_patients:
            assignments = assignments[assignments["patient_id"].isin(valid_patients)].copy()
    
    clusters = _build_patient_cluster_demographics(assignments, base.get("person_demo", pd.DataFrame()))
    cluster_summary_df = _build_cluster_concept_demographics(
        base.get("concept_pairs", pd.DataFrame()),
        base.get("ordinal_membership", pd.DataFrame()),
        assignments,
        base.get("person_demo", pd.DataFrame()),
        base.get("concept_name_lookup", {})
    )
    
    k_value, silhouette = _extract_active_k(metadata, clustering_results)
    if k_value is None and not assignments.empty and "cluster" in assignments.columns:
        k_value = int(assignments["cluster"].nunique())
    
    return {
        "metadata": metadata,
        "demo": base.get("demo", {}),
        "overall": base.get("overall", {}),
        "clusters": clusters,
        "concept_summaries": base.get("concept_summaries", pd.DataFrame()),
        "ordinal_summaries": base.get("ordinal_summaries", pd.DataFrame()),
        "cluster_summary_df": cluster_summary_df if isinstance(cluster_summary_df, pd.DataFrame) else pd.DataFrame(),
        "k_value": k_value,
        "silhouette": silhouette
    }


def _prepare_summary_demographics_context(
    parquet_data: Dict,
    clustering_results: Optional[Dict]
) -> Dict:
    """Collect all summary-mode demographic data needed for rendering."""
    metadata = parquet_data.get("_metadata", {}) or parquet_data.get("metadata", {})
    demo = metadata.get("demographics", {}) if isinstance(metadata, dict) else {}
    distributions = demo.get("distributions", {}) if isinstance(demo, dict) else {}
    overall = distributions.get("overall", {}) if isinstance(distributions, dict) else {}
    clusters = distributions.get("clusters", {}) if isinstance(distributions, dict) else {}
    clusters_by_k = distributions.get("clusters_by_k", {}) if isinstance(distributions, dict) else {}
    
    concept_summaries = parquet_data.get("concept_summaries", pd.DataFrame())
    ordinal_summaries = parquet_data.get("ordinal_summaries", pd.DataFrame())
    
    k_value, silhouette = _extract_active_k(metadata, clustering_results)
    
    # Use selected-k demographics when available; fall back to legacy best-k "clusters".
    if k_value is not None and isinstance(clusters_by_k, dict):
        selected_clusters = clusters_by_k.get(str(k_value))
        if isinstance(selected_clusters, dict) and selected_clusters:
            clusters = selected_clusters
    
    cluster_summary_df = pd.DataFrame()
    if k_value is not None:
        summary_key = f"clustering_k{k_value}_summary"
        cluster_summary_df = parquet_data.get(summary_key, pd.DataFrame())
        if cluster_summary_df is None or cluster_summary_df.empty:
            disease_folder = parquet_data.get("_disease_folder")
            if disease_folder:
                loaded = load_clustering_file(disease_folder, k_value, "summary")
                if loaded is not None and not loaded.empty:
                    cluster_summary_df = loaded
                    parquet_data[summary_key] = loaded
    
    return {
        "metadata": metadata,
        "demo": demo,
        "overall": overall,
        "clusters": clusters if isinstance(clusters, dict) else {},
        "concept_summaries": concept_summaries if isinstance(concept_summaries, pd.DataFrame) else pd.DataFrame(),
        "ordinal_summaries": ordinal_summaries if isinstance(ordinal_summaries, pd.DataFrame) else pd.DataFrame(),
        "cluster_summary_df": cluster_summary_df if isinstance(cluster_summary_df, pd.DataFrame) else pd.DataFrame(),
        "k_value": k_value,
        "silhouette": silhouette
    }


def _prepare_demographics_context(
    parquet_data: Dict,
    clustering_results: Optional[Dict]
) -> Dict:
    """Build demographics context for either summary or patient mode."""
    actual_data_mode = parquet_data.get("_mode", "patient")
    if actual_data_mode == "summary":
        return _prepare_summary_demographics_context(parquet_data, clustering_results)
    return _prepare_patient_demographics_context(parquet_data, clustering_results)


def _build_demographic_kpi_cards(context: Dict) -> List[html.Div]:
    """Build top-level KPI cards for demographics tab."""
    demo = context.get("demo", {})
    overall = context.get("overall", {})
    age_hist = overall.get("age_histogram", {}) if isinstance(overall, dict) else {}
    sex = overall.get("sex_distribution", {}) if isinstance(overall, dict) else {}
    
    target_n = _safe_int(demo.get("target_patients"))
    control_n = _safe_int(demo.get("control_patients"))
    
    age_mean = _safe_float(demo.get("age_mean"))
    age_median = _safe_float(demo.get("age_median"))
    age_q1 = _safe_float(age_hist.get("q1"))
    age_q3 = _safe_float(age_hist.get("q3"))
    
    male_n = _safe_int(sex.get("male"), default=0)
    female_n = _safe_int(sex.get("female"), default=0)
    total_n = _safe_int(sex.get("total"), default=target_n or 0)
    other_n = _safe_int(sex.get("other"), default=max((total_n or 0) - male_n - female_n, 0))
    
    male_pct = (100.0 * male_n / total_n) if total_n else _safe_float(demo.get("male_proportion"), default=0) * 100
    female_pct = (100.0 * female_n / total_n) if total_n else max(0, 100 - male_pct)
    
    k_value = context.get("k_value")
    silhouette = context.get("silhouette")
    
    def _kpi_card(title: str, value: str, subtitle: str) -> html.Div:
        return html.Div(
            [
                html.Div(title, style={"fontSize": "12px", "color": "#6c757d", "marginBottom": "4px"}),
                html.Div(value, style={"fontSize": "22px", "fontWeight": "700", "color": "#2c3e50", "lineHeight": "1.2"}),
                html.Div(subtitle, style={"fontSize": "12px", "color": "#7a7a7a", "marginTop": "4px"})
            ],
            style={
                "flex": "1 1 180px",
                "minWidth": "180px",
                "backgroundColor": "#f8f9fa",
                "border": "1px solid #e9ecef",
                "borderRadius": "8px",
                "padding": "12px"
            }
        )
    
    cards = [
        _kpi_card("Target Cohort", f"{target_n:,}" if target_n is not None else "NA", "patients"),
        _kpi_card("Control Cohort", f"{control_n:,}" if control_n is not None else "NA", "patients"),
        _kpi_card(
            "Age (years)",
            f"{age_mean:.1f}" if age_mean is not None else "NA",
            f"median {age_median:.1f}" if age_median is not None else "median NA"
        ),
        _kpi_card(
            "Age IQR",
            f"{age_q1:.1f}-{age_q3:.1f}" if age_q1 is not None and age_q3 is not None else "NA",
            "25th-75th percentile"
        ),
        _kpi_card(
            "Sex Mix",
            f"{male_pct:.2f}% M / {female_pct:.2f}% F",
            f"Other: {other_n:,}"
        ),
        _kpi_card(
            "Clustering",
            f"k={k_value}" if k_value is not None else "k=NA",
            f"silhouette {silhouette:.3f}" if silhouette is not None else "silhouette NA"
        )
    ]
    return cards


def _create_summary_age_histogram_figure(context: Dict) -> go.Figure:
    """Create summary-mode age histogram figure."""
    overall = context.get("overall", {})
    age_hist = overall.get("age_histogram", {}) if isinstance(overall, dict) else {}
    bins = age_hist.get("bins", [])
    counts = age_hist.get("counts", [])
    
    if not bins or not counts:
        return _create_demographics_message_figure("Age histogram not available in summary files.")
    
    fig = go.Figure()
    fig.add_trace(
        go.Bar(
            x=bins,
            y=counts,
            name="Overall",
            marker_color="#4c78a8",
            hovertemplate="Age %{x}<br>Count %{y}<extra></extra>"
        )
    )
    
    age_mean = _safe_float(age_hist.get("mean"))
    age_median = _safe_float(age_hist.get("median"))
    age_q1 = _safe_float(age_hist.get("q1"))
    age_q3 = _safe_float(age_hist.get("q3"))
    summary_text = []
    if age_mean is not None:
        summary_text.append(f"Mean: {age_mean:.1f}")
    if age_median is not None:
        summary_text.append(f"Median: {age_median:.1f}")
    if age_q1 is not None and age_q3 is not None:
        summary_text.append(f"IQR: {age_q1:.1f}-{age_q3:.1f}")
    
    fig.update_layout(
        title="<b>Age Distribution (Overall)</b>",
        height=340,
        margin=dict(t=60, b=60, l=55, r=20),
        plot_bgcolor="white",
        paper_bgcolor="white",
        xaxis_title="Age bin",
        yaxis_title="Patients",
        bargap=0.06,
        annotations=[
            dict(
                text=" | ".join(summary_text),
                xref="paper",
                yref="paper",
                x=1,
                y=1.16,
                xanchor="right",
                showarrow=False,
                font=dict(size=11, color="#666")
            )
        ] if summary_text else []
    )
    fig.update_yaxes(gridcolor="#eee", autorange=True)
    fig.update_xaxes(showgrid=False, autorange=True)
    return fig


def _create_summary_sex_distribution_figure(context: Dict) -> go.Figure:
    """Create summary-mode sex distribution figure with cluster overlays."""
    overall = context.get("overall", {})
    clusters = context.get("clusters", {})
    sex = overall.get("sex_distribution", {}) if isinstance(overall, dict) else {}
    
    male = _safe_int(sex.get("male"), default=0)
    female = _safe_int(sex.get("female"), default=0)
    total = _safe_int(sex.get("total"), default=male + female)
    other = _safe_int(sex.get("other"), default=max(total - male - female, 0))
    
    if total <= 0:
        return _create_demographics_message_figure("Sex distribution not available in summary files.")
    
    categories = ["Male", "Female", "Other"]
    values = [100 * male / total, 100 * female / total, 100 * other / total]
    
    fig = go.Figure()
    fig.add_trace(
        go.Bar(
            x=categories,
            y=values,
            name="Overall",
            marker_color="#2c3e50",
            text=[f"{v:.2f}%<br>(n={n:,})" for v, n in zip(values, [male, female, other])],
            textposition="auto",
            hovertemplate="Overall<br>%{x}: %{y:.2f}%<extra></extra>"
        )
    )
    
    if isinstance(clusters, dict):
        for cluster_name in sorted(clusters.keys()):
            cluster_data = clusters.get(cluster_name, {})
            male_prop = _safe_float(cluster_data.get("male_proportion"))
            if male_prop is None:
                continue
            male_pct = 100 * male_prop
            female_pct = 100 * (1 - male_prop)
            fig.add_trace(
                go.Scatter(
                    x=["Male", "Female"],
                    y=[male_pct, female_pct],
                    mode="markers+text",
                    name=cluster_name,
                    marker=dict(size=11, symbol="diamond"),
                    text=[f"{male_pct:.1f}%", f"{female_pct:.1f}%"],
                    textposition="top center",
                    hovertemplate=f"{cluster_name}<br>%{{x}}: %{{y:.2f}}%<extra></extra>"
                )
            )
    
    fig.update_layout(
        title="<b>Sex Distribution</b>",
        height=340,
        margin=dict(t=60, b=60, l=55, r=20),
        plot_bgcolor="white",
        paper_bgcolor="white",
        yaxis_title="Percentage (%)",
        yaxis=dict(gridcolor="#eee", autorange=True),
        xaxis=dict(showgrid=False, autorange=True),
        legend=dict(orientation="h", yanchor="bottom", y=1.03, xanchor="left", x=0)
    )
    return fig


def _build_cluster_demographics_rows(context: Dict) -> List[Dict]:
    """Build per-cluster demographics table rows."""
    rows = []
    demo = context.get("demo", {})
    clusters = context.get("clusters", {})
    cluster_summary_df = context.get("cluster_summary_df", pd.DataFrame())
    overall_age_mean = _safe_float(demo.get("age_mean"), default=0.0)
    
    # Prefer active clustering summary so this table updates with selected k.
    if isinstance(cluster_summary_df, pd.DataFrame) and not cluster_summary_df.empty and "cluster" in cluster_summary_df.columns:
        for cluster_name in sorted(cluster_summary_df["cluster"].dropna().unique()):
            cluster_df = cluster_summary_df[cluster_summary_df["cluster"] == cluster_name].copy()
            if cluster_df.empty:
                continue
            n_value = None
            if "total_cluster_patients" in cluster_df.columns:
                n_value = _safe_int(cluster_df["total_cluster_patients"].iloc[0])
            age_mean = None
            if "age_mean" in cluster_df.columns and "patient_count" in cluster_df.columns:
                age_vals = pd.to_numeric(cluster_df["age_mean"], errors="coerce")
                weights = pd.to_numeric(cluster_df["patient_count"], errors="coerce").fillna(0)
                if weights.sum() > 0:
                    age_mean = float((age_vals.fillna(0) * weights).sum() / weights.sum())
            male_prop = None
            if "male_proportion" in cluster_df.columns and "patient_count" in cluster_df.columns:
                male_vals = pd.to_numeric(cluster_df["male_proportion"], errors="coerce")
                weights = pd.to_numeric(cluster_df["patient_count"], errors="coerce").fillna(0)
                if weights.sum() > 0:
                    male_prop = float((male_vals.fillna(0) * weights).sum() / weights.sum())
            
            # If detailed cluster demographics exist in metadata (usually best-k), use them for median/IQR.
            metadata_cluster = clusters.get(str(cluster_name), {}) if isinstance(clusters, dict) else {}
            age_median = _safe_float(metadata_cluster.get("age_median"))
            age_q1 = _safe_float(metadata_cluster.get("age_q1"))
            age_q3 = _safe_float(metadata_cluster.get("age_q3"))
            
            rows.append({
                "cluster": str(cluster_name),
                "patient_count": f"{n_value:,}" if n_value is not None else "NA",
                "age_mean": round(age_mean, 2) if age_mean is not None else None,
                "age_median": round(age_median, 2) if age_median is not None else None,
                "age_iqr": f"{age_q1:.1f}-{age_q3:.1f}" if age_q1 is not None and age_q3 is not None else "NA",
                "male_pct": round(male_prop * 100, 2) if male_prop is not None else None,
                "age_delta_vs_overall": round(age_mean - overall_age_mean, 2) if age_mean is not None else None
            })
        if rows:
            return rows
    
    # Fallback to metadata cluster demographics when clustering summary is unavailable.
    if isinstance(clusters, dict) and clusters:
        for cluster_name in sorted(clusters.keys()):
            cluster_data = clusters.get(cluster_name, {})
            age_mean = _safe_float(cluster_data.get("age_mean"))
            age_median = _safe_float(cluster_data.get("age_median"))
            age_q1 = _safe_float(cluster_data.get("age_q1"))
            age_q3 = _safe_float(cluster_data.get("age_q3"))
            male_prop = _safe_float(cluster_data.get("male_proportion"))
            count = _safe_int(cluster_data.get("patient_count"), default=_safe_int(cluster_data.get("n")))
            age_delta = (age_mean - overall_age_mean) if age_mean is not None else None
            
            rows.append({
                "cluster": cluster_name,
                "patient_count": f"{count:,}" if count is not None else "NA",
                "age_mean": round(age_mean, 2) if age_mean is not None else None,
                "age_median": round(age_median, 2) if age_median is not None else None,
                "age_iqr": f"{age_q1:.1f}-{age_q3:.1f}" if age_q1 is not None and age_q3 is not None else "NA",
                "male_pct": round(male_prop * 100, 2) if male_prop is not None else None,
                "age_delta_vs_overall": round(age_delta, 2) if age_delta is not None else None
            })
    
    return rows


def _build_cluster_view_options(context: Dict) -> List[Dict]:
    """Build options for concept cluster demographic view selector."""
    options = [{"label": "Overall", "value": "overall"}]
    labels = set()
    
    clusters = context.get("clusters", {})
    if isinstance(clusters, dict):
        labels.update(str(c) for c in clusters.keys())
    
    cluster_summary_df = context.get("cluster_summary_df", pd.DataFrame())
    if isinstance(cluster_summary_df, pd.DataFrame) and not cluster_summary_df.empty and "cluster" in cluster_summary_df.columns:
        labels.update(str(c) for c in cluster_summary_df["cluster"].dropna().unique())
    
    for label in sorted(labels):
        options.append({"label": label, "value": label})
    return options


def _get_concept_source_dataframe(context: Dict, cluster_view: str) -> pd.DataFrame:
    """Get concept-level demographics source for selected view."""
    concept_df = context.get("concept_summaries", pd.DataFrame()).copy()
    if cluster_view == "overall":
        return concept_df
    
    cluster_summary_df = context.get("cluster_summary_df", pd.DataFrame()).copy()
    if cluster_summary_df.empty or "cluster" not in cluster_summary_df.columns:
        return concept_df
    
    source = cluster_summary_df[cluster_summary_df["cluster"].astype(str) == str(cluster_view)].copy()
    if "IS_ORDINAL" in source.columns:
        source = source[source["IS_ORDINAL"] != True]
    return source


def _build_demographics_concept_rows(
    context: Dict,
    min_patients: Optional[int],
    cluster_view: str
) -> Tuple[List[Dict], List[Dict]]:
    """Build concept-level age-delta and sex-skew table rows."""
    demo = context.get("demo", {})
    overall_age = _safe_float(demo.get("age_mean"), default=0.0)
    overall_male_prop = _safe_float(demo.get("male_proportion"), default=0.0)
    
    df = _get_concept_source_dataframe(context, cluster_view)
    if df.empty:
        return [], []
    
    result_df = df.copy()
    if "CONCEPT_NAME" not in result_df.columns:
        return [], []
    
    patient_col = "patient_count" if "patient_count" in result_df.columns else ("n_ages" if "n_ages" in result_df.columns else None)
    if patient_col is None:
        return [], []
    
    result_df["patient_count_num"] = pd.to_numeric(result_df[patient_col], errors="coerce")
    if min_patients is not None:
        result_df = result_df[result_df["patient_count_num"] >= float(min_patients)]
    
    if result_df.empty:
        return [], []
    
    result_df["age_mean_num"] = pd.to_numeric(result_df.get("age_mean"), errors="coerce")
    result_df["male_prop_num"] = pd.to_numeric(result_df.get("male_proportion"), errors="coerce")
    
    # Age deltas: top 5 younger and top 5 older concepts
    age_rows: List[Dict] = []
    age_df = result_df.dropna(subset=["age_mean_num"]).copy()
    if not age_df.empty:
        age_df["age_delta"] = age_df["age_mean_num"] - overall_age
        younger = age_df.nsmallest(5, "age_delta")
        older = age_df.nlargest(5, "age_delta")
        
        for _, row in younger.iterrows():
            age_rows.append({
                "segment": "Younger",
                "concept_name": str(row.get("CONCEPT_NAME", "")),
                "patient_count": f"{int(row['patient_count_num']):,}",
                "age_mean": round(float(row["age_mean_num"]), 2),
                "delta_years": round(float(row["age_delta"]), 2)
            })
        for _, row in older.iterrows():
            age_rows.append({
                "segment": "Older",
                "concept_name": str(row.get("CONCEPT_NAME", "")),
                "patient_count": f"{int(row['patient_count_num']):,}",
                "age_mean": round(float(row["age_mean_num"]), 2),
                "delta_years": round(float(row["age_delta"]), 2)
            })
    
    # Sex skew: top 10 absolute deviation from overall male proportion
    sex_rows: List[Dict] = []
    sex_df = result_df.dropna(subset=["male_prop_num"]).copy()
    if not sex_df.empty:
        sex_df["male_pct"] = sex_df["male_prop_num"] * 100
        sex_df["female_pct"] = (1 - sex_df["male_prop_num"]) * 100
        sex_df["male_delta_pct"] = sex_df["male_pct"] - (overall_male_prop * 100)
        sex_df["abs_delta"] = sex_df["male_delta_pct"].abs()
        skewed = sex_df.nlargest(10, "abs_delta")
        
        for _, row in skewed.iterrows():
            sex_rows.append({
                "concept_name": str(row.get("CONCEPT_NAME", "")),
                "patient_count": f"{int(row['patient_count_num']):,}",
                "male_pct": round(float(row["male_pct"]), 2),
                "female_pct": round(float(row["female_pct"]), 2),
                "delta_male_pct": round(float(row["male_delta_pct"]), 2)
            })
    
    return age_rows, sex_rows


def _build_ordinal_dropdown_options(context: Dict) -> Tuple[List[Dict], Optional[str]]:
    """Build ordinal concept dropdown options and default value."""
    ordinal_df = context.get("ordinal_summaries", pd.DataFrame())
    if ordinal_df.empty or "ORIGINAL_CONCEPT_ID" not in ordinal_df.columns:
        return [], None
    
    working = ordinal_df.copy()
    if "patient_count" in working.columns:
        working["patient_count_num"] = pd.to_numeric(working["patient_count"], errors="coerce").fillna(0)
    else:
        working["patient_count_num"] = 0
    
    if "CONCEPT_NAME" in working.columns:
        concept_names = working["CONCEPT_NAME"].astype(str)
    else:
        concept_names = working["ORIGINAL_CONCEPT_ID"].astype(str).apply(lambda x: f"Concept {x}")
    
    working["concept_label"] = concept_names.str.replace(r"\s+\d+(st|nd|rd|th)$", "", regex=True)
    
    # Take highest patient_count row per ORIGINAL_CONCEPT_ID
    representative = working.sort_values("patient_count_num", ascending=False).drop_duplicates("ORIGINAL_CONCEPT_ID")
    representative = representative.sort_values("patient_count_num", ascending=False)
    
    options = []
    for _, row in representative.iterrows():
        original_id = str(row["ORIGINAL_CONCEPT_ID"])
        label = row.get("concept_label") or f"Concept {original_id}"
        n_value = int(row.get("patient_count_num", 0))
        options.append({"label": f"{label} (n={n_value:,})", "value": original_id})
    
    default_value = options[0]["value"] if options else None
    return options, default_value


def _create_ordinal_demographics_figures(
    context: Dict,
    selected_original_concept_id: Optional[str]
) -> Tuple[go.Figure, go.Figure]:
    """Create ordinal progression charts for age and sex."""
    ordinal_df = context.get("ordinal_summaries", pd.DataFrame())
    overall_age = _safe_float(context.get("demo", {}).get("age_mean"))
    
    if ordinal_df.empty or selected_original_concept_id is None:
        msg = "Select an ordinal concept to view progression."
        return _create_demographics_message_figure(msg), _create_demographics_message_figure(msg)
    
    selected_df = ordinal_df[
        ordinal_df["ORIGINAL_CONCEPT_ID"].astype(str) == str(selected_original_concept_id)
    ].copy()
    if selected_df.empty:
        msg = "No ordinal summary rows for selected concept."
        return _create_demographics_message_figure(msg), _create_demographics_message_figure(msg)
    
    selected_df["ORDINAL_NUM"] = pd.to_numeric(selected_df.get("ORDINAL"), errors="coerce")
    selected_df = selected_df.dropna(subset=["ORDINAL_NUM"]).sort_values("ORDINAL_NUM")
    
    # Age progression figure
    age_fig = go.Figure()
    selected_df["AGE_MEAN_NUM"] = pd.to_numeric(selected_df.get("age_mean"), errors="coerce")
    age_plot_df = selected_df.dropna(subset=["AGE_MEAN_NUM"])
    if not age_plot_df.empty:
        age_fig.add_trace(
            go.Scatter(
                x=age_plot_df["ORDINAL_NUM"],
                y=age_plot_df["AGE_MEAN_NUM"],
                mode="lines+markers+text",
                text=[f"n={int(v):,}" if pd.notna(v) else "" for v in age_plot_df.get("patient_count", [])],
                textposition="top center",
                name="Mean age",
                marker=dict(size=8, color="#1f77b4")
            )
        )
        if overall_age is not None:
            age_fig.add_hline(
                y=overall_age,
                line_dash="dash",
                line_color="#888",
                annotation_text=f"Overall mean age {overall_age:.1f}",
                annotation_position="top left"
            )
    else:
        age_fig = _create_demographics_message_figure("Age progression not available for selected concept.")
    
    if age_plot_df is not None and not age_plot_df.empty:
        age_fig.update_layout(
            title="<b>Ordinal Age Progression</b>",
            height=320,
            margin=dict(t=60, b=50, l=55, r=20),
            plot_bgcolor="white",
            paper_bgcolor="white",
            xaxis_title="Ordinal occurrence",
            yaxis_title="Mean age (years)"
        )
        age_fig.update_xaxes(dtick=1, gridcolor="#f3f3f3", autorange=True)
        age_fig.update_yaxes(gridcolor="#eee", autorange=True)
    
    # Sex progression figure
    sex_fig = go.Figure()
    selected_df["MALE_PROP_NUM"] = pd.to_numeric(selected_df.get("male_proportion"), errors="coerce")
    sex_plot_df = selected_df.dropna(subset=["MALE_PROP_NUM"])
    if not sex_plot_df.empty:
        sex_fig.add_trace(
            go.Scatter(
                x=sex_plot_df["ORDINAL_NUM"],
                y=sex_plot_df["MALE_PROP_NUM"] * 100,
                mode="lines+markers",
                name="Male %",
                marker=dict(size=7, color="#2c3e50")
            )
        )
        sex_fig.add_trace(
            go.Scatter(
                x=sex_plot_df["ORDINAL_NUM"],
                y=(1 - sex_plot_df["MALE_PROP_NUM"]) * 100,
                mode="lines+markers",
                name="Female %",
                marker=dict(size=7, color="#9ecae1")
            )
        )
        sex_fig.update_layout(
            title="<b>Ordinal Sex Progression</b>",
            height=320,
            margin=dict(t=60, b=50, l=55, r=20),
            plot_bgcolor="white",
            paper_bgcolor="white",
            xaxis_title="Ordinal occurrence",
            yaxis_title="Percentage (%)",
            yaxis=dict(autorange=True),
            legend=dict(orientation="h", yanchor="bottom", y=1.02, xanchor="left", x=0)
        )
        sex_fig.update_xaxes(dtick=1, gridcolor="#f3f3f3", autorange=True)
        sex_fig.update_yaxes(gridcolor="#eee", autorange=True)
    else:
        sex_fig = _create_demographics_message_figure("Sex progression not available for selected concept.")
    
    return age_fig, sex_fig
