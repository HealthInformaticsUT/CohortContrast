"""Occurrence metric calculations for dashboard concept tables."""

import warnings

import pandas as pd

from utils.helpers import get_min_time, get_unique_occurrences
def calculate_median_first_occurrence(data_patients: pd.DataFrame) -> pd.DataFrame:
    """
    Calculate the median first occurrence time for each concept in the target cohort.
    
    For each concept and person in the target cohort:
    1. Find the minimum TIME_TO_EVENT (first occurrence)
    2. Calculate the median of these minimum times across all persons
    
    Groups by both CONCEPT_ID and HERITAGE since the pair is unique.
    
    Args:
        data_patients: DataFrame with columns COHORT_DEFINITION_ID, PERSON_ID, CONCEPT_ID, HERITAGE, TIME_TO_EVENT
        
    Returns:
        DataFrame with columns CONCEPT_ID, HERITAGE, and MEDIAN_FIRST_OCCURRENCE (in days)
    """
    # Filter for target cohort only
    df_target = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
    
    if df_target.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Ensure HERITAGE column exists
    if "HERITAGE" not in df_target.columns:
        warnings.warn("HERITAGE column not found in data_patients. Using CONCEPT_ID only for grouping.")
        # Fallback to CONCEPT_ID only if HERITAGE is missing
        has_heritage = False
    else:
        has_heritage = True
    
    # Calculate first occurrence (minimum time) for each person-concept combination
    df_target["FIRST_OCCURRENCE"] = df_target["TIME_TO_EVENT"].apply(get_min_time)
    
    # Remove rows where first occurrence couldn't be calculated
    df_target = df_target[df_target["FIRST_OCCURRENCE"].notna()].copy()
    
    if df_target.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Group by both CONCEPT_ID and HERITAGE (the pair is unique)
    if has_heritage:
        median_df = df_target.groupby(["CONCEPT_ID", "HERITAGE"])["FIRST_OCCURRENCE"].median().reset_index()
        median_df.columns = ["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"]
    else:
        # Fallback: group by CONCEPT_ID only if HERITAGE is missing
        median_df = df_target.groupby("CONCEPT_ID")["FIRST_OCCURRENCE"].median().reset_index()
        median_df.columns = ["CONCEPT_ID", "MEDIAN_FIRST_OCCURRENCE"]
        median_df["HERITAGE"] = None
    
    return median_df


def calculate_ordinal_medians(data_patients: pd.DataFrame, ordinal_concepts_df: pd.DataFrame) -> pd.DataFrame:
    """
    Calculate median occurrence time for ordinal concepts (1st, 2nd, 3rd, etc.).
    
    Uses vectorized operations for improved performance.
    
    Args:
        data_patients: DataFrame with COHORT_DEFINITION_ID, PERSON_ID, CONCEPT_ID, HERITAGE, TIME_TO_EVENT
        ordinal_concepts_df: DataFrame with ordinal concepts (ORIGINAL_CONCEPT_ID, ORDINAL, CONCEPT_ID, HERITAGE)
        
    Returns:
        DataFrame with CONCEPT_ID, HERITAGE, and MEDIAN_FIRST_OCCURRENCE for ordinal concepts
    """
    # Filter for target cohort only
    df_target = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
    
    if df_target.empty or ordinal_concepts_df.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Pre-compute unique occurrences once for all target patients
    df_target["OCCURRENCES"] = df_target["TIME_TO_EVENT"].apply(get_unique_occurrences)
    df_target["NUM_OCCURRENCES"] = df_target["OCCURRENCES"].apply(len)
    
    # Filter to only rows with at least 1 occurrence
    df_target = df_target[df_target["NUM_OCCURRENCES"] > 0].copy()
    
    if df_target.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Prepare ordinal concepts for merge
    ordinal_df = ordinal_concepts_df[["CONCEPT_ID", "ORIGINAL_CONCEPT_ID", "ORDINAL", "HERITAGE"]].copy()
    ordinal_df = ordinal_df.dropna(subset=["ORIGINAL_CONCEPT_ID", "ORDINAL"])
    
    if ordinal_df.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Merge ordinal concepts with patient data on ORIGINAL_CONCEPT_ID and HERITAGE
    has_heritage = "HERITAGE" in df_target.columns and "HERITAGE" in ordinal_df.columns
    
    if has_heritage:
        merged = df_target.merge(
            ordinal_df,
            left_on=["CONCEPT_ID", "HERITAGE"],
            right_on=["ORIGINAL_CONCEPT_ID", "HERITAGE"],
            suffixes=("_patient", "_ordinal")
        )
        # Use the ordinal CONCEPT_ID for the result
        merged["RESULT_CONCEPT_ID"] = merged["CONCEPT_ID_ordinal"]
        merged["RESULT_HERITAGE"] = merged["HERITAGE"]
    else:
        merged = df_target.merge(
            ordinal_df,
            left_on="CONCEPT_ID",
            right_on="ORIGINAL_CONCEPT_ID",
            suffixes=("_patient", "_ordinal")
        )
        merged["RESULT_CONCEPT_ID"] = merged["CONCEPT_ID_ordinal"]
        merged["RESULT_HERITAGE"] = merged.get("HERITAGE_ordinal", merged.get("HERITAGE"))
    
    if merged.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Filter to patients with enough occurrences for their ordinal
    merged = merged[merged["NUM_OCCURRENCES"] >= merged["ORDINAL"]].copy()
    
    if merged.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Get the specific ordinal occurrence for each row
    def get_nth_occurrence(row):
        occs = row["OCCURRENCES"]
        ordinal = int(row["ORDINAL"])
        idx = ordinal - 1  # Convert to 0-indexed
        return occs[idx] if len(occs) > idx else None
    
    merged["ORDINAL_OCCURRENCE"] = merged.apply(get_nth_occurrence, axis=1)
    merged = merged[merged["ORDINAL_OCCURRENCE"].notna()].copy()
    
    if merged.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Group by ordinal concept and calculate median
    # First, get one value per person per ordinal concept
    person_first = merged.groupby(
        ["RESULT_CONCEPT_ID", "RESULT_HERITAGE", "PERSON_ID"]
    )["ORDINAL_OCCURRENCE"].first().reset_index()
    
    # Then calculate median per ordinal concept
    medians = person_first.groupby(
        ["RESULT_CONCEPT_ID", "RESULT_HERITAGE"]
    )["ORDINAL_OCCURRENCE"].median().reset_index()
    
    medians.columns = ["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"]
    medians = medians[medians["MEDIAN_FIRST_OCCURRENCE"].notna()]
    
    return medians


# =============================================================================
