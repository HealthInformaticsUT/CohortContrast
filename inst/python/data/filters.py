"""
Data filtering functions for ContrastViewer.
"""

from typing import Dict, List, Set
import pandas as pd

from utils.helpers import normalize_concept_id, is_ordinal_concept


def apply_filters_to_data(
    updated_data: List[Dict],
    target_min: float,
    target_max: float,
    ratio_min: float,
    ratio_max: float,
    selected_heritages_set: Set[str],
    selected_study: str,
    dashboard_show_state: Dict[str, Dict[int, bool]]
) -> None:
    """
    Apply filter criteria to dashboard data and update _show column.
    
    Args:
        updated_data: List of concept dictionaries to filter
        target_min: Minimum target prevalence percentage
        target_max: Maximum target prevalence percentage
        ratio_min: Minimum prevalence difference ratio
        ratio_max: Maximum prevalence difference ratio
        selected_heritages_set: Set of selected heritage types
        selected_study: Current study name
        dashboard_show_state: Dictionary tracking show/hide state per study
    """
    for row in updated_data:
        target_prevalence = row.get("TARGET_SUBJECT_PREVALENCE_PCT", 0)
        if target_prevalence is None or pd.isna(target_prevalence):
            target_prevalence = 0
        
        ratio = row.get("PREVALENCE_DIFFERENCE_RATIO_DISPLAY", 0)
        if ratio is None or pd.isna(ratio):
            ratio = 0
        
        heritage = row.get("HERITAGE")
        heritage_str = str(heritage) if heritage is not None and pd.notna(heritage) else ""
        heritage_selected = heritage_str in selected_heritages_set if heritage_str else False
        
        matches_target = target_min <= target_prevalence <= target_max
        matches_ratio = ratio_min <= ratio <= ratio_max
        matches_heritage = heritage_selected
        
        row["_show"] = matches_target and matches_ratio and matches_heritage
        
        # Update stored state
        concept_id = row.get("_concept_id")
        if concept_id is not None:
            if selected_study not in dashboard_show_state:
                dashboard_show_state[selected_study] = {}
            dashboard_show_state[selected_study][concept_id] = row["_show"]


def filter_ordinal_concepts_for_filtered_mains(
    updated_data: List[Dict],
    selected_study: str,
    dashboard_show_state: Dict[str, Dict[int, bool]]
) -> None:
    """
    Filter out ordinal concepts when their main concept is filtered out.
    
    Args:
        updated_data: List of concept dictionaries
        selected_study: Current study name
        dashboard_show_state: Dictionary tracking show/hide state per study
    """
    filtered_out_main_concepts = {}
    
    # Find all filtered-out main concepts
    for row in updated_data:
        if not is_ordinal_concept(row) and not row.get("_show", True):
            main_concept_id = row.get("CONCEPT_ID")
            main_heritage = row.get("HERITAGE")
            
            if main_concept_id is not None:
                main_concept_id_normalized = normalize_concept_id(main_concept_id)
                heritage_key = str(main_heritage) if main_heritage is not None and pd.notna(main_heritage) else None
                key = (main_concept_id_normalized, heritage_key)
                filtered_out_main_concepts[key] = True
    
    # Filter out ordinal concepts linked to filtered-out main concepts
    for ordinal_row in updated_data:
        if is_ordinal_concept(ordinal_row):
            original_concept_id = ordinal_row.get("ORIGINAL_CONCEPT_ID")
            ordinal_heritage = ordinal_row.get("HERITAGE")
            
            if original_concept_id is not None:
                original_concept_id_normalized = normalize_concept_id(original_concept_id)
                heritage_key = str(ordinal_heritage) if ordinal_heritage is not None and pd.notna(ordinal_heritage) else None
                key = (original_concept_id_normalized, heritage_key)
                
                if key in filtered_out_main_concepts:
                    ordinal_row["_show"] = False
                    ordinal_concept_id_for_state = ordinal_row.get("_concept_id")
                    if ordinal_concept_id_for_state is not None:
                        if selected_study not in dashboard_show_state:
                            dashboard_show_state[selected_study] = {}
                        dashboard_show_state[selected_study][ordinal_concept_id_for_state] = False

