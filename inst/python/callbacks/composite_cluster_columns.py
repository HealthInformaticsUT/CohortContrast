"""Cluster column rendering for combined composite figure."""

from typing import Dict, List, Optional

import numpy as np
import pandas as pd
import plotly.graph_objects as go

from callbacks.composite_helpers import extract_first_time as _extract_first_time
from utils.helpers import (
    get_unique_occurrences,
    normalize_concept_id as _normalize_concept_id,
    normalize_concept_name,
)


def _to_dataframe(data) -> pd.DataFrame:
    if isinstance(data, list):
        return pd.DataFrame(data) if data else pd.DataFrame()
    return data if isinstance(data, pd.DataFrame) else pd.DataFrame()


def _get_cluster_patients(patient_assignments_df: pd.DataFrame, cluster_label: str) -> set:
    if (
        patient_assignments_df.empty
        or "cluster" not in patient_assignments_df.columns
        or "patient_id" not in patient_assignments_df.columns
    ):
        return set()
    return set(
        patient_assignments_df[patient_assignments_df["cluster"] == cluster_label][
            "patient_id"
        ].tolist()
    )


def _set_zero_cluster_entries(
    *,
    cluster_id_map: Dict,
    cluster_data_map: Dict,
    concept_id,
    concept_name: str,
    optimal_k: int,
) -> None:
    normalized_name = normalize_concept_name(concept_name)
    for cluster_idx in range(optimal_k):
        cluster_label = f"C{cluster_idx + 1}"
        data_entry = {"prevalence": 0.0, "median_days": np.nan, "count_category": "1"}
        if concept_id is not None:
            cluster_id_map[(str(concept_id), cluster_label)] = data_entry
        cluster_data_map[(normalized_name, cluster_label)] = data_entry


def _count_category_from_median(median_count) -> str:
    if pd.isna(median_count) or median_count <= 1:
        return "1"
    if median_count <= 2:
        return "2"
    return "3+"


def _median_occurrence_count(concept_rows: pd.DataFrame) -> float:
    """
    Estimate median occurrence count per patient for a concept.

    Uses TIME_TO_EVENT list lengths when available (patient-mode data stores
    occurrences as arrays per patient-concept row), with grouped row-count
    fallback for legacy shapes.
    """
    if concept_rows.empty or "PERSON_ID" not in concept_rows.columns:
        return np.nan

    if "TIME_TO_EVENT" in concept_rows.columns:
        per_patient_counts = concept_rows.groupby("PERSON_ID")["TIME_TO_EVENT"].apply(
            lambda values: max((len(get_unique_occurrences(v)) for v in values), default=0)
        )
        per_patient_counts = per_patient_counts[per_patient_counts > 0]
    else:
        per_patient_counts = concept_rows.groupby("PERSON_ID").size()

    if len(per_patient_counts) == 0:
        return np.nan
    return float(per_patient_counts.median())


def add_cluster_columns_to_figure(
    fig_combined,
    *,
    clustering_results: Optional[Dict],
    heritage_groups_order: Dict,
    heritages_with_data: List[str],
    base_cols: int,
    optimal_k: int,
    dashboard_data: List[Dict],
    data_patients: pd.DataFrame,
) -> None:
    """Add cluster heatmap columns and annotations to the combined figure."""
    # Get cluster data
    summary_matrix_data = clustering_results.get('summary_matrix', [])
    patient_assignments_data = clustering_results.get('patient_assignments', [])
    
    summary_matrix = _to_dataframe(summary_matrix_data)
    
    # Always create patient_assignments_df (even if empty for summary mode)
    patient_assignments_df = _to_dataframe(patient_assignments_data)
    
    if not summary_matrix.empty:
        count_category_map = {}
        
        is_ordinal_col = 'IS_ORDINAL' in summary_matrix.columns or 'is_ordinal' in summary_matrix.columns
        original_id_col = 'ORIGINAL_CONCEPT_ID' if 'ORIGINAL_CONCEPT_ID' in summary_matrix.columns else ('original_concept_id' if 'original_concept_id' in summary_matrix.columns else None)
        ordinal_col = 'ORDINAL' if 'ORDINAL' in summary_matrix.columns else ('ordinal' if 'ordinal' in summary_matrix.columns else None)
        patient_count_col = 'patient_count' if 'patient_count' in summary_matrix.columns else ('PATIENT_COUNT' if 'PATIENT_COUNT' in summary_matrix.columns else None)
        
        if original_id_col and ordinal_col and patient_count_col:
            if is_ordinal_col:
                is_ordinal_col_name = 'IS_ORDINAL' if 'IS_ORDINAL' in summary_matrix.columns else 'is_ordinal'
                ordinal_rows = summary_matrix[summary_matrix[is_ordinal_col_name] == True].copy()
            else:
                concept_id_col = 'CONCEPT_ID' if 'CONCEPT_ID' in summary_matrix.columns else ('concept_id' if 'concept_id' in summary_matrix.columns else None)
                if concept_id_col and ordinal_col:
                    ordinal_mask = summary_matrix[ordinal_col] > 0
                    ordinal_mask = ordinal_mask | summary_matrix[concept_id_col].astype(str).str.contains('_', na=False)
                    ordinal_rows = summary_matrix[ordinal_mask].copy()
                else:
                    ordinal_rows = pd.DataFrame()
            
            if not ordinal_rows.empty:
                for (orig_concept_id, cluster), group in ordinal_rows.groupby([original_id_col, 'cluster']):
                    group_sorted = group.sort_values(ordinal_col)
                    max_valid_ordinal = 0
                    prev_count = None
                    
                    for _, row in group_sorted.iterrows():
                        ordinal = row[ordinal_col]
                        patient_count = row[patient_count_col]
                        
                        if ordinal == 1:
                            if patient_count > 0:
                                max_valid_ordinal = 1
                                prev_count = patient_count
                            else:
                                break
                        else:
                            if prev_count is not None and prev_count > 0:
                                if patient_count >= prev_count * 0.5:
                                    max_valid_ordinal = ordinal
                                    prev_count = patient_count
                                else:
                                    break
                            else:
                                break
                    
                    if max_valid_ordinal <= 1:
                        count_cat = "1"
                    elif max_valid_ordinal == 2:
                        count_cat = "2"
                    else:
                        count_cat = "3+"
                    
                    orig_concept_id_str = str(orig_concept_id).replace('.0', '')
                    cluster_str = str(cluster)
                    count_category_map[(orig_concept_id_str, cluster_str)] = count_cat
        
        # Build concept map from dashboard_data for ordinal lookup
        dashboard_concept_map_cluster = {}
        for row in dashboard_data:
            cid = row.get("_concept_id") or row.get("CONCEPT_ID")
            if cid is not None:
                dashboard_concept_map_cluster[str(cid)] = row
                dashboard_concept_map_cluster[_normalize_concept_id(cid)] = row
        
        # Build lookup maps for cluster data (by name, by concept_id, and by ordinal key)
        cluster_data_map = {}
        cluster_id_map = {}
        ordinal_cluster_map = {}  # For ordinal lookup: (heritage, original_concept_id, ordinal, cluster) -> data
        
        # First, populate maps from summary_matrix for concepts that were in original clustering
        # This ensures consistency with Trajectories tab
        # Note: column names may be uppercase (from parquet) or lowercase
        concept_name_col = 'CONCEPT_NAME' if 'CONCEPT_NAME' in summary_matrix.columns else 'concept_name'
        concept_id_col = 'CONCEPT_ID' if 'CONCEPT_ID' in summary_matrix.columns else 'concept_id'
        cluster_col = 'cluster'
        
        # Use to_dict instead of iterrows
        if concept_name_col in summary_matrix.columns and cluster_col in summary_matrix.columns:
            for row in summary_matrix.to_dict('records'):
                name_normalized = normalize_concept_name(row.get(concept_name_col, ''))
                cluster_label = row.get(cluster_col, '')
                key = (name_normalized, cluster_label)
                
                # Get count_category: compute from ordinals if not present, or use from map
                # Check IS_ORDINAL column (may be uppercase or lowercase)
                is_ordinal_row = False
                if 'IS_ORDINAL' in row:
                    is_ordinal_row = row['IS_ORDINAL'] == True
                elif 'is_ordinal' in row:
                    is_ordinal_row = row['is_ordinal'] == True
                
                if is_ordinal_row:
                    count_category = "1"
                else:
                    concept_id_normalized = None
                    concept_id_sm = row.get(concept_id_col)
                    if concept_id_sm is not None:
                        concept_id_normalized = str(concept_id_sm).replace('.0', '')
                    elif 'ORIGINAL_CONCEPT_ID' in row:
                        concept_id_normalized = str(row['ORIGINAL_CONCEPT_ID']).replace('.0', '')
                    elif 'original_concept_id' in row:
                        concept_id_normalized = str(row['original_concept_id']).replace('.0', '')
                    
                    if concept_id_normalized:
                        cluster_str = str(cluster_label)
                        count_category = count_category_map.get((concept_id_normalized, cluster_str))
                        if count_category is None:
                            count_category = row.get('count_category', "1")
                    else:
                        count_category = row.get('count_category', "1")
                
                data_entry = {
                    'prevalence': row.get('prevalence', 0),
                    'median_days': row.get('time_median') or row.get('median_days'),
                    'count_category': count_category
                }
                cluster_data_map[key] = data_entry
                
                # Also map by concept_id
                concept_id_sm = row.get(concept_id_col)
                if concept_id_sm is not None:
                    key_id = (str(concept_id_sm), cluster_label)
                    cluster_id_map[key_id] = data_entry
                    key_id_norm = (_normalize_concept_id(concept_id_sm), cluster_label)
                    cluster_id_map[key_id_norm] = data_entry
        
        # Now calculate dynamically for concepts not in summary_matrix
        # Only if patient-level data is available
        if not data_patients.empty and 'COHORT_DEFINITION_ID' in data_patients.columns:
            df_target_cluster = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
        else:
            df_target_cluster = pd.DataFrame()
        
        # Process ALL concepts from heritage_groups_order (both main and ordinals)
        # Calculate cluster metrics dynamically using patient assignments
        # Skip if no patient-level data is available (summary mode)
        has_patient_data = not df_target_cluster.empty and 'CONCEPT_ID' in df_target_cluster.columns
        
        for heritage_key, items in heritage_groups_order.items():
            for item in items:
                # Skip dynamic calculation if no patient data
                if not has_patient_data:
                    # In summary mode, rely only on pre-computed data in cluster_data_map/cluster_id_map
                    continue
                
                is_ordinal_item = item.get("is_ordinal", False)
                concept_id = item.get("concept_id") or item.get("_concept_id") or item.get("CONCEPT_ID")
                concept_name = item.get("concept_name", "Unknown")
                
                if is_ordinal_item:
                    # Handle ordinal concepts
                    original_concept_id = item.get("original_concept_id") or item.get("ORIGINAL_CONCEPT_ID")
                    ordinal_num = item.get("ordinal") or item.get("ORDINAL", 0)
                    
                    # Get ordinal info from dashboard data if needed
                    if (not original_concept_id or ordinal_num == 0) and concept_id:
                        concept_info = dashboard_concept_map_cluster.get(str(concept_id)) or dashboard_concept_map_cluster.get(_normalize_concept_id(concept_id))
                        if concept_info:
                            if not original_concept_id:
                                original_concept_id = concept_info.get("ORIGINAL_CONCEPT_ID", concept_id)
                            if ordinal_num == 0:
                                ordinal_num = concept_info.get("ORDINAL", 0)
                    
                    if ordinal_num == 0 or not original_concept_id:
                        continue
                    
                    # Filter data for this original concept
                    if "HERITAGE" in df_target_cluster.columns and heritage_key is not None:
                        concept_data = df_target_cluster[
                            (df_target_cluster["CONCEPT_ID"] == original_concept_id) &
                            (df_target_cluster["HERITAGE"] == heritage_key)
                        ].copy()
                    else:
                        concept_data = df_target_cluster[df_target_cluster["CONCEPT_ID"] == original_concept_id].copy()
                    
                    if concept_data.empty:
                        _set_zero_cluster_entries(
                            cluster_id_map=cluster_id_map,
                            cluster_data_map=cluster_data_map,
                            concept_id=concept_id,
                            concept_name=concept_name,
                            optimal_k=optimal_k,
                        )
                        continue
                    
                    # Get unique occurrences for each person
                    concept_data["OCCURRENCES"] = concept_data["TIME_TO_EVENT"].apply(get_unique_occurrences)
                    concept_data = concept_data[concept_data["OCCURRENCES"].apply(len) >= ordinal_num].copy()
                    
                    if concept_data.empty:
                        _set_zero_cluster_entries(
                            cluster_id_map=cluster_id_map,
                            cluster_data_map=cluster_data_map,
                            concept_id=concept_id,
                            concept_name=concept_name,
                            optimal_k=optimal_k,
                        )
                        continue
                    
                    # Get the specific occurrence
                    concept_data["TIME_VALUE"] = concept_data["OCCURRENCES"].apply(
                        lambda occs: occs[ordinal_num - 1] if len(occs) >= ordinal_num else None
                    )
                    concept_data = concept_data[concept_data["TIME_VALUE"].notna()].copy()
                    
                    if concept_data.empty:
                        _set_zero_cluster_entries(
                            cluster_id_map=cluster_id_map,
                            cluster_data_map=cluster_data_map,
                            concept_id=concept_id,
                            concept_name=concept_name,
                            optimal_k=optimal_k,
                        )
                        continue
                    
                    # Group by person
                    person_occurrences = concept_data.groupby("PERSON_ID")["TIME_VALUE"].first()
                    patients_with_concept = set(person_occurrences.index.tolist())
                    
                else:
                    # Handle main concepts
                    # First check if this concept is already in the maps (from summary_matrix)
                    if concept_id is not None:
                        test_key = (str(concept_id), 'C1')
                        test_key_norm = (_normalize_concept_id(concept_id), 'C1')
                        if test_key in cluster_id_map or test_key_norm in cluster_id_map:
                            continue  # Already have data from summary_matrix
                    
                    # Also check by name
                    name_key = (normalize_concept_name(concept_name), 'C1')
                    if name_key in cluster_data_map:
                        continue  # Already have data from summary_matrix
                    
                    # Get the concept_id to query data_patients
                    query_concept_id = concept_id
                    
                    # Get concept info from dashboard data
                    if concept_id:
                        concept_info = dashboard_concept_map_cluster.get(str(concept_id)) or dashboard_concept_map_cluster.get(_normalize_concept_id(concept_id))
                        if concept_info:
                            query_concept_id = concept_info.get("CONCEPT_ID") or concept_id
                    
                    # Normalize concept_id for comparison (handle float vs int mismatch)
                    # Try multiple formats to match data_patients CONCEPT_ID
                    query_id_normalized = _normalize_concept_id(query_concept_id)
                    
                    # Get unique CONCEPT_IDs in data to check types
                    unique_ids_in_data = df_target_cluster["CONCEPT_ID"].unique()
                    
                    # Filter data for this concept - try different ID formats
                    concept_data = pd.DataFrame()
                    # Build list of IDs to try
                    ids_to_try = [query_concept_id, query_id_normalized]
                    if query_id_normalized:
                        try:
                            ids_to_try.append(float(query_id_normalized))
                        except (ValueError, TypeError):
                            pass
                        try:
                            ids_to_try.append(int(query_id_normalized))
                        except (ValueError, TypeError):
                            pass
                    
                    for try_id in ids_to_try:
                        if try_id is None:
                            continue
                        if "HERITAGE" in df_target_cluster.columns and heritage_key is not None:
                            concept_data = df_target_cluster[
                                (df_target_cluster["CONCEPT_ID"] == try_id) &
                                (df_target_cluster["HERITAGE"] == heritage_key)
                            ].copy()
                        else:
                            concept_data = df_target_cluster[df_target_cluster["CONCEPT_ID"] == try_id].copy()
                        
                        if not concept_data.empty:
                            break
                    
                    # If still empty, try string comparison as last resort
                    if concept_data.empty and query_id_normalized:
                        str_query = str(query_id_normalized)
                        for uid in unique_ids_in_data:
                            if str(uid).replace('.0', '') == str_query:
                                if "HERITAGE" in df_target_cluster.columns and heritage_key is not None:
                                    concept_data = df_target_cluster[
                                        (df_target_cluster["CONCEPT_ID"] == uid) &
                                        (df_target_cluster["HERITAGE"] == heritage_key)
                                    ].copy()
                                else:
                                    concept_data = df_target_cluster[df_target_cluster["CONCEPT_ID"] == uid].copy()
                                if not concept_data.empty:
                                    break
                    
                    if concept_data.empty:
                        _set_zero_cluster_entries(
                            cluster_id_map=cluster_id_map,
                            cluster_data_map=cluster_data_map,
                            concept_id=concept_id,
                            concept_name=concept_name,
                            optimal_k=optimal_k,
                        )
                        continue
                    
                    # Get patients with this concept and their first occurrence time
                    concept_data["FIRST_TIME"] = concept_data["TIME_TO_EVENT"].apply(
                        _extract_first_time
                    )
                    person_occurrences = concept_data.groupby("PERSON_ID")["FIRST_TIME"].min()
                    patients_with_concept = set(concept_data["PERSON_ID"].unique())
                
                # Calculate prevalence for each cluster
                for cluster_idx in range(optimal_k):
                    cluster_label = f'C{cluster_idx + 1}'
                    cluster_patients = _get_cluster_patients(
                        patient_assignments_df, cluster_label
                    )
                    total_cluster_patients = len(cluster_patients)
                    
                    if total_cluster_patients == 0:
                        prevalence = 0.0
                        median_days = np.nan
                        count_category = '1'
                    else:
                        patients_in_cluster = patients_with_concept & cluster_patients
                        prevalence = len(patients_in_cluster) / total_cluster_patients
                        
                        # Calculate median days and count category
                        if patients_in_cluster:
                            cluster_times = person_occurrences[person_occurrences.index.isin(patients_in_cluster)]
                            valid_times = cluster_times.dropna()
                            median_days = valid_times.median() if len(valid_times) > 0 else np.nan
                            
                            # Count category based on median occurrence count
                            if not is_ordinal_item:
                                cluster_rows = concept_data[
                                    concept_data["PERSON_ID"].isin(patients_in_cluster)
                                ]
                                median_count = _median_occurrence_count(cluster_rows)
                                count_category = _count_category_from_median(median_count)
                            else:
                                count_category = "1"
                        else:
                            median_days = np.nan
                            count_category = "1"
                    
                    # Add to maps
                    data_entry = {
                        'prevalence': prevalence,
                        'median_days': median_days,
                        'count_category': count_category
                    }
                    
                    if concept_id is not None:
                        cluster_id_map[(str(concept_id), cluster_label)] = data_entry
                    
                    cluster_data_map[(normalize_concept_name(concept_name), cluster_label)] = data_entry
                    
                    if is_ordinal_item and original_concept_id is not None and heritage_key is not None:
                        ordinal_key = (str(heritage_key), _normalize_concept_id(original_concept_id), ordinal_num, cluster_label)
                        ordinal_cluster_map[ordinal_key] = data_entry
        
        # Add heatmap cells for each heritage row
        for row_idx, heritage in enumerate(heritages_with_data, start=1):
            items = heritage_groups_order[heritage]
            
            for cluster_col_idx in range(optimal_k):
                cluster_label = f'C{cluster_col_idx + 1}'
                col_idx = base_cols + cluster_col_idx + 1
                
                # Build z_matrix (prevalences) for heatmap
                z_col = []
                text_data = []  # (prevalence_text, median_days_text, count_category)
                
                for y_idx, item in enumerate(items):
                    concept_name_raw = item.get("concept_name", "")
                    concept_name = normalize_concept_name(concept_name_raw)
                    concept_id = item.get("concept_id") or item.get("_concept_id") or item.get("CONCEPT_ID")
                    is_ordinal_item = item.get("is_ordinal", False)
                    cluster_data = {"prevalence": 0.0, "median_days": None, "count_category": "1"}
                    key_id = (str(concept_id), cluster_label) if concept_id is not None else None
                    if key_id is not None:
                        cluster_data = (
                            cluster_id_map.get(key_id)
                            or cluster_id_map.get((_normalize_concept_id(concept_id), cluster_label))
                            or cluster_data
                        )
                    
                    # If not found by concept_id, try by ordinal lookup
                    if cluster_data.get('prevalence', 0.0) == 0.0 and is_ordinal_item:
                        original_concept_id = item.get("original_concept_id") or item.get("ORIGINAL_CONCEPT_ID")
                        ordinal_num = item.get("ordinal") or item.get("ORDINAL", 0)
                        item_heritage = item.get("heritage") or item.get("HERITAGE") or heritage
                        
                        # Try ordinal map lookup (still in old format for backward compat)
                        if original_concept_id is not None and item_heritage is not None and ordinal_num > 0:
                            ordinal_key = (str(item_heritage), _normalize_concept_id(original_concept_id), ordinal_num, cluster_label)
                            cluster_data = ordinal_cluster_map.get(ordinal_key, cluster_data)
                    
                    # Fallback: try old maps (for backward compatibility)
                    if cluster_data.get('prevalence', 0.0) == 0.0:
                        # Try old cluster_id_map
                        key_id = (str(concept_id), cluster_label) if concept_id else None
                        if key_id:
                            old_data = cluster_id_map.get(key_id) or cluster_id_map.get((_normalize_concept_id(concept_id), cluster_label))
                            if old_data:
                                cluster_data = old_data
                        
                        # Try old cluster_data_map by name
                        if cluster_data.get('prevalence', 0.0) == 0.0:
                            key = (concept_name, cluster_label)
                            old_data = cluster_data_map.get(key)
                            if old_data:
                                cluster_data = old_data
                    
                    # For ordinals in SUMMARY mode: inherit from parent concept
                    # Ordinals share their patients with the parent, so use parent's cluster data
                    if cluster_data.get('prevalence', 0.0) == 0.0 and is_ordinal_item and not has_patient_data:
                        original_concept_id = item.get("original_concept_id") or item.get("ORIGINAL_CONCEPT_ID")
                        if original_concept_id is not None:
                            parent_key = (str(original_concept_id), cluster_label)
                            parent_data = cluster_id_map.get(parent_key)
                            if parent_data and parent_data.get('prevalence', 0.0) > 0.0:
                                cluster_data = parent_data
                            else:
                                norm_parent_key = (_normalize_concept_id(original_concept_id), cluster_label)
                                parent_data = cluster_id_map.get(norm_parent_key)
                                if parent_data and parent_data.get('prevalence', 0.0) > 0.0:
                                    cluster_data = parent_data
                    
                    # On-the-fly calculation for concepts not in any map
                    # This handles concepts added to the plot AFTER clustering
                    if cluster_data.get('prevalence', 0.0) == 0.0 and not patient_assignments_df.empty:
                        cluster_patients = _get_cluster_patients(
                            patient_assignments_df, cluster_label
                        )
                        
                        if cluster_patients and not df_target_cluster.empty:
                            # Normalize concept_id for comparison
                            norm_concept_id = _normalize_concept_id(concept_id)
                            
                            # Find patients with this concept
                            if is_ordinal_item:
                                # For ordinals, check specific ordinal occurrence
                                original_concept_id = item.get("original_concept_id") or item.get("ORIGINAL_CONCEPT_ID")
                                ordinal_num = item.get("ordinal") or item.get("ORDINAL", 0)
                                
                                if original_concept_id and ordinal_num > 0:
                                    norm_original_id = _normalize_concept_id(original_concept_id)
                                    
                                    # Get patients with enough occurrences
                                    concept_data_calc = df_target_cluster[
                                        df_target_cluster["CONCEPT_ID"].astype(str).str.replace('.0', '', regex=False) == norm_original_id
                                    ].copy()
                                    
                                    if not concept_data_calc.empty:
                                        # Count occurrences per patient and filter those with >= ordinal_num
                                        patient_occ_counts = concept_data_calc.groupby("PERSON_ID").size()
                                        patients_with_ordinal = set(patient_occ_counts[patient_occ_counts >= ordinal_num].index.tolist())
                                        patients_in_cluster = patients_with_ordinal & cluster_patients
                                        
                                        if cluster_patients:
                                            calc_prevalence = len(patients_in_cluster) / len(cluster_patients)
                                            cluster_data = {'prevalence': calc_prevalence, 'median_days': None, 'count_category': '1'}
                            else:
                                # For main concepts
                                # Try multiple ID formats
                                concept_data_calc = pd.DataFrame()
                                for try_id in [concept_id, norm_concept_id]:
                                    if try_id is None:
                                        continue
                                    concept_data_calc = df_target_cluster[
                                        df_target_cluster["CONCEPT_ID"].astype(str).str.replace('.0', '', regex=False) == str(try_id).replace('.0', '')
                                    ]
                                    if not concept_data_calc.empty:
                                        break
                                
                                if not concept_data_calc.empty:
                                    patients_with_concept = set(concept_data_calc["PERSON_ID"].unique())
                                    patients_in_cluster = patients_with_concept & cluster_patients
                                    
                                    if cluster_patients:
                                        calc_prevalence = len(patients_in_cluster) / len(cluster_patients)
                                        cluster_rows = concept_data_calc[
                                            concept_data_calc["PERSON_ID"].isin(patients_in_cluster)
                                        ]
                                        median_count = _median_occurrence_count(cluster_rows)
                                        count_category = _count_category_from_median(median_count)
                                        cluster_data = {
                                            'prevalence': calc_prevalence,
                                            'median_days': None,
                                            'count_category': count_category
                                        }
                    
                    prevalence = cluster_data.get('prevalence', 0.0)
                    median_days = cluster_data.get('median_days')
                    count_cat = cluster_data.get('count_category', '1')
                    
                    z_col.append([prevalence])
                    
                    if median_days is not None and not pd.isna(median_days):
                        text_data.append((f"{prevalence:.0%}", f"{median_days:.0f}d", count_cat, prevalence))
                    else:
                        text_data.append((f"{prevalence:.0%}", None, count_cat, prevalence))
                
                # Add heatmap trace for this cluster column
                fig_combined.add_trace(
                    go.Heatmap(
                        z=z_col,
                        colorscale=[[0, 'white'], [1, '#2E86AB']],
                        zmin=0,
                        zmax=1.0,
                        showscale=False,
                        hoverongaps=False,
                        xgap=2,
                        ygap=2,
                        hoverinfo='skip'
                    ),
                    row=row_idx,
                    col=col_idx
                )
                
                # Add annotations and circles for each cell
                circle_x = []
                circle_y = []
                circle_colors = []
                circle_line_colors = []
                
                for y_idx, (prev_text, days_text, count_cat, prev_val) in enumerate(text_data):
                    # Prevalence text on left
                    fig_combined.add_annotation(
                        x=-0.35,
                        y=y_idx,
                        text=f"<b>{prev_text}</b>",
                        showarrow=False,
                        font=dict(size=9, color='black'),
                        xanchor='left',
                        yanchor='middle',
                        row=row_idx,
                        col=col_idx
                    )
                    
                    # Median days text on right
                    if days_text:
                        fig_combined.add_annotation(
                            x=0.35,
                            y=y_idx,
                            text=days_text,
                            showarrow=False,
                            font=dict(size=8, color='black'),
                            xanchor='right',
                            yanchor='middle',
                            row=row_idx,
                            col=col_idx
                        )
                    
                    # Circle data
                    circle_x.append(0)
                    circle_y.append(y_idx)
                    
                    if prev_val == 0 or prev_val == 0.0:
                        circle_colors.append("white")
                        circle_line_colors.append("black")
                    elif count_cat == "1":
                        circle_colors.append("#2ECC71")  # Green
                        circle_line_colors.append("white")
                    elif count_cat == "2":
                        circle_colors.append("#F1C40F")  # Yellow
                        circle_line_colors.append("white")
                    else:
                        circle_colors.append("#E74C3C")  # Red
                        circle_line_colors.append("white")
                
                # Add circles
                fig_combined.add_trace(
                    go.Scatter(
                        x=circle_x,
                        y=circle_y,
                        mode='markers',
                        marker=dict(
                            size=10,
                            color=circle_colors,
                            line=dict(width=1, color=circle_line_colors)
                        ),
                        hoverinfo='skip',
                        showlegend=False
                    ),
                    row=row_idx,
                    col=col_idx
                )
