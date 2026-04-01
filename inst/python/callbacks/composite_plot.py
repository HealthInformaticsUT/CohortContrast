"""Composite plot callback registration."""

from datetime import datetime
import hashlib
from pathlib import Path
from typing import Dict, List, Optional, Tuple

import numpy as np
import pandas as pd
import plotly.graph_objects as go
from dash import Input, Output, State, callback_context, no_update
from plotly.subplots import make_subplots

from callbacks.composite_helpers import (
    apply_passed_concept_filter_to_table as _apply_passed_concept_filter_to_table,
    build_passed_concept_ids as _build_passed_concept_ids,
    compute_age_axis_range as _compute_age_axis_range,
    compute_male_prop_axis_range as _compute_male_prop_axis_range,
    extract_first_time as _extract_first_time,
    expand_id_set as _expand_id_set,
    get_trace_row as _get_trace_row,
)
from callbacks.composite_cluster_columns import (
    add_cluster_columns_to_figure as _add_cluster_columns_to_figure,
)
from callbacks.composite_layout_helpers import (
    add_base_column_backgrounds as _add_base_column_backgrounds,
    add_combined_column_titles as _add_combined_column_titles,
    add_overall_age_reference_lines as _add_overall_age_reference_lines,
    add_overall_male_reference_lines as _add_overall_male_reference_lines,
    update_combined_x_axes as _update_combined_x_axes,
    update_combined_y_axes as _update_combined_y_axes,
)
from callbacks.composite_stats_helpers import (
    build_patient_cluster_age_stats as _build_patient_cluster_age_stats,
    build_patient_cluster_male_prop_stats as _build_patient_cluster_male_prop_stats,
    build_patient_cluster_prevalence_data as _build_patient_cluster_prevalence_data,
    build_summary_age_stats as _build_summary_age_stats,
    build_summary_cluster_overlay_data as _build_summary_cluster_overlay_data,
)
from callbacks.common import load_study_parquet_data
from config.constants import (
    BOTTOM_MARGIN_COMBINED,
    HERITAGE_ORDER,
    HORIZONTAL_SPACING,
    MIN_PLOT_AREA_COMBINED,
    PIXELS_PER_CONCEPT_COMBINED,
    SMALL_GAP_COMBINED,
    TOP_MARGIN_COMBINED,
    VERTICAL_SPACING,
)
from data.loader import get_available_cluster_k_values, load_clustering_file
from plots.age import calculate_age_stats, create_age_plot
from plots.composite import create_composite_plot
from plots.male_proportion import calculate_male_prop_stats, create_male_prop_plot
from plots.prevalence import create_prevalence_plot
from utils.helpers import (
    build_cluster_prevalence_lookup,
    create_empty_figure,
    extend_range_with_stats,
    filter_concepts_by_cluster_prevalence,
    filter_top_n_concepts_by_sd,
    get_unique_occurrences,
    get_default_container_style,
    is_exports_enabled,
    normalize_concept_id as _normalize_concept_id,
    safe_float,
)


def _safe_float(value) -> float:
    return safe_float(value, default=np.nan)


def _normalize_cluster_label(label) -> str:
    if label is None:
        return ""
    text = str(label).strip()
    if text.isdigit():
        return f"C{int(text)}"
    upper = text.upper()
    if upper.startswith("C") and upper[1:].isdigit():
        return f"C{int(upper[1:])}"
    return text


def _cluster_sort_key(label: str):
    normalized = _normalize_cluster_label(label)
    if normalized.startswith("C") and normalized[1:].isdigit():
        return (0, int(normalized[1:]))
    return (1, normalized)


def _first_existing_column(df: pd.DataFrame, candidates: List[str]) -> Optional[str]:
    for candidate in candidates:
        if candidate in df.columns:
            return candidate
    return None


def _build_export_heritage_groups(active_concepts: List[Dict]) -> Dict[str, List[Dict]]:
    grouped: Dict[str, List[Dict]] = {}
    for concept in active_concepts:
        heritage = concept.get("HERITAGE") or concept.get("heritage") or "unknown"
        grouped.setdefault(str(heritage), []).append(concept)

    ordered: Dict[str, List[Dict]] = {}
    for heritage in HERITAGE_ORDER:
        if heritage in grouped:
            ordered[heritage] = grouped[heritage]
    for heritage, items in grouped.items():
        if heritage not in ordered:
            ordered[heritage] = items
    return ordered


def _extract_ordinal_number(concept: Dict) -> int:
    ordinal_raw = concept.get("ORDINAL")
    if ordinal_raw is None:
        ordinal_raw = concept.get("ordinal")
    if ordinal_raw is None:
        return 0
    ordinal_val = _safe_float(ordinal_raw)
    if np.isnan(ordinal_val):
        return 0
    return int(ordinal_val)


def _is_ordinal_concept_row(concept: Dict) -> bool:
    flag = concept.get("IS_ORDINAL")
    if flag is None:
        flag = concept.get("is_ordinal")
    if isinstance(flag, str):
        return flag.strip().lower() in {"true", "1", "yes", "y"}
    try:
        return bool(flag) and not pd.isna(flag)
    except Exception:
        return bool(flag)


def _concept_stat_lookup_keys(concept: Dict) -> List[Tuple[str, object, int]]:
    """Build robust stat lookup keys (str/int concept-id variants)."""
    heritage = concept.get("HERITAGE") or concept.get("heritage") or "unknown"
    heritage_candidates = [str(heritage)]
    if "unknown" not in heritage_candidates:
        heritage_candidates.append("unknown")
    if "ALL" not in heritage_candidates:
        heritage_candidates.append("ALL")
    concept_id = concept.get("_concept_id") or concept.get("CONCEPT_ID") or concept.get("concept_id")
    original_concept_id = concept.get("ORIGINAL_CONCEPT_ID") or concept.get("original_concept_id")
    ordinal = _extract_ordinal_number(concept)
    is_ordinal = _is_ordinal_concept_row(concept)

    lookup_values = []
    primary_lookup_id = original_concept_id if is_ordinal and original_concept_id is not None else concept_id
    if primary_lookup_id is not None:
        lookup_values.append(primary_lookup_id)
    if concept_id is not None:
        lookup_values.append(concept_id)
    if original_concept_id is not None:
        lookup_values.append(original_concept_id)

    keys: List[Tuple[str, object, int]] = []
    seen = set()
    for raw_id in lookup_values:
        norm_str = _normalize_concept_id(raw_id)
        candidates: List[object] = [norm_str]
        if norm_str and norm_str.lstrip("-").isdigit():
            try:
                candidates.append(int(norm_str))
            except ValueError:
                pass
        for heritage_candidate in heritage_candidates:
            for candidate in candidates:
                key = (heritage_candidate, candidate, ordinal)
                if key not in seen:
                    seen.add(key)
                    keys.append(key)
    return keys


def _resolve_concept_stat_entry(stats: Dict, concept: Dict) -> Dict:
    """Resolve concept stats using tolerant key matching for export."""
    if not stats:
        return {}
    for key in _concept_stat_lookup_keys(concept):
        entry = stats.get(key)
        if entry is not None:
            return entry
    return {}


def _format_ci(low, high, *, decimals: int = 2) -> str:
    low_val = _safe_float(low)
    high_val = _safe_float(high)
    if np.isnan(low_val) or np.isnan(high_val):
        return ""
    return f"[{low_val:.{decimals}f}, {high_val:.{decimals}f}]"


def _normalize_person_id(value) -> str:
    if value is None:
        return ""
    value_str = str(value).replace(".0", "")
    try:
        return str(int(float(value_str)))
    except (TypeError, ValueError):
        return value_str


def _compute_patient_cluster_metrics_for_concept(
    concept: Dict,
    *,
    cluster_patients: set,
    target_patients_df: pd.DataFrame,
    concept_col: str,
    person_col: str,
    time_col: str,
    heritage_col: Optional[str],
) -> Tuple[float, float]:
    total_cluster_patients = len(cluster_patients)
    if total_cluster_patients == 0:
        return 0.0, np.nan

    is_ordinal = _is_ordinal_concept_row(concept)
    ordinal = _extract_ordinal_number(concept)
    concept_id = concept.get("CONCEPT_ID") or concept.get("_concept_id") or concept.get("concept_id")
    original_concept_id = concept.get("ORIGINAL_CONCEPT_ID") or concept.get("original_concept_id")
    lookup_concept_id = original_concept_id if is_ordinal and original_concept_id is not None else concept_id
    if lookup_concept_id is None:
        return 0.0, np.nan

    normalized_concept_id = str(_normalize_concept_id(lookup_concept_id))
    concept_data = target_patients_df[
        target_patients_df["__concept_norm"] == normalized_concept_id
    ].copy()

    heritage_value = concept.get("HERITAGE") or concept.get("heritage")
    if heritage_col and heritage_value is not None and heritage_col in concept_data.columns:
        concept_data = concept_data[
            concept_data[heritage_col].astype(str) == str(heritage_value)
        ].copy()

    if concept_data.empty:
        return 0.0, np.nan

    if is_ordinal and ordinal > 0:
        patient_occurrence_map: Dict[str, List[float]] = {}
        for _, row in concept_data[[person_col, time_col]].iterrows():
            person_norm = _normalize_person_id(row.get(person_col))
            occurrences = get_unique_occurrences(row.get(time_col))
            if not occurrences:
                continue
            valid_occurrences = []
            for occ in occurrences:
                occ_val = _safe_float(occ)
                if not np.isnan(occ_val):
                    valid_occurrences.append(occ_val)
            if not valid_occurrences:
                continue
            patient_occurrence_map.setdefault(person_norm, []).extend(valid_occurrences)

        ordinal_time_by_patient: Dict[str, float] = {}
        for person_norm, occurrences in patient_occurrence_map.items():
            unique_sorted = sorted(set(occurrences))
            if len(unique_sorted) >= ordinal:
                ordinal_time_by_patient[person_norm] = unique_sorted[ordinal - 1]

        concept_patients = set(ordinal_time_by_patient.keys())
        patients_in_cluster = concept_patients & cluster_patients
        prevalence_pct = (len(patients_in_cluster) / total_cluster_patients) * 100
        selected_times = [ordinal_time_by_patient[p] for p in patients_in_cluster if p in ordinal_time_by_patient]
        median_days = float(np.median(selected_times)) if selected_times else np.nan
        return prevalence_pct, median_days

    concept_data["__first_time"] = concept_data[time_col].apply(_extract_first_time)
    person_first_times = concept_data.groupby("__person_norm")["__first_time"].min()
    concept_patients = set(person_first_times.index.tolist())
    patients_in_cluster = concept_patients & cluster_patients
    prevalence_pct = (len(patients_in_cluster) / total_cluster_patients) * 100
    cluster_times = person_first_times[person_first_times.index.isin(patients_in_cluster)].dropna().tolist()
    median_days = float(np.median(cluster_times)) if cluster_times else np.nan
    return prevalence_pct, median_days


def register_composite_plot_callbacks(
    app,
    *,
    data_dir: Path,
    cache_store,
    loaded_parquet_data_store: Dict[str, Dict[str, pd.DataFrame]],
    plot_figure_cache_store,
    max_plot_cache_size: int,
    logger_obj,
) -> None:
    """Register callbacks responsible for composite plot rendering."""
    DATA_DIR = data_dir
    cache = cache_store
    loaded_parquet_data = loaded_parquet_data_store
    plot_figure_cache = plot_figure_cache_store
    MAX_PLOT_CACHE_SIZE = max_plot_cache_size
    logger = logger_obj
    exports_enabled = is_exports_enabled()
    @app.callback(
        [Output("composite-plot", "figure"),
         Output("composite-plot-container", "style"),
         Output("dashboard-table", "rowData", allow_duplicate=True)],  # Update table when top N SD filter is applied
        [Input("plots-update-trigger-store", "data"),
         Input("selected-study-store", "data"),
         Input("dashboard-data-store", "data"),
         Input("clustering-results-store", "data"),
         Input("cluster-view-store", "data")],
        [State("dashboard-table", "rowData"),
         State("cluster-prevalence-slider", "value"),
         State("top-n-sd-filter", "value"),
         State("divergence-cluster-scope", "value"),
         State("show-ordinal-checkbox", "value")],
        running=[
            (Output("loading-message-store", "data"), "Rendering dashboard plot...", "")
        ],
        prevent_initial_call='initial_duplicate'  # Required when using allow_duplicate=True
    )
    def update_composite_plot(
        _plots_trigger: Optional[float],
        selected_study: Optional[str],
        dashboard_data_store: Optional[List[Dict]],
        clustering_results: Optional[Dict],
        selected_cluster: Optional[str],
        row_data: Optional[List[Dict]],
        cluster_prevalence_threshold: int,
        top_n_sd_filter: Optional[int],
        divergence_cluster_scope: Optional[List[str]],
        _show_ordinal_checkbox: Optional[List[str]],
    ) -> Tuple[go.Figure, Dict, any]:  # Returns (figure, container_style, table_data or no_update)
        """
        Update the composite plot based on active concepts.
        Triggered by Apply Filters button or data/study changes.
        
        Args:
            selected_study: Currently selected study
            dashboard_data_store: Full dashboard data from store
            clustering_results: Clustering results from store
            row_data: Current table row data
            selected_cluster: Selected cluster view ("all" or "C1", "C2", etc.)
            cluster_prevalence_threshold: Minimum cluster prevalence % to show concept (0-100)
            top_n_sd_filter: Number of top concepts to show by SD of prevalence across clusters (0 = disabled)
            divergence_cluster_scope: Optional list of clusters used for SD divergence ranking
            
        Returns:
            Plotly figure
        """
        # Check which input triggered this callback
        ctx = callback_context
        trigger_id = None
        dashboard_store_triggered = False
        if ctx.triggered:
            trigger_id = ctx.triggered[0]["prop_id"].split(".")[0]
            if trigger_id == "dashboard-data-store":
                dashboard_store_triggered = True
        
        # When dashboard-data-store changes (e.g., after initial load), update table to match it
        # The dashboard_data_store already has the ordinal checkbox state applied from load_study_data
        # Only update if top N SD filter is not active (it will set updated_table_data later)
        updated_table_data = None
        if dashboard_store_triggered and dashboard_data_store and not (top_n_sd_filter and top_n_sd_filter > 0):
            # Update table to match dashboard_data_store (which respects ordinal checkbox state)
            # This ensures the table reflects the filters and ordinal checkbox state after initial load
            updated_table_data = [row.copy() for row in dashboard_data_store]
        
        # Use current table rowData as the source of truth for active concepts.
        # Fall back to dashboard_data_store only when table data is not available yet.
        data_to_use = row_data if row_data else dashboard_data_store
        
        if not data_to_use or not selected_study:
            # Transitional state during initial load/study switch:
            # keep existing figure to avoid transient blank plot flashes.
            return no_update, no_update, (updated_table_data if updated_table_data is not None else no_update)
        
        active_concepts = [row.copy() for row in data_to_use if row.get("_show") is True]
        
        if not active_concepts:
            # If this came from explicit filter/apply action, show empty-state message.
            # Otherwise keep existing figure during transitional updates.
            if trigger_id == "plots-update-trigger-store":
                return create_empty_figure("No active concepts. Check concepts in the table to display them."), get_default_container_style(), (updated_table_data if updated_table_data is not None else no_update)
            return no_update, no_update, (updated_table_data if updated_table_data is not None else no_update)

        selected_divergence_clusters = (
            [str(cluster).strip() for cluster in divergence_cluster_scope if str(cluster).strip()]
            if isinstance(divergence_cluster_scope, list)
            else []
        )
        sd_cluster_scope = selected_divergence_clusters if len(selected_divergence_clusters) >= 2 else None
        
        # Get data_patients from loaded parquet data
        # Since load_study_data is a background callback, loaded_parquet_data might not be
        # accessible in this process. Check if we have the data, and if not, try to access it.
        # If dashboard_data_store exists, data should be loaded
        if not dashboard_data_store:
            # Data is still loading in background: keep current figure to avoid blank flash.
            return no_update, no_update, (updated_table_data if updated_table_data is not None else no_update)
        
        # Try to get parquet data using centralized cache (works across background callbacks)
        parquet_data = load_study_parquet_data(
            selected_study,
            DATA_DIR,
            cache,
            loaded_parquet_data,
        )
        if parquet_data is None:
            # During load transitions, parquet may not yet be available in this worker.
            # Keep current plot instead of flashing an error/empty figure.
            return no_update, no_update, (updated_table_data if updated_table_data is not None else no_update)
        
        # Check data mode
        data_mode = parquet_data.get("_mode", "patient")
        
        # Get dashboard data for concept info
        dashboard_data = data_to_use
        
        # Get mapping data for hover tooltips
        mapping_data = parquet_data.get("complementaryMappingTable")
        
        # Note: updated_table_data may already be set if dashboard-data-store was triggered
        
        # Initialize cache_key for summary mode caching (set later after filters are applied)
        cache_key = None
        
        if data_mode == "summary":
            # SUMMARY MODE: Use pre-computed distributions
            concept_summaries = parquet_data.get("concept_summaries", pd.DataFrame())
            ordinal_summaries = parquet_data.get("ordinal_summaries", pd.DataFrame())
            
            if concept_summaries.empty:
                return create_empty_figure("Summary data not available."), get_default_container_style(), no_update
            
            # Import summary mode function
            from plots.composite import create_composite_plot_from_summary
            
            # Get clustering summary matrix for cluster overlay in summary mode
            # Load clustering data if clustering_results exists OR if top_n_sd_filter is set (even when "all" is selected)
            clustering_summary_matrix = None
            show_cluster_overlay = False
            
            # Determine k_value - use from clustering_results if available, otherwise default to 3
            k_value = 3
            if clustering_results:
                k_value = (
                    clustering_results.get("optimal_cluster_count")
                    or clustering_results.get("optimal_k")
                    or clustering_results.get("n_clusters")
                    or 3
                )
            elif top_n_sd_filter and top_n_sd_filter > 0:
                # If top_n_sd_filter is set but clustering_results is None, try to find available k
                available_k = get_available_cluster_k_values(parquet_data)
                if available_k:
                    k_value = min(available_k)  # Use smallest available k
            
            # Load clustering summary matrix if we have clustering_results OR if top_n_sd_filter is set
            if clustering_results or (top_n_sd_filter and top_n_sd_filter > 0):
                summary_key = f"clustering_k{k_value}_summary"
                if summary_key in parquet_data:
                    clustering_summary_matrix = parquet_data[summary_key]
                else:
                    # Try to load clustering file on demand
                    disease_folder = parquet_data.get("_disease_folder") or (DATA_DIR / selected_study)
                    clustering_summary_matrix = load_clustering_file(disease_folder, k_value, "summary")
                    if clustering_summary_matrix is not None:
                        # Cache it in parquet_data for future use
                        parquet_data[summary_key] = clustering_summary_matrix
                
                # Only show overlay when a specific cluster is selected (not "all")
                if selected_cluster and selected_cluster != "all":
                    show_cluster_overlay = True
            
            # Apply cluster prevalence filter when a specific cluster is selected
            # This filter is applied BEFORE the top N SD filter
            if show_cluster_overlay and clustering_summary_matrix is not None and cluster_prevalence_threshold and cluster_prevalence_threshold > 0:
                cluster_prevalence_lookup = build_cluster_prevalence_lookup(clustering_summary_matrix, selected_cluster)
                original_count = len(active_concepts)
                active_concepts = filter_concepts_by_cluster_prevalence(active_concepts, cluster_prevalence_lookup, cluster_prevalence_threshold)
                logger.info(f"  Cluster prevalence filter: {original_count} -> {len(active_concepts)} concepts (threshold: {cluster_prevalence_threshold}%)")
                
                if not active_concepts:
                    return create_empty_figure(f"No concepts with ≥{cluster_prevalence_threshold}% prevalence in Cluster {selected_cluster}."), get_default_container_style(), no_update
            
            # Apply top N SD filter LAST - after all other filters (target prevalence, ratio, heritage, cluster prevalence)
            # This ensures the top N selection is based on the concepts that passed all previous filters
            # Works for both "all" and specific cluster views
            if top_n_sd_filter and top_n_sd_filter > 0:
                if clustering_summary_matrix is None:
                    logger.warning(f"Top N SD filter is set to {top_n_sd_filter} but clustering_summary_matrix is not available. Filter will not be applied.")
                else:
                    # active_concepts at this point contains all concepts that passed:
                    # - Target prevalence filter
                    # - Ratio filter  
                    # - Heritage filter
                    # - Cluster prevalence filter (if a specific cluster is selected)
                    original_count = len(active_concepts)
                    active_concepts = filter_top_n_concepts_by_sd(
                        active_concepts,
                        clustering_summary_matrix,
                        top_n_sd_filter,
                        selected_clusters=sd_cluster_scope,
                    )
                    # When top N SD filter is applied, automatically disable all ordinals
                    ordinal_count = sum(1 for c in active_concepts if c.get("IS_ORDINAL", False))
                    active_concepts = [c for c in active_concepts if not c.get("IS_ORDINAL", False)]
                    scope_text = ",".join(sd_cluster_scope) if sd_cluster_scope else "all"
                    logger.info(
                        f"  Top N SD filter (LAST): {original_count} -> {len(active_concepts)} concepts "
                        f"(top {top_n_sd_filter} by SD, scope={scope_text}, removed {ordinal_count} ordinals)"
                    )
                    
                    if not active_concepts:
                        return create_empty_figure(f"No concepts found after filtering to top {top_n_sd_filter} by SD across clusters."), get_default_container_style(), no_update
                    
                    passed_concept_ids = _build_passed_concept_ids(active_concepts)
                    updated_table_data = _apply_passed_concept_filter_to_table(
                        data_to_use, passed_concept_ids
                    )
            
            # Check if any concepts remain after all filters are applied
            # This handles the case where filters result in 0 active concepts
            if not active_concepts:
                return create_empty_figure("After filtering, no concepts remain. Please use more lenient filters."), get_default_container_style(), (updated_table_data if updated_table_data is not None else no_update)
            
            # Generate cache key AFTER all filters are applied (cluster prevalence, top N SD)
            # Use the FINAL active_concepts list to ensure cache key matches the actual plot
            # Note: Don't use cache when top_n_sd_filter is applied, as table needs updating
            if not (top_n_sd_filter and top_n_sd_filter > 0):
                # Create deterministic cache key from final active concepts, cluster selection, and threshold
                active_concept_ids = sorted([str(row.get("_concept_id") or row.get("CONCEPT_ID", "")) for row in active_concepts])
                cache_key_parts = [
                    selected_study,
                    ",".join(active_concept_ids),
                    str(k_value),  # Include selected cluster count (k) for summary mode
                    str(selected_cluster or "all"),
                    str(cluster_prevalence_threshold),
                    str(data_mode),
                    str(top_n_sd_filter or 0)  # Include top_n_sd_filter in cache key
                ]
                cache_key = hashlib.md5("|".join(cache_key_parts).encode()).hexdigest()
                
                # Check cache BEFORE creating plot - early return if found
                if cache_key in plot_figure_cache:
                    # Move to end (most recently used)
                    plot_figure_cache.move_to_end(cache_key)
                    cached_fig, cached_style = plot_figure_cache[cache_key]
                    # Return cached figure with updated table data if needed
                    return cached_fig, cached_style, (updated_table_data if updated_table_data is not None else no_update)
            
            try:
                fig_composite, y_labels_order, heritage_groups_order = create_composite_plot_from_summary(
                    active_concepts, concept_summaries, dashboard_data,
                    ordinal_summaries=ordinal_summaries,
                    mapping_data=mapping_data,
                    selected_cluster=selected_cluster,  # Pass the actual selected_cluster (can be "all" or specific cluster)
                    clustering_summary_matrix=clustering_summary_matrix
                )
            except Exception as e:
                logger.error(f"Error creating summary plot: {e}", exc_info=True)
                raise
            
            # Initialize variables needed for later code (not used in summary mode but required to exist)
            cluster_patient_ids = None
            data_patients = pd.DataFrame()  # Empty placeholder
            data_initial = pd.DataFrame()
            data_person = pd.DataFrame()
        else:
            # PATIENT MODE: Use patient-level data
            if "data_patients" not in parquet_data:
                return create_empty_figure("Patient data not available."), get_default_container_style(), no_update
            
            data_patients = parquet_data["data_patients"]
            data_initial = parquet_data.get("data_initial", pd.DataFrame())
            data_person = parquet_data.get("data_person", pd.DataFrame())
            
            # Get cluster patient IDs if a specific cluster is selected
            cluster_patient_ids = None  # Primary cluster (red overlay)
            show_cluster_overlay = False
            
            if selected_cluster and selected_cluster != "all" and clustering_results:
                patient_assignments = clustering_results.get('patient_assignments', [])
                if patient_assignments:
                    if isinstance(patient_assignments, list):
                        df_assignments = pd.DataFrame(patient_assignments)
                    else:
                        df_assignments = patient_assignments
                    
                    if not df_assignments.empty and 'cluster' in df_assignments.columns and 'patient_id' in df_assignments.columns:
                        # Get raw cluster patient IDs for primary cluster (red)
                        raw_cluster_ids = df_assignments[df_assignments['cluster'] == selected_cluster]['patient_id'].tolist()
                        
                        # Normalize cluster_patient_ids for robust matching with data_patients
                        cluster_patient_ids = _expand_id_set(raw_cluster_ids, include_float=True)
                        
                        show_cluster_overlay = True
            
            # Apply cluster prevalence filter for patient mode when a specific cluster is selected
            if show_cluster_overlay and cluster_patient_ids and cluster_prevalence_threshold and cluster_prevalence_threshold > 0:
                # Get patient counts per concept in the cluster
                cluster_prevalence_lookup = {}
                
                # Check what column identifies patients in data_patients
                patient_col = None
                for col in ["SUBJECT_ID", "patient_id", "PATIENT_ID", "person_id", "PERSON_ID"]:
                    if col in data_patients.columns:
                        patient_col = col
                        break
                
                if patient_col:
                    # Calculate prevalence for each concept
                    cluster_patients_in_data = data_patients[data_patients[patient_col].isin(cluster_patient_ids)]
                    
                    # Get concept column
                    concept_col = None
                    for col in ["CONCEPT_ID", "concept_id"]:
                        if col in data_patients.columns:
                            concept_col = col
                            break
                    
                    if concept_col and not cluster_patients_in_data.empty:
                        # Get unique patients count in cluster
                        unique_cluster_patients = cluster_patients_in_data[patient_col].nunique()
                        if unique_cluster_patients > 0:
                            # Count unique patients per concept in cluster
                            concept_patient_counts = cluster_patients_in_data.groupby(concept_col)[patient_col].nunique()
                            for concept_id, patient_count in concept_patient_counts.items():
                                norm_id = _normalize_concept_id(concept_id)
                                prevalence_pct = (patient_count / unique_cluster_patients) * 100
                                cluster_prevalence_lookup[norm_id] = prevalence_pct
                
                # Also store string versions for robust lookup
                for concept_id in list(cluster_prevalence_lookup.keys()):
                    if isinstance(concept_id, str):
                        cluster_prevalence_lookup[str(concept_id)] = cluster_prevalence_lookup[concept_id]
                
                # Filter active_concepts using helper function
                original_count = len(active_concepts)
                active_concepts = filter_concepts_by_cluster_prevalence(active_concepts, cluster_prevalence_lookup, cluster_prevalence_threshold)
                logger.info(f"  Patient mode cluster prevalence filter: {original_count} -> {len(active_concepts)} concepts (threshold: {cluster_prevalence_threshold}%)")
                
                if not active_concepts:
                    return create_empty_figure(f"No concepts with ≥{cluster_prevalence_threshold}% prevalence in Cluster {selected_cluster}."), get_default_container_style(), no_update
            
            # Apply top N SD filter LAST for patient mode - after all other filters
            # This ensures the top N selection is based on the concepts that passed all previous filters
            if top_n_sd_filter and top_n_sd_filter > 0 and clustering_results:
                # Build summary matrix from patient data for SD calculation
                if "summary_matrix" in clustering_results and clustering_results["summary_matrix"]:
                    # Convert list of dicts to DataFrame if needed
                    if isinstance(clustering_results["summary_matrix"], list):
                        clustering_summary_matrix_patient = pd.DataFrame(clustering_results["summary_matrix"])
                    else:
                        clustering_summary_matrix_patient = clustering_results["summary_matrix"]
                    
                    if not clustering_summary_matrix_patient.empty:
                        # active_concepts at this point contains all concepts that passed:
                        # - Target prevalence filter
                        # - Ratio filter  
                        # - Heritage filter
                        # - Cluster prevalence filter (if a specific cluster is selected)
                        original_count = len(active_concepts)
                        active_concepts = filter_top_n_concepts_by_sd(
                            active_concepts,
                            clustering_summary_matrix_patient,
                            top_n_sd_filter,
                            selected_clusters=sd_cluster_scope,
                        )
                        # When top N SD filter is applied, automatically disable all ordinals
                        ordinal_count = sum(1 for c in active_concepts if c.get("IS_ORDINAL", False))
                        active_concepts = [c for c in active_concepts if not c.get("IS_ORDINAL", False)]
                        scope_text = ",".join(sd_cluster_scope) if sd_cluster_scope else "all"
                        logger.info(
                            f"  Patient mode top N SD filter (LAST): {original_count} -> {len(active_concepts)} concepts "
                            f"(top {top_n_sd_filter} by SD, scope={scope_text}, removed {ordinal_count} ordinals)"
                        )
                        
                        if not active_concepts:
                            return create_empty_figure(f"No concepts found after filtering to top {top_n_sd_filter} by SD across clusters."), get_default_container_style(), no_update
                        
                        passed_concept_ids = _build_passed_concept_ids(active_concepts)
                        updated_table_data = _apply_passed_concept_filter_to_table(
                            data_to_use, passed_concept_ids
                        )
            
            # Check if any concepts remain after all filters are applied (PATIENT MODE)
            # This handles the case where filters result in 0 active concepts
            if not active_concepts:
                return create_empty_figure("After filtering, no concepts remain. Please use more lenient filters."), get_default_container_style(), (updated_table_data if updated_table_data is not None else no_update)
            
            # Create the composite plot and get ordering information (PATIENT MODE)
            fig_composite, y_labels_order, heritage_groups_order = create_composite_plot(
                active_concepts, data_patients, dashboard_data,
                cluster_patient_ids=cluster_patient_ids,
                show_all_background=show_cluster_overlay,
                mapping_data=mapping_data
            )
        
        # Set flag for patient data availability
        has_patient_data = data_mode != "summary" and not data_patients.empty
        
        # Build cluster prevalence data for prevalence plot
        # Key structure must match: (heritage, original_concept_id_for_ordinals OR concept_id_for_main, ordinal)
        cluster_prevalence_data = None
        cluster_age_stats_summary = {}  # For summary mode cluster age stats
        cluster_male_prop_stats_summary = {}  # For summary mode cluster male prop stats
        
        # Initialize clustering_summary_matrix for patient mode to avoid NameError
        if data_mode != "summary":
            clustering_summary_matrix = None
        
        if (
            show_cluster_overlay
            and clustering_results
            and data_mode == "summary"
            and clustering_summary_matrix is not None
        ):
            (
                cluster_prevalence_data,
                cluster_age_stats_summary,
                cluster_male_prop_stats_summary,
            ) = _build_summary_cluster_overlay_data(
                clustering_summary_matrix=clustering_summary_matrix,
                selected_cluster=selected_cluster,
                concept_summaries=concept_summaries,
                ordinal_summaries=ordinal_summaries,
            )

        
        if show_cluster_overlay and clustering_results and has_patient_data:
            cluster_prevalence_data = _build_patient_cluster_prevalence_data(
                clustering_results=clustering_results,
                data_patients=data_patients,
                dashboard_data=dashboard_data,
                active_concepts=active_concepts,
                selected_cluster=selected_cluster,
            )
        
        # Create the prevalence plot using the same ordering
        fig_prevalence = create_prevalence_plot(
            active_concepts, dashboard_data, y_labels_order, heritage_groups_order,
            cluster_prevalence_data=cluster_prevalence_data,
            show_all_background=show_cluster_overlay
        )
        
        # Calculate age statistics
        if has_patient_data:
            age_stats = calculate_age_stats(
                active_concepts, data_patients, data_initial, data_person, heritage_groups_order
            )
        else:
            concept_summaries = parquet_data.get("concept_summaries", pd.DataFrame())
            ordinal_summaries = parquet_data.get("ordinal_summaries", pd.DataFrame())
            age_stats = _build_summary_age_stats(
                active_concepts,
                concept_summaries,
                ordinal_summaries,
            )
        
        time_x_min = None
        time_x_max = None
        all_ci_lows = []
        all_ci_highs = []
        for items in heritage_groups_order.values():
            for item in items:
                ci_low = item.get("q1") if item.get("q1") is not None else item.get("min")
                ci_high = item.get("q3") if item.get("q3") is not None else item.get("max")
                if ci_low is not None and not (isinstance(ci_low, float) and pd.isna(ci_low)):
                    all_ci_lows.append(ci_low)
                if ci_high is not None and not (isinstance(ci_high, float) and pd.isna(ci_high)):
                    all_ci_highs.append(ci_high)
        
        if all_ci_lows and all_ci_highs:
            time_x_min = min(all_ci_lows)
            time_x_max = max(all_ci_highs)
        
        # 2. Prevalence/Enrichment (Column 2) - calculate from prevalence plot or use 0-1
        # Prevalence is always 0-100%, so we can use 0 to 1 (or 0 to max prevalence if we want to be more precise)
        prevalence_x_min = 0.0
        prevalence_x_max = 1.0  # 100% = 1.0
        
        # Try to extract from prevalence plot if available
        if hasattr(fig_prevalence.layout, 'xaxis') and hasattr(fig_prevalence.layout.xaxis, 'range'):
            prev_range = fig_prevalence.layout.xaxis.range
            if prev_range:
                prevalence_x_min = prev_range[0]
                prevalence_x_max = prev_range[1]
        
        # 3. Age (Column 3) - calculate from age statistics
        age_x_min = None
        age_x_max = None
        overall_avg_age = None
        
        if age_stats and not data_patients.empty and not data_initial.empty and not data_person.empty:
            # Calculate overall average age from all unique people in the target cohort
            # This is the true study average, not person-concept combinations
            all_mean_ages = []
            
            # Create mappings for age calculation
            # IMPORTANT: Filter to target cohort first to avoid using control cohort dates
            target_initial = data_initial[data_initial['COHORT_DEFINITION_ID'] == 'target']
            initial_map = target_initial.set_index('SUBJECT_ID')['COHORT_START_DATE'].to_dict()
            person_map = data_person.set_index('PERSON_ID')['YEAR_OF_BIRTH'].to_dict()
            
            # Get all unique people in the target cohort
            target_patients = data_initial[data_initial['COHORT_DEFINITION_ID'] == 'target']['SUBJECT_ID'].unique()
            
            # Calculate ages for all unique people in target cohort
            all_individual_ages = []
            for person_id in target_patients:
                birth_year = person_map.get(person_id)
                if birth_year is None or pd.isna(birth_year):
                    continue
                
                cohort_start = initial_map.get(person_id)
                if cohort_start is None or pd.isna(cohort_start):
                    continue
                
                try:
                    cohort_date = pd.to_datetime(cohort_start)
                    birth_year_int = int(birth_year)
                    age = cohort_date.year - birth_year_int
                    
                    if age >= 0 and age <= 150:
                        all_individual_ages.append(age)
                except (ValueError, TypeError, AttributeError):
                    continue
            
            # Calculate overall average from all unique people in target cohort
            if all_individual_ages:
                overall_avg_age = np.mean(all_individual_ages)
            
            # For x-axis range, use mean ages only (not CIs)
            # CIs are for error bars, not axis limits
            all_mean_ages = [stats["mean_age"] for stats in age_stats.values()]
            age_x_min, age_x_max = _compute_age_axis_range(all_mean_ages)
        elif age_stats and data_mode == "summary":
            # SUMMARY MODE: Read overall_avg_age from metadata
            metadata = parquet_data.get("_metadata", {})
            demographics = metadata.get("demographics", {})
            overall_avg_age = demographics.get("age_mean")
            
            # Calculate x-axis range from mean ages only (not CIs)
            # CIs are for error bars, not axis limits
            # Age means span a natural range; CIs from small samples can be extreme
            all_mean_ages = [stats["mean_age"] for stats in age_stats.values()]
            age_x_min, age_x_max = _compute_age_axis_range(all_mean_ages)
        
        cluster_age_stats = {}
        if show_cluster_overlay and cluster_patient_ids:
            cluster_age_stats = _build_patient_cluster_age_stats(
                heritage_groups_order=heritage_groups_order,
                data_patients=data_patients,
                data_initial=data_initial,
                data_person=data_person,
                cluster_patient_ids=cluster_patient_ids,
            )
        
        # Create the age plot using the same ordering
        # Use cluster_age_stats_summary for summary mode, cluster_age_stats for patient mode
        effective_cluster_age_stats = None
        if show_cluster_overlay:
            if data_mode == "summary" and cluster_age_stats_summary:
                effective_cluster_age_stats = cluster_age_stats_summary
            elif cluster_age_stats:
                effective_cluster_age_stats = cluster_age_stats
        
        fig_age = create_age_plot(
            heritage_groups_order, age_stats, overall_avg_age,
            cluster_age_stats=effective_cluster_age_stats,
            show_cluster_overlay=show_cluster_overlay,
            show_error_bars=True  # Always show error bars (same as patient mode)
        )
        
        displayed_keys = set()
        for heritage, items in heritage_groups_order.items():
            for item in items:
                cid = item.get("concept_id") or item.get("_concept_id") or item.get("CONCEPT_ID")
                ordinal = item.get("ordinal") or item.get("ORDINAL") or 0
                original_cid = item.get("original_concept_id") or item.get("ORIGINAL_CONCEPT_ID")
                is_ordinal = item.get("is_ordinal") or item.get("IS_ORDINAL") or False
                lookup_cid = original_cid if is_ordinal and original_cid else cid
                if lookup_cid is not None:
                    try:
                        displayed_keys.add((heritage, int(float(str(lookup_cid).replace(".0", ""))), ordinal))
                    except (ValueError, TypeError):
                        displayed_keys.add((heritage, str(lookup_cid).replace(".0", ""), ordinal))
        
        if effective_cluster_age_stats:
            filtered_cluster_age_stats = {k: v for k, v in effective_cluster_age_stats.items() if k in displayed_keys}
            if filtered_cluster_age_stats:
                age_x_min, age_x_max = extend_range_with_stats(
                    age_x_min, age_x_max, filtered_cluster_age_stats,
                    mean_key="mean_age", ci_low_key="ci_low", ci_high_key="ci_high",
                    padding_ratio=0.0, default_padding=0.0
                )
        
        # Calculate male proportion statistics
        if has_patient_data:
            male_prop_stats = calculate_male_prop_stats(
                active_concepts, data_patients, data_initial, data_person, heritage_groups_order
            )
        else:
            # In summary mode, extract male proportion from age_stats (which now includes male_proportion)
            male_prop_stats = {}
            for key, stats in age_stats.items():
                male_prop = stats.get("male_proportion")
                if male_prop is not None and not pd.isna(male_prop):
                    n = stats.get("n", 0)
                    # Calculate binomial CI
                    if n > 1:
                        se = np.sqrt(male_prop * (1 - male_prop) / n)
                        z_critical = 1.96
                        ci_low = max(0, male_prop - z_critical * se)
                        ci_high = min(1, male_prop + z_critical * se)
                    else:
                        ci_low = male_prop
                        ci_high = male_prop
                    
                    male_prop_stats[key] = {
                        "mean_male_prop": male_prop,
                        "ci_low": ci_low,
                        "ci_high": ci_high,
                        "n": n
                    }
        
        # Calculate overall average male proportion from all unique people in the target cohort
        overall_avg_male_prop = None
        male_prop_x_min = 0.0
        male_prop_x_max = 1.0
        
        if male_prop_stats and has_patient_data and not data_patients.empty and not data_person.empty:
            # Get all unique people in the target cohort
            target_patients = data_initial[data_initial['COHORT_DEFINITION_ID'] == 'target']['SUBJECT_ID'].unique()
            
            # Create mapping from PERSON_ID to GENDER_CONCEPT_ID
            person_map = data_person.set_index('PERSON_ID')['GENDER_CONCEPT_ID'].to_dict()
            
            # Calculate male proportions for all unique people in target cohort
            all_male_indicators = []
            for person_id in target_patients:
                gender_concept_id = person_map.get(person_id)
                if gender_concept_id is None or pd.isna(gender_concept_id):
                    continue
                # 8507 = male
                is_male = (gender_concept_id == 8507)
                all_male_indicators.append(1 if is_male else 0)
            
            # Calculate overall average from all unique people in target cohort
            if all_male_indicators:
                overall_avg_male_prop = np.mean(all_male_indicators)
            
            # For x-axis range, use mean proportions and CIs from stats
            all_mean_props = []
            all_ci_lows = []
            all_ci_highs = []
            
            for stats in male_prop_stats.values():
                all_mean_props.append(stats["mean_male_prop"])
                all_ci_lows.append(stats["ci_low"])
                all_ci_highs.append(stats["ci_high"])
            
            male_prop_x_min, male_prop_x_max = _compute_male_prop_axis_range(
                all_mean_props, all_ci_lows, all_ci_highs
            )
        elif male_prop_stats and data_mode == "summary":
            # SUMMARY MODE: Read overall_avg_male_prop from metadata
            metadata = parquet_data.get("_metadata", {})
            demographics = metadata.get("demographics", {})
            overall_avg_male_prop = demographics.get("male_proportion")
            
            # Calculate x-axis range from male_prop_stats
            all_mean_props = []
            all_ci_lows = []
            all_ci_highs = []
            
            for stats in male_prop_stats.values():
                all_mean_props.append(stats["mean_male_prop"])
                all_ci_lows.append(stats["ci_low"])
                all_ci_highs.append(stats["ci_high"])
            
            male_prop_x_min, male_prop_x_max = _compute_male_prop_axis_range(
                all_mean_props, all_ci_lows, all_ci_highs
            )
        
        cluster_male_prop_stats = {}
        if show_cluster_overlay and cluster_patient_ids:
            cluster_male_prop_stats = _build_patient_cluster_male_prop_stats(
                heritage_groups_order=heritage_groups_order,
                data_patients=data_patients,
                data_person=data_person,
                cluster_patient_ids=cluster_patient_ids,
            )
        
        # Create the male proportion plot using the same ordering
        # Use cluster_male_prop_stats_summary for summary mode, cluster_male_prop_stats for patient mode
        effective_cluster_male_prop_stats = None
        if show_cluster_overlay:
            if data_mode == "summary" and cluster_male_prop_stats_summary:
                effective_cluster_male_prop_stats = cluster_male_prop_stats_summary
            elif cluster_male_prop_stats:
                effective_cluster_male_prop_stats = cluster_male_prop_stats
        
        fig_male_prop, male_prop_x_min, male_prop_x_max, _ = create_male_prop_plot(
            heritage_groups_order, male_prop_stats, overall_avg_male_prop,
            cluster_male_prop_stats=effective_cluster_male_prop_stats,
            show_cluster_overlay=show_cluster_overlay,
            show_error_bars=True  # Always show error bars (same as patient mode)
        )
        
        if effective_cluster_male_prop_stats:
            filtered_cluster_male_prop_stats = {k: v for k, v in effective_cluster_male_prop_stats.items() if k in displayed_keys}
            if filtered_cluster_male_prop_stats:
                male_prop_x_min, male_prop_x_max = extend_range_with_stats(
                    male_prop_x_min, male_prop_x_max, filtered_cluster_male_prop_stats,
                    mean_key="mean_male_prop", ci_low_key="ci_low", ci_high_key="ci_high",
                    padding_ratio=0.0, default_padding=0.0
                )
            if male_prop_x_min is not None:
                male_prop_x_min = max(0, male_prop_x_min)
            if male_prop_x_max is not None:
                male_prop_x_max = min(1, male_prop_x_max)
        
        # Calculate row heights based on concepts per heritage (30px per concept)
        concepts_per_heritage = {}
        for heritage, items in heritage_groups_order.items():
            concepts_per_heritage[heritage] = len(items)
        
        # Get heritages in order (only those that have data AND have at least 1 concept)
        heritages_with_data = [h for h in HERITAGE_ORDER if h in heritage_groups_order and concepts_per_heritage.get(h, 0) > 0]
        num_rows = len(heritages_with_data)
        
        # Also remove empty heritages from heritage_groups_order to keep consistent
        heritage_groups_order = {h: heritage_groups_order[h] for h in heritages_with_data}
        
        # Handle case where no heritages have concepts
        if num_rows == 0:
            return create_empty_figure("No concepts to display after filtering."), get_default_container_style(), (updated_table_data if updated_table_data is not None else no_update)
        
        # Calculate row heights: use actual item count from heritage_groups_order for accurate heights
        # This ensures the height reflects the filtered concepts, not the original counts
        row_heights = []
        for h in heritages_with_data:
            item_count = len(heritage_groups_order.get(h, []))
            row_heights.append(max(1, item_count) * 30)  # Ensure at least 30px per row to avoid zero height
        
        # Check if clustering results are available.
        # Be tolerant to differing payload schemas (optimal_cluster_count vs optimal_k/n_clusters).
        effective_clustering_results = clustering_results
        optimal_k = 0
        cluster_data_available = False

        # Summary-mode fallback: build minimal clustering payload from loaded summary matrix.
        if (
            effective_clustering_results is None
            and data_mode == "summary"
            and clustering_summary_matrix is not None
            and not clustering_summary_matrix.empty
        ):
            cluster_col = "cluster" if "cluster" in clustering_summary_matrix.columns else (
                "CLUSTER" if "CLUSTER" in clustering_summary_matrix.columns else None
            )
            if cluster_col:
                unique_clusters = [str(c) for c in clustering_summary_matrix[cluster_col].dropna().unique().tolist()]
                cluster_counts = {}
                count_col = "total_cluster_patients" if "total_cluster_patients" in clustering_summary_matrix.columns else (
                    "TOTAL_CLUSTER_PATIENTS" if "TOTAL_CLUSTER_PATIENTS" in clustering_summary_matrix.columns else None
                )
                if count_col:
                    for cluster_label in unique_clusters:
                        cluster_rows = clustering_summary_matrix[
                            clustering_summary_matrix[cluster_col].astype(str) == cluster_label
                        ]
                        if not cluster_rows.empty:
                            try:
                                cluster_counts[cluster_label] = int(cluster_rows[count_col].iloc[0])
                            except Exception:
                                continue

                effective_clustering_results = {
                    "summary_matrix": clustering_summary_matrix.to_dict("records"),
                    "patient_assignments": [],
                    "cluster_counts": cluster_counts,
                    "optimal_cluster_count": len(unique_clusters),
                    "_mode": "summary",
                }

        if effective_clustering_results:
            optimal_k = (
                effective_clustering_results.get('optimal_cluster_count')
                or effective_clustering_results.get('optimal_k')
                or effective_clustering_results.get('n_clusters')
                or 0
            )
            summary_matrix_data = effective_clustering_results.get('summary_matrix', [])
            if not optimal_k and isinstance(summary_matrix_data, list) and summary_matrix_data:
                try:
                    summary_df_tmp = pd.DataFrame(summary_matrix_data)
                    cluster_col = "cluster" if "cluster" in summary_df_tmp.columns else ("CLUSTER" if "CLUSTER" in summary_df_tmp.columns else None)
                    if cluster_col:
                        optimal_k = int(summary_df_tmp[cluster_col].nunique())
                except Exception:
                    optimal_k = 0
            if optimal_k > 0 and bool(summary_matrix_data):
                cluster_data_available = True
        
        # Create combined figure with 4 base columns + cluster columns if available
        # Base columns: Event occurrences, Prevalence/Enrichment, Age, Male %
        base_cols = 4
        num_cols = base_cols + (optimal_k if cluster_data_available else 0)
        
        # Calculate column widths dynamically
        if cluster_data_available:
            # Shrink base columns to make more room for cluster columns
            # Base: 0.30, 0.12, 0.12, 0.12 = 0.66, leaving 0.34 for clusters
            cluster_width = 0.34 / optimal_k if optimal_k > 0 else 0.15
            column_widths = [0.30, 0.12, 0.12, 0.12] + [cluster_width] * optimal_k
        else:
            column_widths = [0.4, 0.2, 0.2, 0.2]  # Original 4 columns
        
        fig_combined = make_subplots(
            rows=num_rows,
            cols=num_cols,
            column_widths=column_widths,
            row_heights=row_heights,
            shared_yaxes=True,
            horizontal_spacing=HORIZONTAL_SPACING,
            vertical_spacing=VERTICAL_SPACING,
            subplot_titles=([None] * (num_rows * num_cols))
        )
        
        # Copy traces from composite plot to column 1
        for trace in fig_composite.data:
            row_num = _get_trace_row(trace, 1)
            fig_combined.add_trace(trace, row=row_num, col=1)
        
        # Copy traces from prevalence plot to column 2
        for trace in fig_prevalence.data:
            row_num = _get_trace_row(trace, 1)
            fig_combined.add_trace(trace, row=row_num, col=2)
        
        # Copy traces from age plot to column 3
        for trace in fig_age.data:
            row_num = _get_trace_row(trace, 1)
            fig_combined.add_trace(trace, row=row_num, col=3)
        
        # Copy traces from male proportion plot to column 4
        for trace in fig_male_prop.data:
            row_num = _get_trace_row(trace, 1)
            fig_combined.add_trace(trace, row=row_num, col=4)
        
        # Add cluster columns if clustering data is available
        if cluster_data_available and effective_clustering_results:
            _add_cluster_columns_to_figure(
                fig_combined,
                clustering_results=effective_clustering_results,
                heritage_groups_order=heritage_groups_order,
                heritages_with_data=heritages_with_data,
                base_cols=base_cols,
                optimal_k=optimal_k,
                dashboard_data=dashboard_data,
                data_patients=data_patients,
            )
        
        # Calculate proper height for combined figure using constants from config
        # Recalculate concepts_per_heritage to ensure it reflects actual filtered items
        # This is needed because heritage_groups_order might have been filtered
        concepts_per_heritage_actual = {}
        for h in heritages_with_data:
            concepts_per_heritage_actual[h] = len(heritage_groups_order.get(h, []))
        
        total_concepts_combined = sum(concepts_per_heritage_actual[h] for h in heritages_with_data)
        plot_area_combined = max(MIN_PLOT_AREA_COMBINED, total_concepts_combined * PIXELS_PER_CONCEPT_COMBINED)
        heritage_gaps_combined = (num_rows - 1) * SMALL_GAP_COMBINED if num_rows > 1 else 0
        # Height includes data rows + gaps + margins
        combined_height = TOP_MARGIN_COMBINED + plot_area_combined + heritage_gaps_combined + BOTTOM_MARGIN_COMBINED
        
        # Update layout from composite plot - fixed margins
        fig_combined.update_layout(
            title="",  # No main title
            height=combined_height,
            showlegend=show_cluster_overlay,  # Show legend when cluster is selected
            legend=dict(
                orientation="h",
                yanchor="bottom",
                y=1.02,
                xanchor="left",
                x=0,
                font=dict(size=10)
            ) if show_cluster_overlay else {},
            plot_bgcolor="white",
            paper_bgcolor="white",
            margin=dict(l=200, r=50, t=TOP_MARGIN_COMBINED, b=BOTTOM_MARGIN_COMBINED),  # Fixed margins
            dragmode=False,
            hovermode="closest",  # Use closest point hover
            hoverlabel=dict(
                bgcolor='white',
                bordercolor='#ccc',
                font=dict(color='black', size=12),
                align='left'
            )
        )
        
        _add_combined_column_titles(
            fig_combined,
            column_widths,
            cluster_data_available=cluster_data_available,
            optimal_k=optimal_k,
            clustering_results=effective_clustering_results,
        )
        _add_base_column_backgrounds(
            fig_combined, base_cols=base_cols, column_widths=column_widths
        )
        _update_combined_x_axes(
            fig_combined,
            num_rows=num_rows,
            time_x_min=time_x_min,
            time_x_max=time_x_max,
            prevalence_x_min=prevalence_x_min,
            prevalence_x_max=prevalence_x_max,
            age_x_min=age_x_min,
            age_x_max=age_x_max,
            male_prop_x_min=male_prop_x_min,
            male_prop_x_max=male_prop_x_max,
            cluster_data_available=cluster_data_available,
            optimal_k=optimal_k,
            base_cols=base_cols,
        )
        _add_overall_male_reference_lines(
            fig_combined,
            overall_avg_male_prop=overall_avg_male_prop,
            heritages_with_data=heritages_with_data,
            heritage_groups_order=heritage_groups_order,
        )
        _update_combined_y_axes(
            fig_combined,
            fig_composite,
            num_rows=num_rows,
            cluster_data_available=cluster_data_available,
            optimal_k=optimal_k,
            base_cols=base_cols,
        )
        _add_overall_age_reference_lines(
            fig_combined,
            overall_avg_age=overall_avg_age,
            heritages_with_data=heritages_with_data,
            heritage_groups_order=heritage_groups_order,
        )
        
        # Calculate container height based on plot height plus UI chrome
        # (heading, controls, legend, and loading wrapper), to prevent overlap
        # with the dashboard table on smaller screens.
        plot_height = fig_combined.layout.height if hasattr(fig_combined.layout, 'height') and fig_combined.layout.height else 400
        container_chrome_height = 320
        container_min_height = int(plot_height) + container_chrome_height
        
        # Set explicit height but allow width to adapt
        fig_combined.update_layout(
            autosize=True,
            height=plot_height
        )
        
        # Container style: allow natural growth and enforce a protective minimum.
        # The clientside callback re-evaluates minHeight on tab switches/resizes.
        container_style = {
            "width": "100%",
            "marginBottom": "60px",
            "overflow": "visible",
            "height": "auto",
            "minHeight": f"{container_min_height}px",
        }
        
        # Store in cache (LRU eviction) - cache the complete combined figure
        # Cache key was generated earlier (before plot creation) for summary mode
        # Only store if cache_key exists (summary mode and not using top_n_sd_filter)
        if data_mode == "summary" and cache_key is not None:
            # Remove oldest entry if cache is full
            if len(plot_figure_cache) >= MAX_PLOT_CACHE_SIZE:
                plot_figure_cache.popitem(last=False)  # Remove oldest (first) item
            # Store new figure and move to end (most recently used)
            plot_figure_cache[cache_key] = (fig_combined, container_style)
            plot_figure_cache.move_to_end(cache_key)
        
        # Return figure, container style, and updated table data (if top N SD filter was applied)
        return fig_combined, container_style, (updated_table_data if updated_table_data is not None else no_update)

    @app.callback(
        Output("dashboard-export-download", "data"),
        Input("download-dashboard-tsv-btn", "n_clicks"),
        [
            State("dashboard-table", "rowData"),
            State("dashboard-data-store", "data"),
            State("selected-study-store", "data"),
            State("clustering-results-store", "data"),
            State("data-mode-store", "data"),
        ],
        prevent_initial_call=True,
    )
    def export_active_concepts_tsv(
        n_clicks: Optional[int],
        row_data: Optional[List[Dict]],
        dashboard_data_store: Optional[List[Dict]],
        selected_study: Optional[str],
        clustering_results: Optional[Dict],
        data_mode_store: Optional[str],
    ):
        if not exports_enabled:
            return no_update
        if not n_clicks or not selected_study:
            return no_update

        data_to_use = row_data if row_data else dashboard_data_store
        if not data_to_use:
            return no_update

        active_concepts = [row.copy() for row in data_to_use if row.get("_show") is True]
        if not active_concepts:
            return no_update

        parquet_data = load_study_parquet_data(
            selected_study,
            DATA_DIR,
            cache,
            loaded_parquet_data,
        )
        if parquet_data is None:
            return no_update

        data_mode = parquet_data.get("_mode", data_mode_store or "patient")
        heritage_groups_order = _build_export_heritage_groups(active_concepts)

        age_stats = {}
        male_prop_stats = {}

        if data_mode == "summary":
            concept_summaries = parquet_data.get("concept_summaries", pd.DataFrame())
            ordinal_summaries = parquet_data.get("ordinal_summaries", pd.DataFrame())
            age_stats = _build_summary_age_stats(
                active_concepts,
                concept_summaries,
                ordinal_summaries,
            )
            for key, stats in age_stats.items():
                male_prop = _safe_float(stats.get("male_proportion"))
                if np.isnan(male_prop):
                    continue
                n = _safe_float(stats.get("n"))
                if np.isnan(n) or n <= 1:
                    ci_low = male_prop
                    ci_high = male_prop
                else:
                    se = np.sqrt(male_prop * (1 - male_prop) / n)
                    margin = 1.96 * se
                    ci_low = max(0, male_prop - margin)
                    ci_high = min(1, male_prop + margin)
                male_prop_stats[key] = {
                    "mean_male_prop": male_prop,
                    "ci_low": ci_low,
                    "ci_high": ci_high,
                    "n": n,
                }
        else:
            data_patients = parquet_data.get("data_patients", pd.DataFrame())
            data_initial = parquet_data.get("data_initial", pd.DataFrame())
            data_person = parquet_data.get("data_person", pd.DataFrame())
            if not data_patients.empty and not data_initial.empty and not data_person.empty:
                age_stats = calculate_age_stats(
                    active_concepts, data_patients, data_initial, data_person, heritage_groups_order
                )
                male_prop_stats = calculate_male_prop_stats(
                    active_concepts, data_patients, data_initial, data_person, heritage_groups_order
                )

        summary_matrix = pd.DataFrame()
        if clustering_results and clustering_results.get("summary_matrix"):
            summary_data = clustering_results.get("summary_matrix", [])
            summary_matrix = pd.DataFrame(summary_data) if isinstance(summary_data, list) else summary_data

        if summary_matrix is None or summary_matrix.empty:
            summary_matrix = pd.DataFrame()
            if data_mode == "summary":
                k_value = 3
                if clustering_results:
                    k_value = (
                        clustering_results.get("optimal_cluster_count")
                        or clustering_results.get("optimal_k")
                        or clustering_results.get("n_clusters")
                        or 3
                    )
                else:
                    available_k = get_available_cluster_k_values(parquet_data)
                    if available_k:
                        k_value = min(available_k)
                summary_key = f"clustering_k{k_value}_summary"
                summary_matrix = parquet_data.get(summary_key, pd.DataFrame())
                if summary_matrix is None or summary_matrix.empty:
                    disease_folder = parquet_data.get("_disease_folder") or (DATA_DIR / selected_study)
                    loaded_summary = load_clustering_file(disease_folder, k_value, "summary")
                    if loaded_summary is not None:
                        summary_matrix = loaded_summary

        summary_matrix = summary_matrix if isinstance(summary_matrix, pd.DataFrame) else pd.DataFrame()

        summary_concept_col = _first_existing_column(summary_matrix, ["CONCEPT_ID", "concept_id"])
        summary_cluster_col = _first_existing_column(summary_matrix, ["cluster", "CLUSTER"])
        summary_prevalence_col = _first_existing_column(summary_matrix, ["prevalence", "PREVALENCE"])
        summary_median_col = _first_existing_column(
            summary_matrix,
            ["time_median", "median_days", "MEDIAN_FIRST_OCCURRENCE", "TIME_MEDIAN", "MEDIAN_DAYS"],
        )
        summary_ordinal_col = _first_existing_column(summary_matrix, ["ORDINAL", "ordinal"])

        if not summary_matrix.empty and summary_concept_col and summary_cluster_col:
            summary_matrix = summary_matrix.copy()
            summary_matrix["__concept_norm"] = summary_matrix[summary_concept_col].apply(
                lambda cid: str(_normalize_concept_id(cid))
            )
            summary_matrix["__cluster_norm"] = summary_matrix[summary_cluster_col].apply(_normalize_cluster_label)
            if summary_ordinal_col:
                summary_matrix["__ordinal_num"] = pd.to_numeric(
                    summary_matrix[summary_ordinal_col], errors="coerce"
                ).fillna(0).astype(int)

        cluster_labels: List[str] = []
        if not summary_matrix.empty and "__cluster_norm" in summary_matrix.columns:
            cluster_labels = sorted(
                [c for c in summary_matrix["__cluster_norm"].dropna().unique().tolist() if c],
                key=_cluster_sort_key,
            )
        elif clustering_results:
            cluster_counts = clustering_results.get("cluster_counts", {})
            if cluster_counts:
                cluster_labels = sorted(
                    [_normalize_cluster_label(label) for label in cluster_counts.keys()],
                    key=_cluster_sort_key,
                )
            if not cluster_labels:
                k_val = (
                    clustering_results.get("optimal_cluster_count")
                    or clustering_results.get("optimal_k")
                    or clustering_results.get("n_clusters")
                    or 0
                )
                if k_val:
                    cluster_labels = [f"C{i + 1}" for i in range(int(k_val))]

        cluster_labels = [label for label in cluster_labels if label]

        cluster_patient_map: Dict[str, set] = {}
        target_patients_df = pd.DataFrame()
        patient_concept_col = None
        patient_person_col = None
        patient_time_col = None
        patient_heritage_col = None

        if cluster_labels and data_mode != "summary" and clustering_results:
            data_patients = parquet_data.get("data_patients", pd.DataFrame())
            patient_assignments_data = clustering_results.get("patient_assignments", [])
            patient_assignments_df = (
                pd.DataFrame(patient_assignments_data)
                if isinstance(patient_assignments_data, list)
                else patient_assignments_data
            )
            if not isinstance(patient_assignments_df, pd.DataFrame):
                patient_assignments_df = pd.DataFrame()

            assignment_patient_col = _first_existing_column(
                patient_assignments_df, ["patient_id", "PERSON_ID", "person_id", "SUBJECT_ID", "subject_id"]
            )
            assignment_cluster_col = _first_existing_column(patient_assignments_df, ["cluster", "CLUSTER"])

            cohort_col = _first_existing_column(data_patients, ["COHORT_DEFINITION_ID", "cohort_definition_id"])
            patient_concept_col = _first_existing_column(data_patients, ["CONCEPT_ID", "concept_id"])
            patient_person_col = _first_existing_column(
                data_patients, ["PERSON_ID", "person_id", "SUBJECT_ID", "subject_id", "patient_id"]
            )
            patient_time_col = _first_existing_column(data_patients, ["TIME_TO_EVENT", "time_to_event"])
            patient_heritage_col = _first_existing_column(data_patients, ["HERITAGE", "heritage"])

            if (
                not data_patients.empty
                and not patient_assignments_df.empty
                and assignment_patient_col
                and assignment_cluster_col
                and cohort_col
                and patient_concept_col
                and patient_person_col
                and patient_time_col
            ):
                target_patients_df = data_patients[
                    data_patients[cohort_col].astype(str).str.lower() == "target"
                ].copy()
                if not target_patients_df.empty:
                    target_patients_df["__concept_norm"] = target_patients_df[patient_concept_col].apply(
                        lambda cid: str(_normalize_concept_id(cid))
                    )
                    target_patients_df["__person_norm"] = target_patients_df[patient_person_col].apply(
                        _normalize_person_id
                    )

                    patient_assignments_df = patient_assignments_df.copy()
                    patient_assignments_df["__person_norm"] = patient_assignments_df[assignment_patient_col].apply(
                        _normalize_person_id
                    )
                    patient_assignments_df["__cluster_norm"] = patient_assignments_df[assignment_cluster_col].apply(
                        _normalize_cluster_label
                    )
                    for cluster_label in cluster_labels:
                        cluster_patient_map[cluster_label] = set(
                            patient_assignments_df[
                                patient_assignments_df["__cluster_norm"] == cluster_label
                            ]["__person_norm"].tolist()
                        )

        export_rows = []
        for concept in active_concepts:
            concept_name = concept.get("CONCEPT_NAME") or concept.get("concept_name") or ""
            concept_id = concept.get("CONCEPT_ID") or concept.get("_concept_id") or concept.get("concept_id")
            heritage = concept.get("HERITAGE") or concept.get("heritage") or ""
            ordinal_number = _extract_ordinal_number(concept)

            age_entry = _resolve_concept_stat_entry(age_stats, concept)
            male_entry = _resolve_concept_stat_entry(male_prop_stats, concept)

            target_prev_pct = _safe_float(concept.get("TARGET_SUBJECT_PREVALENCE_PCT"))
            if np.isnan(target_prev_pct):
                base_prev = _safe_float(concept.get("TARGET_SUBJECT_PREVALENCE"))
                if not np.isnan(base_prev):
                    target_prev_pct = base_prev * 100

            enrichment_ratio = _safe_float(concept.get("PREVALENCE_DIFFERENCE_RATIO_DISPLAY"))
            if np.isnan(enrichment_ratio):
                enrichment_ratio = _safe_float(concept.get("PREVALENCE_DIFFERENCE_RATIO"))

            mean_age = _safe_float(age_entry.get("mean_age"))
            age_ci_low = _safe_float(age_entry.get("ci_low"))
            age_ci_high = _safe_float(age_entry.get("ci_high"))

            male_prop = _safe_float(male_entry.get("mean_male_prop"))
            male_ci_low = _safe_float(male_entry.get("ci_low"))
            male_ci_high = _safe_float(male_entry.get("ci_high"))

            row = {
                "Concept Name": concept_name,
                "Concept ID": concept_id,
                "Domain": heritage,
                "Ordinal Index": ordinal_number if ordinal_number > 0 else "",
                "Median Time to First Occurrence (Days)": _safe_float(concept.get("MEDIAN_FIRST_OCCURRENCE")),
                "Target Prevalence (%)": target_prev_pct,
                "Enrichment Ratio (Target vs Control)": enrichment_ratio,
                "Mean Age (Years)": mean_age,
                "Age 95% CI Lower (Years)": age_ci_low,
                "Age 95% CI Upper (Years)": age_ci_high,
                "Age 95% CI (Years)": _format_ci(age_ci_low, age_ci_high),
                "Male Proportion (%)": male_prop * 100 if not np.isnan(male_prop) else np.nan,
                "Male Proportion 95% CI Lower (%)": male_ci_low * 100 if not np.isnan(male_ci_low) else np.nan,
                "Male Proportion 95% CI Upper (%)": male_ci_high * 100 if not np.isnan(male_ci_high) else np.nan,
                "Male Proportion 95% CI (%)": _format_ci(
                    male_ci_low * 100 if not np.isnan(male_ci_low) else np.nan,
                    male_ci_high * 100 if not np.isnan(male_ci_high) else np.nan,
                ),
            }

            for cluster_label in cluster_labels:
                prevalence_pct = np.nan
                median_days = np.nan

                if not summary_matrix.empty and summary_concept_col and summary_cluster_col:
                    is_ordinal = _is_ordinal_concept_row(concept)
                    original_concept_id = concept.get("ORIGINAL_CONCEPT_ID") or concept.get("original_concept_id")
                    concept_candidates = [
                        concept.get("CONCEPT_ID"),
                        concept.get("_concept_id"),
                        concept.get("concept_id"),
                    ]
                    if original_concept_id is not None:
                        concept_candidates.append(original_concept_id)
                    concept_candidates = [
                        str(_normalize_concept_id(candidate))
                        for candidate in concept_candidates
                        if candidate is not None
                    ]
                    concept_candidates = list(dict.fromkeys(concept_candidates))

                    concept_subset = summary_matrix[
                        (summary_matrix["__cluster_norm"] == cluster_label)
                        & (summary_matrix["__concept_norm"].isin(concept_candidates))
                    ]

                    if is_ordinal and ordinal_number > 0 and "__ordinal_num" in summary_matrix.columns:
                        ordinal_subset = concept_subset[concept_subset["__ordinal_num"] == ordinal_number]
                        if not ordinal_subset.empty:
                            concept_subset = ordinal_subset

                    if concept_subset.empty and is_ordinal and original_concept_id is not None:
                        parent_norm = str(_normalize_concept_id(original_concept_id))
                        parent_subset = summary_matrix[
                            (summary_matrix["__cluster_norm"] == cluster_label)
                            & (summary_matrix["__concept_norm"] == parent_norm)
                        ]
                        if not parent_subset.empty:
                            concept_subset = parent_subset

                    if not concept_subset.empty:
                        selected_row = concept_subset.iloc[0]
                        if summary_prevalence_col:
                            prevalence_raw = _safe_float(selected_row.get(summary_prevalence_col))
                            if not np.isnan(prevalence_raw):
                                prevalence_pct = prevalence_raw * 100 if prevalence_raw <= 1 else prevalence_raw
                        if summary_median_col:
                            median_days = _safe_float(selected_row.get(summary_median_col))

                if (
                    (np.isnan(prevalence_pct) or np.isnan(median_days))
                    and not target_patients_df.empty
                    and cluster_label in cluster_patient_map
                    and patient_concept_col
                    and patient_person_col
                    and patient_time_col
                ):
                    patient_prev, patient_median = _compute_patient_cluster_metrics_for_concept(
                        concept,
                        cluster_patients=cluster_patient_map[cluster_label],
                        target_patients_df=target_patients_df,
                        concept_col=patient_concept_col,
                        person_col=patient_person_col,
                        time_col=patient_time_col,
                        heritage_col=patient_heritage_col,
                    )
                    if np.isnan(prevalence_pct):
                        prevalence_pct = patient_prev
                    if np.isnan(median_days):
                        median_days = patient_median

                row[f"Cluster {cluster_label} Prevalence (%)"] = prevalence_pct
                row[f"Cluster {cluster_label} Median Time to First Occurrence (Days)"] = median_days

            export_rows.append(row)

        if not export_rows:
            return no_update

        export_df = pd.DataFrame(export_rows)
        fixed_precision = {
            "Median Time to First Occurrence (Days)": 1,
            "Target Prevalence (%)": 2,
            "Enrichment Ratio (Target vs Control)": 3,
            "Mean Age (Years)": 2,
            "Age 95% CI Lower (Years)": 2,
            "Age 95% CI Upper (Years)": 2,
            "Male Proportion (%)": 2,
            "Male Proportion 95% CI Lower (%)": 2,
            "Male Proportion 95% CI Upper (%)": 2,
        }
        for column_name, decimals in fixed_precision.items():
            if column_name in export_df.columns:
                export_df[column_name] = pd.to_numeric(export_df[column_name], errors="coerce").round(decimals)
        for column_name in export_df.columns:
            if column_name.startswith("Cluster ") and column_name.endswith("Prevalence (%)"):
                export_df[column_name] = pd.to_numeric(export_df[column_name], errors="coerce").round(2)
            if column_name.startswith("Cluster ") and column_name.endswith("Occurrence (Days)"):
                export_df[column_name] = pd.to_numeric(export_df[column_name], errors="coerce").round(1)

        safe_study = "".join(
            ch if (ch.isalnum() or ch in {"-", "_"}) else "_" for ch in str(selected_study)
        )
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        filename = f"{safe_study}_active_concepts_{timestamp}.tsv"

        tsv_content = export_df.to_csv(sep="\t", index=False, na_rep="")
        return {
            "content": tsv_content,
            "filename": filename,
            "type": "text/tab-separated-values; charset=utf-8",
        }
