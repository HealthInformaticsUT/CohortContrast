"""Clustering callback registration for initial clustering and reclustering flows."""

from pathlib import Path
from typing import Dict, List, Optional, Tuple

import pandas as pd
from dash import Input, Output, State, no_update

from callbacks.common import load_study_parquet_data
from clustering.clustering import perform_patient_clustering
from data.cache import get_or_load_clustering_file, update_cache
from data.loader import get_available_cluster_k_values, load_clustering_file


def _normalize_summary_matrix_for_store(summary_matrix: pd.DataFrame) -> pd.DataFrame:
    """Add stable column aliases so downstream consumers can use consistent names."""
    if not isinstance(summary_matrix, pd.DataFrame) or summary_matrix.empty:
        return pd.DataFrame()

    normalized = summary_matrix.copy()
    alias_pairs = [
        ("CONCEPT_ID", "concept_id"),
        ("CONCEPT_NAME", "concept_name"),
        ("cluster", "CLUSTER"),
        ("prevalence", "PREVALENCE"),
        ("count_category", "COUNT_CATEGORY"),
        ("median_days", "MEDIAN_DAYS"),
        ("patient_count", "PATIENT_COUNT"),
        ("total_cluster_patients", "TOTAL_CLUSTER_PATIENTS"),
        ("age_mean", "AGE_MEAN"),
        ("male_proportion", "MALE_PROPORTION"),
        ("ORIGINAL_CONCEPT_ID", "original_concept_id"),
        ("ORDINAL", "ordinal"),
        ("IS_ORDINAL", "is_ordinal"),
    ]
    for canonical_col, alias_col in alias_pairs:
        if canonical_col in normalized.columns and alias_col not in normalized.columns:
            normalized[alias_col] = normalized[canonical_col]
        elif alias_col in normalized.columns and canonical_col not in normalized.columns:
            normalized[canonical_col] = normalized[alias_col]
    return normalized


def _extract_cluster_counts(summary_matrix: pd.DataFrame) -> Dict[str, int]:
    """Extract cluster -> patient count mapping from summary matrix."""
    if not isinstance(summary_matrix, pd.DataFrame) or summary_matrix.empty:
        return {}

    cluster_col = "cluster" if "cluster" in summary_matrix.columns else ("CLUSTER" if "CLUSTER" in summary_matrix.columns else None)
    total_col = (
        "total_cluster_patients" if "total_cluster_patients" in summary_matrix.columns else (
            "TOTAL_CLUSTER_PATIENTS" if "TOTAL_CLUSTER_PATIENTS" in summary_matrix.columns else None
        )
    )
    if cluster_col is None or total_col is None:
        return {}

    cluster_counts: Dict[str, int] = {}
    for cluster_label in summary_matrix[cluster_col].dropna().unique():
        cluster_rows = summary_matrix[summary_matrix[cluster_col] == cluster_label]
        if not cluster_rows.empty:
            try:
                cluster_counts[str(cluster_label)] = int(cluster_rows[total_col].iloc[0])
            except Exception:
                continue
    return cluster_counts


def _summary_matrix_records(summary_matrix: pd.DataFrame) -> List[Dict]:
    """Serialize normalized summary matrix for storage."""
    normalized = _normalize_summary_matrix_for_store(summary_matrix)
    if normalized.empty:
        return []
    return normalized.to_dict("records")


def _resolve_summary_k_value(parquet_data: Dict, cluster_count: str) -> Optional[int]:
    """Resolve requested k value for summary mode, including auto-selection."""
    if cluster_count != "auto":
        try:
            return int(cluster_count)
        except (ValueError, TypeError):
            return 3

    available_k = get_available_cluster_k_values(parquet_data)
    if not available_k:
        return None

    metadata = parquet_data.get("_metadata", {})
    clustering_info = metadata.get("clustering", {})
    best_k = available_k[0]
    best_score = -1
    for k in available_k:
        k_info = clustering_info.get(str(k), {})
        score = k_info.get("silhouette_score", 0)
        if score > best_score:
            best_score = score
            best_k = k
    return best_k


def _load_summary_matrix(
    *,
    selected_study: str,
    parquet_data: Dict,
    data_dir: Path,
    cache_store,
    k_value: int,
    use_cache_loader: bool,
) -> Tuple[Optional[pd.DataFrame], Optional[int]]:
    """Load summary matrix for requested/closest available k."""
    summary_key = f"clustering_k{k_value}_summary"
    if summary_key in parquet_data:
        return parquet_data[summary_key], k_value

    def _load_for_k(current_k: int) -> Optional[pd.DataFrame]:
        cache_key = f"clustering_k{current_k}_summary"
        if use_cache_loader:
            summary_matrix_local = get_or_load_clustering_file(
                selected_study,
                data_dir,
                current_k,
                "summary",
                cache_store,
            )
            if summary_matrix_local is not None:
                parquet_data[cache_key] = summary_matrix_local
                update_cache(selected_study, parquet_data, cache_store, data_dir=data_dir)
            return summary_matrix_local

        disease_folder = parquet_data.get("_disease_folder") or (data_dir / selected_study)
        summary_matrix_local = load_clustering_file(disease_folder, current_k, "summary")
        if summary_matrix_local is not None:
            parquet_data[cache_key] = summary_matrix_local
        return summary_matrix_local

    summary_matrix = _load_for_k(k_value)
    if summary_matrix is not None:
        return summary_matrix, k_value

    available_k = get_available_cluster_k_values(parquet_data)
    if not available_k:
        return None, None

    closest_k = min(available_k, key=lambda candidate: abs(candidate - k_value))
    summary_matrix = _load_for_k(closest_k)
    if summary_matrix is None:
        return None, None
    return summary_matrix, closest_k


def _build_summary_clustering_result(
    parquet_data: Dict,
    summary_matrix: pd.DataFrame,
    k_value: int,
) -> Tuple[Dict, List[str]]:
    """Convert summary matrix into clustering store payload."""
    metadata = parquet_data.get("_metadata", {})
    clustering_info = metadata.get("clustering", {}).get(str(k_value), {})

    summary_matrix_normalized = _normalize_summary_matrix_for_store(summary_matrix)
    cluster_counts = _extract_cluster_counts(summary_matrix_normalized)
    results_dict = {
        "summary_matrix": _summary_matrix_records(summary_matrix_normalized),
        "patient_assignments": [],
        "cluster_counts": cluster_counts,
        "best_silhouette_score": clustering_info.get("silhouette_score", 0),
        "optimal_cluster_count": k_value,
        "_mode": "summary",
    }

    concept_col = (
        "CONCEPT_ID"
        if "CONCEPT_ID" in summary_matrix_normalized.columns
        else ("concept_id" if "concept_id" in summary_matrix_normalized.columns else None)
    )
    concept_ids_used = (
        [str(cid) for cid in summary_matrix_normalized[concept_col].dropna().unique()]
        if concept_col
        else []
    )
    return results_dict, concept_ids_used


def _filter_initial_patient_concepts(dashboard_data_store: List[Dict]) -> List[Dict]:
    """Filter concepts for initial patient clustering."""
    concepts_to_use = []
    for concept in dashboard_data_store:
        prevalence = concept.get("TARGET_SUBJECT_PREVALENCE", 0)
        ratio = concept.get("PREVALENCE_DIFFERENCE_RATIO", 0)
        try:
            prevalence = float(prevalence) if prevalence is not None else 0
            ratio = float(ratio) if ratio is not None else 0
        except (ValueError, TypeError):
            prevalence = 0
            ratio = 0
        if prevalence > 0.01 and ratio > 1:
            concepts_to_use.append(concept)
    return concepts_to_use


def _resolve_patient_k_value(cluster_count: str) -> Optional[int]:
    if cluster_count == "auto":
        return None
    try:
        return int(cluster_count)
    except (ValueError, TypeError):
        return None


def _run_patient_clustering(
    *,
    parquet_data: Dict,
    concepts_to_use: List[Dict],
    k_value: Optional[int],
    include_mode_flag: bool,
) -> Tuple[Optional[Dict], Optional[List[str]]]:
    """Run patient clustering and convert results to callback payload format."""
    data_patients = parquet_data["data_patients"]
    data_initial = parquet_data.get("data_initial", pd.DataFrame())
    try:
        clustering_results = perform_patient_clustering(
            data_patients=data_patients,
            data_initial=data_initial,
            dashboard_data=concepts_to_use,
            concept_limit=60,
            cluster_range=(2, 5),
            pca_components=20,
            time_window=180,
            k_value=k_value,
        )
        summary_matrix_normalized = _normalize_summary_matrix_for_store(
            clustering_results["summary_matrix"]
        )
        results_dict = {
            "summary_matrix": _summary_matrix_records(summary_matrix_normalized),
            "patient_assignments": (
                clustering_results["patient_assignments"].to_dict("records")
                if not clustering_results["patient_assignments"].empty
                else []
            ),
            "best_silhouette_score": clustering_results["best_silhouette_score"],
            "optimal_cluster_count": clustering_results["optimal_cluster_count"],
        }
        if include_mode_flag:
            results_dict["_mode"] = "patient"
        concept_ids_used = [
            str(item.get("CONCEPT_ID") or item.get("_concept_id", ""))
            for item in concepts_to_use
        ]
        return results_dict, concept_ids_used
    except Exception:
        import traceback

        traceback.print_exc()
        return None, None


def register_clustering_callbacks(
    app,
    *,
    logger_obj,
    data_dir: Path,
    cache_store,
    loaded_parquet_data_store: Dict[str, Dict[str, pd.DataFrame]],
) -> None:
    """Register clustering callbacks."""
    logger = logger_obj
    DATA_DIR = data_dir
    cache = cache_store
    loaded_parquet_data = loaded_parquet_data_store
    @app.callback(
        [Output("clustering-results-store", "data", allow_duplicate=True),
         Output("clustering-concepts-store", "data", allow_duplicate=True)],
        [Input("clustering-trigger-store", "data")],  # Trigger from clustering-trigger-store
        [State("dashboard-data-store", "data"),
         State("selected-study-store", "data"),
         State("cluster-count", "value")],
        prevent_initial_call='initial_duplicate'  # Allow initial call with duplicates
        # NOTE: background=True disabled due to macOS multiprocessing crashes
    )
    def perform_initial_clustering(
        clustering_trigger: Optional[float],
        dashboard_data_store: Optional[List[Dict]],
        selected_study: Optional[str],
        cluster_count: str
    ) -> Tuple[Optional[Dict], Optional[List[str]]]:
        """
        Perform initial clustering with filtered concepts when study data is loaded.
        This callback is triggered by clustering-trigger-store after dashboard data is loaded.
        Uses concepts where prevalence > 1% and ratio > 1.
        
        In summary mode, uses pre-computed clustering results.
        """
        logger.info(f"perform_initial_clustering called for: {selected_study}")
        
        if not clustering_trigger or not selected_study or not dashboard_data_store:
            return None, None
        parquet_data = load_study_parquet_data(
            selected_study,
            DATA_DIR,
            cache,
            loaded_parquet_data,
        )
        if parquet_data is None:
            return None, None
        data_mode = parquet_data.get("_mode", "patient")
        
        if data_mode == "summary":
            k_value = _resolve_summary_k_value(parquet_data, cluster_count)
            if k_value is None:
                return None, None
            summary_matrix, resolved_k = _load_summary_matrix(
                selected_study=selected_study,
                parquet_data=parquet_data,
                data_dir=DATA_DIR,
                cache_store=cache,
                k_value=k_value,
                use_cache_loader=False,
            )
            if summary_matrix is None or resolved_k is None:
                return None, None
            return _build_summary_clustering_result(parquet_data, summary_matrix, resolved_k)

        if "data_patients" not in parquet_data:
            return None, None

        concepts_to_use = _filter_initial_patient_concepts(dashboard_data_store)
        if not concepts_to_use:
            return None, None
        return _run_patient_clustering(
            parquet_data=parquet_data,
            concepts_to_use=concepts_to_use,
            k_value=_resolve_patient_k_value(cluster_count),
            include_mode_flag=True,
        )
    
    
    @app.callback(
        [Output("clustering-results-store", "data", allow_duplicate=True),
         Output("clustering-concepts-store", "data", allow_duplicate=True)],
        [Input("recluster-btn", "n_clicks"),
         Input("cluster-count", "value"),
         Input("apply-filters-btn", "n_clicks")],
        [State("clustering-scope", "value"),
         State("dashboard-table", "rowData"),
         State("selected-study-store", "data"),
         State("dashboard-data-store", "data"),
         State("data-mode-store", "data")],
        prevent_initial_call=True
        # NOTE: background=True disabled due to macOS multiprocessing crashes
    )
    def perform_reclustering(
        recluster_clicks: Optional[int],
        cluster_count: str,
        apply_filters_clicks: Optional[int],
        clustering_scope: str,
        row_data: Optional[List[Dict]],
        selected_study: Optional[str],
        dashboard_data_store: Optional[List[Dict]],
        data_mode: Optional[str]
    ) -> Tuple[Optional[Dict], Optional[List[str]]]:
        """
        Perform reclustering with concepts based on scope selection.
        This callback is triggered by the recluster button and, in summary mode,
        by Apply Filters. Cluster-count changes alone do not trigger reclustering.
        
        In summary mode, loads pre-computed clustering for the selected k value.
        In patient mode, performs live clustering only on button click (not dropdown change).
        """
        from dash import ctx
        
        # Check what triggered the callback
        triggered_id = ctx.triggered_id if ctx.triggered_id else None
        
        # Cluster-count changes alone should not trigger reclustering in any mode.
        # In summary mode, selected cluster-count is applied when user presses Apply Filters.
        if triggered_id == "cluster-count":
            return no_update, no_update
    
        # Apply Filters only triggers this path in summary mode
        if triggered_id == "apply-filters-btn":
            if data_mode != "summary" or apply_filters_clicks is None or apply_filters_clicks <= 0:
                return no_update, no_update
        
        if recluster_clicks is None and triggered_id == "recluster-btn":
            return no_update, no_update
        
        if not selected_study or not dashboard_data_store:
            return None, None
        
        parquet_data = load_study_parquet_data(
            selected_study,
            DATA_DIR,
            cache,
            loaded_parquet_data,
        )
        if parquet_data is None:
            return None, None
        if not parquet_data:
            return None, None
        
        # Check actual data mode from parquet_data
        actual_data_mode = parquet_data.get("_mode", "patient")
        
        # SUMMARY MODE: Load pre-computed clustering for selected k
        if actual_data_mode == "summary":
            k_value = _resolve_summary_k_value(parquet_data, cluster_count)
            if k_value is None:
                return None, None
            summary_matrix, resolved_k = _load_summary_matrix(
                selected_study=selected_study,
                parquet_data=parquet_data,
                data_dir=DATA_DIR,
                cache_store=cache,
                k_value=k_value,
                use_cache_loader=True,
            )
            if summary_matrix is None or resolved_k is None:
                return None, None
            return _build_summary_clustering_result(parquet_data, summary_matrix, resolved_k)
        
        # PATIENT MODE: Perform live clustering
        if "data_patients" not in parquet_data:
            return None, None
        
        # Determine which concepts to use based on scope
        if clustering_scope == "active" and row_data:
            # Use only active (checked) concepts from the table
            concepts_to_use = [row for row in row_data if row.get("_show") is True]
        else:
            # Use all concepts from dashboard data (with prevalence > 1% and ratio > 1 for efficiency)
            concepts_to_use = [
                item for item in dashboard_data_store
                if item.get('TARGET_SUBJECT_PREVALENCE', 0) > 0.01 and
                   item.get('PREVALENCE_DIFFERENCE_RATIO', 0) > 1
            ]
        
        if not concepts_to_use:
            return None, None
        
        return _run_patient_clustering(
            parquet_data=parquet_data,
            concepts_to_use=concepts_to_use,
            k_value=_resolve_patient_k_value(cluster_count),
            include_mode_flag=False,
        )
    
    
