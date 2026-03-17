"""Overlap and trajectory tab callback registration."""

import os
from pathlib import Path
from typing import Dict, List, Optional

import pandas as pd
from dash import Input, Output, State, dash_table, dcc, html

from data.cache import get_or_load_clustering_file, get_or_load_parquet_data, update_cache
from data.loader import load_clustering_file
from plots.clustering_plots import (
    create_overlap_plot,
    create_overlap_plot_from_summary,
    create_trajectory_plot,
    create_trajectory_plot_from_summary,
)


def register_plot_tab_callbacks(
    app,
    *,
    data_dir: Path,
    cache_store,
    loaded_parquet_data_store: Dict[str, Dict[str, pd.DataFrame]],
    normalize_concept_id_fn,
) -> None:
    """Register callbacks for overlap and trajectory tab content."""
    DATA_DIR = data_dir
    cache = cache_store
    loaded_parquet_data = loaded_parquet_data_store
    _normalize_concept_id = normalize_concept_id_fn
    exports_enabled = os.environ.get("CONTRAST_VIEWER_ALLOW_EXPORTS", "1").strip().lower() not in {
        "0", "false", "no", "off"
    }
    plotly_graph_config = {"displayModeBar": True, "scrollZoom": False}
    if not exports_enabled:
        plotly_graph_config["modeBarButtonsToRemove"] = ["toImage"]

    # Overlap tab callbacks
    @app.callback(
        Output("overlap-group-selector", "options"),
        Input("clustering-results-store", "data"),
        prevent_initial_call=False
    )
    def update_overlap_group_options(clustering_results: Optional[Dict]):
        """Update the group selector options based on clustering results."""
        options = [{"label": "Overall", "value": "overall"}]
        cluster_counts = {}  # Initialize to avoid UnboundLocalError
        optimal_k = 0  # Initialize to avoid UnboundLocalError
        
        if clustering_results:
            optimal_k = clustering_results.get('optimal_k', 0)
            # Also try 'n_clusters' and 'optimal_cluster_count' keys
            if optimal_k == 0:
                optimal_k = clustering_results.get('n_clusters', 0)
            if optimal_k == 0:
                optimal_k = clustering_results.get('optimal_cluster_count', 0)
            
            # First try to get cluster_counts directly (works for both patient and summary mode)
            cluster_counts = clustering_results.get('cluster_counts', {})
        
        if cluster_counts:
            # Use pre-computed cluster counts (summary mode or patient mode with counts)
            # Sort clusters by name (C1, C2, etc.)
            sorted_clusters = sorted(cluster_counts.keys(), key=lambda x: int(x[1:]) if x.startswith('C') and x[1:].isdigit() else 999)
            for cluster_label in sorted_clusters:
                count = cluster_counts.get(cluster_label, 0)
                cluster_num = cluster_label[1:] if cluster_label.startswith('C') else cluster_label
                options.append({
                    "label": f"Cluster {cluster_num} (n={count})",
                    "value": f"cluster_{cluster_num}"
                })
        elif clustering_results:
            # Fallback: try to get from patient_assignments (patient mode only)
            patient_assignments_data = clustering_results.get('patient_assignments', [])
            
            if isinstance(patient_assignments_data, list):
                patient_assignments = pd.DataFrame(patient_assignments_data) if patient_assignments_data else pd.DataFrame()
            else:
                patient_assignments = patient_assignments_data
            
            # If optimal_k is still 0, try to infer from patient_assignments
            if optimal_k == 0 and not patient_assignments.empty and 'cluster' in patient_assignments.columns:
                unique_clusters = patient_assignments['cluster'].unique()
                optimal_k = len(unique_clusters)
            
            if not patient_assignments.empty and optimal_k > 0:
                # Count patients per cluster
                cluster_counts = patient_assignments['cluster'].value_counts().to_dict()
                
                for i in range(optimal_k):
                    cluster_label = f"C{i+1}"
                    count = cluster_counts.get(cluster_label, 0)
                    options.append({
                        "label": f"Cluster {i+1} (n={count})",
                        "value": f"cluster_{i+1}"
                    })
        
        return options
    
    
    @app.callback(
        Output("overlap-plot-container", "children"),
        [Input("clustering-results-store", "data"),
         Input("plots-update-trigger-store", "data"),
         Input("dashboard-table", "rowData"),
         Input("overlap-group-selector", "value"),
         Input("overlap-metric-selector", "value")],
        [State("selected-study-store", "data"),
         State("data-mode-store", "data")],
        prevent_initial_call=True
    )
    def update_overlap_plot(
        clustering_results: Optional[Dict],
        plots_trigger: Optional[float],
        row_data: Optional[List[Dict]],
        selected_group: str,
        metric: str,
        selected_study: Optional[str],
        data_mode: Optional[str]
    ):
        """Update the overlap plot when parameters change.
        
        In summary mode, uses pre-computed pairwise overlap data.
        In patient mode, computes overlap from patient-level data.
        """
        if not clustering_results or not selected_study:
            return html.P("Waiting for clustering results...", 
                          style={"color": "#999", "textAlign": "center", "padding": "50px"})
        
        # Get data using centralized cache (works across background callbacks)
        parquet_data = get_or_load_parquet_data(selected_study, DATA_DIR, cache)
        if parquet_data is None:
            return html.P("Study data not loaded.", 
                          style={"color": "#999", "textAlign": "center", "padding": "50px"})
        
        # Also update in-memory cache for this process
        loaded_parquet_data[selected_study] = parquet_data
        actual_data_mode = parquet_data.get("_mode", "patient")
        
        # Get active concepts from row_data
        active_concepts = set()
        heritage_groups_order = {}
        
        if row_data:
            for row in row_data:
                if row.get("_show", False):
                    concept_id = row.get("_concept_id") or row.get("CONCEPT_ID")
                    if concept_id:
                        norm_id = _normalize_concept_id(concept_id)
                        active_concepts.add(norm_id)
                        
                        heritage = row.get("HERITAGE", "unknown")
                        if heritage not in heritage_groups_order:
                            heritage_groups_order[heritage] = []
                        heritage_groups_order[heritage].append(row)
        
        if not active_concepts:
            return html.P("No active concepts selected.", 
                          style={"color": "#999", "textAlign": "center", "padding": "50px"})
        
        try:
            # Summary mode: use pre-computed pairwise overlap data
            if actual_data_mode == "summary":
                # Get the k value from clustering results
                optimal_k = clustering_results.get("optimal_k") or clustering_results.get("optimal_cluster_count", 4)
                
                # Load the pairwise overlap data for this k
                pairwise_overlap_key = f"clustering_k{optimal_k}_pairwise_overlap"
                pairwise_overlap_df = parquet_data.get(pairwise_overlap_key)
                
                if pairwise_overlap_df is None or pairwise_overlap_df.empty:
                    # Try to load clustering file on demand using cache
                    pairwise_overlap_df = get_or_load_clustering_file(selected_study, DATA_DIR, optimal_k, "pairwise_overlap", cache)
                    if pairwise_overlap_df is not None:
                        # Cache it in parquet_data for future use
                        parquet_data[pairwise_overlap_key] = pairwise_overlap_df
                        update_cache(selected_study, parquet_data, cache, data_dir=DATA_DIR)
                    else:
                        return html.P(f"No pre-computed overlap data available for k={optimal_k}. Please re-run precomputation.", 
                                  style={"color": "#999", "textAlign": "center", "padding": "50px"})
                
                # Map selected_group format: "overall" or "cluster_1" -> "overall" or "C1"
                if selected_group and selected_group.startswith("cluster_"):
                    cluster_num = selected_group.split("_")[1]
                    group_filter = f"C{cluster_num}"
                else:
                    group_filter = "overall"
                
                result = create_overlap_plot_from_summary(
                    clustering_results=clustering_results,
                    pairwise_overlap_df=pairwise_overlap_df,
                    heritage_groups_order=heritage_groups_order,
                    active_concept_ids=active_concepts,
                    selected_group=group_filter
                )
            else:
                # Patient mode: compute from patient data
                data_patients = parquet_data.get("data_patients", pd.DataFrame())
                
                if data_patients.empty:
                    return html.P("No patient data available.", 
                                  style={"color": "#999", "textAlign": "center", "padding": "50px"})
                
                result = create_overlap_plot(
                    clustering_results=clustering_results,
                    data_patients=data_patients,
                    heritage_groups_order=heritage_groups_order,
                    active_concept_ids=active_concepts,
                    selected_group=selected_group or "overall",
                    metric=metric or "jaccard"
                )
            
            # Handle both old (just fig) and new (fig, pairwise_data) return formats
            if isinstance(result, tuple):
                fig, pairwise_data = result
            else:
                fig = result
                pairwise_data = []
            
            # Create table from pairwise data - use simple dash_table instead of AgGrid
            if pairwise_data and len(pairwise_data) > 0:
                table_component = dash_table.DataTable(
                    id="overlap-table",
                    columns=[
                        {"name": "Concept 1", "id": "Concept 1"},
                        {"name": "Concept 2", "id": "Concept 2"},
                        {"name": "Correlation", "id": "Correlation", "type": "numeric", "format": {"specifier": ".3f"}},
                        {"name": "Jaccard", "id": "Jaccard", "type": "numeric", "format": {"specifier": ".3f"}},
                        {"name": "Co-occur (n)", "id": "Co-occur", "type": "numeric"},
                        {"name": "Total (n)", "id": "Total", "type": "numeric"},
                    ],
                    data=pairwise_data,
                    sort_action="native",
                    filter_action="native",
                    page_action="native",
                    page_size=15,
                    style_table={"overflowX": "auto"},
                    style_cell={"textAlign": "left", "padding": "8px", "fontSize": "12px"},
                    style_header={"fontWeight": "bold", "backgroundColor": "#f8f9fa"},
                    style_data_conditional=[
                        {"if": {"filter_query": "{Correlation} > 0.3", "column_id": "Correlation"},
                         "backgroundColor": "rgba(33, 102, 172, 0.3)"},
                        {"if": {"filter_query": "{Correlation} < -0.3", "column_id": "Correlation"},
                         "backgroundColor": "rgba(178, 24, 43, 0.3)"},
                        {"if": {"filter_query": "{Jaccard} > 0.5", "column_id": "Jaccard"},
                         "backgroundColor": "rgba(33, 102, 172, 0.3)"},
                    ]
                )
            else:
                table_component = html.P("No pairwise data available")
            
            return html.Div([
                dcc.Graph(
                    figure=fig,
                    config=plotly_graph_config,
                    style={"width": "100%"}
                ),
                html.Hr(style={"margin": "20px 0"}),
                html.H5("Pairwise Concept Relationships", style={"marginBottom": "10px", "color": "#2c3e50"}),
                html.P("Sorted by absolute correlation (strongest first)", style={"color": "#666", "fontSize": "12px", "marginBottom": "10px"}),
                table_component
            ])
        except Exception as e:
            import traceback
            traceback.print_exc()
            return html.P(f"Error generating overlap plot: {str(e)}", 
                          style={"color": "#d62728", "textAlign": "center", "padding": "50px"})
    
    
    @app.callback(
        Output("trajectories-plot-container", "children"),
        [Input("clustering-results-store", "data"),
         Input("plots-update-trigger-store", "data"),
         Input("dashboard-table", "rowData"),
         Input("trajectory-order-mode", "value"),
         Input("cluster-view-store", "data")],
        [State("selected-study-store", "data"),
         State("data-mode-store", "data"),
         State("cluster-prevalence-slider", "value")],
        prevent_initial_call=True
    )
    def update_trajectory_plot(
        clustering_results: Optional[Dict],
        plots_trigger: Optional[float],
        row_data: Optional[List[Dict]],
        ordering_mode: Optional[str],
        selected_cluster: Optional[str],
        selected_study: Optional[str],
        data_mode: Optional[str],
        cluster_prevalence_threshold: int
    ):
        """
        Update the trajectory plot when clustering results change or concepts are updated.
        cluster_prevalence_threshold: Minimum cluster prevalence percentage to show concept (0-100)
            Only applies when a specific cluster is selected (not "all") and Apply Filters is pressed.
        
        In summary mode, uses pre-computed clustering summary matrix.
        In patient mode, uses patient-level data.
        """
        if not clustering_results or not selected_study:
            return html.P("Waiting for clustering results...", 
                          style={"color": "#999", "textAlign": "center", "padding": "50px"})
        
        # Get data using centralized cache (works across background callbacks)
        parquet_data = get_or_load_parquet_data(selected_study, DATA_DIR, cache)
        if parquet_data is None:
            return html.P("Study data not loaded.", 
                          style={"color": "#999", "textAlign": "center", "padding": "50px"})
        
        # Also update in-memory cache for this process
        loaded_parquet_data[selected_study] = parquet_data
        actual_data_mode = parquet_data.get("_mode", "patient")
        
        # Get active concepts from row_data
        active_concepts = set()
        heritage_groups_order = {}
        
        if row_data:
            for row in row_data:
                if row.get("_show", False):
                    concept_id = row.get("_concept_id") or row.get("CONCEPT_ID")
                    if concept_id:
                        norm_id = _normalize_concept_id(concept_id)
                        active_concepts.add(norm_id)
                        
                        heritage = row.get("HERITAGE", "unknown")
                        if heritage not in heritage_groups_order:
                            heritage_groups_order[heritage] = []
                        heritage_groups_order[heritage].append(row)
        
        if not active_concepts:
            return html.P("No active concepts selected.", 
                          style={"color": "#999", "textAlign": "center", "padding": "50px"})
        
        try:
            # Determine the effective min_prevalence_pct
            # Only apply cluster prevalence filter when a specific cluster is selected (not "all")
            if selected_cluster and selected_cluster != "all":
                min_prevalence_pct = cluster_prevalence_threshold or 0
            else:
                min_prevalence_pct = 0  # No filtering when viewing all clusters
            
            # SUMMARY MODE: Use pre-computed clustering summary
            if actual_data_mode == "summary":
                # Get the clustering summary matrix for the current k
                k_value = clustering_results.get('optimal_cluster_count', 3)
                summary_key = f"clustering_k{k_value}_summary"
                clustering_summary_matrix = parquet_data.get(summary_key)
                
                if clustering_summary_matrix is None or clustering_summary_matrix.empty:
                    # Try to load clustering file on demand
                    disease_folder = parquet_data.get("_disease_folder") or (DATA_DIR / selected_study)
                    clustering_summary_matrix = load_clustering_file(disease_folder, k_value, "summary")
                    if clustering_summary_matrix is not None:
                        # Cache it in parquet_data for future use
                        parquet_data[summary_key] = clustering_summary_matrix
                    else:
                        return html.P("No pre-computed clustering data available.", 
                                      style={"color": "#999", "textAlign": "center", "padding": "50px"})
                
                fig = create_trajectory_plot_from_summary(
                    clustering_results=clustering_results,
                    clustering_summary_matrix=clustering_summary_matrix,
                    heritage_groups_order=heritage_groups_order,
                    active_concept_ids=active_concepts,
                    min_prevalence_pct=min_prevalence_pct,
                    ordering_mode=ordering_mode or "order"
                )
            else:
                # PATIENT MODE: Use patient-level data
                data_patients = parquet_data.get("data_patients", pd.DataFrame())
                
                if data_patients.empty:
                    return html.P("No patient data available.", 
                                  style={"color": "#999", "textAlign": "center", "padding": "50px"})
                
                fig = create_trajectory_plot(
                    clustering_results=clustering_results,
                    data_patients=data_patients,
                    heritage_groups_order=heritage_groups_order,
                    active_concept_ids=active_concepts,
                    min_prevalence_pct=min_prevalence_pct,
                    ordering_mode=ordering_mode or "order"
                )
            
            return dcc.Graph(
                figure=fig,
                config=plotly_graph_config,
                style={"width": "100%"}
            )
        except Exception as e:
            import traceback
            traceback.print_exc()
            return html.P(f"Error generating trajectory plot: {str(e)}", 
                          style={"color": "#d62728", "textAlign": "center", "padding": "50px"})
    
    
