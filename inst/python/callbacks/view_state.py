"""Callback registration for dashboard view state and UI selectors."""

from typing import Dict, List, Optional, Tuple

import pandas as pd
import dash
from dash import Input, Output, State, dcc, html, no_update

from utils.helpers import format_heritage_label


def register_view_state_callbacks(app) -> None:
    """Register callbacks that synchronize dashboard view selectors and labels."""

    @app.callback(
        [Output("cluster-view-selector-summary", "options", allow_duplicate=True),
         Output("cluster-view-selector-patient", "options", allow_duplicate=True),
         Output("cluster-view-store", "data", allow_duplicate=True),
         Output("divergence-cluster-scope", "options"),
         Output("divergence-cluster-scope", "value")],
        Input("clustering-results-store", "data"),
        State("cluster-view-store", "data"),
        State("divergence-cluster-scope", "value"),
        prevent_initial_call=True
    )
    def update_cluster_view_options(
        clustering_results: Optional[Dict],
        current_value: str,
        current_divergence_scope: Optional[List[str]],
    ) -> Tuple[List[Dict], List[Dict], str, List[Dict], List[str]]:
        options = [{"label": "All", "value": "all"}]
        valid_values = ["all"]
        divergence_options = []
        valid_divergence_values = []

        if clustering_results:
            optimal_k = clustering_results.get("optimal_cluster_count", 0)
            if not optimal_k:
                optimal_k = clustering_results.get("optimal_k", 0)
            if not optimal_k:
                optimal_k = clustering_results.get("n_clusters", 0)
            patient_assignments = clustering_results.get("patient_assignments", [])

            cluster_counts = {}
            if "cluster_counts" in clustering_results and clustering_results["cluster_counts"]:
                cluster_counts = clustering_results["cluster_counts"]
            elif patient_assignments:
                if isinstance(patient_assignments, list):
                    df = pd.DataFrame(patient_assignments)
                else:
                    df = patient_assignments
                if not df.empty and "cluster" in df.columns:
                    cluster_counts = df["cluster"].value_counts().to_dict()

            for i in range(optimal_k):
                cluster_label = f"C{i + 1}"
                valid_values.append(cluster_label)
                valid_divergence_values.append(cluster_label)
                count = cluster_counts.get(cluster_label, 0)
                options.append({"label": f"Cluster {i + 1} (n={count})", "value": cluster_label})
                divergence_options.append({"label": f"C{i + 1} (n={count})", "value": cluster_label})

        new_value = current_value if current_value in valid_values else "all"

        scope_values = current_divergence_scope if isinstance(current_divergence_scope, list) else []
        new_scope = [value for value in scope_values if value in valid_divergence_values]

        return options, options, new_value, divergence_options, new_scope

    @app.callback(
        Output("cluster-view-store", "data", allow_duplicate=True),
        [Input("cluster-view-selector-summary", "value"),
         Input("cluster-view-selector-patient", "value")],
        prevent_initial_call=True
    )
    def sync_cluster_view_to_store(summary_value: Optional[str], patient_value: Optional[str]) -> str:
        # Pick the value from the selector that actually triggered.
        # Without this, the hidden selector (often "all") can overwrite the
        # visible selector and snap the view back to "All".
        ctx = dash.callback_context
        triggered_id = None
        if ctx.triggered:
            triggered_id = ctx.triggered[0]["prop_id"].split(".")[0]

        if triggered_id == "cluster-view-selector-patient" and patient_value is not None:
            return patient_value
        if triggered_id == "cluster-view-selector-summary" and summary_value is not None:
            return summary_value

        # Fallback when trigger metadata is unavailable
        if patient_value not in (None, "all") and summary_value in (None, "all"):
            return patient_value
        selected_value = summary_value if summary_value is not None else patient_value
        return selected_value if selected_value else "all"

    @app.callback(
        [Output("cluster-view-selector-summary", "value", allow_duplicate=True),
         Output("cluster-view-selector-patient", "value", allow_duplicate=True)],
        Input("cluster-view-store", "data"),
        prevent_initial_call=True
    )
    def sync_store_to_cluster_view(store_value: Optional[str]) -> Tuple[str, str]:
        value = store_value if store_value else "all"
        return value, value

    @app.callback(
        Output("silhouette-score-display", "children"),
        Input("clustering-results-store", "data"),
        prevent_initial_call=True
    )
    def update_silhouette_display(clustering_results: Optional[Dict]) -> str:
        if not clustering_results:
            return ""

        silhouette_score = clustering_results.get("best_silhouette_score")
        optimal_k = clustering_results.get("optimal_cluster_count", 0)

        if silhouette_score is not None and optimal_k > 0:
            score_str = f"Silhouette: {silhouette_score:.3f}"
            if silhouette_score >= 0.5:
                quality = "(strong)"
            elif silhouette_score >= 0.25:
                quality = "(reasonable)"
            else:
                quality = "(weak)"
            return f"| {optimal_k} clusters | {score_str} {quality}"

        return ""

    @app.callback(
        Output("heritage-checkboxes", "children"),
        Input("dashboard-data-store", "data")
    )
    def populate_heritage_checkboxes(dashboard_data: Optional[List[Dict]]) -> List:
        if not dashboard_data:
            return []

        heritages = set()
        for row in dashboard_data:
            heritage = row.get("HERITAGE")
            if heritage is not None and pd.notna(heritage) and str(heritage).strip():
                heritages.add(str(heritage))

        heritages = sorted(list(heritages))

        if not heritages:
            return [html.P("No heritage types available", style={"color": "#999", "fontSize": "12px", "padding": "5px"})]

        default_heritages = ["procedure_occurrence", "measurement", "drug_exposure"]

        checkboxes = []
        for heritage in heritages:
            formatted_label = format_heritage_label(heritage)
            is_default = heritage in default_heritages
            checkboxes.append(
                html.Div([
                    dcc.Checklist(
                        id={"type": "heritage-checkbox", "index": heritage},
                        options=[{"label": formatted_label, "value": heritage}],
                        value=[heritage] if is_default else [],
                        style={"display": "inline-block", "marginRight": "10px"},
                    )
                ], style={"marginBottom": "8px", "padding": "5px", "backgroundColor": "#fff", "borderRadius": "4px"})
            )

        return checkboxes

    @app.callback(
        Output("target-prevalence-display", "children"),
        Input("target-prevalence-range", "value")
    )
    def update_target_prevalence_display(value: List[float]) -> str:
        if value and len(value) == 2:
            return f"{value[0]:.1f}% - {value[1]:.1f}%"
        return ""

    @app.callback(
        Output("ratio-display", "children"),
        Input("ratio-range", "value")
    )
    def update_ratio_display(value: List[float]) -> str:
        if value and len(value) == 2:
            return f"{value[0]:.1f} - {value[1]:.1f}"
        return ""

    @app.callback(
        Output("cluster-prevalence-display", "children"),
        [Input("cluster-prevalence-slider", "value"), Input("cluster-view-store", "data")]
    )
    def update_cluster_prevalence_display(value: int, selected_cluster: Optional[str]) -> str:
        if value is None:
            value = 0
        if selected_cluster and selected_cluster != "all":
            return f"≥ {value}% (Active for Cluster {selected_cluster})"
        return f"≥ {value}% (Inactive - select a specific cluster)"

    @app.callback(
        Output("divergence-cluster-display", "children"),
        [Input("divergence-cluster-scope", "value"),
         Input("top-n-sd-filter", "value")]
    )
    def update_divergence_cluster_display(
        selected_clusters: Optional[List[str]],
        top_n_value: Optional[int],
    ) -> str:
        chosen_clusters = selected_clusters if isinstance(selected_clusters, list) else []
        chosen_clusters = [str(cluster) for cluster in chosen_clusters if cluster]

        if not top_n_value or top_n_value <= 0:
            if len(chosen_clusters) >= 2:
                return f"Scoped divergence prepared for {', '.join(chosen_clusters)}. Set Top N > 0 to activate."
            if len(chosen_clusters) == 1:
                return f"Only {chosen_clusters[0]} selected. Pick at least 2 clusters for scoped divergence."
            return "Scope defaults to all clusters. Set Top N > 0 to activate divergence filtering."

        if len(chosen_clusters) >= 2:
            return f"Top N SD is using selected clusters: {', '.join(chosen_clusters)}."
        if len(chosen_clusters) == 1:
            return f"Top N SD is using all clusters because only {chosen_clusters[0]} is selected."
        return "Top N SD is using all available clusters."

    @app.callback(
        Output("heritage-selection-store", "data"),
        Input({"type": "heritage-checkbox", "index": dash.ALL}, "value"),
        prevent_initial_call=False
    )
    def collect_heritage_selections(heritage_values: List[List[str]]) -> List[str]:
        if not heritage_values:
            return []

        selected = []
        for value_list in heritage_values:
            if value_list:
                selected.extend(value_list)
        return selected

    @app.callback(
        Output("composite-plot-container", "style", allow_duplicate=True),
        Input("main-tabs", "value"),
        State("composite-plot", "figure"),
        prevent_initial_call=True
    )
    def update_container_style_on_tab_change(tab_value: str, plot_figure: Optional[Dict]) -> Dict:
        if tab_value == "dashboard" and plot_figure:
            plot_height = plot_figure.get("layout", {}).get("height", 400)
            if plot_height:
                return {
                    "width": "100%",
                    "marginBottom": "60px",
                    "overflow": "visible",
                    "height": f"{plot_height}px",
                }
        return no_update
