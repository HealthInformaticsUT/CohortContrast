"""Demographics tab callback registration and helpers."""

import os
from pathlib import Path
from typing import Dict, Optional

import pandas as pd
from dash import Input, Output, dash_table, dcc, html

from callbacks.demographics_helpers import (
    _build_cluster_demographics_rows,
    _build_cluster_view_options,
    _build_demographic_kpi_cards,
    _build_demographics_concept_rows,
    _build_ordinal_dropdown_options,
    _create_demographics_message_figure,
    _create_ordinal_demographics_figures,
    _create_summary_age_histogram_figure,
    _prepare_demographics_context,
)
from data.cache import get_or_load_parquet_data


def register_demographics_callbacks(
    app,
    *,
    data_dir: Path,
    cache_store,
    loaded_parquet_data_store: Dict[str, Dict[str, pd.DataFrame]],
) -> None:
    """Register demographics tab callbacks and related helper functions."""
    DATA_DIR = data_dir
    cache = cache_store
    loaded_parquet_data = loaded_parquet_data_store
    exports_enabled = os.environ.get("CONTRAST_VIEWER_ALLOW_EXPORTS", "1").strip().lower() not in {
        "0", "false", "no", "off"
    }
    graph_config = {"displayModeBar": True, "scrollZoom": False}
    if not exports_enabled:
        graph_config["modeBarButtonsToRemove"] = ["toImage"]

    @app.callback(
        Output("demographics-plot-container", "children"),
        [Input("selected-study-store", "data"),
         Input("clustering-results-store", "data"),
         Input("data-mode-store", "data")],
        prevent_initial_call=True
    )
    def update_demographics_plot(
        selected_study: Optional[str],
        clustering_results: Optional[Dict],
        data_mode: Optional[str]
    ):
        """Build the demographics tab for summary and patient data modes."""
        if not selected_study:
            return html.P(
                "Select a study to view demographics.",
                style={"color": "#999", "textAlign": "center", "padding": "50px"}
            )
        
        parquet_data = get_or_load_parquet_data(selected_study, DATA_DIR, cache)
        if parquet_data is None:
            return html.P(
                "Study data not loaded.",
                style={"color": "#999", "textAlign": "center", "padding": "50px"}
            )
        
        loaded_parquet_data[selected_study] = parquet_data
        
        try:
            summary_context = _prepare_demographics_context(parquet_data, clustering_results)
            
            cluster_view_options = _build_cluster_view_options(summary_context)
            ordinal_options, ordinal_default = _build_ordinal_dropdown_options(summary_context)
            default_min_patients = 200
            age_rows, sex_rows = _build_demographics_concept_rows(
                summary_context,
                default_min_patients,
                cluster_view="overall"
            )
            ordinal_age_fig, ordinal_sex_fig = _create_ordinal_demographics_figures(summary_context, ordinal_default)
            
            summary_section = html.Div(
                [
                    html.Div(
                        _build_demographic_kpi_cards(summary_context),
                        id="demographics-kpi-row",
                        style={"display": "flex", "flexWrap": "wrap", "gap": "10px", "marginBottom": "16px"}
                    ),
                    html.Div(
                        [
                            dcc.Graph(
                                id="demographics-age-hist-fig",
                                figure=_create_summary_age_histogram_figure(summary_context),
                                config=graph_config,
                                style={"width": "100%"}
                            ),
                        ],
                        style={"display": "block", "marginBottom": "16px"}
                    ),
                    html.Div(
                        [
                            html.H4("Cluster Demographics", style={"fontSize": "16px", "marginBottom": "10px", "color": "#2c3e50"}),
                            dash_table.DataTable(
                                id="demographics-cluster-table",
                                columns=[
                                    {"name": "Cluster", "id": "cluster"},
                                    {"name": "Patients", "id": "patient_count"},
                                    {"name": "Age mean", "id": "age_mean"},
                                    {"name": "Age median", "id": "age_median"},
                                    {"name": "Age IQR", "id": "age_iqr"},
                                    {"name": "Male %", "id": "male_pct"},
                                    {"name": "Age delta vs overall", "id": "age_delta_vs_overall"},
                                ],
                                data=_build_cluster_demographics_rows(summary_context),
                                style_table={"overflowX": "auto"},
                                style_cell={"textAlign": "left", "padding": "8px", "fontSize": "12px"},
                                style_header={"backgroundColor": "#f1f3f5", "fontWeight": "600"},
                                page_size=10
                            ),
                        ],
                        style={"marginBottom": "18px"}
                    ),
                    html.Div(
                        [
                            html.H4("Demographics by Concept", style={"fontSize": "16px", "marginBottom": "10px", "color": "#2c3e50"}),
                            html.Div(
                                [
                                    html.Div(
                                        [
                                            html.Label("Minimum patients", style={"fontSize": "12px", "color": "#555"}),
                                            dcc.Slider(
                                                id="demographics-min-patient-slider",
                                                min=25,
                                                max=2000,
                                                step=25,
                                                value=default_min_patients,
                                                marks={25: "25", 500: "500", 1000: "1000", 1500: "1500", 2000: "2000"},
                                                tooltip={"placement": "bottom", "always_visible": False}
                                            ),
                                        ],
                                        style={"flex": "1 1 380px", "paddingRight": "15px"}
                                    ),
                                    html.Div(
                                        [
                                            html.Label("Data source", style={"fontSize": "12px", "color": "#555"}),
                                            dcc.RadioItems(
                                                id="demographics-concept-cluster-view",
                                                options=cluster_view_options,
                                                value="overall",
                                                inline=True,
                                                inputStyle={"marginRight": "5px"},
                                                labelStyle={"marginRight": "10px"}
                                            ),
                                        ],
                                        style={"flex": "1 1 260px"}
                                    ),
                                ],
                                style={
                                    "display": "flex",
                                    "flexWrap": "wrap",
                                    "gap": "10px",
                                    "backgroundColor": "#f8f9fa",
                                    "border": "1px solid #e9ecef",
                                    "borderRadius": "8px",
                                    "padding": "12px",
                                    "marginBottom": "10px"
                                }
                            ),
                            html.P(
                                "Ranking computed from current demographics data source.",
                                id="demographics-concept-source-note",
                                style={"fontSize": "12px", "color": "#666", "marginBottom": "8px"}
                            ),
                            html.Div(
                                [
                                    dash_table.DataTable(
                                        id="demographics-age-delta-table",
                                        columns=[
                                            {"name": "Segment", "id": "segment"},
                                            {"name": "Concept", "id": "concept_name"},
                                            {"name": "Patients", "id": "patient_count"},
                                            {"name": "Mean age", "id": "age_mean"},
                                            {"name": "Delta years", "id": "delta_years"},
                                        ],
                                        data=age_rows,
                                        style_table={"overflowX": "auto"},
                                        style_cell={"textAlign": "left", "padding": "8px", "fontSize": "12px"},
                                        style_header={"backgroundColor": "#f1f3f5", "fontWeight": "600"},
                                        page_size=10
                                    ),
                                    dash_table.DataTable(
                                        id="demographics-sex-skew-table",
                                        columns=[
                                            {"name": "Concept", "id": "concept_name"},
                                            {"name": "Patients", "id": "patient_count"},
                                            {"name": "Male %", "id": "male_pct"},
                                            {"name": "Female %", "id": "female_pct"},
                                            {"name": "Delta male %", "id": "delta_male_pct"},
                                        ],
                                        data=sex_rows,
                                        style_table={"overflowX": "auto"},
                                        style_cell={"textAlign": "left", "padding": "8px", "fontSize": "12px"},
                                        style_header={"backgroundColor": "#f1f3f5", "fontWeight": "600"},
                                        page_size=10
                                    ),
                                ],
                                style={"display": "grid", "gridTemplateColumns": "1fr", "gap": "12px"}
                            ),
                        ],
                        style={"marginBottom": "18px"}
                    ),
                    html.Div(
                        [
                            html.H4("Ordinal Progression", style={"fontSize": "16px", "marginBottom": "10px", "color": "#2c3e50"}),
                            dcc.Dropdown(
                                id="demographics-ordinal-concept-dropdown",
                                options=ordinal_options,
                                value=ordinal_default,
                                placeholder="Select a concept with ordinal summaries",
                                style={"marginBottom": "10px"}
                            ),
                            html.Div(
                                [
                                    dcc.Graph(
                                        id="demographics-ordinal-age-fig",
                                        figure=ordinal_age_fig,
                                        config=graph_config,
                                        style={"flex": "1 1 480px"}
                                    ),
                                    dcc.Graph(
                                        id="demographics-ordinal-sex-fig",
                                        figure=ordinal_sex_fig,
                                        config=graph_config,
                                        style={"flex": "1 1 480px"}
                                    ),
                                ],
                                style={"display": "flex", "flexWrap": "wrap", "gap": "12px"}
                            ),
                        ]
                    ),
                ],
                id="demographics-summary-section",
                style={"display": "block"}
            )
            
            if not summary_context.get("demo"):
                return html.Div(
                    [
                        html.P(
                            "Demographics data not available for this study.",
                            style={"color": "#999", "textAlign": "center", "padding": "30px"}
                        ),
                        summary_section
                    ]
                )
            
            return html.Div(
                [summary_section],
                style={"display": "block"}
            )
        
        except Exception as e:
            import traceback
            traceback.print_exc()
            return html.P(
                f"Error generating demographics plots: {str(e)}",
                style={"color": "#d62728", "textAlign": "center", "padding": "50px"}
            )
    
    
    @app.callback(
        [Output("demographics-age-delta-table", "data"),
         Output("demographics-sex-skew-table", "data"),
         Output("demographics-concept-source-note", "children")],
        [Input("selected-study-store", "data"),
         Input("clustering-results-store", "data"),
         Input("data-mode-store", "data"),
         Input("demographics-min-patient-slider", "value"),
         Input("demographics-concept-cluster-view", "value")],
        prevent_initial_call=True
    )
    def update_demographics_concept_tables(
        selected_study: Optional[str],
        clustering_results: Optional[Dict],
        data_mode: Optional[str],
        min_patients: Optional[int],
        cluster_view: Optional[str]
    ):
        """Update concept-level demographic rankings."""
        if not selected_study:
            return [], [], "Select a study."
        
        parquet_data = get_or_load_parquet_data(selected_study, DATA_DIR, cache)
        if parquet_data is None:
            return [], [], "Study data not loaded."
        
        context = _prepare_demographics_context(parquet_data, clustering_results)
        actual_data_mode = parquet_data.get("_mode", "patient")
        selected_view = cluster_view or "overall"
        age_rows, sex_rows = _build_demographics_concept_rows(context, min_patients, selected_view)
        
        if selected_view == "overall":
            if actual_data_mode == "summary":
                note = "Ranking computed from overall concept_summaries.parquet."
            else:
                note = "Ranking computed from patient-level concept demographics."
        else:
            k_value = context.get("k_value")
            if actual_data_mode == "summary" and k_value is not None:
                note = f"Ranking computed from clustering_k{k_value}_summary.parquet for {selected_view}."
            elif actual_data_mode != "summary" and k_value is not None:
                note = f"Ranking computed from patient-level clustered demographics (k={k_value}) for {selected_view}."
            else:
                note = f"Ranking computed from cluster view {selected_view}."
        
        if not age_rows and not sex_rows:
            threshold_txt = min_patients if min_patients is not None else 0
            note += f" No concepts met the minimum patient threshold ({threshold_txt})."
        
        return age_rows, sex_rows, note
    
    
    @app.callback(
        [Output("demographics-ordinal-age-fig", "figure"),
         Output("demographics-ordinal-sex-fig", "figure")],
        [Input("selected-study-store", "data"),
         Input("data-mode-store", "data"),
         Input("demographics-ordinal-concept-dropdown", "value")],
        prevent_initial_call=True
    )
    def update_demographics_ordinal_figures(
        selected_study: Optional[str],
        data_mode: Optional[str],
        selected_original_concept_id: Optional[str]
    ):
        """Update ordinal progression figures for selected concept."""
        if not selected_study:
            msg = "Select a study to view ordinal progression."
            return _create_demographics_message_figure(msg), _create_demographics_message_figure(msg)
        
        parquet_data = get_or_load_parquet_data(selected_study, DATA_DIR, cache)
        if parquet_data is None:
            msg = "Study data not loaded."
            return _create_demographics_message_figure(msg), _create_demographics_message_figure(msg)
        
        context = _prepare_demographics_context(parquet_data, clustering_results=None)
        options, default_value = _build_ordinal_dropdown_options(context)
        selected_value = selected_original_concept_id or default_value
        
        if not options:
            msg = "No ordinal concepts available for this study."
            return _create_demographics_message_figure(msg), _create_demographics_message_figure(msg)
        
        return _create_ordinal_demographics_figures(context, selected_value)
    
    
