"""Study selection and mode-related callback registration."""

from typing import Dict, List, Optional, Tuple

import pandas as pd
from dash import Input, Output, State, dash_table, html


def register_study_callbacks(app, study_summaries_df: pd.DataFrame, default_study: Optional[str]) -> None:
    """Register callbacks for study selection and mode state UI."""

    @app.callback(
        Output("study-table-container", "children"),
        Input("study-search", "value")
    )
    def update_study_table(search_value: Optional[str]) -> html.Div:
        """Update the study table based on search query."""
        if study_summaries_df.empty:
            return html.P("No studies found.", style={"color": "#666", "padding": "10px"})

        filtered_df = study_summaries_df.copy()
        if search_value:
            search_lower = search_value.lower()
            filtered_df = filtered_df[
                filtered_df["study"].str.lower().str.contains(search_lower, na=False)
            ]

        if filtered_df.empty:
            return html.P("No studies match your search.", style={"color": "#666", "padding": "10px"})

        columns = [
            {"name": "Study", "id": "study"},
            {"name": "Mode", "id": "mode"},
            {"name": "Target Patients", "id": "target_patients", "type": "numeric"},
            {"name": "Significant Concepts", "id": "z_count", "type": "numeric"}
        ]

        required_cols = ["study", "mode", "target_patients", "z_count"]
        available_cols = [col for col in required_cols if col in filtered_df.columns]
        data = filtered_df[available_cols].copy().to_dict("records")

        return dash_table.DataTable(
            id="study-table",
            columns=columns,
            data=data,
            row_selectable="single",
            selected_rows=[],
            style_table={"overflowX": "auto", "fontSize": "12px"},
            style_cell={
                "textAlign": "left",
                "padding": "8px",
                "fontFamily": "Arial, sans-serif",
                "fontSize": "12px"
            },
            style_header={
                "backgroundColor": "rgb(70, 130, 180)",
                "color": "white",
                "fontWeight": "bold",
                "textAlign": "center"
            },
            style_data={"whiteSpace": "normal", "height": "auto"},
            style_data_conditional=[
                {"if": {"row_index": "odd"}, "backgroundColor": "rgb(248, 248, 248)"},
                {"if": {"state": "selected"}, "backgroundColor": "rgb(220, 220, 255)"}
            ]
        )

    @app.callback(
        Output("selected-study-store", "data"),
        Input("study-table", "selected_rows"),
        State("study-table", "data")
    )
    def update_selected_study(selected_rows: List[int], table_data: List[Dict]) -> Optional[str]:
        """Update the selected study store when a study row is clicked."""
        if not selected_rows or not table_data:
            return default_study

        selected_row_idx = selected_rows[0]
        if selected_row_idx < len(table_data):
            return table_data[selected_row_idx]["study"]

        return default_study

    @app.callback(
        [Output("data-mode-badge", "children"), Output("data-mode-badge", "style")],
        Input("data-mode-store", "data"),
        prevent_initial_call=False
    )
    def update_data_mode_badge(data_mode: str) -> Tuple[str, Dict]:
        """Update the data mode badge based on the active study mode."""
        if data_mode == "summary":
            return (
                "Summary Mode",
                {
                    "display": "inline-block",
                    "marginLeft": "15px",
                    "padding": "5px 12px",
                    "borderRadius": "15px",
                    "fontSize": "12px",
                    "fontWeight": "bold",
                    "verticalAlign": "middle",
                    "backgroundColor": "#27ae60",
                    "color": "white"
                }
            )
        if data_mode == "patient":
            return (
                "Patient Level Data",
                {
                    "display": "inline-block",
                    "marginLeft": "15px",
                    "padding": "5px 12px",
                    "borderRadius": "15px",
                    "fontSize": "12px",
                    "fontWeight": "bold",
                    "verticalAlign": "middle",
                    "backgroundColor": "#3498db",
                    "color": "white"
                }
            )
        return ("", {"display": "none"})

    @app.callback(
        Output("clustering-scope-container", "style"),
        Input("data-mode-store", "data"),
        prevent_initial_call=False
    )
    def update_clustering_scope_visibility(data_mode: str) -> Dict:
        """Gray out clustering scope options in summary mode."""
        base_style = {"marginBottom": "15px", "padding": "0 10px"}

        if data_mode == "summary":
            return {
                **base_style,
                "opacity": "0.5",
                "pointerEvents": "none",
                "position": "relative"
            }
        return base_style

    @app.callback(
        [Output("recluster-btn", "style"), Output("recluster-btn", "disabled")],
        Input("data-mode-store", "data"),
        prevent_initial_call=False
    )
    def update_recluster_button_state(data_mode: str) -> Tuple[Dict, bool]:
        """Disable recluster in summary mode."""
        base_style = {
            "width": "calc(100% - 20px)",
            "padding": "10px",
            "margin": "10px",
            "backgroundColor": "#8b7fa8",
            "color": "white",
            "border": "none",
            "borderRadius": "6px",
            "fontSize": "14px",
            "fontWeight": "500",
            "cursor": "pointer",
            "boxShadow": "0 2px 4px rgba(0,0,0,0.1)",
            "transition": "all 0.2s ease"
        }

        if data_mode == "summary":
            return (
                {
                    **base_style,
                    "backgroundColor": "#c8c8c8",
                    "color": "#6b6b6b",
                    "cursor": "not-allowed",
                    "boxShadow": "none",
                    "opacity": "0.85"
                },
                True,
            )

        return base_style, False
