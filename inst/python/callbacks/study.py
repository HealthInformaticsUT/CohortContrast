"""Study selection and mode-related callback registration."""

from typing import Dict, List, Optional, Tuple

import pandas as pd
from dash import Input, Output, State, dash_table, html, no_update


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

        def _coerce_count(value):
            """Normalize table counts to integers when possible, else None."""
            if value is None:
                return None
            if isinstance(value, str) and value.strip().upper() == "NA":
                return None
            try:
                if pd.isna(value):
                    return None
            except Exception:
                pass
            try:
                return int(float(value))
            except (TypeError, ValueError):
                return None

        display_df = filtered_df.copy()

        mode_map = {
            "summary": "Summary",
            "Summary": "Summary",
            "patient": "Patient-level",
            "Patient": "Patient-level",
            "Patient-level": "Patient-level",
        }
        if "mode" in display_df.columns:
            display_df["mode"] = display_df["mode"].apply(
                lambda m: mode_map.get(m, str(m) if m is not None else "Unknown")
            )
        else:
            display_df["mode"] = "Unknown"

        display_df["patients"] = display_df.get("target_patients")

        concept_candidates = [col for col in ["chi2y_count", "z_count", "significant_concepts", "concept_count"] if col in display_df.columns]
        if concept_candidates:
            display_df["concepts"] = display_df[concept_candidates].bfill(axis=1).iloc[:, 0]
        else:
            display_df["concepts"] = None

        display_df["patients"] = display_df["patients"].apply(_coerce_count)
        display_df["concepts"] = display_df["concepts"].apply(_coerce_count)

        columns = [
            {"name": "Study", "id": "study"},
            {"name": "Mode", "id": "mode"},
            {"name": "Patients", "id": "patients", "type": "numeric"},
            {"name": "Concepts", "id": "concepts", "type": "numeric"},
        ]

        required_cols = ["study", "mode", "patients", "concepts"]
        data = display_df[required_cols].copy().to_dict("records")

        return dash_table.DataTable(
            id="study-table",
            columns=columns,
            data=data,
            row_selectable="single",
            selected_rows=[],
            page_action="native",
            page_size=10,
            style_table={"overflowX": "auto", "fontSize": "12px"},
            style_cell={
                "textAlign": "left",
                "padding": "8px",
                "fontFamily": "Arial, sans-serif",
                "fontSize": "12px"
            },
            style_cell_conditional=[
                {"if": {"column_id": "study"}, "minWidth": "320px", "width": "55%", "maxWidth": "680px"},
                {"if": {"column_id": "mode"}, "minWidth": "140px", "width": "140px", "maxWidth": "180px"},
                {"if": {"column_id": "patients"}, "minWidth": "110px", "width": "110px", "maxWidth": "140px", "textAlign": "right"},
                {"if": {"column_id": "concepts"}, "minWidth": "110px", "width": "110px", "maxWidth": "140px", "textAlign": "right"},
            ],
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
        [State("study-table", "data"),
         State("selected-study-store", "data")]
    )
    def update_selected_study(
        selected_rows: List[int],
        table_data: List[Dict],
        current_selected_study: Optional[str],
    ) -> Optional[str]:
        """Update the selected study store when a study row is clicked."""
        if not selected_rows or not table_data:
            return no_update

        selected_row_idx = selected_rows[0]
        if selected_row_idx < len(table_data):
            selected_study = table_data[selected_row_idx].get("study")
            if selected_study == current_selected_study:
                return no_update
            return selected_study

        return no_update

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
