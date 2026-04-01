"""Dashboard tab layout builders."""

from typing import Dict, List, Optional

from dash import dcc, html
from dash_ag_grid import AgGrid

from config.constants import HERITAGE_COLORS, HERITAGE_ORDER
from utils.helpers import (
    create_empty_figure,
    format_column_name,
    format_heritage_label,
    is_exports_enabled,
)

_HERITAGE_VALUE_FORMATTER_JS = """
function(params) {
    if (!params.value) return '';
    var heritage = params.value;
    var mapping = {
        'procedure_occurrence': 'Procedures',
        'condition_occurrence': 'Conditions',
        'drug_exposure': 'Drug Exposures',
        'measurement': 'Measurements',
        'observation': 'Observations',
        'device_exposure': 'Device Exposures',
        'visit_occurrence': 'Visits',
        'specimen': 'Specimens',
        'note': 'Notes',
        'note_nlp': 'Note NLP',
        'death': 'Deaths',
        'payer_plan_period': 'Payer Plan Periods',
        'cost': 'Costs',
        'episode': 'Episodes',
        'metadata': 'Metadata'
    };
    if (mapping[heritage.toLowerCase()]) {
        return mapping[heritage.toLowerCase()];
    }
    return heritage.replace(/_/g, ' ').replace(/\\b\\w/g, function(l) { return l.toUpperCase(); });
}
"""

_ROW_STYLE_JS = """
function(params) {
    var style = {};
    if (params.data && params.data._show === false) {
        style.backgroundColor = '#d3d3d3';
        style.color = '#6c757d';
    } else if (params.node && params.node.rowIndex % 2 === 0) {
        style.backgroundColor = '#f8f9fa';
    }
    return style;
}
"""

_ON_FIRST_DATA_RENDERED_JS = """
function(params) {
    params.api.sizeColumnsToFit();
    params.api.applyColumnState({
        state: [{ colId: '_show', sort: 'desc' }],
        defaultState: { sort: null }
    });
}
"""

_ON_GRID_SIZE_CHANGED_JS = """
function(params) {
    if (params.clientWidth > 0) {
        params.api.sizeColumnsToFit();
    }
}
"""


def _build_dashboard_column_defs(summary_mode: bool) -> List[Dict]:
    show_col = {
        "field": "_show",
        "headerName": "Show",
        "width": 100,
        "cellStyle": {"textAlign": "center"},
        "cellRenderer": "agCheckboxCellRenderer",
        "cellEditor": "agCheckboxCellEditor",
        "editable": True,
        "sort": "desc",
    }
    if summary_mode:
        show_col["filter"] = "agSetColumnFilter"
    else:
        show_col.update({
            "suppressMovable": True,
            "resizable": True,
            "suppressSizeToFit": True,
            "filter": "agSetColumnFilter",
            "filterParams": {"values": [True, False], "buttons": ["apply", "reset"]},
        })

    concept_col = {
        "field": "CONCEPT_NAME",
        "headerName": format_column_name("CONCEPT_NAME"),
        "flex": 3,
        "minWidth": 200,
        "filter": "agTextColumnFilter",
        "resizable": True,
        "sortable": True,
    }
    if not summary_mode:
        concept_col["filterParams"] = {"caseSensitive": False, "buttons": ["apply", "reset"]}

    heritage_col = {
        "field": "HERITAGE",
        "headerName": "Heritage Type",
        "flex": 2,
        "minWidth": 150,
        "filter": "agTextColumnFilter",
        "resizable": True,
        "sortable": True,
        "valueFormatter": {"function": _HERITAGE_VALUE_FORMATTER_JS},
    }
    if not summary_mode:
        heritage_col["filterParams"] = {"caseSensitive": False, "buttons": ["apply", "reset"]}

    median_col = {
        "field": "MEDIAN_FIRST_OCCURRENCE",
        "headerName": "Median First Occurrence (days)",
        "flex": 2,
        "minWidth": 200,
        "filter": "agNumberColumnFilter",
        "valueFormatter": {"function": "params.value != null ? params.value.toFixed(1) + ' days' : ''"},
        "resizable": True,
        "sortable": True,
        "type": "numericColumn",
    }
    prevalence_col = {
        "field": "TARGET_SUBJECT_PREVALENCE_PCT",
        "headerName": format_column_name("TARGET_SUBJECT_PREVALENCE") + " (%)",
        "flex": 2,
        "minWidth": 180,
        "filter": "agNumberColumnFilter",
        "valueFormatter": {"function": "params.value ? params.value.toFixed(2) + '%' : ''"},
        "resizable": True,
        "sortable": True,
        "type": "numericColumn",
    }
    ratio_col = {
        "field": "PREVALENCE_DIFFERENCE_RATIO_DISPLAY",
        "headerName": format_column_name("PREVALENCE_DIFFERENCE_RATIO"),
        "flex": 2,
        "minWidth": 180,
        "filter": "agNumberColumnFilter",
        "valueFormatter": {"function": "params.value ? params.value.toFixed(2) : ''"},
        "resizable": True,
        "sortable": True,
        "type": "numericColumn",
    }
    if not summary_mode:
        numeric_filter_params = {"buttons": ["apply", "reset"], "allowedCharPattern": "\\d\\-\\."}
        median_col["filterParams"] = numeric_filter_params
        prevalence_col["filterParams"] = numeric_filter_params
        ratio_col["filterParams"] = numeric_filter_params

    return [show_col, concept_col, heritage_col, median_col, prevalence_col, ratio_col]


def _build_dashboard_grid_options(summary_mode: bool) -> Dict:
    options = {
        "pagination": True,
        "paginationPageSize": 20,
        "animateRows": True,
        "suppressRowClickSelection": True,
        "getRowStyle": {"function": _ROW_STYLE_JS},
    }
    if not summary_mode:
        options["onFirstDataRendered"] = {"function": _ON_FIRST_DATA_RENDERED_JS}
        options["onGridSizeChanged"] = {"function": _ON_GRID_SIZE_CHANGED_JS}
    return options


def build_dashboard_table(
    dashboard_data: List[Dict],
    *,
    summary_mode: bool,
) -> AgGrid:
    return AgGrid(
        id="dashboard-table",
        rowData=dashboard_data,
        columnDefs=_build_dashboard_column_defs(summary_mode),
        defaultColDef={"resizable": True, "sortable": True, "filter": True, "floatingFilter": True},
        dashGridOptions=_build_dashboard_grid_options(summary_mode),
        style={"width": "100%", "height": "calc(100vh - 250px)", "minHeight": "500px"},
        className="ag-theme-alpine",
        columnSize="sizeToFit",
    )


def _build_description(study_description: Optional[str], *, summary_mode: bool) -> List[html.Div]:
    if not study_description:
        return []
    border_color = "#27ae60" if summary_mode else "#2c3e50"
    return [html.Div([
        html.P(
            study_description,
            style={
                "backgroundColor": "#f8f9fa",
                "padding": "15px",
                "borderRadius": "6px",
                "borderLeft": f"4px solid {border_color}",
                "marginBottom": "20px",
                "color": "#495057",
                "fontSize": "14px",
                "whiteSpace": "pre-wrap",
            },
        )
    ])]


def _build_info_text(concept_count: int, *, summary_mode: bool) -> html.Div:
    line1 = (
        f"Displaying {concept_count:,} concepts from pre-computed summary data."
        if summary_mode
        else f"Displaying {concept_count:,} concepts. Use the checkbox in the 'Show' column to toggle visibility for plotting."
    )
    return html.Div([
        html.P(line1, className="text-muted mb-2", style={"fontSize": "14px"}),
        html.P(
            "Tip: Click filter icons in column headers for advanced filtering. Numeric columns support operators: equals, not equal, less than, greater than, in range (between), and blank.",
            className="text-muted mb-4",
            style={"fontSize": "12px", "fontStyle": "italic", "color": "#7f8c8d"},
        ),
    ], style={"marginTop": "20px", "marginBottom": "15px"})


def _build_cluster_view_selector(summary_mode: bool) -> html.Div:
    summary_style = {"display": "inline-block"} if summary_mode else {"display": "none"}
    patient_style = {"display": "none"} if summary_mode else {"display": "inline-block"}
    return html.Div([
        html.Span("View: ", style={"fontWeight": "500", "marginRight": "10px", "color": "#666"}),
        dcc.RadioItems(
            id="cluster-view-selector-summary",
            options=[{"label": "All", "value": "all"}],
            value="all",
            inline=True,
            style=summary_style,
            labelStyle={"marginRight": "15px", "cursor": "pointer"} if summary_mode else None,
            inputStyle={"marginRight": "5px"} if summary_mode else None,
        ),
        dcc.RadioItems(
            id="cluster-view-selector-patient",
            options=[{"label": "All", "value": "all"}],
            value="all",
            inline=True,
            style=patient_style,
            labelStyle={"marginRight": "15px", "cursor": "pointer"} if not summary_mode else None,
            inputStyle={"marginRight": "5px"} if not summary_mode else None,
        ),
    ], style={"display": "inline-block", "marginLeft": "30px", "verticalAlign": "middle"})


def _build_composite_legend() -> html.Div:
    domain_items = []
    for heritage in HERITAGE_ORDER:
        heritage_color = HERITAGE_COLORS.get(heritage)
        if not heritage_color:
            continue
        domain_items.append(
            html.Span(
                [
                    html.Span(
                        style={
                            "width": "10px",
                            "height": "10px",
                            "backgroundColor": heritage_color,
                            "border": "1px solid rgba(0, 0, 0, 0.15)",
                            "borderRadius": "2px",
                            "display": "inline-block",
                            "marginRight": "6px",
                            "verticalAlign": "middle",
                            "flexShrink": "0",
                        }
                    ),
                    html.Span(
                        format_heritage_label(heritage),
                        style={"fontSize": "11px", "color": "#555", "lineHeight": "1.1"},
                    ),
                ],
                style={
                    "display": "inline-flex",
                    "alignItems": "center",
                    "padding": "4px 8px",
                    "backgroundColor": "#ffffff",
                    "border": "1px solid #e2e6ea",
                    "borderRadius": "999px",
                    "marginRight": "8px",
                    "marginBottom": "6px",
                },
            )
        )

    return html.Div([
        html.Div([
            html.Span("Enrichment:", style={"fontWeight": "600", "marginRight": "8px", "color": "#555", "fontSize": "12px"}),
            html.Span("1", style={"fontSize": "10px", "color": "#666", "marginRight": "5px", "verticalAlign": "middle"}),
            html.Div(
                style={
                    "width": "100px",
                    "height": "12px",
                    "background": "linear-gradient(to right, #440154, #482878, #3e4989, #31688e, #26828e, #1f9e89, #35b779, #6ece58, #b5de2b, #fde725)",
                    "borderRadius": "2px",
                    "display": "inline-block",
                    "verticalAlign": "middle",
                }
            ),
            html.Span("100+", style={"fontSize": "10px", "color": "#666", "marginLeft": "5px", "verticalAlign": "middle"}),
        ], style={"display": "flex", "alignItems": "center", "marginRight": "30px"}),
        html.Div([
            html.Span("Significance:", style={"fontWeight": "600", "marginRight": "8px", "color": "#555", "fontSize": "12px"}),
            html.Span("*", style={"color": "#2E86AB", "fontSize": "14px", "marginRight": "3px"}),
            html.Span("Significant", style={"fontSize": "11px", "color": "#666", "marginRight": "12px"}),
            html.Span("*", style={"color": "#666666", "fontSize": "14px", "marginRight": "3px"}),
            html.Span("Not significant", style={"fontSize": "11px", "color": "#666"}),
        ], style={"display": "flex", "alignItems": "center", "marginRight": "30px"}),
        html.Div([
            html.Span("Median Occurrences:", style={"fontWeight": "600", "marginRight": "8px", "color": "#555", "fontSize": "12px"}),
            html.Span("*", style={"color": "#2ECC71", "fontSize": "14px", "marginRight": "3px"}),
            html.Span("1", style={"fontSize": "11px", "color": "#666", "marginRight": "10px"}),
            html.Span("*", style={"color": "#F1C40F", "fontSize": "14px", "marginRight": "3px"}),
            html.Span("2", style={"fontSize": "11px", "color": "#666", "marginRight": "10px"}),
            html.Span("*", style={"color": "#E74C3C", "fontSize": "14px", "marginRight": "3px"}),
            html.Span("3+", style={"fontSize": "11px", "color": "#666"}),
        ], style={"display": "flex", "alignItems": "center", "marginRight": "30px"}),
        html.Div([
            html.Span("Domains:", style={"fontWeight": "600", "marginRight": "8px", "color": "#555", "fontSize": "12px"}),
            html.Span(
                domain_items,
                style={
                    "display": "flex",
                    "flexWrap": "wrap",
                    "alignItems": "center",
                    "maxWidth": "100%",
                },
            ),
        ], style={"display": "flex", "alignItems": "center", "maxWidth": "100%"}),
    ], style={
        "backgroundColor": "#f8f9fa",
        "padding": "10px 15px",
        "borderRadius": "6px",
        "marginBottom": "15px",
        "border": "1px solid #e9ecef",
        "display": "flex",
        "flexWrap": "wrap",
        "alignItems": "center",
        "gap": "10px 0",
    })


def _build_composite_section(summary_mode: bool) -> html.Div:
    exports_enabled = is_exports_enabled()

    return html.Div([
        html.Div([
            html.Div([
                html.H4("The Composite", style={"color": "#2c3e50", "fontWeight": "600", "marginBottom": "10px", "display": "inline-block"}),
                html.Span(
                    id="silhouette-score-display",
                    style={"marginLeft": "15px", "color": "#666", "fontSize": "13px", "verticalAlign": "middle"},
                ),
                _build_cluster_view_selector(summary_mode),
            ], style={"display": "inline-block", "verticalAlign": "middle"}),
            html.Div([
                html.Div([
                    html.Button(
                        "Export Active Concepts (TSV)",
                        id="download-dashboard-tsv-btn",
                        n_clicks=0,
                        style={
                            "padding": "8px 14px",
                            "backgroundColor": "#1f6f8b",
                            "color": "white",
                            "border": "none",
                            "borderRadius": "6px",
                            "fontSize": "12px",
                            "fontWeight": "600",
                            "cursor": "pointer",
                            "boxShadow": "0 1px 3px rgba(0,0,0,0.15)",
                        },
                        className="download-dashboard-tsv-button",
                        disabled=not exports_enabled,
                    ),
                    html.Button(
                        "Save Composite (PNG)",
                        id="download-composite-png-btn",
                        n_clicks=0,
                        style={
                            "padding": "8px 14px",
                            "backgroundColor": "#2c7a5f",
                            "color": "white",
                            "border": "none",
                            "borderRadius": "6px",
                            "fontSize": "12px",
                            "fontWeight": "600",
                            "cursor": "pointer",
                            "boxShadow": "0 1px 3px rgba(0,0,0,0.15)",
                        },
                        className="download-composite-png-button",
                        disabled=not exports_enabled,
                    ),
                ], style={"display": "flex", "gap": "8px", "justifyContent": "flex-end", "flexWrap": "wrap"}),
                html.Div(
                    (
                        "TSV exports active concepts. PNG saves the current composite view."
                        if exports_enabled
                        else "Exports are disabled for this viewer session."
                    ),
                    style={"fontSize": "11px", "color": "#6c757d", "marginTop": "4px", "textAlign": "right"},
                ),
            ], style={"display": "inline-block", "textAlign": "right"}),
        ], style={
            "marginBottom": "15px",
            "display": "flex",
            "justifyContent": "space-between",
            "alignItems": "flex-start",
            "gap": "12px",
            "flexWrap": "wrap",
        }),
        _build_composite_legend(),
        dcc.Loading(
            type="circle",
            children=dcc.Graph(
                id="composite-plot",
                figure=create_empty_figure("Initialising dashboard visualisation. This may take a moment for larger studies."),
                style={"width": "100%"},
                config={"displayModeBar": False},
            ),
        ),
        dcc.Download(id="dashboard-export-download"),
        html.Div(id="composite-image-download-status", style={"display": "none"}),
    ], id="composite-plot-container", style={"width": "100%", "marginBottom": "20px", "overflow": "visible"})


def build_dashboard_tab(
    dashboard_table: AgGrid,
    *,
    study_description: Optional[str],
    concept_count: int,
    summary_mode: bool,
) -> dcc.Tab:
    heading = "Features Dashboard (Summary Mode)" if summary_mode else "Features Dashboard"
    heading_color = "#27ae60" if summary_mode else "#2c3e50"
    content_style = {"padding": "20px"} if summary_mode else {"width": "100%", "padding": "20px", "boxSizing": "border-box"}
    return dcc.Tab(
        label="Dashboard",
        value="dashboard",
        children=[
            html.Div([
                html.H3(heading, className="mb-3", style={"color": heading_color, "fontWeight": "600"}),
                *_build_description(study_description, summary_mode=summary_mode),
                _build_composite_section(summary_mode),
                _build_info_text(concept_count, summary_mode=summary_mode),
                html.Div(
                    dashboard_table,
                    style={
                        "width": "100%",
                        "margin": "0",
                        "padding": "0",
                        "marginTop": "10px",
                        "boxSizing": "border-box",
                        "overflow": "auto",
                        "position": "relative",
                    },
                    id="dashboard-table-container",
                ),
            ], style=content_style),
        ],
    )
