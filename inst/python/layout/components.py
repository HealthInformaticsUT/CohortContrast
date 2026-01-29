"""
Layout components for ContrastViewer.
"""

from dash import html, dcc
import dash_bootstrap_components as dbc


def create_loading_overlay(message: str = "Loading...") -> html.Div:
    """
    Create a full-screen loading overlay with a message.
    
    Args:
        message: Text to display while loading
        
    Returns:
        HTML Div with loading spinner and message
    """
    return html.Div([
        html.Div([
            html.Div([
                html.Div(className="spinner-border text-primary", role="status", style={
                    "width": "3rem",
                    "height": "3rem",
                    "borderWidth": "0.3rem"
                }),
                html.Div([
                    html.P(message, style={
                        "marginTop": "20px",
                        "fontSize": "18px",
                        "fontWeight": "500",
                        "color": "#2c3e50"
                    })
                ])
            ], style={
                "textAlign": "center",
                "position": "absolute",
                "top": "50%",
                "left": "50%",
                "transform": "translate(-50%, -50%)"
            })
        ], style={
            "position": "fixed",
            "top": "0",
            "left": "0",
            "width": "100%",
            "height": "100%",
            "backgroundColor": "rgba(255, 255, 255, 0.9)",
            "zIndex": "9999",
            "display": "flex",
            "justifyContent": "center",
            "alignItems": "center"
        })
    ], id="loading-overlay")


def create_sidebar() -> html.Div:
    """
    Create the collapsible sidebar component.
    
    Returns:
        HTML Div containing the sidebar
    """
    return html.Div([
        # Sidebar header with toggle button
        html.Div([
            html.H4("Disease Data Viewer", style={
                "color": "#2c3e50",
                "margin": "0",
                "fontWeight": "600"
            }),
            html.Button(
                "◀",
                id="toggle-sidebar",
                style={
                    "background": "none",
                    "border": "none",
                    "fontSize": "20px",
                    "cursor": "pointer",
                    "color": "#666"
                }
            )
        ], style={
            "display": "flex",
            "justifyContent": "space-between",
            "alignItems": "center",
            "marginBottom": "20px",
            "paddingBottom": "15px",
            "borderBottom": "2px solid #e9ecef"
        }),
        
        # Search input
        html.Div([
            html.Label("Search Studies:", style={
                "fontWeight": "500",
                "marginBottom": "5px",
                "display": "block",
                "color": "#495057"
            }),
            dcc.Input(
                id="study-search",
                type="text",
                placeholder="Type to filter studies...",
                style={
                    "width": "100%",
                    "padding": "10px",
                    "border": "1px solid #ced4da",
                    "borderRadius": "5px",
                    "fontSize": "14px"
                }
            )
        ], style={"marginBottom": "15px"}),
        
        # Study table placeholder
        html.Div(id="study-table-container", style={
            "flex": "1",
            "overflowY": "auto",
            "overflowX": "hidden"
        })
    ], id="sidebar", style={
        "width": "300px",
        "backgroundColor": "#f8f9fa",
        "padding": "20px",
        "borderRight": "1px solid #dee2e6",
        "height": "100vh",
        "overflowY": "auto",
        "transition": "width 0.3s ease",
        "display": "flex",
        "flexDirection": "column"
    })


def create_main_content() -> html.Div:
    """
    Create the main content area.
    
    Returns:
        HTML Div containing the main content
    """
    return html.Div([
        # Content header
        html.Div([
            html.H3(
                "Select a study from the sidebar",
                id="content-header",
                style={
                    "color": "#2c3e50",
                    "marginBottom": "5px"
                }
            ),
            html.P(
                "Click on a row to view details",
                id="content-subheader",
                style={
                    "color": "#6c757d",
                    "marginBottom": "0"
                }
            )
        ], style={
            "marginBottom": "25px",
            "paddingBottom": "15px",
            "borderBottom": "2px solid #e9ecef"
        }),
        
        # Tabs container
        html.Div(id="tabs-container", children=[
            html.P("Select a study to view data tables", style={
                "color": "#6c757d",
                "textAlign": "center",
                "marginTop": "50px"
            })
        ])
    ], id="main-content", style={
        "flex": "1",
        "padding": "25px",
        "overflowY": "auto",
        "backgroundColor": "white"
    })


def create_filter_sidebar() -> html.Div:
    """
    Create the filter controls sidebar.
    
    Returns:
        HTML Div containing filter controls
    """
    return html.Div([
        html.H4("Filters", style={
            "fontWeight": "600",
            "marginBottom": "20px",
            "color": "#2c3e50"
        }),
        
        # Heritage checkboxes container
        html.Div([
            html.Label("Heritage Types:", style={
                "fontWeight": "500",
                "marginBottom": "10px",
                "display": "block"
            }),
            html.Div(id="heritage-checkboxes")
        ], style={"marginBottom": "20px"}),
        
        # Target prevalence slider
        html.Div([
            html.Label("Target Prevalence (%):", style={
                "fontWeight": "500",
                "marginBottom": "10px",
                "display": "block"
            }),
            dcc.RangeSlider(
                id="target-prevalence-slider",
                min=0,
                max=100,
                step=0.5,
                value=[1, 100],
                marks={0: "0%", 25: "25%", 50: "50%", 75: "75%", 100: "100%"}
            ),
            html.Div(id="target-prevalence-display", style={
                "textAlign": "center",
                "marginTop": "5px",
                "fontSize": "12px",
                "color": "#666"
            })
        ], style={"marginBottom": "20px"}),
        
        # Ratio slider
        html.Div([
            html.Label("Prevalence Ratio:", style={
                "fontWeight": "500",
                "marginBottom": "10px",
                "display": "block"
            }),
            dcc.RangeSlider(
                id="ratio-slider",
                min=0,
                max=10,
                step=0.1,
                value=[1, 10],
                marks={0: "0", 2: "2", 5: "5", 8: "8", 10: "10+"}
            ),
            html.Div(id="ratio-display", style={
                "textAlign": "center",
                "marginTop": "5px",
                "fontSize": "12px",
                "color": "#666"
            })
        ], style={"marginBottom": "20px"}),
        
        # Apply filters button
        html.Button(
            "Apply Filters",
            id="apply-filters-btn",
            className="apply-filters-button",
            style={
                "width": "100%",
                "padding": "12px",
                "backgroundColor": "#6B8FB9",
                "color": "white",
                "border": "none",
                "borderRadius": "5px",
                "cursor": "pointer",
                "fontWeight": "500",
                "fontSize": "14px",
                "transition": "all 0.2s ease"
            }
        )
    ], style={
        "padding": "20px",
        "backgroundColor": "#f8f9fa",
        "borderRadius": "8px",
        "marginBottom": "20px"
    })

