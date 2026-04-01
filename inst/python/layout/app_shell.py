"""Main app shell layout for ContrastViewer."""

from typing import Optional

from dash import dcc, html


def build_welcome_content() -> html.Div:
    """Build the default welcome panel shown before a study is selected."""
    return html.Div([
        html.H3("Welcome to CohortContrast Viewer", style={
            "marginBottom": "20px",
            "color": "#2c3e50",
            "fontWeight": "600"
        }),
        html.P(
            "Select a study from the left sidebar to begin exploring the data.",
            style={
                "fontSize": "16px",
                "color": "#666",
                "marginTop": "10px",
                "lineHeight": "1.6"
            }
        )
    ], style={
        "padding": "40px",
        "textAlign": "left"
    })


def build_app_layout(default_study: Optional[str]) -> html.Div:
    return html.Div([
        dcc.Store(id="session-id-store", data=None, storage_type="session"),
        dcc.Store(id="session-heartbeat-store", data=None, storage_type="memory"),
        dcc.Store(id="selected-study-store", data=default_study),
        dcc.Store(id="dashboard-data-store", data=None),  # Store full dashboard data for filtering
        dcc.Store(id="heritage-selection-store", data=None),  # Store selected heritages
        dcc.Store(id="loading-message-store", data=""),  # Store current loading message
        dcc.Store(id="clustering-results-store", data=None),  # Store clustering results
        dcc.Store(id="clustering-concepts-store", data=None),  # Store which concepts were used for clustering
        dcc.Store(id="clustering-trigger-store", data=None),  # Trigger for initial clustering
        dcc.Store(id="plots-update-trigger-store", data=None),  # Trigger for plot updates after filter changes
        dcc.Store(id="data-mode-store", data=None),  # Track current data mode: "patient" or "summary"
        dcc.Store(id="cluster-view-store", data="all"),  # Store cluster view selection ("all" or "C1", "C2", etc.)
        dcc.Interval(id="session-heartbeat-interval", interval=5 * 60 * 1000, n_intervals=0),
        html.Div([
        html.H1("CohortContrast Viewer", style={
            "textAlign": "center",
                "marginBottom": "0px",
            "padding": "10px",
            "color": "#2c3e50",
                "fontWeight": "600",
                "display": "inline-block"
            }),
            # Data mode badge (hidden by default, shown when study loaded)
            html.Span(
                id="data-mode-badge",
                children="",
                style={
                    "display": "none",
                    "marginLeft": "15px",
                    "padding": "5px 12px",
                    "borderRadius": "15px",
                    "fontSize": "12px",
                    "fontWeight": "bold",
                    "verticalAlign": "middle"
                }
            )
        ], style={"textAlign": "center", "marginBottom": "20px"}),
        # Floating toggle button (always visible)
        html.Button(
            "☰",
            id="sidebar-toggle",
            n_clicks=0,
            style={
                "position": "fixed",
                "top": "20px",
                "left": "20px",
                "zIndex": "2000",
                "width": "40px",
                "height": "40px",
                "borderRadius": "50%",
                "border": "2px solid #333",
                "backgroundColor": "#fff",
                "fontSize": "20px",
                "cursor": "pointer",
                "boxShadow": "0 2px 5px rgba(0,0,0,0.2)",
                "transition": "all 0.3s ease"
            }
        ),
        
        html.Div([
            # Left sidebar - collapsible panel
            html.Div([
                html.Div([
                    html.H3("Studies", style={"margin": "0", "padding": "10px", "color": "#2c3e50", "fontWeight": "600"})
                ], style={"position": "relative", "borderBottom": "1px solid #ddd"}),
                
                html.Div([
                    dcc.Input(
                        id="study-search",
                        type="text",
                        placeholder="Search studies...",
                        style={
                            "width": "calc(100% - 20px)",
                            "padding": "8px",
                            "margin": "10px 0",
                            "border": "1px solid #ddd",
                            "borderRadius": "4px",
                            "boxSizing": "border-box"
                        }
                    ),
                    html.Div(id="study-table-container", style={"maxHeight": "500px", "overflowY": "auto"})
                ], id="sidebar-content", style={"padding": "10px"}),
                
                # Filters section
                html.Div([
                    html.Div([
                        html.H3("Filters", style={"margin": "0", "padding": "10px", "color": "#5a7d9a"})
                    ], style={"position": "relative", "borderBottom": "1px solid #ddd", "backgroundColor": "#e8f0f5"}),
                    
                    html.Div([
                        # Apply Filters button
                        html.Button(
                            "Apply Filters",
                            id="apply-filters-btn",
                            n_clicks=0,
                            style={
                                "width": "calc(100% - 20px)",
                                "padding": "10px",
                                "margin": "15px 10px 10px 10px",
                                "backgroundColor": "#6c9bd1",
                                "color": "white",
                                "border": "none",
                                "borderRadius": "6px",
                                "fontSize": "14px",
                                "fontWeight": "500",
                                "cursor": "pointer",
                                "boxShadow": "0 2px 4px rgba(0,0,0,0.1)",
                                "transition": "all 0.2s ease"
                            },
                            className="apply-filters-button"
                        ),
                        
                        # Apply Table Selection button
                        html.Button(
                            "Apply Table Selection",
                            id="apply-table-selection-btn",
                            n_clicks=0,
                            style={
                                "width": "calc(100% - 20px)",
                                "padding": "10px",
                                "margin": "5px 10px 15px 10px",
                                "backgroundColor": "#5a9f68",
                                "color": "white",
                                "border": "none",
                                "borderRadius": "6px",
                                "fontSize": "14px",
                                "fontWeight": "500",
                                "cursor": "pointer",
                                "boxShadow": "0 2px 4px rgba(0,0,0,0.1)",
                                "transition": "all 0.2s ease"
                            },
                            className="apply-table-selection-button"
                        ),
                        
                        # Heritage checkboxes
                        html.Div([
                            html.Label(
                                "Heritage Types",
                                style={
                                    "display": "block",
                                    "fontWeight": "500",
                                    "marginBottom": "10px",
                                    "color": "#5a7d9a",
                                    "fontSize": "13px"
                                }
                            ),
                            html.Div(id="heritage-checkboxes", style={"maxHeight": "200px", "overflowY": "auto"})
                        ], style={"marginBottom": "20px", "padding": "0 10px"}),
                        
                        # Target Prevalence range
                        html.Div([
                            html.Label(
                                "Target Prevalence (%)",
                                style={
                                    "display": "block",
                                    "fontWeight": "500",
                                    "marginBottom": "10px",
                                    "color": "#5a7d9a",
                                    "fontSize": "13px"
                                }
                            ),
                            dcc.RangeSlider(
                                id="target-prevalence-range",
                                min=0,
                                max=100,
                                step=1,
                                value=[10, 100],
                                marks={i: str(i) for i in range(0, 101, 20)},
                                tooltip={"placement": "bottom", "always_visible": False},
                                updatemode="drag"
                            ),
                            html.Div(
                                id="target-prevalence-display",
                                style={
                                    "textAlign": "center",
                                    "marginTop": "5px",
                                    "fontSize": "12px",
                                    "color": "#666"
                                }
                            )
                        ], style={"marginBottom": "20px", "padding": "0 10px"}),
                        
                        # Ratio range
                        html.Div([
                            html.Label(
                                "Prevalence Difference Ratio",
                                style={
                                    "display": "block",
                                    "fontWeight": "500",
                                    "marginBottom": "10px",
                                    "color": "#5a7d9a",
                                    "fontSize": "13px"
                                }
                            ),
                            dcc.RangeSlider(
                                id="ratio-range",
                                min=0,
                                max=100,
                                step=1,
                                value=[5, 100],
                                marks={i: str(i) for i in range(0, 101, 20)},
                                tooltip={"placement": "bottom", "always_visible": False},
                                updatemode="drag"
                            ),
                            html.Div(
                                id="ratio-display",
                                style={
                                    "textAlign": "center",
                                    "marginTop": "5px",
                                    "fontSize": "12px",
                                    "color": "#666"
                                }
                            )
                        ], style={"marginBottom": "20px", "padding": "0 10px"}),
                        
                        html.Div([
                            html.Label(
                                "Show ordinal data rows for active main concepts",
                                style={
                                    "display": "block",
                                    "fontWeight": "500",
                                    "marginBottom": "10px",
                                    "color": "#5a7d9a",
                                    "fontSize": "13px"
                                }
                            ),
                            html.Div([
                                dcc.Checklist(
                                    id="show-ordinal-checkbox",
                                    options=[
                                        {
                                            "label": "Enable showing ordinal rows for active main concepts",
                                            "value": "show_ordinals"
                                        }
                                    ],
                                    value=[],
                                    style={"display": "inline-block"},
                                    inputStyle={"marginRight": "8px", "cursor": "pointer"},
                                    labelStyle={"cursor": "pointer", "fontSize": "13px", "color": "#333"}
                                )
                            ], style={"marginBottom": "5px"}),
                            html.P(
                                "When enabled, ordinal concept rows (1st, 2nd, 3rd occurrence, etc.) will be shown for main concepts that pass the filters.",
                                style={
                                    "fontSize": "11px",
                                    "color": "#888",
                                    "marginTop": "5px",
                                    "fontStyle": "italic",
                                    "marginLeft": "0px"
                                }
                            )
                        ], style={"marginBottom": "20px", "padding": "0 10px"}),
                        
                        # Cluster Prevalence filter (only applies when a specific cluster is selected)
                        html.Div([
                            html.Label(
                                "Cluster Prevalence (%)",
                                style={
                                    "display": "block",
                                    "fontWeight": "500",
                                    "marginBottom": "10px",
                                    "color": "#5a7d9a",
                                    "fontSize": "13px"
                                }
                            ),
                            dcc.Slider(
                                id="cluster-prevalence-slider",
                                min=0,
                                max=100,
                                step=5,
                                value=0,
                                marks={i: f"{i}%" for i in range(0, 101, 20)},
                                tooltip={"placement": "bottom", "always_visible": False},
                                updatemode="drag"
                            ),
                            html.Div(
                                id="cluster-prevalence-display",
                                style={
                                    "textAlign": "center",
                                    "marginTop": "5px",
                                    "fontSize": "12px",
                                    "color": "#666"
                                }
                            ),
                            html.P(
                                "Applies when a specific cluster is selected (not 'All')",
                                style={
                                    "fontSize": "11px",
                                    "color": "#888",
                                    "marginTop": "5px",
                                    "fontStyle": "italic"
                                }
                            )
                        ], style={"marginBottom": "20px", "padding": "0 10px"}),
                        
                        # Top N concepts by SD filter
                        html.Div([
                            html.Label(
                                "Top N Concepts by SD (across clusters)",
                                style={
                                    "display": "block",
                                    "fontWeight": "500",
                                    "marginBottom": "10px",
                                    "color": "#5a7d9a",
                                    "fontSize": "13px"
                                }
                            ),
                            dcc.Input(
                                id="top-n-sd-filter",
                                type="number",
                                min=0,
                                max=1000,
                                step=1,
                                value=0,
                                style={
                                    "width": "100%",
                                    "padding": "8px",
                                    "border": "1px solid #ced4da",
                                    "borderRadius": "4px",
                                    "fontSize": "13px"
                                }
                            ),
                            html.P(
                                "0 = disabled. Shows top N main concepts with highest SD of prevalence across the selected divergence cluster scope.",
                                style={
                                    "fontSize": "11px",
                                    "color": "#888",
                                    "marginTop": "5px",
                                    "fontStyle": "italic"
                                }
                            )
                        ], style={"marginBottom": "20px", "padding": "0 10px"}),

                        # Divergence cluster scope for Top N SD filter
                        html.Div([
                            html.Label(
                                "Divergence Cluster Scope",
                                style={
                                    "display": "block",
                                    "fontWeight": "500",
                                    "marginBottom": "10px",
                                    "color": "#5a7d9a",
                                    "fontSize": "13px"
                                }
                            ),
                            dcc.Dropdown(
                                id="divergence-cluster-scope",
                                options=[],
                                value=[],
                                multi=True,
                                placeholder="All clusters (default). Select 2+ to target specific cluster divergence.",
                                style={
                                    "fontSize": "13px"
                                }
                            ),
                            html.Div(
                                id="divergence-cluster-display",
                                style={
                                    "textAlign": "left",
                                    "marginTop": "7px",
                                    "fontSize": "12px",
                                    "color": "#4f6f8b",
                                    "backgroundColor": "#eef5fb",
                                    "border": "1px solid #d7e5f2",
                                    "borderRadius": "4px",
                                    "padding": "6px 8px",
                                }
                            ),
                            html.P(
                                "Leave empty to use all clusters. Select at least two clusters (for example C1 and C4) for scoped divergence ranking.",
                                style={
                                    "fontSize": "11px",
                                    "color": "#888",
                                    "marginTop": "6px",
                                    "fontStyle": "italic"
                                }
                            )
                        ], style={"marginBottom": "20px", "padding": "0 10px"}),
                        
                        # Cluster selection
                        html.Div([
                            html.Label(
                                "Clusters",
                                style={
                                    "display": "block",
                                    "fontWeight": "500",
                                    "marginBottom": "10px",
                                    "color": "#5a7d9a",
                                    "fontSize": "13px"
                                }
                            ),
                            dcc.RadioItems(
                                id="cluster-count",
                                options=[
                                    {"label": "Auto", "value": "auto"},
                                    {"label": "2", "value": "2"},
                                    {"label": "3", "value": "3"},
                                    {"label": "4", "value": "4"},
                                    {"label": "5", "value": "5"}
                                ],
                                value="auto",
                                style={
                                    "display": "flex",
                                    "flexDirection": "row",
                                    "gap": "15px",
                                    "flexWrap": "wrap"
                                },
                                labelStyle={
                                    "marginRight": "10px",
                                    "fontSize": "12px",
                                    "color": "#333"
                                }
                            )
                        ], style={"marginBottom": "20px", "padding": "0 10px"}),
                        
                        # Clustering concept scope selection (disabled in summary mode)
                        html.Div([
                            html.Label("Clustering scope:", style={
                                "fontWeight": "600",
                                "marginBottom": "8px",
                                "display": "block",
                                "color": "#333",
                                "fontSize": "13px"
                            }),
                            dcc.RadioItems(
                                id="clustering-scope",
                                options=[
                                    {"label": "All concepts", "value": "all"},
                                    {"label": "Active concepts only", "value": "active"}
                                ],
                                value="all",
                                inline=True,
                                style={"fontSize": "12px"},
                                labelStyle={"marginRight": "15px", "cursor": "pointer"},
                                inputStyle={"marginRight": "5px"}
                            )
                        ], id="clustering-scope-container", style={"marginBottom": "15px", "padding": "0 10px"}),
                        
                        # Recluster button
                        html.Div([
                            html.Button(
                                "Recluster",
                                id="recluster-btn",
                                n_clicks=0,
                                style={
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
                            )
                        ], style={"marginBottom": "20px", "padding": "0 10px"})
                    ], id="filters-content", style={"padding": "10px", "backgroundColor": "#f8f9fa"})
                ], id="filters-section", style={"borderTop": "1px solid #ddd"})
            ], id="sidebar", style={
                "width": "450px",
                "height": "100vh",
                "position": "fixed",
                "left": "0",
                "top": "0",
                "backgroundColor": "#f8f9fa",
                "borderRight": "1px solid #ddd",
                "boxShadow": "2px 0 5px rgba(0,0,0,0.1)",
                "zIndex": "1000",
                "transition": "transform 0.3s ease",
                "overflowY": "auto",
                "overflowX": "hidden"
            }),
            
            # Main content area
            html.Div([
                dcc.Loading(
                    id="loading-main",
                    type="circle",
                    children=html.Div(
                        id="table-container",
                        children=[build_welcome_content()],
                        style={"margin": "20px"}
                    ),
                    fullscreen=True,
                    overlay_style={"backgroundColor": "rgba(255, 255, 255, 0.9)", "zIndex": "9999"}
                ),
                html.Div(id="loading-message-display", style={
                    "position": "fixed",
                    "top": "60%",
                    "left": "50%",
                    "transform": "translateX(-50%)",
                    "zIndex": "10001",
                    "color": "#2c3e50",
                    "fontSize": "18px",
                    "fontWeight": "500",
                    "textAlign": "center",
                    "pointerEvents": "none",
                    "display": "none"  # Hidden by default, shown when message exists
                }),
                html.Div([
                    dcc.RadioItems(
                        id="cluster-view-selector-summary",
                        options=[{"label": "All", "value": "all"}],
                        value="all",
                        inline=True,
                        style={"display": "none"}
                    ),
                    dcc.RadioItems(
                        id="cluster-view-selector-patient",
                        options=[{"label": "All", "value": "all"}],
                        value="all",
                        inline=True,
                        style={"display": "none"}
                    )
                ], style={"display": "none"})
            ], id="main-content", style={
                "marginLeft": "450px",
                "padding": "20px",
                "transition": "margin-left 0.3s ease"
            })
        ], style={"display": "flex"})
    ])
    
