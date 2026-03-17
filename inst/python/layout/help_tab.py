"""Help tab content for ContrastViewer."""

from dash import html

def build_help_tab_content() -> html.Div:
    """Build the Help tab content covering all interface functionality."""
    section_style = {
        "backgroundColor": "#ffffff",
        "border": "1px solid #e3e7eb",
        "borderRadius": "8px",
        "padding": "16px",
        "marginBottom": "14px"
    }

    table_header_style = {
        "textAlign": "left",
        "padding": "8px 10px",
        "borderBottom": "1px solid #d8dde3",
        "fontWeight": "600",
        "backgroundColor": "#f6f8fa"
    }
    table_cell_style = {
        "verticalAlign": "top",
        "padding": "8px 10px",
        "borderBottom": "1px solid #edf1f5"
    }

    return html.Div([
        html.H3("Help and Methods Guide", style={"color": "#2c3e50", "fontWeight": "700", "marginBottom": "8px"}),
        html.P(
            "This guide documents the full interface workflow for reproducible cohort contrast analysis. "
            "It is written for technical and research users who require explicit interpretation of controls, tabs, and outputs.",
            style={"color": "#4b5563", "fontSize": "14px", "marginBottom": "16px"}
        ),

        html.Div([
            html.H4("Recommended Workflow", style={"color": "#2c3e50", "fontWeight": "600", "marginBottom": "10px"}),
            html.Ol([
                html.Li("Select a study in the Studies panel and confirm the data mode badge (Patient Level Data or Summary Mode)."),
                html.Li("Set inclusion criteria in Filters, then select Apply Filters to refresh active concepts."),
                html.Li("Inspect the Dashboard composite plot and table to confirm concept relevance and statistical direction."),
                html.Li("Optionally adjust clustering controls and use Recluster when operating in patient mode."),
                html.Li("Use Trajectories to inspect ordering changes across groups and identify unstable concepts."),
                html.Li("Use Overlap to assess pairwise concept co-occurrence structure."),
                html.Li("Use Demographics to compare cohort and cluster composition by age and sex."),
                html.Li("Use Mappings to audit source-to-mapped concept correspondence and mapping types.")
            ], style={"marginBottom": "0", "paddingLeft": "20px", "lineHeight": "1.55"})
        ], style=section_style),

        html.Div([
            html.H4("Global Interface Elements", style={"color": "#2c3e50", "fontWeight": "600", "marginBottom": "10px"}),
            html.Table([
                html.Thead(html.Tr([
                    html.Th("Component", style=table_header_style),
                    html.Th("Function", style=table_header_style),
                    html.Th("Operational Note", style=table_header_style),
                ])),
                html.Tbody([
                    html.Tr([
                        html.Td("Studies + Search studies...", style=table_cell_style),
                        html.Td("Selects the study context and filters visible studies by name.", style=table_cell_style),
                        html.Td("All tabs and plots are scoped to the selected study.", style=table_cell_style),
                    ]),
                    html.Tr([
                        html.Td("Sidebar toggle", style=table_cell_style),
                        html.Td("Collapses or expands the left navigation and filter panel.", style=table_cell_style),
                        html.Td("Use to maximise horizontal space for wide plots and tables.", style=table_cell_style),
                    ]),
                    html.Tr([
                        html.Td("Data mode badge", style=table_cell_style),
                        html.Td("Displays whether the study is loaded in summary mode or patient mode.", style=table_cell_style),
                        html.Td("Some controls (for example reclustering scope) are constrained in summary mode.", style=table_cell_style),
                    ]),
                ]),
            ], style={"width": "100%", "borderCollapse": "collapse", "fontSize": "13px"})
        ], style=section_style),

        html.Div([
            html.H4("Filters Panel Reference", style={"color": "#2c3e50", "fontWeight": "600", "marginBottom": "10px"}),
            html.Table([
                html.Thead(html.Tr([
                    html.Th("Control", style=table_header_style),
                    html.Th("Purpose", style=table_header_style),
                    html.Th("Interpretation", style=table_header_style),
                ])),
                html.Tbody([
                    html.Tr([
                        html.Td("Apply Filters", style=table_cell_style),
                        html.Td("Applies all sidebar thresholds to concept activation.", style=table_cell_style),
                        html.Td("This is the primary commit action for filter changes.", style=table_cell_style),
                    ]),
                    html.Tr([
                        html.Td("Apply Table Selection", style=table_cell_style),
                        html.Td("Commits manual row visibility edits from the Dashboard table.", style=table_cell_style),
                        html.Td("Use after interactive inclusion or exclusion via the Show checkbox column.", style=table_cell_style),
                    ]),
                    html.Tr([
                        html.Td("Heritage Types", style=table_cell_style),
                        html.Td("Limits analysis to selected domain families.", style=table_cell_style),
                        html.Td("Unchecked heritage domains are excluded from downstream tabs.", style=table_cell_style),
                    ]),
                    html.Tr([
                        html.Td("Target Prevalence (%)", style=table_cell_style),
                        html.Td("Filters concepts by prevalence in the target cohort.", style=table_cell_style),
                        html.Td("Higher lower-bound values increase specificity.", style=table_cell_style),
                    ]),
                    html.Tr([
                        html.Td("Prevalence Difference Ratio", style=table_cell_style),
                        html.Td("Filters concepts by target-versus-control contrast magnitude.", style=table_cell_style),
                        html.Td("Higher thresholds prioritise stronger differential signals.", style=table_cell_style),
                    ]),
                    html.Tr([
                        html.Td("Show ordinal data rows...", style=table_cell_style),
                        html.Td("Shows or hides ordinal occurrences linked to active main concepts.", style=table_cell_style),
                        html.Td("Useful for progression analyses (1st, 2nd, 3rd occurrence, etc.).", style=table_cell_style),
                    ]),
                    html.Tr([
                        html.Td("Cluster Prevalence (%)", style=table_cell_style),
                        html.Td("Applies a minimum prevalence threshold within the selected cluster.", style=table_cell_style),
                        html.Td("Active only when a specific cluster view is selected, not All.", style=table_cell_style),
                    ]),
                    html.Tr([
                        html.Td("Top N Concepts by SD", style=table_cell_style),
                        html.Td("Retains concepts with highest prevalence variability across clusters.", style=table_cell_style),
                        html.Td("Set to 0 to disable this restriction.", style=table_cell_style),
                    ]),
                    html.Tr([
                        html.Td("Export Active Concepts (TSV)", style=table_cell_style),
                        html.Td("Downloads the currently active dashboard concepts with composite and per-cluster metrics.", style=table_cell_style),
                        html.Td("Exports only rows that are active (shown) at click time.", style=table_cell_style),
                    ]),
                    html.Tr([
                        html.Td("Divergence Cluster Scope", style=table_cell_style),
                        html.Td("Selects which clusters are used for SD-based divergence ranking.", style=table_cell_style),
                        html.Td("Leave empty for all clusters; pick 2+ clusters (for example C1 and C4) to scope divergence.", style=table_cell_style),
                    ]),
                    html.Tr([
                        html.Td("Clusters / Clustering scope / Recluster", style=table_cell_style),
                        html.Td("Controls cluster count and cohort partition strategy.", style=table_cell_style),
                        html.Td("Reclustering is intended for patient mode; summary mode uses pre-computed artefacts.", style=table_cell_style),
                    ]),
                ]),
            ], style={"width": "100%", "borderCollapse": "collapse", "fontSize": "13px"})
        ], style=section_style),

        html.Div([
            html.H4("Button Actions and Override Rules", style={"color": "#2c3e50", "fontWeight": "600", "marginBottom": "10px"}),
            html.P(
                "The following precedence rules define how button actions interact. This section is normative for current interface behaviour.",
                style={"color": "#4b5563", "fontSize": "13px", "marginBottom": "10px"}
            ),
            html.Table([
                html.Thead(html.Tr([
                    html.Th("Button", style=table_header_style),
                    html.Th("What it does", style=table_header_style),
                    html.Th("What it overrides", style=table_header_style),
                    html.Th("Important constraints", style=table_header_style),
                ])),
                html.Tbody([
                    html.Tr([
                        html.Td("Apply Filters", style=table_cell_style),
                        html.Td(
                            "Recomputes which concepts are shown based on sidebar thresholds and heritage selection, then reapplies ordinal visibility rules.",
                            style=table_cell_style
                        ),
                        html.Td(
                            "Overrides manual checkbox edits currently shown in the table because visibility is recalculated from filter criteria.",
                            style=table_cell_style
                        ),
                        html.Td(
                            "In summary mode, it also triggers loading of pre-computed clustering for the selected cluster count. "
                            "In patient mode, it does not perform live reclustering.",
                            style=table_cell_style
                        ),
                    ]),
                    html.Tr([
                        html.Td("Apply Table Selection", style=table_cell_style),
                        html.Td(
                            "Commits the current Dashboard table checkbox state as the active visibility state.",
                            style=table_cell_style
                        ),
                        html.Td(
                            "Overrides previous stored selection state for the current study.",
                            style=table_cell_style
                        ),
                        html.Td(
                            "If an ordinal row is shown, its main concept is forced to shown. "
                            "Showing a main concept does not auto-show ordinals.",
                            style=table_cell_style
                        ),
                    ]),
                    html.Tr([
                        html.Td("Recluster", style=table_cell_style),
                        html.Td(
                            "Runs clustering update using the current cluster count and clustering scope.",
                            style=table_cell_style
                        ),
                        html.Td(
                            "Overrides previously stored clustering results (cluster assignments/summary matrix) for the active study context.",
                            style=table_cell_style
                        ),
                        html.Td(
                            "In patient mode, reclustering runs only on this button click. "
                            "In summary mode, this button is disabled; clustering changes are applied via Apply Filters using pre-computed clustering artefacts.",
                            style=table_cell_style
                        ),
                    ]),
                ]),
            ], style={"width": "100%", "borderCollapse": "collapse", "fontSize": "13px"}),
            html.Div([
                html.P(
                    "Practical precedence:",
                    style={"fontWeight": "600", "marginTop": "12px", "marginBottom": "6px", "color": "#2c3e50"}
                ),
                html.Ul([
                    html.Li("Apply Filters after manual table edits will replace those edits with filter-derived visibility."),
                    html.Li("Apply Table Selection after filtering can be used to make additional manual refinements."),
                    html.Li("Cluster-count changes alone do not re-run clustering; use Recluster (patient mode) or Apply Filters (summary mode)."),
                    html.Li("When clustering scope is Active concepts only, reclustering uses only rows currently shown in the Dashboard table."),
                ], style={"paddingLeft": "20px", "lineHeight": "1.55", "marginBottom": "0"})
            ])
        ], style=section_style),

        html.Div([
            html.H4("Tab-by-Tab Functionality", style={"color": "#2c3e50", "fontWeight": "600", "marginBottom": "10px"}),
            html.Ul([
                html.Li([
                    html.Strong("Dashboard: "),
                    "Primary analytic workspace combining the composite plot with a feature table. "
                    "Use cluster view selectors, plot legends (enrichment, significance, median occurrences), and the Show column to curate active concepts."
                ]),
                html.Li([
                    html.Strong("Trajectories: "),
                    "Compares concept ordering shifts against overall ordering. "
                    "Row-order modes (Overall order, Top movers, Most stable) support stability-focused interpretation."
                ]),
                html.Li([
                    html.Strong("Overlap: "),
                    "Visualises pairwise concept co-occurrence structure and associated tabular summaries. "
                    "Use group selection to compare overall versus cluster-specific overlap behaviour."
                ]),
                html.Li([
                    html.Strong("Demographics: "),
                    "Provides age and sex distributions and, in summary mode, extended cluster and concept-level demographic diagnostics, including ordinal progression panels."
                ]),
                html.Li([
                    html.Strong("Mappings: "),
                    "Audits concept mapping output, including source concept, mapped concept, and mapping type."
                ]),
                html.Li([
                    html.Strong("Help: "),
                    "This reference tab."
                ]),
            ], style={"paddingLeft": "20px", "lineHeight": "1.6", "marginBottom": "0"})
        ], style=section_style),

    ], style={"padding": "20px", "maxWidth": "1200px"})

