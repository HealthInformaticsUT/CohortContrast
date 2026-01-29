"""
Callback registration utilities for ContrastViewer.

This module provides utilities for managing callbacks.
The main callbacks are registered in app.py due to their dependencies
on global state and stores.
"""

from typing import Dict, List, Tuple
from dash import clientside_callback, ClientsideFunction


def setup_clientside_callbacks(app):
    """
    Register clientside (JavaScript) callbacks for performance-critical operations.
    
    Clientside callbacks run in the browser without server round-trips,
    making them ideal for UI interactions like:
    - Sidebar toggle animations
    - Theme switching
    - Scroll synchronization
    
    Args:
        app: Dash application instance
    """
    # Sidebar toggle callback - runs entirely in browser
    clientside_callback(
        """
        function(n_clicks, current_style) {
            if (!n_clicks) return window.dash_clientside.no_update;
            
            const sidebar = document.getElementById('sidebar');
            const mainContent = document.getElementById('main-content');
            
            if (sidebar.style.width === '300px' || !sidebar.style.width) {
                // Collapse
                return [
                    {...current_style, width: '50px', overflow: 'hidden'},
                    {display: 'none'},
                    '▶'
                ];
            } else {
                // Expand
                return [
                    {...current_style, width: '300px', overflow: 'auto'},
                    {display: 'block'},
                    '◀'
                ];
            }
        }
        """,
        [
            app.callback_output('sidebar', 'style'),
            app.callback_output('sidebar-content', 'style'),
            app.callback_output('toggle-sidebar', 'children'),
        ],
        app.callback_input('toggle-sidebar', 'n_clicks'),
        app.callback_state('sidebar', 'style'),
        prevent_initial_call=True
    )


def get_callback_dependencies() -> Dict[str, List[str]]:
    """
    Return a mapping of callback IDs to their dependencies.
    
    Useful for debugging callback chains and understanding data flow.
    
    Returns:
        Dictionary mapping callback names to list of dependent store/component IDs
    """
    return {
        'load_study_data': [
            'selected-study-store',
            'tabs-container',
            'dashboard-data-store',
        ],
        'update_dashboard_table': [
            'dashboard-data-store',
            'dashboard-table-container',
        ],
        'update_composite_plot': [
            'dashboard-data-store',
            'clustering-results-store',
            'composite-plot-container',
        ],
        'perform_clustering': [
            'dashboard-data-store',
            'clustering-results-store',
        ],
        'update_demographics_plot': [
            'clustering-results-store',
            'demographics-plot-container',
        ],
        'update_trajectory_plot': [
            'clustering-results-store',
            'trajectory-plot-container',
        ],
        'update_overlap_plot': [
            'clustering-results-store',
            'overlap-plot-container',
        ],
    }


def get_store_descriptions() -> Dict[str, str]:
    """
    Return descriptions of the data stores used in the application.
    
    Useful for documentation and debugging.
    
    Returns:
        Dictionary mapping store IDs to their descriptions
    """
    return {
        'selected-study-store': 'Currently selected study name',
        'dashboard-data-store': 'Processed concept data for the dashboard table',
        'clustering-results-store': 'Results from patient clustering (summary_matrix, assignments)',
        'data-mode-store': 'Current data mode ("patient" or "summary")',
        'filter-state-store': 'Current filter settings (heritage, prevalence, ratio)',
    }
