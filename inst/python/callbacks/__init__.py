"""Callback registration entrypoints for ContrastViewer."""

from .clustering_callbacks import register_clustering_callbacks
from .composite_plot import register_composite_plot_callbacks
from .data_loading import register_data_loading_callbacks
from .dashboard_table import register_dashboard_table_callbacks
from .demographics_tabs import register_demographics_callbacks
from .layout_interactions import register_layout_interaction_callbacks
from .mappings import register_mappings_callbacks
from .plot_tabs import register_plot_tab_callbacks
from .session_state import register_session_callbacks
from .study import register_study_callbacks
from .view_state import register_view_state_callbacks

__all__ = [
    'register_clustering_callbacks',
    'register_composite_plot_callbacks',
    'register_data_loading_callbacks',
    'register_dashboard_table_callbacks',
    'register_demographics_callbacks',
    'register_layout_interaction_callbacks',
    'register_mappings_callbacks',
    'register_plot_tab_callbacks',
    'register_session_callbacks',
    'register_study_callbacks',
    'register_view_state_callbacks',
]
