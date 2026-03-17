"""
ContrastViewer - Disease Data Dashboard

A Dash application for visualizing and analyzing disease cohort data.

Refactored to use modular structure:
- config/: Constants and configuration
- models/: ConceptRegistry and data models  
- data/: Data loading and processing
- plots/: Visualization functions
- clustering/: Patient clustering
- layout/: UI components
- utils/: Helper functions
"""

# Fix for macOS: use spawn instead of fork for multiprocessing
# This prevents crashes when using background callbacks
import multiprocessing
import sys
if sys.platform == 'darwin':
    try:
        multiprocessing.set_start_method('spawn', force=True)
    except RuntimeError:
        pass  # Already set

from pathlib import Path
from typing import Dict
import logging

import os

# Runtime mode / feature flags from environment
RUN_MODE = os.environ.get("CONTRAST_VIEWER_MODE", "simple").strip().lower()
if RUN_MODE not in {"simple", "server", "debug"}:
    RUN_MODE = "simple"

# Backward compatibility: explicit debug flag promotes mode to debug
if os.environ.get("CONTRAST_VIEWER_DEBUG", "0") == "1":
    RUN_MODE = "debug"

ALLOW_EXPORTS = os.environ.get("CONTRAST_VIEWER_ALLOW_EXPORTS", "1").strip().lower() not in {
    "0", "false", "no", "off"
}

if RUN_MODE == "debug":
    log_level = logging.DEBUG
elif RUN_MODE == "server":
    log_level = logging.INFO
else:
    log_level = logging.WARNING

log_handlers = [logging.StreamHandler()]
log_file_path = os.environ.get("CONTRAST_VIEWER_LOG_FILE", "").strip()
if RUN_MODE == "server":
    if not log_file_path:
        data_dir_for_log = Path(os.environ.get("CONTRAST_VIEWER_DATA_DIR", Path.cwd()))
        log_file_path = str((data_dir_for_log / "contrast_viewer.log").resolve())
if log_file_path:
    try:
        Path(log_file_path).parent.mkdir(parents=True, exist_ok=True)
        log_handlers.append(logging.FileHandler(log_file_path, encoding="utf-8"))
    except Exception:
        pass

logging.basicConfig(
    level=log_level,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=log_handlers,
    force=True,
)
logger = logging.getLogger('ContrastViewer')
logging.getLogger("werkzeug").setLevel(logging.WARNING if RUN_MODE == "simple" else log_level)
logger.info("ContrastViewer starting in mode=%s (exports=%s)", RUN_MODE, ALLOW_EXPORTS)

import pandas as pd
from collections import OrderedDict
import dash
import dash_bootstrap_components as dbc
import diskcache
import plotly.graph_objects as go

# =============================================================================
# Import from modules
# =============================================================================

from data.loader import load_study_summaries
from data.cache import initialize_disk_cache
from data.occurrence_metrics import calculate_median_first_occurrence, calculate_ordinal_medians
from utils.helpers import (
    normalize_concept_id as _normalize_concept_id,
)
from callbacks.clustering_callbacks import register_clustering_callbacks
from callbacks.composite_plot import register_composite_plot_callbacks
from callbacks.data_loading import register_data_loading_callbacks
from callbacks.dashboard_table import register_dashboard_table_callbacks
from callbacks.demographics_tabs import register_demographics_callbacks
from callbacks.layout_interactions import register_layout_interaction_callbacks
from callbacks.mappings import register_mappings_callbacks
from callbacks.plot_tabs import register_plot_tab_callbacks
from callbacks.session_state import register_session_callbacks
from callbacks.study import register_study_callbacks
from callbacks.view_state import register_view_state_callbacks


# =============================================================================
# Application Setup
# =============================================================================

# Data directory - can be overridden via environment variable
DATA_DIR = Path(os.environ.get("CONTRAST_VIEWER_DATA_DIR", 
                               Path.cwd()))

# Initialize disk cache for background callbacks
cache = diskcache.Cache("./cache")
# Initialize data cache system
initialize_disk_cache("./cache")

# Initialize the Dash app with Bootstrap theme
app = dash.Dash(
    __name__, 
    suppress_callback_exceptions=True, 
    external_stylesheets=[dbc.themes.BOOTSTRAP],
    background_callback_manager=dash.DiskcacheManager(cache)
)

# Add custom CSS for button hover effects
app.index_string = '''
<!DOCTYPE html>
<html>
    <head>
        {%metas%}
        <title>ContrastViewer - Disease Data Dashboard</title>
        {%favicon%}
        {%css%}
        <style>
            .apply-filters-button:hover {
                background-color: #5a8bc0 !important;
                box-shadow: 0 4px 6px rgba(0,0,0,0.15) !important;
                transform: translateY(-1px);
            }
            .apply-filters-button:active {
                transform: translateY(0);
                box-shadow: 0 2px 4px rgba(0,0,0,0.1) !important;
            }
            .download-dashboard-tsv-button:hover {
                background-color: #195f77 !important;
                box-shadow: 0 3px 6px rgba(0,0,0,0.18) !important;
            }
            .download-composite-png-button:hover {
                background-color: #23664f !important;
                box-shadow: 0 3px 6px rgba(0,0,0,0.18) !important;
            }
            .study-card:hover {
                background-color: #f8f9fa !important;
                border-color: #6B8FB9 !important;
            }
        </style>
    </head>
    <body>
        {%app_entry%}
        <footer>
            {%config%}
            {%scripts%}
            {%renderer%}
        </footer>
    </body>
</html>
'''

# =============================================================================
# Global State
# =============================================================================

# Load study summaries on startup
study_summaries_df = load_study_summaries(DATA_DIR)
if RUN_MODE != "simple":
    print(f"Loaded {len(study_summaries_df)} studies from {DATA_DIR}")

# Store loaded parquet data (lazy loading)
loaded_parquet_data: Dict[str, Dict[str, pd.DataFrame]] = {}

# Store for tracking which concepts are shown/hidden in dashboard
dashboard_show_state: Dict[str, Dict[int, bool]] = {}

# Plot figure cache for summary mode (LRU cache with max size)
plot_figure_cache: OrderedDict[str, go.Figure] = OrderedDict()
MAX_PLOT_CACHE_SIZE = 50  # Increased from 20 to 50 for better caching

# Default study (None - no study selected initially)
default_study = None

from data.dashboard_filters import (
    apply_filters_to_data as _apply_filters_to_data,
    filter_ordinal_concepts_for_filtered_mains as _filter_ordinal_concepts_for_filtered_mains,
    hide_ordinals_for_active_mains as _hide_ordinals_for_active_mains,
    show_ordinals_for_active_mains as _show_ordinals_for_active_mains,
    configure_dashboard_show_state,
)
from layout.app_shell import build_app_layout
from layout.help_tab import build_help_tab_content

configure_dashboard_show_state(dashboard_show_state)

# =============================================================================
# App Layout
# =============================================================================

app.layout = build_app_layout(default_study)

# =============================================================================
# Callbacks
# =============================================================================

register_study_callbacks(app, study_summaries_df, default_study)


register_data_loading_callbacks(
    app=app,
    data_dir=DATA_DIR,
    cache_store=cache,
    loaded_parquet_data_store=loaded_parquet_data,
    dashboard_show_state_store=dashboard_show_state,
    build_help_tab_content_fn=build_help_tab_content,
    logger_obj=logger,
    calculate_median_first_occurrence_fn=calculate_median_first_occurrence,
    calculate_ordinal_medians_fn=calculate_ordinal_medians,
    apply_filters_to_data_fn=_apply_filters_to_data,
    filter_ordinal_concepts_for_filtered_mains_fn=_filter_ordinal_concepts_for_filtered_mains,
    show_ordinals_for_active_mains_fn=_show_ordinals_for_active_mains,
    hide_ordinals_for_active_mains_fn=_hide_ordinals_for_active_mains,
)

register_layout_interaction_callbacks(app)

register_dashboard_table_callbacks(
    app=app,
    dashboard_show_state_store=dashboard_show_state,
    normalize_concept_id_fn=_normalize_concept_id,
    apply_filters_to_data_fn=_apply_filters_to_data,
    filter_ordinal_concepts_for_filtered_mains_fn=_filter_ordinal_concepts_for_filtered_mains,
    show_ordinals_for_active_mains_fn=_show_ordinals_for_active_mains,
    hide_ordinals_for_active_mains_fn=_hide_ordinals_for_active_mains,
)


register_clustering_callbacks(
    app=app,
    logger_obj=logger,
    data_dir=DATA_DIR,
    cache_store=cache,
    loaded_parquet_data_store=loaded_parquet_data,
)

register_composite_plot_callbacks(
    app=app,
    data_dir=DATA_DIR,
    cache_store=cache,
    loaded_parquet_data_store=loaded_parquet_data,
    plot_figure_cache_store=plot_figure_cache,
    max_plot_cache_size=MAX_PLOT_CACHE_SIZE,
    logger_obj=logger,
)

register_view_state_callbacks(app)
register_session_callbacks(
    app,
    dashboard_show_state_store=dashboard_show_state,
    logger_obj=logger,
    background=True,
)


register_plot_tab_callbacks(
    app=app,
    data_dir=DATA_DIR,
    cache_store=cache,
    loaded_parquet_data_store=loaded_parquet_data,
    normalize_concept_id_fn=_normalize_concept_id,
)
register_demographics_callbacks(
    app=app,
    data_dir=DATA_DIR,
    cache_store=cache,
    loaded_parquet_data_store=loaded_parquet_data,
)
register_mappings_callbacks(
    app=app,
    logger_obj=logger,
    data_dir=DATA_DIR,
    cache_store=cache,
    loaded_parquet_data_store=loaded_parquet_data,
)


if __name__ == "__main__":
    if RUN_MODE != "simple":
        if study_summaries_df.empty:
            print("Warning: No studies found. Please check the results_parquet/ folder.")
        else:
            print(f"Found {len(study_summaries_df)} study(ies): {', '.join(study_summaries_df['study'].tolist())}")
    
    # Configuration from environment variables (for R package integration)
    port = int(os.environ.get("CONTRAST_VIEWER_PORT", 8050))
    host = os.environ.get("CONTRAST_VIEWER_HOST", "127.0.0.1")
    debug = RUN_MODE == "debug"
    
    if RUN_MODE != "simple":
        print(f"Starting ContrastViewer on http://{host}:{port} (mode={RUN_MODE})")
        if log_file_path:
            print(f"Logging to: {log_file_path}")
    app.run(
        host=host,
        port=port,
        debug=debug,
        dev_tools_silence_routes_logging=(RUN_MODE != "debug"),
    )
