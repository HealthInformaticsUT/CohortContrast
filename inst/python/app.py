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
from typing import Dict, Optional, List, Tuple
import warnings
import logging

# Configure logging (cross-platform)
import os
import tempfile

log_file = os.environ.get(
    'CONTRAST_VIEWER_LOG_FILE',
    os.path.join(tempfile.gettempdir(), 'contrast_viewer.log')
)
log_level = logging.DEBUG if os.environ.get('CONTRAST_VIEWER_DEBUG') == '1' else logging.INFO

logging.basicConfig(
    level=log_level,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
    handlers=[
        logging.StreamHandler(),
        logging.FileHandler(log_file)
    ]
)
logger = logging.getLogger('ContrastViewer')
logger.info(f"ContrastViewer starting... (log: {log_file})")

import pandas as pd
import numpy as np
import dash
from dash import dcc, html, dash_table, Input, Output, State, callback_context, clientside_callback, no_update
import dash_bootstrap_components as dbc
from dash_ag_grid import AgGrid
import diskcache
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from scipy import stats

# =============================================================================
# Import from modules
# =============================================================================

from config.constants import (
    HERITAGE_ORDER,
    TOP_MARGIN_COMBINED,
    BOTTOM_MARGIN_COMBINED,
    SMALL_GAP_COMBINED,
    PIXELS_PER_CONCEPT_COMBINED,
    MIN_PLOT_AREA_COMBINED,
    VERTICAL_SPACING,
    HORIZONTAL_SPACING,
)
from models.registry import concept_registry
from data.loader import load_parquet_files, load_study_summaries, get_available_cluster_k_values
from data.processing import calculate_concept_metrics_from_patients, create_ordinal_concepts
from utils.helpers import (
    format_column_name,
    format_heritage_label,
    normalize_concept_id as _normalize_concept_id,
    convert_list_columns_to_strings,
    is_ordinal_concept as _is_ordinal_concept,
    get_unique_occurrences,
    get_min_time,
    normalize_concept_name,
    create_empty_figure,
    create_empty_figure_with_style,
    get_default_container_style,
    build_cluster_prevalence_lookup,
    filter_concepts_by_cluster_prevalence,
    extend_range_with_stats,
)
from plots.composite import create_composite_plot
from plots.prevalence import create_prevalence_plot
from plots.age import calculate_age_stats, create_age_plot
from plots.male_proportion import calculate_male_prop_stats, create_male_prop_plot
from plots.demographics import (
    create_demographics_plots_from_patient_data,
    create_demographics_plots_from_summary,
    create_empty_demographics_plot
)
from plots.clustering_plots import (
    create_trajectory_plot, 
    create_overlap_plot,
    create_trajectory_plot_from_summary,
    create_overlap_plot_from_summary
)
from clustering.clustering import perform_patient_clustering


# =============================================================================
# Data processing functions (kept here due to complex dependencies)
# =============================================================================

def calculate_median_first_occurrence(data_patients: pd.DataFrame) -> pd.DataFrame:
    """
    Calculate the median first occurrence time for each concept in the target cohort.
    
    For each concept and person in the target cohort:
    1. Find the minimum TIME_TO_EVENT (first occurrence)
    2. Calculate the median of these minimum times across all persons
    
    Groups by both CONCEPT_ID and HERITAGE since the pair is unique.
    
    Args:
        data_patients: DataFrame with columns COHORT_DEFINITION_ID, PERSON_ID, CONCEPT_ID, HERITAGE, TIME_TO_EVENT
        
    Returns:
        DataFrame with columns CONCEPT_ID, HERITAGE, and MEDIAN_FIRST_OCCURRENCE (in days)
    """
    # Filter for target cohort only
    df_target = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
    
    if df_target.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Ensure HERITAGE column exists
    if "HERITAGE" not in df_target.columns:
        warnings.warn("HERITAGE column not found in data_patients. Using CONCEPT_ID only for grouping.")
        # Fallback to CONCEPT_ID only if HERITAGE is missing
        has_heritage = False
    else:
        has_heritage = True
    
    # Calculate first occurrence (minimum time) for each person-concept combination
    df_target["FIRST_OCCURRENCE"] = df_target["TIME_TO_EVENT"].apply(get_min_time)
    
    # Remove rows where first occurrence couldn't be calculated
    df_target = df_target[df_target["FIRST_OCCURRENCE"].notna()].copy()
    
    if df_target.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Group by both CONCEPT_ID and HERITAGE (the pair is unique)
    if has_heritage:
        median_df = df_target.groupby(["CONCEPT_ID", "HERITAGE"])["FIRST_OCCURRENCE"].median().reset_index()
        median_df.columns = ["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"]
    else:
        # Fallback: group by CONCEPT_ID only if HERITAGE is missing
        median_df = df_target.groupby("CONCEPT_ID")["FIRST_OCCURRENCE"].median().reset_index()
        median_df.columns = ["CONCEPT_ID", "MEDIAN_FIRST_OCCURRENCE"]
        median_df["HERITAGE"] = None
    
    return median_df


def calculate_ordinal_medians(data_patients: pd.DataFrame, ordinal_concepts_df: pd.DataFrame) -> pd.DataFrame:
    """
    Calculate median occurrence time for ordinal concepts (1st, 2nd, 3rd, etc.).
    
    Optimized version using vectorized operations instead of iterrows.
    
    Args:
        data_patients: DataFrame with COHORT_DEFINITION_ID, PERSON_ID, CONCEPT_ID, HERITAGE, TIME_TO_EVENT
        ordinal_concepts_df: DataFrame with ordinal concepts (ORIGINAL_CONCEPT_ID, ORDINAL, CONCEPT_ID, HERITAGE)
        
    Returns:
        DataFrame with CONCEPT_ID, HERITAGE, and MEDIAN_FIRST_OCCURRENCE for ordinal concepts
    """
    # Filter for target cohort only
    df_target = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
    
    if df_target.empty or ordinal_concepts_df.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Pre-compute unique occurrences once for all target patients
    df_target["OCCURRENCES"] = df_target["TIME_TO_EVENT"].apply(get_unique_occurrences)
    df_target["NUM_OCCURRENCES"] = df_target["OCCURRENCES"].apply(len)
    
    # Filter to only rows with at least 1 occurrence
    df_target = df_target[df_target["NUM_OCCURRENCES"] > 0].copy()
    
    if df_target.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Prepare ordinal concepts for merge
    ordinal_df = ordinal_concepts_df[["CONCEPT_ID", "ORIGINAL_CONCEPT_ID", "ORDINAL", "HERITAGE"]].copy()
    ordinal_df = ordinal_df.dropna(subset=["ORIGINAL_CONCEPT_ID", "ORDINAL"])
    
    if ordinal_df.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Merge ordinal concepts with patient data on ORIGINAL_CONCEPT_ID and HERITAGE
    has_heritage = "HERITAGE" in df_target.columns and "HERITAGE" in ordinal_df.columns
    
    if has_heritage:
        merged = df_target.merge(
            ordinal_df,
            left_on=["CONCEPT_ID", "HERITAGE"],
            right_on=["ORIGINAL_CONCEPT_ID", "HERITAGE"],
            suffixes=("_patient", "_ordinal")
        )
        # Use the ordinal CONCEPT_ID for the result
        merged["RESULT_CONCEPT_ID"] = merged["CONCEPT_ID_ordinal"]
        merged["RESULT_HERITAGE"] = merged["HERITAGE"]
    else:
        merged = df_target.merge(
            ordinal_df,
            left_on="CONCEPT_ID",
            right_on="ORIGINAL_CONCEPT_ID",
            suffixes=("_patient", "_ordinal")
        )
        merged["RESULT_CONCEPT_ID"] = merged["CONCEPT_ID_ordinal"]
        merged["RESULT_HERITAGE"] = merged.get("HERITAGE_ordinal", merged.get("HERITAGE"))
    
    if merged.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Filter to patients with enough occurrences for their ordinal
    merged = merged[merged["NUM_OCCURRENCES"] >= merged["ORDINAL"]].copy()
    
    if merged.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Get the specific ordinal occurrence for each row
    def get_nth_occurrence(row):
        occs = row["OCCURRENCES"]
        ordinal = int(row["ORDINAL"])
        idx = ordinal - 1  # Convert to 0-indexed
        return occs[idx] if len(occs) > idx else None
    
    merged["ORDINAL_OCCURRENCE"] = merged.apply(get_nth_occurrence, axis=1)
    merged = merged[merged["ORDINAL_OCCURRENCE"].notna()].copy()
    
    if merged.empty:
        return pd.DataFrame(columns=["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"])
    
    # Group by ordinal concept and calculate median
    # First, get one value per person per ordinal concept
    person_first = merged.groupby(
        ["RESULT_CONCEPT_ID", "RESULT_HERITAGE", "PERSON_ID"]
    )["ORDINAL_OCCURRENCE"].first().reset_index()
    
    # Then calculate median per ordinal concept
    medians = person_first.groupby(
        ["RESULT_CONCEPT_ID", "RESULT_HERITAGE"]
    )["ORDINAL_OCCURRENCE"].median().reset_index()
    
    medians.columns = ["CONCEPT_ID", "HERITAGE", "MEDIAN_FIRST_OCCURRENCE"]
    medians = medians[medians["MEDIAN_FIRST_OCCURRENCE"].notna()]
    
    return medians


# =============================================================================
# Application Setup
# =============================================================================

# Data directory - can be overridden via environment variable
import os
DATA_DIR = Path(os.environ.get("CONTRAST_VIEWER_DATA_DIR", 
                               Path(__file__).parent / "results_parquet"))

# Initialize disk cache for background callbacks
cache = diskcache.Cache("./cache")

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
print(f"Loaded {len(study_summaries_df)} studies from {DATA_DIR}")

# Store loaded parquet data (lazy loading)
loaded_parquet_data: Dict[str, Dict[str, pd.DataFrame]] = {}

# Store for tracking which concepts are shown/hidden in dashboard
dashboard_show_state: Dict[str, Dict[int, bool]] = {}

# Default study (None - no study selected initially)
default_study = None


# =============================================================================
# Filter helper functions
# =============================================================================

def _apply_filters_to_data(
    updated_data: List[Dict],
    target_min: float,
    target_max: float,
    ratio_min: float,
    ratio_max: float,
    selected_heritages_set: set,
    selected_study: str
) -> None:
    """Apply filter criteria to dashboard data and update _show column."""
    for row in updated_data:
        target_prevalence = row.get("TARGET_SUBJECT_PREVALENCE_PCT", 0)
        if target_prevalence is None or pd.isna(target_prevalence):
            target_prevalence = 0
        
        ratio = row.get("PREVALENCE_DIFFERENCE_RATIO_DISPLAY", 0)
        if ratio is None or pd.isna(ratio):
            ratio = 0
        
        heritage = row.get("HERITAGE")
        heritage_str = str(heritage) if heritage is not None and pd.notna(heritage) else ""
        heritage_selected = heritage_str in selected_heritages_set if heritage_str else False
        
        matches_target = target_min <= target_prevalence <= target_max
        matches_ratio = ratio_min <= ratio <= ratio_max
        matches_heritage = heritage_selected
        
        row["_show"] = matches_target and matches_ratio and matches_heritage
        
        # Update stored state
        concept_id = row.get("_concept_id")
        if concept_id is not None:
            if selected_study not in dashboard_show_state:
                dashboard_show_state[selected_study] = {}
            dashboard_show_state[selected_study][concept_id] = row["_show"]


def _filter_ordinal_concepts_for_filtered_mains(
    updated_data: List[Dict],
    selected_study: str
) -> None:
    """Filter out ordinal concepts when their main concept is filtered out."""
    filtered_out_main_concepts = {}
    
    # Find all filtered-out main concepts
    for row in updated_data:
        if not _is_ordinal_concept(row) and not row.get("_show", True):
            main_concept_id = row.get("CONCEPT_ID")
            main_heritage = row.get("HERITAGE")
            
            if main_concept_id is not None:
                main_concept_id_normalized = _normalize_concept_id(main_concept_id)
                heritage_key = str(main_heritage) if main_heritage is not None and pd.notna(main_heritage) else None
                key = (main_concept_id_normalized, heritage_key)
                filtered_out_main_concepts[key] = True
    
    # Filter out ordinal concepts linked to filtered-out main concepts
    for ordinal_row in updated_data:
        if _is_ordinal_concept(ordinal_row):
            original_concept_id = ordinal_row.get("ORIGINAL_CONCEPT_ID")
            ordinal_heritage = ordinal_row.get("HERITAGE")
            
            if original_concept_id is not None:
                original_concept_id_normalized = _normalize_concept_id(original_concept_id)
                heritage_key = str(ordinal_heritage) if ordinal_heritage is not None and pd.notna(ordinal_heritage) else None
                key = (original_concept_id_normalized, heritage_key)
                
                if key in filtered_out_main_concepts:
                    ordinal_row["_show"] = False
                    ordinal_concept_id_for_state = ordinal_row.get("_concept_id")
                    if ordinal_concept_id_for_state is not None:
                        if selected_study not in dashboard_show_state:
                            dashboard_show_state[selected_study] = {}
                        dashboard_show_state[selected_study][ordinal_concept_id_for_state] = False



# =============================================================================
# App Layout
# =============================================================================

app.layout = html.Div([
    dcc.Store(id="selected-study-store", data=default_study),
    dcc.Store(id="dashboard-data-store", data=None),  # Store full dashboard data for filtering
    dcc.Store(id="heritage-selection-store", data=[]),  # Store selected heritages
    dcc.Store(id="loading-message-store", data=""),  # Store current loading message
    dcc.Store(id="clustering-results-store", data=None),  # Store clustering results
    dcc.Store(id="clustering-concepts-store", data=None),  # Store which concepts were used for clustering
    dcc.Store(id="clustering-trigger-store", data=None),  # Trigger for initial clustering
    dcc.Store(id="plots-update-trigger-store", data=None),  # Trigger for plot updates after filter changes
    dcc.Store(id="data-mode-store", data="patient"),  # Track current data mode: "patient" or "summary"
    html.Div([
    html.H1("Contrast Viewer", style={
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
                    placeholder="🔍 Search studies...",
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
                children=html.Div(id="table-container", style={"margin": "20px"}),
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
            })
        ], id="main-content", style={
            "marginLeft": "450px",
            "padding": "20px",
            "transition": "margin-left 0.3s ease"
        })
    ], style={"display": "flex"})
])



# =============================================================================
# Callbacks
# =============================================================================

@app.callback(
    Output("study-table-container", "children"),
    Input("study-search", "value")
)
def update_study_table(search_value: Optional[str]) -> html.Div:
    """
    Update the study table based on search query.
    
    Args:
        search_value: Search query string
        
    Returns:
        HTML Div containing the study table
    """
    if study_summaries_df.empty:
        return html.P("📭 No studies found.", style={"color": "#666", "padding": "10px"})
    
    # Filter studies based on search
    filtered_df = study_summaries_df.copy()
    if search_value:
        search_lower = search_value.lower()
        filtered_df = filtered_df[
            filtered_df["study"].str.lower().str.contains(search_lower, na=False)
        ]
    
    if filtered_df.empty:
        return html.P("🔍 No studies match your search.", style={"color": "#666", "padding": "10px"})
    
    # Prepare columns for the table
    # Show: Study, Target Patients, Z Count (Significant Concepts)
    columns = [
        {"name": "Study", "id": "study"},
        {"name": "Target Patients", "id": "target_patients", "type": "numeric"},
        {"name": "Significant Concepts", "id": "z_count", "type": "numeric"}
    ]
    
    # Prepare data - only include the columns we want to display
    # Ensure columns exist
    required_cols = ["study", "target_patients", "z_count"]
    available_cols = [col for col in required_cols if col in filtered_df.columns]
    display_df = filtered_df[available_cols].copy()
    
    # Keep original numeric values for row selection
    data = display_df.to_dict("records")
    
    return dash_table.DataTable(
        id="study-table",
        columns=columns,
        data=data,
        row_selectable="single",
        selected_rows=[],
        style_table={
            "overflowX": "auto",
            "fontSize": "12px"
        },
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
        style_data={
            "whiteSpace": "normal",
            "height": "auto"
        },
        style_data_conditional=[
            {
                "if": {"row_index": "odd"},
                "backgroundColor": "rgb(248, 248, 248)"
            },
            {
                "if": {"state": "selected"},
                "backgroundColor": "rgb(220, 220, 255)"
            }
        ]
    )


@app.callback(
    Output("selected-study-store", "data"),
    Input("study-table", "selected_rows"),
    State("study-table", "data")
)
def update_selected_study(selected_rows: List[int], table_data: List[Dict]) -> Optional[str]:
    """
    Update the selected study store when a study row is clicked.
    
    Args:
        selected_rows: List of selected row indices
        table_data: Data from the study table
        
    Returns:
        Selected study name
    """
    if not selected_rows or not table_data:
        return default_study
    
    # Get the study name from the selected row
    selected_row_idx = selected_rows[0]
    if selected_row_idx < len(table_data):
        selected_study = table_data[selected_row_idx]["study"]
        return selected_study
    
    return default_study


@app.callback(
    [Output("data-mode-badge", "children"),
     Output("data-mode-badge", "style")],
    Input("data-mode-store", "data"),
    prevent_initial_call=False
)
def update_data_mode_badge(data_mode: str) -> Tuple[str, Dict]:
    """
    Update the data mode badge to show whether viewing patient-level or summary data.
    """
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
    elif data_mode == "patient":
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
    else:
        return ("", {"display": "none"})


@app.callback(
    Output("clustering-scope-container", "style"),
    Input("data-mode-store", "data"),
    prevent_initial_call=False
)
def update_clustering_scope_visibility(data_mode: str) -> Dict:
    """
    Gray out the clustering scope options when in summary mode.
    In summary mode, we can only use pre-computed clustering, not re-cluster with different scopes.
    """
    base_style = {"marginBottom": "15px", "padding": "0 10px"}
    
    if data_mode == "summary":
        # Gray out and disable in summary mode
        return {
            **base_style,
            "opacity": "0.5",
            "pointerEvents": "none",
            "position": "relative"
        }
    else:
        # Normal style for patient mode
        return base_style


@app.callback(
    [Output("table-container", "children"),
     Output("dashboard-data-store", "data"),
     Output("clustering-trigger-store", "data"),
     Output("data-mode-store", "data")],
    Input("selected-study-store", "data"),
    background=True,
    running=[
        (Output("loading-message-store", "data"), "Loading study data from parquet files...", "")
    ],
    prevent_initial_call=False
)
def load_study_data(selected_study: Optional[str]) -> Tuple[html.Div, Optional[List[Dict]], Optional[float], str]:
    """
    Load parquet files for the selected study and display them in tables.
    Only loads parquet files when a study is selected (clicked).
    
    Args:
        selected_study: Name of the selected study
        
    Returns:
        HTML Div containing tables for all parquet files
    """
    # Import inside function for background callback compatibility
    import dash_ag_grid as dag
    logger.info(f"load_study_data called with: {selected_study}")
    
    if selected_study is None:
        logger.info("No study selected, returning welcome message")
        return (html.Div([
            html.H3("👋 Welcome to Contrast Viewer", style={"marginBottom": "20px", "color": "#2c3e50"}),
            html.P("Select a study from the left sidebar to begin exploring the data.", 
                   style={"fontSize": "16px", "color": "#666", "marginTop": "10px"})
        ]), None, None, "patient")
    
    # Load parquet files if not already loaded (only when study is clicked)
    if selected_study not in loaded_parquet_data:
        study_folder = DATA_DIR / selected_study
        logger.info(f"Loading parquet files from: {study_folder}")
        if study_folder.exists():
            loaded_parquet_data[selected_study] = load_parquet_files(study_folder)
            logger.info(f"Loaded keys: {list(loaded_parquet_data[selected_study].keys())}")
        else:
            logger.error(f"Study folder not found: {study_folder}")
            return (html.Div(f"Study folder {selected_study} not found."), None, None, "patient")
    
    parquet_data = loaded_parquet_data[selected_study]
    logger.info(f"Data mode: {parquet_data.get('_mode', 'unknown')}")
    
    if not parquet_data:
        return (html.Div(f"No parquet files found for {selected_study}."), None, None, "patient")
    
    # Detect data mode from loaded data
    data_mode = parquet_data.get("_mode", "patient")
    logger.info(f"Processing study in {data_mode} mode")
    
    # Load study description if available
    study_folder = DATA_DIR / selected_study
    desc_file = study_folder / "desc.txt"
    study_description = None
    if desc_file.exists():
        try:
            study_description = desc_file.read_text().strip()
        except Exception:
            pass
    
    # Create tabs for different views
    tabs_content = []
    
    # SUMMARY MODE: Use pre-computed concept_summaries directly
    if data_mode == "summary" and "concept_summaries" in parquet_data:
        logger.info("Using SUMMARY mode data processing")
        
        df_features = parquet_data["concept_summaries"].copy()
        
        # Ensure required columns exist
        if "TARGET_SUBJECT_PREVALENCE" not in df_features.columns:
            df_features["TARGET_SUBJECT_PREVALENCE"] = 0
        if "PREVALENCE_DIFFERENCE_RATIO" not in df_features.columns:
            df_features["PREVALENCE_DIFFERENCE_RATIO"] = 0
        if "MEDIAN_FIRST_OCCURRENCE" not in df_features.columns:
            df_features["MEDIAN_FIRST_OCCURRENCE"] = df_features.get("time_median", None)
        if "IS_ORDINAL" not in df_features.columns:
            df_features["IS_ORDINAL"] = False
            
        # Add ordinal summaries if available
        if "ordinal_summaries" in parquet_data:
            ordinal_df = parquet_data["ordinal_summaries"].copy()
            if "IS_ORDINAL" not in ordinal_df.columns:
                ordinal_df["IS_ORDINAL"] = True
            if "MEDIAN_FIRST_OCCURRENCE" not in ordinal_df.columns:
                ordinal_df["MEDIAN_FIRST_OCCURRENCE"] = ordinal_df.get("time_median", None)
            df_features = pd.concat([df_features, ordinal_df], ignore_index=True)
        
        # Create df_dashboard directly from df_features for summary mode
        required_cols = ["CONCEPT_NAME", "HERITAGE", "TARGET_SUBJECT_PREVALENCE", "PREVALENCE_DIFFERENCE_RATIO", "CONCEPT_ID"]
        available_cols = [col for col in required_cols if col in df_features.columns]
        df_dashboard = df_features[available_cols].copy()
        
        # Preserve linking columns
        for col in ["ORIGINAL_CONCEPT_ID", "ORDINAL", "IS_ORDINAL", "MEDIAN_FIRST_OCCURRENCE"]:
            if col in df_features.columns:
                df_dashboard[col] = df_features[col]
        
        # Common processing for summary mode - create the rest of dashboard
        # Get or initialize show state for this study
        if selected_study not in dashboard_show_state:
            dashboard_show_state[selected_study] = {}
        
        # Add a "show" column based on stored state
        concept_ids = df_dashboard.get("CONCEPT_ID", range(len(df_dashboard))).tolist()
        df_dashboard["_concept_id"] = concept_ids
        df_dashboard["_show"] = [
            dashboard_show_state[selected_study].get(cid, True) for cid in concept_ids
        ]
        
        # Scale TARGET_SUBJECT_PREVALENCE to 0-100%
        if "TARGET_SUBJECT_PREVALENCE" in df_dashboard.columns:
            df_dashboard["TARGET_SUBJECT_PREVALENCE_PCT"] = df_dashboard["TARGET_SUBJECT_PREVALENCE"] * 100
        else:
            df_dashboard["TARGET_SUBJECT_PREVALENCE_PCT"] = 0
        
        # Handle PREVALENCE_DIFFERENCE_RATIO
        if "PREVALENCE_DIFFERENCE_RATIO" in df_dashboard.columns:
            df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"] = df_dashboard["PREVALENCE_DIFFERENCE_RATIO"].copy()
            df_dashboard.loc[
                (df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"] > 100) | 
                (df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"].isna()) |
                (~np.isfinite(df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"])),
                "PREVALENCE_DIFFERENCE_RATIO_DISPLAY"
            ] = 100
        else:
            df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"] = 0
        
        # Convert list columns to strings
        df_dashboard = convert_list_columns_to_strings(df_dashboard)
        
        # Apply default filters
        default_heritages = ["procedure_occurrence", "measurement", "drug_exposure"]
        available_heritages = set()
        if "HERITAGE" in df_dashboard.columns:
            for heritage in df_dashboard["HERITAGE"].dropna().unique():
                if heritage is not None and str(heritage).strip():
                    available_heritages.add(str(heritage))
        
        selected_heritages = [h for h in default_heritages if h in available_heritages]
        if not selected_heritages and available_heritages:
            selected_heritages = list(available_heritages)
        
        selected_heritages_set = set(selected_heritages)
        target_min, target_max = 10, 100
        ratio_min, ratio_max = 5, 100
        
        # OPTIMIZED: Vectorized filtering instead of iterrows
        target_prev = df_dashboard["TARGET_SUBJECT_PREVALENCE_PCT"].fillna(0)
        ratio = df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"].fillna(0)
        heritage_str = df_dashboard["HERITAGE"].fillna("").astype(str)
        
        matches_target = (target_prev >= target_min) & (target_prev <= target_max)
        matches_ratio = (ratio >= ratio_min) & (ratio <= ratio_max)
        matches_heritage = heritage_str.isin(selected_heritages_set)
        
        df_dashboard["_show"] = matches_target & matches_ratio & matches_heritage
        
        # OPTIMIZED: Filter out ordinal concepts when their main concept is filtered out
        filtered_out_main_concepts = set()
        for row in df_dashboard.to_dict('records'):
            if not _is_ordinal_concept(row) and not row.get("_show", True):
                main_concept_id = row.get("CONCEPT_ID")
                if main_concept_id is not None:
                    main_heritage = row.get("HERITAGE")
                    heritage_key = str(main_heritage) if main_heritage is not None and pd.notna(main_heritage) else None
                    filtered_out_main_concepts.add((_normalize_concept_id(main_concept_id), heritage_key))
        
        # Apply ordinal filtering using vectorized approach
        if filtered_out_main_concepts:
            ordinal_mask = df_dashboard.apply(lambda r: _is_ordinal_concept(r.to_dict()), axis=1)
            for idx in df_dashboard[ordinal_mask].index:
                row = df_dashboard.loc[idx]
                original_id = row.get("ORIGINAL_CONCEPT_ID")
                if original_id is not None:
                    heritage = row.get("HERITAGE")
                    heritage_key = str(heritage) if heritage is not None and pd.notna(heritage) else None
                    if (_normalize_concept_id(original_id), heritage_key) in filtered_out_main_concepts:
                        df_dashboard.loc[idx, "_show"] = False
        
        # OPTIMIZED: Update stored state using dict comprehension
        dashboard_show_state[selected_study] = {
            row["_concept_id"]: bool(row.get("_show", True))
            for row in df_dashboard.to_dict('records')
            if row.get("_concept_id") is not None
        }
        
        # Prepare dashboard data
        dashboard_data = df_dashboard.to_dict("records")
        
        # Import AgGrid inside function for background callback compatibility
        import dash_ag_grid as dag
        
        # Define columns for AgGrid (same as patient mode)
        column_defs = [
            {"field": "_show", "headerName": "Show", "width": 100, "cellStyle": {"textAlign": "center"},
             "cellRenderer": "agCheckboxCellRenderer", "cellEditor": "agCheckboxCellEditor", "editable": True,
             "filter": "agSetColumnFilter", "sort": "desc"},
            {"field": "CONCEPT_NAME", "headerName": format_column_name("CONCEPT_NAME"), "flex": 3, "minWidth": 200,
             "filter": "agTextColumnFilter", "resizable": True, "sortable": True},
            {"field": "HERITAGE", "headerName": "Heritage Type", "flex": 2, "minWidth": 150,
             "filter": "agTextColumnFilter", "resizable": True, "sortable": True,
             "valueFormatter": {"function": """
                function(params) {
                    if (!params.value) return '';
                    var mapping = {'procedure_occurrence': 'Procedures', 'condition_occurrence': 'Conditions',
                        'drug_exposure': 'Drug Exposures', 'measurement': 'Measurements', 'observation': 'Observations'};
                    return mapping[params.value.toLowerCase()] || params.value.replace(/_/g, ' ');
                }"""}},
            {"field": "MEDIAN_FIRST_OCCURRENCE", "headerName": "Median First Occurrence (days)", "flex": 2, "minWidth": 200,
             "filter": "agNumberColumnFilter", "valueFormatter": {"function": "params.value != null ? params.value.toFixed(1) + ' days' : ''"},
             "resizable": True, "sortable": True, "type": "numericColumn"},
            {"field": "TARGET_SUBJECT_PREVALENCE_PCT", "headerName": format_column_name("TARGET_SUBJECT_PREVALENCE") + " (%)",
             "flex": 2, "minWidth": 180, "filter": "agNumberColumnFilter",
             "valueFormatter": {"function": "params.value ? params.value.toFixed(2) + '%' : ''"}, "type": "numericColumn"},
            {"field": "PREVALENCE_DIFFERENCE_RATIO_DISPLAY", "headerName": format_column_name("PREVALENCE_DIFFERENCE_RATIO"),
             "flex": 2, "minWidth": 180, "filter": "agNumberColumnFilter",
             "valueFormatter": {"function": "params.value ? params.value.toFixed(2) : ''"}, "type": "numericColumn"}
        ]
        
        dashboard_table = dag.AgGrid(
            id="dashboard-table",
            rowData=dashboard_data,
            columnDefs=column_defs,
            defaultColDef={"resizable": True, "sortable": True, "filter": True, "floatingFilter": True},
            dashGridOptions={
                "pagination": True, "paginationPageSize": 20, "animateRows": True,
                "rowSelection": None, "suppressRowClickSelection": True,
                "getRowStyle": {"function": """
                    function(params) {
                        var style = {};
                        if (params.data && params.data._show === false) {
                            style.backgroundColor = '#d3d3d3'; style.color = '#6c757d';
                        } else if (params.node && params.node.rowIndex % 2 === 0) {
                            style.backgroundColor = '#f8f9fa';
                        }
                        return style;
                    }"""}
            },
            style={"width": "100%", "height": "calc(100vh - 250px)", "minHeight": "500px"},
            className="ag-theme-alpine",
            columnSize="sizeToFit"
        )
        
        description_component = []
        if study_description:
            description_component = [html.Div([
                html.P(study_description, style={
                    "backgroundColor": "#f8f9fa",
                    "padding": "15px",
                    "borderRadius": "6px",
                    "borderLeft": "4px solid #27ae60",
                    "marginBottom": "20px",
                    "color": "#495057",
                    "fontSize": "14px",
                    "whiteSpace": "pre-wrap"
                })
            ])]
        
        info_text_component = html.Div([
            html.P(f"Displaying {len(df_dashboard):,} concepts from pre-computed summary data.",
                   className="text-muted mb-2", style={"fontSize": "14px"}),
            html.P(
                "💡 Tip: Click filter icons in column headers for advanced filtering. Numeric columns support operators: equals, not equal, less than, greater than, in range (between), and blank.",
                className="text-muted mb-4",
                style={"fontSize": "12px", "fontStyle": "italic", "color": "#7f8c8d"}
            )
        ], style={"marginTop": "20px", "marginBottom": "15px"})
        
        tabs_content.append(
            dcc.Tab(label="Dashboard", value="dashboard", children=[
                html.Div([
                    html.H3("Features Dashboard (Summary Mode)", className="mb-3", style={"color": "#27ae60", "fontWeight": "600"}),
                    *description_component,
                    # The Composite Plot
                    html.Div([
                        html.Div([
                            html.H4("The Composite", style={"color": "#2c3e50", "fontWeight": "600", "marginBottom": "10px", "display": "inline-block"}),
                            html.Span(id="silhouette-score-display", style={
                                "marginLeft": "15px", 
                                "color": "#666", 
                                "fontSize": "13px",
                                "verticalAlign": "middle"
                            }),
                            html.Div([
                                html.Span("View: ", style={"fontWeight": "500", "marginRight": "10px", "color": "#666"}),
                                dcc.RadioItems(
                                    id="cluster-view-selector",
                                    options=[{"label": "All", "value": "all"}],
                                    value="all",
                                    inline=True,
                                    style={"display": "inline-block"},
                                    labelStyle={"marginRight": "15px", "cursor": "pointer"},
                                    inputStyle={"marginRight": "5px"}
                                )
                            ], style={"display": "inline-block", "marginLeft": "30px", "verticalAlign": "middle"}),
                        ], style={"marginBottom": "15px"}),
                        # Legend section (same as patient mode)
                        html.Div([
                            # Enrichment legend (Viridis colorscale)
                            html.Div([
                                html.Span("Enrichment:", style={"fontWeight": "600", "marginRight": "8px", "color": "#555", "fontSize": "12px"}),
                                html.Span("1", style={"fontSize": "10px", "color": "#666", "marginRight": "5px", "verticalAlign": "middle"}),
                                html.Div(style={
                                    "width": "100px", 
                                    "height": "12px", 
                                    "background": "linear-gradient(to right, #440154, #482878, #3e4989, #31688e, #26828e, #1f9e89, #35b779, #6ece58, #b5de2b, #fde725)",
                                    "borderRadius": "2px",
                                    "display": "inline-block",
                                    "verticalAlign": "middle"
                                }),
                                html.Span("100+", style={"fontSize": "10px", "color": "#666", "marginLeft": "5px", "verticalAlign": "middle"})
                            ], style={"display": "inline-block", "marginRight": "30px"}),
                            # Significance legend (for male% and age)
                            html.Div([
                                html.Span("Significance:", style={"fontWeight": "600", "marginRight": "8px", "color": "#555", "fontSize": "12px"}),
                                html.Span("*", style={"color": "#2E86AB", "fontSize": "14px", "marginRight": "3px"}),
                                html.Span("Significant", style={"fontSize": "11px", "color": "#666", "marginRight": "12px"}),
                                html.Span("*", style={"color": "#666666", "fontSize": "14px", "marginRight": "3px"}),
                                html.Span("Not significant", style={"fontSize": "11px", "color": "#666"})
                            ], style={"display": "inline-block", "marginRight": "30px"}),
                            # Median occurrence count legend
                            html.Div([
                                html.Span("Median Occurrences:", style={"fontWeight": "600", "marginRight": "8px", "color": "#555", "fontSize": "12px"}),
                                html.Span("*", style={"color": "#2ECC71", "fontSize": "14px", "marginRight": "3px"}),
                                html.Span("1", style={"fontSize": "11px", "color": "#666", "marginRight": "10px"}),
                                html.Span("*", style={"color": "#F1C40F", "fontSize": "14px", "marginRight": "3px"}),
                                html.Span("2", style={"fontSize": "11px", "color": "#666", "marginRight": "10px"}),
                                html.Span("*", style={"color": "#E74C3C", "fontSize": "14px", "marginRight": "3px"}),
                                html.Span("3+", style={"fontSize": "11px", "color": "#666"})
                            ], style={"display": "inline-block"})
                        ], style={
                            "backgroundColor": "#f8f9fa", 
                            "padding": "10px 15px", 
                            "borderRadius": "6px", 
                            "marginBottom": "15px",
                            "border": "1px solid #e9ecef"
                        }),
                        dcc.Graph(
                            id="composite-plot", 
                            style={"width": "100%"},
                            config={"displayModeBar": False}
                        )
                    ], id="composite-plot-container", style={"width": "100%", "marginBottom": "20px", "overflow": "visible"}),
                    # Add/remove concepts button - above the table
                    html.Div([
                        html.Button(
                            "Add/remove concepts",
                            id="update-plots-btn",
                            n_clicks=0,
                            style={
                                "padding": "10px 20px",
                                "backgroundColor": "#27ae60",
                                "color": "white",
                                "border": "none",
                                "borderRadius": "6px",
                                "fontSize": "14px",
                                "fontWeight": "500",
                                "cursor": "pointer",
                                "boxShadow": "0 2px 4px rgba(0,0,0,0.1)",
                                "transition": "all 0.2s ease"
                            }
                        ),
                        html.Span(
                            "  (Apply Filters also updates plots)",
                            style={"color": "#666", "fontSize": "12px", "marginLeft": "15px"}
                        )
                    ], style={"marginTop": "30px", "marginBottom": "15px"}),
                    info_text_component,
                    html.Div(
                        dashboard_table, 
                        style={
                            "width": "100%", 
                            "margin": "0",
                            "padding": "0",
                            "marginTop": "10px",
                            "boxSizing": "border-box",
                            "overflow": "auto",
                            "position": "relative"
                        },
                        id="dashboard-table-container"
                    )
                ], style={"padding": "20px"})
            ])
        )
        
    # PATIENT MODE: Process data_features with patient-level data
    elif "data_features" in parquet_data:
        df_features = parquet_data["data_features"].copy()
        
        # Filter for ABSTRACTION_LEVEL = -1
        if "ABSTRACTION_LEVEL" in df_features.columns:
            df_features = df_features[df_features["ABSTRACTION_LEVEL"] == -1].copy()
        
        # Filter out concepts that are never present in target patients
        # Check if TARGET_SUBJECT_COUNT or TARGET_SUBJECT_PREVALENCE is available
        if "TARGET_SUBJECT_COUNT" in df_features.columns:
            # Filter where TARGET_SUBJECT_COUNT > 0
            df_features = df_features[df_features["TARGET_SUBJECT_COUNT"] > 0].copy()
        elif "TARGET_SUBJECT_PREVALENCE" in df_features.columns:
            # Filter where TARGET_SUBJECT_PREVALENCE > 0
            df_features = df_features[df_features["TARGET_SUBJECT_PREVALENCE"] > 0].copy()
        elif "data_patients" in parquet_data:
            # Fallback: check data_patients directly
            df_patients = parquet_data["data_patients"]
            target_patients = df_patients[
                (df_patients["COHORT_DEFINITION_ID"] == "target") & 
                (df_patients["PREVALENCE"] > 0)
            ]
            # Use both CONCEPT_ID and HERITAGE for filtering
            if "HERITAGE" in target_patients.columns and "HERITAGE" in df_features.columns:
                target_concept_heritage = target_patients[["CONCEPT_ID", "HERITAGE"]].drop_duplicates()
                df_features = df_features.merge(
                    target_concept_heritage,
                    on=["CONCEPT_ID", "HERITAGE"],
                    how="inner"
                )
            else:
                # Fallback to CONCEPT_ID only if HERITAGE is missing
                target_concepts = target_patients["CONCEPT_ID"].unique()
                df_features = df_features[df_features["CONCEPT_ID"].isin(target_concepts)].copy()
        
        # Create ordinal concept variants (1st, 2nd, 3rd, etc.)
        if "data_patients" in parquet_data:
            df_features = create_ordinal_concepts(parquet_data["data_patients"], df_features)
        
        # Recalculate metrics from data_patients for all concepts (main + ordinals)
        if "data_patients" in parquet_data and "data_initial" in parquet_data:
            # Calculate metrics for all concepts
            metrics_df = calculate_concept_metrics_from_patients(
                parquet_data["data_patients"],
                parquet_data["data_initial"],
                df_features
            )
            
            # Merge metrics back to df_features, using main concept values as fallback
            if "HERITAGE" in df_features.columns and "HERITAGE" in metrics_df.columns:
                df_features = df_features.merge(
                    metrics_df,
                    on=["CONCEPT_ID", "HERITAGE"],
                    how="left",
                    suffixes=("", "_new")
                )
            else:
                df_features = df_features.merge(
                    metrics_df,
                    on="CONCEPT_ID",
                    how="left",
                    suffixes=("", "_new")
                )
            
            # Update with new calculated values, fallback to original for missing values
            if "TARGET_SUBJECT_PREVALENCE_new" in df_features.columns:
                # For ordinal concepts, use new value if available, otherwise use main concept value
                for idx, row in df_features.iterrows():
                    if pd.isna(df_features.loc[idx, "TARGET_SUBJECT_PREVALENCE_new"]):
                        # Try to get from main concept
                        if row.get("IS_ORDINAL") and "ORIGINAL_CONCEPT_ID" in row:
                            original_id = row["ORIGINAL_CONCEPT_ID"]
                            heritage = row.get("HERITAGE")
                            main_row = df_features[
                                (df_features["CONCEPT_ID"] == original_id) &
                                (df_features["IS_ORDINAL"] == False)
                            ]
                            if not main_row.empty:
                                if "HERITAGE" in df_features.columns and heritage:
                                    main_row = main_row[main_row["HERITAGE"] == heritage]
                                if not main_row.empty:
                                    df_features.loc[idx, "TARGET_SUBJECT_PREVALENCE_new"] = main_row.iloc[0]["TARGET_SUBJECT_PREVALENCE"]
                                    df_features.loc[idx, "PREVALENCE_DIFFERENCE_RATIO_new"] = main_row.iloc[0]["PREVALENCE_DIFFERENCE_RATIO"]
                
                df_features["TARGET_SUBJECT_PREVALENCE"] = df_features["TARGET_SUBJECT_PREVALENCE_new"].fillna(df_features["TARGET_SUBJECT_PREVALENCE"])
                df_features["PREVALENCE_DIFFERENCE_RATIO"] = df_features["PREVALENCE_DIFFERENCE_RATIO_new"].fillna(df_features["PREVALENCE_DIFFERENCE_RATIO"])
                df_features = df_features.drop(columns=["TARGET_SUBJECT_PREVALENCE_new", "PREVALENCE_DIFFERENCE_RATIO_new"])
        
        # Select only the required columns, but also include CONCEPT_ID for merging
        required_cols = ["CONCEPT_NAME", "HERITAGE", "TARGET_SUBJECT_PREVALENCE", "PREVALENCE_DIFFERENCE_RATIO"]
        available_cols = [col for col in required_cols if col in df_features.columns]
        # Always include CONCEPT_ID if it exists
        if "CONCEPT_ID" in df_features.columns:
            available_cols.append("CONCEPT_ID")
        df_dashboard = df_features[available_cols].copy()
        
        # Preserve linking columns if they exist
        if "ORIGINAL_CONCEPT_ID" in df_features.columns:
            df_dashboard["ORIGINAL_CONCEPT_ID"] = df_features["ORIGINAL_CONCEPT_ID"]
        if "ORDINAL" in df_features.columns:
            df_dashboard["ORDINAL"] = df_features["ORDINAL"]
        if "IS_ORDINAL" in df_features.columns:
            df_dashboard["IS_ORDINAL"] = df_features["IS_ORDINAL"]
        
        # Calculate median occurrence time for each concept (including ordinals)
        if "data_patients" in parquet_data:
            # Calculate for main concepts (1st occurrence)
            median_first_occurrence_df = calculate_median_first_occurrence(parquet_data["data_patients"])
            
            # For ordinal concepts, calculate median for the specific ordinal
            if "IS_ORDINAL" in df_dashboard.columns and df_dashboard["IS_ORDINAL"].any():
                ordinal_concepts_df = df_dashboard[df_dashboard["IS_ORDINAL"] == True].copy()
                if not ordinal_concepts_df.empty:
                    # Ensure we have CONCEPT_ID in ordinal_concepts_df for the calculation
                    if "CONCEPT_ID" not in ordinal_concepts_df.columns and "CONCEPT_ID" in df_features.columns:
                        # Get CONCEPT_IDs from df_features for ordinal rows
                        ordinal_indices = ordinal_concepts_df.index
                        ordinal_concepts_df["CONCEPT_ID"] = df_features.loc[ordinal_indices, "CONCEPT_ID"].values
                    
                    ordinal_medians = calculate_ordinal_medians(
                        parquet_data["data_patients"],
                        ordinal_concepts_df
                    )
                    # Combine main and ordinal medians
                    if not ordinal_medians.empty:
                        # Ensure CONCEPT_ID types match before concatenating
                        if "CONCEPT_ID" in median_first_occurrence_df.columns:
                            median_first_occurrence_df["CONCEPT_ID"] = median_first_occurrence_df["CONCEPT_ID"].astype(str)
                        if "CONCEPT_ID" in ordinal_medians.columns:
                            ordinal_medians["CONCEPT_ID"] = ordinal_medians["CONCEPT_ID"].astype(str)
                        median_first_occurrence_df = pd.concat([median_first_occurrence_df, ordinal_medians], ignore_index=True)
            
            # Ensure CONCEPT_ID and HERITAGE are in df_dashboard for merging
            if "CONCEPT_ID" not in df_dashboard.columns:
                concept_ids = df_features.get("CONCEPT_ID", range(len(df_features))).tolist()
                df_dashboard["CONCEPT_ID"] = concept_ids
            if "HERITAGE" not in df_dashboard.columns and "HERITAGE" in df_features.columns:
                df_dashboard["HERITAGE"] = df_features["HERITAGE"].values
            
            # Ensure CONCEPT_ID types match (convert to same type for merge)
            if "CONCEPT_ID" in df_dashboard.columns and "CONCEPT_ID" in median_first_occurrence_df.columns:
                df_dashboard["CONCEPT_ID"] = df_dashboard["CONCEPT_ID"].astype(str)
                median_first_occurrence_df["CONCEPT_ID"] = median_first_occurrence_df["CONCEPT_ID"].astype(str)
            
            # Merge on both CONCEPT_ID and HERITAGE if both are available
            if "HERITAGE" in df_dashboard.columns and "HERITAGE" in median_first_occurrence_df.columns:
                # Convert HERITAGE to string for consistent merging
                df_dashboard["HERITAGE"] = df_dashboard["HERITAGE"].astype(str)
                median_first_occurrence_df["HERITAGE"] = median_first_occurrence_df["HERITAGE"].astype(str)
                
                df_dashboard = df_dashboard.merge(
                    median_first_occurrence_df,
                    on=["CONCEPT_ID", "HERITAGE"],
                    how="left",
                    suffixes=("", "_median")
                )
                # If merge created duplicate column, use the median one
                if "MEDIAN_FIRST_OCCURRENCE_median" in df_dashboard.columns:
                    df_dashboard["MEDIAN_FIRST_OCCURRENCE"] = df_dashboard["MEDIAN_FIRST_OCCURRENCE_median"].fillna(df_dashboard.get("MEDIAN_FIRST_OCCURRENCE"))
                    df_dashboard = df_dashboard.drop(columns=["MEDIAN_FIRST_OCCURRENCE_median"])
            else:
                # Fallback to CONCEPT_ID only if HERITAGE is missing
                df_dashboard = df_dashboard.merge(
                    median_first_occurrence_df,
                    on="CONCEPT_ID",
                    how="left",
                    suffixes=("", "_median")
                )
                # If merge created duplicate column, use the median one
                if "MEDIAN_FIRST_OCCURRENCE_median" in df_dashboard.columns:
                    df_dashboard["MEDIAN_FIRST_OCCURRENCE"] = df_dashboard["MEDIAN_FIRST_OCCURRENCE_median"].fillna(df_dashboard.get("MEDIAN_FIRST_OCCURRENCE"))
                    df_dashboard = df_dashboard.drop(columns=["MEDIAN_FIRST_OCCURRENCE_median"])
            
            # Debug: Check if merge worked correctly
            # The merge should have worked, but let's verify ordinal concepts got their values
            # Only use fallback if median is actually missing (not just checking isna, but also checking if it's the same as main)
            
            # For ordinal concepts with missing median (only if merge failed), use main concept value as fallback
            if "IS_ORDINAL" in df_dashboard.columns and "ORIGINAL_CONCEPT_ID" in df_dashboard.columns:
                missing_ordinals = df_dashboard[
                    (df_dashboard["IS_ORDINAL"] == True) & 
                    (df_dashboard["MEDIAN_FIRST_OCCURRENCE"].isna())
                ]
                
                for idx in missing_ordinals.index:
                    ordinal_row = df_dashboard.loc[idx]
                    original_id = ordinal_row.get("ORIGINAL_CONCEPT_ID")
                    heritage = ordinal_row.get("HERITAGE")
                    
                    # Find main concept row
                    if "HERITAGE" in df_dashboard.columns and heritage is not None:
                        main_row = df_dashboard[
                            (df_dashboard["CONCEPT_ID"] == original_id) &
                            (df_dashboard["HERITAGE"] == heritage) &
                            (df_dashboard["IS_ORDINAL"] == False)
                        ]
                    else:
                        main_row = df_dashboard[
                            (df_dashboard["CONCEPT_ID"] == original_id) &
                            (df_dashboard["IS_ORDINAL"] == False)
                        ]
                    
                    # Only use fallback if median is actually missing (NaN)
                    if not main_row.empty and not pd.isna(main_row.iloc[0].get("MEDIAN_FIRST_OCCURRENCE")):
                        df_dashboard.loc[idx, "MEDIAN_FIRST_OCCURRENCE"] = main_row.iloc[0]["MEDIAN_FIRST_OCCURRENCE"]
        else:
            df_dashboard["MEDIAN_FIRST_OCCURRENCE"] = None
        
        # Get or initialize show state for this study
        if selected_study not in dashboard_show_state:
            dashboard_show_state[selected_study] = {}
        
        # Add a "show" column based on stored state
        concept_ids = df_dashboard.get("CONCEPT_ID", range(len(df_dashboard))).tolist()
        df_dashboard["_concept_id"] = concept_ids
        df_dashboard["_show"] = [
            dashboard_show_state[selected_study].get(cid, True) for cid in concept_ids
        ]
        
        # Scale TARGET_SUBJECT_PREVALENCE to 0-100% (multiply by 100)
        if "TARGET_SUBJECT_PREVALENCE" in df_dashboard.columns:
            df_dashboard["TARGET_SUBJECT_PREVALENCE_PCT"] = df_dashboard["TARGET_SUBJECT_PREVALENCE"] * 100
        
        # Handle PREVALENCE_DIFFERENCE_RATIO: cap at 100 for values > 100 or NA/0 in control
        if "PREVALENCE_DIFFERENCE_RATIO" in df_dashboard.columns:
            # Replace inf, very large values, or NaN with 100
            df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"] = df_dashboard["PREVALENCE_DIFFERENCE_RATIO"].copy()
            # Replace infinite or very large values with 100
            df_dashboard.loc[
                (df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"] > 100) | 
                (df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"].isna()) |
                (~np.isfinite(df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"])),
                "PREVALENCE_DIFFERENCE_RATIO_DISPLAY"
            ] = 100
        
        # Convert list columns to strings
        df_dashboard = convert_list_columns_to_strings(df_dashboard)
        
        # Apply default filters before preparing data
        # Default heritage selections: procedures, measurements, drugs
        default_heritages = ["procedure_occurrence", "measurement", "drug_exposure"]
        # Get available heritages from data
        available_heritages = set()
        if "HERITAGE" in df_dashboard.columns:
            for heritage in df_dashboard["HERITAGE"].dropna().unique():
                if heritage is not None and str(heritage).strip():
                    available_heritages.add(str(heritage))
        
        # Use default heritages that exist in the data, or all if none match
        selected_heritages = [h for h in default_heritages if h in available_heritages]
        if not selected_heritages and available_heritages:
            selected_heritages = list(available_heritages)
        
        selected_heritages_set = set(selected_heritages)
        
        # Default filter ranges
        target_min, target_max = 10, 100
        ratio_min, ratio_max = 5, 100
        
        # OPTIMIZED: Vectorized filtering instead of iterrows
        target_prev = df_dashboard["TARGET_SUBJECT_PREVALENCE_PCT"].fillna(0)
        ratio = df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"].fillna(0)
        heritage_str = df_dashboard["HERITAGE"].fillna("").astype(str)
        
        matches_target = (target_prev >= target_min) & (target_prev <= target_max)
        matches_ratio = (ratio >= ratio_min) & (ratio <= ratio_max)
        matches_heritage = heritage_str.isin(selected_heritages_set)
        
        df_dashboard["_show"] = matches_target & matches_ratio & matches_heritage
        
        # OPTIMIZED: Filter out ordinal concepts when their main concept is filtered out
        filtered_out_main_concepts = set()
        for row in df_dashboard.to_dict('records'):
            if not _is_ordinal_concept(row) and not row.get("_show", True):
                main_concept_id = row.get("CONCEPT_ID")
                if main_concept_id is not None:
                    main_heritage = row.get("HERITAGE")
                    heritage_key = str(main_heritage) if main_heritage is not None and pd.notna(main_heritage) else None
                    filtered_out_main_concepts.add((_normalize_concept_id(main_concept_id), heritage_key))
        
        # Apply ordinal filtering
        if filtered_out_main_concepts:
            ordinal_mask = df_dashboard.apply(lambda r: _is_ordinal_concept(r.to_dict()), axis=1)
            for idx in df_dashboard[ordinal_mask].index:
                row = df_dashboard.loc[idx]
                original_id = row.get("ORIGINAL_CONCEPT_ID")
                if original_id is not None:
                    heritage = row.get("HERITAGE")
                    heritage_key = str(heritage) if heritage is not None and pd.notna(heritage) else None
                    if (_normalize_concept_id(original_id), heritage_key) in filtered_out_main_concepts:
                        df_dashboard.loc[idx, "_show"] = False
        
        # OPTIMIZED: Update stored state using dict comprehension
        if selected_study not in dashboard_show_state:
            dashboard_show_state[selected_study] = {}
        dashboard_show_state[selected_study] = {
            row["_concept_id"]: bool(row.get("_show", True))
            for row in df_dashboard.to_dict('records')
            if row.get("_concept_id") is not None
        }
        
        # Prepare data
        dashboard_data = df_dashboard.to_dict("records")
        
        # Keep _show as boolean for checkbox
        # Don't convert to string - AgGrid checkbox needs boolean
        
        # Define columns for AgGrid with proper filters
        column_defs = [
            {
                "field": "_show",
                "headerName": "Show",
                "width": 100,
                "cellStyle": {"textAlign": "center"},
                "cellRenderer": "agCheckboxCellRenderer",
                "cellEditor": "agCheckboxCellEditor",
                "editable": True,
                "suppressMovable": True,
                "resizable": True,
                "suppressSizeToFit": True,
                "filter": "agSetColumnFilter",
                "filterParams": {
                    "values": [True, False],
                    "buttons": ["apply", "reset"]
                },
                "sort": "desc"  # Default sort descending
            },
            {
                "field": "CONCEPT_NAME",
                "headerName": format_column_name("CONCEPT_NAME"),
                "flex": 3,
                "minWidth": 200,
                "filter": "agTextColumnFilter",
                "filterParams": {
                    "caseSensitive": False,
                    "buttons": ["apply", "reset"]
                },
                "resizable": True,
                "sortable": True
            },
            {
                "field": "HERITAGE",
                "headerName": "Heritage Type",
                "flex": 2,
                "minWidth": 150,
                "filter": "agTextColumnFilter",
                "filterParams": {
                    "caseSensitive": False,
                    "buttons": ["apply", "reset"]
                },
                "valueFormatter": {
                    "function": f"""
                    function(params) {{
                        if (!params.value) return '';
                        var heritage = params.value;
                        var mapping = {{
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
                        }};
                        if (mapping[heritage.toLowerCase()]) {{
                            return mapping[heritage.toLowerCase()];
                        }}
                        return heritage.replace(/_/g, ' ').replace(/\\b\\w/g, function(l) {{ return l.toUpperCase(); }});
                    }}
                    """
                },
                "resizable": True,
                "sortable": True
            },
            {
                "field": "MEDIAN_FIRST_OCCURRENCE",
                "headerName": "Median First Occurrence (days)",
                "flex": 2,
                "minWidth": 200,
                "filter": "agNumberColumnFilter",
                "filterParams": {
                    "buttons": ["apply", "reset"],
                    "allowedCharPattern": "\\d\\-\\."
                },
                "valueFormatter": {"function": "params.value != null ? params.value.toFixed(1) + ' days' : ''"},
                "resizable": True,
                "sortable": True,
                "type": "numericColumn"
            },
            {
                "field": "TARGET_SUBJECT_PREVALENCE_PCT",
                "headerName": format_column_name("TARGET_SUBJECT_PREVALENCE") + " (%)",
                "flex": 2,
                "minWidth": 180,
                "filter": "agNumberColumnFilter",
                "filterParams": {
                    "buttons": ["apply", "reset"],
                    "allowedCharPattern": "\\d\\-\\."
                },
                "valueFormatter": {"function": "params.value ? params.value.toFixed(2) + '%' : ''"},
                "resizable": True,
                "sortable": True,
                "type": "numericColumn"
            },
            {
                "field": "PREVALENCE_DIFFERENCE_RATIO_DISPLAY",
                "headerName": format_column_name("PREVALENCE_DIFFERENCE_RATIO"),
                "flex": 2,
                "minWidth": 180,
                "filter": "agNumberColumnFilter",
                "filterParams": {
                    "buttons": ["apply", "reset"],
                    "allowedCharPattern": "\\d\\-\\."
                },
                "valueFormatter": {"function": "params.value ? params.value.toFixed(2) : ''"},
                "resizable": True,
                "sortable": True,
                "type": "numericColumn"
            }
        ]
        
        dashboard_table = AgGrid(
            id="dashboard-table",
            rowData=dashboard_data,
            columnDefs=column_defs,
            defaultColDef={
                "resizable": True,
                "sortable": True,
                "filter": True,
                "floatingFilter": True
            },
            dashGridOptions={
                "pagination": True,
                "paginationPageSize": 20,
                "animateRows": True,
                "rowSelection": None,
                "suppressRowClickSelection": True,
                "getRowStyle": {
                    "function": """
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
                },
                "onFirstDataRendered": {
                    "function": """
                    function(params) {
                        // Size columns to fit on initial render
                            params.api.sizeColumnsToFit();
                        // Sort by _show column descending
                        params.api.applyColumnState({
                            state: [{ colId: '_show', sort: 'desc' }],
                            defaultState: { sort: null }
                        });
                    }
                    """
                },
                "onGridSizeChanged": {
                    "function": """
                    function(params) {
                        if (params.clientWidth > 0) {
                            params.api.sizeColumnsToFit();
                        }
                    }
                    """
                }
                },
            style={"width": "100%", "height": "calc(100vh - 250px)", "minHeight": "500px"},
            className="ag-theme-alpine",
            columnSize="sizeToFit"
        )
        
        description_component = []
        if study_description:
            description_component = [html.Div([
                html.P(study_description, style={
                    "backgroundColor": "#f8f9fa",
                    "padding": "15px",
                    "borderRadius": "6px",
                    "borderLeft": "4px solid #2c3e50",
                    "marginBottom": "20px",
                    "color": "#495057",
                    "fontSize": "14px",
                    "whiteSpace": "pre-wrap"
                })
            ])]
        
        info_text_component = html.Div([
            html.P(
                f"Displaying {len(df_dashboard):,} concepts. Use the checkbox in the 'Show' column to toggle visibility for plotting.",
                className="text-muted mb-2",
                style={"fontSize": "14px"}
            ),
            html.P(
                "💡 Tip: Click filter icons in column headers for advanced filtering. Numeric columns support operators: equals, not equal, less than, greater than, in range (between), and blank.",
                className="text-muted mb-4",
                style={"fontSize": "12px", "fontStyle": "italic", "color": "#7f8c8d"}
            )
        ], style={"marginTop": "20px", "marginBottom": "15px"})
        
        tabs_content.append(
            dcc.Tab(label="Dashboard", value="dashboard", children=[
                html.Div([
                    html.H3("Features Dashboard", className="mb-3", style={"color": "#2c3e50", "fontWeight": "600"}),
                    *description_component,
                    # The Composite Plot
                    html.Div([
                        html.Div([
                            html.H4("The Composite", style={"color": "#2c3e50", "fontWeight": "600", "marginBottom": "10px", "display": "inline-block"}),
                            html.Span(id="silhouette-score-display", style={
                                "marginLeft": "15px", 
                                "color": "#666", 
                                "fontSize": "13px",
                                "verticalAlign": "middle"
                            }),
                            html.Div([
                                html.Span("View: ", style={"fontWeight": "500", "marginRight": "10px", "color": "#666"}),
                                dcc.RadioItems(
                                    id="cluster-view-selector",
                                    options=[{"label": "All", "value": "all"}],  # Will be updated dynamically
                                    value="all",
                                    inline=True,
                                    style={"display": "inline-block"},
                                    labelStyle={"marginRight": "15px", "cursor": "pointer"},
                                    inputStyle={"marginRight": "5px"}
                                )
                            ], style={"display": "inline-block", "marginLeft": "30px", "verticalAlign": "middle"}),
                            # Compare selector removed to reduce visual clutter
                        ], style={"marginBottom": "15px"}),
                        # Legend section
                        html.Div([
                            # Enrichment legend (Viridis colorscale)
                            html.Div([
                                html.Span("Enrichment:", style={"fontWeight": "600", "marginRight": "8px", "color": "#555", "fontSize": "12px"}),
                                html.Span("1", style={"fontSize": "10px", "color": "#666", "marginRight": "5px", "verticalAlign": "middle"}),
                                html.Div(style={
                                    "width": "100px", 
                                    "height": "12px", 
                                    "background": "linear-gradient(to right, #440154, #482878, #3e4989, #31688e, #26828e, #1f9e89, #35b779, #6ece58, #b5de2b, #fde725)",
                                    "borderRadius": "2px",
                                    "display": "inline-block",
                                    "verticalAlign": "middle"
                                }),
                                html.Span("100+", style={"fontSize": "10px", "color": "#666", "marginLeft": "5px", "verticalAlign": "middle"})
                            ], style={"display": "inline-block", "marginRight": "30px"}),
                            # Significance legend (for male% and age)
                            html.Div([
                                html.Span("Significance:", style={"fontWeight": "600", "marginRight": "8px", "color": "#555", "fontSize": "12px"}),
                                html.Span("*", style={"color": "#2E86AB", "fontSize": "14px", "marginRight": "3px"}),
                                html.Span("Significant", style={"fontSize": "11px", "color": "#666", "marginRight": "12px"}),
                                html.Span("*", style={"color": "#666666", "fontSize": "14px", "marginRight": "3px"}),
                                html.Span("Not significant", style={"fontSize": "11px", "color": "#666"})
                            ], style={"display": "inline-block", "marginRight": "30px"}),
                            # Median occurrence count legend
                            html.Div([
                                html.Span("Median Occurrences:", style={"fontWeight": "600", "marginRight": "8px", "color": "#555", "fontSize": "12px"}),
                                html.Span("*", style={"color": "#2ECC71", "fontSize": "14px", "marginRight": "3px"}),
                                html.Span("1", style={"fontSize": "11px", "color": "#666", "marginRight": "10px"}),
                                html.Span("*", style={"color": "#F1C40F", "fontSize": "14px", "marginRight": "3px"}),
                                html.Span("2", style={"fontSize": "11px", "color": "#666", "marginRight": "10px"}),
                                html.Span("*", style={"color": "#E74C3C", "fontSize": "14px", "marginRight": "3px"}),
                                html.Span("3+", style={"fontSize": "11px", "color": "#666"})
                            ], style={"display": "inline-block"})
                        ], style={
                            "backgroundColor": "#f8f9fa", 
                            "padding": "10px 15px", 
                            "borderRadius": "6px", 
                            "marginBottom": "15px",
                            "border": "1px solid #e9ecef"
                        }),
                        dcc.Graph(
                            id="composite-plot", 
                            style={"width": "100%"},
                            config={"displayModeBar": False}
                        )
                    ], id="composite-plot-container", style={"width": "100%", "marginBottom": "20px", "overflow": "visible"}),
                    # Add/remove concepts button - above the table
                    html.Div([
                        html.Button(
                            "Add/remove concepts",
                            id="update-plots-btn",
                            n_clicks=0,
                            style={
                                "padding": "10px 20px",
                                "backgroundColor": "#27ae60",
                                "color": "white",
                                "border": "none",
                                "borderRadius": "6px",
                                "fontSize": "14px",
                                "fontWeight": "500",
                                "cursor": "pointer",
                                "boxShadow": "0 2px 4px rgba(0,0,0,0.1)",
                                "transition": "all 0.2s ease"
                            }
                        ),
                        html.Span(
                            "  (Apply Filters also updates plots)",
                            style={"color": "#666", "fontSize": "12px", "marginLeft": "15px"}
                        )
                    ], style={"marginTop": "30px", "marginBottom": "15px"}),
                    info_text_component,
                    html.Div(
                        dashboard_table, 
                        style={
                            "width": "100%", 
                            "margin": "0",
                            "padding": "0",
                            "marginTop": "10px",
                            "boxSizing": "border-box",
                            "overflow": "auto",
                            "position": "relative"
                        },
                        id="dashboard-table-container"
                    )
                ], style={"width": "100%", "padding": "20px", "boxSizing": "border-box"})
            ])
        )
    
    # Add Mappings tab with complementaryMappingTable data
    mapping_table_content = html.Div([
                html.H3("Concept Mappings", style={"marginBottom": "15px", "color": "#2c3e50"}),
        html.P("No mapping data available.", style={"color": "#666", "fontSize": "14px"})
            ], style={"padding": "20px"})
    
    if parquet_data and "complementaryMappingTable" in parquet_data:
        mapping_df = parquet_data["complementaryMappingTable"].copy()
        
        # Remove ABSTRACTION_LEVEL column if it exists
        if "ABSTRACTION_LEVEL" in mapping_df.columns:
            mapping_df = mapping_df.drop(columns=["ABSTRACTION_LEVEL"])
        
        # Convert any list columns to strings for display
        mapping_df = convert_list_columns_to_strings(mapping_df)
        
        # Create column definitions for AG Grid
        mapping_columns = []
        column_display_names = {
            "CONCEPT_ID": "Source Concept ID",
            "CONCEPT_NAME": "Source Concept Name",
            "NEW_CONCEPT_ID": "Mapped Concept ID",
            "NEW_CONCEPT_NAME": "Mapped Concept Name",
            "TYPE": "Mapping Type"
        }
        
        for col in mapping_df.columns:
            col_def = {
                "field": col,
                "headerName": column_display_names.get(col, format_column_name(col)),
                "sortable": True,
                "filter": True,
                "resizable": True
            }
            # Make concept name columns wider
            if "NAME" in col:
                col_def["minWidth"] = 250
                col_def["flex"] = 2
            elif "ID" in col:
                col_def["minWidth"] = 120
            else:
                col_def["minWidth"] = 100
            mapping_columns.append(col_def)
        
        mapping_table_content = html.Div([
            html.H3("Concept Mappings", style={"marginBottom": "15px", "color": "#2c3e50"}),
            html.P(f"Showing {len(mapping_df)} mapping entries", style={"color": "#666", "fontSize": "14px", "marginBottom": "15px"}),
            dag.AgGrid(
                id="mappings-table",
                rowData=mapping_df.to_dict("records"),
                columnDefs=mapping_columns,
                defaultColDef={
                    "sortable": True,
                    "filter": True,
                    "resizable": True,
                    "floatingFilter": True
                },
                dashGridOptions={
                    "pagination": True,
                    "paginationPageSize": 50,
                    "animateRows": True,
                    "rowSelection": "single"
                },
                style={"height": "calc(100vh - 250px)", "width": "100%"},
                className="ag-theme-alpine"
            )
        ], style={"padding": "20px"})
    
    # Trajectories Tab - Show concept ordering by median time per cluster
    trajectories_content = html.Div([
        html.H3("Concept Trajectories", className="mb-3", style={"color": "#2c3e50", "fontWeight": "600"}),
        html.P("Concepts ordered by their median occurrence time. Delta shows position change from overall ordering.", 
               style={"color": "#666", "marginBottom": "10px"}),
        
        # Info note about cluster prevalence filter
        html.Div([
            html.Span("Use the ", style={"color": "#666", "fontSize": "13px"}),
            html.Span("Cluster Prevalence", style={"color": "#5a7d9a", "fontWeight": "600", "fontSize": "13px"}),
            html.Span(" slider in the sidebar Filters section to filter concepts by minimum cluster prevalence.", 
                     style={"color": "#666", "fontSize": "13px"})
        ], style={
            "backgroundColor": "#f0f7ff", 
            "padding": "10px 15px", 
            "borderRadius": "6px", 
            "marginBottom": "15px",
            "border": "1px solid #d0e3ff"
        }),
        
        dcc.Loading(
            id="trajectories-loading",
            type="circle",
            children=[
                html.Div(id="trajectories-plot-container", children=[
                    html.P("Select a study and wait for clustering to complete.", 
                           style={"color": "#999", "textAlign": "center", "padding": "50px"})
                ])
            ]
        )
    ], style={"padding": "20px"})
    
    tabs_content.append(
        dcc.Tab(label="Trajectories", value="trajectories", children=[trajectories_content])
    )
    
    # Overlap Tab - Show concept co-occurrence matrix
    overlap_content = html.Div([
        html.H3("Concept Overlap", className="mb-3", style={"color": "#2c3e50", "fontWeight": "600"}),
        html.P("Shows how often concepts co-occur in the same patients. Higher values indicate concepts appear together.", 
               style={"color": "#666", "marginBottom": "15px"}),
        
        # Controls row
            html.Div([
            # Group selector
            html.Div([
                html.Label("Group:", style={"marginRight": "10px", "fontWeight": "500"}),
                dcc.RadioItems(
                    id="overlap-group-selector",
                    options=[{"label": "Overall", "value": "overall"}],
                    value="overall",
                    inline=True,
                    style={"display": "inline-flex", "gap": "15px"},
                    inputStyle={"marginRight": "5px"}
                )
            ], style={"display": "flex", "alignItems": "center"}),
            # Hidden metric selector (kept for callback compatibility)
            dcc.RadioItems(
                id="overlap-metric-selector",
                options=[{"label": "Combined", "value": "combined"}],
                value="combined",
                style={"display": "none"}
            )
        ], style={"display": "flex", "flexWrap": "wrap", "marginBottom": "20px", "padding": "10px", 
                  "backgroundColor": "#f8f9fa", "borderRadius": "8px"}),
        
        # Plot container
        html.Div(id="overlap-plot-container", style={"marginTop": "10px"})
            ], style={"padding": "20px"})
    
    tabs_content.append(
        dcc.Tab(label="Overlap", value="overlap", children=[overlap_content])
    )
    
    # Demographics Tab
    demographics_content = html.Div([
        html.H3("Demographics", style={"marginBottom": "15px", "color": "#2c3e50", "fontWeight": "600"}),
        html.P("Age and sex distributions for the target cohort.", 
               style={"color": "#666", "marginBottom": "20px", "fontSize": "14px"}),
        html.Div(id="demographics-plot-container", style={"marginTop": "10px"})
    ], style={"padding": "20px"})
    
    tabs_content.append(
        dcc.Tab(label="Demographics", value="demographics", children=[demographics_content])
    )
    
    tabs_content.append(
        dcc.Tab(label="Mappings", value="mappings", children=[mapping_table_content])
    )
    
    
    # Register all concepts in the global registry for consistent lookups
    if 'dashboard_data' in locals() and dashboard_data:
        concept_registry.register_from_dashboard_data(dashboard_data)
    
    # Return (loading message is handled by running parameter)
    # Include a timestamp trigger to initiate clustering after data load
    import time
    clustering_trigger = time.time() if 'dashboard_data' in locals() and dashboard_data else None
    return (html.Div([
        dcc.Tabs(id="main-tabs", value="dashboard", children=tabs_content)
    ]), dashboard_data if 'dashboard_data' in locals() else None, clustering_trigger, data_mode)


@app.callback(
    [Output("sidebar", "style"),
     Output("main-content", "style")],
    Input("sidebar-toggle", "n_clicks")
)
def toggle_sidebar(n_clicks: Optional[int]):
    """
    Toggle sidebar visibility and adjust main content margin.
    
    Args:
        n_clicks: Number of times toggle button was clicked (None on initial load)
        
    Returns:
        Updated sidebar and main content styles
    """
    if n_clicks is None:
        n_clicks = 0
    
    base_sidebar_style = {
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
    }
    
    base_main_style = {
        "marginLeft": "450px",
        "padding": "0",
        "transition": "margin-left 0.3s ease",
        "width": "calc(100% - 450px)",
        "boxSizing": "border-box"
    }
    
    # Sidebar is hidden when n_clicks is odd
    if n_clicks % 2 == 1:  # Odd clicks = hidden
        base_sidebar_style["transform"] = "translateX(-100%)"
        base_main_style["marginLeft"] = "0"
        base_main_style["width"] = "100%"
    else:  # Even clicks (including 0) = visible
        base_sidebar_style["transform"] = "translateX(0)"
        base_main_style["marginLeft"] = "450px"
        base_main_style["width"] = "calc(100% - 450px)"
    
    return base_sidebar_style, base_main_style


@app.callback(
    [Output("dashboard-table", "rowData"),
     Output("plots-update-trigger-store", "data")],
    [Input("dashboard-table", "cellValueChanged"),
     Input("apply-filters-btn", "n_clicks"),
     Input("apply-table-selection-btn", "n_clicks")],
    [State("dashboard-table", "rowData"),
     State("dashboard-data-store", "data"),
     State("target-prevalence-range", "value"),
     State("ratio-range", "value"),
     State("heritage-selection-store", "data"),
     State("selected-study-store", "data"),
     State("plots-update-trigger-store", "data")],
    background=True,
    running=[
        (Output("loading-message-store", "data"), "Applying filters to concepts...", "")
    ],
    prevent_initial_call=True
)
def update_dashboard_table(
    cell_changed: Optional[Dict],
    apply_n_clicks: Optional[int],
    apply_table_n_clicks: Optional[int],
    row_data: Optional[List[Dict]],
    dashboard_data: Optional[List[Dict]],
    target_prevalence_range: List[float],
    ratio_range: List[float],
    selected_heritages: List[str],
    selected_study: Optional[str],
    current_trigger: Optional[int]
) -> Tuple[List[Dict], Optional[int]]:
    """
    Update dashboard table rowData - handles checkbox toggles, filter application, and table selection.
    
    Args:
        cell_changed: Information about the changed cell(s) - can be a dict or list of dicts
        apply_n_clicks: Number of times Apply Filters button was clicked
        apply_table_n_clicks: Number of times Apply Table Selection button was clicked
        row_data: Current table row data
        dashboard_data: Full dashboard data from store
        target_prevalence_range: [min, max] for target prevalence percentage
        ratio_range: [min, max] for prevalence difference ratio
        selected_heritages: List of selected heritage strings
        selected_study: Currently selected study
        
    Returns:
        Updated table row data
    """
    ctx = callback_context
    if not ctx.triggered:
        return row_data or [], no_update
    
    trigger_id = ctx.triggered[0]["prop_id"].split(".")[0]
    
    # Handle checkbox toggle (no loading needed, very fast)
    if trigger_id == "dashboard-table" and cell_changed is not None:
        if selected_study is None or not row_data:
            return row_data or [], no_update
        
        # Handle both single dict and list of dicts from AgGrid
        if isinstance(cell_changed, list):
            # If it's a list, process the first item that matches our column
            cell_changed = next((cell for cell in cell_changed if isinstance(cell, dict) and cell.get("colId") == "_show"), None)
            if cell_changed is None:
                return row_data, no_update
        
        # Ensure it's a dict at this point
        if not isinstance(cell_changed, dict):
            return row_data, no_update
        
        # Check if the changed cell is in the "_show" column
        if cell_changed.get("colId") == "_show":
            # Get row data from event (more reliable than rowIndex for sorted/filtered tables)
            changed_row_data = cell_changed.get("data")
            new_show = cell_changed.get("value")

            if changed_row_data is None:
                return row_data, no_update

            # Find the row in row_data by matching concept_id (unique identifier)
            changed_concept_id = changed_row_data.get("_concept_id")

            if changed_concept_id is None:
                # Fallback to rowIndex if no concept_id (shouldn't happen)
                row_idx = cell_changed.get("rowIndex")
                if row_idx is not None and row_idx < len(row_data):
                    updated_row_data = [row.copy() for row in row_data]
                    updated_row_data[row_idx]["_show"] = new_show
                    return updated_row_data, no_update
                return row_data, no_update

            # Create a copy of row_data and find the matching row by concept_id
            updated_row_data = [row.copy() for row in row_data]
            found = False
            changed_row_idx = None
            for i, row in enumerate(updated_row_data):
                if row.get("_concept_id") == changed_concept_id:
                    updated_row_data[i]["_show"] = new_show
                    found = True
                    changed_row_idx = i
                    break

            if found:
                # Update the stored state
                if selected_study not in dashboard_show_state:
                    dashboard_show_state[selected_study] = {}
                dashboard_show_state[selected_study][changed_concept_id] = bool(new_show)

                changed_row = updated_row_data[changed_row_idx]
                is_ordinal = changed_row.get("IS_ORDINAL", False)
                changed_heritage = changed_row.get("HERITAGE")
                
                if new_show:
                    # When checking a main concept, also check all its ordinals
                    if not is_ordinal:
                        main_concept_id = changed_row.get("CONCEPT_ID")
                        if main_concept_id is not None:
                            # Normalize main_concept_id for comparison
                            main_concept_id_str = str(main_concept_id).replace('.0', '') if '.' in str(main_concept_id) else str(main_concept_id)
                            
                            # Find and check all ordinals of this main concept (must match heritage too)
                            for i, row in enumerate(updated_row_data):
                                row_is_ordinal = row.get("IS_ORDINAL", False)
                                if row_is_ordinal:
                                    original_id = row.get("ORIGINAL_CONCEPT_ID")
                                    row_heritage = row.get("HERITAGE")
                                    # Normalize original_id for comparison
                                    original_id_str = str(original_id).replace('.0', '') if original_id and '.' in str(original_id) else str(original_id)
                                    if original_id_str == main_concept_id_str and row_heritage == changed_heritage:
                                        if not row.get("_show", False):
                                            updated_row_data[i]["_show"] = True
                                            ordinal_concept_id = row.get("_concept_id")
                                            if ordinal_concept_id:
                                                dashboard_show_state[selected_study][ordinal_concept_id] = True
                    
                    # When checking an ordinal, also check its main concept
                    if is_ordinal:
                        original_concept_id = changed_row.get("ORIGINAL_CONCEPT_ID")
                        if original_concept_id is not None:
                            # Normalize original_concept_id for comparison
                            original_concept_id_str = str(original_concept_id).replace('.0', '') if '.' in str(original_concept_id) else str(original_concept_id)
                            
                            # Find and check the main concept (must match heritage too)
                            for i, row in enumerate(updated_row_data):
                                row_concept_id = row.get("CONCEPT_ID")
                                row_heritage = row.get("HERITAGE")
                                row_is_ordinal = row.get("IS_ORDINAL", False)
                                
                                # Normalize row_concept_id for comparison
                                row_concept_id_str = str(row_concept_id).replace('.0', '') if row_concept_id and '.' in str(row_concept_id) else str(row_concept_id)
                                
                                # Match by CONCEPT_ID and HERITAGE (main concepts don't have IS_ORDINAL=True)
                                if not row_is_ordinal and row_concept_id_str == original_concept_id_str and row_heritage == changed_heritage:
                                    if not row.get("_show", False):
                                        updated_row_data[i]["_show"] = True
                                        main_concept_id = row.get("_concept_id")
                                        if main_concept_id:
                                            dashboard_show_state[selected_study][main_concept_id] = True
                                    break
                else:
                    # When unchecking a main concept, also uncheck all its ordinals
                    if not is_ordinal:
                        main_concept_id = changed_row.get("CONCEPT_ID")
                        if main_concept_id is not None:
                            # Normalize main_concept_id for comparison
                            main_concept_id_str = str(main_concept_id).replace('.0', '') if '.' in str(main_concept_id) else str(main_concept_id)
                            
                            # Find and uncheck all ordinals of this main concept (must match heritage too)
                            for i, row in enumerate(updated_row_data):
                                row_is_ordinal = row.get("IS_ORDINAL", False)
                                if row_is_ordinal:
                                    original_id = row.get("ORIGINAL_CONCEPT_ID")
                                    row_heritage = row.get("HERITAGE")
                                    # Normalize original_id for comparison
                                    original_id_str = str(original_id).replace('.0', '') if original_id and '.' in str(original_id) else str(original_id)
                                    if original_id_str == main_concept_id_str and row_heritage == changed_heritage:
                                        updated_row_data[i]["_show"] = False
                                        ordinal_concept_id = row.get("_concept_id")
                                        if ordinal_concept_id:
                                            dashboard_show_state[selected_study][ordinal_concept_id] = False
                
                return updated_row_data, no_update
        
        return row_data, no_update
    
    # Handle filter application
    if trigger_id == "apply-filters-btn" and apply_n_clicks is not None and apply_n_clicks > 0:
        if not dashboard_data or not selected_study:
            return row_data or [], no_update
        
        # Create a copy of dashboard data
        updated_data = [row.copy() for row in dashboard_data]
        
        # If no heritages selected, select all by default
        if not selected_heritages:
            # Get all unique heritages from data
            all_heritages = set()
            for row in updated_data:
                heritage = row.get("HERITAGE")
                if heritage is not None and pd.notna(heritage) and str(heritage).strip():
                    all_heritages.add(str(heritage))
            selected_heritages = list(all_heritages)
        
        # Convert selected_heritages to set for faster lookup
        selected_heritages_set = set(selected_heritages)
        
        # Extract filter criteria with defaults
        target_min = target_prevalence_range[0] if target_prevalence_range and len(target_prevalence_range) >= 1 else 0
        target_max = target_prevalence_range[1] if target_prevalence_range and len(target_prevalence_range) >= 2 else 100
        ratio_min = ratio_range[0] if ratio_range and len(ratio_range) >= 1 else 0
        ratio_max = ratio_range[1] if ratio_range and len(ratio_range) >= 2 else 100
        
        # Apply filters to all rows
        _apply_filters_to_data(
            updated_data, target_min, target_max, ratio_min, ratio_max,
            selected_heritages_set, selected_study
        )
        
        # Filter out ordinal concepts when their main concept is filtered out
        _filter_ordinal_concepts_for_filtered_mains(updated_data, selected_study)
        
        # Return updated data AND trigger plot updates with timestamp
        import time
        return updated_data, time.time()
    
    # Handle table selection application - use current Show column values
    if trigger_id == "apply-table-selection-btn" and apply_table_n_clicks is not None and apply_table_n_clicks > 0:
        if not row_data or not selected_study:
            return row_data or [], no_update
        
        # The current row_data already has the _show values from manual checkbox changes.
        # Just trigger a plot update with the current state - no need to modify anything.
        # This "locks in" the manual selections and triggers plot refresh.
        import time
        return row_data, time.time()
    
    return row_data or [], no_update


@app.callback(
    [Output("clustering-results-store", "data", allow_duplicate=True),
     Output("clustering-concepts-store", "data", allow_duplicate=True)],
    [Input("clustering-trigger-store", "data")],  # Trigger from clustering-trigger-store
    [State("dashboard-data-store", "data"),
     State("selected-study-store", "data"),
     State("cluster-count", "value")],
    prevent_initial_call='initial_duplicate'  # Allow initial call with duplicates
    # NOTE: background=True disabled due to macOS multiprocessing crashes
)
def perform_initial_clustering(
    clustering_trigger: Optional[float],
    dashboard_data_store: Optional[List[Dict]],
    selected_study: Optional[str],
    cluster_count: str
) -> Tuple[Optional[Dict], Optional[List[str]]]:
    """
    Perform initial clustering with filtered concepts when study data is loaded.
    This callback is triggered by clustering-trigger-store after dashboard data is loaded.
    Uses concepts where prevalence > 1% and ratio > 1.
    
    In summary mode, uses pre-computed clustering results.
    """
    logger.info(f"perform_initial_clustering called for: {selected_study}")
    
    if not clustering_trigger or not selected_study or not dashboard_data_store:
        return None, None
    # Background callbacks run in separate processes, so we need to load data here
    # Check if data is in cache, if not, load it
    if selected_study not in loaded_parquet_data:
        study_folder = DATA_DIR / selected_study
        if study_folder.exists():
            loaded_parquet_data[selected_study] = load_parquet_files(study_folder)
        else:
            return None, None
    
    parquet_data = loaded_parquet_data[selected_study]
    
    # Check if we're in summary mode
    data_mode = parquet_data.get("_mode", "patient")
    
    if data_mode == "summary":
        # SUMMARY MODE: Use pre-computed clustering results
        # Determine which k to use
        if cluster_count == "auto":
            # Find the k with best silhouette score
            available_k = get_available_cluster_k_values(parquet_data)
            if not available_k:
                return None, None
            
            # Get metadata for silhouette scores
            metadata = parquet_data.get("_metadata", {})
            clustering_info = metadata.get("clustering", {})
            
            best_k = available_k[0]
            best_score = -1
            for k in available_k:
                k_info = clustering_info.get(str(k), {})
                score = k_info.get("silhouette_score", 0)
                if score > best_score:
                    best_score = score
                    best_k = k
            k_value = best_k
        else:
            try:
                k_value = int(cluster_count)
            except (ValueError, TypeError):
                k_value = 3  # Default
        
        # Load pre-computed clustering for this k
        summary_key = f"clustering_k{k_value}_summary"
        if summary_key not in parquet_data:
            # Try to find closest available k
            available_k = get_available_cluster_k_values(parquet_data)
            if available_k:
                k_value = min(available_k, key=lambda x: abs(x - k_value))
                summary_key = f"clustering_k{k_value}_summary"
            else:
                return None, None
        
        if summary_key not in parquet_data:
            return None, None
        
        summary_matrix = parquet_data[summary_key]
        metadata = parquet_data.get("_metadata", {})
        clustering_info = metadata.get("clustering", {}).get(str(k_value), {})
        
        # Extract cluster counts from summary_matrix
        # Each cluster has 'total_cluster_patients' which is the same for all concepts in that cluster
        cluster_counts = {}
        if 'cluster' in summary_matrix.columns and 'total_cluster_patients' in summary_matrix.columns:
            # Get unique cluster -> count mapping (take first row for each cluster)
            for cluster_label in summary_matrix['cluster'].unique():
                cluster_rows = summary_matrix[summary_matrix['cluster'] == cluster_label]
                if not cluster_rows.empty:
                    # total_cluster_patients should be the same for all rows in a cluster
                    cluster_counts[cluster_label] = int(cluster_rows['total_cluster_patients'].iloc[0])
        
        # Convert to expected format
        results_dict = {
            'summary_matrix': summary_matrix.to_dict('records') if not summary_matrix.empty else [],
            'patient_assignments': [],  # Not available in summary mode
            'cluster_counts': cluster_counts,  # Pre-computed cluster counts for summary mode
            'best_silhouette_score': clustering_info.get("silhouette_score", 0),
            'optimal_cluster_count': k_value,
            '_mode': 'summary'  # Flag to indicate summary mode
        }
        
        # Get concept IDs from summary matrix
        concept_ids_used = [str(cid) for cid in summary_matrix["CONCEPT_ID"].unique()] if "CONCEPT_ID" in summary_matrix.columns else []
        
        return results_dict, concept_ids_used
    
    # PATIENT MODE: Compute clustering on the fly
    if "data_patients" not in parquet_data:
        return None, None
    
    data_patients = parquet_data["data_patients"]
    data_initial = parquet_data.get("data_initial", pd.DataFrame())
    
    # Filter concepts: prevalence > 1% and ratio > 1
    # Use TARGET_SUBJECT_PREVALENCE and PREVALENCE_DIFFERENCE_RATIO fields
    concepts_to_use = []
    for concept in dashboard_data_store:
        prevalence = concept.get("TARGET_SUBJECT_PREVALENCE", 0)
        ratio = concept.get("PREVALENCE_DIFFERENCE_RATIO", 0)
        
        # Convert to float if needed
        try:
            prevalence = float(prevalence) if prevalence is not None else 0
            ratio = float(ratio) if ratio is not None else 0
        except (ValueError, TypeError):
            prevalence = 0
            ratio = 0
        
        # Filter: prevalence > 1% (0.01) and ratio > 1
        if prevalence > 0.01 and ratio > 1:
            concepts_to_use.append(concept)
    
    if not concepts_to_use:
        return None, None
    
    # Parse cluster count
    if cluster_count == "auto":
        k_value = None
    else:
        try:
            k_value = int(cluster_count)
        except (ValueError, TypeError):
            k_value = None
    
    # Perform clustering - pass full dashboard_data but use filtered concepts for clustering
    # The perform_patient_clustering function will use concepts from dashboard_data
    try:
        clustering_results = perform_patient_clustering(
            data_patients=data_patients,
            data_initial=data_initial,
            dashboard_data=concepts_to_use,  # Use filtered concepts
            concept_limit=60,
            cluster_range=(2, 5),
            pca_components=20,
            time_window=180,
            k_value=k_value
        )
        
        # Convert DataFrames to dict for storage
        results_dict = {
            'summary_matrix': clustering_results['summary_matrix'].to_dict('records') if not clustering_results['summary_matrix'].empty else [],
            'patient_assignments': clustering_results['patient_assignments'].to_dict('records') if not clustering_results['patient_assignments'].empty else [],
            'best_silhouette_score': clustering_results['best_silhouette_score'],
            'optimal_cluster_count': clustering_results['optimal_cluster_count'],
            '_mode': 'patient'
        }
        
        # Store concept IDs used
        concept_ids_used = [str(item.get('CONCEPT_ID') or item.get('_concept_id', '')) for item in concepts_to_use]
        
        return results_dict, concept_ids_used
    except Exception as e:
        import traceback
        traceback.print_exc()
        return None, None


@app.callback(
    [Output("clustering-results-store", "data", allow_duplicate=True),
     Output("clustering-concepts-store", "data", allow_duplicate=True)],
    [Input("recluster-btn", "n_clicks"),
     Input("cluster-count", "value")],  # Also trigger on cluster count change
    [State("clustering-scope", "value"),
     State("dashboard-table", "rowData"),
     State("selected-study-store", "data"),
     State("dashboard-data-store", "data"),
     State("data-mode-store", "data")],
    prevent_initial_call=True
    # NOTE: background=True disabled due to macOS multiprocessing crashes
)
def perform_reclustering(
    recluster_clicks: Optional[int],
    cluster_count: str,
    clustering_scope: str,
    row_data: Optional[List[Dict]],
    selected_study: Optional[str],
    dashboard_data_store: Optional[List[Dict]],
    data_mode: Optional[str]
) -> Tuple[Optional[Dict], Optional[List[str]]]:
    """
    Perform reclustering with concepts based on scope selection.
    This callback is triggered by the recluster button OR cluster count change.
    
    In summary mode, loads pre-computed clustering for the selected k value.
    In patient mode, performs live clustering only on button click (not dropdown change).
    """
    from dash import ctx
    
    # Check what triggered the callback
    triggered_id = ctx.triggered_id if ctx.triggered_id else None
    
    # For patient mode, only recluster on button click (not dropdown change)
    # For summary mode, we can load pre-computed data on dropdown change too
    if triggered_id == "cluster-count":
        # Dropdown changed - only proceed for summary mode (fast, pre-computed)
        if data_mode != "summary":
            # In patient mode, dropdown change alone shouldn't trigger expensive reclustering
            return no_update, no_update
    
    if recluster_clicks is None and triggered_id == "recluster-btn":
        return None, None
    
    if not selected_study or not dashboard_data_store:
        return None, None
    
    # Load parquet data if needed
    if selected_study not in loaded_parquet_data:
        study_folder = DATA_DIR / selected_study
        if study_folder.exists():
            loaded_parquet_data[selected_study] = load_parquet_files(study_folder)
        else:
            return None, None
    
    parquet_data = loaded_parquet_data[selected_study]
    if not parquet_data:
        return None, None
    
    # Check actual data mode from parquet_data
    actual_data_mode = parquet_data.get("_mode", "patient")
    
    # SUMMARY MODE: Load pre-computed clustering for selected k
    if actual_data_mode == "summary":
        # Parse requested cluster count
        if cluster_count == "auto":
            # Find the k with best silhouette score
            available_k = get_available_cluster_k_values(parquet_data)
            if not available_k:
                return None, None
            
            # Get metadata for silhouette scores
            metadata = parquet_data.get("_metadata", {})
            clustering_info = metadata.get("clustering", {})
            
            best_k = available_k[0]
            best_score = -1
            for k in available_k:
                k_info = clustering_info.get(str(k), {})
                score = k_info.get("silhouette_score", 0)
                if score > best_score:
                    best_score = score
                    best_k = k
            k_value = best_k
        else:
            try:
                k_value = int(cluster_count)
            except (ValueError, TypeError):
                k_value = 3  # Default
        
        # Load pre-computed clustering for this k
        summary_key = f"clustering_k{k_value}_summary"
        if summary_key not in parquet_data:
            # Try to find closest available k
            available_k = get_available_cluster_k_values(parquet_data)
            if available_k:
                k_value = min(available_k, key=lambda x: abs(x - k_value))
                summary_key = f"clustering_k{k_value}_summary"
            else:
                return None, None
        
        if summary_key not in parquet_data:
            return None, None
        
        summary_matrix = parquet_data[summary_key]
        metadata = parquet_data.get("_metadata", {})
        clustering_info = metadata.get("clustering", {}).get(str(k_value), {})
        
        # Extract cluster counts from summary_matrix
        cluster_counts = {}
        if 'cluster' in summary_matrix.columns and 'total_cluster_patients' in summary_matrix.columns:
            for cluster_label in summary_matrix['cluster'].unique():
                cluster_rows = summary_matrix[summary_matrix['cluster'] == cluster_label]
                if not cluster_rows.empty:
                    cluster_counts[cluster_label] = int(cluster_rows['total_cluster_patients'].iloc[0])
        
        # Convert to expected format
        results_dict = {
            'summary_matrix': summary_matrix.to_dict('records') if not summary_matrix.empty else [],
            'patient_assignments': [],  # Not available in summary mode
            'cluster_counts': cluster_counts,
            'best_silhouette_score': clustering_info.get("silhouette_score", 0),
            'optimal_cluster_count': k_value,
            '_mode': 'summary'
        }
        
        # Get concept IDs from summary matrix
        concept_ids_used = [str(cid) for cid in summary_matrix["CONCEPT_ID"].unique()] if "CONCEPT_ID" in summary_matrix.columns else []
        
        return results_dict, concept_ids_used
    
    # PATIENT MODE: Perform live clustering
    if "data_patients" not in parquet_data:
        return None, None
    
    # Determine which concepts to use based on scope
    if clustering_scope == "active" and row_data:
        # Use only active (checked) concepts from the table
        concepts_to_use = [row for row in row_data if row.get("_show") is True]
    else:
        # Use all concepts from dashboard data (with prevalence > 1% and ratio > 1 for efficiency)
        concepts_to_use = [
            item for item in dashboard_data_store
            if item.get('TARGET_SUBJECT_PREVALENCE', 0) > 0.01 and
               item.get('PREVALENCE_DIFFERENCE_RATIO', 0) > 1
        ]
    
    if not concepts_to_use:
        return None, None
    
    data_patients = parquet_data["data_patients"]
    data_initial = parquet_data.get("data_initial", pd.DataFrame())
    
    # Parse cluster count
    if cluster_count == "auto":
        k_value = None
    else:
        try:
            k_value = int(cluster_count)
        except (ValueError, TypeError):
            k_value = None
    
    # Perform clustering
    try:
        clustering_results = perform_patient_clustering(
            data_patients=data_patients,
            data_initial=data_initial,
            dashboard_data=concepts_to_use,
            concept_limit=60,
            cluster_range=(2, 5),
            pca_components=20,
            time_window=180,
            k_value=k_value
        )
        
        # Convert DataFrames to dict for storage
        results_dict = {
            'summary_matrix': clustering_results['summary_matrix'].to_dict('records') if not clustering_results['summary_matrix'].empty else [],
            'patient_assignments': clustering_results['patient_assignments'].to_dict('records') if not clustering_results['patient_assignments'].empty else [],
            'best_silhouette_score': clustering_results['best_silhouette_score'],
            'optimal_cluster_count': clustering_results['optimal_cluster_count']
        }
        
        # Store concept IDs used
        concept_ids_used = [str(item.get('CONCEPT_ID') or item.get('_concept_id', '')) for item in concepts_to_use]
        
        return results_dict, concept_ids_used
    except Exception as e:
        import traceback
        traceback.print_exc()
        return None, None


@app.callback(
    [Output("composite-plot", "figure"),
     Output("composite-plot-container", "style")],
    [Input("update-plots-btn", "n_clicks"),
     Input("plots-update-trigger-store", "data"),
     Input("selected-study-store", "data"),
     Input("dashboard-data-store", "data"),
     Input("clustering-results-store", "data"),
     Input("cluster-view-selector", "value")],
    [State("dashboard-table", "rowData"),
     State("cluster-prevalence-slider", "value")],
    prevent_initial_call=False
)
def update_composite_plot(
    update_plots_clicks: Optional[int],
    plots_trigger: Optional[float],
    selected_study: Optional[str],
    dashboard_data_store: Optional[List[Dict]],
    clustering_results: Optional[Dict],
    selected_cluster: Optional[str],
    row_data: Optional[List[Dict]],
    cluster_prevalence_threshold: int
) -> Tuple[go.Figure, Dict]:
    """
    Update the composite plot based on active concepts.
    Triggered by Update Plots button, Apply Filters button, or data/study changes.
    
    Args:
        update_plots_clicks: Number of times Update Plots button clicked
        selected_study: Currently selected study
        dashboard_data_store: Full dashboard data from store
        clustering_results: Clustering results from store
        row_data: Current table row data
        selected_cluster: Selected cluster view ("all" or "C1", "C2", etc.)
        cluster_prevalence_threshold: Minimum cluster prevalence % to show concept (0-100)
        
    Returns:
        Plotly figure
    """
    # Use row_data if available, otherwise fall back to dashboard_data_store
    data_to_use = row_data if row_data else dashboard_data_store
    
    if not data_to_use or not selected_study:
        logger.warning("No data or study selected, returning empty figure")
        return create_empty_figure_with_style("Select a study and activate concepts to see the plot.")
    
    # Get active concepts (where _show is True)
    active_concepts = [row for row in data_to_use if row.get("_show") is True]
    
    if not active_concepts:
        return create_empty_figure_with_style("No active concepts. Check concepts in the table to display them.")
    
    # Get data_patients from loaded parquet data
    # Since load_study_data is a background callback, loaded_parquet_data might not be
    # accessible in this process. Check if we have the data, and if not, try to access it.
    # If dashboard_data_store exists, data should be loaded
    if not dashboard_data_store:
        return create_empty_figure_with_style("Loading study data...")
    
    # Try to get parquet data - it should be loaded if dashboard_data_store exists
    # Background callbacks may run in separate processes, so reload if needed
    if selected_study not in loaded_parquet_data:
        # Data not in cache - reload it since dashboard_data_store exists (data was processed)
        study_folder = DATA_DIR / selected_study
        if study_folder.exists():
            try:
                loaded_parquet_data[selected_study] = load_parquet_files(study_folder)
            except Exception as e:
                return create_empty_figure_with_style("Error loading data. Please refresh.")
        else:
            return create_empty_figure_with_style("Study folder not found.")
    
    parquet_data = loaded_parquet_data[selected_study]
    
    # Check data mode
    data_mode = parquet_data.get("_mode", "patient")
    
    # Get dashboard data for concept info
    dashboard_data = data_to_use
    
    # Get mapping data for hover tooltips
    mapping_data = parquet_data.get("complementaryMappingTable")
    
    if data_mode == "summary":
        # SUMMARY MODE: Use pre-computed distributions
        concept_summaries = parquet_data.get("concept_summaries", pd.DataFrame())
        ordinal_summaries = parquet_data.get("ordinal_summaries", pd.DataFrame())
        
        if concept_summaries.empty:
            return create_empty_figure_with_style("Summary data not available.")
        
        # Import summary mode function
        from plots.composite import create_composite_plot_from_summary
        
        # Get clustering summary matrix for cluster overlay in summary mode
        clustering_summary_matrix = None
        show_cluster_overlay = False
        if selected_cluster and selected_cluster != "all" and clustering_results:
            show_cluster_overlay = True
            # Get the clustering summary matrix for the selected k
            k_value = clustering_results.get("optimal_cluster_count", 3)
            summary_key = f"clustering_k{k_value}_summary"
            if summary_key in parquet_data:
                clustering_summary_matrix = parquet_data[summary_key]
        
        # Apply cluster prevalence filter when a specific cluster is selected
        if show_cluster_overlay and clustering_summary_matrix is not None and cluster_prevalence_threshold and cluster_prevalence_threshold > 0:
            cluster_prevalence_lookup = build_cluster_prevalence_lookup(clustering_summary_matrix, selected_cluster)
            original_count = len(active_concepts)
            active_concepts = filter_concepts_by_cluster_prevalence(active_concepts, cluster_prevalence_lookup, cluster_prevalence_threshold)
            logger.info(f"  Cluster prevalence filter: {original_count} -> {len(active_concepts)} concepts (threshold: {cluster_prevalence_threshold}%)")
            
            if not active_concepts:
                return create_empty_figure_with_style(f"No concepts with ≥{cluster_prevalence_threshold}% prevalence in Cluster {selected_cluster}.")
        
        try:
            fig_composite, y_labels_order, heritage_groups_order = create_composite_plot_from_summary(
                active_concepts, concept_summaries, dashboard_data,
                ordinal_summaries=ordinal_summaries,
                mapping_data=mapping_data,
                selected_cluster=selected_cluster if show_cluster_overlay else None,
                clustering_summary_matrix=clustering_summary_matrix
            )
        except Exception as e:
            logger.error(f"Error creating summary plot: {e}", exc_info=True)
            raise
        
        # Initialize variables needed for later code (not used in summary mode but required to exist)
        cluster_patient_ids = None
        data_patients = pd.DataFrame()  # Empty placeholder
        data_initial = pd.DataFrame()
        data_person = pd.DataFrame()
    else:
        # PATIENT MODE: Use patient-level data
        if "data_patients" not in parquet_data:
            return create_empty_figure_with_style("Patient data not available.")
        
        data_patients = parquet_data["data_patients"]
        data_initial = parquet_data.get("data_initial", pd.DataFrame())
        data_person = parquet_data.get("data_person", pd.DataFrame())
        
        # Get cluster patient IDs if a specific cluster is selected
        cluster_patient_ids = None  # Primary cluster (red overlay)
        show_cluster_overlay = False
        
        if selected_cluster and selected_cluster != "all" and clustering_results:
            patient_assignments = clustering_results.get('patient_assignments', [])
            if patient_assignments:
                if isinstance(patient_assignments, list):
                    df_assignments = pd.DataFrame(patient_assignments)
                else:
                    df_assignments = patient_assignments
                
                if not df_assignments.empty and 'cluster' in df_assignments.columns and 'patient_id' in df_assignments.columns:
                    # Get raw cluster patient IDs for primary cluster (red)
                    raw_cluster_ids = df_assignments[df_assignments['cluster'] == selected_cluster]['patient_id'].tolist()
                    
                    # Normalize cluster_patient_ids for robust matching with data_patients
                    cluster_patient_ids = set()
                    for pid in raw_cluster_ids:
                        cluster_patient_ids.add(pid)
                        cluster_patient_ids.add(str(pid))
                        try:
                            cluster_patient_ids.add(int(pid))
                        except (ValueError, TypeError):
                            pass
                        try:
                            cluster_patient_ids.add(float(pid))
                        except (ValueError, TypeError):
                            pass
                    
                    show_cluster_overlay = True
        
        # Apply cluster prevalence filter for patient mode when a specific cluster is selected
        if show_cluster_overlay and cluster_patient_ids and cluster_prevalence_threshold and cluster_prevalence_threshold > 0:
            # Calculate cluster prevalence for each concept from patient data
            cluster_size = len(set([pid for pid in cluster_patient_ids if isinstance(pid, (int, str)) and not isinstance(pid, float)]) or cluster_patient_ids) // 4  # Divide by normalization factor
            if cluster_size == 0:
                cluster_size = len(cluster_patient_ids) // 4 if len(cluster_patient_ids) > 0 else 1
            
            # Get patient counts per concept in the cluster
            cluster_prevalence_lookup = {}
            
            # Check what column identifies patients in data_patients
            patient_col = None
            for col in ["SUBJECT_ID", "patient_id", "PATIENT_ID", "person_id", "PERSON_ID"]:
                if col in data_patients.columns:
                    patient_col = col
                    break
            
            if patient_col:
                # Calculate prevalence for each concept
                cluster_patients_in_data = data_patients[data_patients[patient_col].isin(cluster_patient_ids)]
                
                # Get concept column
                concept_col = None
                for col in ["CONCEPT_ID", "concept_id"]:
                    if col in data_patients.columns:
                        concept_col = col
                        break
                
                if concept_col and not cluster_patients_in_data.empty:
                    # Get unique patients count in cluster
                    unique_cluster_patients = cluster_patients_in_data[patient_col].nunique()
                    if unique_cluster_patients > 0:
                        # Count unique patients per concept in cluster
                        concept_patient_counts = cluster_patients_in_data.groupby(concept_col)[patient_col].nunique()
                        for concept_id, patient_count in concept_patient_counts.items():
                            norm_id = _normalize_concept_id(concept_id)
                            prevalence_pct = (patient_count / unique_cluster_patients) * 100
                            cluster_prevalence_lookup[norm_id] = prevalence_pct
            
            # Also store string versions for robust lookup
            for concept_id in list(cluster_prevalence_lookup.keys()):
                if isinstance(concept_id, str):
                    cluster_prevalence_lookup[str(concept_id)] = cluster_prevalence_lookup[concept_id]
            
            # Filter active_concepts using helper function
            original_count = len(active_concepts)
            active_concepts = filter_concepts_by_cluster_prevalence(active_concepts, cluster_prevalence_lookup, cluster_prevalence_threshold)
            logger.info(f"  Patient mode cluster prevalence filter: {original_count} -> {len(active_concepts)} concepts (threshold: {cluster_prevalence_threshold}%)")
            
            if not active_concepts:
                return create_empty_figure_with_style(f"No concepts with ≥{cluster_prevalence_threshold}% prevalence in Cluster {selected_cluster}.")
        
        # Create the composite plot and get ordering information (PATIENT MODE)
        fig_composite, y_labels_order, heritage_groups_order = create_composite_plot(
            active_concepts, data_patients, dashboard_data,
            cluster_patient_ids=cluster_patient_ids,
            show_all_background=show_cluster_overlay,
            mapping_data=mapping_data
        )
    
    # Set flag for patient data availability
    has_patient_data = data_mode != "summary" and not data_patients.empty
    
    # Build cluster prevalence data for prevalence plot
    # Key structure must match: (heritage, original_concept_id_for_ordinals OR concept_id_for_main, ordinal)
    cluster_prevalence_data = None
    cluster_age_stats_summary = {}  # For summary mode cluster age stats
    cluster_male_prop_stats_summary = {}  # For summary mode cluster male prop stats
    
    # Initialize clustering_summary_matrix for patient mode to avoid NameError
    if data_mode != "summary":
        clustering_summary_matrix = None
    
    # SUMMARY MODE: Build cluster data from pre-computed clustering summary matrix
    if show_cluster_overlay and clustering_results and data_mode == "summary" and clustering_summary_matrix is not None:
        cluster_prevalence_data = {}
        from scipy import stats as scipy_stats
        
        # OPTIMIZED: Pre-build lookup dictionaries for concept data (avoid repeated DataFrame filtering)
        concept_data_lookup = {}
        for row in concept_summaries.to_dict('records'):
            cid = str(row.get("CONCEPT_ID", "")).replace(".0", "")
            concept_data_lookup[cid] = row
        
        if ordinal_summaries is not None and not ordinal_summaries.empty:
            for row in ordinal_summaries.to_dict('records'):
                cid = str(row.get("CONCEPT_ID", "")).replace(".0", "")
                concept_data_lookup[cid] = row
        
        # OPTIMIZED: Filter clustering_summary_matrix upfront and use to_dict
        filtered_cluster_rows = clustering_summary_matrix[
            clustering_summary_matrix['cluster'] == selected_cluster
        ].to_dict('records')
        
        for row in filtered_cluster_rows:
            cid = str(row.get("CONCEPT_ID", "")).replace(".0", "")
            is_ordinal = row.get("IS_ORDINAL", False)
            ordinal_raw = row.get("ORDINAL")
            ordinal = int(ordinal_raw) if ordinal_raw is not None and not pd.isna(ordinal_raw) else 0
            original_concept_id = row.get("ORIGINAL_CONCEPT_ID")
            
            # Determine lookup concept ID
            if is_ordinal and original_concept_id is not None:
                lookup_concept_id_str = _normalize_concept_id(original_concept_id)
            else:
                lookup_concept_id_str = _normalize_concept_id(cid)
            
            try:
                lookup_concept_id_int = int(lookup_concept_id_str)
            except (ValueError, TypeError):
                lookup_concept_id_int = lookup_concept_id_str
            
            cluster_prev = row.get("prevalence", 0)
            
            # Get concept data from pre-built lookup (FAST)
            concept_data = concept_data_lookup.get(cid, {})
            
            control_prevalence = concept_data.get("COMPARATOR_SUBJECT_PREVALENCE", 0)
            target_prevalence = concept_data.get("TARGET_SUBJECT_PREVALENCE", 0)
            overall_ratio = concept_data.get("PREVALENCE_DIFFERENCE_RATIO", 1.0)
            
            if (not control_prevalence or control_prevalence <= 0) and target_prevalence and overall_ratio and overall_ratio > 0:
                control_prevalence = target_prevalence / overall_ratio
            
            cluster_enrichment = cluster_prev / control_prevalence if control_prevalence and control_prevalence > 0 else 1.0
            
            # Get heritage from lookup
            heritage = row.get("HERITAGE")
            if heritage is None or pd.isna(heritage):
                lookup_cid = str(original_concept_id).replace(".0", "") if is_ordinal and original_concept_id else cid
                heritage = concept_data_lookup.get(lookup_cid, {}).get("HERITAGE", "unknown") or "unknown"
            
            key_str = (heritage, lookup_concept_id_str, ordinal)
            cluster_prevalence_data[key_str] = {'prevalence': cluster_prev, 'enrichment': cluster_enrichment}
            
            key_int = (heritage, lookup_concept_id_int, ordinal)
            
            # Build cluster age and male prop stats
            cluster_age_mean = row.get("age_mean")
            cluster_age_std = row.get("age_std")
            cluster_male_prop = row.get("male_proportion")
            patient_count = row.get("patient_count", 0) or 1
            
            if cluster_age_mean is not None and not pd.isna(cluster_age_mean):
                n = patient_count
                if n > 1 and cluster_age_std is not None and not pd.isna(cluster_age_std):
                    t_critical = scipy_stats.t.ppf(0.975, n - 1)
                    margin = t_critical * (cluster_age_std / np.sqrt(n))
                    cluster_age_stats_summary[key_int] = {
                        "mean_age": cluster_age_mean, "ci_low": cluster_age_mean - margin,
                        "ci_high": cluster_age_mean + margin, "n": n
                    }
                else:
                    cluster_age_stats_summary[key_int] = {
                        "mean_age": cluster_age_mean, "ci_low": cluster_age_mean,
                        "ci_high": cluster_age_mean, "n": n
                    }
            
            if cluster_male_prop is not None and not pd.isna(cluster_male_prop):
                n = patient_count
                if n > 1:
                    se = np.sqrt(cluster_male_prop * (1 - cluster_male_prop) / n)
                    ci_low = max(0, cluster_male_prop - 1.96 * se)
                    ci_high = min(1, cluster_male_prop + 1.96 * se)
                else:
                    ci_low = ci_high = cluster_male_prop
                
                cluster_male_prop_stats_summary[key_int] = {
                    "mean_male_prop": cluster_male_prop, "ci_low": ci_low, "ci_high": ci_high, "n": n
                }
        
    
    if show_cluster_overlay and clustering_results and has_patient_data:
        cluster_prevalence_data = {}
        summary_matrix = clustering_results.get('summary_matrix')
        
        # Handle both list and DataFrame formats
        if summary_matrix is not None:
            if isinstance(summary_matrix, list):
                summary_matrix = pd.DataFrame(summary_matrix)
        
        # Get cluster patients for this cluster
        patient_assignments = clustering_results.get('patient_assignments', [])
        if isinstance(patient_assignments, list):
            df_assignments = pd.DataFrame(patient_assignments)
        else:
            df_assignments = patient_assignments
        
        cluster_patients_set = set()
        if not df_assignments.empty and 'cluster' in df_assignments.columns:
            cluster_patients_set = set(df_assignments[df_assignments['cluster'] == selected_cluster]['patient_id'].tolist())
        
        # Get target cohort data for calculating prevalence
        df_target_for_calc = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
        
        # Build set of active concept IDs for filtering
        active_concept_ids = set()
        for concept in active_concepts:
            cid = concept.get('_concept_id') or concept.get('CONCEPT_ID')
            if cid:
                active_concept_ids.add(_normalize_concept_id(cid))
        
        
        # Build lookup only for ACTIVE concepts (not all dashboard_data)
        for item in dashboard_data:
            item_concept_id = item.get('_concept_id') or item.get('CONCEPT_ID')
            if item_concept_id is None:
                continue
            
            # Skip if not an active concept
            if _normalize_concept_id(item_concept_id) not in active_concept_ids:
                continue
            
            heritage = item.get('HERITAGE', 'unknown')
            is_ordinal = item.get('IS_ORDINAL', False)
            ordinal_num = item.get('ORDINAL', 0)
            original_concept_id = item.get('ORIGINAL_CONCEPT_ID')
            
            # Key structure MUST match create_prevalence_plot
            if is_ordinal:
                lookup_concept_id = _normalize_concept_id(original_concept_id)
            else:
                lookup_concept_id = _normalize_concept_id(item_concept_id)
            
            # Calculate cluster prevalence directly (same as heatmap)
            cluster_prev = 0.0
            
            if is_ordinal and ordinal_num > 0 and original_concept_id:
                # For ordinals: count patients with >= ordinal_num unique occurrences
                # Must count occurrences from TIME_TO_EVENT array, not rows!
                norm_original_id = _normalize_concept_id(original_concept_id)
                concept_data_calc = df_target_for_calc[
                    df_target_for_calc["CONCEPT_ID"].astype(str).str.replace('.0', '', regex=False) == norm_original_id
                ].copy()
                
                if not concept_data_calc.empty:
                    # Count unique occurrences per patient from TIME_TO_EVENT
                    def count_unique_occurrences(time_to_event):
                        if time_to_event is None:
                            return 0
                        if isinstance(time_to_event, np.ndarray):
                            if time_to_event.size == 0:
                                return 0
                            valid = time_to_event[~np.isnan(time_to_event)]
                            return len(np.unique(valid))
                        if isinstance(time_to_event, (list, tuple)):
                            valid = [t for t in time_to_event if t is not None and not (isinstance(t, (int, float)) and pd.isna(t))]
                            return len(set(valid))
                        if isinstance(time_to_event, (int, float)) and not pd.isna(time_to_event):
                            return 1
                        return 0
                    
                    concept_data_calc['OCC_COUNT'] = concept_data_calc['TIME_TO_EVENT'].apply(count_unique_occurrences)
                    patient_occ_counts = concept_data_calc.groupby("PERSON_ID")['OCC_COUNT'].sum()
                    patients_with_ordinal = set(patient_occ_counts[patient_occ_counts >= ordinal_num].index.tolist())
                    patients_in_cluster = patients_with_ordinal & cluster_patients_set
                    
                    if cluster_patients_set:
                        cluster_prev = len(patients_in_cluster) / len(cluster_patients_set)
            else:
                # For main concepts - try multiple ID formats like heatmap does
                norm_concept_id = _normalize_concept_id(item_concept_id)
                concept_data_calc = pd.DataFrame()
                
                # Try multiple ID formats
                for try_id in [item_concept_id, norm_concept_id, str(item_concept_id)]:
                    if try_id is None:
                        continue
                    try_id_str = str(try_id).replace('.0', '')
                    concept_data_calc = df_target_for_calc[
                        df_target_for_calc["CONCEPT_ID"].astype(str).str.replace('.0', '', regex=False) == try_id_str
                    ]
                    if not concept_data_calc.empty:
                        break
                
                if not concept_data_calc.empty:
                    patients_with_concept = set(concept_data_calc["PERSON_ID"].unique())
                    patients_in_cluster = patients_with_concept & cluster_patients_set
                    
                    if cluster_patients_set:
                        cluster_prev = len(patients_in_cluster) / len(cluster_patients_set)
                    
            
            # Calculate cluster-specific enrichment
            # Formula: cluster_prevalence / overall_control_prevalence
            # The control prevalence is the SAME as for overall enrichment (not cluster-specific)
            # This lets us compare: how enriched is this cluster vs the control population
            
            control_prevalence = item.get('COMPARATOR_SUBJECT_PREVALENCE', 0)
            target_prevalence = item.get('TARGET_SUBJECT_PREVALENCE', 0)
            overall_ratio = item.get('PREVALENCE_DIFFERENCE_RATIO', 1.0)
            
            # If control_prevalence is missing, derive it from target_prevalence / overall_ratio
            if (not control_prevalence or control_prevalence <= 0) and target_prevalence and overall_ratio and overall_ratio > 0:
                control_prevalence = target_prevalence / overall_ratio
            
            if control_prevalence and control_prevalence > 0:
                cluster_enrichment = cluster_prev / control_prevalence
            else:
                cluster_enrichment = 1.0
            
            key = (heritage, lookup_concept_id, ordinal_num)
            cluster_prevalence_data[key] = {
                'prevalence': cluster_prev,
                'enrichment': cluster_enrichment
            }
            
            # Also store by direct concept_id
            direct_key = (heritage, _normalize_concept_id(item_concept_id), ordinal_num)
            if direct_key != key:
                cluster_prevalence_data[direct_key] = {
                    'prevalence': cluster_prev,
                    'enrichment': cluster_enrichment
                }
    
    # Create the prevalence plot using the same ordering
    fig_prevalence = create_prevalence_plot(
        active_concepts, dashboard_data, y_labels_order, heritage_groups_order,
        cluster_prevalence_data=cluster_prevalence_data,
        show_all_background=show_cluster_overlay
    )
    
    # Calculate age statistics
    if has_patient_data:
        age_stats = calculate_age_stats(
            active_concepts, data_patients, data_initial, data_person, heritage_groups_order
        )
    else:
        # In summary mode, extract age stats from pre-computed concept_summaries
        # IMPORTANT: Only build age_stats for ACTIVE (displayed) concepts, same as patient mode
        # This ensures x-axis range is calculated only from displayed concepts
        age_stats = {}
        
        # Build a set of active concept IDs for filtering
        active_concept_ids = set()
        for concept in active_concepts:
            cid = concept.get("CONCEPT_ID") or concept.get("_concept_id")
            if cid is not None:
                active_concept_ids.add(str(cid).replace(".0", ""))
                try:
                    active_concept_ids.add(int(float(str(cid).replace(".0", ""))))
                except (ValueError, TypeError):
                    pass
        
        # Build lookup from concept_summaries and ordinal_summaries
        concept_summaries = parquet_data.get("concept_summaries", pd.DataFrame())
        ordinal_summaries = parquet_data.get("ordinal_summaries", pd.DataFrame())
        
        # Combine both summaries
        all_summaries = pd.concat([concept_summaries, ordinal_summaries], ignore_index=True) if not ordinal_summaries.empty else concept_summaries
        
        # OPTIMIZED: Vectorized filtering and processing instead of iterrows
        if not all_summaries.empty and active_concept_ids:
            from scipy import stats as scipy_stats
            
            # Create check_id column for filtering
            # IMPORTANT: Use CONCEPT_ID for filtering because that's what's in active_concept_ids
            # (For ordinals, CONCEPT_ID is the transformed ID like 9201001, not ORIGINAL_CONCEPT_ID like 9201)
            all_summaries = all_summaries.copy()
            
            # Normalize CONCEPT_ID for comparison
            def normalize_id(x):
                if pd.isna(x):
                    return None
                try:
                    return int(float(str(x).replace(".0", "")))
                except (ValueError, TypeError):
                    return str(x).replace(".0", "")
            
            all_summaries['_check_id_norm'] = all_summaries['CONCEPT_ID'].apply(normalize_id)
            
            # Filter to active concepts only
            filtered = all_summaries[all_summaries['_check_id_norm'].isin(active_concept_ids)]
            
        
            for row in filtered.to_dict('records'):
                age_mean = row.get("age_mean")
                if age_mean is None or pd.isna(age_mean):
                    continue
                
                heritage = row.get("HERITAGE", "ALL") or "unknown"
                is_ordinal = bool(row.get("IS_ORDINAL")) if row.get("IS_ORDINAL") is not None and not pd.isna(row.get("IS_ORDINAL")) else False
                ordinal = int(row.get("ORDINAL")) if row.get("ORDINAL") is not None and not pd.isna(row.get("ORDINAL")) else 0
                
                # Determine lookup concept ID
                original_concept_id = row.get("ORIGINAL_CONCEPT_ID")
                concept_id = row.get("CONCEPT_ID")
                lookup_concept_id = original_concept_id if is_ordinal and pd.notna(original_concept_id) else concept_id
                
                age_std = row.get("age_std")
                n_ages = row.get("n_ages") or row.get("patient_count", 0) or 1
                male_proportion = row.get("male_proportion")
                
                # Calculate CI
                if n_ages > 1 and age_std and not pd.isna(age_std):
                    se = age_std / np.sqrt(n_ages)
                    t_critical = scipy_stats.t.ppf(0.975, df=n_ages - 1)
                    ci_low = age_mean - t_critical * se
                    ci_high = age_mean + t_critical * se
                else:
                    ci_low = ci_high = age_mean
                
                # Normalize concept ID for key
                try:
                    normalized_concept_id = int(float(str(lookup_concept_id).replace(".0", "")))
                except (ValueError, TypeError):
                    normalized_concept_id = str(lookup_concept_id).replace(".0", "")
                
                key = (heritage, normalized_concept_id, ordinal)
                age_stats[key] = {
                    "mean_age": age_mean,
                    "ci_low": ci_low,
                    "ci_high": ci_high,
                    "n": n_ages,
                    "male_proportion": male_proportion
                }
    
    time_x_min = None
    time_x_max = None
    all_ci_lows = []
    all_ci_highs = []
    for items in heritage_groups_order.values():
        for item in items:
            ci_low = item.get("q1") if item.get("q1") is not None else item.get("min")
            ci_high = item.get("q3") if item.get("q3") is not None else item.get("max")
            if ci_low is not None and not (isinstance(ci_low, float) and pd.isna(ci_low)):
                all_ci_lows.append(ci_low)
            if ci_high is not None and not (isinstance(ci_high, float) and pd.isna(ci_high)):
                all_ci_highs.append(ci_high)
    
    if all_ci_lows and all_ci_highs:
        time_x_min = min(all_ci_lows)
        time_x_max = max(all_ci_highs)
    
    # 2. Prevalence/Enrichment (Column 2) - calculate from prevalence plot or use 0-1
    # Prevalence is always 0-100%, so we can use 0 to 1 (or 0 to max prevalence if we want to be more precise)
    prevalence_x_min = 0.0
    prevalence_x_max = 1.0  # 100% = 1.0
    
    # Try to extract from prevalence plot if available
    if hasattr(fig_prevalence.layout, 'xaxis') and hasattr(fig_prevalence.layout.xaxis, 'range'):
        prev_range = fig_prevalence.layout.xaxis.range
        if prev_range:
            prevalence_x_min = prev_range[0]
            prevalence_x_max = prev_range[1]
    
    # 3. Age (Column 3) - calculate from age statistics
    age_x_min = None
    age_x_max = None
    overall_avg_age = None
    
    if age_stats and not data_patients.empty and not data_initial.empty and not data_person.empty:
        # Calculate overall average age from all unique people in the target cohort
        # This is the true study average, not person-concept combinations
        all_mean_ages = []
        
        # Create mappings for age calculation
        # IMPORTANT: Filter to target cohort first to avoid using control cohort dates
        target_initial = data_initial[data_initial['COHORT_DEFINITION_ID'] == 'target']
        initial_map = target_initial.set_index('SUBJECT_ID')['COHORT_START_DATE'].to_dict()
        person_map = data_person.set_index('PERSON_ID')['YEAR_OF_BIRTH'].to_dict()
        
        # Get all unique people in the target cohort
        target_patients = data_initial[data_initial['COHORT_DEFINITION_ID'] == 'target']['SUBJECT_ID'].unique()
        
        # Calculate ages for all unique people in target cohort
        all_individual_ages = []
        for person_id in target_patients:
            birth_year = person_map.get(person_id)
            if birth_year is None or pd.isna(birth_year):
                continue
            
            cohort_start = initial_map.get(person_id)
            if cohort_start is None or pd.isna(cohort_start):
                continue
            
            try:
                cohort_date = pd.to_datetime(cohort_start)
                birth_year_int = int(birth_year)
                age = cohort_date.year - birth_year_int
                
                if age >= 0 and age <= 150:
                    all_individual_ages.append(age)
            except (ValueError, TypeError, AttributeError):
                continue
        
        # Calculate overall average from all unique people in target cohort
        if all_individual_ages:
            overall_avg_age = np.mean(all_individual_ages)
        
        # For x-axis range, use mean ages only (not CIs)
        # CIs are for error bars, not axis limits
        for stats in age_stats.values():
            all_mean_ages.append(stats["mean_age"])
        
        if all_mean_ages:
            min_age = min(all_mean_ages)
            max_age = max(all_mean_ages)
            age_range = max_age - min_age
            if age_range == 0:
                age_x_min = min_age - 5
                age_x_max = max_age + 5
            else:
                age_x_min = min_age - age_range * 0.1
                age_x_max = max_age + age_range * 0.1
    elif age_stats and data_mode == "summary":
        # SUMMARY MODE: Read overall_avg_age from metadata
        metadata = parquet_data.get("_metadata", {})
        demographics = metadata.get("demographics", {})
        overall_avg_age = demographics.get("age_mean")
        
        # Calculate x-axis range from mean ages only (not CIs)
        # CIs are for error bars, not axis limits
        # Age means span a natural range; CIs from small samples can be extreme
        all_mean_ages = []
        for stats in age_stats.values():
            all_mean_ages.append(stats["mean_age"])
        
        if all_mean_ages:
            min_age = min(all_mean_ages)
            max_age = max(all_mean_ages)
            age_range = max_age - min_age
            if age_range == 0:
                age_x_min = min_age - 5
                age_x_max = max_age + 5
            else:
                # 10% padding to accommodate error bars visually
                age_x_min = min_age - age_range * 0.1
                age_x_max = max_age + age_range * 0.1
    
    # Calculate cluster-specific age statistics if a cluster is selected
    cluster_age_stats = {}
    if show_cluster_overlay and cluster_patient_ids and not data_patients.empty and not data_initial.empty and not data_person.empty:
        # Normalize cluster_patient_ids for robust matching
        cluster_patient_ids_normalized_age = set()
        for pid in cluster_patient_ids:
            cluster_patient_ids_normalized_age.add(pid)
            cluster_patient_ids_normalized_age.add(str(pid))
            try:
                cluster_patient_ids_normalized_age.add(int(pid))
            except (ValueError, TypeError):
                pass
        
        # Create mappings for age calculation with multiple key types
        initial_map = {}
        for _, row in data_initial.iterrows():
            sid = row['SUBJECT_ID']
            date = row['COHORT_START_DATE']
            initial_map[sid] = date
            initial_map[str(sid)] = date
            try:
                initial_map[int(sid)] = date
            except (ValueError, TypeError):
                pass
        
        person_map_birth = {}
        for _, row in data_person.iterrows():
            pid = row['PERSON_ID']
            year = row['YEAR_OF_BIRTH']
            person_map_birth[pid] = year
            person_map_birth[str(pid)] = year
            try:
                person_map_birth[int(pid)] = year
            except (ValueError, TypeError):
                pass
        
        # Filter data_patients to cluster patients only (use normalized IDs)
        df_cluster = data_patients[
            (data_patients["COHORT_DEFINITION_ID"] == "target") &
            (data_patients["PERSON_ID"].isin(cluster_patient_ids_normalized_age))
        ].copy()
        
        def _normalize_concept_id_age(concept_id):
            if concept_id is None:
                return None
            concept_id_str = str(concept_id).replace(".0", "")
            try:
                return int(float(concept_id_str))
            except (ValueError, TypeError):
                return concept_id_str
        
        # Calculate age stats for each concept in heritage_groups_order
        for heritage_key, items in heritage_groups_order.items():
            for item in items:
                is_ordinal_item = item.get("is_ordinal", False)
                
                if is_ordinal_item:
                    concept_id_key = _normalize_concept_id_age(item.get("original_concept_id"))
                    ordinal_num = item.get("ordinal", 0)
                else:
                    concept_id_key = _normalize_concept_id_age(
                        item.get("concept_id") or item.get("_concept_id")
                    )
                    ordinal_num = 0
                
                lookup_key = (heritage_key or "unknown", concept_id_key, ordinal_num)
                
                # Get patients with this concept
                # Helper to match concept IDs with type flexibility
                def match_concept_id(df_col, target_id):
                    if target_id is None:
                        return pd.Series([False] * len(df_col))
                    # Try multiple formats
                    target_normalized = _normalize_concept_id_age(target_id)
                    matches = (df_col == target_id) | (df_col == target_normalized)
                    try:
                        matches = matches | (df_col == str(target_id))
                    except:
                        pass
                    try:
                        matches = matches | (df_col == int(float(str(target_id).replace('.0', ''))))
                    except:
                        pass
                    return matches
                
                if is_ordinal_item:
                    original_concept_id = item.get("original_concept_id")
                    concept_data = df_cluster[match_concept_id(df_cluster["CONCEPT_ID"], original_concept_id)].copy()
                    
                    if not concept_data.empty and ordinal_num > 0:
                        # Count occurrences per patient
                        def count_occurrences(time_arr):
                            if time_arr is None:
                                return 0
                            if isinstance(time_arr, np.ndarray):
                                return len(time_arr[~np.isnan(time_arr)])
                            if isinstance(time_arr, (list, tuple)):
                                return len([t for t in time_arr if t is not None and not (isinstance(t, (int, float)) and pd.isna(t))])
                            return 1 if not pd.isna(time_arr) else 0
                        
                        concept_data["OCC_COUNT"] = concept_data["TIME_TO_EVENT"].apply(count_occurrences)
                        concept_data = concept_data[concept_data["OCC_COUNT"] >= ordinal_num].copy()
                else:
                    concept_id = item.get("concept_id") or item.get("_concept_id") or item.get("CONCEPT_ID")
                    concept_data = df_cluster[match_concept_id(df_cluster["CONCEPT_ID"], concept_id)].copy()
                
                if concept_data.empty:
                    continue
                
                patient_ids = concept_data["PERSON_ID"].unique()
                
                # Calculate ages for these patients
                ages = []
                for pid in patient_ids:
                    birth_year = person_map_birth.get(pid)
                    cohort_start = initial_map.get(pid)
                    
                    if birth_year is None or pd.isna(birth_year) or cohort_start is None or pd.isna(cohort_start):
                        continue
                    
                    try:
                        cohort_date = pd.to_datetime(cohort_start)
                        age = cohort_date.year - int(birth_year)
                        if 0 <= age <= 150:
                            ages.append(age)
                    except (ValueError, TypeError, AttributeError):
                        continue
                
                if len(ages) >= 2:
                    from scipy import stats as scipy_stats
                    mean_age = np.mean(ages)
                    std_age = np.std(ages, ddof=1)
                    n = len(ages)
                    t_critical = scipy_stats.t.ppf(0.975, n - 1)
                    margin = t_critical * (std_age / np.sqrt(n))
                    
                    cluster_age_stats[lookup_key] = {
                        "mean_age": mean_age,
                        "ci_low": mean_age - margin,
                        "ci_high": mean_age + margin,
                        "n": n
                    }
                elif len(ages) == 1:
                    cluster_age_stats[lookup_key] = {
                        "mean_age": ages[0],
                        "ci_low": ages[0],
                        "ci_high": ages[0],
                        "n": 1
                    }
    
    # Create the age plot using the same ordering
    # Use cluster_age_stats_summary for summary mode, cluster_age_stats for patient mode
    effective_cluster_age_stats = None
    if show_cluster_overlay:
        if data_mode == "summary" and cluster_age_stats_summary:
            effective_cluster_age_stats = cluster_age_stats_summary
        elif cluster_age_stats:
            effective_cluster_age_stats = cluster_age_stats
    
    fig_age = create_age_plot(
        heritage_groups_order, age_stats, overall_avg_age,
        cluster_age_stats=effective_cluster_age_stats,
        show_cluster_overlay=show_cluster_overlay,
        show_error_bars=True  # Always show error bars (same as patient mode)
    )
    
    displayed_keys = set()
    for heritage, items in heritage_groups_order.items():
        for item in items:
            cid = item.get("concept_id") or item.get("_concept_id") or item.get("CONCEPT_ID")
            ordinal = item.get("ordinal") or item.get("ORDINAL") or 0
            original_cid = item.get("original_concept_id") or item.get("ORIGINAL_CONCEPT_ID")
            is_ordinal = item.get("is_ordinal") or item.get("IS_ORDINAL") or False
            lookup_cid = original_cid if is_ordinal and original_cid else cid
            if lookup_cid is not None:
                try:
                    displayed_keys.add((heritage, int(float(str(lookup_cid).replace(".0", ""))), ordinal))
                except (ValueError, TypeError):
                    displayed_keys.add((heritage, str(lookup_cid).replace(".0", ""), ordinal))
    
    if effective_cluster_age_stats:
        filtered_cluster_age_stats = {k: v for k, v in effective_cluster_age_stats.items() if k in displayed_keys}
        if filtered_cluster_age_stats:
            age_x_min, age_x_max = extend_range_with_stats(
                age_x_min, age_x_max, filtered_cluster_age_stats,
                mean_key="mean_age", ci_low_key="ci_low", ci_high_key="ci_high",
                padding_ratio=0.0, default_padding=0.0
            )
    
    # Calculate male proportion statistics
    if has_patient_data:
        male_prop_stats = calculate_male_prop_stats(
            active_concepts, data_patients, data_initial, data_person, heritage_groups_order
        )
    else:
        # In summary mode, extract male proportion from age_stats (which now includes male_proportion)
        male_prop_stats = {}
        for key, stats in age_stats.items():
            male_prop = stats.get("male_proportion")
            if male_prop is not None and not pd.isna(male_prop):
                n = stats.get("n", 0)
                # Calculate binomial CI
                if n > 1:
                    se = np.sqrt(male_prop * (1 - male_prop) / n)
                    z_critical = 1.96
                    ci_low = max(0, male_prop - z_critical * se)
                    ci_high = min(1, male_prop + z_critical * se)
                else:
                    ci_low = male_prop
                    ci_high = male_prop
                
                male_prop_stats[key] = {
                    "mean_male_prop": male_prop,
                    "ci_low": ci_low,
                    "ci_high": ci_high,
                    "n": n
                }
    
    # Calculate overall average male proportion from all unique people in the target cohort
    overall_avg_male_prop = None
    male_prop_x_min = 0.0
    male_prop_x_max = 1.0
    
    if male_prop_stats and has_patient_data and not data_patients.empty and not data_person.empty:
        # Get all unique people in the target cohort
        target_patients = data_initial[data_initial['COHORT_DEFINITION_ID'] == 'target']['SUBJECT_ID'].unique()
        
        # Create mapping from PERSON_ID to GENDER_CONCEPT_ID
        person_map = data_person.set_index('PERSON_ID')['GENDER_CONCEPT_ID'].to_dict()
        
        # Calculate male proportions for all unique people in target cohort
        all_male_indicators = []
        for person_id in target_patients:
            gender_concept_id = person_map.get(person_id)
            if gender_concept_id is None or pd.isna(gender_concept_id):
                continue
            # 8507 = male
            is_male = (gender_concept_id == 8507)
            all_male_indicators.append(1 if is_male else 0)
        
        # Calculate overall average from all unique people in target cohort
        if all_male_indicators:
            overall_avg_male_prop = np.mean(all_male_indicators)
        
        # For x-axis range, use mean proportions and CIs from stats
        all_mean_props = []
        all_ci_lows = []
        all_ci_highs = []
        
        for stats in male_prop_stats.values():
            all_mean_props.append(stats["mean_male_prop"])
            all_ci_lows.append(stats["ci_low"])
            all_ci_highs.append(stats["ci_high"])
        
        if all_mean_props:
            min_prop = min(min(all_mean_props), min(all_ci_lows)) if all_ci_lows else min(all_mean_props)
            max_prop = max(max(all_mean_props), max(all_ci_highs)) if all_ci_highs else max(all_mean_props)
            prop_range = max_prop - min_prop
            if prop_range == 0:
                if min_prop == 1.0:
                    # All at 100% - show from 0.95 to 1.0 to avoid "double 100%" appearance
                    male_prop_x_min = 0.95
                    male_prop_x_max = 1.0
                elif min_prop == 0.0:
                    male_prop_x_min = 0.0
                    male_prop_x_max = 0.05
                else:
                    male_prop_x_min = max(0, min_prop - 0.1)
                    male_prop_x_max = min(1, max_prop + 0.1)
            else:
                male_prop_x_min = max(0, min_prop - prop_range * 0.1)
                male_prop_x_max = min(1, max_prop + prop_range * 0.1)
                # If all values are very close to 1.0, show a tighter range
                if max_prop >= 0.99 and min_prop >= 0.99:
                    male_prop_x_min = 0.95
                    male_prop_x_max = 1.0
    elif male_prop_stats and data_mode == "summary":
        # SUMMARY MODE: Read overall_avg_male_prop from metadata
        metadata = parquet_data.get("_metadata", {})
        demographics = metadata.get("demographics", {})
        overall_avg_male_prop = demographics.get("male_proportion")
        
        # Calculate x-axis range from male_prop_stats
        all_mean_props = []
        all_ci_lows = []
        all_ci_highs = []
        
        for stats in male_prop_stats.values():
            all_mean_props.append(stats["mean_male_prop"])
            all_ci_lows.append(stats["ci_low"])
            all_ci_highs.append(stats["ci_high"])
        
        if all_mean_props:
            min_prop = min(min(all_mean_props), min(all_ci_lows)) if all_ci_lows else min(all_mean_props)
            max_prop = max(max(all_mean_props), max(all_ci_highs)) if all_ci_highs else max(all_mean_props)
            prop_range = max_prop - min_prop
            if prop_range == 0:
                if min_prop == 1.0:
                    male_prop_x_min = 0.95
                    male_prop_x_max = 1.0
                elif min_prop == 0.0:
                    male_prop_x_min = 0.0
                    male_prop_x_max = 0.05
                else:
                    male_prop_x_min = max(0, min_prop - 0.1)
                    male_prop_x_max = min(1, max_prop + 0.1)
            else:
                male_prop_x_min = max(0, min_prop - prop_range * 0.1)
                male_prop_x_max = min(1, max_prop + prop_range * 0.1)
                if max_prop >= 0.99 and min_prop >= 0.99:
                    male_prop_x_min = 0.95
                    male_prop_x_max = 1.0
    
    # Calculate cluster-specific male proportion statistics if a cluster is selected
    cluster_male_prop_stats = {}
    if show_cluster_overlay and cluster_patient_ids and not data_patients.empty and not data_person.empty:
        # Create mapping from PERSON_ID to GENDER_CONCEPT_ID
        # Handle potential type mismatches by creating maps with multiple key types
        person_gender_map = {}
        for _, row in data_person.iterrows():
            pid = row['PERSON_ID']
            gender = row['GENDER_CONCEPT_ID']
            # Store with multiple key formats for robust lookup
            person_gender_map[pid] = gender
            person_gender_map[str(pid)] = gender
            try:
                person_gender_map[int(pid)] = gender
            except (ValueError, TypeError):
                pass
        
        # Also normalize cluster_patient_ids for robust matching
        cluster_patient_ids_normalized = set()
        for pid in cluster_patient_ids:
            cluster_patient_ids_normalized.add(pid)
            cluster_patient_ids_normalized.add(str(pid))
            try:
                cluster_patient_ids_normalized.add(int(pid))
            except (ValueError, TypeError):
                pass
        
        # Filter data_patients to cluster patients only - TARGET cohort only
        df_cluster = data_patients[
            (data_patients["COHORT_DEFINITION_ID"] == "target") &
            (data_patients["PERSON_ID"].isin(cluster_patient_ids_normalized))
        ].copy()
        
        def _normalize_concept_id_male(concept_id):
            if concept_id is None:
                return None
            concept_id_str = str(concept_id).replace(".0", "")
            try:
                return int(float(concept_id_str))
            except (ValueError, TypeError):
                return concept_id_str
        
        # Calculate male prop stats for each concept in heritage_groups_order
        for heritage_key, items in heritage_groups_order.items():
            for item in items:
                is_ordinal_item = item.get("is_ordinal", False)
                
                if is_ordinal_item:
                    concept_id_key = _normalize_concept_id_male(item.get("original_concept_id"))
                    ordinal_num = item.get("ordinal", 0)
                else:
                    concept_id_key = _normalize_concept_id_male(
                        item.get("concept_id") or item.get("_concept_id")
                    )
                    ordinal_num = 0
                
                # Use string key to match male_prop_stats lookup
                lookup_key = (heritage_key or "unknown", str(concept_id_key), ordinal_num)
                
                # Get patients with this concept
                # Helper to match concept IDs with type flexibility
                def match_concept_id_male(df_col, target_id):
                    if target_id is None:
                        return pd.Series([False] * len(df_col))
                    # Try multiple formats
                    target_normalized = _normalize_concept_id_male(target_id)
                    matches = (df_col == target_id) | (df_col == target_normalized)
                    try:
                        matches = matches | (df_col == str(target_id))
                    except:
                        pass
                    try:
                        matches = matches | (df_col == int(float(str(target_id).replace('.0', ''))))
                    except:
                        pass
                    return matches
                
                if is_ordinal_item:
                    original_concept_id = item.get("original_concept_id")
                    concept_data = df_cluster[match_concept_id_male(df_cluster["CONCEPT_ID"], original_concept_id)].copy()
                    
                    if not concept_data.empty and ordinal_num > 0:
                        # Count occurrences per patient
                        def count_occurrences_male(time_arr):
                            if time_arr is None:
                                return 0
                            if isinstance(time_arr, np.ndarray):
                                return len(time_arr[~np.isnan(time_arr)])
                            if isinstance(time_arr, (list, tuple)):
                                return len([t for t in time_arr if t is not None and not (isinstance(t, (int, float)) and pd.isna(t))])
                            return 1 if not pd.isna(time_arr) else 0
                        
                        concept_data["OCC_COUNT"] = concept_data["TIME_TO_EVENT"].apply(count_occurrences_male)
                        concept_data = concept_data[concept_data["OCC_COUNT"] >= ordinal_num].copy()
                else:
                    concept_id = item.get("concept_id") or item.get("_concept_id") or item.get("CONCEPT_ID")
                    concept_data = df_cluster[match_concept_id_male(df_cluster["CONCEPT_ID"], concept_id)].copy()
                
                if concept_data.empty:
                    continue
                
                patient_ids = concept_data["PERSON_ID"].unique()
                
                # Calculate male proportion for these patients
                male_indicators = []
                failed_lookups = 0
                for pid in patient_ids:
                    # Try multiple key formats for robust lookup
                    gender_concept_id = person_gender_map.get(pid)
                    if gender_concept_id is None:
                        gender_concept_id = person_gender_map.get(str(pid))
                    if gender_concept_id is None:
                        try:
                            gender_concept_id = person_gender_map.get(int(pid))
                        except (ValueError, TypeError):
                            pass
                    
                    if gender_concept_id is None or pd.isna(gender_concept_id):
                        failed_lookups += 1
                        continue
                    # 8507 = male
                    is_male = (gender_concept_id == 8507)
                    male_indicators.append(1 if is_male else 0)
                
                
                if len(male_indicators) >= 2:
                    from scipy import stats as scipy_stats
                    mean_male = np.mean(male_indicators)
                    n = len(male_indicators)
                    # Use normal approximation for proportion CI (same as original)
                    se = np.sqrt(mean_male * (1 - mean_male) / n)
                    z_critical = scipy_stats.norm.ppf(0.975)
                    ci_low = max(0, mean_male - z_critical * se)
                    ci_high = min(1, mean_male + z_critical * se)
                    
                    cluster_male_prop_stats[lookup_key] = {
                        "mean_male_prop": mean_male,
                        "ci_low": ci_low,
                        "ci_high": ci_high,
                        "n": n
                    }
                elif len(male_indicators) == 1:
                    mean_male = male_indicators[0]
                    cluster_male_prop_stats[lookup_key] = {
                        "mean_male_prop": mean_male,
                        "ci_low": mean_male,
                        "ci_high": mean_male,
                        "n": 1
                    }
    
    # Create the male proportion plot using the same ordering
    # Use cluster_male_prop_stats_summary for summary mode, cluster_male_prop_stats for patient mode
    effective_cluster_male_prop_stats = None
    if show_cluster_overlay:
        if data_mode == "summary" and cluster_male_prop_stats_summary:
            effective_cluster_male_prop_stats = cluster_male_prop_stats_summary
        elif cluster_male_prop_stats:
            effective_cluster_male_prop_stats = cluster_male_prop_stats
    
    fig_male_prop, male_prop_x_min, male_prop_x_max, _ = create_male_prop_plot(
        heritage_groups_order, male_prop_stats, overall_avg_male_prop,
        cluster_male_prop_stats=effective_cluster_male_prop_stats,
        show_cluster_overlay=show_cluster_overlay,
        show_error_bars=True  # Always show error bars (same as patient mode)
    )
    
    if effective_cluster_male_prop_stats:
        filtered_cluster_male_prop_stats = {k: v for k, v in effective_cluster_male_prop_stats.items() if k in displayed_keys}
        if filtered_cluster_male_prop_stats:
            male_prop_x_min, male_prop_x_max = extend_range_with_stats(
                male_prop_x_min, male_prop_x_max, filtered_cluster_male_prop_stats,
                mean_key="mean_male_prop", ci_low_key="ci_low", ci_high_key="ci_high",
                padding_ratio=0.0, default_padding=0.0
            )
        if male_prop_x_min is not None:
            male_prop_x_min = max(0, male_prop_x_min)
        if male_prop_x_max is not None:
            male_prop_x_max = min(1, male_prop_x_max)
    
    # Combine plots side by side using subplots
    from plotly.subplots import make_subplots
    
    # Calculate row heights based on concepts per heritage (30px per concept)
    concepts_per_heritage = {}
    for heritage, items in heritage_groups_order.items():
        concepts_per_heritage[heritage] = len(items)
    
    # Get heritages in order (only those that have data AND have at least 1 concept)
    heritages_with_data = [h for h in HERITAGE_ORDER if h in heritage_groups_order and concepts_per_heritage.get(h, 0) > 0]
    num_rows = len(heritages_with_data)
    
    # Also remove empty heritages from heritage_groups_order to keep consistent
    heritage_groups_order = {h: heritage_groups_order[h] for h in heritages_with_data}
    
    # Handle case where no heritages have concepts
    if num_rows == 0:
        return create_empty_figure_with_style("No concepts to display after filtering.")
    
    # Calculate row heights: use actual item count from heritage_groups_order for accurate heights
    # This ensures the height reflects the filtered concepts, not the original counts
    row_heights = []
    for h in heritages_with_data:
        item_count = len(heritage_groups_order.get(h, []))
        row_heights.append(max(1, item_count) * 30)  # Ensure at least 30px per row to avoid zero height
    
    # Check if clustering results are available
    optimal_k = 0
    cluster_data_available = False
    if clustering_results:
        optimal_k = clustering_results.get('optimal_cluster_count', 0)
        summary_matrix_data = clustering_results.get('summary_matrix', [])
        if optimal_k > 0 and summary_matrix_data:
            cluster_data_available = True
    
    # Create combined figure with 4 base columns + cluster columns if available
    # Base columns: Event occurrences, Prevalence/Enrichment, Age, Male %
    base_cols = 4
    num_cols = base_cols + (optimal_k if cluster_data_available else 0)
    
    # Calculate column widths dynamically
    if cluster_data_available:
        # Shrink base columns to make more room for cluster columns
        # Base: 0.30, 0.12, 0.12, 0.12 = 0.66, leaving 0.34 for clusters
        cluster_width = 0.34 / optimal_k if optimal_k > 0 else 0.15
        column_widths = [0.30, 0.12, 0.12, 0.12] + [cluster_width] * optimal_k
    else:
        column_widths = [0.4, 0.2, 0.2, 0.2]  # Original 4 columns
    
    fig_combined = make_subplots(
        rows=num_rows,
        cols=num_cols,
        column_widths=column_widths,
        row_heights=row_heights,
        shared_yaxes=True,
        horizontal_spacing=HORIZONTAL_SPACING,
        vertical_spacing=VERTICAL_SPACING,
        subplot_titles=([None] * (num_rows * num_cols))
    )
    
    # Helper function to extract row number from trace
    def get_trace_row(trace, default_row=1):
        """Extract row number from trace's yaxis attribute."""
        if hasattr(trace, 'yaxis') and trace.yaxis:
            yaxis_str = str(trace.yaxis).lower()
            row_str = yaxis_str.replace('y', '').replace('axis', '').strip()
            if row_str.isdigit():
                return int(row_str)
            elif row_str == '':
                return 1
        return default_row
    
    # Copy traces from composite plot to column 1
    for trace in fig_composite.data:
        row_num = get_trace_row(trace, 1)
        fig_combined.add_trace(trace, row=row_num, col=1)
    
    # Copy traces from prevalence plot to column 2
    for trace in fig_prevalence.data:
        row_num = get_trace_row(trace, 1)
        fig_combined.add_trace(trace, row=row_num, col=2)
    
    # Copy traces from age plot to column 3
    for trace in fig_age.data:
        row_num = get_trace_row(trace, 1)
        fig_combined.add_trace(trace, row=row_num, col=3)
    
    # Copy traces from male proportion plot to column 4
    for trace in fig_male_prop.data:
        row_num = get_trace_row(trace, 1)
        fig_combined.add_trace(trace, row=row_num, col=4)
    
    # Add cluster columns if clustering data is available
    if cluster_data_available and clustering_results:
        # Get cluster data
        summary_matrix_data = clustering_results.get('summary_matrix', [])
        patient_assignments_data = clustering_results.get('patient_assignments', [])
        
        if isinstance(summary_matrix_data, list):
            summary_matrix = pd.DataFrame(summary_matrix_data) if summary_matrix_data else pd.DataFrame()
        else:
            summary_matrix = summary_matrix_data if isinstance(summary_matrix_data, pd.DataFrame) else pd.DataFrame()
        
        # Populate the global concept_registry with cluster data from store
        # This ensures the registry has the latest cluster data for lookups
        if not summary_matrix.empty:
            concept_registry.populate_cluster_data_from_matrix(summary_matrix.to_dict('records'))
        
        # Get cluster counts
        cluster_counts = {}
        
        # Always create patient_assignments_df (even if empty for summary mode)
        if isinstance(patient_assignments_data, list):
            patient_assignments_df = pd.DataFrame(patient_assignments_data) if patient_assignments_data else pd.DataFrame()
        else:
            patient_assignments_df = patient_assignments_data if isinstance(patient_assignments_data, pd.DataFrame) else pd.DataFrame()
        
        # Check for pre-computed cluster counts (summary mode)
        if 'cluster_counts' in clustering_results and clustering_results['cluster_counts']:
            cluster_counts = clustering_results['cluster_counts']
        elif not patient_assignments_df.empty and 'cluster' in patient_assignments_df.columns:
            # Calculate from patient assignments (patient mode)
            cluster_counts = patient_assignments_df['cluster'].value_counts().to_dict()
        
        if not summary_matrix.empty:
            # Build concept map from dashboard_data for ordinal lookup
            dashboard_concept_map_cluster = {}
            for row in dashboard_data:
                cid = row.get("_concept_id") or row.get("CONCEPT_ID")
                if cid is not None:
                    dashboard_concept_map_cluster[str(cid)] = row
                    dashboard_concept_map_cluster[_normalize_concept_id(cid)] = row
            
            # Build lookup maps for cluster data (by name, by concept_id, and by ordinal key)
            cluster_data_map = {}
            cluster_id_map = {}
            ordinal_cluster_map = {}  # For ordinal lookup: (heritage, original_concept_id, ordinal, cluster) -> data
            
            # First, populate maps from summary_matrix for concepts that were in original clustering
            # This ensures consistency with Trajectories tab
            # Note: column names may be uppercase (from parquet) or lowercase
            concept_name_col = 'CONCEPT_NAME' if 'CONCEPT_NAME' in summary_matrix.columns else 'concept_name'
            concept_id_col = 'CONCEPT_ID' if 'CONCEPT_ID' in summary_matrix.columns else 'concept_id'
            cluster_col = 'cluster'
            
            # OPTIMIZED: Use to_dict instead of iterrows
            if concept_name_col in summary_matrix.columns and cluster_col in summary_matrix.columns:
                for row in summary_matrix.to_dict('records'):
                    name_normalized = normalize_concept_name(row.get(concept_name_col, ''))
                    cluster_label = row.get(cluster_col, '')
                    key = (name_normalized, cluster_label)
                    data_entry = {
                        'prevalence': row.get('prevalence', 0),
                        'median_days': row.get('time_median') or row.get('median_days'),
                        'count_category': row.get('count_category', '1')
                    }
                    cluster_data_map[key] = data_entry
                    
                    # Also map by concept_id
                    concept_id_sm = row.get(concept_id_col)
                    if concept_id_sm is not None:
                        key_id = (str(concept_id_sm), cluster_label)
                        cluster_id_map[key_id] = data_entry
                        key_id_norm = (_normalize_concept_id(concept_id_sm), cluster_label)
                        cluster_id_map[key_id_norm] = data_entry
            
            # Now calculate dynamically for concepts not in summary_matrix
            # Only if patient-level data is available
            if not data_patients.empty and 'COHORT_DEFINITION_ID' in data_patients.columns:
                df_target_cluster = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
            else:
                df_target_cluster = pd.DataFrame()
            
            # Process ALL concepts from heritage_groups_order (both main and ordinals)
            # Calculate cluster metrics dynamically using patient assignments
            # Skip if no patient-level data is available (summary mode)
            has_patient_data = not df_target_cluster.empty and 'CONCEPT_ID' in df_target_cluster.columns
            
            for heritage_key, items in heritage_groups_order.items():
                for item in items:
                    # Skip dynamic calculation if no patient data
                    if not has_patient_data:
                        # In summary mode, rely only on pre-computed data in cluster_data_map/cluster_id_map
                        continue
                    
                    is_ordinal_item = item.get("is_ordinal", False)
                    concept_id = item.get("concept_id") or item.get("_concept_id") or item.get("CONCEPT_ID")
                    concept_name = item.get("concept_name", "Unknown")
                    
                    if is_ordinal_item:
                        # Handle ordinal concepts
                        original_concept_id = item.get("original_concept_id") or item.get("ORIGINAL_CONCEPT_ID")
                        ordinal_num = item.get("ordinal") or item.get("ORDINAL", 0)
                        
                        # Get ordinal info from dashboard data if needed
                        if (not original_concept_id or ordinal_num == 0) and concept_id:
                            concept_info = dashboard_concept_map_cluster.get(str(concept_id)) or dashboard_concept_map_cluster.get(_normalize_concept_id(concept_id))
                            if concept_info:
                                if not original_concept_id:
                                    original_concept_id = concept_info.get("ORIGINAL_CONCEPT_ID", concept_id)
                                if ordinal_num == 0:
                                    ordinal_num = concept_info.get("ORDINAL", 0)
                        
                        if ordinal_num == 0 or not original_concept_id:
                            continue
                        
                        # Filter data for this original concept
                        if "HERITAGE" in df_target_cluster.columns and heritage_key is not None:
                            concept_data = df_target_cluster[
                                (df_target_cluster["CONCEPT_ID"] == original_concept_id) &
                                (df_target_cluster["HERITAGE"] == heritage_key)
                            ].copy()
                        else:
                            concept_data = df_target_cluster[df_target_cluster["CONCEPT_ID"] == original_concept_id].copy()
                        
                        if concept_data.empty:
                            # No data - set all clusters to 0
                            for cluster_idx in range(optimal_k):
                                cluster_label = f'C{cluster_idx + 1}'
                                data_entry = {'prevalence': 0.0, 'median_days': np.nan, 'count_category': '1'}
                                if concept_id is not None:
                                    cluster_id_map[(str(concept_id), cluster_label)] = data_entry
                                cluster_data_map[(normalize_concept_name(concept_name), cluster_label)] = data_entry
                            continue
                        
                        # Get unique occurrences for each person
                        concept_data["OCCURRENCES"] = concept_data["TIME_TO_EVENT"].apply(get_unique_occurrences)
                        concept_data = concept_data[concept_data["OCCURRENCES"].apply(len) >= ordinal_num].copy()
                        
                        if concept_data.empty:
                            for cluster_idx in range(optimal_k):
                                cluster_label = f'C{cluster_idx + 1}'
                                data_entry = {'prevalence': 0.0, 'median_days': np.nan, 'count_category': '1'}
                                if concept_id is not None:
                                    cluster_id_map[(str(concept_id), cluster_label)] = data_entry
                                cluster_data_map[(normalize_concept_name(concept_name), cluster_label)] = data_entry
                            continue
                        
                        # Get the specific occurrence
                        concept_data["TIME_VALUE"] = concept_data["OCCURRENCES"].apply(
                            lambda occs: occs[ordinal_num - 1] if len(occs) >= ordinal_num else None
                        )
                        concept_data = concept_data[concept_data["TIME_VALUE"].notna()].copy()
                        
                        if concept_data.empty:
                            for cluster_idx in range(optimal_k):
                                cluster_label = f'C{cluster_idx + 1}'
                                data_entry = {'prevalence': 0.0, 'median_days': np.nan, 'count_category': '1'}
                                if concept_id is not None:
                                    cluster_id_map[(str(concept_id), cluster_label)] = data_entry
                                cluster_data_map[(normalize_concept_name(concept_name), cluster_label)] = data_entry
                            continue
                        
                        # Group by person
                        person_occurrences = concept_data.groupby("PERSON_ID")["TIME_VALUE"].first()
                        patients_with_concept = set(person_occurrences.index.tolist())
                        
                    else:
                        # Handle main concepts
                        # First check if this concept is already in the maps (from summary_matrix)
                        if concept_id is not None:
                            test_key = (str(concept_id), 'C1')
                            test_key_norm = (_normalize_concept_id(concept_id), 'C1')
                            if test_key in cluster_id_map or test_key_norm in cluster_id_map:
                                continue  # Already have data from summary_matrix
                        
                        # Also check by name
                        name_key = (normalize_concept_name(concept_name), 'C1')
                        if name_key in cluster_data_map:
                            continue  # Already have data from summary_matrix
                        
                        # Get the concept_id to query data_patients
                        query_concept_id = concept_id
                        
                        # Get concept info from dashboard data
                        if concept_id:
                            concept_info = dashboard_concept_map_cluster.get(str(concept_id)) or dashboard_concept_map_cluster.get(_normalize_concept_id(concept_id))
                            if concept_info:
                                query_concept_id = concept_info.get("CONCEPT_ID") or concept_id
                        
                        # Normalize concept_id for comparison (handle float vs int mismatch)
                        # Try multiple formats to match data_patients CONCEPT_ID
                        query_id_normalized = _normalize_concept_id(query_concept_id)
                        
                        # Get unique CONCEPT_IDs in data to check types
                        unique_ids_in_data = df_target_cluster["CONCEPT_ID"].unique()
                        
                        # Filter data for this concept - try different ID formats
                        concept_data = pd.DataFrame()
                        matched_id = None
                        
                        # Build list of IDs to try
                        ids_to_try = [query_concept_id, query_id_normalized]
                        if query_id_normalized:
                            try:
                                ids_to_try.append(float(query_id_normalized))
                            except (ValueError, TypeError):
                                pass
                            try:
                                ids_to_try.append(int(query_id_normalized))
                            except (ValueError, TypeError):
                                pass
                        
                        for try_id in ids_to_try:
                            if try_id is None:
                                continue
                            if "HERITAGE" in df_target_cluster.columns and heritage_key is not None:
                                concept_data = df_target_cluster[
                                    (df_target_cluster["CONCEPT_ID"] == try_id) &
                                    (df_target_cluster["HERITAGE"] == heritage_key)
                                ].copy()
                            else:
                                concept_data = df_target_cluster[df_target_cluster["CONCEPT_ID"] == try_id].copy()
                            
                            if not concept_data.empty:
                                matched_id = try_id
                                break
                        
                        # If still empty, try string comparison as last resort
                        if concept_data.empty and query_id_normalized:
                            str_query = str(query_id_normalized)
                            for uid in unique_ids_in_data:
                                if str(uid).replace('.0', '') == str_query:
                                    if "HERITAGE" in df_target_cluster.columns and heritage_key is not None:
                                        concept_data = df_target_cluster[
                                            (df_target_cluster["CONCEPT_ID"] == uid) &
                                            (df_target_cluster["HERITAGE"] == heritage_key)
                                        ].copy()
                                    else:
                                        concept_data = df_target_cluster[df_target_cluster["CONCEPT_ID"] == uid].copy()
                                    if not concept_data.empty:
                                        matched_id = uid
                                        break
                        
                        if concept_data.empty:
                            # No data - set all clusters to 0
                            for cluster_idx in range(optimal_k):
                                cluster_label = f'C{cluster_idx + 1}'
                                data_entry = {'prevalence': 0.0, 'median_days': np.nan, 'count_category': '1'}
                                if concept_id is not None:
                                    cluster_id_map[(str(concept_id), cluster_label)] = data_entry
                                cluster_data_map[(normalize_concept_name(concept_name), cluster_label)] = data_entry
                            continue
                        
                        # Get patients with this concept and their first occurrence time
                        # TIME_TO_EVENT can be arrays, so we need to extract the first value from each
                        def get_first_time(time_values):
                            if time_values is None:
                                return np.nan
                            if isinstance(time_values, np.ndarray):
                                if time_values.size == 0:
                                    return np.nan
                                valid = time_values[~np.isnan(time_values)]
                                return float(np.min(valid)) if len(valid) > 0 else np.nan
                            if isinstance(time_values, (list, tuple)):
                                valid = [t for t in time_values if t is not None and not (isinstance(t, (int, float)) and pd.isna(t))]
                                return float(min(valid)) if valid else np.nan
                            if isinstance(time_values, (int, float)) and not pd.isna(time_values):
                                return float(time_values)
                            return np.nan
                        
                        concept_data["FIRST_TIME"] = concept_data["TIME_TO_EVENT"].apply(get_first_time)
                        person_occurrences = concept_data.groupby("PERSON_ID")["FIRST_TIME"].min()
                        patients_with_concept = set(concept_data["PERSON_ID"].unique())
                    
                    # Calculate prevalence for each cluster
                    for cluster_idx in range(optimal_k):
                        cluster_label = f'C{cluster_idx + 1}'
                        # Check if patient_assignments_df has required columns
                        if patient_assignments_df.empty or 'cluster' not in patient_assignments_df.columns or 'patient_id' not in patient_assignments_df.columns:
                            cluster_patients = set()
                        else:
                            cluster_patients = set(patient_assignments_df[patient_assignments_df['cluster'] == cluster_label]['patient_id'].tolist())
                        total_cluster_patients = len(cluster_patients)
                        
                        if total_cluster_patients == 0:
                            prevalence = 0.0
                            median_days = np.nan
                            count_category = '1'
                        else:
                            patients_in_cluster = patients_with_concept & cluster_patients
                            prevalence = len(patients_in_cluster) / total_cluster_patients
                            
                            # Calculate median days and count category
                            if patients_in_cluster:
                                cluster_times = person_occurrences[person_occurrences.index.isin(patients_in_cluster)]
                                valid_times = cluster_times.dropna()
                                median_days = valid_times.median() if len(valid_times) > 0 else np.nan
                                
                                # Count category based on median occurrence count
                                if not is_ordinal_item:
                                    concept_counts = concept_data[concept_data["PERSON_ID"].isin(patients_in_cluster)].groupby("PERSON_ID").size()
                                    median_count = concept_counts.median() if len(concept_counts) > 0 else 1
                                    if median_count <= 1:
                                        count_category = "1"
                                    elif median_count <= 2:
                                        count_category = "2"
                                    else:
                                        count_category = "3+"
                                else:
                                    count_category = "1"
                            else:
                                median_days = np.nan
                                count_category = "1"
                        
                        # Add to maps
                        data_entry = {
                            'prevalence': prevalence,
                            'median_days': median_days,
                            'count_category': count_category
                        }
                        
                        if concept_id is not None:
                            cluster_id_map[(str(concept_id), cluster_label)] = data_entry
                        
                        cluster_data_map[(normalize_concept_name(concept_name), cluster_label)] = data_entry
                        
                        if is_ordinal_item and original_concept_id is not None and heritage_key is not None:
                            ordinal_key = (str(heritage_key), _normalize_concept_id(original_concept_id), ordinal_num, cluster_label)
                            ordinal_cluster_map[ordinal_key] = data_entry
            
            # Add heatmap cells for each heritage row
            for row_idx, heritage in enumerate(heritages_with_data, start=1):
                items = heritage_groups_order[heritage]
                num_concepts = len(items)
                
                for cluster_col_idx in range(optimal_k):
                    cluster_label = f'C{cluster_col_idx + 1}'
                    col_idx = base_cols + cluster_col_idx + 1
                    
                    # Build z_matrix (prevalences) for heatmap
                    z_col = []
                    text_data = []  # (prevalence_text, median_days_text, count_category)
                    
                    for y_idx, item in enumerate(items):
                        concept_name_raw = item.get("concept_name", "")
                        concept_name = normalize_concept_name(concept_name_raw)
                        concept_id = item.get("concept_id") or item.get("_concept_id") or item.get("CONCEPT_ID")
                        is_ordinal_item = item.get("is_ordinal", False)
                        
                        # Use the global concept_registry for cluster data lookup
                        # This is the single source of truth for cluster prevalence data
                        cluster_data = concept_registry.get_cluster_data(concept_id, cluster_label)
                        
                        # If not found by concept_id, try by ordinal lookup
                        if cluster_data.get('prevalence', 0.0) == 0.0 and is_ordinal_item:
                            original_concept_id = item.get("original_concept_id") or item.get("ORIGINAL_CONCEPT_ID")
                            ordinal_num = item.get("ordinal") or item.get("ORDINAL", 0)
                            item_heritage = item.get("heritage") or item.get("HERITAGE") or heritage
                            
                            # Try ordinal map lookup (still in old format for backward compat)
                            if original_concept_id is not None and item_heritage is not None and ordinal_num > 0:
                                ordinal_key = (str(item_heritage), concept_registry.normalize_id(original_concept_id), ordinal_num, cluster_label)
                                cluster_data = ordinal_cluster_map.get(ordinal_key, cluster_data)
                        
                        # Fallback: try old maps (for backward compatibility)
                        if cluster_data.get('prevalence', 0.0) == 0.0:
                            # Try old cluster_id_map
                            key_id = (str(concept_id), cluster_label) if concept_id else None
                            if key_id:
                                old_data = cluster_id_map.get(key_id) or cluster_id_map.get((concept_registry.normalize_id(concept_id), cluster_label))
                                if old_data:
                                    cluster_data = old_data
                            
                            # Try old cluster_data_map by name
                            if cluster_data.get('prevalence', 0.0) == 0.0:
                                key = (concept_name, cluster_label)
                                old_data = cluster_data_map.get(key)
                                if old_data:
                                    cluster_data = old_data
                        
                        # For ordinals in SUMMARY mode: inherit from parent concept
                        # Ordinals share their patients with the parent, so use parent's cluster data
                        if cluster_data.get('prevalence', 0.0) == 0.0 and is_ordinal_item and not has_patient_data:
                            original_concept_id = item.get("original_concept_id") or item.get("ORIGINAL_CONCEPT_ID")
                            if original_concept_id is not None:
                                # Try to get parent concept's cluster data
                                parent_cluster_data = concept_registry.get_cluster_data(original_concept_id, cluster_label)
                                if parent_cluster_data.get('prevalence', 0.0) > 0.0:
                                    cluster_data = parent_cluster_data
                                else:
                                    # Try in cluster_id_map with parent's concept_id
                                    parent_key = (str(original_concept_id), cluster_label)
                                    parent_data = cluster_id_map.get(parent_key)
                                    if parent_data and parent_data.get('prevalence', 0.0) > 0.0:
                                        cluster_data = parent_data
                                    else:
                                        # Try with normalized ID
                                        norm_parent_key = (str(original_concept_id).replace(".0", ""), cluster_label)
                                        parent_data = cluster_id_map.get(norm_parent_key)
                                        if parent_data and parent_data.get('prevalence', 0.0) > 0.0:
                                            cluster_data = parent_data
                        
                        # On-the-fly calculation for concepts not in any map
                        # This handles concepts added to the plot AFTER clustering
                        if cluster_data.get('prevalence', 0.0) == 0.0 and not patient_assignments_df.empty:
                            # Get patients in this cluster
                            cluster_patients = set()
                            if 'cluster' in patient_assignments_df.columns and 'patient_id' in patient_assignments_df.columns:
                                cluster_patients = set(
                                    patient_assignments_df[patient_assignments_df['cluster'] == cluster_label]['patient_id'].tolist()
                                )
                            
                            if cluster_patients and not df_target_cluster.empty:
                                # Normalize concept_id for comparison
                                norm_concept_id = concept_registry.normalize_id(concept_id)
                                
                                # Find patients with this concept
                                if is_ordinal_item:
                                    # For ordinals, check specific ordinal occurrence
                                    original_concept_id = item.get("original_concept_id") or item.get("ORIGINAL_CONCEPT_ID")
                                    ordinal_num = item.get("ordinal") or item.get("ORDINAL", 0)
                                    item_heritage = item.get("heritage") or item.get("HERITAGE") or heritage
                                    
                                    if original_concept_id and ordinal_num > 0:
                                        norm_original_id = concept_registry.normalize_id(original_concept_id)
                                        
                                        # Get patients with enough occurrences
                                        concept_data_calc = df_target_cluster[
                                            df_target_cluster["CONCEPT_ID"].astype(str).str.replace('.0', '', regex=False) == norm_original_id
                                        ].copy()
                                        
                                        if not concept_data_calc.empty:
                                            # Count occurrences per patient and filter those with >= ordinal_num
                                            patient_occ_counts = concept_data_calc.groupby("PERSON_ID").size()
                                            patients_with_ordinal = set(patient_occ_counts[patient_occ_counts >= ordinal_num].index.tolist())
                                            patients_in_cluster = patients_with_ordinal & cluster_patients
                                            
                                            if cluster_patients:
                                                calc_prevalence = len(patients_in_cluster) / len(cluster_patients)
                                                cluster_data = {'prevalence': calc_prevalence, 'median_days': None, 'count_category': '1'}
                                else:
                                    # For main concepts
                                    # Try multiple ID formats
                                    concept_data_calc = pd.DataFrame()
                                    for try_id in [concept_id, norm_concept_id]:
                                        if try_id is None:
                                            continue
                                        concept_data_calc = df_target_cluster[
                                            df_target_cluster["CONCEPT_ID"].astype(str).str.replace('.0', '', regex=False) == str(try_id).replace('.0', '')
                                        ]
                                        if not concept_data_calc.empty:
                                            break
                                    
                                    if not concept_data_calc.empty:
                                        patients_with_concept = set(concept_data_calc["PERSON_ID"].unique())
                                        patients_in_cluster = patients_with_concept & cluster_patients
                                        
                                        if cluster_patients:
                                            calc_prevalence = len(patients_in_cluster) / len(cluster_patients)
                                            cluster_data = {'prevalence': calc_prevalence, 'median_days': None, 'count_category': '1'}
                                            
                                            # Store in registry for future lookups
                                            concept_registry.set_cluster_data(concept_id, cluster_label, calc_prevalence)
                        
                        prevalence = cluster_data.get('prevalence', 0.0)
                        median_days = cluster_data.get('median_days')
                        count_cat = cluster_data.get('count_category', '1')
                        
                        z_col.append([prevalence])
                        
                        if median_days is not None and not pd.isna(median_days):
                            text_data.append((f"{prevalence:.0%}", f"{median_days:.0f}d", count_cat, prevalence))
                        else:
                            text_data.append((f"{prevalence:.0%}", None, count_cat, prevalence))
                    
                    # Add heatmap trace for this cluster column
                    fig_combined.add_trace(
                        go.Heatmap(
                            z=z_col,
                            colorscale=[[0, 'white'], [1, '#2E86AB']],
                            zmin=0,
                            zmax=1.0,
                            showscale=False,
                            hoverongaps=False,
                            xgap=2,
                            ygap=2,
                            hoverinfo='skip'
                        ),
                        row=row_idx,
                        col=col_idx
                    )
                    
                    # Add annotations and circles for each cell
                    circle_x = []
                    circle_y = []
                    circle_colors = []
                    circle_line_colors = []
                    
                    for y_idx, (prev_text, days_text, count_cat, prev_val) in enumerate(text_data):
                        # Prevalence text on left
                        fig_combined.add_annotation(
                            x=-0.35,
                            y=y_idx,
                            text=f"<b>{prev_text}</b>",
                            showarrow=False,
                            font=dict(size=9, color='black'),
                            xanchor='left',
                            yanchor='middle',
                            row=row_idx,
                            col=col_idx
                        )
                        
                        # Median days text on right
                        if days_text:
                            fig_combined.add_annotation(
                                x=0.35,
                                y=y_idx,
                                text=days_text,
                                showarrow=False,
                                font=dict(size=8, color='black'),
                                xanchor='right',
                                yanchor='middle',
                                row=row_idx,
                                col=col_idx
                            )
                        
                        # Circle data
                        circle_x.append(0)
                        circle_y.append(y_idx)
                        
                        if prev_val == 0 or prev_val == 0.0:
                            circle_colors.append("white")
                            circle_line_colors.append("black")
                        elif count_cat == "1":
                            circle_colors.append("#2ECC71")  # Green
                            circle_line_colors.append("white")
                        elif count_cat == "2":
                            circle_colors.append("#F1C40F")  # Yellow
                            circle_line_colors.append("white")
                        else:
                            circle_colors.append("#E74C3C")  # Red
                            circle_line_colors.append("white")
                    
                    # Add circles
                    fig_combined.add_trace(
                        go.Scatter(
                            x=circle_x,
                            y=circle_y,
                            mode='markers',
                            marker=dict(
                                size=10,
                                color=circle_colors,
                                line=dict(width=1, color=circle_line_colors)
                            ),
                            hoverinfo='skip',
                            showlegend=False
                        ),
                        row=row_idx,
                        col=col_idx
                    )
    
    # Calculate proper height for combined figure using constants from config
    # Recalculate concepts_per_heritage to ensure it reflects actual filtered items
    # This is needed because heritage_groups_order might have been filtered
    concepts_per_heritage_actual = {}
    for h in heritages_with_data:
        concepts_per_heritage_actual[h] = len(heritage_groups_order.get(h, []))
    
    total_concepts_combined = sum(concepts_per_heritage_actual[h] for h in heritages_with_data)
    plot_area_combined = max(MIN_PLOT_AREA_COMBINED, total_concepts_combined * PIXELS_PER_CONCEPT_COMBINED)
    heritage_gaps_combined = (num_rows - 1) * SMALL_GAP_COMBINED if num_rows > 1 else 0
    # Height includes data rows + gaps + margins
    combined_height = TOP_MARGIN_COMBINED + plot_area_combined + heritage_gaps_combined + BOTTOM_MARGIN_COMBINED
    
    # Update layout from composite plot - fixed margins
    fig_combined.update_layout(
        title="",  # No main title
        height=combined_height,
        showlegend=show_cluster_overlay,  # Show legend when cluster is selected
        legend=dict(
            orientation="h",
            yanchor="bottom",
            y=1.02,
            xanchor="left",
            x=0,
            font=dict(size=10)
        ) if show_cluster_overlay else {},
        plot_bgcolor="white",
        paper_bgcolor="white",
        margin=dict(l=200, r=50, t=TOP_MARGIN_COMBINED, b=BOTTOM_MARGIN_COMBINED),  # Fixed margins
        dragmode=False,
        hovermode="closest",  # Use closest point hover
        hoverlabel=dict(
            bgcolor='white',
            bordercolor='#ccc',
            font=dict(color='black', size=12),
            align='left'
        )
    )
    
    # Add column titles as annotations at the top
    # Position them at the very top of the paper, above the plot area
    base_col_titles = ["Event occurrences", "Prevalence/Enrichment", "Age", "Male %"]
    
    # Add cluster column titles if clustering is available
    if cluster_data_available:
        # Get cluster counts for titles - check for pre-computed counts first (summary mode)
        cluster_counts_for_titles = {}
        if clustering_results:
            # Check for pre-computed cluster counts (summary mode)
            if 'cluster_counts' in clustering_results and clustering_results['cluster_counts']:
                cluster_counts_for_titles = clustering_results['cluster_counts']
            else:
                # Calculate from patient assignments (patient mode)
                patient_assignments_data = clustering_results.get('patient_assignments', [])
                if isinstance(patient_assignments_data, list):
                    patient_assignments_df = pd.DataFrame(patient_assignments_data) if patient_assignments_data else pd.DataFrame()
                else:
                    patient_assignments_df = patient_assignments_data if isinstance(patient_assignments_data, pd.DataFrame) else pd.DataFrame()
                
                if not patient_assignments_df.empty and 'cluster' in patient_assignments_df.columns:
                    cluster_counts_for_titles = patient_assignments_df['cluster'].value_counts().to_dict()
        
        cluster_titles = []
        for i in range(optimal_k):
            cluster_label = f'C{i+1}'
            count = cluster_counts_for_titles.get(cluster_label, 0)
            cluster_titles.append(f'{cluster_label} (n={count})')
        
        col_titles = base_col_titles + cluster_titles
    else:
        col_titles = base_col_titles
    
    # Calculate x positions for all columns based on column widths
    # Each position should be at the center of its column
    total_width = sum(column_widths)
    col_x_positions = []
    cumulative_width = 0
    for width in column_widths:
        center = (cumulative_width + width / 2) / total_width
        col_x_positions.append(center)
        cumulative_width += width
    
    for col_idx, (title, x_pos) in enumerate(zip(col_titles, col_x_positions), start=1):
        fig_combined.add_annotation(
            text=title,
            xref="paper",
            yref="paper",
            x=x_pos,
            y=1.0,  # At the top of the paper, within the margin
            showarrow=False,
            font=dict(size=10 if cluster_data_available else 12, color="black"),
            xanchor="center",
            yanchor="bottom"  # Anchor from bottom so text sits above
        )
    
    # Update x-axes for each column - apply shared ranges to all subplots
    for row_idx in range(1, num_rows + 1):
        # Column 1: Composite plot (Time to Event) - shared x-axis range
        fig_combined.update_xaxes(
            range=[time_x_min, time_x_max] if time_x_min is not None and time_x_max is not None else None,
            title_text="Time to Event (days)" if row_idx == num_rows else "",
            fixedrange=True,
            tickangle=0,  # Force horizontal labels
            row=row_idx,
            col=1
        )
        
        # Column 2: Prevalence plot - shared x-axis range (0-1 for 0-100%)
        fig_combined.update_xaxes(
            range=[prevalence_x_min, prevalence_x_max],
            title_text="Prevalence" if row_idx == num_rows else "",
            tickformat=".0%",
            fixedrange=True,
            tickangle=0,  # Force horizontal labels
            row=row_idx,
            col=2
        )
        
        # Column 3: Age plot - shared x-axis range
        fig_combined.update_xaxes(
            range=[age_x_min, age_x_max] if age_x_min is not None and age_x_max is not None else None,
            title_text="Age (years)" if row_idx == num_rows else "",
            fixedrange=True,
            tickangle=0,  # Force horizontal labels (same as patient mode)
            row=row_idx,
            col=3
        )
        
        # Column 4: Male proportion plot - shared x-axis range
        if male_prop_x_min is not None and male_prop_x_max is not None:
            fig_combined.update_xaxes(
                range=[male_prop_x_min, male_prop_x_max],
                title_text="Male Proportion" if row_idx == num_rows else "",
                tickformat=".0%",
                fixedrange=True,
                tickangle=0,  # Force horizontal labels (same as patient mode)
                row=row_idx,
                col=4
            )
        else:
            fig_combined.update_xaxes(
                range=[0.0, 1.0],
                title_text="Male Proportion" if row_idx == num_rows else "",
                tickformat=".0%",
                fixedrange=True,
                tickangle=0,  # Force horizontal labels (same as patient mode)
                row=row_idx,
                col=4
            )
        
        # Cluster columns (5+): heatmap cells - hide x-axis
        if cluster_data_available:
            for cluster_col_idx in range(optimal_k):
                col_idx = base_cols + cluster_col_idx + 1
                fig_combined.update_xaxes(
                    showticklabels=False,
                    showgrid=False,
                    zeroline=False,
                    fixedrange=True,
                    row=row_idx,
                    col=col_idx
                )
    
    # Add vertical lines for overall average male proportion in column 4
    if overall_avg_male_prop is not None:
        for row_idx, heritage in enumerate(heritages_with_data, start=1):
            num_concepts = len(heritage_groups_order[heritage])
            fig_combined.add_shape(
                type="line",
                x0=overall_avg_male_prop,
                x1=overall_avg_male_prop,
                y0=-0.5,
                y1=num_concepts - 0.5,
                line=dict(color="gray", width=1, dash="dot"),
                layer="below",
                row=row_idx,
                col=4
            )
    
    # Update y-axes - use composite plot's y-axis settings for all columns
    for row_idx in range(1, num_rows + 1):
        # Get y-axis settings from composite plot
        yaxis_key = f'yaxis{row_idx if row_idx > 1 else ""}'
        yaxis_layout = getattr(fig_composite.layout, yaxis_key, None) if hasattr(fig_composite.layout, yaxis_key) else None
        
        tickvals = yaxis_layout.tickvals if yaxis_layout and hasattr(yaxis_layout, 'tickvals') else None
        ticktext = yaxis_layout.ticktext if yaxis_layout and hasattr(yaxis_layout, 'ticktext') else None
        y_range = yaxis_layout.range if yaxis_layout and hasattr(yaxis_layout, 'range') else None
        
        # Column 1: show labels
        fig_combined.update_yaxes(
            tickmode='array',
            tickvals=tickvals,
            ticktext=ticktext,
            range=y_range,
            fixedrange=True,
            row=row_idx,
            col=1
        )
        
        # Column 2: no labels
        fig_combined.update_yaxes(
            tickmode='array',
            tickvals=tickvals,
            ticktext=[""] * len(ticktext) if ticktext else [],
            range=y_range,
            showticklabels=False,
            showgrid=False,
            fixedrange=True,
            row=row_idx,
            col=2
        )
        
        # Column 3: no labels (age plot)
        fig_combined.update_yaxes(
            tickmode='array',
            tickvals=tickvals,
            ticktext=[""] * len(ticktext) if ticktext else [],
            range=y_range,
            showticklabels=False,
            showgrid=False,
            fixedrange=True,
            row=row_idx,
            col=3
        )
        
        # Column 4: no labels (male proportion plot)
        fig_combined.update_yaxes(
            tickmode='array',
            tickvals=tickvals,
            ticktext=[""] * len(ticktext) if ticktext else [],
            range=y_range,
            showticklabels=False,
            showgrid=False,
            fixedrange=True,
            row=row_idx,
            col=4
        )
        
        # Cluster columns (5+): no labels
        if cluster_data_available:
            for cluster_col_idx in range(optimal_k):
                col_idx = base_cols + cluster_col_idx + 1
                fig_combined.update_yaxes(
                    tickmode='array',
                    tickvals=tickvals,
                    ticktext=[""] * len(ticktext) if ticktext else [],
                    range=y_range,
                    showticklabels=False,
                    showgrid=False,
                    fixedrange=True,
                    row=row_idx,
                    col=col_idx
                )
    
    # Add vertical lines for overall average age in column 3
    if overall_avg_age is not None:
        for row_idx, heritage in enumerate(heritages_with_data, start=1):
            num_concepts = len(heritage_groups_order[heritage])
            fig_combined.add_shape(
                type="line",
                x0=overall_avg_age,
                x1=overall_avg_age,
                y0=-0.5,
                y1=num_concepts - 0.5,
                line=dict(color="gray", width=1, dash="dot"),
                layer="below",
                row=row_idx,
                col=3
            )
    
    # Calculate container height based on plot height
    plot_height = fig_combined.layout.height if hasattr(fig_combined.layout, 'height') and fig_combined.layout.height else 400
    
    # Set explicit height but allow width to adapt
    fig_combined.update_layout(
        autosize=True,
        height=plot_height
    )
    
    container_style = {
        "width": "100%",
        "marginBottom": "60px",
        "overflow": "visible",
        "height": f"{plot_height}px"
    }
    
    return fig_combined, container_style




@app.callback(
    Output("cluster-view-selector", "options"),
    Input("clustering-results-store", "data")
)
def update_cluster_view_options(clustering_results: Optional[Dict]) -> List[Dict]:
    """
    Update the cluster view selector options based on clustering results.
    """
    # Always include "All" option
    options = [{"label": "All", "value": "all"}]
    
    if clustering_results:
        optimal_k = clustering_results.get('optimal_cluster_count', 0)
        patient_assignments = clustering_results.get('patient_assignments', [])
        
        # Get cluster counts - check for pre-computed counts first (summary mode)
        cluster_counts = {}
        if 'cluster_counts' in clustering_results and clustering_results['cluster_counts']:
            cluster_counts = clustering_results['cluster_counts']
        elif patient_assignments:
            import pandas as pd
            if isinstance(patient_assignments, list):
                df = pd.DataFrame(patient_assignments)
            else:
                df = patient_assignments
            if not df.empty and 'cluster' in df.columns:
                cluster_counts = df['cluster'].value_counts().to_dict()
        
        # Add cluster options with counts
        for i in range(optimal_k):
            cluster_label = f'C{i + 1}'
            count = cluster_counts.get(cluster_label, 0)
            options.append({
                "label": f"Cluster {i + 1} (n={count})",
                "value": cluster_label
            })
    
    return options


@app.callback(
    Output("silhouette-score-display", "children"),
    Input("clustering-results-store", "data"),
    prevent_initial_call=True
)
def update_silhouette_display(clustering_results: Optional[Dict]) -> str:
    """Update the silhouette score display."""
    if not clustering_results:
        return ""
    
    silhouette_score = clustering_results.get('best_silhouette_score')
    optimal_k = clustering_results.get('optimal_cluster_count', 0)
    
    if silhouette_score is not None and optimal_k > 0:
        score_str = f"Silhouette: {silhouette_score:.3f}"
        if silhouette_score >= 0.5:
            quality = "(strong)"
        elif silhouette_score >= 0.25:
            quality = "(reasonable)"
        else:
            quality = "(weak)"
        return f"| {optimal_k} clusters | {score_str} {quality}"
    
    return ""


@app.callback(
    Output("heritage-checkboxes", "children"),
    Input("dashboard-data-store", "data")
)
def populate_heritage_checkboxes(dashboard_data: Optional[List[Dict]]) -> List:
    """
    Populate heritage checkboxes based on available heritages in dashboard data.
    
    Args:
        dashboard_data: Full dashboard data from store
        
    Returns:
        List of checkbox components
    """
    if not dashboard_data:
        return []
    
    # Extract unique heritages
    heritages = set()
    for row in dashboard_data:
        heritage = row.get("HERITAGE")
        if heritage is not None and pd.notna(heritage) and str(heritage).strip():
            heritages.add(str(heritage))
    
    heritages = sorted(list(heritages))
    
    if not heritages:
        return [html.P("No heritage types available", style={"color": "#999", "fontSize": "12px", "padding": "5px"})]
    
    # Default heritage selections: procedures, measurements, drugs
    default_heritages = ["procedure_occurrence", "measurement", "drug_exposure"]
    
    checkboxes = []
    for heritage in heritages:
        # Use formatted label for display, but keep original value for filtering
        formatted_label = format_heritage_label(heritage)
        # Check if this heritage should be selected by default
        is_default = heritage in default_heritages
        checkboxes.append(
            html.Div([
                dcc.Checklist(
                    id={"type": "heritage-checkbox", "index": heritage},
                    options=[{"label": formatted_label, "value": heritage}],
                    value=[heritage] if is_default else [],  # Only default heritages checked
                    style={"display": "inline-block", "marginRight": "10px"}
                )
            ], style={"marginBottom": "8px", "padding": "5px", "backgroundColor": "#fff", "borderRadius": "4px"})
        )
    
    return checkboxes


@app.callback(
    Output("target-prevalence-display", "children"),
    Input("target-prevalence-range", "value")
)
def update_target_prevalence_display(value: List[float]) -> str:
    """Display the selected target prevalence range."""
    if value and len(value) == 2:
        return f"{value[0]:.1f}% - {value[1]:.1f}%"
    return ""


@app.callback(
    Output("ratio-display", "children"),
    Input("ratio-range", "value")
)
def update_ratio_display(value: List[float]) -> str:
    """Display the selected ratio range."""
    if value and len(value) == 2:
        return f"{value[0]:.1f} - {value[1]:.1f}"
    return ""


@app.callback(
    Output("cluster-prevalence-display", "children"),
    [Input("cluster-prevalence-slider", "value"),
     Input("cluster-view-selector", "value")]
)
def update_cluster_prevalence_display(value: int, selected_cluster: Optional[str]) -> str:
    """Display the selected cluster prevalence threshold and whether it's active."""
    if value is None:
        value = 0
    if selected_cluster and selected_cluster != "all":
        return f"≥ {value}% (Active for Cluster {selected_cluster})"
    return f"≥ {value}% (Inactive - select a specific cluster)"


# Pattern matching callback to collect all heritage checkbox values
@app.callback(
    Output("heritage-selection-store", "data"),
    Input({"type": "heritage-checkbox", "index": dash.ALL}, "value"),
    prevent_initial_call=False
)
def collect_heritage_selections(heritage_values: List[List[str]]) -> List[str]:
    """
    Collect all selected heritages from checkboxes.
    
    Args:
        heritage_values: List of values from all heritage checkboxes
        
    Returns:
        List of selected heritage strings
    """
    if not heritage_values:
        return []
    
    selected = []
    for value_list in heritage_values:
        if value_list:
            selected.extend(value_list)
    return selected


# Clientside callback to resize plot when switching to dashboard tab
clientside_callback(
    """
    function(tabValue) {
        if (tabValue === "dashboard") {
            // Small delay to ensure tab content is visible before resizing
            setTimeout(function() {
                var plotDiv = document.getElementById("composite-plot");
                if (plotDiv && window.Plotly) {
                    window.Plotly.Plots.resize(plotDiv);
                }
            }, 100);
        }
        return window.dash_clientside.no_update;
    }
    """,
    Output("composite-plot", "id"),  # Dummy output (returns no_update)
    Input("main-tabs", "value")
)

# Clientside callback to update loading message display
clientside_callback(
    """
    function(message) {
        const baseStyle = {
            "position": "fixed",
            "top": "60%",
            "left": "50%",
            "transform": "translateX(-50%)",
            "zIndex": "10001",
            "color": "#2c3e50",
            "fontSize": "18px",
            "fontWeight": "500",
            "textAlign": "center",
            "pointerEvents": "none"
        };
        
        if (message && message.trim() !== "") {
            return [message, {...baseStyle, "display": "block"}];
        }
        return ["", {...baseStyle, "display": "none"}];
    }
    """,
    [Output("loading-message-display", "children"),
     Output("loading-message-display", "style")],
    Input("loading-message-store", "data")
)


# Overlap tab callbacks
@app.callback(
    Output("overlap-group-selector", "options"),
    Input("clustering-results-store", "data"),
    prevent_initial_call=False
)
def update_overlap_group_options(clustering_results: Optional[Dict]):
    """Update the group selector options based on clustering results."""
    options = [{"label": "Overall", "value": "overall"}]
    
    if clustering_results:
        optimal_k = clustering_results.get('optimal_k', 0)
        # Also try 'n_clusters' and 'optimal_cluster_count' keys
        if optimal_k == 0:
            optimal_k = clustering_results.get('n_clusters', 0)
        if optimal_k == 0:
            optimal_k = clustering_results.get('optimal_cluster_count', 0)
        
        # First try to get cluster_counts directly (works for both patient and summary mode)
        cluster_counts = clustering_results.get('cluster_counts', {})
        
        if cluster_counts:
            # Use pre-computed cluster counts (summary mode or patient mode with counts)
            # Sort clusters by name (C1, C2, etc.)
            sorted_clusters = sorted(cluster_counts.keys(), key=lambda x: int(x[1:]) if x.startswith('C') and x[1:].isdigit() else 999)
            for cluster_label in sorted_clusters:
                count = cluster_counts.get(cluster_label, 0)
                cluster_num = cluster_label[1:] if cluster_label.startswith('C') else cluster_label
                options.append({
                    "label": f"Cluster {cluster_num} (n={count})",
                    "value": f"cluster_{cluster_num}"
                })
        else:
            # Fallback: try to get from patient_assignments (patient mode only)
            patient_assignments_data = clustering_results.get('patient_assignments', [])
            
            if isinstance(patient_assignments_data, list):
                patient_assignments = pd.DataFrame(patient_assignments_data) if patient_assignments_data else pd.DataFrame()
            else:
                patient_assignments = patient_assignments_data
            
            # If optimal_k is still 0, try to infer from patient_assignments
            if optimal_k == 0 and not patient_assignments.empty and 'cluster' in patient_assignments.columns:
                unique_clusters = patient_assignments['cluster'].unique()
                optimal_k = len(unique_clusters)
            
            if not patient_assignments.empty and optimal_k > 0:
                # Count patients per cluster
                cluster_counts = patient_assignments['cluster'].value_counts().to_dict()
                
                for i in range(optimal_k):
                    cluster_label = f"C{i+1}"
                    count = cluster_counts.get(cluster_label, 0)
                    options.append({
                        "label": f"Cluster {i+1} (n={count})",
                        "value": f"cluster_{i+1}"
                    })
    
    return options


@app.callback(
    Output("overlap-plot-container", "children"),
    [Input("clustering-results-store", "data"),
     Input("update-plots-btn", "n_clicks"),
     Input("plots-update-trigger-store", "data"),
     Input("overlap-group-selector", "value"),
     Input("overlap-metric-selector", "value")],
    [State("selected-study-store", "data"),
     State("dashboard-table", "rowData"),
     State("data-mode-store", "data")],
    prevent_initial_call=True
)
def update_overlap_plot(
    clustering_results: Optional[Dict],
    update_clicks: Optional[int],
    plots_trigger: Optional[float],
    selected_group: str,
    metric: str,
    selected_study: Optional[str],
    row_data: Optional[List[Dict]],
    data_mode: Optional[str]
):
    """Update the overlap plot when parameters change.
    
    In summary mode, uses pre-computed pairwise overlap data.
    In patient mode, computes overlap from patient-level data.
    """
    if not clustering_results or not selected_study:
        return html.P("Waiting for clustering results...", 
                      style={"color": "#999", "textAlign": "center", "padding": "50px"})
    
    # Get data
    if selected_study not in loaded_parquet_data:
        return html.P("Study data not loaded.", 
                      style={"color": "#999", "textAlign": "center", "padding": "50px"})
    
    parquet_data = loaded_parquet_data[selected_study]
    actual_data_mode = parquet_data.get("_mode", "patient")
    
    # Get active concepts from row_data
    active_concepts = set()
    heritage_groups_order = {}
    
    if row_data:
        for row in row_data:
            if row.get("_show", False):
                concept_id = row.get("_concept_id") or row.get("CONCEPT_ID")
                if concept_id:
                    norm_id = _normalize_concept_id(concept_id)
                    active_concepts.add(norm_id)
                    
                    heritage = row.get("HERITAGE", "unknown")
                    if heritage not in heritage_groups_order:
                        heritage_groups_order[heritage] = []
                    heritage_groups_order[heritage].append(row)
    
    if not active_concepts:
        return html.P("No active concepts selected.", 
                      style={"color": "#999", "textAlign": "center", "padding": "50px"})
    
    try:
        # Summary mode: use pre-computed pairwise overlap data
        if actual_data_mode == "summary":
            # Get the k value from clustering results
            optimal_k = clustering_results.get("optimal_k") or clustering_results.get("optimal_cluster_count", 4)
            
            # Load the pairwise overlap data for this k
            pairwise_overlap_key = f"clustering_k{optimal_k}_pairwise_overlap"
            pairwise_overlap_df = parquet_data.get(pairwise_overlap_key, pd.DataFrame())
            
            if pairwise_overlap_df.empty:
                return html.P(f"No pre-computed overlap data available for k={optimal_k}. Please re-run precomputation.", 
                              style={"color": "#999", "textAlign": "center", "padding": "50px"})
            
            # Map selected_group format: "overall" or "cluster_1" -> "overall" or "C1"
            if selected_group and selected_group.startswith("cluster_"):
                cluster_num = selected_group.split("_")[1]
                group_filter = f"C{cluster_num}"
            else:
                group_filter = "overall"
            
            result = create_overlap_plot_from_summary(
                clustering_results=clustering_results,
                pairwise_overlap_df=pairwise_overlap_df,
                heritage_groups_order=heritage_groups_order,
                active_concept_ids=active_concepts,
                selected_group=group_filter
            )
        else:
            # Patient mode: compute from patient data
            data_patients = parquet_data.get("data_patients", pd.DataFrame())
            
            if data_patients.empty:
                return html.P("No patient data available.", 
                              style={"color": "#999", "textAlign": "center", "padding": "50px"})
            
            result = create_overlap_plot(
                clustering_results=clustering_results,
                data_patients=data_patients,
                heritage_groups_order=heritage_groups_order,
                active_concept_ids=active_concepts,
                selected_group=selected_group or "overall",
                metric=metric or "jaccard"
            )
        
        # Handle both old (just fig) and new (fig, pairwise_data) return formats
        if isinstance(result, tuple):
            fig, pairwise_data = result
        else:
            fig = result
            pairwise_data = []
        
        # Create table from pairwise data - use simple dash_table instead of AgGrid
        if pairwise_data and len(pairwise_data) > 0:
            table_component = dash_table.DataTable(
                id="overlap-table",
                columns=[
                    {"name": "Concept 1", "id": "Concept 1"},
                    {"name": "Concept 2", "id": "Concept 2"},
                    {"name": "Correlation", "id": "Correlation", "type": "numeric", "format": {"specifier": ".3f"}},
                    {"name": "Jaccard", "id": "Jaccard", "type": "numeric", "format": {"specifier": ".3f"}},
                    {"name": "Co-occur (n)", "id": "Co-occur", "type": "numeric"},
                    {"name": "Total (n)", "id": "Total", "type": "numeric"},
                ],
                data=pairwise_data,
                sort_action="native",
                filter_action="native",
                page_action="native",
                page_size=15,
                style_table={"overflowX": "auto"},
                style_cell={"textAlign": "left", "padding": "8px", "fontSize": "12px"},
                style_header={"fontWeight": "bold", "backgroundColor": "#f8f9fa"},
                style_data_conditional=[
                    {"if": {"filter_query": "{Correlation} > 0.3", "column_id": "Correlation"},
                     "backgroundColor": "rgba(33, 102, 172, 0.3)"},
                    {"if": {"filter_query": "{Correlation} < -0.3", "column_id": "Correlation"},
                     "backgroundColor": "rgba(178, 24, 43, 0.3)"},
                    {"if": {"filter_query": "{Jaccard} > 0.5", "column_id": "Jaccard"},
                     "backgroundColor": "rgba(33, 102, 172, 0.3)"},
                ]
            )
        else:
            table_component = html.P("No pairwise data available")
        
        return html.Div([
            dcc.Graph(
                figure=fig,
                config={'displayModeBar': True, 'scrollZoom': False},
                style={"width": "100%"}
            ),
            html.Hr(style={"margin": "20px 0"}),
            html.H5("Pairwise Concept Relationships", style={"marginBottom": "10px", "color": "#2c3e50"}),
            html.P("Sorted by absolute correlation (strongest first)", style={"color": "#666", "fontSize": "12px", "marginBottom": "10px"}),
            table_component
        ])
    except Exception as e:
        import traceback
        traceback.print_exc()
        return html.P(f"Error generating overlap plot: {str(e)}", 
                      style={"color": "#d62728", "textAlign": "center", "padding": "50px"})


@app.callback(
    Output("trajectories-plot-container", "children"),
    [Input("clustering-results-store", "data"),
     Input("update-plots-btn", "n_clicks"),
     Input("plots-update-trigger-store", "data"),
     Input("cluster-view-selector", "value")],
    [State("selected-study-store", "data"),
     State("dashboard-table", "rowData"),
     State("data-mode-store", "data"),
     State("cluster-prevalence-slider", "value")],
    prevent_initial_call=True
)
def update_trajectory_plot(
    clustering_results: Optional[Dict],
    update_clicks: Optional[int],
    plots_trigger: Optional[float],
    selected_cluster: Optional[str],
    selected_study: Optional[str],
    row_data: Optional[List[Dict]],
    data_mode: Optional[str],
    cluster_prevalence_threshold: int
):
    """
    Update the trajectory plot when clustering results change or concepts are updated.
    cluster_prevalence_threshold: Minimum cluster prevalence percentage to show concept (0-100)
        Only applies when a specific cluster is selected (not "all") and Apply Filters is pressed.
    
    In summary mode, uses pre-computed clustering summary matrix.
    In patient mode, uses patient-level data.
    """
    if not clustering_results or not selected_study:
        return html.P("Waiting for clustering results...", 
                      style={"color": "#999", "textAlign": "center", "padding": "50px"})
    
    # Get data
    if selected_study not in loaded_parquet_data:
        return html.P("Study data not loaded.", 
                      style={"color": "#999", "textAlign": "center", "padding": "50px"})
    
    parquet_data = loaded_parquet_data[selected_study]
    actual_data_mode = parquet_data.get("_mode", "patient")
    
    # Get active concepts from row_data
    active_concepts = set()
    heritage_groups_order = {}
    
    if row_data:
        for row in row_data:
            if row.get("_show", False):
                concept_id = row.get("_concept_id") or row.get("CONCEPT_ID")
                if concept_id:
                    norm_id = _normalize_concept_id(concept_id)
                    active_concepts.add(norm_id)
                    
                    heritage = row.get("HERITAGE", "unknown")
                    if heritage not in heritage_groups_order:
                        heritage_groups_order[heritage] = []
                    heritage_groups_order[heritage].append(row)
    
    if not active_concepts:
        return html.P("No active concepts selected.", 
                      style={"color": "#999", "textAlign": "center", "padding": "50px"})
    
    try:
        # Determine the effective min_prevalence_pct
        # Only apply cluster prevalence filter when a specific cluster is selected (not "all")
        if selected_cluster and selected_cluster != "all":
            min_prevalence_pct = cluster_prevalence_threshold or 0
        else:
            min_prevalence_pct = 0  # No filtering when viewing all clusters
        
        # SUMMARY MODE: Use pre-computed clustering summary
        if actual_data_mode == "summary":
            # Get the clustering summary matrix for the current k
            k_value = clustering_results.get('optimal_cluster_count', 3)
            summary_key = f"clustering_k{k_value}_summary"
            clustering_summary_matrix = parquet_data.get(summary_key, pd.DataFrame())
            
            if clustering_summary_matrix.empty:
                return html.P("No pre-computed clustering data available.", 
                              style={"color": "#999", "textAlign": "center", "padding": "50px"})
            
            fig = create_trajectory_plot_from_summary(
                clustering_results=clustering_results,
                clustering_summary_matrix=clustering_summary_matrix,
                heritage_groups_order=heritage_groups_order,
                active_concept_ids=active_concepts,
                min_prevalence_pct=min_prevalence_pct
            )
        else:
            # PATIENT MODE: Use patient-level data
            data_patients = parquet_data.get("data_patients", pd.DataFrame())
            
            if data_patients.empty:
                return html.P("No patient data available.", 
                              style={"color": "#999", "textAlign": "center", "padding": "50px"})
            
            fig = create_trajectory_plot(
                clustering_results=clustering_results,
                data_patients=data_patients,
                heritage_groups_order=heritage_groups_order,
                active_concept_ids=active_concepts,
                min_prevalence_pct=min_prevalence_pct
            )
        
        return dcc.Graph(
            figure=fig,
            config={'displayModeBar': True, 'scrollZoom': False},
            style={"width": "100%"}
        )
    except Exception as e:
        import traceback
        traceback.print_exc()
        return html.P(f"Error generating trajectory plot: {str(e)}", 
                      style={"color": "#d62728", "textAlign": "center", "padding": "50px"})


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
    """
    Update the demographics plots when study or clustering changes.
    
    Shows age distribution, sex distribution, and index date distribution.
    Works with both patient-level data and pre-computed summary data.
    """
    if not selected_study:
        return html.P("Select a study to view demographics.", 
                      style={"color": "#999", "textAlign": "center", "padding": "50px"})
    
    if selected_study not in loaded_parquet_data:
        return html.P("Study data not loaded.", 
                      style={"color": "#999", "textAlign": "center", "padding": "50px"})
    
    try:
        parquet_data = loaded_parquet_data[selected_study]
        actual_data_mode = parquet_data.get("_mode", "patient")
        
        if actual_data_mode == "summary":
            # Summary mode: use pre-computed demographics from metadata
            # Note: metadata is stored as "_metadata" in the loader
            metadata = parquet_data.get("_metadata", {})
            
            if not metadata:
                # Fallback: try without underscore
                metadata = parquet_data.get("metadata", {})
            
            # Check if we have demographic data
            demo = metadata.get("demographics", {})
            distributions = demo.get("distributions", {})
            
            if not distributions and not demo.get("age_mean"):
                return html.Div([
                    html.P("Demographics data not available in summary mode.", 
                           style={"color": "#999", "textAlign": "center", "padding": "30px"}),
                    html.P("To generate demographic summaries, re-run the precompute script with updated code.", 
                           style={"color": "#666", "textAlign": "center", "fontSize": "12px"})
                ])
            
            # Create plot from summary data
            fig = create_demographics_plots_from_summary(metadata, clustering_results=clustering_results)
            
            return dcc.Graph(
                figure=fig,
                style={"width": "100%", "height": "450px"},
                config={"displayModeBar": True}
            )
        else:
            # Patient mode: use patient-level data
            data_person = parquet_data.get("data_person")
            data_initial = parquet_data.get("data_initial")
            data_patients = parquet_data.get("data_patients")
            
            if data_person is None or data_person.empty:
                return html.P("Patient demographics data (data_person.parquet) not available.", 
                              style={"color": "#999", "textAlign": "center", "padding": "50px"})
            
            if data_initial is None or data_initial.empty:
                return html.P("Cohort data (data_initial.parquet) not available.", 
                              style={"color": "#999", "textAlign": "center", "padding": "50px"})
            
            if data_patients is None or data_patients.empty:
                return html.P("Patient-concept data (data_patients.parquet) not available.", 
                              style={"color": "#999", "textAlign": "center", "padding": "50px"})
            
            # Create plot from patient data
            fig = create_demographics_plots_from_patient_data(
                data_person=data_person,
                data_initial=data_initial,
                data_patients=data_patients,
                clustering_results=clustering_results
            )
            
            return dcc.Graph(
                figure=fig,
                style={"width": "100%", "height": "450px"},
                config={"displayModeBar": True}
            )
    
    except Exception as e:
        import traceback
        traceback.print_exc()
        return html.P(f"Error generating demographics plots: {str(e)}", 
                      style={"color": "#d62728", "textAlign": "center", "padding": "50px"})


if __name__ == "__main__":
    if study_summaries_df.empty:
        print("Warning: No studies found. Please check the results_parquet/ folder.")
    else:
        print(f"Found {len(study_summaries_df)} study(ies): {', '.join(study_summaries_df['study'].tolist())}")
    
    # Configuration from environment variables (for R package integration)
    port = int(os.environ.get("CONTRAST_VIEWER_PORT", 8050))
    host = os.environ.get("CONTRAST_VIEWER_HOST", "127.0.0.1")
    debug = os.environ.get("CONTRAST_VIEWER_DEBUG", "0") == "1"
    
    print(f"Starting ContrastViewer on http://{host}:{port}")
    app.run(host=host, port=port, debug=debug)
