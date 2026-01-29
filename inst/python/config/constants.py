"""
Constants and configuration values for ContrastViewer.
"""

import numpy as np
from pathlib import Path

# =============================================================================
# Heritage Configuration
# =============================================================================

HERITAGE_ORDER = [
    "condition_occurrence",
    "drug_exposure",
    "procedure_occurrence",
    "measurement",
    "observation",
    "visit_occurrence",
    "visit_detail",
    "death"
]

HERITAGE_COLORS = {
    "condition_occurrence": "#FFB347",
    "drug_exposure": "#B39EB5",
    "procedure_occurrence": "#7FBFEA",  # Lighter blue for better text contrast
    "measurement": "#FFB3AB",
    "observation": "#77DD77",
    "visit_occurrence": "#FDFD96",
    "visit_detail": "#AEC6CF",
    "death": "#3A3B3C"
}

HERITAGE_LABELS = {
    "procedure_occurrence": "Procedures",
    "condition_occurrence": "Conditions",
    "drug_exposure": "Drug Exposures",
    "measurement": "Measurements",
    "observation": "Observations",
    "device_exposure": "Device Exposures",
    "visit_occurrence": "Visits",
    "visit_detail": "Visit Details",
    "specimen": "Specimens",
    "note": "Notes",
    "note_nlp": "Note NLP",
    "death": "Deaths",
    "payer_plan_period": "Payer Plan Periods",
    "cost": "Costs",
    "episode": "Episodes",
    "metadata": "Metadata"
}

# =============================================================================
# Enrichment Display
# =============================================================================

MAX_ENRICHMENT_VALUE = 100
MAX_ENRICHMENT_LOG = np.log10(MAX_ENRICHMENT_VALUE)

# =============================================================================
# Plot Layout Constants
# =============================================================================

PIXELS_PER_CONCEPT = 30
MIN_PLOT_AREA = 200
TOP_MARGIN = 25
BOTTOM_MARGIN = 25
SMALL_GAP = 60  # Gap between heritage groups

# Combined plot specific
TOP_MARGIN_COMBINED = 60
BOTTOM_MARGIN_COMBINED = 25
SMALL_GAP_COMBINED = 250  # Large gap between heritage groups to prevent overlap
PIXELS_PER_CONCEPT_COMBINED = 30
MIN_PLOT_AREA_COMBINED = 200

# Subplot spacing
VERTICAL_SPACING = 0.03  # Large gap between heritage groups
HORIZONTAL_SPACING = 0.02  # Gap between columns

# Minimum plot height
MIN_PLOT_HEIGHT = 200

# =============================================================================
# Data Paths
# =============================================================================

def get_data_dir() -> Path:
    """
    Get the data directory from environment variable or default location.
    
    Priority:
    1. CONTRAST_VIEWER_DATA_DIR environment variable
    2. results_parquet/ in package directory
    3. results_parquet/ in current working directory
    """
    import os
    
    # Check environment variable first
    env_dir = os.environ.get("CONTRAST_VIEWER_DATA_DIR")
    if env_dir and Path(env_dir).exists():
        return Path(env_dir)
    
    # Check relative to package
    pkg_data = Path(__file__).parent.parent / "results_parquet"
    if pkg_data.exists():
        return pkg_data
    
    # Check current working directory
    cwd_data = Path.cwd() / "results_parquet"
    if cwd_data.exists():
        return cwd_data
    
    # Return package location as default (will be created if needed)
    return pkg_data

# For backwards compatibility
DATA_DIR = get_data_dir()

# =============================================================================
# Gender Constants
# =============================================================================

MALE_GENDER_CONCEPT_ID = 8507
FEMALE_GENDER_CONCEPT_ID = 8532

# =============================================================================
# Cluster Colors
# =============================================================================

CLUSTER_COLORS = [
    '#1f77b4',  # Blue
    '#ff7f0e',  # Orange
    '#2ca02c',  # Green
    '#d62728',  # Red
    '#9467bd',  # Purple
]

# =============================================================================
# UI Constants
# =============================================================================

DEFAULT_PREVALENCE_THRESHOLD = 0.01  # 1%
DEFAULT_RATIO_THRESHOLD = 1.0

