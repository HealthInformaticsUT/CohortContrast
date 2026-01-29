"""
Helper utility functions for ContrastViewer.
"""

from typing import Dict, Any, List, Union, Tuple, Optional
import pandas as pd
import numpy as np
import plotly.graph_objects as go

from config.constants import HERITAGE_LABELS


# =============================================================================
# Empty Figure Helpers
# =============================================================================

def create_empty_figure(message: str = "No data available.") -> go.Figure:
    """
    Create an empty Plotly figure with a centered message.
    
    Args:
        message: Text to display in the center of the figure
        
    Returns:
        Empty Plotly figure with annotation
    """
    fig = go.Figure()
    fig.add_annotation(
        text=message,
        xref="paper", yref="paper",
        x=0.5, y=0.5, showarrow=False,
        font=dict(size=16, color="#666")
    )
    fig.update_layout(
        xaxis=dict(visible=False),
        yaxis=dict(visible=False),
        plot_bgcolor="white"
    )
    return fig


def create_empty_figure_with_style(
    message: str = "No data available.",
    height: int = 400
) -> Tuple[go.Figure, Dict]:
    """
    Create an empty Plotly figure with a centered message and container style.
    
    Args:
        message: Text to display in the center of the figure
        height: Height in pixels for the container
        
    Returns:
        Tuple of (empty figure, container style dict)
    """
    fig = create_empty_figure(message)
    container_style = {
        "width": "100%",
        "marginBottom": "60px",
        "overflow": "visible",
        "height": f"{height}px"
    }
    return fig, container_style


def get_default_container_style(height: int = 400) -> Dict:
    """
    Get the default container style for plot containers.
    
    Args:
        height: Height in pixels
        
    Returns:
        Container style dictionary
    """
    return {
        "width": "100%",
        "marginBottom": "60px",
        "overflow": "visible",
        "height": f"{height}px"
    }


# =============================================================================
# Cluster Prevalence Filtering
# =============================================================================

def build_cluster_prevalence_lookup(
    clustering_summary_matrix: pd.DataFrame,
    selected_cluster: str
) -> Dict[str, float]:
    """
    Build a lookup dictionary of concept prevalence for a specific cluster.
    
    Args:
        clustering_summary_matrix: DataFrame with cluster summary data
        selected_cluster: The cluster ID (e.g., "C1", "C2")
        
    Returns:
        Dictionary mapping normalized concept IDs to prevalence percentages
    """
    lookup = {}
    for _, row in clustering_summary_matrix.iterrows():
        if row.get("cluster") == selected_cluster:
            cid = row.get("CONCEPT_ID")
            prevalence_pct = row.get("prevalence", 0) * 100  # Convert to percentage
            
            # Store with multiple key formats for robust lookup
            norm_id = normalize_concept_id(cid)
            lookup[norm_id] = prevalence_pct
            lookup[str(cid)] = prevalence_pct
            if isinstance(cid, (int, float)):
                try:
                    lookup[int(cid)] = prevalence_pct
                except (ValueError, TypeError):
                    pass
    return lookup


def filter_concepts_by_cluster_prevalence(
    concepts: List[Dict],
    cluster_prevalence_lookup: Dict[str, float],
    threshold: float
) -> List[Dict]:
    """
    Filter concepts based on cluster prevalence threshold.
    
    Args:
        concepts: List of concept dictionaries
        cluster_prevalence_lookup: Dict mapping concept IDs to prevalence %
        threshold: Minimum prevalence percentage to include
        
    Returns:
        Filtered list of concepts meeting the threshold
    """
    filtered = []
    for concept in concepts:
        concept_id = concept.get("_concept_id") or concept.get("CONCEPT_ID")
        concept_prev = 0
        
        if concept_id:
            # Try normalized ID first
            norm_id = normalize_concept_id(concept_id)
            concept_prev = cluster_prevalence_lookup.get(norm_id, 0)
            
            # If not found, try string version
            if concept_prev == 0:
                concept_prev = cluster_prevalence_lookup.get(str(concept_id), 0)
            
            # If still not found and this is an ordinal, try the CONCEPT_ID directly
            if concept_prev == 0 and concept.get("IS_ORDINAL"):
                raw_cid = concept.get("CONCEPT_ID")
                if raw_cid:
                    concept_prev = cluster_prevalence_lookup.get(normalize_concept_id(raw_cid), 0)
        
        if concept_prev >= threshold:
            filtered.append(concept)
    
    return filtered


# =============================================================================
# X-Axis Range Calculation
# =============================================================================

def extend_range_with_stats(
    current_min: Optional[float],
    current_max: Optional[float],
    stats_dict: Dict,
    mean_key: str = "mean",
    ci_low_key: str = "ci_low",
    ci_high_key: str = "ci_high",
    padding_ratio: float = 0.1,
    default_padding: float = 5.0
) -> Tuple[Optional[float], Optional[float]]:
    """
    Extend x-axis range to include all values from a stats dictionary.
    
    Args:
        current_min: Current minimum of range
        current_max: Current maximum of range
        stats_dict: Dictionary mapping keys to stat dicts with mean/ci values
        mean_key: Key name for mean value in stat dicts
        ci_low_key: Key name for CI low value
        ci_high_key: Key name for CI high value
        padding_ratio: Ratio of range to add as padding
        default_padding: Default padding when range is 0
        
    Returns:
        Tuple of (new_min, new_max)
    """
    if current_min is None or current_max is None:
        return current_min, current_max
    
    all_values = []
    for stats in stats_dict.values():
        if stats.get(mean_key) is not None:
            all_values.append(stats[mean_key])
        if stats.get(ci_low_key) is not None:
            all_values.append(stats[ci_low_key])
        if stats.get(ci_high_key) is not None:
            all_values.append(stats[ci_high_key])
    
    if not all_values:
        return current_min, current_max
    
    stats_min = min(all_values)
    stats_max = max(all_values)
    stats_range = stats_max - stats_min if stats_max > stats_min else default_padding
    
    new_min = min(current_min, stats_min - stats_range * padding_ratio)
    new_max = max(current_max, stats_max + stats_range * padding_ratio)
    
    return new_min, new_max


# =============================================================================
# Time/Occurrence Processing Functions
# =============================================================================

def get_unique_occurrences(time_list) -> List[float]:
    """
    Extract unique, sorted occurrence times from a TIME_TO_EVENT value.
    
    Handles various input formats: numpy arrays, lists, tuples, or single values.
    Filters out NaN and None values.
    
    Args:
        time_list: A single value, list, tuple, or numpy array of time values
        
    Returns:
        Sorted list of unique, valid time values
    """
    if time_list is None:
        return []
    
    if isinstance(time_list, np.ndarray):
        if time_list.size == 0:
            return []
        # Filter out NaN values and get unique sorted values
        valid_times = time_list[~np.isnan(time_list)]
        return sorted(np.unique(valid_times).tolist())
    
    if isinstance(time_list, (list, tuple)):
        if len(time_list) == 0:
            return []
        # Filter out None/NaN and get unique sorted values
        valid_times = [
            t for t in time_list 
            if t is not None and not (isinstance(t, (int, float)) and pd.isna(t))
        ]
        return sorted(list(set(valid_times)))
    
    # Single value
    if isinstance(time_list, (int, float)) and not pd.isna(time_list):
        return [float(time_list)]
    
    return []


def get_min_time(time_list) -> Union[float, None]:
    """
    Get the minimum time from a TIME_TO_EVENT value.
    
    Handles various input formats: numpy arrays, lists, tuples, or single values.
    Filters out NaN and None values before finding minimum.
    
    Args:
        time_list: A single value, list, tuple, or numpy array of time values
        
    Returns:
        Minimum valid time value, or None if no valid times
    """
    if time_list is None:
        return None
    
    if isinstance(time_list, np.ndarray):
        if time_list.size == 0:
            return None
        valid_times = time_list[~np.isnan(time_list)]
        if len(valid_times) == 0:
            return None
        return float(np.min(valid_times))
    
    if isinstance(time_list, (list, tuple)):
        if len(time_list) == 0:
            return None
        valid_times = [
            t for t in time_list 
            if t is not None and not (isinstance(t, (int, float)) and pd.isna(t))
        ]
        return min(valid_times) if valid_times else None
    
    # Single value - check if it's NaN
    if isinstance(time_list, (int, float)) and pd.isna(time_list):
        return None
    
    try:
        return float(time_list)
    except (ValueError, TypeError):
        return None


def normalize_concept_name(name) -> str:
    """
    Normalize a concept name by removing line breaks and normalizing whitespace.
    
    Args:
        name: The concept name to normalize
        
    Returns:
        Normalized concept name string
    """
    if not name or pd.isna(name):
        return ""
    name_str = str(name)
    # Remove line breaks and normalize whitespace
    name_str = name_str.replace('\n', ' ').replace('\r', ' ')
    name_str = ' '.join(name_str.split())  # Normalize whitespace
    return name_str.strip()


def format_column_name(col_name: str) -> str:
    """
    Convert SNAKE_CASE column names to Title Case with spaces.
    
    Examples:
        COHORT_DEFINITION_ID -> Cohort Definition Id
        PERSON_ID -> Person Id
        TARGET_SUBJECT_PREVALENCE -> Target Subject Prevalence
    """
    formatted = col_name.replace("_", " ").title()
    return formatted


def format_heritage_label(heritage: str) -> str:
    """
    Convert heritage values to user-friendly labels.
    
    Examples:
        procedure_occurrence -> Procedures
        condition_occurrence -> Conditions
        drug_exposure -> Drug Exposures
        measurement -> Measurements
    """
    # Check if we have a direct mapping
    if heritage.lower() in HERITAGE_LABELS:
        return HERITAGE_LABELS[heritage.lower()]
    
    # Otherwise, convert snake_case to Title Case
    formatted = heritage.replace("_", " ").title()
    return formatted


def normalize_concept_id(concept_id) -> str:
    """
    Normalize concept ID to string, removing .0 suffix from floats.
    
    Args:
        concept_id: The concept ID to normalize (can be int, float, or str)
        
    Returns:
        Normalized string representation of the concept ID
    """
    if concept_id is None:
        return ""
    # Handle float that is a whole number
    if isinstance(concept_id, float) and concept_id.is_integer():
        return str(int(concept_id))
    # Convert to string
    s = str(concept_id)
    # Remove ".0" suffix if present (but NOT using rstrip which removes ALL trailing 0s!)
    if s.endswith('.0'):
        return s[:-2]
    return s


def convert_list_columns_to_strings(df: pd.DataFrame) -> pd.DataFrame:
    """
    Convert list/array columns to string representations for DataTable compatibility.
    
    DataTable only accepts string, number, or boolean values. This function
    converts any columns containing lists/arrays to string representations.
    
    Args:
        df: DataFrame that may contain list/array columns
        
    Returns:
        DataFrame with list/array columns converted to strings
    """
    df_copy = df.copy()
    
    for col in df_copy.columns:
        # Check if column contains list-like values
        non_null_values = df_copy[col].dropna()
        if len(non_null_values) > 0:
            sample_value = non_null_values.iloc[0]
            
            # Check if it's a list, array, or other iterable (but not a string)
            is_list_like = (
                isinstance(sample_value, (list, tuple, np.ndarray)) or
                (hasattr(sample_value, '__iter__') and 
                 not isinstance(sample_value, (str, bytes, int, float, bool)))
            )
            
            if is_list_like:
                # Convert list/array values to string representation
                def convert_value(x):
                    # Handle numpy arrays first
                    if isinstance(x, np.ndarray):
                        return str(x.tolist())
                    # Handle lists and tuples
                    if isinstance(x, (list, tuple)):
                        return str(list(x))
                    # Check for NaN/None
                    if not isinstance(x, (np.ndarray, list, tuple)):
                        try:
                            if pd.isna(x):
                                return None
                        except (ValueError, TypeError):
                            pass
                    # Handle other iterables
                    if hasattr(x, '__iter__') and not isinstance(x, (str, bytes, int, float, bool, np.ndarray, list, tuple)):
                        try:
                            return str(list(x))
                        except (TypeError, ValueError):
                            return str(x)
                    return str(x) if x is not None else None
                
                df_copy[col] = df_copy[col].apply(convert_value)
    
    return df_copy


def is_ordinal_concept(row: Dict) -> bool:
    """
    Check if a row represents an ordinal concept.
    
    Args:
        row: Dictionary containing concept data
        
    Returns:
        True if the concept is an ordinal (1st, 2nd, etc. occurrence)
    """
    # Check IS_ORDINAL flag
    if row.get("IS_ORDINAL", False):
        return True
    
    # Check if ORDINAL field exists and is not None/0
    ordinal = row.get("ORDINAL")
    if ordinal is not None and ordinal != 0:
        return True
        
    return False

