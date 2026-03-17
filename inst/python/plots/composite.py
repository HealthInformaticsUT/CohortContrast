"""
Composite plot - Event occurrences visualization.
"""

from typing import Dict, List, Optional, Tuple
import logging
import json
import hashlib
from collections import OrderedDict
import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots

from config.constants import (
    HERITAGE_ORDER,
    HERITAGE_COLORS,
    PIXELS_PER_CONCEPT,
    MIN_PLOT_AREA,
    TOP_MARGIN,
    BOTTOM_MARGIN,
    SMALL_GAP,
)
from utils.helpers import format_heritage_label, get_unique_occurrences

logger = logging.getLogger('ContrastViewer.composite')

# Cache for parsed KDE data to avoid repeated JSON parsing
# Key: hash of (concept_id, heritage, raw_json_string) or (concept_id, heritage)
# Value: tuple of (kde_x, kde_y) lists
_kde_data_cache: OrderedDict[str, Tuple[List, List]] = OrderedDict()
MAX_KDE_CACHE_SIZE = 1000  # Maximum number of cached KDE parsing results


def _parse_kde_with_cache(
    concept_id: str,
    heritage: Optional[str],
    time_kde_x: Optional[str],
    time_kde_y: Optional[str]
) -> Tuple[List, List]:
    """
    Parse KDE data with caching to avoid repeated JSON parsing.
    
    Args:
        concept_id: Concept ID (normalized string)
        heritage: Heritage string or None
        time_kde_x: JSON string or already-parsed list for KDE x-coordinates
        time_kde_y: JSON string or already-parsed list for KDE y-coordinates
    
    Returns:
        Tuple of (kde_x, kde_y) lists
    """
    global _kde_data_cache
    
    # Create cache key from concept_id, heritage, and raw JSON strings
    # Use hash of the JSON strings to handle large data efficiently
    heritage_str = str(heritage) if heritage and pd.notna(heritage) else "None"
    kde_x_str = str(time_kde_x) if time_kde_x is not None else ""
    kde_y_str = str(time_kde_y) if time_kde_y is not None else ""
    
    # Create deterministic cache key
    cache_key_parts = [concept_id, heritage_str, kde_x_str, kde_y_str]
    cache_key = hashlib.md5("|".join(cache_key_parts).encode()).hexdigest()
    
    # Check cache first
    if cache_key in _kde_data_cache:
        # Move to end (most recently used) for LRU behavior
        _kde_data_cache.move_to_end(cache_key)
        return _kde_data_cache[cache_key]
    
    # Parse JSON if needed
    kde_x = []
    kde_y = []
    try:
        # Check if time_kde_x exists and is not empty (handles both strings and numpy arrays)
        if time_kde_x is not None:
            if isinstance(time_kde_x, str):
                if len(time_kde_x) > 0:
                    kde_x = json.loads(time_kde_x)
            else:
                # Already parsed (numpy array or list) - check length safely
                try:
                    if len(time_kde_x) > 0:
                        kde_x = time_kde_x
                except (TypeError, ValueError):
                    pass
        # Check if time_kde_y exists and is not empty (handles both strings and numpy arrays)
        if time_kde_y is not None:
            if isinstance(time_kde_y, str):
                if len(time_kde_y) > 0:
                    kde_y = json.loads(time_kde_y)
            else:
                # Already parsed (numpy array or list) - check length safely
                try:
                    if len(time_kde_y) > 0:
                        kde_y = time_kde_y
                except (TypeError, ValueError):
                    pass
    except (json.JSONDecodeError, TypeError, ValueError) as e:
        logger.debug(f"Error parsing KDE data for concept {concept_id}: {e}")
        pass
    
    # Store in cache (LRU eviction)
    if len(_kde_data_cache) >= MAX_KDE_CACHE_SIZE:
        _kde_data_cache.popitem(last=False)  # Remove oldest (first) item
    
    _kde_data_cache[cache_key] = (kde_x, kde_y)
    _kde_data_cache.move_to_end(cache_key)
    
    return kde_x, kde_y


def wrap_text(text: str, max_chars: int = 45) -> str:
    """Wrap text with <br> for plotly display after max_chars characters."""
    if not text or len(text) <= max_chars:
        return text
    
    words = text.split(' ')
    lines = []
    current_line = []
    current_length = 0
    
    for word in words:
        if current_length + len(word) + 1 <= max_chars or not current_line:
            current_line.append(word)
            current_length += len(word) + 1
        else:
            lines.append(' '.join(current_line))
            current_line = [word]
            current_length = len(word)
    
    if current_line:
        lines.append(' '.join(current_line))
    
    return '<br>'.join(lines)


def create_composite_plot(
    active_concepts: List[Dict],
    data_patients: pd.DataFrame,
    dashboard_data: List[Dict],
    cluster_patient_ids: Optional[set] = None,
    show_all_background: bool = False,
    mapping_data: Optional[pd.DataFrame] = None
) -> go.Figure:
    """
    Create the composite plot showing time_to_event distributions for active concepts.
    
    Args:
        active_concepts: List of active concept dictionaries from dashboard table (with _show=True)
        data_patients: DataFrame with patient data including TIME_TO_EVENT
        dashboard_data: Full dashboard data to get concept names and ordinals
        cluster_patient_ids: Optional set of patient IDs to filter for cluster view (foreground)
        show_all_background: If True and cluster_patient_ids provided, show background
                            as faded with cluster data overlaid
        mapping_data: Optional DataFrame with complementaryMappingTable data for hover tooltips
        
    Returns:
        Plotly figure with violin and box plots
    """
    # Build mapping lookup: concept_id -> list of source concept names that mapped to it
    concept_mappings = {}
    if mapping_data is not None and not mapping_data.empty:
        # Convert DataFrame to list of dicts for efficient iteration
        for row in mapping_data.to_dict('records'):
            target_id = row.get("NEW_CONCEPT_ID")
            source_name = row.get("CONCEPT_NAME")
            if target_id is not None and source_name:
                target_id_str = str(target_id).replace('.0', '') if '.' in str(target_id) else str(target_id)
                if target_id_str not in concept_mappings:
                    concept_mappings[target_id_str] = []
                if source_name not in concept_mappings[target_id_str]:
                    concept_mappings[target_id_str].append(source_name)
    if not active_concepts or data_patients.empty:
        # Return empty figure
        fig = go.Figure()
        fig.add_annotation(
            text="No active concepts selected. Check concepts in the table to display them.",
            xref="paper", yref="paper",
            x=0.5, y=0.5, showarrow=False,
            font=dict(size=16, color="#666")
        )
        fig.update_layout(
            xaxis=dict(visible=False),
            yaxis=dict(visible=False),
            plot_bgcolor="white"
        )
        return fig, [], {}
    
    # Create a mapping from concept_id to concept info
    concept_map = {}
    for row in dashboard_data:
        concept_id = row.get("_concept_id") or row.get("CONCEPT_ID")
        if concept_id is not None:
            concept_map[str(concept_id)] = row
    
    # Build a set of active main concept IDs (for filtering ordinals)
    # Normalize concept IDs to handle type mismatches
    def normalize_id(cid):
        return str(cid).replace(".0", "") if cid is not None else None
    
    active_main_concept_keys = set()
    for concept_row in active_concepts:
        is_ordinal = concept_row.get("IS_ORDINAL", False)
        if not is_ordinal:
            # This is a main concept
            concept_id = concept_row.get("_concept_id") or concept_row.get("CONCEPT_ID")
            heritage = concept_row.get("HERITAGE")
            if concept_id is not None:
                key = (normalize_id(concept_id), str(heritage) if heritage is not None and pd.notna(heritage) else None)
                active_main_concept_keys.add(key)
    
    # Filter active_concepts to remove ordinals whose main concept is not active
    filtered_active_concepts = []
    for concept_row in active_concepts:
        is_ordinal = concept_row.get("IS_ORDINAL", False)
        if is_ordinal:
            # Check if the main concept is active
            original_concept_id = concept_row.get("ORIGINAL_CONCEPT_ID")
            heritage = concept_row.get("HERITAGE")
            if original_concept_id is not None:
                key = (normalize_id(original_concept_id), str(heritage) if heritage is not None and pd.notna(heritage) else None)
                if key not in active_main_concept_keys:
                    # Main concept is not active, skip this ordinal
                    continue
        filtered_active_concepts.append(concept_row)
    
    active_concepts = filtered_active_concepts
    
    # Prepare data for plotting
    plot_data = []
    
    # Filter data_patients for target cohort
    df_target = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
    
    # Pre-group data by CONCEPT_ID and HERITAGE for faster lookups
    has_heritage = "HERITAGE" in df_target.columns
    if has_heritage:
        df_target_grouped = {
            key: group for key, group in df_target.groupby(["CONCEPT_ID", "HERITAGE"])
        }
    else:
        df_target_grouped = {
            (key, None): group for key, group in df_target.groupby("CONCEPT_ID")
        }
    
    # Track seen concept keys to avoid duplicates
    seen_concept_keys = set()
    
    for concept_row in active_concepts:
        concept_id = concept_row.get("_concept_id") or concept_row.get("CONCEPT_ID")
        if concept_id is None:
            continue
        
        concept_id_str = str(concept_id)
        concept_info = concept_map.get(concept_id_str, concept_row)
        
        concept_name = concept_info.get("CONCEPT_NAME", f"Concept {concept_id}")
        heritage = concept_info.get("HERITAGE")
        is_ordinal = concept_info.get("IS_ORDINAL", False)
        ordinal = concept_info.get("ORDINAL", 0)
        original_concept_id = concept_info.get("ORIGINAL_CONCEPT_ID", concept_id)
        
        # Look up pre-grouped data for this concept (O(1) instead of O(n))
        lookup_key = (original_concept_id, heritage) if has_heritage and heritage is not None else (original_concept_id, None)
        concept_data = df_target_grouped.get(lookup_key)
        
        if concept_data is None or concept_data.empty:
            # Try without heritage as fallback
            if has_heritage:
                for key, group in df_target_grouped.items():
                    if key[0] == original_concept_id:
                        concept_data = group
                        break
            if concept_data is None or (hasattr(concept_data, 'empty') and concept_data.empty):
                continue
        
        concept_data = concept_data.copy()
        
        if concept_data.empty:
            continue
        
        # Extract time_to_event values
        if is_ordinal and ordinal > 0:
            # For ordinal concepts, get the specific ordinal occurrence
            # Match the table calculation: get unique sorted occurrences, then Nth occurrence, then group by person
            concept_data["OCCURRENCES"] = concept_data["TIME_TO_EVENT"].apply(get_unique_occurrences)
            # Filter to people who have at least that many occurrences
            concept_data = concept_data[concept_data["OCCURRENCES"].apply(len) >= ordinal].copy()
            if concept_data.empty:
                continue
            # Get the specific occurrence (ordinal-1 because 1st is index 0)
            concept_data["TIME_VALUE"] = concept_data["OCCURRENCES"].apply(
                lambda occs: occs[ordinal - 1] if len(occs) >= ordinal else None
            )
            concept_data = concept_data[concept_data["TIME_VALUE"].notna()].copy()
            
            if concept_data.empty:
                continue
            
            # Group by person to get one value per person (matching table calculation)
            person_occurrences = concept_data.groupby("PERSON_ID")["TIME_VALUE"].first()
            time_values = person_occurrences.tolist()
            patient_ids = set(person_occurrences.index.tolist())
            # Store per-patient time values for cluster filtering
            patient_time_map = person_occurrences.to_dict()
            # For ordinals, only show the ordinal suffix (e.g., "1st", "2nd", "3rd") as display
            ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(ordinal, f"{ordinal}th")
            display_name = ordinal_suffix  # Short name for axis label
            # concept_name already contains the full name with ordinal (e.g., "Pneumonia 1st")
            full_concept_name = concept_name
        else:
            # For main concepts, get ALL occurrences from raw data (not just first)
            all_times = []
            patient_ids = set()
            patient_time_map = {}
            # Convert DataFrame to list of dicts for efficient iteration
            for row in concept_data.to_dict('records'):
                pid = row.get("PERSON_ID")
                time_list = row.get("TIME_TO_EVENT")
                if time_list is None:
                    continue
                patient_ids.add(pid)
                patient_times = []
                if isinstance(time_list, np.ndarray):
                    if time_list.size == 0:
                        continue
                    valid_times = time_list[~np.isnan(time_list)]
                    patient_times = valid_times.tolist()
                    all_times.extend(patient_times)
                elif isinstance(time_list, (list, tuple)):
                    if len(time_list) == 0:
                        continue
                    valid_times = [t for t in time_list if t is not None and not (isinstance(t, (int, float)) and pd.isna(t))]
                    patient_times = valid_times
                    all_times.extend(valid_times)
                elif isinstance(time_list, (int, float)) and not pd.isna(time_list):
                    patient_times = [float(time_list)]
                    all_times.append(float(time_list))
                if patient_times:
                    patient_time_map[pid] = patient_times
            time_values = all_times
            display_name = concept_name
            full_concept_name = concept_name  # Full name same as display for main concepts
        
        if not time_values:
            continue
        
        # Create unique key to prevent duplicates (concept_id, heritage, ordinal)
        concept_key = (normalize_id(concept_id), str(heritage) if heritage is not None and pd.notna(heritage) else None, ordinal)
        if concept_key in seen_concept_keys:
            # Skip duplicate entry
            continue
        seen_concept_keys.add(concept_key)
        
        # Wrap concept name after 50 chars
        if len(display_name) > 50:
            # Try to wrap at word boundaries
            words = display_name.split()
            wrapped_lines = []
            current_line = ""
            for word in words:
                if len(current_line + word) <= 50:
                    current_line += (word + " " if current_line else word)
                else:
                    if current_line:
                        wrapped_lines.append(current_line.strip())
                    current_line = word + " "
            if current_line:
                wrapped_lines.append(current_line.strip())
            display_name = "<br>".join(wrapped_lines)
        
        # Calculate median for this concept
        median_time = np.median(time_values) if time_values else np.nan
        
        plot_data.append({
            "concept_name": display_name,
            "full_concept_name": full_concept_name,
            "heritage": heritage or "unknown",
            "time_values": time_values,
            "is_ordinal": is_ordinal,
            "ordinal": ordinal,
            "original_concept_id": original_concept_id,
            "concept_id": concept_id,
            "median_time": median_time,
            "patient_ids": patient_ids,
            "patient_time_map": patient_time_map
        })
    
    # Add main_time_median and normalize ordinal (0 for main, keep ordinal for subrows)
    # First, create a map of main concept medians
    main_concept_medians = {}
    for item in plot_data:
        if not item["is_ordinal"]:
            # This is a main concept
            key = (str(item["concept_id"]).replace(".0", ""), item["heritage"])
            main_concept_medians[key] = item["median_time"]
    
    # Now add main_time_median to all items and set ordinal to 0 for main concepts
    for item in plot_data:
        if item["is_ordinal"]:
            # For ordinals, find their parent main concept's median
            key = (str(item["original_concept_id"]).replace(".0", ""), item["heritage"])
            item["main_time_median"] = main_concept_medians.get(key, item["median_time"])
            # Keep ordinal as is (1, 2, 3, etc.)
        else:
            # For main concepts, use their own median and set ordinal to 0
            item["main_time_median"] = item["median_time"]
            item["ordinal"] = 0
    
    # Sort all items by heritage, main_time_median, then ordinal
    heritage_order_map = {h: i for i, h in enumerate(HERITAGE_ORDER)}
    
    def sort_key(item):
        heritage = item["heritage"] or "unknown"
        heritage_idx = heritage_order_map.get(heritage, 999)
        main_median = item["main_time_median"]
        ordinal = item["ordinal"]
        return (heritage_idx, main_median, ordinal)
    
    plot_data.sort(key=sort_key)
    
    if not plot_data:
        fig = go.Figure()
        fig.add_annotation(
            text="No data available for active concepts.",
            xref="paper", yref="paper",
            x=0.5, y=0.5, showarrow=False,
            font=dict(size=16, color="#666")
        )
        fig.update_layout(
            xaxis=dict(visible=False),
            yaxis=dict(visible=False),
            plot_bgcolor="white"
        )
        return fig, [], {}
    
    # Group by heritage first
    heritage_groups = {}
    for item in plot_data:
        heritage = item["heritage"] or "unknown"
        if heritage not in heritage_groups:
            heritage_groups[heritage] = []
        heritage_groups[heritage].append(item)
    
    # Within each heritage, group main concepts with their ordinals
    heritage_concept_groups = {}
    for heritage, items in heritage_groups.items():
        main_concept_groups = {}
        for item in items:
            if item["is_ordinal"]:
                # This is an ordinal, group by original_concept_id
                # Normalize to string to handle type mismatches
                orig_id = str(item["original_concept_id"]).replace(".0", "")
                heritage = item["heritage"]
                key = (orig_id, heritage)
                if key not in main_concept_groups:
                    main_concept_groups[key] = {"main": None, "ordinals": []}
                main_concept_groups[key]["ordinals"].append(item)
            else:
                # This is a main concept
                # Normalize to string to handle type mismatches
                concept_id = str(item["concept_id"]).replace(".0", "")
                heritage = item["heritage"]
                key = (concept_id, heritage)
                if key not in main_concept_groups:
                    main_concept_groups[key] = {"main": None, "ordinals": []}
                main_concept_groups[key]["main"] = item
        
        # Sort ordinals within each group by ordinal number
        for key in main_concept_groups:
            main_concept_groups[key]["ordinals"].sort(key=lambda x: x["ordinal"])
        
        # Create list of concept groups for this heritage
        concept_groups = []
        for key, group in main_concept_groups.items():
            if group["main"] is not None:
                concept_groups.append(group)
            elif group["ordinals"]:
                concept_groups.append(group)
        
        # Sort concept groups by main_time_median (all items in a group have the same main_time_median)
        # Use the main concept's main_time_median if available, otherwise use first ordinal's
        concept_groups.sort(key=lambda g: (
            g["main"]["main_time_median"] if g["main"] is not None
            else (g["ordinals"][0]["main_time_median"] if g["ordinals"] else np.inf)
        ))
        
        heritage_concept_groups[heritage] = concept_groups
    
    all_ci_lows = []
    all_ci_highs = []
    for items in heritage_groups.values():
        for item in items:
            ci_low = item.get("q1") if item.get("q1") is not None else item.get("min")
            ci_high = item.get("q3") if item.get("q3") is not None else item.get("max")
            if ci_low is not None:
                all_ci_lows.append(ci_low)
            if ci_high is not None:
                all_ci_highs.append(ci_high)
    
    if all_ci_lows and all_ci_highs:
        x_range = [min(all_ci_lows), max(all_ci_highs)]
    else:
        x_range = None
    
    # Get heritages in order (only those that have data)
    heritages_with_data = [h for h in HERITAGE_ORDER if h in heritage_concept_groups]
    
    if not heritages_with_data:
        fig = go.Figure()
        fig.add_annotation(
            text="No data available for active concepts.",
            xref="paper", yref="paper",
            x=0.5, y=0.5, showarrow=False,
            font=dict(size=16, color="#666")
        )
        fig.update_layout(
            xaxis=dict(visible=False),
            yaxis=dict(visible=False),
            plot_bgcolor="white"
        )
        return fig, [], {}
    
    # Calculate number of concepts per heritage for uniform distribution (30px per concept)
    concepts_per_heritage = {}
    total_concepts = 0
    for heritage in heritages_with_data:
        concept_groups = heritage_concept_groups[heritage]
        concept_count = 0
        for group in concept_groups:
            # Count main concept
            if group["main"] is not None:
                concept_count += 1
            # Count ordinals
            concept_count += len(group["ordinals"])
        concepts_per_heritage[heritage] = concept_count
        total_concepts += concept_count
    
    # Calculate row heights: 30px per concept per heritage panel (proportional)
    row_heights = [concepts_per_heritage[h] * 30 for h in heritages_with_data]
    
    # Create subplots - one row per heritage with proportional heights
    # Use very small vertical spacing for gaps between heritage groups
    # The spacing is a fraction of the subplot height, so it stays small
    fig = make_subplots(
        rows=len(heritages_with_data),
        cols=1,
        shared_xaxes=True,
        vertical_spacing=0.002,  # Very small gap between heritage groups (0.2% of subplot height)
        row_heights=row_heights,  # Set proportional heights based on concept count
        subplot_titles=[format_heritage_label(h) for h in heritages_with_data]
    )
    
    # Store ordering information for prevalence plot
    all_y_labels_order = []
    heritage_groups_order = {}
    
    # Add traces for each heritage
    for row_idx, heritage in enumerate(heritages_with_data, start=1):
        # Get the sorted items for this heritage (already sorted by main_time_median, then ordinal)
        items = heritage_groups[heritage]
        heritage_color = HERITAGE_COLORS.get(heritage, "#CCCCCC")
        
        # Store items for this heritage (for prevalence plot)
        heritage_groups_order[heritage] = items
        
        # Build y-axis labels directly from sorted items
        # Items are already sorted by: heritage, main_time_median, ordinal
        # This gives us: Concept B (main, median=10), Concept F (main, median=20), Concept F 1st, Concept F 2nd, etc.
        y_labels = []
        concept_data_map = {}
        
        for item in items:
            y_label = item["concept_name"]
            y_labels.append(y_label)
            concept_data_map[y_label] = item
            # Store in global order list
            all_y_labels_order.append(y_label)
        
        # Add violin and box plots for each concept in this heritage
        # Use numeric y positions and map to labels
        for y_idx, y_label in enumerate(y_labels):
            item = concept_data_map[y_label]
            time_values = item["time_values"]
            is_ordinal = item["is_ordinal"]
            
            # Build hover text with concept name and mapped source concepts
            concept_id = item.get("concept_id") or item.get("_concept_id")
            original_concept_id = item.get("original_concept_id")
            full_concept_name = item.get("full_concept_name", y_label)
            
            # For ordinals, use the original concept ID to look up mappings
            lookup_id = original_concept_id if is_ordinal and original_concept_id else concept_id
            lookup_id_str = str(lookup_id).replace('.0', '') if lookup_id and '.' in str(lookup_id) else str(lookup_id) if lookup_id else None
            
            # Get mapped source concepts
            mapped_sources = []
            if lookup_id_str and concept_mappings:
                mapped_sources = concept_mappings.get(lookup_id_str, [])
            
            # Build hover text - use full concept name (e.g., "Pneumonia 1st" for ordinals)
            hover_lines = [f"<b>{full_concept_name}</b>"]
            if mapped_sources:
                hover_lines.append("<br><b>Mapped from:</b>")
                for source_name in mapped_sources[:10]:  # Limit to 10 to prevent huge tooltips
                    hover_lines.append(f"<br>• {source_name}")
                if len(mapped_sources) > 10:
                    hover_lines.append(f"<br>... and {len(mapped_sources) - 10} more")
            hover_text = "".join(hover_lines) + "<extra></extra>"
            
            # Get cluster-specific time values if filtering
            cluster_time_values = []
            patient_ids_with_concept = item.get("patient_ids", set())
            patient_time_map = item.get("patient_time_map", {})
            
            if cluster_patient_ids is not None and show_all_background:
                if patient_ids_with_concept and patient_time_map:
                    # Filter to primary cluster patients (red overlay)
                    cluster_patients_with_concept = patient_ids_with_concept & cluster_patient_ids
                    if cluster_patients_with_concept:
                        for pid in cluster_patients_with_concept:
                            times = patient_time_map.get(pid, [])
                            if isinstance(times, (list, np.ndarray)):
                                cluster_time_values.extend(times if isinstance(times, list) else times.tolist())
                            elif times is not None and not pd.isna(times):
                                cluster_time_values.append(times)
            
            # Background is always "All" (time_values)
            if not time_values:
                continue
            
            # Determine opacity and whether to add overlay
            if show_all_background and cluster_patient_ids is not None:
                # Background layer (all patients) - slightly reduced opacity for visibility
                bg_alpha = 0.35
                fg_alpha = 0.75
            else:
                bg_alpha = 0.5
                fg_alpha = None  # No foreground layer needed
            
            # Add violin plot (background - always "All" patients)
            fig.add_trace(
                go.Violin(
                    x=time_values,
                    y=[y_idx] * len(time_values),  # Use numeric position
                    name=y_label,
                    orientation='h',
                    side='both',
                    width=1.2,
                    points=False,
                    box_visible=False,  # We'll add boxplot separately
                    meanline_visible=False,
                    fillcolor=heritage_color,
                    line_color="rgba(0,0,0,0)",  # No border (color=NA in R)
                    opacity=bg_alpha,
                    showlegend=False,
                    hoverinfo='skip'  # Skip violin hover, use box for hover
                ),
                row=row_idx,
                col=1
            )
            
            # Add boxplot with different line style for ordinals (background - always "All")
            # "gray40" in R is approximately #666666 (40% gray)
            box_color = "#666666" if is_ordinal else "black"
            box_opacity = bg_alpha if show_all_background and cluster_patient_ids else 1.0
            
            # Add boxplot trace (background)
            fig.add_trace(
                go.Box(
                    x=time_values,
                    y=[y_idx] * len(time_values),  # Use numeric position
                    name=y_label,
                    orientation='h',
                    fillcolor="rgba(0,0,0,0)",  # Transparent fill
                    line=dict(color=box_color, width=0.6),
                    boxmean=False,
                    showlegend=False,
                    marker=dict(opacity=0),  # Hide outliers
                    boxpoints=False,
                    notched=False,
                    width=0.3,
                    opacity=box_opacity,
                    hoverinfo='skip'  # Skip box hover, use scatter for hover
                ),
                row=row_idx,
                col=1
            )
            
            # Add invisible scatter trace for clean hover (just concept name)
            median_x = np.median(time_values) if time_values else 0
            fig.add_trace(
                go.Scatter(
                    x=[median_x],
                    y=[y_idx],
                    mode='markers',
                    marker=dict(size=30, opacity=0),  # Invisible but hoverable
                    showlegend=False,
                    hovertemplate=hover_text,
                    hoverlabel=dict(
                        bgcolor='white',
                        bordercolor='#ccc',
                        font=dict(color='black', size=12),
                        align='left'
                    )
                ),
                row=row_idx,
                col=1
            )
    
            # Add cluster overlay if we have cluster data and show_all_background
            if show_all_background and cluster_patient_ids is not None and cluster_time_values:
                # Foreground layer (cluster patients only) - smaller size for better comparison
                fig.add_trace(
                    go.Violin(
                        x=cluster_time_values,
                        y=[y_idx] * len(cluster_time_values),
                        name=f"{y_label} (Cluster)",
                        orientation='h',
                        side='both',
                        width=0.5,  # Half the size of background violin
                        points=False,
                        box_visible=False,
                        meanline_visible=False,
                        fillcolor=heritage_color,
                        line_color="rgba(0,0,0,0.3)",
                        opacity=fg_alpha,
                        showlegend=False,
                        hoverinfo='skip'  # Skip violin hover
                    ),
                    row=row_idx,
                    col=1
                )
                
                # Add boxplot for cluster data - smaller for better comparison
                fig.add_trace(
                    go.Box(
                        x=cluster_time_values,
                        y=[y_idx] * len(cluster_time_values),
                        name=f"{y_label} (Cluster)",
                        orientation='h',
                        fillcolor="rgba(0,0,0,0)",
                        line=dict(color="#E74C3C", width=1.5),  # Red for primary cluster
                        boxmean=False,
                        showlegend=False,
                        marker=dict(opacity=0),
                        boxpoints=False,
                        notched=False,
                        width=0.12,  # Half the size of background box
                        hoverinfo='skip'  # Skip hover on cluster box
                    ),
                    row=row_idx,
                    col=1
                )
            
    # Calculate total figure height: 200 + 30 * num_concepts
    # Formula: total = top_margin + plot_area + bottom_margin
    # Where plot_area = max(200, 30*num_concepts) + small gaps
    PIXELS_PER_CONCEPT = 30
    MIN_PLOT_AREA = 200  # Minimum plot area for concept rows
    TOP_MARGIN = 25  # Fixed top margin for titles
    BOTTOM_MARGIN = 25  # Fixed bottom margin
    SMALL_GAP = 60  # Fixed gap between heritage groups (in pixels) - should match combined figure
    
    # Plot area needed: max(200, 30*num_concepts) + gaps between heritages
    plot_area = max(MIN_PLOT_AREA, total_concepts * PIXELS_PER_CONCEPT)
    heritage_gaps = (len(heritages_with_data) - 1) * SMALL_GAP if len(heritages_with_data) > 1 else 0
    total_plot_area = plot_area + heritage_gaps
    
    # Total figure height = margins + plot area = 200 + 30*num_concepts (approximately)
    # This ensures: plot_area ≈ 200 + 30*num_concepts, total ≈ plot_area + margins
    plot_height = TOP_MARGIN + total_plot_area + BOTTOM_MARGIN
    
    # Update layout - disable interactivity
    fig.update_layout(
        title="",  # No title - column titles are added as annotations
        height=plot_height,
        showlegend=False,
        plot_bgcolor="white",
        paper_bgcolor="white",
        margin=dict(l=200, r=50, t=TOP_MARGIN, b=BOTTOM_MARGIN),  # Fixed margins
        dragmode=False,  # Disable pan
        hovermode="closest",  # Use closest point hover
        hoverlabel=dict(
            bgcolor='white',
            bordercolor='#ccc',
            font=dict(color='black', size=12),
            align='left'
        )
    )
    
    # Update x-axes (shared, so update all) and disable interactions
    for row_idx in range(1, len(heritages_with_data) + 1):
        fig.update_xaxes(
            range=x_range,
            title_text="Time to Event (days)" if row_idx == len(heritages_with_data) else "",
            fixedrange=True,  # Disable zoom/pan
            row=row_idx,
            col=1
        )
    
    # Update y-axes (free per heritage) - use numeric positions with tick labels
    # Rebuild y_labels in the same order as when adding traces
    for row_idx, heritage in enumerate(heritages_with_data, start=1):
        # Get the sorted items for this heritage (already sorted by main_time_median, then ordinal)
        items = heritage_groups[heritage]
        y_labels = []
        for item in items:
            # Wrap long concept names for display
            y_labels.append(wrap_text(item["concept_name"], max_chars=60))
        
        # Use numeric y-axis with custom tick labels
        # Set domain to use full range for uniform spacing (30px per concept)
        num_concepts = len(y_labels)
        # Reversed range: [max, min] so first item (y=0) appears at top
        fig.update_yaxes(
            tickmode='array',
            tickvals=list(range(num_concepts)),
            ticktext=y_labels,
            range=[num_concepts - 0.5, -0.5],  # Reversed range: [max, min] so first item at top
            title="",
            fixedrange=True,  # Disable zoom/pan
            row=row_idx,
            col=1
        )
    
    return fig, all_y_labels_order, heritage_groups_order


def create_composite_plot_from_summary(
    active_concepts: List[Dict],
    concept_summaries: pd.DataFrame,
    dashboard_data: List[Dict],
    ordinal_summaries: Optional[pd.DataFrame] = None,
    mapping_data: Optional[pd.DataFrame] = None,
    selected_cluster: Optional[str] = None,
    clustering_summary_matrix: Optional[pd.DataFrame] = None
) -> Tuple[go.Figure, List[str], Dict]:
    """
    Create the composite plot from pre-computed summary data (summary mode).
    
    Uses KDE data stored in concept_summaries to create violin-like visualizations
    without needing patient-level data.
    
    Args:
        active_concepts: List of active concept dictionaries from dashboard table
        concept_summaries: DataFrame with pre-computed time distributions
        dashboard_data: Full dashboard data for concept names
        ordinal_summaries: DataFrame with ordinal concept summaries
        mapping_data: Optional mapping data for tooltips
        selected_cluster: Optional cluster ID (e.g., "C1") to show as overlay
        clustering_summary_matrix: Optional DataFrame with cluster-specific summary stats
        
    Returns:
        Tuple of (figure, y_labels, heritage_groups)
    """
    # Build mapping lookup: concept_id -> list of source concept names that mapped to it
    # Same as patient mode
    concept_mappings = {}
    if mapping_data is not None and not mapping_data.empty:
        # Convert DataFrame to list of dicts for efficient iteration
        for row in mapping_data.to_dict('records'):
            target_id = row.get("NEW_CONCEPT_ID")
            source_name = row.get("CONCEPT_NAME")
            if target_id is not None and source_name:
                target_id_str = str(target_id).replace('.0', '') if '.' in str(target_id) else str(target_id)
                if target_id_str not in concept_mappings:
                    concept_mappings[target_id_str] = []
                if source_name not in concept_mappings[target_id_str]:
                    concept_mappings[target_id_str].append(source_name)
    
    if not active_concepts or concept_summaries.empty:
        fig = go.Figure()
        fig.add_annotation(
            text="No active concepts selected.",
            xref="paper", yref="paper",
            x=0.5, y=0.5, showarrow=False,
            font=dict(size=16, color="#666")
        )
        fig.update_layout(
            xaxis=dict(visible=False),
            yaxis=dict(visible=False),
            plot_bgcolor="white"
        )
        return fig, [], {}
    
    # Build concept lookup from dashboard_data
    concept_map = {}
    for row in dashboard_data:
        concept_id = row.get("_concept_id") or row.get("CONCEPT_ID")
        if concept_id is not None:
            concept_map[str(concept_id).replace(".0", "")] = row
    
    # Filter active_concepts to remove ordinals whose main concept is not active
    # This matches the logic in create_composite_plot (patient mode)
    def normalize_id(cid):
        return str(cid).replace(".0", "") if cid is not None else None
    
    # Build a set of active main concept IDs (for filtering ordinals)
    active_main_concept_keys = set()
    for concept_row in active_concepts:
        is_ordinal = concept_row.get("IS_ORDINAL", False)
        if not is_ordinal:
            # This is a main concept
            concept_id = concept_row.get("_concept_id") or concept_row.get("CONCEPT_ID")
            heritage = concept_row.get("HERITAGE")
            if concept_id is not None:
                key = (normalize_id(concept_id), str(heritage) if heritage is not None and pd.notna(heritage) else None)
                active_main_concept_keys.add(key)
    
    # Filter active_concepts to remove ordinals whose main concept is not active
    filtered_active_concepts = []
    for concept_row in active_concepts:
        is_ordinal = concept_row.get("IS_ORDINAL", False)
        if is_ordinal:
            # Check if the main concept is active
            original_concept_id = concept_row.get("ORIGINAL_CONCEPT_ID")
            heritage = concept_row.get("HERITAGE")
            if original_concept_id is not None:
                key = (normalize_id(original_concept_id), str(heritage) if heritage is not None and pd.notna(heritage) else None)
                if key not in active_main_concept_keys:
                    # Main concept is not active, skip this ordinal
                    continue
        filtered_active_concepts.append(concept_row)
    
    active_concepts = filtered_active_concepts
    
    if not active_concepts:
        fig = go.Figure()
        fig.add_annotation(
            text="No active concepts selected after filtering ordinals.",
            xref="paper", yref="paper",
            x=0.5, y=0.5, showarrow=False,
            font=dict(size=16, color="#666")
        )
        fig.update_layout(
            xaxis=dict(visible=False),
            yaxis=dict(visible=False),
            plot_bgcolor="white"
        )
        return fig, [], {}
    
    # Extract active concept keys first, then filter DataFrames before building lookup.
    # This avoids processing all concepts when only a subset is active (especially with strict filters).
    active_keys = set()
    # Initialize summary_lookup_df for DataFrame-based lookups (replaces dict lookup)
    summary_lookup_df = pd.DataFrame()
    for concept_row in active_concepts:
        concept_id = concept_row.get("_concept_id") or concept_row.get("CONCEPT_ID")
        heritage = concept_row.get("HERITAGE")
        if concept_id is not None:
            cid = normalize_id(concept_id)
            heritage_str = str(heritage) if heritage is not None and pd.notna(heritage) else None
            key = (cid, heritage_str)
            active_keys.add(key)
    
    # Filter concept_summaries to only active concepts using vectorized operations
    # This is much faster when M (total concepts) >> N (active concepts)
    # Note: summary_lookup_df is initialized above, will be populated here
    
    if not concept_summaries.empty:
        # Create normalized CONCEPT_ID and HERITAGE columns for efficient filtering
        # Use vectorized operations where possible
        concept_summaries_filtered = concept_summaries.copy()
        
        # Normalize CONCEPT_ID: convert to string and remove ".0" suffix
        concept_summaries_filtered['_cid_norm'] = concept_summaries_filtered['CONCEPT_ID'].astype(str).str.replace('.0', '', regex=False)
        # Handle NaN values
        concept_summaries_filtered.loc[concept_summaries_filtered['CONCEPT_ID'].isna(), '_cid_norm'] = None
        
        # Normalize HERITAGE: convert to string, handle NaN
        concept_summaries_filtered['_heritage_str'] = concept_summaries_filtered['HERITAGE'].astype(str)
        concept_summaries_filtered.loc[concept_summaries_filtered['HERITAGE'].isna(), '_heritage_str'] = None
        # Replace "nan" string with None
        concept_summaries_filtered.loc[concept_summaries_filtered['_heritage_str'] == 'nan', '_heritage_str'] = None
        
        # Create key tuples for filtering
        concept_summaries_filtered['_key'] = list(zip(
            concept_summaries_filtered['_cid_norm'],
            concept_summaries_filtered['_heritage_str']
        ))
        
        # Filter to only active concepts using vectorized isin()
        concept_summaries_filtered = concept_summaries_filtered[
            concept_summaries_filtered['_key'].isin(active_keys)
        ]
        
        # Use DataFrame indexing with multi-index for efficient lookups.
        # This avoids converting the entire DataFrame to a dictionary.
        if not concept_summaries_filtered.empty:
            concept_summaries_filtered = concept_summaries_filtered.set_index(['_cid_norm', '_heritage_str']).sort_index()
            # Store indexed DataFrame for lookups (will be merged with ordinal summaries)
            summary_lookup_df = concept_summaries_filtered.copy()
    
    # Filter ordinal_summaries to only active ordinals
    if ordinal_summaries is not None and not ordinal_summaries.empty:
        # Create normalized columns for filtering (same approach as above)
        ordinal_summaries_filtered = ordinal_summaries.copy()
        
        # Normalize CONCEPT_ID
        ordinal_summaries_filtered['_cid_norm'] = ordinal_summaries_filtered['CONCEPT_ID'].astype(str).str.replace('.0', '', regex=False)
        ordinal_summaries_filtered.loc[ordinal_summaries_filtered['CONCEPT_ID'].isna(), '_cid_norm'] = None
        
        # Normalize HERITAGE
        ordinal_summaries_filtered['_heritage_str'] = ordinal_summaries_filtered['HERITAGE'].astype(str)
        ordinal_summaries_filtered.loc[ordinal_summaries_filtered['HERITAGE'].isna(), '_heritage_str'] = None
        ordinal_summaries_filtered.loc[ordinal_summaries_filtered['_heritage_str'] == 'nan', '_heritage_str'] = None
        
        # Create key tuples
        ordinal_summaries_filtered['_key'] = list(zip(
            ordinal_summaries_filtered['_cid_norm'],
            ordinal_summaries_filtered['_heritage_str']
        ))
        
        # Filter to only active ordinals
        ordinal_summaries_filtered = ordinal_summaries_filtered[
            ordinal_summaries_filtered['_key'].isin(active_keys)
        ]
        
        # Use DataFrame indexing with multi-index and combine with concept summaries.
        # This avoids converting the entire DataFrame to a dictionary.
        if not ordinal_summaries_filtered.empty:
            ordinal_summaries_filtered = ordinal_summaries_filtered.set_index(['_cid_norm', '_heritage_str']).sort_index()
            # Combine with concept summaries (if any)
            if summary_lookup_df.empty:
                summary_lookup_df = ordinal_summaries_filtered.copy()
            else:
                summary_lookup_df = pd.concat([summary_lookup_df, ordinal_summaries_filtered]).sort_index()
    
    # Build cluster summary lookup if we have cluster data
    # Always build lookup when clustering data exists (for stats display)
    # Only show overlay when a specific cluster is selected (not "all")
    show_cluster_overlay = selected_cluster is not None and selected_cluster != "all" and clustering_summary_matrix is not None
    # Initialize cluster_summary_lookup_df for DataFrame-based lookups
    cluster_summary_lookup_df = pd.DataFrame()
    if clustering_summary_matrix is not None and not clustering_summary_matrix.empty:
        # If a specific cluster is selected, filter to that cluster
        # If "all" is selected, include all clusters (for stats, but no overlay)
        if show_cluster_overlay:
            # Filter to selected cluster before indexing
            filtered_cluster = clustering_summary_matrix[clustering_summary_matrix['cluster'] == selected_cluster]
        else:
            # For "all", include all clusters (but won't show overlay)
            filtered_cluster = clustering_summary_matrix
        
        # Use DataFrame indexing with multi-index for efficient lookups.
        # This avoids converting the entire DataFrame to a dictionary.
        if not filtered_cluster.empty:
            # Normalize CONCEPT_ID and create index
            filtered_cluster = filtered_cluster.copy()
            filtered_cluster['_cid_norm'] = filtered_cluster['CONCEPT_ID'].astype(str).str.replace('.0', '', regex=False)
            filtered_cluster.loc[filtered_cluster['CONCEPT_ID'].isna(), '_cid_norm'] = None
            # Handle ORDINAL - convert to int, default to 0
            filtered_cluster['_ordinal_int'] = filtered_cluster['ORDINAL'].fillna(0).astype(int)
            # Set multi-index for fast lookups (sort to avoid PerformanceWarning)
            cluster_summary_lookup_df = filtered_cluster.set_index(['_cid_norm', '_ordinal_int']).sort_index()
    
    # Group by heritage - use HERITAGE_ORDER for consistency with patient mode
    has_heritage = "HERITAGE" in concept_summaries.columns
    if has_heritage:
        # Get heritages in HERITAGE_ORDER (same as patient mode)
        available_heritages = set([h for h in concept_summaries["HERITAGE"].unique() if h and pd.notna(h)])
        heritages = [h for h in HERITAGE_ORDER if h in available_heritages]
        # Add any heritages not in HERITAGE_ORDER at the end (sorted)
        remaining = sorted([h for h in available_heritages if h not in HERITAGE_ORDER])
        heritages.extend(remaining)
        if not heritages:
            heritages = ["ALL"]
    else:
        heritages = ["ALL"]
    
    # Pre-compute common operations before loop to reduce per-iteration overhead.
    # Pre-compute ordinal suffix dictionary (used in loop).
    ordinal_suffix_map = {1: "1st", 2: "2nd", 3: "3rd"}
    
    # Pre-compute normalized concept data to avoid repeated operations in loop
    normalized_concepts = []
    for concept_row in active_concepts:
        concept_id = concept_row.get("_concept_id") or concept_row.get("CONCEPT_ID")
        if concept_id is None:
            continue
        
        # Pre-normalize concept ID (avoid repeated string operations)
        cid = str(concept_id).replace(".0", "")
        
        # Pre-extract and normalize heritage
        heritage = concept_row.get("HERITAGE")
        heritage_str = str(heritage) if heritage and pd.notna(heritage) else None
        
        # Pre-build lookup key
        key = (cid, heritage_str)
        
        # Pre-extract concept name
        concept_name = concept_row.get("CONCEPT_NAME", f"Concept {concept_id}")
        
        # Pre-compute ordinal flags (avoid repeated type conversions)
        is_ordinal_raw = concept_row.get("IS_ORDINAL")
        is_ordinal = bool(is_ordinal_raw) if is_ordinal_raw is not None and not pd.isna(is_ordinal_raw) else False
        
        ordinal_raw = concept_row.get("ORDINAL")
        ordinal = int(ordinal_raw) if ordinal_raw is not None and not pd.isna(ordinal_raw) else 0
        
        # Pre-compute original concept ID
        original_concept_id = concept_row.get("ORIGINAL_CONCEPT_ID") if is_ordinal else concept_id
        
        # Store pre-computed data
        normalized_concepts.append({
            'concept_id': concept_id,
            'cid': cid,
            'key': key,
            'heritage': heritage,
            'heritage_str': heritage_str,
            'concept_name': concept_name,
            'is_ordinal': is_ordinal,
            'ordinal': ordinal,
            'original_concept_id': original_concept_id
        })
    
    # Prepare data for plotting
    plot_data = []
    
    # Cache has_heritage check (doesn't change in loop)
    heritage_default = "ALL"
    
    for norm_concept in normalized_concepts:
        concept_id = norm_concept['concept_id']
        cid = norm_concept['cid']
        key = norm_concept['key']
        heritage = norm_concept['heritage']
        concept_name = norm_concept['concept_name']
        is_ordinal = norm_concept['is_ordinal']
        ordinal = norm_concept['ordinal']
        original_concept_id = norm_concept['original_concept_id']
        
        # Use DataFrame indexing for efficient lookup
        try:
            if summary_lookup_df.empty or key not in summary_lookup_df.index:
                continue
            summary_row = summary_lookup_df.loc[key]
            # Convert Series to dict for compatibility with existing code
            summary = summary_row.to_dict()
        except (KeyError, IndexError):
            continue
        
        # Parse KDE data with caching to avoid repeated JSON parsing
        kde_x, kde_y = _parse_kde_with_cache(
            concept_id=cid,
            heritage=heritage,
            time_kde_x=summary.get("time_kde_x"),
            time_kde_y=summary.get("time_kde_y")
        )
        
        # For ordinals, use just the ordinal suffix (like patient mode).
        # Use pre-computed ordinal suffix map for efficiency.
        if is_ordinal and ordinal > 0:
            ordinal_suffix = ordinal_suffix_map.get(ordinal, f"{ordinal}th")
            display_name = ordinal_suffix  # Just show ordinal for axis label
            # concept_name already contains the full name with ordinal (e.g., "Pneumonia 1st")
            full_concept_name = concept_name
        else:
            display_name = concept_name
            full_concept_name = concept_name
        
        # Get cluster-specific stats if available
        # Always get cluster stats when clustering data exists (for display in tooltips/stats)
        # Only show overlay when a specific cluster is selected
        cluster_stats = None
        if not cluster_summary_lookup_df.empty:
            # Use DataFrame indexing for efficient cluster stats lookup
            cluster_key = (cid, ordinal)
            try:
                if cluster_key in cluster_summary_lookup_df.index:
                    cluster_stats_row = cluster_summary_lookup_df.loc[cluster_key]
                    # Convert Series to dict for compatibility
                    cluster_stats = cluster_stats_row.to_dict()
            except (KeyError, IndexError):
                pass
        
        plot_data.append({
            "concept_id": concept_id,
            "_concept_id": concept_id,  # Alias used by some lookups
            "CONCEPT_ID": concept_id,   # Uppercase alias
            "concept_name": display_name,  # Use display_name (ordinal suffix for ordinals)
            "full_concept_name": full_concept_name,  # Full name for hover (e.g., "Pneumonia 1st")
            "heritage": heritage if has_heritage else heritage_default,
            "HERITAGE": heritage if has_heritage else heritage_default,  # Uppercase alias
            "kde_x": kde_x,
            "kde_y": kde_y,
            "median": summary.get("time_median"),
            "q1": summary.get("time_q1"),
            "q3": summary.get("time_q3"),
            "min": summary.get("time_min"),
            "max": summary.get("time_max"),
            "count": summary.get("time_count", summary.get("patient_count", 0)),
            "is_ordinal": is_ordinal,
            "IS_ORDINAL": is_ordinal,  # Uppercase alias
            "ordinal": ordinal,
            "ORDINAL": ordinal,  # Uppercase alias
            "original_concept_id": original_concept_id,
            "ORIGINAL_CONCEPT_ID": original_concept_id,  # Uppercase alias
            # Cluster-specific stats
            "cluster_stats": cluster_stats
        })
    
    if not plot_data:
        fig = go.Figure()
        fig.add_annotation(
            text="No data available for selected concepts.",
            xref="paper", yref="paper",
            x=0.5, y=0.5, showarrow=False,
            font=dict(size=16, color="#666")
        )
        return fig, [], {}
    
    # Add main_time_median for proper sorting (same as patient mode)
    # This groups ordinals with their parent concept
    main_concept_medians = {}
    for item in plot_data:
        if not item.get("is_ordinal", False):
            # This is a main concept
            key = (str(item["concept_id"]).replace(".0", ""), item["heritage"])
            main_concept_medians[key] = item.get("median")
    
    # Add main_time_median to all items
    for item in plot_data:
        if item.get("is_ordinal", False):
            # For ordinals, find their parent main concept's median
            key = (str(item.get("original_concept_id", item["concept_id"])).replace(".0", ""), item["heritage"])
            item["main_time_median"] = main_concept_medians.get(key, item.get("median"))
        else:
            # For main concepts, use their own median
            item["main_time_median"] = item.get("median")
            item["ordinal"] = 0  # Ensure main concepts have ordinal 0
    
    # Group by heritage
    heritage_groups = {}
    for item in plot_data:
        h = item["heritage"] or "ALL"
        if h not in heritage_groups:
            heritage_groups[h] = []
        heritage_groups[h].append(item)
    
    # Sort within each heritage group by main_time_median then ordinal (same as patient mode)
    for h in heritage_groups:
        heritage_groups[h].sort(key=lambda x: (x.get("main_time_median") or 0, x.get("ordinal") or 0))
    
    heritages_with_data = [h for h in heritages if h in heritage_groups]
    if not heritages_with_data:
        heritages_with_data = list(heritage_groups.keys())
    
    all_ci_lows = []
    all_ci_highs = []
    for items in heritage_groups.values():
        for item in items:
            ci_low = item.get("q1") if item.get("q1") is not None else item.get("min")
            ci_high = item.get("q3") if item.get("q3") is not None else item.get("max")
            if ci_low is not None:
                all_ci_lows.append(ci_low)
            if ci_high is not None:
                all_ci_highs.append(ci_high)
    
    if all_ci_lows and all_ci_highs:
        x_range = [min(all_ci_lows), max(all_ci_highs)]
    else:
        x_range = None
    
    # Calculate figure height
    total_concepts = sum(len(items) for items in heritage_groups.values())
    height = max(MIN_PLOT_AREA, TOP_MARGIN + BOTTOM_MARGIN + total_concepts * PIXELS_PER_CONCEPT)
    
    # Create subplot
    fig = make_subplots(
        rows=len(heritages_with_data),
        cols=1,
        shared_xaxes=True,
        vertical_spacing=SMALL_GAP / height,
        row_heights=[len(heritage_groups.get(h, [])) for h in heritages_with_data],
        subplot_titles=[format_heritage_label(h) if h != "ALL" else "" for h in heritages_with_data]
    )
    
    all_y_labels = []
    heritage_groups_order = {}
    
    for row_idx, heritage in enumerate(heritages_with_data, start=1):
        items = heritage_groups.get(heritage, [])
        heritage_groups_order[heritage] = items
        
        # Get heritage color for this group
        heritage_color = HERITAGE_COLORS.get(heritage, "#CCCCCC")
        
        # Batch trace collection to reduce Plotly trace count and improve rendering performance.
        # Collect coordinates for batched traces, separated by color (black for main concepts, gray for ordinals).
        violin_x_batch = []
        violin_y_batch = []
        box_x_batch_black = []
        box_y_batch_black = []
        box_x_batch_gray = []
        box_y_batch_gray = []
        whisker_min_x_batch_black = []
        whisker_min_y_batch_black = []
        whisker_min_x_batch_gray = []
        whisker_min_y_batch_gray = []
        whisker_max_x_batch_black = []
        whisker_max_y_batch_black = []
        whisker_max_x_batch_gray = []
        whisker_max_y_batch_gray = []
        median_x_batch_black = []
        median_y_batch_black = []
        median_x_batch_gray = []
        median_y_batch_gray = []
        cluster_box_x_batch = []
        cluster_box_y_batch = []
        cluster_whisker_min_x_batch = []
        cluster_whisker_min_y_batch = []
        cluster_whisker_max_x_batch = []
        cluster_whisker_max_y_batch = []
        # Collect hover trace data (must remain individual for per-concept hover)
        hover_traces = []
        # Cluster median traces (must remain individual for hover)
        cluster_median_traces = []
        
        for y_pos, item in enumerate(items):
            all_y_labels.append(item["concept_name"])
            
            # Build hover text with concept name and mapped source concepts (like patient mode)
            concept_id = item.get("concept_id") or item.get("_concept_id")
            original_concept_id = item.get("original_concept_id")
            is_ordinal = item.get("is_ordinal", False)
            full_name = item.get("full_concept_name", item["concept_name"])
            
            # For ordinals, use the original concept ID to look up mappings
            lookup_id = original_concept_id if is_ordinal and original_concept_id else concept_id
            lookup_id_str = str(lookup_id).replace('.0', '') if lookup_id and '.' in str(lookup_id) else str(lookup_id) if lookup_id else None
            
            # Get mapped source concepts
            mapped_sources = []
            if lookup_id_str and concept_mappings:
                mapped_sources = concept_mappings.get(lookup_id_str, [])
            
            # Build hover text
            hover_lines = [f"<b>{full_name}</b>"]
            if mapped_sources:
                hover_lines.append("<br><b>Mapped from:</b>")
                for source_name in mapped_sources[:10]:  # Limit to 10 to prevent huge tooltips
                    hover_lines.append(f"<br>• {source_name}")
                if len(mapped_sources) > 10:
                    hover_lines.append(f"<br>... and {len(mapped_sources) - 10} more")
            hover_text = "".join(hover_lines) + "<extra></extra>"
            
            # Collect violin data for batching
            kde_x = item.get("kde_x", [])
            kde_y = item.get("kde_y", [])
            
            # Check if kde_x and kde_y are not empty (handles both lists and numpy arrays)
            if kde_x is not None and kde_y is not None and len(kde_x) > 0 and len(kde_y) > 0:
                # Normalize KDE for violin width (we know kde_y is not empty from the check above)
                max_kde = max(kde_y)
                normalized_kde = [k / max_kde * 0.4 for k in kde_y]
                
                # Add violin coordinates to batch (with None separator for disconnected segments)
                if violin_x_batch:  # Add separator if not first
                    violin_x_batch.append(None)
                    violin_y_batch.append(None)
                
                violin_x_batch.extend(list(kde_x) + list(reversed(kde_x)))
                violin_y_batch.extend([y_pos + k for k in normalized_kde] + [y_pos - k for k in reversed(normalized_kde)])
            
            # Collect box plot data
            median = item.get("median")
            q1 = item.get("q1")
            q3 = item.get("q3")
            min_val = item.get("min")
            max_val = item.get("max")
            
            # Determine box color - gray for ordinals, black for main (like patient mode)
            box_color = "#666666" if is_ordinal else "black"
            
            if median is not None:
                # Box coordinates - separate by color
                if q1 is not None and q3 is not None:
                    if is_ordinal:
                        if box_x_batch_gray:  # Add separator if not first
                            box_x_batch_gray.append(None)
                            box_y_batch_gray.append(None)
                        box_x_batch_gray.extend([q1, q3, q3, q1, q1])
                        box_y_batch_gray.extend([y_pos - 0.15, y_pos - 0.15, y_pos + 0.15, y_pos + 0.15, y_pos - 0.15])
                    else:
                        if box_x_batch_black:  # Add separator if not first
                            box_x_batch_black.append(None)
                            box_y_batch_black.append(None)
                        box_x_batch_black.extend([q1, q3, q3, q1, q1])
                        box_y_batch_black.extend([y_pos - 0.15, y_pos - 0.15, y_pos + 0.15, y_pos + 0.15, y_pos - 0.15])
                    
                    # Whiskers - separate by color
                    if min_val is not None:
                        if is_ordinal:
                            if whisker_min_x_batch_gray:  # Add separator
                                whisker_min_x_batch_gray.append(None)
                                whisker_min_y_batch_gray.append(None)
                            whisker_min_x_batch_gray.extend([min_val, q1])
                            whisker_min_y_batch_gray.extend([y_pos, y_pos])
                        else:
                            if whisker_min_x_batch_black:  # Add separator
                                whisker_min_x_batch_black.append(None)
                                whisker_min_y_batch_black.append(None)
                            whisker_min_x_batch_black.extend([min_val, q1])
                            whisker_min_y_batch_black.extend([y_pos, y_pos])
                    
                    if max_val is not None:
                        if is_ordinal:
                            if whisker_max_x_batch_gray:  # Add separator
                                whisker_max_x_batch_gray.append(None)
                                whisker_max_y_batch_gray.append(None)
                            whisker_max_x_batch_gray.extend([q3, max_val])
                            whisker_max_y_batch_gray.extend([y_pos, y_pos])
                        else:
                            if whisker_max_x_batch_black:  # Add separator
                                whisker_max_x_batch_black.append(None)
                                whisker_max_y_batch_black.append(None)
                            whisker_max_x_batch_black.extend([q3, max_val])
                            whisker_max_y_batch_black.extend([y_pos, y_pos])
                
                # Median line - separate by color
                if is_ordinal:
                    if median_x_batch_gray:  # Add separator
                        median_x_batch_gray.append(None)
                        median_y_batch_gray.append(None)
                    median_x_batch_gray.extend([median, median])
                    median_y_batch_gray.extend([y_pos - 0.15, y_pos + 0.15])
                else:
                    if median_x_batch_black:  # Add separator
                        median_x_batch_black.append(None)
                        median_y_batch_black.append(None)
                    median_x_batch_black.extend([median, median])
                    median_y_batch_black.extend([y_pos - 0.15, y_pos + 0.15])
                
                # Hover trace (must remain individual for per-concept hover text)
                hover_traces.append({
                    'x': [median],
                    'y': [y_pos],
                    'hover_text': hover_text
                })
            
            # Collect cluster overlay data
            cluster_stats = item.get("cluster_stats")
            if cluster_stats and show_cluster_overlay:
                cluster_median = cluster_stats.get("time_median")
                cluster_q1 = cluster_stats.get("time_q1")
                cluster_q3 = cluster_stats.get("time_q3")
                cluster_min = cluster_stats.get("time_min")
                cluster_max = cluster_stats.get("time_max")
                cluster_count = cluster_stats.get("patient_count", 0)
                
                if cluster_median is not None and cluster_q1 is not None and cluster_q3 is not None:
                    # Cluster box
                    if cluster_box_x_batch:  # Add separator
                        cluster_box_x_batch.append(None)
                        cluster_box_y_batch.append(None)
                    cluster_box_x_batch.extend([cluster_q1, cluster_q3, cluster_q3, cluster_q1, cluster_q1])
                    cluster_box_y_batch.extend([y_pos - 0.08, y_pos - 0.08, y_pos + 0.08, y_pos + 0.08, y_pos - 0.08])
                    
                    # Cluster whiskers
                    if cluster_min is not None:
                        if cluster_whisker_min_x_batch:  # Add separator
                            cluster_whisker_min_x_batch.append(None)
                            cluster_whisker_min_y_batch.append(None)
                        cluster_whisker_min_x_batch.extend([cluster_min, cluster_q1])
                        cluster_whisker_min_y_batch.extend([y_pos, y_pos])
                    
                    if cluster_max is not None:
                        if cluster_whisker_max_x_batch:  # Add separator
                            cluster_whisker_max_x_batch.append(None)
                            cluster_whisker_max_y_batch.append(None)
                        cluster_whisker_max_x_batch.extend([cluster_q3, cluster_max])
                        cluster_whisker_max_y_batch.extend([y_pos, y_pos])
                    
                    # Cluster median (with hover - keep individual for hover support)
                    cluster_median_traces.append({
                        'x': [cluster_median, cluster_median],
                        'y': [y_pos - 0.08, y_pos + 0.08],
                        'hover_text': f"Cluster - Median: {cluster_median:.1f} days<br>N={cluster_count}<extra></extra>"
                    })
        
        # Add batched violin trace (if any violins)
        if violin_x_batch:
                fig.add_trace(
                    go.Scatter(
                    x=violin_x_batch,
                    y=violin_y_batch,
                        fill="toself",
                        fillcolor=heritage_color,
                        line=dict(color="rgba(0,0,0,0)", width=0),
                        mode="lines",
                        showlegend=False,
                        hoverinfo="skip",
                        opacity=0.6
                    ),
                    row=row_idx, col=1
                )
            
        # Add batched box traces (separated by color)
        if box_x_batch_black:
            fig.add_trace(
                go.Scatter(
                    x=box_x_batch_black,
                    y=box_y_batch_black,
                    fill="toself",
                    fillcolor="rgba(0,0,0,0)",  # Transparent fill
                    line=dict(color="black", width=0.6),
                    mode="lines",
                    showlegend=False,
                    hoverinfo="skip"
                ),
                row=row_idx, col=1
            )
        
        if box_x_batch_gray:
                    fig.add_trace(
                        go.Scatter(
                    x=box_x_batch_gray,
                    y=box_y_batch_gray,
                            fill="toself",
                    fillcolor="rgba(0,0,0,0)",  # Transparent fill
                    line=dict(color="#666666", width=0.6),
                            mode="lines",
                            showlegend=False,
                            hoverinfo="skip"
                        ),
                        row=row_idx, col=1
                    )
                    
        # Add batched whisker traces (separated by color)
        if whisker_min_x_batch_black:
                        fig.add_trace(
                go.Scatter(
                    x=whisker_min_x_batch_black,
                    y=whisker_min_y_batch_black,
                    mode="lines",
                    line=dict(color="black", width=0.6),
                    showlegend=False,
                    hoverinfo="skip"
                ),
                            row=row_idx, col=1
                        )
        
        if whisker_min_x_batch_gray:
                        fig.add_trace(
                go.Scatter(
                    x=whisker_min_x_batch_gray,
                    y=whisker_min_y_batch_gray,
                    mode="lines",
                    line=dict(color="#666666", width=0.6),
                    showlegend=False,
                    hoverinfo="skip"
                ),
                            row=row_idx, col=1
                        )
                
        if whisker_max_x_batch_black:
                fig.add_trace(
                    go.Scatter(
                    x=whisker_max_x_batch_black,
                    y=whisker_max_y_batch_black,
                        mode="lines",
                    line=dict(color="black", width=0.6),
                        showlegend=False,
                        hoverinfo="skip"
                    ),
                    row=row_idx, col=1
                )
                
        if whisker_max_x_batch_gray:
                fig.add_trace(
                    go.Scatter(
                    x=whisker_max_x_batch_gray,
                    y=whisker_max_y_batch_gray,
                    mode="lines",
                    line=dict(color="#666666", width=0.6),
                    showlegend=False,
                    hoverinfo="skip"
                ),
                row=row_idx, col=1
            )
        
        # Add batched median lines (separated by color)
        if median_x_batch_black:
            fig.add_trace(
                go.Scatter(
                    x=median_x_batch_black,
                    y=median_y_batch_black,
                    mode="lines",
                    line=dict(color="black", width=1.5),
                    showlegend=False,
                    hoverinfo="skip"
                ),
                row=row_idx, col=1
            )
        
        if median_x_batch_gray:
            fig.add_trace(
                go.Scatter(
                    x=median_x_batch_gray,
                    y=median_y_batch_gray,
                    mode="lines",
                    line=dict(color="#666666", width=1.5),
                    showlegend=False,
                    hoverinfo="skip"
                ),
                row=row_idx, col=1
            )
                
        # Add individual hover traces (required for per-concept hover text)
        for hover_data in hover_traces:
                fig.add_trace(
                    go.Scatter(
                    x=hover_data['x'],
                    y=hover_data['y'],
                        mode='markers',
                        marker=dict(size=30, opacity=0),  # Invisible but hoverable
                        showlegend=False,
                    hovertemplate=hover_data['hover_text'],
                        hoverlabel=dict(
                            bgcolor='white',
                            bordercolor='#ccc',
                            font=dict(color='black', size=12),
                            align='left'
                        )
                    ),
                    row=row_idx, col=1
                )
            
        # Add batched cluster overlay traces
        if cluster_box_x_batch and show_cluster_overlay:
                    fig.add_trace(
                        go.Scatter(
                    x=cluster_box_x_batch,
                    y=cluster_box_y_batch,
                            fill="toself",
                            fillcolor="rgba(0,0,0,0)",  # Transparent fill
                            line=dict(color="#E74C3C", width=1.5),  # Red for cluster
                            mode="lines",
                            showlegend=False,
                            hoverinfo="skip"
                        ),
                        row=row_idx, col=1
                    )
                    
        if cluster_whisker_min_x_batch and show_cluster_overlay:
                        fig.add_trace(
                go.Scatter(
                    x=cluster_whisker_min_x_batch,
                    y=cluster_whisker_min_y_batch,
                    mode="lines",
                    line=dict(color="#E74C3C", width=1.5),
                    showlegend=False,
                    hoverinfo="skip"
                ),
                            row=row_idx, col=1
                        )
        
        if cluster_whisker_max_x_batch and show_cluster_overlay:
                        fig.add_trace(
                go.Scatter(
                    x=cluster_whisker_max_x_batch,
                    y=cluster_whisker_max_y_batch,
                    mode="lines",
                    line=dict(color="#E74C3C", width=1.5),
                    showlegend=False,
                    hoverinfo="skip"
                ),
                            row=row_idx, col=1
                        )
                    
        # Add individual cluster median traces (required for hover)
        if cluster_median_traces and show_cluster_overlay:
            for cluster_median_data in cluster_median_traces:
                    fig.add_trace(
                        go.Scatter(
                        x=cluster_median_data['x'],
                        y=cluster_median_data['y'],
                            mode="lines",
                            line=dict(color="#E74C3C", width=2),
                            showlegend=False,
                        hovertemplate=cluster_median_data['hover_text']
                        ),
                        row=row_idx, col=1
                    )
        
        # Update y-axis - wrap long concept names for display
        y_labels = [wrap_text(item["concept_name"], max_chars=60) for item in items]
        fig.update_yaxes(
            tickmode='array',
            tickvals=list(range(len(items))),
            ticktext=y_labels,
            range=[len(items) - 0.5, -0.5],
            title="",
            fixedrange=True,
            row=row_idx, col=1
        )
    
    # Add legend traces if cluster overlay is shown (like patient mode)
    if show_cluster_overlay:
        # Add dummy traces for legend
        fig.add_trace(
            go.Scatter(
                x=[None], y=[None],
                mode='lines',
                line=dict(color="black", width=2),
                name="All Patients",
                showlegend=True
            )
        )
        fig.add_trace(
            go.Scatter(
                x=[None], y=[None],
                mode='lines',
                line=dict(color="#E74C3C", width=2),
                name=f"Cluster {selected_cluster}" if selected_cluster else "Selected Cluster",
                showlegend=True
            )
        )
    
    # Update layout
    fig.update_layout(
        height=height,
        showlegend=show_cluster_overlay,  # Show legend when cluster is selected
        legend=dict(
            orientation="h",
            yanchor="bottom",
            y=1.02,
            xanchor="right",
            x=1
        ) if show_cluster_overlay else {},
        margin=dict(l=250, r=20, t=TOP_MARGIN + (30 if show_cluster_overlay else 0), b=BOTTOM_MARGIN),
        plot_bgcolor="white",
        hovermode="closest"
    )
    
    # Update x-axes (shared, so update all) and disable interactions - same as patient mode
    for row_idx in range(1, len(heritages_with_data) + 1):
        fig.update_xaxes(
            range=x_range,
            title_text="Time to Event (days)" if row_idx == len(heritages_with_data) else "",
            fixedrange=True,  # Disable zoom/pan
            row=row_idx,
            col=1
        )
    
    return fig, all_y_labels, heritage_groups_order

