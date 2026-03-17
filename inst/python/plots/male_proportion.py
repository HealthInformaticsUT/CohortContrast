"""
Male proportion statistics and plot visualization.
"""

from typing import Dict, List, Optional, Tuple, Set
import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from scipy import stats

from config.constants import HERITAGE_ORDER


def calculate_male_prop_stats(
    active_concepts: List[Dict],
    data_patients: pd.DataFrame,
    data_initial: pd.DataFrame,
    data_person: pd.DataFrame,
    heritage_groups_order: Dict[str, List[Dict]],
    filter_patient_ids: Optional[Set] = None
) -> Dict[Tuple, Dict]:
    """
    Calculate male proportion for each concept based on people who have that concept.
    
    Args:
        active_concepts: List of active concept dictionaries
        data_patients: DataFrame with patient data including CONCEPT_ID and PERSON_ID
        data_initial: DataFrame with COHORT_START_DATE and SUBJECT_ID
        data_person: DataFrame with GENDER_CONCEPT_ID and PERSON_ID
        heritage_groups_order: Dictionary mapping heritage to list of items in display order
        filter_patient_ids: Optional set of patient IDs to filter to (for cluster comparisons)
        
    Returns:
        Dictionary mapping (heritage, concept_id, ordinal) -> male proportion statistics
    """
    male_prop_stats = {}
    
    if data_patients.empty or data_initial.empty or data_person.empty:
        return male_prop_stats
    
    # Filter to target cohort only (same patient can be in both target and control)
    data_patients_target = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
    if "CONCEPT_ID" not in data_patients_target.columns:
        return male_prop_stats
    
    # If filter_patient_ids provided, further filter to those patients
    if filter_patient_ids is not None:
        data_patients_target = data_patients_target[
            data_patients_target["PERSON_ID"].isin(filter_patient_ids)
        ].copy()
    
    # Normalize concept IDs as strings so comparisons are robust to mixed int/str dtypes.
    def _normalize_concept_id_local(concept_id):
        if concept_id is None:
            return None
        concept_id_str = str(concept_id).strip().replace(".0", "")
        return concept_id_str if concept_id_str and concept_id_str.lower() != "nan" else None

    data_patients_target["__CONCEPT_ID_NORM"] = (
        data_patients_target["CONCEPT_ID"]
        .astype(str)
        .str.strip()
        .str.replace(".0", "", regex=False)
    )
    
    # Create a mapping from PERSON_ID to GENDER_CONCEPT_ID with multiple key types
    # 8507 = male, 8532 = female
    person_map = {}
    for _, row in data_person.iterrows():
        pid = row['PERSON_ID']
        gender = row['GENDER_CONCEPT_ID']
        # Store with multiple key types for robust lookup
        person_map[pid] = gender
        if pid is not None:
            try:
                person_map[int(float(str(pid).replace('.0', '')))] = gender
            except (ValueError, TypeError):
                pass
            person_map[str(pid).replace('.0', '')] = gender
    
    # Process each concept (using target cohort only)
    
    for heritage, items in heritage_groups_order.items():
        for item in items:
            is_ordinal = item.get("is_ordinal", False)
            
            if is_ordinal:
                concept_id = _normalize_concept_id_local(item.get("original_concept_id"))
            else:
                concept_id = _normalize_concept_id_local(
                    item.get("concept_id") or item.get("_concept_id")
                )
            
            if concept_id is None:
                continue
            
            # Get person IDs who have this concept
            ordinal_num = item.get("ordinal", 0)
            
            if is_ordinal and ordinal_num > 0:
                # For ordinals, filter to people who have at least ordinal_num occurrences
                # Direct comparison (concept_id is normalized to match DataFrame type)
                # Use target cohort only
                concept_data = data_patients_target[
                    (data_patients_target['__CONCEPT_ID_NORM'] == concept_id) &
                    (data_patients_target['PREVALENCE'] > 0)
                ].copy()
                
                if concept_data.empty:
                    continue
                
                # Helper function to get unique occurrences
                def get_unique_occurrences(time_list):
                    if time_list is None:
                        return []
                    if isinstance(time_list, np.ndarray):
                        if time_list.size == 0:
                            return []
                        valid_times = time_list[~np.isnan(time_list)]
                        return sorted(np.unique(valid_times).tolist())
                    if isinstance(time_list, (list, tuple)):
                        if len(time_list) == 0:
                            return []
                        valid_times = [t for t in time_list if not pd.isna(t)]
                        return sorted(list(set(valid_times)))
                    return []
                
                # Extract occurrences for each person
                concept_data["OCCURRENCES"] = concept_data["TIME_TO_EVENT"].apply(get_unique_occurrences)
                
                # Filter to people who have at least ordinal_num occurrences
                concept_data = concept_data[concept_data["OCCURRENCES"].apply(len) >= ordinal_num].copy()
                
                if concept_data.empty:
                    continue
                
                # Get person IDs who have this specific ordinal occurrence
                concept_patients = concept_data['PERSON_ID'].unique()
            else:
                # For main concepts, get all people with the concept
                # Direct comparison (concept_id is normalized to match DataFrame type)
                # Use target cohort only
                concept_patients = data_patients_target[
                    (data_patients_target['__CONCEPT_ID_NORM'] == concept_id) &
                    (data_patients_target['PREVALENCE'] > 0)
                ]['PERSON_ID'].unique()
            
            if len(concept_patients) == 0:
                continue
            
            # Calculate male proportions for these patients
            male_indicators = []
            for person_id in concept_patients:
                # Try multiple key types for robust lookup
                gender_concept_id = person_map.get(person_id)
                if gender_concept_id is None or (isinstance(gender_concept_id, float) and pd.isna(gender_concept_id)):
                    # Try normalized int key
                    try:
                        gender_concept_id = person_map.get(int(float(str(person_id).replace('.0', ''))))
                    except (ValueError, TypeError):
                        pass
                if gender_concept_id is None or (isinstance(gender_concept_id, float) and pd.isna(gender_concept_id)):
                    continue
                
                # 8507 = male
                is_male = (gender_concept_id == 8507)
                male_indicators.append(1 if is_male else 0)
            
            if len(male_indicators) == 0:
                continue
            
            # Calculate statistics
            male_array = np.array(male_indicators)
            mean_male_prop = np.mean(male_array)
            n = len(male_array)
            
            # Calculate 95% confidence interval for proportion
            from scipy import stats
            if n > 1:
                # Use normal approximation for proportion CI
                se = np.sqrt(mean_male_prop * (1 - mean_male_prop) / n)
                z_critical = stats.norm.ppf(0.975)  # 95% CI
                ci_low = max(0, mean_male_prop - z_critical * se)
                ci_high = min(1, mean_male_prop + z_critical * se)
            else:
                ci_low = mean_male_prop
                ci_high = mean_male_prop
            
            # Store statistics - convert concept_id to string for key consistency
            key = (heritage or "unknown", str(concept_id), item.get("ordinal", 0))
            male_prop_stats[key] = {
                "mean_male_prop": mean_male_prop,
                "ci_low": ci_low,
                "ci_high": ci_high,
                "n": n
            }
    
    return male_prop_stats


def create_male_prop_plot(
    heritage_groups_order: Dict[str, List[Dict]],
    male_prop_stats: Dict[Tuple, Dict],
    overall_avg_male_prop: Optional[float] = None,
    cluster_male_prop_stats: Optional[Dict[Tuple, Dict]] = None,
    show_cluster_overlay: bool = False,
    show_error_bars: bool = True
) -> go.Figure:
    """
    Create a male proportion plot showing average male proportion for each concept.
    
    Args:
        heritage_groups_order: Dictionary mapping heritage to list of items in display order
        male_prop_stats: Dictionary mapping (heritage, concept_id, ordinal) -> male proportion statistics
        overall_avg_male_prop: Overall average male proportion for the study (used to determine significance)
        cluster_male_prop_stats: Optional cluster-specific male proportion statistics
        show_cluster_overlay: Whether to show cluster overlay points
        show_error_bars: Whether to show error bars (False for summary mode)
        
    Returns:
        Plotly figure with male proportion plot, x_min, x_max, and overall_avg_male_prop
    """
    # Get heritages in order (only those that have data)
    heritages_with_data = [h for h in HERITAGE_ORDER if h in heritage_groups_order]
    
    if not heritages_with_data:
        fig = go.Figure()
        fig.update_layout(
            title="Male Proportion",
            height=200,
            showlegend=False,
            plot_bgcolor="white",
            paper_bgcolor="white",
            margin=dict(l=50, r=50, t=80, b=25),
            dragmode=False,
            hovermode=False
        )
        return fig, 0.0, 1.0, None
    
    # Calculate plot height
    total_concepts = sum(len(items) for items in heritage_groups_order.values())
    PIXELS_PER_CONCEPT = 30
    MIN_PLOT_AREA = 200
    TOP_MARGIN = 25
    BOTTOM_MARGIN = 25
    SMALL_GAP = 60
    
    # Calculate heritage gaps (3x the small gap)
    num_heritages = len(heritages_with_data)
    heritage_gaps = SMALL_GAP * 3 * max(0, num_heritages - 1)
    
    total_plot_area = max(MIN_PLOT_AREA, total_concepts * PIXELS_PER_CONCEPT) + heritage_gaps
    plot_height = TOP_MARGIN + total_plot_area + BOTTOM_MARGIN
    
    # Create subplots for each heritage
    num_rows = len(heritages_with_data)
    row_heights = [len(heritage_groups_order[h]) * 30 for h in heritages_with_data]
    
    fig = make_subplots(
        rows=num_rows,
        cols=1,
        row_heights=row_heights,
        shared_xaxes=True,
        shared_yaxes=False,
        vertical_spacing=0.002,
        subplot_titles=([None] * num_rows)
    )
    
    # Track x-axis range
    all_mean_props = []
    all_ci_lows = []
    all_ci_highs = []
    
    # Plot data for each heritage
    for row_idx, heritage in enumerate(heritages_with_data, start=1):
        items = heritage_groups_order[heritage]
        y_labels = []
        y_positions = []
        
        for y_idx, item in enumerate(items):
            # Handle potential NaN values from concat
            is_ordinal_raw = item.get("is_ordinal")
            is_ordinal = bool(is_ordinal_raw) if is_ordinal_raw is not None and not pd.isna(is_ordinal_raw) else False
            
            # Use same normalization as in calculate_male_prop_stats
            # Helper function to normalize concept ID - same as in calculate_age_stats
            def _normalize_concept_id_local(concept_id):
                if concept_id is None:
                    return None
                concept_id_str = str(concept_id).replace(".0", "")
                try:
                    return int(float(concept_id_str))
                except (ValueError, TypeError):
                    return concept_id_str
            
            concept_id_normalized = _normalize_concept_id_local(
                item.get("concept_id") or item.get("_concept_id") if not is_ordinal
                else item.get("original_concept_id")
            )
            
            ordinal_raw = item.get("ordinal")
            ordinal = int(ordinal_raw) if ordinal_raw is not None and not pd.isna(ordinal_raw) else 0
            
            # Look up statistics - try int key first (summary mode), then string key (patient mode)
            key_int = (heritage or "unknown", concept_id_normalized, ordinal)
            key_str = (heritage or "unknown", str(concept_id_normalized), ordinal)
            stats = male_prop_stats.get(key_int) or male_prop_stats.get(key_str)
            
            if stats is None:
                continue
            
            mean_male_prop = stats["mean_male_prop"]
            ci_low = stats["ci_low"]
            ci_high = stats["ci_high"]
            
            all_mean_props.append(mean_male_prop)
            all_ci_lows.append(ci_low)
            all_ci_highs.append(ci_high)
            
            # Determine if statistically significant (CI does not include overall average)
            is_significant = False
            if (overall_avg_male_prop is not None and 
                not pd.isna(overall_avg_male_prop) and 
                not pd.isna(ci_low) and 
                not pd.isna(ci_high)):
                # Check if overall average is within the CI
                if ci_low <= overall_avg_male_prop <= ci_high:
                    is_significant = False  # Gray - CI includes overall average
                else:
                    is_significant = True  # Blue - CI does NOT include overall average
            
            # Configure visual properties
            if is_ordinal:
                color = "#2E86AB" if is_significant else "#666666"
                error_bar_width = 1.5
                marker_symbol = "square"
                marker_line_width = 2
                marker_fill_color = "white"
            else:
                color = "#2E86AB" if is_significant else "#666666"
                error_bar_width = 2
                marker_symbol = "circle"
                marker_line_width = 1
                marker_fill_color = color
            
            # Determine y position - nudge "All" down if showing cluster overlay
            y_nudge = 0.15 if show_cluster_overlay else 0
            y_pos_all = y_idx + y_nudge  # "All" is nudged down
            
            # Set opacity based on overlay mode
            opacity_all = 0.4 if show_cluster_overlay else 1.0
            
            # Add point with optional error bars for "All" data
            scatter_kwargs = dict(
                x=[mean_male_prop],
                y=[y_pos_all],
                mode='markers',
                marker=dict(
                    size=8,
                    color=marker_fill_color,
                    symbol=marker_symbol,
                    line=dict(width=marker_line_width, color=color),
                    opacity=opacity_all
                ),
                line=dict(dash="dash" if is_ordinal else "solid", color=color, width=1),
                name=item.get("concept_name", "Unknown"),
                hovertemplate=f"<b>{item.get('concept_name', 'Unknown')}</b><br>" +
                              f"All - Male %: {mean_male_prop:.2%}<br>" +
                              f"95% CI: [{ci_low:.2%}, {ci_high:.2%}]<extra></extra>",
                showlegend=False,
                opacity=opacity_all
            )
            
            # Only add error bars if requested
            if show_error_bars:
                scatter_kwargs["error_x"] = dict(
                    type='data',
                    symmetric=False,
                    array=[ci_high - mean_male_prop],
                    arrayminus=[mean_male_prop - ci_low],
                    width=error_bar_width,
                    color=color
                )
            
            fig.add_trace(go.Scatter(**scatter_kwargs), row=row_idx, col=1)
            
            # Add cluster overlay point if available
            if show_cluster_overlay and cluster_male_prop_stats:
                cluster_stats = cluster_male_prop_stats.get(key_int) or cluster_male_prop_stats.get(key_str)
                if cluster_stats:
                    cluster_mean = cluster_stats["mean_male_prop"]
                    cluster_ci_low = cluster_stats["ci_low"]
                    cluster_ci_high = cluster_stats["ci_high"]
                    
                    # Nudge cluster point up
                    y_pos_cluster = y_idx - y_nudge
                    
                    # Red color for cluster
                    cluster_color = "#E74C3C"
                    
                    cluster_scatter_kwargs = dict(
                        x=[cluster_mean],
                        y=[y_pos_cluster],
                        mode='markers',
                        marker=dict(
                            size=8,
                            color=cluster_color if not is_ordinal else "white",
                            symbol=marker_symbol,
                            line=dict(width=2, color=cluster_color)
                        ),
                        name=item.get("concept_name", "Unknown"),
                        hovertemplate=f"<b>{item.get('concept_name', 'Unknown')}</b><br>" +
                                      f"Cluster - Male %: {cluster_mean:.2%}<br>" +
                                      f"95% CI: [{cluster_ci_low:.2%}, {cluster_ci_high:.2%}]<extra></extra>",
                        showlegend=False
                    )
                    
                    # Only add error bars if requested
                    if show_error_bars:
                        cluster_scatter_kwargs["error_x"] = dict(
                            type='data',
                            symmetric=False,
                            array=[cluster_ci_high - cluster_mean],
                            arrayminus=[cluster_mean - cluster_ci_low],
                            width=error_bar_width,
                            color=cluster_color
                        )
                    
                    fig.add_trace(go.Scatter(**cluster_scatter_kwargs), row=row_idx, col=1)
            
            # Store y label and position
            y_labels.append(item.get("concept_name", "Unknown"))
            y_positions.append(y_idx)
        
        # Update y-axis for this heritage
        if y_labels:
            fig.update_yaxes(
                tickmode='array',
                tickvals=y_positions,
                ticktext=y_labels,
                row=row_idx,
                col=1
            )
    
    # Calculate shared x-axis range
    # Always ensure the range includes [0, 1] or at least shows all data points with padding
    if all_mean_props:
        min_prop = min(min(all_mean_props), min(all_ci_lows)) if all_ci_lows else min(all_mean_props)
        max_prop = max(max(all_mean_props), max(all_ci_highs)) if all_ci_highs else max(all_mean_props)
        prop_range = max_prop - min_prop
        if prop_range == 0:
            # All values are the same - ensure we show the full range with padding
            if min_prop == 1.0:
                # All at 100% - show from 0.95 to 1.0 to avoid "double 100%" appearance
                x_min = 0.95
                x_max = 1.0
            elif min_prop == 0.0:
                # All at 0% - show from 0.0 to 0.05
                x_min = 0.0
                x_max = 0.05
            else:
                # Some middle value - pad around it
                x_min = max(0, min_prop - 0.1)
                x_max = min(1, max_prop + 0.1)
        else:
            # Add padding but ensure we don't go outside [0, 1]
            x_min = max(0, min_prop - prop_range * 0.1)
            x_max = min(1, max_prop + prop_range * 0.1)
            # If max is at 1.0, ensure it's visible but don't show beyond 1.0
            if max_prop >= 0.99:
                x_max = 1.0
                # If all values are very close to 1.0, show a tighter range
                if min_prop >= 0.99:
                    x_min = 0.95
            # If min is at 0.0, ensure it's visible
            if min_prop <= 0.01:
                x_min = 0.0
    else:
        x_min = 0.0
        x_max = 1.0
    
    # Update layout
    fig.update_layout(
        title="Male Proportion",
        height=plot_height,
        showlegend=False,
        plot_bgcolor="white",
        paper_bgcolor="white",
        margin=dict(l=50, r=50, t=80, b=25),
        dragmode=False,
        hovermode=False
    )
    
    # Update x-axes (shared, show proportion, with same range for all)
    for row_idx in range(1, len(heritages_with_data) + 1):
        fig.update_xaxes(
            title_text="Male Proportion" if row_idx == len(heritages_with_data) else "",
            range=[x_min, x_max],
            fixedrange=True,
            row=row_idx,
            col=1,
            tickformat='.0%'  # Format as percentage
        )
    
    # Add vertical lines for overall average male proportion after layout is set
    if overall_avg_male_prop is not None:
        for row_idx, heritage in enumerate(heritages_with_data, start=1):
            num_concepts = len(heritage_groups_order[heritage])
            
            fig.add_shape(
                type="line",
                x0=overall_avg_male_prop,
                x1=overall_avg_male_prop,
                y0=-0.5,
                y1=num_concepts - 0.5,
                line=dict(color="gray", width=1, dash="dot"),
                layer="below",
                row=row_idx,
                col=1
            )
    
    # Update y-axes (no labels, same range as composite plot)
    for row_idx, heritage in enumerate(heritages_with_data, start=1):
        num_concepts = len(heritage_groups_order[heritage])
        fig.update_yaxes(
            tickmode='array',
            tickvals=list(range(num_concepts)),
            ticktext=[item.get("concept_name", "Unknown") for item in heritage_groups_order[heritage]],
            row=row_idx,
            col=1,
            autorange='reversed'  # Reverse y-axis to match composite plot
        )
    
    return fig, x_min, x_max, overall_avg_male_prop

