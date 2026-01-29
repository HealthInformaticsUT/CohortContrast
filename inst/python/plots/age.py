"""
Age statistics and plot visualization.
"""

from typing import Dict, List, Optional, Tuple, Set
import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from scipy import stats

from config.constants import HERITAGE_ORDER


def calculate_age_stats(
    active_concepts: List[Dict],
    data_patients: pd.DataFrame,
    data_initial: pd.DataFrame,
    data_person: pd.DataFrame,
    heritage_groups_order: Dict[str, List[Dict]],
    filter_patient_ids: Optional[Set] = None
) -> Dict[Tuple, Dict]:
    """
    Calculate average age for each concept based on people who have that concept.
    
    Args:
        active_concepts: List of active concept dictionaries
        data_patients: DataFrame with patient data including CONCEPT_ID and PERSON_ID
        data_initial: DataFrame with COHORT_START_DATE and SUBJECT_ID
        data_person: DataFrame with YEAR_OF_BIRTH and PERSON_ID
        heritage_groups_order: Dictionary mapping heritage to list of items in display order
        filter_patient_ids: Optional set of patient IDs to filter to (for cluster comparisons)
        
    Returns:
        Dictionary mapping (heritage, concept_id, ordinal) -> age statistics
    """
    age_stats = {}
    
    if data_patients.empty or data_initial.empty or data_person.empty:
        return age_stats
    
    # Filter to target cohort only (same patient can be in both target and control)
    data_patients_target = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
    
    # If filter_patient_ids provided, further filter to those patients
    if filter_patient_ids is not None:
        data_patients_target = data_patients_target[
            data_patients_target["PERSON_ID"].isin(filter_patient_ids)
        ].copy()
    
    # Helper function to normalize concept ID
    def _normalize_concept_id(concept_id):
        if concept_id is None:
            return None
        concept_id_str = str(concept_id).replace(".0", "")
        try:
            return int(float(concept_id_str))
        except (ValueError, TypeError):
            return concept_id_str
    
    # Create a mapping from SUBJECT_ID to COHORT_START_DATE
    # IMPORTANT: Filter to target cohort first to avoid using control cohort dates
    # (same person can be in both target and control with different dates)
    # Also create mappings with multiple key types for robust lookup (int, float, str)
    target_initial = data_initial[data_initial['COHORT_DEFINITION_ID'] == 'target']
    initial_map = {}
    for _, row in target_initial.iterrows():
        sid = row['SUBJECT_ID']
        date = row['COHORT_START_DATE']
        # Store with multiple key types for robust lookup
        initial_map[sid] = date
        if sid is not None:
            try:
                initial_map[int(float(str(sid).replace('.0', '')))] = date
            except (ValueError, TypeError):
                pass
            initial_map[str(sid).replace('.0', '')] = date
    
    # Create a mapping from PERSON_ID to YEAR_OF_BIRTH with multiple key types
    person_map = {}
    for _, row in data_person.iterrows():
        pid = row['PERSON_ID']
        year = row['YEAR_OF_BIRTH']
        # Store with multiple key types for robust lookup
        person_map[pid] = year
        if pid is not None:
            try:
                person_map[int(float(str(pid).replace('.0', '')))] = year
            except (ValueError, TypeError):
                pass
            person_map[str(pid).replace('.0', '')] = year
    
    # Process each concept
    for heritage, items in heritage_groups_order.items():
        for item in items:
            is_ordinal = item.get("is_ordinal", False)
            
            if is_ordinal:
                concept_id = _normalize_concept_id(item.get("original_concept_id"))
            else:
                concept_id = _normalize_concept_id(
                    item.get("concept_id") or item.get("_concept_id")
                )
            
            if concept_id is None:
                continue
            
            # Get person IDs who have this concept
            # For ordinals, we need to filter to people who have at least that many occurrences
            ordinal_num = item.get("ordinal", 0)
            
            if is_ordinal and ordinal_num > 0:
                # For ordinals, filter to people who have at least ordinal_num occurrences
                # Use target cohort only
                concept_data = data_patients_target[
                    (data_patients_target['CONCEPT_ID'] == concept_id) &
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
                        valid_times = [t for t in time_list if t is not None and not (isinstance(t, (int, float)) and pd.isna(t))]
                        return sorted(list(set(valid_times)))
                    if isinstance(time_list, (int, float)) and not pd.isna(time_list):
                        return [float(time_list)]
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
                # Use target cohort only
                concept_patients = data_patients_target[
                    (data_patients_target['CONCEPT_ID'] == concept_id) &
                    (data_patients_target['PREVALENCE'] > 0)
                ]['PERSON_ID'].unique()
            
            if len(concept_patients) == 0:
                continue
            
            # Calculate ages for these patients
            ages = []
            for person_id in concept_patients:
                # Get birth year - try multiple key types
                birth_year = person_map.get(person_id)
                if birth_year is None or (isinstance(birth_year, float) and pd.isna(birth_year)):
                    # Try normalized int key
                    try:
                        birth_year = person_map.get(int(float(str(person_id).replace('.0', ''))))
                    except (ValueError, TypeError):
                        pass
                if birth_year is None or (isinstance(birth_year, float) and pd.isna(birth_year)):
                    continue
                
                # Get cohort start date (use SUBJECT_ID = PERSON_ID) - try multiple key types
                cohort_start = initial_map.get(person_id)
                if cohort_start is None or (isinstance(cohort_start, float) and pd.isna(cohort_start)):
                    # Try normalized int key
                    try:
                        cohort_start = initial_map.get(int(float(str(person_id).replace('.0', ''))))
                    except (ValueError, TypeError):
                        pass
                if cohort_start is None or (isinstance(cohort_start, float) and pd.isna(cohort_start)):
                    continue
                
                # Calculate age at cohort start date
                try:
                    cohort_date = pd.to_datetime(cohort_start)
                    birth_year_int = int(birth_year)
                    
                    # Calculate age as year difference
                    # Since we only have birth year (not exact date), this is approximate
                    age = cohort_date.year - birth_year_int
                    
                    if age >= 0 and age <= 150:  # Sanity check (reasonable age range)
                        ages.append(age)
                except (ValueError, TypeError, AttributeError):
                    continue
            
            if len(ages) == 0:
                continue
            
            # Calculate statistics
            ages_array = np.array(ages)
            mean_age = np.mean(ages_array)
            std_age = np.std(ages_array, ddof=1) if len(ages) > 1 else 0
            n = len(ages)
            
            # Calculate 95% confidence interval
            # Clamp to reasonable age bounds to avoid extreme values from small samples
            MIN_REASONABLE_AGE = 0
            MAX_REASONABLE_AGE = 120
            
            from scipy import stats
            if n > 1:
                se = std_age / np.sqrt(n)
                t_critical = stats.t.ppf(0.975, df=n-1)
                ci_low = max(MIN_REASONABLE_AGE, mean_age - t_critical * se)
                ci_high = min(MAX_REASONABLE_AGE, mean_age + t_critical * se)
            else:
                ci_low = mean_age
                ci_high = mean_age
            
            # Store statistics
            key = (heritage or "unknown", concept_id, item.get("ordinal", 0))
            age_stats[key] = {
                "mean_age": mean_age,
                "ci_low": ci_low,
                "ci_high": ci_high,
                "n": n,
                "std": std_age
            }
    
    return age_stats


def create_age_plot(
    heritage_groups_order: Dict[str, List[Dict]],
    age_stats: Dict[Tuple, Dict],
    overall_avg_age: Optional[float] = None,
    cluster_age_stats: Optional[Dict[Tuple, Dict]] = None,
    show_cluster_overlay: bool = False,
    show_error_bars: bool = True
) -> go.Figure:
    """
    Create an age plot showing average age for each concept.
    
    Args:
        heritage_groups_order: Dictionary mapping heritage to list of items in display order
        age_stats: Dictionary mapping (heritage, concept_id, ordinal) -> age statistics
        overall_avg_age: Overall average age for the study (used to determine significance)
        cluster_age_stats: Optional cluster-specific age statistics (same format as age_stats)
        show_cluster_overlay: Whether to show cluster overlay points
        show_error_bars: Whether to show error bars (False for summary mode)
        
    Returns:
        Plotly figure with age plot
    """
    if not age_stats:
        fig = go.Figure()
        fig.add_annotation(
            text="No age data available.",
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
    
    # Helper function to normalize concept ID
    def _normalize_concept_id(concept_id):
        if concept_id is None:
            return None
        concept_id_str = str(concept_id).replace(".0", "")
        try:
            return int(float(concept_id_str))
        except (ValueError, TypeError):
            return concept_id_str
    
    # Get heritages in order (only those that have data)
    heritages_with_data = [h for h in HERITAGE_ORDER if h in heritage_groups_order]
    
    # First pass: Calculate overall average age and determine x-axis range
    all_ages = []
    all_ci_lows = []
    all_ci_highs = []
    
    for heritage, items in heritage_groups_order.items():
        for item in items:
            is_ordinal_item = item.get("is_ordinal", False)
            heritage_key = heritage or "unknown"
            
            if is_ordinal_item:
                concept_id_key = _normalize_concept_id(item.get("original_concept_id"))
            else:
                concept_id_key = _normalize_concept_id(
                    item.get("concept_id") or item.get("_concept_id")
                )
            
            lookup_key = (heritage_key, concept_id_key, item.get("ordinal", 0))
            stats = age_stats.get(lookup_key)
            
            if stats:
                all_ages.append(stats["mean_age"])
                all_ci_lows.append(stats["ci_low"])
                all_ci_highs.append(stats["ci_high"])
    
    if not all_ages:
        fig = go.Figure()
        fig.add_annotation(
            text="No age data available.",
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
    
    # Use passed overall_avg_age if available, otherwise calculate from mean ages
    if overall_avg_age is None:
        overall_avg_age = np.mean(all_ages) if all_ages else None
    
    # Determine x-axis range (include all data points and CIs with some padding)
    min_age = min(min(all_ages), min(all_ci_lows)) if all_ci_lows else min(all_ages)
    max_age = max(max(all_ages), max(all_ci_highs)) if all_ci_highs else max(all_ages)
    age_range = max_age - min_age
    if age_range == 0:
        # If all ages are the same, add some padding
        x_min = min_age - 5
        x_max = max_age + 5
    else:
        x_min = min_age - age_range * 0.1  # 10% padding
        x_max = max_age + age_range * 0.1  # 10% padding
    
    # Calculate row heights: 30px per concept per heritage panel
    row_heights = [len(heritage_groups_order[h]) * 30 for h in heritages_with_data]
    
    # Create subplot figure
    fig = make_subplots(
        rows=len(heritages_with_data),
        cols=1,
        row_heights=row_heights,
        shared_xaxes=True,
        vertical_spacing=0.002,
        subplot_titles=([None] * len(heritages_with_data))
    )
    
    # Process each heritage
    for row_idx, heritage in enumerate(heritages_with_data, start=1):
        items_from_order = heritage_groups_order[heritage]
        
        for y_idx, item_from_order in enumerate(items_from_order):
            # Handle potential NaN values from concat
            is_ordinal_raw = item_from_order.get("is_ordinal")
            is_ordinal_item = bool(is_ordinal_raw) if is_ordinal_raw is not None and not pd.isna(is_ordinal_raw) else False
            
            heritage_key = heritage or "unknown"
            
            if is_ordinal_item:
                concept_id_key = _normalize_concept_id(item_from_order.get("original_concept_id"))
            else:
                concept_id_key = _normalize_concept_id(
                    item_from_order.get("concept_id") or item_from_order.get("_concept_id")
                )
            
            ordinal_raw = item_from_order.get("ordinal")
            ordinal_val = int(ordinal_raw) if ordinal_raw is not None and not pd.isna(ordinal_raw) else 0
            
            lookup_key = (heritage_key, concept_id_key, ordinal_val)
            stats = age_stats.get(lookup_key)
            
            if not stats:
                continue
            
            mean_age = stats["mean_age"]
            ci_low = stats["ci_low"]
            ci_high = stats["ci_high"]
            is_ordinal = is_ordinal_item  # Use the already-computed value
            
            # Determine if statistically significant (CI does not include overall average)
            # Gray if CI includes overall average (not significant), blue if it doesn't (significant)
            is_significant = False
            if (overall_avg_age is not None and 
                not pd.isna(overall_avg_age) and 
                not pd.isna(ci_low) and 
                not pd.isna(ci_high)):
                # Check if overall average is within the CI
                # If ci_low <= overall_avg_age <= ci_high, then NOT significant (gray)
                # Otherwise, significant (blue)
                if ci_low <= overall_avg_age <= ci_high:
                    is_significant = False  # Gray - CI includes overall average
                else:
                    is_significant = True  # Blue - CI does NOT include overall average
            
            # Configure visual properties
            # Blue if significant (CI doesn't include overall average), gray if not significant
            # Different marker style for ordinals: hollow/outlined markers
            if is_ordinal:
                color = "#2E86AB" if is_significant else "#666666"  # Blue if significant, gray if not
                error_bar_width = 1.5  # Thinner for ordinals
                marker_symbol = "square"  # Square marker for ordinals
                # Hollow marker: white fill with colored border
                marker_line_width = 2
                marker_fill_color = "white"
            else:
                color = "#2E86AB" if is_significant else "#666666"  # Blue if significant, gray if not
                error_bar_width = 2  # Normal width for main concepts
                marker_symbol = "circle"  # Circle for main concepts
                # Filled marker
                marker_line_width = 1
                marker_fill_color = color
            
            # Determine y position - nudge "All" down if showing cluster overlay
            y_nudge = 0.15 if show_cluster_overlay else 0
            y_pos_all = y_idx + y_nudge  # "All" is nudged down
            
            # Set opacity based on overlay mode
            opacity_all = 0.4 if show_cluster_overlay else 1.0
            
            # Add point with optional error bars for "All" data
            scatter_kwargs = dict(
                x=[mean_age],
                y=[y_pos_all],
                mode='markers',
                marker=dict(
                    size=8,
                    color=marker_fill_color,
                    symbol=marker_symbol,
                    line=dict(width=marker_line_width, color=color),
                    opacity=opacity_all
                ),
                showlegend=False,
                hovertemplate=f"All - Age: {mean_age:.1f} years<br>95% CI: [{ci_low:.1f}, {ci_high:.1f}]<extra></extra>",
                opacity=opacity_all
            )
            
            # Only add error bars if requested
            if show_error_bars:
                scatter_kwargs["error_x"] = dict(
                    type='data',
                    symmetric=False,
                    array=[ci_high - mean_age],
                    arrayminus=[mean_age - ci_low],
                    color=color,
                    width=error_bar_width
                )
            
            fig.add_trace(go.Scatter(**scatter_kwargs), row=row_idx, col=1)
            
            # Add cluster overlay point if available
            if show_cluster_overlay and cluster_age_stats:
                cluster_stats = cluster_age_stats.get(lookup_key)
                if cluster_stats:
                    cluster_mean = cluster_stats["mean_age"]
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
                        showlegend=False,
                        hovertemplate=f"Cluster - Age: {cluster_mean:.1f} years<br>95% CI: [{cluster_ci_low:.1f}, {cluster_ci_high:.1f}]<extra></extra>"
                    )
                    
                    # Only add error bars if requested
                    if show_error_bars:
                        cluster_scatter_kwargs["error_x"] = dict(
                            type='data',
                            symmetric=False,
                            array=[cluster_ci_high - cluster_mean],
                            arrayminus=[cluster_mean - cluster_ci_low],
                            color=cluster_color,
                            width=error_bar_width
                        )
                    
                    fig.add_trace(go.Scatter(**cluster_scatter_kwargs), row=row_idx, col=1)
            
    # Calculate total plot height
    plot_height = sum(row_heights) + (len(heritages_with_data) - 1) * 10 + 160
    
    # Update layout
    fig.update_layout(
        title="Average Age",
        height=plot_height,
        showlegend=False,
        plot_bgcolor="white",
        paper_bgcolor="white",
        margin=dict(l=50, r=50, t=80, b=25),
        dragmode=False,
        hovermode=False
    )
    
    # Update x-axes (shared, show age, with same range for all)
    # First, set the range for all x-axes to ensure they're shared
    for row_idx in range(1, len(heritages_with_data) + 1):
        fig.update_xaxes(
            title_text="Age (years)" if row_idx == len(heritages_with_data) else "",
            range=[x_min, x_max],  # Shared x-axis range
            fixedrange=True,
            row=row_idx,
            col=1
        )
    
    # Add vertical lines for overall average age after layout is set
    # Use shapes with row/col parameters for subplots
    for row_idx, heritage in enumerate(heritages_with_data, start=1):
        num_concepts = len(heritage_groups_order[heritage])
        
        # Add shape using row/col (Plotly should handle subplot references automatically)
        fig.add_shape(
            type="line",
            x0=overall_avg_age,
            x1=overall_avg_age,
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
            ticktext=[""] * num_concepts,
            range=[num_concepts - 0.5, -0.5],
            showticklabels=False,
            showgrid=False,
            title="",
            fixedrange=True,
            row=row_idx,
            col=1
        )
    
    return fig


