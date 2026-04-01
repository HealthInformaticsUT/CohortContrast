"""
Clustering visualization plots.
"""

from typing import Dict, List, Optional, Tuple, Set
import textwrap
import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import plotly.express as px
from dash import html, dcc

from config.constants import (
    HERITAGE_ORDER,
    HERITAGE_COLORS,
    CLUSTER_COLORS,
    PIXELS_PER_CONCEPT,
    MIN_PLOT_AREA,
    TOP_MARGIN,
    BOTTOM_MARGIN,
    SMALL_GAP,
)
from utils.helpers import (
    normalize_concept_id as _normalize_concept_id,
    get_unique_occurrences,
    normalize_concept_name,
)


def _deduplicate_axis_labels(
    labels: List[str],
    concept_ids: List[str],
    concept_info: Dict[str, Dict],
) -> List[str]:
    """
    Keep labels readable while ensuring they are unique for Plotly heatmap axes.
    """
    counts: Dict[str, int] = {}
    for label in labels:
        counts[label] = counts.get(label, 0) + 1

    deduplicated: List[str] = []
    used: Set[str] = set()
    for label, cid in zip(labels, concept_ids):
        candidate = label
        if counts.get(label, 0) > 1 or candidate in used:
            heritage = concept_info.get(cid, {}).get("heritage")
            candidate = f"{label} [{heritage}]" if heritage else f"{label} [{cid}]"
            if candidate in used:
                candidate = f"{label} [{cid}]"
            if candidate in used:
                suffix = 2
                next_candidate = f"{candidate} #{suffix}"
                while next_candidate in used:
                    suffix += 1
                    next_candidate = f"{candidate} #{suffix}"
                candidate = next_candidate

        used.add(candidate)
        deduplicated.append(candidate)

    return deduplicated


def create_clustering_heatmap(
    clustering_results: Dict,
    heritage_groups_order: Dict[str, List[Dict]],
    y_labels_order: List[str]
) -> go.Figure:
    """
    Create a heatmap visualization of clustering results, structured like other plots.
    
    Args:
        clustering_results: Dictionary with summary_matrix, patient_assignments, etc.
        heritage_groups_order: Dictionary mapping heritage to list of items in display order
        y_labels_order: List of concept names in master plot order
        
    Returns:
        Plotly figure with heatmap structured by heritage groups
    """
    # Convert from store format (list of dicts) back to DataFrame if needed
    summary_matrix_data = clustering_results.get('summary_matrix', [])
    patient_assignments_data = clustering_results.get('patient_assignments', [])
    
    if isinstance(summary_matrix_data, list):
        summary_matrix = pd.DataFrame(summary_matrix_data) if summary_matrix_data else pd.DataFrame()
    else:
        summary_matrix = summary_matrix_data if isinstance(summary_matrix_data, pd.DataFrame) else pd.DataFrame()
    
    if isinstance(patient_assignments_data, list):
        patient_assignments = pd.DataFrame(patient_assignments_data) if patient_assignments_data else pd.DataFrame()
    else:
        patient_assignments = patient_assignments_data if isinstance(patient_assignments_data, pd.DataFrame) else pd.DataFrame()
    
    if summary_matrix.empty:
        # Return empty figure with same structure as other plots
        heritages_with_data = [h for h in HERITAGE_ORDER if h in heritage_groups_order]
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
        fig.update_layout(
            height=max(200, sum(row_heights) + 50),
            showlegend=False,
            plot_bgcolor="white",
            paper_bgcolor="white",
            margin=dict(l=50, r=50, t=25, b=25)
        )
        return fig
    
    # Get cluster counts - first try pre-computed (for summary mode), then from patient_assignments
    cluster_counts = clustering_results.get('cluster_counts', {})
    if not cluster_counts and not patient_assignments.empty and 'cluster' in patient_assignments.columns:
        cluster_counts = patient_assignments['cluster'].value_counts().to_dict()
    all_clusters = sorted(summary_matrix['cluster'].unique())
    
    # Get heritages in order
    heritages_with_data = [h for h in HERITAGE_ORDER if h in heritage_groups_order]
    
    # Calculate plot height
    total_concepts = sum(len(items) for items in heritage_groups_order.values())
    PIXELS_PER_CONCEPT = 30
    MIN_PLOT_AREA = 200
    TOP_MARGIN = 25
    BOTTOM_MARGIN = 25
    SMALL_GAP = 60
    
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
    
    # Create mapping from concept name to cluster data
    # Use normalized names for matching
    concept_cluster_map = {}
    for _, row in summary_matrix.iterrows():
        concept_name = normalize_concept_name(row['concept_name'])
        cluster = row['cluster']
        key = (concept_name, cluster)
        concept_cluster_map[key] = {
            'prevalence': row['prevalence'],
            'count_category': row['count_category'],
            'median_days': row['median_days']
        }
    
    # Plot data for each heritage
    for row_idx, heritage in enumerate(heritages_with_data, start=1):
        items = heritage_groups_order[heritage]
        
        for y_idx, item in enumerate(items):
            concept_name_raw = item.get("concept_name", "Unknown")
            concept_name = normalize_concept_name(concept_name_raw)
            is_ordinal = item.get("is_ordinal", False)
            
            # For each cluster, add a rectangle
            for cluster_idx, cluster in enumerate(all_clusters):
                key = (concept_name, cluster)
                cluster_data = concept_cluster_map.get(key, {})
                
                # Try alternative matching if not found (for ordinals with different naming)
                if not cluster_data and is_ordinal:
                    # Try matching without ordinal suffix
                    base_name = concept_name_raw.split('(')[0].strip() if '(' in concept_name_raw else concept_name
                    for cluster_key, data in concept_cluster_map.items():
                        if cluster_key[1] == cluster:
                            cluster_concept_name = cluster_key[0]
                            # Check if base names match
                            if normalize_concept_name(base_name) == normalize_concept_name(cluster_concept_name.split('(')[0].strip() if '(' in cluster_concept_name else cluster_concept_name):
                                cluster_data = data
                                break
                
                prevalence = cluster_data.get('prevalence', 0.0)
                count_category = cluster_data.get('count_category', '1')
                median_days = cluster_data.get('median_days', np.nan)
                
                # Color based on prevalence (light gray to dark blue)
                # Use Viridis-like color scale: light gray (low) to dark blue (high)
                if prevalence == 0:
                    color = 'rgb(240, 240, 240)'  # Light gray
                else:
                    # Interpolate from light gray to dark blue
                    # Use a blue gradient: rgb(30, 136, 171) at 100%
                    intensity = prevalence
                    color_r = int(240 - (240 - 30) * intensity)
                    color_g = int(240 - (240 - 136) * intensity)
                    color_b = int(240 - (240 - 171) * intensity)
                    color = f'rgb({color_r}, {color_g}, {color_b})'
                
                # Add heatmap cell as large square marker (background)
                fig.add_trace(go.Scatter(
                    x=[cluster_idx],
                    y=[y_idx],
                    mode='markers',
                    marker=dict(
                        size=60,  # Large square for heatmap cell
                        color=color,
                        symbol='square',
                        line=dict(width=1, color='white')
                    ),
                    showlegend=False,
                    hovertext=f"Concept: {concept_name}<br>Cluster: {cluster}<br>Prevalence: {prevalence:.2%}<br>Count: {count_category}<br>Median days: {median_days:.0f}" if not pd.isna(median_days) else f"Concept: {concept_name}<br>Cluster: {cluster}<br>Prevalence: {prevalence:.2%}<br>Count: {count_category}",
                    hoverinfo='text',
                    row=row_idx,
                    col=1
                ))
                
                # Add count category marker in center (on top of background)
                if count_category == "1":
                    marker_color = "green"
                    marker_symbol = "circle"
                elif count_category == "2":
                    marker_color = "yellow"
                    marker_symbol = "circle"
                else:  # "3+"
                    marker_color = "red"
                    marker_symbol = "circle"
                
                if is_ordinal:
                    marker_symbol = "triangle-up"
                
                fig.add_trace(go.Scatter(
                    x=[cluster_idx],
                    y=[y_idx],
                    mode='markers',
                    marker=dict(
                        size=8,
                        color=marker_color,
                        symbol=marker_symbol,
                        line=dict(width=1, color='black')
                    ),
                    showlegend=False,
                    hovertext=f"Concept: {concept_name}<br>Cluster: {cluster}<br>Prevalence: {prevalence:.2%}<br>Count: {count_category}<br>Median days: {median_days:.0f}" if not pd.isna(median_days) else f"Concept: {concept_name}<br>Cluster: {cluster}<br>Prevalence: {prevalence:.2%}<br>Count: {count_category}",
                    hoverinfo='text',
                    row=row_idx,
                    col=1
                ))
                
                # Add prevalence text on left
                fig.add_annotation(
                    x=cluster_idx - 0.35,
                    y=y_idx,
                    text=f"<b>{prevalence:.0%}</b>",
                    showarrow=False,
                    font=dict(size=9, color='black'),
                    xanchor='left',
                    yanchor='middle',
                    row=row_idx,
                    col=1
                )
                
                # Add median days on right if available
                if not pd.isna(median_days):
                    fig.add_annotation(
                        x=cluster_idx + 0.35,
                        y=y_idx,
                        text=f"{median_days:.0f}d",
                        showarrow=False,
                        font=dict(size=8, color='gray'),
                        xanchor='right',
                        yanchor='middle',
                        row=row_idx,
                        col=1
                    )
    
    # Update layout
    fig.update_layout(
        title="",
        height=plot_height,
        showlegend=False,
        plot_bgcolor="white",
        paper_bgcolor="white",
        margin=dict(l=50, r=50, t=25, b=25),
        dragmode=False,
        hovermode=False
    )
    
    # Update x-axes (shared, show clusters)
    cluster_labels = [f"{c}<br>(n={cluster_counts.get(c, 0)})" for c in all_clusters]
    for row_idx in range(1, num_rows + 1):
        fig.update_xaxes(
            tickmode='array',
            tickvals=list(range(len(all_clusters))),
            ticktext=cluster_labels if row_idx == num_rows else [""] * len(all_clusters),
            title_text="Cluster" if row_idx == num_rows else "",
            fixedrange=True,
            row=row_idx,
            col=1
        )
    
    # Update y-axes (no labels, same range as composite plot)
    for row_idx, heritage in enumerate(heritages_with_data, start=1):
        num_concepts = len(heritage_groups_order[heritage])
        fig.update_yaxes(
            tickmode='array',
            tickvals=list(range(num_concepts)),
            ticktext=[""] * num_concepts,  # No labels to match other columns
            range=[num_concepts - 0.5, -0.5],  # Reversed to match composite plot
            showticklabels=False,
            showgrid=False,
            fixedrange=True,
            row=row_idx,
            col=1
        )
    
    return fig


def create_cluster_prevalence_plots(
    clustering_results: Dict,
    dashboard_data: List[Dict],
    data_patients: pd.DataFrame,
    heritage_groups_order: Dict[str, List[Dict]],
    y_labels_order: List[str]
) -> go.Figure:
    """
    Create prevalence/enrichment plots for each cluster, similar to the composite plot.
    
    Args:
        clustering_results: Dictionary with summary_matrix, patient_assignments, optimal_cluster_count
        dashboard_data: Full dashboard data to get concept info
        data_patients: Patient data to calculate control prevalence
        heritage_groups_order: Dictionary mapping heritage to list of items in display order
        y_labels_order: List of concept names in master plot order
        
    Returns:
        Plotly figure with subplots (one per cluster) showing prevalence/enrichment
    """
    if not clustering_results:
        fig = go.Figure()
        fig.add_annotation(
            text="No clustering results available. Run clustering first.",
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
    
    # Convert from store format (list of dicts) back to DataFrame if needed
    summary_matrix_data = clustering_results.get('summary_matrix', [])
    patient_assignments_data = clustering_results.get('patient_assignments', [])
    optimal_k = clustering_results.get('optimal_cluster_count', 2)
    is_summary_mode = clustering_results.get('_mode') == 'summary'
    
    if isinstance(summary_matrix_data, list):
        summary_matrix = pd.DataFrame(summary_matrix_data) if summary_matrix_data else pd.DataFrame()
    else:
        summary_matrix = summary_matrix_data if isinstance(summary_matrix_data, pd.DataFrame) else pd.DataFrame()
    
    # Get cluster counts - first try pre-computed (for summary mode), then from patient_assignments
    cluster_counts = clustering_results.get('cluster_counts', {})
    
    if isinstance(patient_assignments_data, list):
        patient_assignments_df = pd.DataFrame(patient_assignments_data) if patient_assignments_data else pd.DataFrame()
    else:
        patient_assignments_df = patient_assignments_data if isinstance(patient_assignments_data, pd.DataFrame) else pd.DataFrame()
    
    # If cluster_counts not pre-computed, calculate from patient_assignments
    if not cluster_counts and not patient_assignments_df.empty and 'cluster' in patient_assignments_df.columns:
        cluster_counts = patient_assignments_df['cluster'].value_counts().to_dict()
    
    if summary_matrix.empty:
        fig = go.Figure()
        fig.add_annotation(
            text="No clustering data available.",
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
    
    # Build concept map
    concept_map = {}
    for row in dashboard_data:
        concept_id = row.get("_concept_id") or row.get("CONCEPT_ID")
        if concept_id is not None:
            concept_id_normalized = _normalize_concept_id(concept_id)
            concept_map[concept_id_normalized] = row
            concept_map[str(concept_id)] = row
    
    # Extend summary matrix with ordinals from active concepts
    # Skip this for summary mode - ordinals are pre-computed in the summary_matrix
    ordinal_rows_to_add = []
    
    # In summary mode, ordinal data is already in summary_matrix from precompute
    # Only compute on-the-fly in patient mode where we have patient assignments
    if not is_summary_mode and not patient_assignments_df.empty:
        df_target = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
        
        # Get existing concept_ids in summary matrix to avoid duplicates
        existing_concept_ids = set(summary_matrix['concept_id'].astype(str).unique()) if 'concept_id' in summary_matrix.columns else set()
    
        # Process ordinals from heritage_groups_order
        
        ordinal_count = 0
        for heritage, items in heritage_groups_order.items():
            for item in items:
                if not item.get("is_ordinal", False):
                    continue
                
                ordinal_count += 1
                # Try different field names that might be in the item
                concept_id = item.get("concept_id") or item.get("_concept_id") or item.get("CONCEPT_ID")
                concept_name = item.get("concept_name", "Unknown")
                original_concept_id = item.get("original_concept_id") or item.get("ORIGINAL_CONCEPT_ID")
                ordinal = item.get("ordinal") or item.get("ORDINAL", 0)
                
                # If concept_id is None but we have original_concept_id and ordinal, look it up from dashboard_data
                if concept_id is None:
                    if original_concept_id and ordinal > 0:
                        # Look up the ordinal concept_id from dashboard_data using original_concept_id and ordinal
                        for dash_row in dashboard_data:
                            dash_original = dash_row.get("ORIGINAL_CONCEPT_ID")
                            dash_ordinal = dash_row.get("ORDINAL", 0)
                            dash_heritage = dash_row.get("HERITAGE")
                            if (dash_original and str(dash_original) == str(original_concept_id) and 
                                dash_heritage == heritage and dash_ordinal == ordinal):
                                concept_id = dash_row.get("_concept_id") or dash_row.get("CONCEPT_ID")
                                break
                    elif ordinal > 0:
                        # Try to find by ordinal name (1st, 2nd, etc.) and find the main concept it belongs to
                        # Look backwards in items to find the main concept
                        item_idx = items.index(item) if item in items else -1
                        if item_idx > 0:
                            for i in range(item_idx - 1, -1, -1):
                                prev_item = items[i]
                                if not prev_item.get("is_ordinal", False):
                                    # This is a main concept
                                    main_concept_id = prev_item.get("concept_id") or prev_item.get("_concept_id") or prev_item.get("CONCEPT_ID")
                                    if main_concept_id:
                                        # Look up ordinal for this main concept
                                        for dash_row in dashboard_data:
                                            dash_original = dash_row.get("ORIGINAL_CONCEPT_ID")
                                            dash_ordinal = dash_row.get("ORDINAL", 0)
                                            dash_heritage = dash_row.get("HERITAGE")
                                            ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(dash_ordinal, f"{dash_ordinal}th")
                                            if (dash_original and str(dash_original) == str(main_concept_id) and 
                                                dash_heritage == heritage and ordinal_suffix == concept_name):
                                                concept_id = dash_row.get("_concept_id") or dash_row.get("CONCEPT_ID")
                                                original_concept_id = dash_row.get("ORIGINAL_CONCEPT_ID", main_concept_id)
                                                ordinal = dash_ordinal
                                                break
                                        if concept_id:
                                            break
                                    if concept_id:
                                        break
                
                # If still missing, try to find by matching ordinal name with main concept
                if concept_id is None:
                    # Find the main concept in the same heritage group that this ordinal belongs to
                    # Look for the main concept that comes before this ordinal in the items list
                    main_concept_for_ordinal = None
                    item_idx = items.index(item) if item in items else -1
                    if item_idx > 0:
                        # Look backwards for the main concept
                        for i in range(item_idx - 1, -1, -1):
                            prev_item = items[i]
                            if not prev_item.get("is_ordinal", False):
                                # This is a main concept
                                main_concept_id = prev_item.get("concept_id") or prev_item.get("_concept_id") or prev_item.get("CONCEPT_ID")
                                if main_concept_id:
                                    # Look up in dashboard_data to find ordinal concepts for this main concept
                                    for dash_row in dashboard_data:
                                        dash_original = dash_row.get("ORIGINAL_CONCEPT_ID")
                                        dash_ordinal = dash_row.get("ORDINAL", 0)
                                        dash_heritage = dash_row.get("HERITAGE")
                                        if (dash_original and str(dash_original) == str(main_concept_id) and 
                                            dash_heritage == heritage and dash_ordinal > 0):
                                            # Check if this matches our ordinal name
                                            ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(dash_ordinal, f"{dash_ordinal}th")
                                            if ordinal_suffix == concept_name or (ordinal > 0 and dash_ordinal == ordinal):
                                                concept_id = dash_row.get("_concept_id") or dash_row.get("CONCEPT_ID")
                                                if not original_concept_id:
                                                    original_concept_id = dash_row.get("ORIGINAL_CONCEPT_ID", main_concept_id)
                                                if ordinal == 0:
                                                    ordinal = dash_ordinal
                                                main_concept_for_ordinal = main_concept_id
                                                break
                                        if main_concept_for_ordinal:
                                            break
                                if main_concept_for_ordinal:
                                    break
                
                if concept_id is None:
                    continue
                
                # Skip if already in summary matrix
                if str(concept_id) in existing_concept_ids:
                    continue
                
                # Get concept_info if we don't have original_concept_id or ordinal yet
                if not original_concept_id or ordinal == 0:
                    concept_info = concept_map.get(str(concept_id)) or concept_map.get(_normalize_concept_id(concept_id))
                    if concept_info:
                        if not original_concept_id:
                            original_concept_id = concept_info.get("ORIGINAL_CONCEPT_ID", concept_id)
                        if ordinal == 0:
                            ordinal = concept_info.get("ORDINAL", 0)
                
                if ordinal == 0:
                    continue
                
                if not original_concept_id:
                    original_concept_id = concept_id
                
                # Filter data for this original concept (same as composite plot)
                if "HERITAGE" in df_target.columns and heritage is not None:
                    concept_data = df_target[
                        (df_target["CONCEPT_ID"] == original_concept_id) &
                        (df_target["HERITAGE"] == heritage)
                    ].copy()
                else:
                    concept_data = df_target[df_target["CONCEPT_ID"] == original_concept_id].copy()
                
                if concept_data.empty:
                    continue
                
                # Get unique occurrences for each person (same as composite plot)
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
                
                # Group by person to get one value per person
                person_occurrences = concept_data.groupby("PERSON_ID")["TIME_VALUE"].first()
                patients_with_ordinal = set(person_occurrences.index.tolist())
                
                # Calculate prevalence and median days for each cluster
                for cluster_label in [f'C{i+1}' for i in range(optimal_k)]:
                    cluster_patients = set(patient_assignments_df[patient_assignments_df['cluster'] == cluster_label]['patient_id'].tolist())
                    total_cluster_patients = len(cluster_patients)
                    
                    if total_cluster_patients == 0:
                        prevalence = 0.0
                        median_days = np.nan
                    else:
                        patients_with_ordinal_in_cluster = len(patients_with_ordinal & cluster_patients)
                        prevalence = patients_with_ordinal_in_cluster / total_cluster_patients
                        
                        # Calculate median days for this cluster
                        cluster_patient_ids = list(patients_with_ordinal & cluster_patients)
                        if cluster_patient_ids:
                            cluster_times = person_occurrences[person_occurrences.index.isin(cluster_patient_ids)]
                            median_days = cluster_times.median()
                            if pd.isna(median_days):
                                median_days = np.nan
                        else:
                            median_days = np.nan
                    
                    # Get concept name
                    concept_name = item.get("concept_name", f"Concept {concept_id}")
                    
                    ordinal_rows_to_add.append({
                        'concept_id': str(concept_id),
                        'concept_name': concept_name,
                        'cluster': cluster_label,
                        'prevalence': prevalence,
                        'count_category': "1",  # Default for ordinals
                        'median_days': median_days
                    })
        
        # Add ordinal rows to summary matrix
        if ordinal_rows_to_add:
            ordinal_df = pd.DataFrame(ordinal_rows_to_add)
            summary_matrix = pd.concat([summary_matrix, ordinal_df], ignore_index=True)
    
    # Get control cohort (target cohort) for enrichment calculation
    df_target = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
    control_total_patients = df_target['PERSON_ID'].nunique()
    
    # Calculate control prevalence for each concept
    # Use the same logic as in create_prevalence_plot
    control_prevalence_map = {}
    
    # Build concept map from dashboard data
    concept_map = {}
    for row in dashboard_data:
        concept_id = row.get("_concept_id") or row.get("CONCEPT_ID")
        if concept_id is not None:
            concept_id_normalized = _normalize_concept_id(concept_id)
            concept_map[concept_id_normalized] = row
            concept_map[str(concept_id)] = row
    
    # Calculate control prevalence for each unique concept in summary matrix
    processed_concepts = set()
    for _, row in summary_matrix.iterrows():
        concept_id = row['concept_id']
        concept_name = normalize_concept_name(row['concept_name'])
        
        # Skip if already processed
        if (concept_name, concept_id) in processed_concepts:
            continue
        processed_concepts.add((concept_name, concept_id))
        
        # Get concept info from dashboard
        concept_info = concept_map.get(str(concept_id)) or concept_map.get(_normalize_concept_id(concept_id))
        if not concept_info:
            continue
        
        original_concept_id = concept_info.get('ORIGINAL_CONCEPT_ID', concept_id)
        heritage = concept_info.get('HERITAGE')
        is_ordinal = concept_info.get('IS_ORDINAL', False)
        ordinal = concept_info.get('ORDINAL', 0)
        
        # Calculate control prevalence (same logic as in create_prevalence_plot)
        if heritage and "HERITAGE" in df_target.columns:
            concept_data = df_target[
                (df_target["CONCEPT_ID"] == original_concept_id) &
                (df_target["HERITAGE"] == heritage)
            ].copy()
        else:
            concept_data = df_target[df_target["CONCEPT_ID"] == original_concept_id].copy()
        
        if is_ordinal and ordinal > 0:
            # For ordinals: count patients with at least N occurrences
            person_counts = concept_data.groupby('PERSON_ID').size()
            patients_with_concept = (person_counts >= ordinal).sum()
        else:
            # For main concepts: count patients with any occurrence
            patients_with_concept = concept_data['PERSON_ID'].nunique()
        
        control_prevalence = patients_with_concept / control_total_patients if control_total_patients > 0 else 0.0
        control_prevalence_map[(concept_name, concept_id)] = control_prevalence
    
    # Get heritages in order
    heritages_with_data = [h for h in HERITAGE_ORDER if h in heritage_groups_order]
    num_rows = len(heritages_with_data)
    
    # Calculate plot height
    total_concepts = sum(len(items) for items in heritage_groups_order.values())
    PIXELS_PER_CONCEPT = 30
    MIN_PLOT_AREA = 200
    TOP_MARGIN = 60
    BOTTOM_MARGIN = 25
    SMALL_GAP = 60
    
    heritage_gaps = SMALL_GAP * 3 * max(0, num_rows - 1)
    total_plot_area = max(MIN_PLOT_AREA, total_concepts * PIXELS_PER_CONCEPT) + heritage_gaps
    plot_height = TOP_MARGIN + total_plot_area + BOTTOM_MARGIN
    
    # Create subplots: one column per cluster, rows for heritages
    row_heights = [len(heritage_groups_order[h]) * 30 for h in heritages_with_data]
    
    # Create figure with optimal_k columns (one per cluster)
    from plotly.subplots import make_subplots
    
    # Create column titles with cluster counts
    column_titles = []
    for i in range(optimal_k):
        cluster_label = f'C{i+1}'
        count = cluster_counts.get(cluster_label, 0)
        column_titles.append(f"Cluster {i+1} (n={count})")
    
    fig = make_subplots(
        rows=num_rows,
        cols=optimal_k,
        row_heights=row_heights,
        column_titles=column_titles,
        shared_xaxes=True,
        shared_yaxes=False,
        vertical_spacing=0.05,  # Increased for visible gaps between heritage groups
        horizontal_spacing=0.05,
        subplot_titles=([None] * (num_rows * optimal_k))
    )
    
    # Constants for prevalence color scale (white to blue gradient)
    MAX_PREVALENCE = 1.0  # 100%
    
    # Create mapping from concept name to cluster data (for main concepts)
    # Also create mapping by (heritage, original_concept_id, ordinal) for ordinals (same as composite plot)
    concept_cluster_map = {}
    ordinal_cluster_map = {}  # Map (heritage, original_concept_id, ordinal, cluster) -> data
    concept_id_cluster_map = {}  # Map (concept_id, cluster) -> data (for direct matching)
    
    # Build concept map from dashboard_data for ordinal lookup
    dashboard_concept_map = {}
    for row in dashboard_data:
        concept_id = row.get("_concept_id") or row.get("CONCEPT_ID")
        if concept_id is not None:
            concept_id_normalized = _normalize_concept_id(concept_id)
            dashboard_concept_map[concept_id_normalized] = row
            dashboard_concept_map[str(concept_id)] = row
    
    count_category_map = {}
    
    is_ordinal_col = 'IS_ORDINAL' in summary_matrix.columns or 'is_ordinal' in summary_matrix.columns
    original_id_col = 'ORIGINAL_CONCEPT_ID' if 'ORIGINAL_CONCEPT_ID' in summary_matrix.columns else ('original_concept_id' if 'original_concept_id' in summary_matrix.columns else None)
    ordinal_col = 'ORDINAL' if 'ORDINAL' in summary_matrix.columns else ('ordinal' if 'ordinal' in summary_matrix.columns else None)
    patient_count_col = 'patient_count' if 'patient_count' in summary_matrix.columns else ('PATIENT_COUNT' if 'PATIENT_COUNT' in summary_matrix.columns else None)
    
    if not summary_matrix.empty and original_id_col and ordinal_col and patient_count_col:
        if is_ordinal_col:
            is_ordinal_col_name = 'IS_ORDINAL' if 'IS_ORDINAL' in summary_matrix.columns else 'is_ordinal'
            ordinal_rows = summary_matrix[summary_matrix[is_ordinal_col_name] == True].copy()
        else:
            concept_id_col = 'CONCEPT_ID' if 'CONCEPT_ID' in summary_matrix.columns else ('concept_id' if 'concept_id' in summary_matrix.columns else None)
            if concept_id_col and ordinal_col:
                ordinal_mask = summary_matrix[ordinal_col] > 0
                ordinal_mask = ordinal_mask | summary_matrix[concept_id_col].astype(str).str.contains('_', na=False)
                ordinal_rows = summary_matrix[ordinal_mask].copy()
            else:
                ordinal_rows = pd.DataFrame()
        
        if not ordinal_rows.empty:
            for (orig_concept_id, cluster), group in ordinal_rows.groupby([original_id_col, 'cluster']):
                group_sorted = group.sort_values(ordinal_col)
                max_valid_ordinal = 0
                prev_count = None
                
                for _, row in group_sorted.iterrows():
                    ordinal = row[ordinal_col]
                    patient_count = row[patient_count_col]
                    
                    if ordinal == 1:
                        if patient_count > 0:
                            max_valid_ordinal = 1
                            prev_count = patient_count
                        else:
                            break
                    else:
                        if prev_count is not None and prev_count > 0:
                            if patient_count >= prev_count * 0.5:
                                max_valid_ordinal = ordinal
                                prev_count = patient_count
                            else:
                                break
                        else:
                            break
                
                if max_valid_ordinal <= 1:
                    count_cat = "1"
                elif max_valid_ordinal == 2:
                    count_cat = "2"
                else:
                    count_cat = "3+"
                
                orig_concept_id_str = str(orig_concept_id).replace('.0', '')
                cluster_str = str(cluster)
                count_category_map[(orig_concept_id_str, cluster_str)] = count_cat
    
    for _, row in summary_matrix.iterrows():
        concept_name = normalize_concept_name(row['concept_name'])
        cluster = row['cluster']
        concept_id = row.get('concept_id', None)
        
        is_ordinal_row = False
        if 'IS_ORDINAL' in row.index:
            is_ordinal_row = row['IS_ORDINAL'] == True
        elif 'is_ordinal' in row.index:
            is_ordinal_row = row['is_ordinal'] == True
        
        if is_ordinal_row:
            count_category = "1"
        else:
            concept_id_normalized = None
            if concept_id is not None:
                concept_id_normalized = str(concept_id).replace('.0', '')
            elif 'CONCEPT_ID' in row.index:
                concept_id_normalized = str(row['CONCEPT_ID']).replace('.0', '')
            elif 'concept_id' in row.index:
                concept_id_normalized = str(row['concept_id']).replace('.0', '')
            elif 'ORIGINAL_CONCEPT_ID' in row.index:
                concept_id_normalized = str(row['ORIGINAL_CONCEPT_ID']).replace('.0', '')
            elif 'original_concept_id' in row.index:
                concept_id_normalized = str(row['original_concept_id']).replace('.0', '')
            
            if concept_id_normalized:
                cluster_str = str(cluster)
                count_category = count_category_map.get((concept_id_normalized, cluster_str), "1")
            else:
                count_category = row.get('count_category', "1")
        
        # Map by name (for main concepts)
        key = (concept_name, cluster)
        concept_cluster_map[key] = {
            'prevalence': row['prevalence'],
            'count_category': count_category,
            'median_days': row['median_days']
        }
        
        # Also map by concept_id for direct matching
        if concept_id is not None:
            concept_id_str = str(concept_id)
            key_id = (concept_id_str, cluster)
            concept_id_cluster_map[key_id] = {
                'prevalence': row['prevalence'],
                'count_category': count_category,
                'median_days': row['median_days']
            }
        
        # Also create ordinal map if this is an ordinal concept
        if concept_id is not None:
            concept_info = dashboard_concept_map.get(str(concept_id)) or dashboard_concept_map.get(_normalize_concept_id(concept_id))
            if concept_info and concept_info.get("IS_ORDINAL", False):
                original_id = concept_info.get("ORIGINAL_CONCEPT_ID")
                ordinal = concept_info.get("ORDINAL", 0)
                heritage = concept_info.get("HERITAGE")
                if original_id is not None and heritage is not None:
                    ordinal_key = (str(heritage), _normalize_concept_id(original_id), ordinal, cluster)
                    # For ordinals, count_category is always "1"
                    ordinal_count_category = "1"
                    ordinal_cluster_map[ordinal_key] = {
                        'prevalence': row['prevalence'],
                        'count_category': ordinal_count_category,
                        'median_days': row['median_days']
                    }
    
    
    # Build heatmap matrices for each heritage group
    for row_idx, heritage in enumerate(heritages_with_data, start=1):
        items = heritage_groups_order[heritage]
        num_concepts = len(items)
        
        # Build matrices: prevalence values and text labels
        z_matrix = []  # Prevalence values for heatmap (rows = concepts, cols = clusters)
        text_matrix = []  # Text labels for each cell
        y_labels = []  # Concept names for y-axis
        
        for y_idx, item in enumerate(items):
            concept_name_raw = item.get("concept_name", "Unknown")
            concept_name = normalize_concept_name(concept_name_raw)
            is_ordinal = item.get("is_ordinal", False)
            # Try different field names that might be in the item
            concept_id = item.get("concept_id") or item.get("_concept_id") or item.get("CONCEPT_ID")
            y_labels.append(concept_name)
            
            row_prevalence = []
            row_text = []
            
            for col_idx in range(1, optimal_k + 1):
                cluster_label = f'C{col_idx}'
                cluster_data = {}
                
                # For ordinals, use the same logic as composite plot: match by (heritage, original_concept_id, ordinal)
                if is_ordinal:
                    original_concept_id = item.get("original_concept_id") or item.get("ORIGINAL_CONCEPT_ID")
                    ordinal = item.get("ordinal") or item.get("ORDINAL", 0)
                    item_heritage = item.get("heritage") or item.get("HERITAGE") or heritage
                    
                    # If missing, try to get from dashboard_data
                    if (not original_concept_id or ordinal == 0) and concept_id:
                        concept_info = concept_map.get(str(concept_id)) or concept_map.get(_normalize_concept_id(concept_id))
                        if concept_info:
                            if not original_concept_id:
                                original_concept_id = concept_info.get("ORIGINAL_CONCEPT_ID", concept_id)
                            if ordinal == 0:
                                ordinal = concept_info.get("ORDINAL", 0)
                    
                    
                    # Try matching by concept_id first (most direct)
                    if concept_id is not None:
                        key_id = (str(concept_id), cluster_label)
                        cluster_data = concept_id_cluster_map.get(key_id, {})
                    
                    # If not found, try ordinal map
                    if not cluster_data and original_concept_id is not None and item_heritage is not None:
                        ordinal_key = (str(item_heritage), _normalize_concept_id(original_concept_id), ordinal, cluster_label)
                        cluster_data = ordinal_cluster_map.get(ordinal_key, {})
                
                # For main concepts, try concept_id first (more reliable), then name as fallback
                if not cluster_data and not is_ordinal:
                    # Try matching by concept_id first (more reliable)
                    if concept_id is not None:
                        key_id = (str(concept_id), cluster_label)
                        cluster_data = concept_id_cluster_map.get(key_id, {})
                    
                    # If not found by concept_id, try by name
                    if not cluster_data:
                        key = (concept_name, cluster_label)
                        cluster_data = concept_cluster_map.get(key, {})
                
                prevalence = cluster_data.get('prevalence', 0.0)
                median_days = cluster_data.get('median_days', np.nan)
                
                row_prevalence.append(prevalence)
                
                # Get count_category for circle color (1=green, 2=yellow, 3+=red)
                count_category = cluster_data.get('count_category', '1')
                
                # Store text data for annotations (we'll add annotations separately, not in heatmap text)
                # Store as tuple: (prevalence_text, median_days_text, count_category)
                if not pd.isna(median_days):
                    row_text.append((f"{prevalence:.2%}", f"{median_days:.0f}d", count_category))
                else:
                    row_text.append((f"{prevalence:.2%}", None, count_category))
            
            z_matrix.append(row_prevalence)
            text_matrix.append(row_text)
        
        # Create one heatmap per cluster column for this heritage group
        for col_idx in range(1, optimal_k + 1):
            # Extract single column for this cluster (transpose to make it vertical)
            z_col = [[row[col_idx - 1]] for row in z_matrix]
            
            # Don't use text in heatmap - we'll add annotations separately for left/right alignment
            fig.add_trace(
                go.Heatmap(
                    z=z_col,
                    text=None,  # No text in heatmap
                    texttemplate=None,
                    colorscale=[[0, 'white'], [1, '#2E86AB']],  # White to blue gradient
                    zmin=0,
                    zmax=MAX_PREVALENCE,
                    showscale=False,  # No colorbar
                    hoverongaps=False,
                    hovertemplate=f"<b>%{{customdata}}</b><br>Cluster: {col_idx}<br>Prevalence: %{{z:.2%}}<extra></extra>",
                    customdata=[[label] for label in y_labels],
                    xgap=2,  # Small gap between cells
                    ygap=2,  # Small gap between cells
                ),
                row=row_idx,
                col=col_idx
            )
            
            # Add text annotations for each cell: prevalence on left, median days on right, circle in center
            # Heatmap cells are at x=0 (center of the single column), y=0, 1, 2, ... (row indices)
            
            # Collect circle data for this column
            circle_x = []
            circle_y = []
            circle_colors = []
            circle_hover = []
            
            for y_idx, text_data in enumerate(text_matrix):
                # text_data is a list of tuples: [(prevalence_text, median_days_text, count_category), ...]
                prevalence_text, median_days_text, count_category = text_data[col_idx - 1]
                
                # Add prevalence text on the left (x=-0.35 from center)
                fig.add_annotation(
                    x=-0.35,  # Left side of cell
                    y=y_idx,
                    text=f"<b>{prevalence_text}</b>",
                    showarrow=False,
                    font=dict(size=10, color='black'),
                    xanchor='left',
                    yanchor='middle',
                    row=row_idx,
                    col=col_idx
                )
                
                # Add median days text on the right if available (x=0.35 from center)
                if median_days_text:
                    fig.add_annotation(
                        x=0.35,  # Right side of cell
                        y=y_idx,
                        text=f"{median_days_text}",
                        showarrow=False,
                        font=dict(size=9, color='black'),
                        xanchor='right',
                        yanchor='middle',
                        row=row_idx,
                        col=col_idx
                    )
                
                # Collect circle data: color based on count_category
                # 1 = green, 2 = yellow, 3+ = red, 0 prevalence = white with black border
                circle_x.append(0)  # Center of cell
                circle_y.append(y_idx)
                
                # Get prevalence value to check if it's 0
                prevalence_val = z_matrix[y_idx][col_idx - 1]
                
                if prevalence_val == 0 or prevalence_val == 0.0:
                    circle_colors.append("white")  # Empty circle for 0 prevalence
                    hover_count = "N/A"
                elif count_category == "1":
                    circle_colors.append("#2ECC71")  # Green
                    hover_count = "1"
                elif count_category == "2":
                    circle_colors.append("#F1C40F")  # Yellow
                    hover_count = "2"
                else:  # "3+" or higher
                    circle_colors.append("#E74C3C")  # Red
                    hover_count = "3+"
                circle_hover.append(f"Median occurrences: {hover_count}")
            
            # Add scatter trace for circles (one trace per column to batch all circles)
            # Line color: black for white circles (0 prevalence), white for colored circles
            line_colors = ['black' if c == 'white' else 'white' for c in circle_colors]
            
            fig.add_trace(
                go.Scatter(
                    x=circle_x,
                    y=circle_y,
                    mode='markers',
                    marker=dict(
                        size=12,
                        color=circle_colors,
                        line=dict(width=1.5, color=line_colors)
                    ),
                    hovertext=circle_hover,
                    hoverinfo='text',
                    showlegend=False
                ),
                row=row_idx,
                col=col_idx
            )
    
    # Update layout
    fig.update_layout(
        title="Cluster Prevalence",
        height=plot_height,
        showlegend=False,
        plot_bgcolor="white",
        paper_bgcolor="white",
        margin=dict(l=200, r=50, t=TOP_MARGIN, b=BOTTOM_MARGIN),
        dragmode=False,
        hovermode=False
    )
    
    # Update x-axes (show cluster labels on bottom row only)
    for row_idx in range(1, num_rows + 1):
        for col_idx in range(1, optimal_k + 1):
            show_labels = (row_idx == num_rows)
            cluster_label = f"C{col_idx}"
            count = cluster_counts.get(cluster_label, 0)
            fig.update_xaxes(
                title_text="",
                tickmode='array',
                tickvals=[0],
                ticktext=[f"Cluster {col_idx}<br>(n={count})"] if show_labels else [""],
                showticklabels=show_labels,
                showgrid=False,
                fixedrange=True,
                row=row_idx,
                col=col_idx
            )
    
    # Update y-axes (show concept names on first column, same range as composite plot)
    for row_idx, heritage in enumerate(heritages_with_data, start=1):
        items = heritage_groups_order[heritage]
        num_concepts = len(items)
        # Build y-axis labels from items (same as composite plot)
        y_labels = [item.get("concept_name", "Unknown") for item in items]
        
        for col_idx in range(1, optimal_k + 1):
            # Show labels only on first column (leftmost)
            show_labels = (col_idx == 1)
            fig.update_yaxes(
                tickmode='array',
                tickvals=list(range(num_concepts)),
                ticktext=y_labels if show_labels else [""] * num_concepts,
                range=[num_concepts - 0.5, -0.5],  # Reversed range: [max, min] so first item at top
                showticklabels=show_labels,
                showgrid=False,
                fixedrange=True,
                row=row_idx,
                col=col_idx
            )
    
    return fig


def _sort_trajectory_items(
    stats: Dict,
    concept_info: Dict[str, Dict],
    min_prevalence_pct: int = 0
) -> List[Tuple[str, float, float]]:
    """
    Return [(concept_id, median_time, prevalence)] sorted by a deterministic key.
    Primary sort: median time (ascending).
    Tie-breakers: concept name, then concept id.
    """
    items = [
        (cid, s["median"], s["prevalence"])
        for cid, s in stats.items()
        if cid in concept_info and s.get("prevalence", 0) >= min_prevalence_pct
    ]
    items.sort(key=lambda x: (
        float(x[1]),
        str(concept_info.get(x[0], {}).get("name", x[0])).lower(),
        str(x[0])
    ))
    return items


def _create_trajectory_rank_matrix(
    concept_info: Dict[str, Dict],
    overall_order: List[Tuple[str, float, float]],
    cluster_orders: Dict[str, List[Tuple[str, float, float]]],
    cluster_labels: List[str],
    cluster_counts: Dict[str, int],
    min_prevalence_pct: int = 0,
    ordering_mode: str = "order",
    preferred_cluster: Optional[str] = None,
) -> go.Figure:
    """
    Create trajectory ordering matrix.

    Default modes align concepts by row and emphasize rank movement.
    Cluster mode renders each column as an independent ordering list.
    """
    def _empty(message: str) -> go.Figure:
        fig = go.Figure()
        fig.update_layout(
            height=220,
            showlegend=False,
            plot_bgcolor="white",
            paper_bgcolor="white",
            annotations=[dict(
                text=message,
                x=0.5, y=0.5,
                xref="paper", yref="paper",
                showarrow=False,
                font=dict(size=14, color="#666")
            )]
        )
        return fig
    
    if not overall_order and not any(cluster_orders.get(label) for label in cluster_labels):
        return _empty(f"No cells with ≥{min_prevalence_pct}% prevalence")
    
    overall_rank = {cid: i + 1 for i, (cid, _, _) in enumerate(overall_order)}
    overall_metrics = {cid: (median, prevalence) for cid, median, prevalence in overall_order}
    
    cluster_rank = {}
    cluster_metrics = {}
    for label in cluster_labels:
        order = cluster_orders.get(label, [])
        cluster_rank[label] = {cid: i + 1 for i, (cid, _, _) in enumerate(order)}
        cluster_metrics[label] = {cid: (median, prevalence) for cid, median, prevalence in order}

    def _wrap_text(text: str, max_chars: int = 42) -> str:
        if len(text) <= max_chars:
            return text
        bp = text.rfind(" ", 0, max_chars)
        if bp == -1:
            bp = max_chars
        return text[:bp] + "<br>" + text[bp:].strip()

    if ordering_mode == "cluster":
        col_labels = ["Overall"] + cluster_labels
        orders_by_col: Dict[str, List[Tuple[str, float, float]]] = {"Overall": overall_order}
        for lbl in cluster_labels:
            orders_by_col[lbl] = cluster_orders.get(lbl, [])

        max_rows = max((len(order) for order in orders_by_col.values()), default=0)
        if max_rows == 0:
            return _empty(f"No cells with ≥{min_prevalence_pct}% prevalence")

        cell_height = 62
        overall_cell_width = 420
        cluster_cell_width = 300
        col_gap = 20
        col_widths = [overall_cell_width] + [cluster_cell_width] * len(cluster_labels)
        col_starts = []
        x_cursor = 0
        for width in col_widths:
            col_starts.append(x_cursor)
            x_cursor += width + col_gap
        total_plot_width = x_cursor - col_gap

        plot_height = max(420, max_rows * cell_height + 130)
        total_width = total_plot_width + 50

        fig = go.Figure()
        shapes = []
        annotations = []

        for col_idx in range(1, len(col_labels)):
            x_line = col_starts[col_idx] - col_gap / 2
            shapes.append(dict(
                type="line",
                x0=x_line, x1=x_line,
                y0=-8, y1=max_rows * cell_height + 38,
                line=dict(color="#aab0b6", width=1, dash="dash"),
                layer="below"
            ))

        col_headers = ["Overall"] + [f"{label}\n(n={cluster_counts.get(label, '?')})" for label in cluster_labels]
        for col_idx, header in enumerate(col_headers):
            x_center = col_starts[col_idx] + col_widths[col_idx] / 2
            annotations.append(dict(
                x=x_center, y=max_rows * cell_height + 30,
                text=f"<b>{header}</b>",
                showarrow=False,
                font=dict(size=12, color="#2c3e50"),
                xref="x", yref="y",
                xanchor="center"
            ))

        annotations.append(dict(
            x=0, y=max_rows * cell_height + 56,
            text="<b>Cluster trajectory (independent ordering per column)</b>",
            showarrow=False,
            font=dict(size=11, color="#4d4d4d"),
            xref="x", yref="y",
            xanchor="left"
        ))

        def _format_concept_label_for_cell(concept_name: str, max_chars_per_line: int = 26) -> Tuple[str, int]:
            """
            Format concept name to fit inside a cell:
            - max 2 wrapped lines
            - reduce font size for long names
            """
            normalized = str(concept_name or "").strip()
            wrapped = textwrap.wrap(
                normalized,
                width=max_chars_per_line,
                break_long_words=True,
                break_on_hyphens=False,
            )

            if not wrapped:
                return "", 10

            if len(wrapped) <= 2:
                max_line_len = max(len(line) for line in wrapped)
                font_size = 10 if max_line_len <= int(max_chars_per_line * 0.85) else 9
                return "<br>".join(wrapped), font_size

            # Keep exactly two lines; truncate second line with ellipsis.
            first_line = wrapped[0]
            second_line = wrapped[1]
            if len(second_line) >= max_chars_per_line:
                second_line = second_line[: max_chars_per_line - 1].rstrip() + "…"
            else:
                second_line = second_line.rstrip() + "…"

            # For very long names, use smaller font.
            font_size = 8 if len(normalized) > (max_chars_per_line * 2 + 10) else 9
            return f"{first_line}<br>{second_line}", font_size

        for row_idx in range(max_rows):
            y_top = max_rows * cell_height - row_idx * cell_height
            y_bottom = y_top - cell_height + 4

            for col_idx, col_label in enumerate(col_labels):
                x0 = col_starts[col_idx]
                x1 = x0 + col_widths[col_idx]
                ordered_items = orders_by_col.get(col_label, [])

                if row_idx < len(ordered_items):
                    cid, median_day, prevalence_pct = ordered_items[row_idx]
                    info = concept_info.get(cid, {})
                    concept_name = str(info.get("name", cid))
                    heritage = info.get("heritage", "unknown")
                    fill_color = HERITAGE_COLORS.get(heritage, "#dddddd")
                    rank = row_idx + 1

                    label_html, label_font = _format_concept_label_for_cell(concept_name)
                    if col_label == "Overall":
                        rank_delta_html = "<span style='color:#666'>=0</span>"
                    else:
                        overall_pos = overall_rank.get(cid)
                        if overall_pos is None:
                            rank_delta_html = "<span style='color:#999'>NA</span>"
                        else:
                            delta = int(rank - overall_pos)
                            if delta > 0:
                                rank_delta_html = f"<span style='color:#d62728'>▼{delta}</span>"
                            elif delta < 0:
                                rank_delta_html = f"<span style='color:#1a7a1a'>▲{abs(delta)}</span>"
                            else:
                                rank_delta_html = "<span style='color:#666'>=0</span>"

                    cell_text = (
                        f"<span style='font-size:{label_font}px'><b>{label_html}</b></span><br>"
                        f"<span style='font-size:10px'>#{rank} {rank_delta_html} | {median_day:.0f}d | {prevalence_pct:.0f}%</span>"
                    )
                else:
                    fill_color = "#f5f5f5"
                    cell_text = "<span style='color:#999'>NA</span>"

                shapes.append(dict(
                    type="rect",
                    x0=x0, x1=x1, y0=y_bottom, y1=y_top,
                    fillcolor=fill_color,
                    line=dict(color="white", width=2),
                    layer="below"
                ))
                annotations.append(dict(
                    x=(x0 + x1) / 2,
                    y=(y_top + y_bottom) / 2,
                    text=cell_text,
                    showarrow=False,
                    font=dict(size=10, color="black"),
                    xref="x", yref="y",
                    xanchor="center", yanchor="middle", align="center"
                ))

        x_max = total_plot_width
        y_max = max_rows * cell_height + 74
        fig.update_layout(
            height=plot_height,
            width=total_width,
            showlegend=False,
            plot_bgcolor="white",
            paper_bgcolor="white",
            margin=dict(l=20, r=20, t=30, b=20),
            shapes=shapes,
            annotations=annotations,
            xaxis=dict(
                showgrid=False,
                showticklabels=False,
                zeroline=False,
                range=[-10, x_max],
                fixedrange=True,
                scaleanchor="y",
                scaleratio=1
            ),
            yaxis=dict(
                showgrid=False,
                showticklabels=False,
                zeroline=False,
                range=[-20, y_max],
                fixedrange=True
            )
        )
        return fig
    
    all_cids = set(overall_rank.keys())
    for label in cluster_labels:
        all_cids.update(cluster_rank[label].keys())
    
    if not all_cids:
        return _empty(f"No cells with ≥{min_prevalence_pct}% prevalence")

    def _normalize_cluster_value(value: Optional[str]) -> Optional[str]:
        if value is None:
            return None
        label = str(value).strip()
        if not label or label.lower() == "all":
            return None
        if label in cluster_labels:
            return label
        if label.isdigit():
            candidate = f"C{label}"
            if candidate in cluster_labels:
                return candidate
        lower = label.lower()
        if lower.startswith("cluster_"):
            suffix = lower.split("_", 1)[1]
            if suffix.isdigit():
                candidate = f"C{suffix}"
                if candidate in cluster_labels:
                    return candidate
        return None

    cluster_focus_label = _normalize_cluster_value(preferred_cluster)
    
    def _rank_delta(cid: str, label: str) -> Optional[int]:
        r_o = overall_rank.get(cid)
        r_c = cluster_rank[label].get(cid)
        if r_o is None or r_c is None:
            return None
        return int(r_c - r_o)
    
    mover_score = {}
    stability_score = {}
    for cid in all_cids:
        deltas = [abs(_rank_delta(cid, lbl)) for lbl in cluster_labels if _rank_delta(cid, lbl) is not None]
        mover_score[cid] = max(deltas) if deltas else 0

        # Stability is based on raw rank variance across overall + clusters.
        ranks = []
        if cid in overall_rank:
            ranks.append(overall_rank[cid])
        for lbl in cluster_labels:
            if cid in cluster_rank[lbl]:
                ranks.append(cluster_rank[lbl][cid])
        stability_score[cid] = float(np.std(ranks)) if len(ranks) >= 2 else float("inf")
    
    def _row_sort_key(cid: str):
        default_name = str(concept_info.get(cid, {}).get("name", cid)).lower()
        default_rank = overall_rank.get(cid, 10**9)
        if ordering_mode == "cluster":
            if cluster_focus_label:
                focus_rank = cluster_rank.get(cluster_focus_label, {}).get(cid)
                return (
                    1 if focus_rank is None else 0,
                    focus_rank if focus_rank is not None else 10**9,
                    default_rank,
                    default_name,
                    str(cid),
                )
            best_rank = 10**9
            best_cluster_idx = 10**9
            for idx, lbl in enumerate(cluster_labels):
                c_rank = cluster_rank.get(lbl, {}).get(cid)
                if c_rank is None:
                    continue
                if c_rank < best_rank or (c_rank == best_rank and idx < best_cluster_idx):
                    best_rank = c_rank
                    best_cluster_idx = idx
            return (best_rank, best_cluster_idx, default_rank, default_name, str(cid))
        if ordering_mode == "movers":
            return (-mover_score.get(cid, 0), default_rank, default_name, str(cid))
        if ordering_mode == "stable":
            return (stability_score.get(cid, float("inf")), default_rank, default_name, str(cid))
        return (default_rank, default_name, str(cid))
    
    row_cids = sorted(all_cids, key=_row_sort_key)
    max_rows = len(row_cids)
    
    if max_rows == 0:
        return _empty(f"No cells with ≥{min_prevalence_pct}% prevalence")
    
    def _delta_fill_color(d: Optional[int]) -> str:
        if d is None:
            return "#f5f5f5"
        if d > 0:
            return "#fbe3e3"
        if d < 0:
            return "#e3f5e8"
        return "#ffffff"
    
    cell_height = 62
    overall_cell_width = 420
    cluster_cell_width = 300
    col_gap = 20
    num_cols = 1 + len(cluster_labels)

    col_widths = [overall_cell_width] + [cluster_cell_width] * len(cluster_labels)
    col_starts = []
    x_cursor = 0
    for width in col_widths:
        col_starts.append(x_cursor)
        x_cursor += width + col_gap
    total_plot_width = x_cursor - col_gap

    plot_height = max(420, max_rows * cell_height + 130)
    total_width = total_plot_width + 50
    
    fig = go.Figure()
    shapes = []
    annotations = []
    
    # Vertical separators.
    for col_idx in range(1, num_cols):
        x_line = col_starts[col_idx] - col_gap / 2
        shapes.append(dict(
            type="line",
            x0=x_line, x1=x_line,
            y0=-8, y1=max_rows * cell_height + 38,
            line=dict(color="#aab0b6", width=1, dash="dash"),
            layer="below"
        ))
    
    col_headers = ["Overall"] + [f"{label}\n(n={cluster_counts.get(label, '?')})" for label in cluster_labels]
    for col_idx, header in enumerate(col_headers):
        x_center = col_starts[col_idx] + col_widths[col_idx] / 2
        annotations.append(dict(
            x=x_center, y=max_rows * cell_height + 30,
            text=f"<b>{header}</b>",
            showarrow=False,
            font=dict(size=12, color="#2c3e50"),
            xref="x", yref="y",
            xanchor="center"
        ))
    
    if ordering_mode == "cluster":
        if cluster_focus_label:
            subtitle = f"Cluster trajectory ({cluster_focus_label})"
        else:
            subtitle = "Cluster trajectory (best cluster rank)"
    else:
        subtitle = {
            "order": "Ordered by overall rank",
            "movers": "Top movers (largest rank shift)",
            "stable": "Most stable (smallest rank variance)"
        }.get(ordering_mode, "Ordered by overall rank")
    annotations.append(dict(
        x=0, y=max_rows * cell_height + 56,
        text=f"<b>{subtitle}</b>",
        showarrow=False,
        font=dict(size=11, color="#4d4d4d"),
        xref="x", yref="y",
        xanchor="left"
    ))
    
    for row_idx, cid in enumerate(row_cids):
        info = concept_info.get(cid, {})
        name = info.get("name", cid)
        heritage = info.get("heritage", "unknown")
        is_ordinal = info.get("is_ordinal", False)
        ordinal_num = info.get("ordinal_num")
        
        if ordering_mode == "cluster":
            concept_label = str(name)
        elif is_ordinal and ordinal_num:
            suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(ordinal_num, f"{ordinal_num}th")
            concept_label = f"{name} ({suffix})"
        else:
            concept_label = str(name)
        
        y_top = max_rows * cell_height - row_idx * cell_height
        y_bottom = y_top - cell_height + 4
        
        # Overall cell
        x0 = col_starts[0]
        x1 = x0 + col_widths[0]
        shapes.append(dict(
            type="rect",
            x0=x0, x1=x1, y0=y_bottom, y1=y_top,
            fillcolor=HERITAGE_COLORS.get(heritage, "#dddddd"),
            line=dict(color="white", width=2),
            layer="below"
        ))
        o_rank = overall_rank.get(cid)
        o_median, o_prev = overall_metrics.get(cid, (None, None))
        overall_text = (
            f"<b>{_wrap_text(concept_label, max_chars=64)}</b><br>"
            f"<span style='font-size:10px'>#{o_rank if o_rank is not None else 'NA'} | "
            f"{o_median:.0f}d | {o_prev:.0f}%</span>"
            if o_median is not None and o_prev is not None
            else f"<b>{_wrap_text(concept_label, max_chars=64)}</b><br><span style='font-size:10px'>#NA | NA | NA</span>"
        )
        annotations.append(dict(
            x=(x0 + x1) / 2,
            y=(y_top + y_bottom) / 2,
            text=overall_text,
            showarrow=False,
            font=dict(size=10, color="black"),
            xref="x", yref="y",
            xanchor="center", yanchor="middle", align="center"
        ))
        
        # Cluster cells
        for c_idx, lbl in enumerate(cluster_labels, start=1):
            xs = col_starts[c_idx]
            xe = xs + col_widths[c_idx]
            
            d = _rank_delta(cid, lbl)
            shapes.append(dict(
                type="rect",
                x0=xs, x1=xe, y0=y_bottom, y1=y_top,
                fillcolor=_delta_fill_color(d),
                line=dict(color="white", width=2),
                layer="below"
            ))
            
            c_rank = cluster_rank[lbl].get(cid)
            c_median, c_prev = cluster_metrics[lbl].get(cid, (None, None))
            
            if d is None:
                delta_text = "<span style='color:#999'>NA</span>"
            elif d > 0:
                delta_text = f"<span style='color:#d62728'>▼{d}</span>"
            elif d < 0:
                delta_text = f"<span style='color:#1a7a1a'>▲{abs(d)}</span>"
            else:
                delta_text = "<span style='color:#666'>=0</span>"
            
            if c_median is not None and c_prev is not None and c_rank is not None:
                cluster_text = (
                    f"<b>#{c_rank}</b> {delta_text}<br>"
                    f"<span style='font-size:10px'>{c_median:.0f}d | {c_prev:.0f}%</span>"
                )
            else:
                cluster_text = "<b>#NA</b> <span style='color:#999'>NA</span><br><span style='font-size:10px'>NA | NA</span>"
            
            annotations.append(dict(
                x=(xs + xe) / 2,
                y=(y_top + y_bottom) / 2,
                text=cluster_text,
                showarrow=False,
                font=dict(size=10, color="black"),
                xref="x", yref="y",
                xanchor="center", yanchor="middle", align="center"
            ))
    
    x_max = total_plot_width
    y_max = max_rows * cell_height + 74
    fig.update_layout(
        height=plot_height,
        width=total_width,
        showlegend=False,
        plot_bgcolor="white",
        paper_bgcolor="white",
        margin=dict(l=20, r=20, t=30, b=20),
        shapes=shapes,
        annotations=annotations,
        xaxis=dict(
            showgrid=False,
            showticklabels=False,
            zeroline=False,
            range=[-10, x_max],
            fixedrange=True,
            scaleanchor="y",
            scaleratio=1
        ),
        yaxis=dict(
            showgrid=False,
            showticklabels=False,
            zeroline=False,
            range=[-20, y_max],
            fixedrange=True
        )
    )
    return fig


def create_trajectory_plot(
    clustering_results: Dict,
    data_patients: pd.DataFrame,
    heritage_groups_order: Dict[str, List[Dict]],
    active_concept_ids: Set[str],
    min_prevalence_pct: int = 0,
    ordering_mode: str = "order",
    selected_cluster: Optional[str] = None,
) -> go.Figure:
    """
    Create a trajectory visualization showing concept ordering by median time.
    Each column shows concepts ordered by median time FOR THAT COLUMN.
    Concept names are displayed on each cell.
    
    Uses summary_matrix from clustering_results when available to ensure
    consistency with composite plot cluster columns (both use the same
    pre-computed data from clustering).
    
    Args:
        clustering_results: Dictionary with patient_assignments, summary_matrix, and cluster info
        data_patients: DataFrame with patient-concept TIME_TO_EVENT data (fallback)
        heritage_groups_order: Dictionary mapping heritage to list of items in display order
        active_concept_ids: Set of active concept IDs to include
        min_prevalence_pct: Minimum prevalence percentage (0-100) to include concept
        ordering_mode: One of "order", "cluster", "movers", "stable"
        selected_cluster: Selected cluster label ("C1", "C2", etc.) for cluster ordering mode
        
    Returns:
        Plotly figure with trajectory visualization
    """
    # Get patient assignments
    patient_assignments = clustering_results.get('patient_assignments', [])
    if isinstance(patient_assignments, list):
        patient_assignments = pd.DataFrame(patient_assignments) if patient_assignments else pd.DataFrame()
    
    optimal_k = clustering_results.get('optimal_cluster_count', 2)
    
    # Get summary_matrix from clustering results for consistent prevalence calculations
    # Using the same pre-computed data ensures consistency with the composite plot's cluster columns
    summary_matrix_data = clustering_results.get('summary_matrix', [])
    if isinstance(summary_matrix_data, list):
        summary_matrix = pd.DataFrame(summary_matrix_data) if summary_matrix_data else pd.DataFrame()
    else:
        summary_matrix = summary_matrix_data if isinstance(summary_matrix_data, pd.DataFrame) else pd.DataFrame()
    
    if patient_assignments.empty:
        fig = go.Figure()
        fig.update_layout(
            height=200,
            showlegend=False,
            plot_bgcolor="white",
            paper_bgcolor="white",
            annotations=[dict(
                text="No clustering data available",
                x=0.5, y=0.5,
                xref="paper", yref="paper",
                showarrow=False,
                font=dict(size=14)
            )]
        )
        return fig
    
    # Filter to target cohort only
    df_target = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
    
    # Helper function to get first occurrence time
    def get_first_occurrence(time_list):
        if time_list is None:
            return None
        if isinstance(time_list, np.ndarray):
            if time_list.size == 0:
                return None
            valid_times = time_list[~np.isnan(time_list)]
            return float(np.min(valid_times)) if valid_times.size > 0 else None
        if isinstance(time_list, (list, tuple)):
            valid_times = [t for t in time_list if t is not None and not (isinstance(t, (int, float)) and pd.isna(t))]
            return float(min(valid_times)) if valid_times else None
        if isinstance(time_list, (int, float)) and not pd.isna(time_list):
            return float(time_list)
        return None
    
    # Build concept info from heritage_groups_order
    concept_info = {}  # concept_id -> {name, heritage, is_ordinal, original_id, ordinal_num}
    main_concepts_with_ordinals = set()  # Track main concepts that have ordinals
    
    # First pass: identify main concepts that have ordinals
    for heritage, concepts in heritage_groups_order.items():
        for concept in concepts:
            is_ordinal = concept.get("IS_ORDINAL", False)
            if is_ordinal:
                original_id = concept.get("ORIGINAL_CONCEPT_ID")
                if original_id:
                    main_concepts_with_ordinals.add(_normalize_concept_id(original_id))
    
    # Second pass: build concept_info, excluding main concepts that have ordinals
    for heritage, concepts in heritage_groups_order.items():
        for concept in concepts:
            concept_id = concept.get("_concept_id") or concept.get("CONCEPT_ID")
            if concept_id:
                norm_id = _normalize_concept_id(concept_id)
                if norm_id in active_concept_ids:
                    name = concept.get("CONCEPT_NAME") or concept.get("_display_name", str(concept_id))
                    is_ordinal = concept.get("IS_ORDINAL", False)
                    original_id = concept.get("ORIGINAL_CONCEPT_ID")
                    
                    # Skip main concepts that have ordinals (to avoid duplicates)
                    if not is_ordinal and norm_id in main_concepts_with_ordinals:
                        continue
                    
                    # Extract ordinal number from name or concept_id
                    ordinal_num = None
                    if is_ordinal:
                        # Try to extract from concept_id suffix (e.g., 19096753002 -> 2)
                        if original_id:
                            orig_norm = _normalize_concept_id(original_id)
                            if norm_id.startswith(orig_norm) and len(norm_id) > len(orig_norm):
                                try:
                                    ordinal_num = int(norm_id[len(orig_norm):])
                                except ValueError:
                                    pass
                        # Fallback: try to extract from name
                        if ordinal_num is None:
                            for i in range(1, 10):
                                if f"≥{i}" in name or f">={i}" in name or f"({i})" in name:
                                    ordinal_num = i
                                    break
                    
                    concept_info[norm_id] = {
                        'name': name,
                        'heritage': heritage,
                        'is_ordinal': is_ordinal,
                        'original_id': _normalize_concept_id(original_id) if original_id else None,
                        'ordinal_num': ordinal_num,
                    }
    
    
    if not concept_info:
        fig = go.Figure()
        fig.update_layout(height=200, showlegend=False)
        return fig
    
    # Get cluster assignments
    cluster_map = {}  # person_id -> cluster_index (0-based)
    for _, row in patient_assignments.iterrows():
        patient_id = row['patient_id']
        cluster_label = row['cluster']  # Format: 'C1', 'C2', etc.
        cluster_idx = int(cluster_label[1:]) - 1
        cluster_map[patient_id] = cluster_idx
    
    # Calculate median first occurrence and patient count for each concept per group
    def calculate_concept_stats(patient_ids: Set[int]) -> Dict[str, Dict]:
        """Calculate median time and patient count for each concept (including ordinals)."""
        times_by_concept = {}
        patients_by_concept = {}
        
        # Build a map of original concept IDs to their TIME_TO_EVENT data per patient
        # This is needed for ordinal calculations
        patient_concept_times = {}  # {person_id: {concept_id: [times]}}
        
        for _, row in df_target.iterrows():
            person_id = row['PERSON_ID']
            if person_id not in patient_ids:
                continue
            
            raw_concept_id = _normalize_concept_id(row['CONCEPT_ID'])
            time_list = row.get('TIME_TO_EVENT', [])
            
            # Get all valid times for this person-concept
            if isinstance(time_list, np.ndarray):
                valid_times = sorted([t for t in time_list if not np.isnan(t)])
            elif isinstance(time_list, (list, tuple)):
                valid_times = sorted([t for t in time_list if t is not None and not (isinstance(t, (int, float)) and pd.isna(t))])
            elif isinstance(time_list, (int, float)) and not pd.isna(time_list):
                valid_times = [float(time_list)]
            else:
                valid_times = []
            
            if valid_times:
                if person_id not in patient_concept_times:
                    patient_concept_times[person_id] = {}
                patient_concept_times[person_id][raw_concept_id] = valid_times
        
        # Now calculate stats for each concept in concept_info
        for cid, info in concept_info.items():
            is_ordinal = info.get('is_ordinal', False)
            original_id = info.get('original_id')
            ordinal_num = info.get('ordinal_num')
            
            times_list = []
            patients_set = set()
            
            if is_ordinal and original_id and ordinal_num:
                # For ordinals, look up the original concept and get the Nth occurrence
                for person_id in patient_ids:
                    if person_id not in patient_concept_times:
                        continue
                    person_times = patient_concept_times[person_id].get(original_id, [])
                    if len(person_times) >= ordinal_num:
                        times_list.append(person_times[ordinal_num - 1])
                        patients_set.add(person_id)
            else:
                # For main concepts, get first occurrence
                for person_id in patient_ids:
                    if person_id not in patient_concept_times:
                        continue
                    person_times = patient_concept_times[person_id].get(cid, [])
                    if person_times:
                        times_list.append(person_times[0])  # First occurrence
                        patients_set.add(person_id)
            
            if times_list:
                total_patients = len(patient_ids)
                prev_pct = (len(patients_set) / total_patients * 100) if total_patients > 0 else 0
                times_by_concept[cid] = times_list
                patients_by_concept[cid] = patients_set
        
        result = {}
        total_patients = len(patient_ids)
        for cid, times in times_by_concept.items():
            prev_pct = (len(patients_by_concept[cid]) / total_patients * 100) if total_patients > 0 else 0
            result[cid] = {
                'median': np.median(times),
                'count': len(patients_by_concept[cid]),
                'prevalence': prev_pct
            }
        return result
    
    # Build cluster_patient_counts first (needed regardless of data source)
    cluster_patient_counts = {}
    for cluster in range(optimal_k):
        cluster_patients = {pid for pid, c in cluster_map.items() if c == cluster}
        cluster_patient_counts[cluster] = len(cluster_patients)
    
    # Try to use summary_matrix data for cluster stats (ensures consistency with composite plot)
    # Using the same pre-computed data avoids recalculating and ensures consistency
    cluster_stats = {}
    summary_matrix_used = False
    
    if not summary_matrix.empty and 'cluster' in summary_matrix.columns:
        # Determine column names (may be uppercase from parquet or lowercase)
        concept_id_col = 'CONCEPT_ID' if 'CONCEPT_ID' in summary_matrix.columns else 'concept_id'
        prevalence_col = 'prevalence'
        median_col = 'time_median' if 'time_median' in summary_matrix.columns else 'median_days'
        
        if concept_id_col in summary_matrix.columns and prevalence_col in summary_matrix.columns:
            # Build stats from summary_matrix
            for cluster_idx in range(optimal_k):
                cluster_label = f'C{cluster_idx + 1}'
                cluster_stats[cluster_idx] = {}
                
                cluster_rows = summary_matrix[summary_matrix['cluster'] == cluster_label]
                for _, row in cluster_rows.iterrows():
                    cid = _normalize_concept_id(row.get(concept_id_col, ''))
                    # Only include concepts that are in our active concept_info
                    if cid in concept_info:
                        prevalence = row.get(prevalence_col, 0)
                        # summary_matrix stores prevalence as decimal (0-1), convert to percentage
                        prevalence_pct = prevalence * 100 if prevalence <= 1 else prevalence
                        median_time = row.get(median_col)
                        
                        if median_time is not None and not pd.isna(median_time):
                            cluster_stats[cluster_idx][cid] = {
                                'median': median_time,
                                'prevalence': prevalence_pct,
                                'count': 0  # Not available from summary_matrix
                            }
            
            summary_matrix_used = True
    
    # Fall back to calculating from patient data if summary_matrix not available
    if not summary_matrix_used:
        for cluster in range(optimal_k):
            cluster_patients = {pid for pid, c in cluster_map.items() if c == cluster}
            cluster_stats[cluster] = calculate_concept_stats(cluster_patients)
    
    # Calculate overall stats
    # For overall, we need to calculate weighted average from cluster stats
    all_patient_ids = set(cluster_map.keys())
    
    if summary_matrix_used:
        # Calculate overall from cluster stats (weighted by cluster size)
        overall_stats = {}
        total_patients = len(all_patient_ids)
        
        for cid in concept_info.keys():
            medians = []
            total_weight = 0
            weighted_prev = 0
            
            for cluster_idx in range(optimal_k):
                if cid in cluster_stats[cluster_idx]:
                    stats = cluster_stats[cluster_idx][cid]
                    count = cluster_patient_counts[cluster_idx]
                    medians.append((stats['median'], count))
                    weighted_prev += stats['prevalence'] * count
                    total_weight += count
            
            if medians:
                weighted_median = sum(m * w for m, w in medians) / sum(w for _, w in medians)
                avg_prevalence = weighted_prev / total_weight if total_weight > 0 else 0
                overall_stats[cid] = {
                    'median': weighted_median,
                    'prevalence': avg_prevalence,
                    'count': 0
                }
    else:
        overall_stats = calculate_concept_stats(all_patient_ids)
    
    overall_order = _sort_trajectory_items(overall_stats, concept_info, min_prevalence_pct=min_prevalence_pct)
    cluster_orders = {
        f"C{c + 1}": _sort_trajectory_items(cluster_stats[c], concept_info, min_prevalence_pct=min_prevalence_pct)
        for c in range(optimal_k)
    }
    cluster_labels = [f"C{c + 1}" for c in range(optimal_k)]
    cluster_counts = {f"C{c + 1}": cluster_patient_counts[c] for c in range(optimal_k)}
    
    return _create_trajectory_rank_matrix(
        concept_info=concept_info,
        overall_order=overall_order,
        cluster_orders=cluster_orders,
        cluster_labels=cluster_labels,
        cluster_counts=cluster_counts,
        min_prevalence_pct=min_prevalence_pct,
        ordering_mode=ordering_mode,
        preferred_cluster=selected_cluster,
    )


def create_overlap_plot(
    clustering_results: Dict,
    data_patients: pd.DataFrame,
    heritage_groups_order: Dict[str, List[Dict]],
    active_concept_ids: Set[str],
    selected_group: str = "overall",  # "overall", "cluster_1", "cluster_2", etc.
    metric: str = "jaccard"  # "jaccard" or "correlation"
) -> go.Figure:
    """
    Create a concept overlap/co-occurrence matrix heatmap.
    
    Shows how often concepts co-occur in the same patients within a cluster.
    Uses Jaccard index: |A ∩ B| / |A ∪ B|
    
    Args:
        clustering_results: Dictionary with patient_assignments, optimal_k, etc.
        data_patients: DataFrame with PERSON_ID, CONCEPT_ID, COHORT_DEFINITION_ID
        heritage_groups_order: Dictionary mapping heritage to list of concept items
        active_concept_ids: Set of normalized concept IDs to include
        selected_group: Which group to analyze ("overall" or "cluster_N")
        metric: "jaccard" for Jaccard index, "correlation" for phi coefficient
        
    Returns:
        Plotly figure with overlap heatmap
    """
    if not clustering_results or data_patients.empty:
        fig = go.Figure()
        fig.update_layout(height=200, showlegend=False)
        fig.add_annotation(text="No data available", x=0.5, y=0.5, 
                          xref="paper", yref="paper", showarrow=False)
        return fig, []
    
    # Get patient assignments
    patient_assignments_data = clustering_results.get('patient_assignments', [])
    if isinstance(patient_assignments_data, list):
        patient_assignments = pd.DataFrame(patient_assignments_data) if patient_assignments_data else pd.DataFrame()
    else:
        patient_assignments = patient_assignments_data
    
    optimal_k = clustering_results.get('optimal_k', 0)
    
    if patient_assignments.empty:
        fig = go.Figure()
        fig.update_layout(height=200, showlegend=False)
        fig.add_annotation(text="No clustering data available", x=0.5, y=0.5,
                          xref="paper", yref="paper", showarrow=False)
        return fig, []
    
    # Build cluster map
    cluster_map = {}  # person_id -> cluster_index (0-based)
    for _, row in patient_assignments.iterrows():
        patient_id = row['patient_id']
        cluster_label = row['cluster']  # Format: 'C1', 'C2', etc.
        cluster_idx = int(cluster_label[1:]) - 1
        cluster_map[patient_id] = cluster_idx
    
    # Determine which patients to analyze
    df_target = data_patients[data_patients['COHORT_DEFINITION_ID'] == 'target'].copy()
    
    if selected_group == "overall":
        target_patient_ids = set(df_target['PERSON_ID'].unique())
    else:
        # Parse cluster number from "cluster_1", "cluster_2", etc.
        try:
            cluster_num = int(selected_group.split("_")[1]) - 1  # 0-indexed
            target_patient_ids = {pid for pid, c in cluster_map.items() if c == cluster_num}
        except (ValueError, IndexError):
            target_patient_ids = set(df_target['PERSON_ID'].unique())
    
    # Build concept info and patient-concept mapping
    concept_info = {}  # concept_id -> {name, heritage}
    main_concepts_with_ordinals = set()
    
    # First pass: find main concepts with ordinals
    for heritage, concepts in heritage_groups_order.items():
        for concept in concepts:
            is_ordinal = concept.get("IS_ORDINAL", False)
            if is_ordinal:
                original_id = concept.get("ORIGINAL_CONCEPT_ID")
                if original_id:
                    main_concepts_with_ordinals.add(_normalize_concept_id(original_id))
    
    # Second pass: build concept_info (skip main concepts that have ordinals)
    for heritage, concepts in heritage_groups_order.items():
        for concept in concepts:
            concept_id = concept.get("_concept_id") or concept.get("CONCEPT_ID")
            if concept_id:
                norm_id = _normalize_concept_id(concept_id)
                if norm_id in active_concept_ids:
                    is_ordinal = concept.get("IS_ORDINAL", False)
                    
                    # Skip main concepts that have ordinals
                    if not is_ordinal and norm_id in main_concepts_with_ordinals:
                        continue
                    
                    name = concept.get("CONCEPT_NAME") or concept.get("_display_name", str(concept_id))
                    ordinal_num = None
                    if is_ordinal:
                        original_id = concept.get("ORIGINAL_CONCEPT_ID")
                        if original_id:
                            orig_norm = _normalize_concept_id(original_id)
                            if norm_id.startswith(orig_norm) and len(norm_id) > len(orig_norm):
                                try:
                                    ordinal_num = int(norm_id[len(orig_norm):])
                                except ValueError:
                                    pass
                    
                    if is_ordinal and ordinal_num:
                        ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(ordinal_num, f"{ordinal_num}th")
                        display_name = f"{name[:25]}... ({ordinal_suffix})" if len(name) > 25 else f"{name} ({ordinal_suffix})"
                    else:
                        display_name = f"{name[:30]}..." if len(name) > 30 else name
                    
                    concept_info[norm_id] = {
                        'name': name,
                        'display_name': display_name,
                        'heritage': heritage,
                        'is_ordinal': is_ordinal,
                        'ordinal_num': ordinal_num,
                        'original_id': _normalize_concept_id(concept.get("ORIGINAL_CONCEPT_ID")) if concept.get("ORIGINAL_CONCEPT_ID") else None
                    }
    
    if not concept_info:
        fig = go.Figure()
        fig.update_layout(height=200, showlegend=False)
        fig.add_annotation(text="No concepts to display", x=0.5, y=0.5,
                          xref="paper", yref="paper", showarrow=False)
        return fig, []
    
    # Build patient -> concepts mapping (which concepts each patient has)
    patient_concepts = {}  # person_id -> set of concept_ids
    
    for _, row in df_target.iterrows():
        person_id = row['PERSON_ID']
        if person_id not in target_patient_ids:
            continue
        
        raw_concept_id = _normalize_concept_id(row['CONCEPT_ID'])
        time_list = row.get('TIME_TO_EVENT', [])
        
        # Count occurrences
        if isinstance(time_list, np.ndarray):
            num_occurrences = len([t for t in time_list if not np.isnan(t)])
        elif isinstance(time_list, (list, tuple)):
            num_occurrences = len([t for t in time_list if t is not None and not (isinstance(t, (int, float)) and pd.isna(t))])
        elif isinstance(time_list, (int, float)) and not pd.isna(time_list):
            num_occurrences = 1
        else:
            num_occurrences = 0
        
        if num_occurrences > 0:
            if person_id not in patient_concepts:
                patient_concepts[person_id] = {}
            patient_concepts[person_id][raw_concept_id] = num_occurrences
    
    # Build binary matrix: which concepts each patient has
    concept_ids = list(concept_info.keys())
    n_concepts = len(concept_ids)
    
    # For each concept, get set of patients that have it
    concept_patients = {}  # concept_id -> set of patient_ids
    for cid, info in concept_info.items():
        patients_with_concept = set()
        
        if info.get('is_ordinal') and info.get('original_id') and info.get('ordinal_num'):
            # For ordinals, patient must have >= ordinal_num occurrences
            orig_id = info['original_id']
            ordinal_num = info['ordinal_num']
            for pid, concepts in patient_concepts.items():
                if orig_id in concepts and concepts[orig_id] >= ordinal_num:
                    patients_with_concept.add(pid)
        else:
            # For main concepts
            for pid, concepts in patient_concepts.items():
                if cid in concepts:
                    patients_with_concept.add(pid)
        
        concept_patients[cid] = patients_with_concept
    
    # Calculate BOTH Jaccard and Correlation matrices
    jaccard_matrix = np.zeros((n_concepts, n_concepts))
    corr_matrix = np.zeros((n_concepts, n_concepts))
    
    for i, cid_i in enumerate(concept_ids):
        patients_i = concept_patients[cid_i]
        for j, cid_j in enumerate(concept_ids):
            patients_j = concept_patients[cid_j]
            
            if i == j:
                jaccard_matrix[i, j] = 1.0
                corr_matrix[i, j] = 1.0
            else:
                intersection = len(patients_i & patients_j)
                union = len(patients_i | patients_j)
                
                # Jaccard index
                jaccard_matrix[i, j] = intersection / union if union > 0 else 0
                
                # Phi coefficient (correlation)
                n = len(target_patient_ids)
                n_11 = intersection
                n_10 = len(patients_i - patients_j)
                n_01 = len(patients_j - patients_i)
                n_00 = n - n_11 - n_10 - n_01
                
                denom = np.sqrt((n_11 + n_10) * (n_11 + n_01) * (n_00 + n_10) * (n_00 + n_01))
                if denom > 0:
                    corr_matrix[i, j] = (n_11 * n_00 - n_10 * n_01) / denom
                else:
                    corr_matrix[i, j] = 0
    
    # Create combined matrix with disjoint value bands:
    # - Correlation (upper): -1..1 mapped to 0.00..0.49
    # - Jaccard (lower): 0..1 mapped to 0.51..1.00
    # - Diagonal: neutral midpoint (0.50)
    combined_matrix = np.zeros((n_concepts, n_concepts))
    for i in range(n_concepts):
        for j in range(n_concepts):
            if i == j:
                combined_matrix[i, j] = 0.5  # Neutral diagonal
            elif i < j:
                # Upper triangle: Correlation mapped to 0.00..0.49
                combined_matrix[i, j] = ((corr_matrix[i, j] + 1) / 2) * 0.49
            else:
                # Lower triangle: Jaccard mapped to 0.51..1.00
                combined_matrix[i, j] = 0.51 + (jaccard_matrix[i, j] * 0.49)
    
    # Axis labels:
    # - X-axis keeps existing shortened labels
    # - Y-axis shows up to 60 characters
    x_label_base = [concept_info[cid]['display_name'] for cid in concept_ids]
    x_labels = _deduplicate_axis_labels(x_label_base, concept_ids, concept_info)

    def build_y_label(cid):
        info = concept_info[cid]
        base_name = info['name']
        if info.get('is_ordinal') and info.get('ordinal_num'):
            ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(info['ordinal_num'], f"{info['ordinal_num']}th")
            full_label = f"{base_name} ({ordinal_suffix})"
        else:
            full_label = base_name
        return f"{full_label[:60]}..." if len(full_label) > 60 else full_label

    y_label_base = [build_y_label(cid) for cid in concept_ids]
    y_labels = _deduplicate_axis_labels(y_label_base, concept_ids, concept_info)
    # Full names for hover
    full_names = [concept_info[cid]['name'] for cid in concept_ids]
    
    # Create hover text with full concept names and BOTH metrics
    hover_text = []
    for i, cid_i in enumerate(concept_ids):
        row_text = []
        for j, cid_j in enumerate(concept_ids):
            full_name_i = full_names[i]
            full_name_j = full_names[j]
            # Add ordinal suffix to full names if applicable
            info_i = concept_info[cid_i]
            info_j = concept_info[cid_j]
            if info_i.get('is_ordinal') and info_i.get('ordinal_num'):
                ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(info_i['ordinal_num'], f"{info_i['ordinal_num']}th")
                full_name_i = f"{full_name_i} ({ordinal_suffix})"
            if info_j.get('is_ordinal') and info_j.get('ordinal_num'):
                ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(info_j['ordinal_num'], f"{info_j['ordinal_num']}th")
                full_name_j = f"{full_name_j} ({ordinal_suffix})"
            
            if i == j:
                prevalence = len(concept_patients[cid_i]) / len(target_patient_ids) if target_patient_ids else 0
                row_text.append(f"<b>A:</b> {full_name_i}<br><br>Prevalence: {prevalence:.2%}<br>Patients: {len(concept_patients[cid_i])}")
            else:
                intersection = len(concept_patients[cid_i] & concept_patients[cid_j])
                union = len(concept_patients[cid_i] | concept_patients[cid_j])
                jaccard_val = jaccard_matrix[i, j]
                corr_val = corr_matrix[i, j]
                
                if i < j:
                    # Upper triangle - emphasize Correlation
                    row_text.append(
                        f"<b>A:</b> {full_name_i}<br>"
                        f"<b>B:</b> {full_name_j}<br><br>"
                        f"<b>Correlation (Phi):</b> {corr_val:.2f}<br>"
                        f"Jaccard Index: {jaccard_val:.2f}<br>"
                        f"Co-occur: {intersection} patients"
                    )
                else:
                    # Lower triangle - emphasize Jaccard
                    row_text.append(
                        f"<b>A:</b> {full_name_i}<br>"
                        f"<b>B:</b> {full_name_j}<br><br>"
                        f"<b>Jaccard Index:</b> {jaccard_val:.2f}<br>"
                        f"Correlation (Phi): {corr_val:.2f}<br>"
                        f"Co-occur: {intersection} patients<br>"
                        f"Union: {union} patients"
                    )
        hover_text.append(row_text)
    
    # Custom mixed colorscale with disjoint bands:
    # 0.00..0.49 = Correlation (red -> white -> blue, centered at 0.245 == corr 0.0)
    # 0.50       = Neutral divider/diagonal (white)
    # 0.51..1.00 = Jaccard (white -> light green -> green -> dark green)
    custom_colorscale = [
        [0.0, 'rgb(178, 24, 43)'],    # Dark red
        [0.1225, 'rgb(239, 138, 98)'],# Light red
        [0.245, 'rgb(255, 255, 255)'],# Correlation zero
        [0.3675, 'rgb(103, 169, 207)'],# Light blue
        [0.49, 'rgb(33, 102, 172)'],  # Dark blue (corr +1)
        [0.50, 'rgb(255, 255, 255)'], # Neutral divider
        [0.51, 'rgb(255, 255, 255)'], # Jaccard zero
        [0.70, 'rgb(217, 240, 211)'], # Light green (Jaccard)
        [0.85, 'rgb(116, 196, 118)'], # Green (Jaccard)
        [1.0, 'rgb(35, 139, 69)']     # Dark green (Jaccard high)
    ]
    
    # Build diagonal-only prevalence matrix (off-diagonal as NaN for transparency)
    diagonal_prevalence = np.full((n_concepts, n_concepts), np.nan)
    diagonal_hover_text = [["" for _ in range(n_concepts)] for _ in range(n_concepts)]
    for i, cid in enumerate(concept_ids):
        prevalence = len(concept_patients[cid]) / len(target_patient_ids) if target_patient_ids else 0
        diagonal_prevalence[i, i] = prevalence
        diagonal_hover_text[i][i] = (
            f"<b>A:</b> {full_names[i]}<br><br>"
            f"<b>Prevalence:</b> {prevalence:.2%}<br>"
            f"Patients: {len(concept_patients[cid])}"
        )
    
    # Dedicated diagonal color (prevalence): fixed sand
    prevalence_colorscale = [
        [0.0, "rgb(224, 205, 162)"],
        [1.0, "rgb(224, 205, 162)"]
    ]
    
    fig = go.Figure()
    fig.add_trace(go.Heatmap(
        z=combined_matrix,
        x=x_labels,
        y=y_labels,
        colorscale=custom_colorscale,
        zmin=0,
        zmax=1,
        text=hover_text,
        hoverinfo='text',
        showscale=False,  # Hide default colorbar since we have mixed scales
        hoverongaps=False,
        zsmooth=False
    ))
    fig.add_trace(go.Heatmap(
        z=diagonal_prevalence,
        x=x_labels,
        y=y_labels,
        colorscale=prevalence_colorscale,
        zmin=0,
        zmax=1,
        text=diagonal_hover_text,
        hoverinfo='skip',
        hovertemplate=None,
        hoverongaps=False,
        showscale=False,
        zsmooth=False
    ))
    
    # Add value annotations for cells - show raw values (not scaled)
    def get_text_color(val):
        return 'black'
    
    annotations = []
    for i in range(n_concepts):
        for j in range(n_concepts):
            display_val = combined_matrix[i, j]
            
            if i == j:
                # Diagonal - show prevalence
                prevalence = len(concept_patients[concept_ids[i]]) / len(target_patient_ids) if target_patient_ids else 0
                text_color = get_text_color(display_val)
                annotations.append(dict(
                    x=x_labels[j],
                    y=y_labels[i],
                    text=f"{prevalence:.0%}",
                    showarrow=False,
                    font=dict(size=9, color=text_color)
                ))
            elif i < j:
                # Upper triangle - show Correlation
                corr_val = corr_matrix[i, j]
                text_color = get_text_color(display_val)
                annotations.append(dict(
                    x=x_labels[j],
                    y=y_labels[i],
                    text=f"{corr_val:.2f}",
                    showarrow=False,
                    font=dict(size=9, color=text_color)
                ))
            else:
                # Lower triangle - show Jaccard
                jaccard_val = jaccard_matrix[i, j]
                text_color = get_text_color(display_val)
                annotations.append(dict(
                    x=x_labels[j],
                    y=y_labels[i],
                    text=f"{jaccard_val:.2f}",
                    showarrow=False,
                    font=dict(size=9, color=text_color)
                ))
    
    # Keep matrix cell geometry stable across concept counts.
    # Matrix area controls cell size; figure width/height then add label margins.
    matrix_pixels = max(260, n_concepts * 46)
    max_y_label_len = max((len(label) for label in y_labels), default=0)
    left_margin = min(520, max(180, int(max_y_label_len * 5.5)))
    right_margin = 50
    top_margin = 60
    bottom_margin = 150
    fig_width = left_margin + matrix_pixels + right_margin
    fig_height = top_margin + matrix_pixels + bottom_margin
    
    # Title with legend explanation
    title_text = "Concept Co-occurrence"
    if selected_group != "overall":
        cluster_num = selected_group.split("_")[1]
        title_text += f" - Cluster {cluster_num}"
    else:
        title_text += " - Overall"
    title_text += "<br><sup>Upper △: Correlation (Phi) | Lower △: Jaccard Index | Diagonal: Prevalence</sup>"
    
    fig.update_layout(
        title=dict(text=title_text, x=0.5, font=dict(size=14)),
        height=fig_height,
        width=fig_width,
        xaxis=dict(
            tickangle=45,
            tickfont=dict(size=10),
            side="bottom",
            constrain="domain"
        ),
        yaxis=dict(
            tickfont=dict(size=10),
            autorange="reversed",  # Top to bottom
            scaleanchor="x",
            scaleratio=1,
            constrain="domain"
        ),
        margin=dict(l=left_margin, r=right_margin, t=top_margin, b=bottom_margin),
        annotations=annotations
    )
    
    # Build pairwise data for table (only lower triangle to avoid duplicates)
    pairwise_data = []
    for i in range(n_concepts):
        for j in range(i + 1, n_concepts):
            cid_i = concept_ids[i]
            cid_j = concept_ids[j]
            
            # Get full names with ordinal suffix
            full_name_i = full_names[i]
            full_name_j = full_names[j]
            info_i = concept_info[cid_i]
            info_j = concept_info[cid_j]
            if info_i.get('is_ordinal') and info_i.get('ordinal_num'):
                ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(info_i['ordinal_num'], f"{info_i['ordinal_num']}th")
                full_name_i = f"{full_name_i} ({ordinal_suffix})"
            if info_j.get('is_ordinal') and info_j.get('ordinal_num'):
                ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(info_j['ordinal_num'], f"{info_j['ordinal_num']}th")
                full_name_j = f"{full_name_j} ({ordinal_suffix})"
            
            intersection = len(concept_patients[cid_i] & concept_patients[cid_j])
            union = len(concept_patients[cid_i] | concept_patients[cid_j])
            
            pairwise_data.append({
                'Concept 1': full_name_i,
                'Concept 2': full_name_j,
                'Correlation': round(corr_matrix[i, j], 3),
                'Jaccard': round(jaccard_matrix[i, j], 3),
                'Co-occur': intersection,
                'Total': union
            })
    
    # Sort by absolute correlation (strongest relationships first)
    pairwise_data.sort(key=lambda x: abs(x['Correlation']), reverse=True)
    
    return fig, pairwise_data


def create_trajectory_plot_from_summary(
    clustering_results: Dict,
    clustering_summary_matrix: pd.DataFrame,
    heritage_groups_order: Dict[str, List[Dict]],
    active_concept_ids: Set[str],
    min_prevalence_pct: int = 0,
    ordering_mode: str = "order",
    selected_cluster: Optional[str] = None,
) -> go.Figure:
    """
    Create trajectory ranking matrix from pre-computed summary data.
    
    Args:
        clustering_results: Dictionary with optimal_cluster_count, cluster_counts, etc.
        clustering_summary_matrix: Pre-computed DataFrame with time_median, prevalence per concept per cluster
        heritage_groups_order: Dictionary mapping heritage to list of items in display order
        active_concept_ids: Set of active concept IDs to include
        min_prevalence_pct: Minimum prevalence percentage (0-100) to include concept
        ordering_mode: One of "order", "cluster", "movers", "stable"
        selected_cluster: Selected cluster label ("C1", "C2", etc.) for cluster ordering mode
        
    Returns:
        Plotly figure with trajectory rank matrix
    """
    optimal_k = clustering_results.get('optimal_cluster_count', 2)
    cluster_counts = clustering_results.get('cluster_counts', {})
    
    if clustering_summary_matrix.empty:
        fig = go.Figure()
        fig.update_layout(
            height=200,
            showlegend=False,
            plot_bgcolor="white",
            paper_bgcolor="white",
            annotations=[dict(
                text="No clustering data available",
                x=0.5, y=0.5,
                xref="paper", yref="paper",
                showarrow=False,
                font=dict(size=14)
            )]
        )
        return fig
    
    # Build concept info from heritage_groups_order
    concept_info = {}  # concept_id -> {name, heritage, is_ordinal, ordinal_num}
    main_concepts_with_ordinals = set()
    
    # First pass: identify main concepts that have ordinals
    for heritage, concepts in heritage_groups_order.items():
        for concept in concepts:
            is_ordinal = concept.get("IS_ORDINAL", False)
            if is_ordinal:
                original_id = concept.get("ORIGINAL_CONCEPT_ID")
                if original_id:
                    main_concepts_with_ordinals.add(_normalize_concept_id(original_id))
    
    # Second pass: build concept_info
    for heritage, concepts in heritage_groups_order.items():
        for concept in concepts:
            concept_id = concept.get("_concept_id") or concept.get("CONCEPT_ID")
            if concept_id:
                norm_id = _normalize_concept_id(concept_id)
                if norm_id in active_concept_ids:
                    name = concept.get("CONCEPT_NAME") or concept.get("_display_name", str(concept_id))
                    is_ordinal = concept.get("IS_ORDINAL", False)
                    original_id = concept.get("ORIGINAL_CONCEPT_ID")
                    
                    # Skip main concepts that have ordinals
                    if not is_ordinal and norm_id in main_concepts_with_ordinals:
                        continue
                    
                    ordinal_num = None
                    if is_ordinal:
                        if original_id:
                            orig_norm = _normalize_concept_id(original_id)
                            if norm_id.startswith(orig_norm) and len(norm_id) > len(orig_norm):
                                try:
                                    ordinal_num = int(norm_id[len(orig_norm):])
                                except ValueError:
                                    pass
                        # Fallback: try to extract from name
                        if ordinal_num is None:
                            for i in range(1, 10):
                                if f"≥{i}" in name or f">={i}" in name or f"({i})" in name:
                                    ordinal_num = i
                                    break
                    
                    concept_info[norm_id] = {
                        'name': name,
                        'heritage': heritage,
                        'is_ordinal': is_ordinal,
                        'ordinal_num': ordinal_num
                    }
    
    if not concept_info:
        fig = go.Figure()
        fig.update_layout(height=200, showlegend=False)
        fig.add_annotation(text="No active concepts found", x=0.5, y=0.5,
                          xref="paper", yref="paper", showarrow=False)
        return fig

    cluster_labels = [f"C{i + 1}" for i in range(optimal_k)]
    cluster_stats = {label: {} for label in cluster_labels}

    def _normalize_cluster_label(raw_label) -> str:
        label = str(raw_label).strip()
        if label in cluster_stats:
            return label
        if label.isdigit():
            candidate = f"C{label}"
            if candidate in cluster_stats:
                return candidate
        lower = label.lower()
        if lower.startswith("cluster_"):
            suffix = lower.split("_", 1)[1]
            if suffix.isdigit():
                candidate = f"C{suffix}"
                if candidate in cluster_stats:
                    return candidate
        return label

    def _cluster_size(label: str) -> int:
        value = cluster_counts.get(label, 1)
        try:
            return int(value)
        except (TypeError, ValueError):
            return 1

    concept_col = "CONCEPT_ID" if "CONCEPT_ID" in clustering_summary_matrix.columns else "concept_id"
    cluster_col = "cluster" if "cluster" in clustering_summary_matrix.columns else None
    median_col = "time_median" if "time_median" in clustering_summary_matrix.columns else "median_days"
    prevalence_col = "prevalence"

    if cluster_col is None or concept_col not in clustering_summary_matrix.columns:
        fig = go.Figure()
        fig.update_layout(height=200, showlegend=False)
        fig.add_annotation(
            text="Clustering summary missing required columns",
            x=0.5, y=0.5, xref="paper", yref="paper", showarrow=False
        )
        return fig

    for _, row in clustering_summary_matrix.iterrows():
        cluster_label = _normalize_cluster_label(row.get(cluster_col, ""))
        if cluster_label not in cluster_stats:
            continue

        concept_id = _normalize_concept_id(row.get(concept_col, ""))
        if concept_id not in concept_info:
            continue

        median_time = row.get(median_col)
        prevalence_raw = row.get(prevalence_col, 0)
        if median_time is None or pd.isna(median_time):
            continue

        if pd.isna(prevalence_raw):
            prevalence_pct = 0.0
        else:
            prevalence_pct = float(prevalence_raw) * 100 if float(prevalence_raw) <= 1 else float(prevalence_raw)

        cluster_stats[cluster_label][concept_id] = {
            "median": float(median_time),
            "prevalence": prevalence_pct
        }

    # Calculate overall stats by weighting cluster stats by cluster size.
    overall_stats = {}
    for cid in concept_info.keys():
        medians = []
        total_weight = 0
        weighted_prev = 0
        
        for cluster_label in cluster_labels:
            if cid in cluster_stats[cluster_label]:
                stats = cluster_stats[cluster_label][cid]
                count = _cluster_size(cluster_label)
                medians.append((stats['median'], count))
                weighted_prev += stats['prevalence'] * count
                total_weight += count
        
        if medians and total_weight > 0:
            weighted_median = sum(m * w for m, w in medians) / sum(w for _, w in medians)
            avg_prevalence = weighted_prev / total_weight
            overall_stats[cid] = {
                'median': weighted_median,
                'prevalence': avg_prevalence
            }

    overall_order = _sort_trajectory_items(
        overall_stats,
        concept_info,
        min_prevalence_pct=min_prevalence_pct
    )
    cluster_orders = {
        label: _sort_trajectory_items(
            cluster_stats[label],
            concept_info,
            min_prevalence_pct=min_prevalence_pct
        )
        for label in cluster_labels
    }

    return _create_trajectory_rank_matrix(
        concept_info=concept_info,
        overall_order=overall_order,
        cluster_orders=cluster_orders,
        cluster_labels=cluster_labels,
        cluster_counts={label: _cluster_size(label) for label in cluster_labels},
        min_prevalence_pct=min_prevalence_pct,
        ordering_mode=ordering_mode,
        preferred_cluster=selected_cluster,
    )


def create_overlap_plot_from_summary(
    clustering_results: Dict,
    pairwise_overlap_df: pd.DataFrame,
    heritage_groups_order: Dict[str, List[Dict]],
    active_concept_ids: Set[str],
    selected_group: str = "overall"
) -> Tuple[go.Figure, List[Dict]]:
    """
    Create a concept overlap visualization from pre-computed summary data.
    Shows Jaccard index, Phi correlation, and prevalence in a heatmap.
    
    Args:
        clustering_results: Dictionary with optimal_cluster_count, cluster_counts, etc.
        pairwise_overlap_df: Pre-computed DataFrame with jaccard, phi_correlation per concept pair
        heritage_groups_order: Dictionary mapping heritage to list of items in display order
        active_concept_ids: Set of active concept IDs to include
        selected_group: Which group to show ("overall" or "C1", "C2", etc.)
        
    Returns:
        Tuple of (Plotly figure, pairwise data list for table)
    """
    if pairwise_overlap_df is None or pairwise_overlap_df.empty:
        fig = go.Figure()
        fig.update_layout(height=200, showlegend=False)
        fig.add_annotation(text="No overlap data available", x=0.5, y=0.5,
                          xref="paper", yref="paper", showarrow=False)
        return fig, []
    
    # Filter pairwise_overlap_df to selected group FIRST to get available concepts
    group_filter = selected_group if selected_group != "overall" else "overall"
    group_data = pairwise_overlap_df[pairwise_overlap_df["group"] == group_filter].copy()
    
    if group_data.empty:
        fig = go.Figure()
        fig.update_layout(height=200, showlegend=False)
        fig.add_annotation(text=f"No overlap data for {selected_group}", x=0.5, y=0.5,
                          xref="paper", yref="paper", showarrow=False)
        return fig, []
    
    # Get list of concept IDs available in the pairwise data
    all_concept_ids_in_data = set()
    for col in ["concept_id_1", "concept_id_2"]:
        if col in group_data.columns:
            all_concept_ids_in_data.update(group_data[col].apply(lambda x: _normalize_concept_id(x)).unique())
    
    # Build concept info from heritage_groups_order - include both main AND ordinal concepts
    # But exclude main concepts if their ordinals are being displayed
    concept_info = {}
    
    # First pass: identify which main concepts have active ordinals in the data
    main_concepts_with_active_ordinals = set()
    
    for heritage, concepts in heritage_groups_order.items():
        for concept in concepts:
            concept_id = concept.get("_concept_id") or concept.get("CONCEPT_ID")
            if concept_id:
                norm_id = _normalize_concept_id(concept_id)
                is_ordinal = concept.get("IS_ORDINAL", False)
                
                # Check if this ordinal concept is active and in data
                if is_ordinal and norm_id in active_concept_ids and norm_id in all_concept_ids_in_data:
                    original_id = concept.get("ORIGINAL_CONCEPT_ID")
                    if original_id:
                        main_concepts_with_active_ordinals.add(_normalize_concept_id(original_id))
    
    # Second pass: build concept_info, excluding main concepts that have active ordinals
    for heritage, concepts in heritage_groups_order.items():
        for concept in concepts:
            concept_id = concept.get("_concept_id") or concept.get("CONCEPT_ID")
            if concept_id:
                norm_id = _normalize_concept_id(concept_id)
                
                # Check if this concept is active
                if norm_id not in active_concept_ids:
                    continue
                
                # Check if this concept is in the pairwise data
                if norm_id not in all_concept_ids_in_data:
                    continue
                
                is_ordinal = concept.get("IS_ORDINAL", False)
                
                # Skip main concepts that have active ordinals (like patient view)
                if not is_ordinal and norm_id in main_concepts_with_active_ordinals:
                    continue
                
                name = concept.get("CONCEPT_NAME") or concept.get("_display_name", str(concept_id))
                
                # For ordinal concepts, extract the ordinal number for display
                ordinal_num = None
                if is_ordinal:
                    original_id = concept.get("ORIGINAL_CONCEPT_ID")
                    if original_id:
                        orig_norm = _normalize_concept_id(original_id)
                        if norm_id.startswith(orig_norm) and len(norm_id) > len(orig_norm):
                            try:
                                ordinal_num = int(norm_id[len(orig_norm):])
                            except ValueError:
                                pass
                
                # Create display name
                if is_ordinal and ordinal_num:
                    ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(ordinal_num, f"{ordinal_num}th")
                    display_name = f"{name[:25]}... ({ordinal_suffix})" if len(name) > 25 else f"{name} ({ordinal_suffix})"
                else:
                    display_name = f"{name[:30]}..." if len(name) > 30 else name
                
                concept_info[norm_id] = {
                    'name': name,
                    'display_name': display_name,
                    'heritage': heritage,
                    'is_ordinal': is_ordinal,
                    'ordinal_num': ordinal_num
                }
    
    if not concept_info:
        fig = go.Figure()
        fig.update_layout(height=200, showlegend=False)
        fig.add_annotation(text="No active concepts found in overlap data", x=0.5, y=0.5,
                          xref="paper", yref="paper", showarrow=False)
        return fig, []
    
    # Now concept_info contains ordinals + main concepts without ordinals
    active_in_data = list(concept_info.keys())
    
    concept_ids = active_in_data
    n_concepts = len(concept_ids)
    
    # Build matrices
    jaccard_matrix = np.zeros((n_concepts, n_concepts))
    corr_matrix = np.zeros((n_concepts, n_concepts))
    prevalence_diag = {}
    co_occurrence = {}
    union_counts = {}
    
    # Index mapping
    concept_to_idx = {cid: i for i, cid in enumerate(concept_ids)}
    
    for _, row in group_data.iterrows():
        cid1 = _normalize_concept_id(row.get("concept_id_1", ""))
        cid2 = _normalize_concept_id(row.get("concept_id_2", ""))
        
        if cid1 not in concept_to_idx or cid2 not in concept_to_idx:
            continue
        
        i = concept_to_idx[cid1]
        j = concept_to_idx[cid2]
        
        if i == j:
            # Diagonal - prevalence
            prevalence_diag[cid1] = row.get("prevalence", 0)
            jaccard_matrix[i, j] = 1.0
            corr_matrix[i, j] = 1.0
        else:
            jaccard = row.get("jaccard", 0)
            phi = row.get("phi_correlation", 0)
            jaccard_matrix[i, j] = jaccard
            jaccard_matrix[j, i] = jaccard
            corr_matrix[i, j] = phi
            corr_matrix[j, i] = phi
            co_occurrence[(cid1, cid2)] = row.get("co_occurrence", 0)
            union_counts[(cid1, cid2)] = row.get("union", 0)
    
    # Create combined matrix with disjoint value bands:
    # - Correlation (upper): -1..1 mapped to 0.00..0.49
    # - Jaccard (lower): 0..1 mapped to 0.51..1.00
    # - Diagonal: neutral midpoint (0.50)
    combined_matrix = np.zeros((n_concepts, n_concepts))
    for i in range(n_concepts):
        for j in range(n_concepts):
            if i == j:
                combined_matrix[i, j] = 0.5
            elif i < j:
                # Upper triangle: Correlation mapped to 0.00..0.49
                combined_matrix[i, j] = ((corr_matrix[i, j] + 1) / 2) * 0.49
            else:
                # Lower triangle: Jaccard mapped to 0.51..1.00
                combined_matrix[i, j] = 0.51 + (jaccard_matrix[i, j] * 0.49)
    
    # Axis labels:
    # - X-axis keeps existing shortened labels
    # - Y-axis shows up to 60 characters
    x_label_base = [concept_info[cid]['display_name'] for cid in concept_ids]
    x_labels = _deduplicate_axis_labels(x_label_base, concept_ids, concept_info)

    def build_y_label(cid):
        info = concept_info[cid]
        base_name = info['name']
        if info.get('is_ordinal') and info.get('ordinal_num'):
            ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(info['ordinal_num'], f"{info['ordinal_num']}th")
            full_label = f"{base_name} ({ordinal_suffix})"
        else:
            full_label = base_name
        return f"{full_label[:60]}..." if len(full_label) > 60 else full_label

    y_label_base = [build_y_label(cid) for cid in concept_ids]
    y_labels = _deduplicate_axis_labels(y_label_base, concept_ids, concept_info)
    full_names = [concept_info[cid]['name'] for cid in concept_ids]
    
    # Create hover text
    hover_text = []
    for i, cid_i in enumerate(concept_ids):
        row_text = []
        for j, cid_j in enumerate(concept_ids):
            full_name_i = full_names[i]
            full_name_j = full_names[j]
            info_i = concept_info[cid_i]
            info_j = concept_info[cid_j]
            
            if info_i.get('is_ordinal') and info_i.get('ordinal_num'):
                ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(info_i['ordinal_num'], f"{info_i['ordinal_num']}th")
                full_name_i = f"{full_name_i} ({ordinal_suffix})"
            if info_j.get('is_ordinal') and info_j.get('ordinal_num'):
                ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(info_j['ordinal_num'], f"{info_j['ordinal_num']}th")
                full_name_j = f"{full_name_j} ({ordinal_suffix})"
            
            if i == j:
                prevalence = prevalence_diag.get(cid_i, 0)
                row_text.append(f"<b>A:</b> {full_name_i}<br><br>Prevalence: {prevalence:.2%}")
            else:
                jaccard_val = jaccard_matrix[i, j]
                corr_val = corr_matrix[i, j]
                cooccur_raw = co_occurrence.get((cid_i, cid_j), co_occurrence.get((cid_j, cid_i), 0))
                union_raw = union_counts.get((cid_i, cid_j), union_counts.get((cid_j, cid_i), 0))
                cooccur = int(round(cooccur_raw)) if pd.notna(cooccur_raw) else 0
                union = int(round(union_raw)) if pd.notna(union_raw) else 0
                
                if i < j:
                    row_text.append(
                        f"<b>A:</b> {full_name_i}<br>"
                        f"<b>B:</b> {full_name_j}<br><br>"
                        f"<b>Correlation (Phi):</b> {corr_val:.2f}<br>"
                        f"Jaccard Index: {jaccard_val:.2f}<br>"
                        f"Co-occur: {cooccur} patients"
                    )
                else:
                    row_text.append(
                        f"<b>A:</b> {full_name_i}<br>"
                        f"<b>B:</b> {full_name_j}<br><br>"
                        f"<b>Jaccard Index:</b> {jaccard_val:.2f}<br>"
                        f"Correlation (Phi): {corr_val:.2f}<br>"
                        f"Co-occur: {cooccur} patients<br>"
                        f"Union: {union} patients"
                    )
        hover_text.append(row_text)
    
    # Custom mixed colorscale with disjoint bands:
    # 0.00..0.49 = Correlation (red -> white -> blue, centered at 0.245 == corr 0.0)
    # 0.50       = Neutral divider/diagonal (white)
    # 0.51..1.00 = Jaccard (white -> light green -> green -> dark green)
    custom_colorscale = [
        [0.0, 'rgb(178, 24, 43)'],
        [0.1225, 'rgb(239, 138, 98)'],
        [0.245, 'rgb(255, 255, 255)'],
        [0.3675, 'rgb(103, 169, 207)'],
        [0.49, 'rgb(33, 102, 172)'],
        [0.50, 'rgb(255, 255, 255)'],
        [0.51, 'rgb(255, 255, 255)'],
        [0.70, 'rgb(217, 240, 211)'],
        [0.85, 'rgb(116, 196, 118)'],
        [1.0, 'rgb(35, 139, 69)']
    ]
    
    # Build diagonal-only prevalence matrix (off-diagonal as NaN for transparency)
    diagonal_prevalence = np.full((n_concepts, n_concepts), np.nan)
    diagonal_hover_text = [["" for _ in range(n_concepts)] for _ in range(n_concepts)]
    for i, cid in enumerate(concept_ids):
        prevalence = prevalence_diag.get(cid, 0)
        diagonal_prevalence[i, i] = prevalence
        diagonal_hover_text[i][i] = (
            f"<b>A:</b> {full_names[i]}<br><br>"
            f"<b>Prevalence:</b> {prevalence:.2%}"
        )
    
    # Dedicated diagonal color (prevalence): fixed sand
    prevalence_colorscale = [
        [0.0, "rgb(224, 205, 162)"],
        [1.0, "rgb(224, 205, 162)"]
    ]
    
    fig = go.Figure()
    fig.add_trace(go.Heatmap(
        z=combined_matrix,
        x=x_labels,
        y=y_labels,
        colorscale=custom_colorscale,
        zmin=0,
        zmax=1,
        text=hover_text,
        hoverinfo='text',
        hoverongaps=False,
        showscale=False,
        zsmooth=False
    ))
    fig.add_trace(go.Heatmap(
        z=diagonal_prevalence,
        x=x_labels,
        y=y_labels,
        colorscale=prevalence_colorscale,
        zmin=0,
        zmax=1,
        text=diagonal_hover_text,
        hoverinfo='skip',
        hovertemplate=None,
        hoverongaps=False,
        showscale=False,
        zsmooth=False
    ))
    
    # Add value annotations
    def get_text_color(val):
        return 'black'
    
    annotations = []
    for i in range(n_concepts):
        for j in range(n_concepts):
            display_val = combined_matrix[i, j]
            
            if i == j:
                prevalence = prevalence_diag.get(concept_ids[i], 0)
                text_color = get_text_color(display_val)
                annotations.append(dict(
                    x=x_labels[j],
                    y=y_labels[i],
                    text=f"{prevalence:.0%}",
                    showarrow=False,
                    font=dict(size=9, color=text_color)
                ))
            elif i < j:
                corr_val = corr_matrix[i, j]
                text_color = get_text_color(display_val)
                annotations.append(dict(
                    x=x_labels[j],
                    y=y_labels[i],
                    text=f"{corr_val:.2f}",
                    showarrow=False,
                    font=dict(size=9, color=text_color)
                ))
            else:
                jaccard_val = jaccard_matrix[i, j]
                text_color = get_text_color(display_val)
                annotations.append(dict(
                    x=x_labels[j],
                    y=y_labels[i],
                    text=f"{jaccard_val:.2f}",
                    showarrow=False,
                    font=dict(size=9, color=text_color)
                ))
    
    matrix_pixels = max(260, n_concepts * 46)
    max_y_label_len = max((len(label) for label in y_labels), default=0)
    left_margin = min(520, max(180, int(max_y_label_len * 5.5)))
    right_margin = 50
    top_margin = 60
    bottom_margin = 150
    fig_width = left_margin + matrix_pixels + right_margin
    fig_height = top_margin + matrix_pixels + bottom_margin
    
    title_text = "Concept Co-occurrence"
    if selected_group != "overall":
        title_text += f" - Cluster {selected_group[1:]}"
    else:
        title_text += " - Overall"
    title_text += "<br><sup>Upper △: Correlation (Phi) | Lower △: Jaccard Index | Diagonal: Prevalence</sup>"
    
    fig.update_layout(
        title=dict(text=title_text, x=0.5, font=dict(size=14)),
        height=fig_height,
        width=fig_width,
        xaxis=dict(
            tickangle=45,
            tickfont=dict(size=10),
            side="bottom",
            constrain="domain"
        ),
        yaxis=dict(
            tickfont=dict(size=10),
            autorange="reversed",
            scaleanchor="x",
            scaleratio=1,
            constrain="domain"
        ),
        margin=dict(l=left_margin, r=right_margin, t=top_margin, b=bottom_margin),
        annotations=annotations
    )
    
    # Build pairwise data for table
    pairwise_data = []
    for i in range(n_concepts):
        for j in range(i + 1, n_concepts):
            cid_i = concept_ids[i]
            cid_j = concept_ids[j]
            
            full_name_i = full_names[i]
            full_name_j = full_names[j]
            info_i = concept_info[cid_i]
            info_j = concept_info[cid_j]
            
            if info_i.get('is_ordinal') and info_i.get('ordinal_num'):
                ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(info_i['ordinal_num'], f"{info_i['ordinal_num']}th")
                full_name_i = f"{full_name_i} ({ordinal_suffix})"
            if info_j.get('is_ordinal') and info_j.get('ordinal_num'):
                ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(info_j['ordinal_num'], f"{info_j['ordinal_num']}th")
                full_name_j = f"{full_name_j} ({ordinal_suffix})"
            
            cooccur_raw = co_occurrence.get((cid_i, cid_j), co_occurrence.get((cid_j, cid_i), 0))
            union_raw = union_counts.get((cid_i, cid_j), union_counts.get((cid_j, cid_i), 0))
            cooccur = int(round(cooccur_raw)) if pd.notna(cooccur_raw) else 0
            union = int(round(union_raw)) if pd.notna(union_raw) else 0
            
            pairwise_data.append({
                'Concept 1': full_name_i,
                'Concept 2': full_name_j,
                'Correlation': round(corr_matrix[i, j], 3),
                'Jaccard': round(jaccard_matrix[i, j], 3),
                'Co-occur': cooccur,
                'Total': union
            })
    
    pairwise_data.sort(key=lambda x: abs(x['Correlation']), reverse=True)
    
    return fig, pairwise_data
