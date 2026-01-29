"""
Clustering visualization plots.
"""

from typing import Dict, List, Optional, Tuple, Set
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
from models.registry import concept_registry


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
    
    for _, row in summary_matrix.iterrows():
        concept_name = normalize_concept_name(row['concept_name'])
        cluster = row['cluster']
        concept_id = row.get('concept_id', None)
        
        # Map by name (for main concepts)
        key = (concept_name, cluster)
        concept_cluster_map[key] = {
            'prevalence': row['prevalence'],
            'count_category': row['count_category'],
            'median_days': row['median_days']
        }
        
        # Also map by concept_id for direct matching
        if concept_id is not None:
            concept_id_str = str(concept_id)
            key_id = (concept_id_str, cluster)
            concept_id_cluster_map[key_id] = {
                'prevalence': row['prevalence'],
                'count_category': row['count_category'],
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
                    ordinal_cluster_map[ordinal_key] = {
                        'prevalence': row['prevalence'],
                        'count_category': row['count_category'],
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


def create_trajectory_plot(
    clustering_results: Dict,
    data_patients: pd.DataFrame,
    heritage_groups_order: Dict[str, List[Dict]],
    active_concept_ids: Set[str],
    min_prevalence_pct: int = 0
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
    
    # Create orderings for each column (sorted by median time)
    # Filter directly by prevalence threshold in each column (not globally)
    def get_ordered_concepts(stats: Dict) -> List[Tuple[str, float, float]]:
        """Return [(concept_id, median_time, prevalence)] sorted by median time.
        Only include concepts with prevalence >= min_prevalence_pct in THIS column."""
        items = [(cid, s['median'], s['prevalence']) 
                 for cid, s in stats.items() 
                 if cid in concept_info and s['prevalence'] >= min_prevalence_pct]
        items.sort(key=lambda x: x[1])  # Sort by median time
        return items
    
    overall_order = get_ordered_concepts(overall_stats)
    cluster_orders = {c: get_ordered_concepts(cluster_stats[c]) for c in range(optimal_k)}
    
    # Find max concepts in any column for height
    max_concepts = max(
        len(overall_order),
        max(len(cluster_orders[c]) for c in range(optimal_k)) if optimal_k > 0 else 0
    )
    
    if max_concepts == 0:
        fig = go.Figure()
        fig.update_layout(
            height=200,
            showlegend=False,
            annotations=[dict(
                text=f"No cells with ≥{min_prevalence_pct}% prevalence",
                x=0.5, y=0.5,
                xref="paper", yref="paper",
                showarrow=False,
                font=dict(size=14, color="#666")
            )]
        )
        return fig
    
    # Build overall position map for delta calculation
    overall_position = {}  # concept_id -> position (0-indexed, 0 = earliest)
    for pos, (cid, _, _) in enumerate(overall_order):
        overall_position[cid] = pos
    
    # Layout parameters (10% larger than before)
    cell_height = 72  # Taller cells for text wrapping (was 65)
    cell_width = 308  # Wider cells for concept names (was 280)
    col_gap = 20  # Gap between columns for vertical lines
    num_cols = 1 + optimal_k
    
    plot_height = max(400, max_concepts * cell_height + 120)
    total_width = num_cols * cell_width + (num_cols - 1) * col_gap + 50
    
    fig = go.Figure()
    annotations = []
    shapes = []
    
    # Add vertical dashed lines between columns
    for col_idx in range(1, num_cols):
        x_line = col_idx * (cell_width + col_gap) - col_gap / 2
        shapes.append(dict(
            type="line",
            x0=x_line, x1=x_line,
            y0=-10, y1=max_concepts * cell_height + 40,
            line=dict(color='#999', width=1, dash='dash'),
            layer="below"
        ))
    
    # Column headers
    column_labels = ["Overall"] + [f"Cluster {c+1}\n(n={cluster_patient_counts[c]})" for c in range(optimal_k)]
    
    for col_idx, label in enumerate(column_labels):
        x_center = col_idx * (cell_width + col_gap) + cell_width / 2
        annotations.append(dict(
            x=x_center, y=max_concepts * cell_height + 30,
            text=f"<b>{label}</b>",
            showarrow=False,
            font=dict(size=12, color='#2c3e50'),
            xref="x", yref="y",
            xanchor="center"
        ))
    
    # Helper to wrap text after ~50 chars
    def wrap_text(text: str, max_chars: int = 50) -> str:
        if len(text) <= max_chars:
            return text
        # Find a good break point near max_chars
        break_point = text.rfind(' ', 0, max_chars)
        if break_point == -1:
            break_point = max_chars
        return text[:break_point] + '<br>' + text[break_point:].strip()
    
    # Draw cells for each column
    def draw_column(col_idx: int, ordered_concepts: List[Tuple[str, float, float]], is_overall: bool = False):
        x_start = col_idx * (cell_width + col_gap)
        
        for row_idx, (cid, median_time, prevalence) in enumerate(ordered_concepts):
            info = concept_info.get(cid, {})
            name = info.get('name', cid)
            heritage = info.get('heritage', 'unknown')
            is_ordinal = info.get('is_ordinal', False)
            ordinal_num = info.get('ordinal_num')
            color = HERITAGE_COLORS.get(heritage, '#cccccc')
            
            # Y position (top to bottom, earliest at top)
            y_top = max_concepts * cell_height - row_idx * cell_height
            y_bottom = y_top - cell_height + 4  # Small gap between cells
            
            # Draw cell background
            shapes.append(dict(
                type="rect",
                x0=x_start, x1=x_start + cell_width,
                y0=y_bottom, y1=y_top,
                fillcolor=color,
                line=dict(color='white', width=2),
                layer="below"
            ))
            
            # Format display name - for ordinals, add occurrence indicator
            if is_ordinal and ordinal_num:
                ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(ordinal_num, f"{ordinal_num}th")
                full_name = f"{name} ({ordinal_suffix})"
            else:
                full_name = name
            
            # Wrap long names
            display_name = wrap_text(full_name, 45)
            
            # Calculate delta from overall position (only for cluster columns)
            delta_text = ""
            if not is_overall and cid in overall_position:
                overall_pos = overall_position[cid]
                delta = row_idx - overall_pos  # positive = later in this cluster, negative = earlier
                if delta > 0:
                    delta_text = f" <span style='color:#d62728'>▼{delta}</span>"  # Red down arrow - later
                elif delta < 0:
                    delta_text = f" <span style='color:#1a7a1a'>▲{abs(delta)}</span>"  # Darker green up arrow - earlier
                # delta == 0 means same position, no indicator
            elif not is_overall and cid not in overall_position:
                # Concept not in overall (e.g., filtered out there)
                delta_text = " <span style='color:#999'>●</span>"  # Gray dot - unique to this cluster
            
            # Cell text: concept name, median time, prevalence, delta
            annotations.append(dict(
                x=x_start + cell_width / 2,
                y=(y_top + y_bottom) / 2,
                text=f"<b>{display_name}</b><br><span style='font-size:10px'>{median_time:.0f}d | {prevalence:.0f}%{delta_text}</span>",
                showarrow=False,
                font=dict(size=10, color='black'),
                xref="x", yref="y",
                xanchor="center", yanchor="middle",
                align="center"
            ))
    
    # Draw Overall column (no delta)
    draw_column(0, overall_order, is_overall=True)
    
    # Draw Cluster columns (with delta from overall)
    for cluster in range(optimal_k):
        draw_column(cluster + 1, cluster_orders[cluster], is_overall=False)
    
    # Set axis ranges
    x_max = num_cols * (cell_width + col_gap)
    y_max = max_concepts * cell_height + 50
    
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
    
    # Create combined matrix: Upper triangle = Correlation, Lower triangle = Jaccard
    # Diagonal = prevalence displayed as 1.0
    combined_matrix = np.zeros((n_concepts, n_concepts))
    for i in range(n_concepts):
        for j in range(n_concepts):
            if i == j:
                combined_matrix[i, j] = 1.0  # Diagonal
            elif i < j:
                # Upper triangle: Correlation (scale from -1,1 to 0,1 for display)
                combined_matrix[i, j] = (corr_matrix[i, j] + 1) / 2  # Map -1..1 to 0..1
            else:
                # Lower triangle: Jaccard - map 0..1 to 0.5..1 (white to blue only, no red)
                combined_matrix[i, j] = 0.5 + jaccard_matrix[i, j] / 2
    
    # Create labels for axes (shorter display names)
    labels = [concept_info[cid]['display_name'] for cid in concept_ids]
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
    
    # Custom diverging colorscale: Red (-1/0) -> White (0/0.5) -> Blue (+1/1)
    # Since values are mapped to 0-1 range: 0 = red, 0.5 = white, 1 = blue
    custom_colorscale = [
        [0.0, 'rgb(178, 24, 43)'],    # Dark red
        [0.25, 'rgb(239, 138, 98)'],  # Light red
        [0.5, 'rgb(255, 255, 255)'],  # White
        [0.75, 'rgb(103, 169, 207)'], # Light blue
        [1.0, 'rgb(33, 102, 172)']    # Dark blue
    ]
    
    fig = go.Figure(data=go.Heatmap(
        z=combined_matrix,
        x=labels,
        y=labels,
        colorscale=custom_colorscale,
        zmin=0,
        zmax=1,
        text=hover_text,
        hoverinfo='text',
        showscale=False  # Hide default colorbar since we have mixed scales
    ))
    
    # Add value annotations for cells - show raw values (not scaled)
    # Text color: white on dark (near 0 or 1), black on light (near 0.5)
    def get_text_color(val):
        """Return text color based on background intensity."""
        if val < 0.3 or val > 0.7:
            return 'white'
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
                    x=labels[j],
                    y=labels[i],
                    text=f"{prevalence:.0%}",
                    showarrow=False,
                    font=dict(size=9, color=text_color)
                ))
            elif i < j:
                # Upper triangle - show Correlation
                corr_val = corr_matrix[i, j]
                text_color = get_text_color(display_val)
                annotations.append(dict(
                    x=labels[j],
                    y=labels[i],
                    text=f"{corr_val:.2f}",
                    showarrow=False,
                    font=dict(size=9, color=text_color)
                ))
            else:
                # Lower triangle - show Jaccard
                jaccard_val = jaccard_matrix[i, j]
                text_color = get_text_color(display_val)
                annotations.append(dict(
                    x=labels[j],
                    y=labels[i],
                    text=f"{jaccard_val:.2f}",
                    showarrow=False,
                    font=dict(size=9, color=text_color)
                ))
    
    # Calculate plot size based on number of concepts
    plot_size = max(400, n_concepts * 45 + 150)
    
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
        height=plot_size,
        width=plot_size + 100,
        xaxis=dict(
            tickangle=45,
            tickfont=dict(size=10),
            side="bottom"
        ),
        yaxis=dict(
            tickfont=dict(size=10),
            autorange="reversed"  # Top to bottom
        ),
        margin=dict(l=150, r=50, t=60, b=150),
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
    min_prevalence_pct: int = 0
) -> go.Figure:
    """
    Create a trajectory visualization from pre-computed summary data.
    Shows concepts ordered by their median occurrence time per cluster.
    Matches the visual style of the patient-mode trajectory plot.
    
    Args:
        clustering_results: Dictionary with optimal_cluster_count, cluster_counts, etc.
        clustering_summary_matrix: Pre-computed DataFrame with time_median, prevalence per concept per cluster
        heritage_groups_order: Dictionary mapping heritage to list of items in display order
        active_concept_ids: Set of active concept IDs to include
        min_prevalence_pct: Minimum prevalence percentage (0-100) to include concept
        
    Returns:
        Plotly figure with trajectory visualization (card-based layout)
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
    
    # Get cluster labels from the summary matrix
    cluster_labels = sorted(clustering_summary_matrix['cluster'].unique())
    
    # Build stats from clustering summary matrix
    # Structure: cluster_stats[cluster_label][concept_id] = {'median': ..., 'prevalence': ...}
    cluster_stats = {label: {} for label in cluster_labels}
    
    for _, row in clustering_summary_matrix.iterrows():
        concept_id = str(row.get('CONCEPT_ID', '')).replace('.0', '')
        cluster = row.get('cluster')
        median_time = row.get('time_median')
        prevalence = row.get('prevalence', 0) * 100  # Convert to percentage
        
        if concept_id in concept_info and cluster in cluster_stats:
            if median_time is not None and not pd.isna(median_time):
                cluster_stats[cluster][concept_id] = {
                    'median': median_time,
                    'prevalence': prevalence
                }
    
    # Calculate "Overall" stats by averaging across clusters (weighted by cluster size)
    overall_stats = {}
    for cid in concept_info.keys():
        medians = []
        total_weight = 0
        weighted_prev = 0
        
        for cluster_label in cluster_labels:
            if cid in cluster_stats[cluster_label]:
                stats = cluster_stats[cluster_label][cid]
                # Get cluster count for weighting
                count = cluster_counts.get(cluster_label, 1)
                medians.append((stats['median'], count))
                weighted_prev += stats['prevalence'] * count
                total_weight += count
        
        if medians:
            # Weighted average of median times
            weighted_median = sum(m * w for m, w in medians) / sum(w for _, w in medians)
            avg_prevalence = weighted_prev / total_weight if total_weight > 0 else 0
            overall_stats[cid] = {
                'median': weighted_median,
                'prevalence': avg_prevalence
            }
    
    # Create orderings for each column (sorted by median time, filtered by prevalence)
    def get_ordered_concepts(stats: Dict) -> List[Tuple[str, float, float]]:
        """Return [(concept_id, median_time, prevalence)] sorted by median time."""
        items = [(cid, s['median'], s['prevalence']) 
                 for cid, s in stats.items() 
                 if cid in concept_info and s['prevalence'] >= min_prevalence_pct]
        items.sort(key=lambda x: x[1])  # Sort by median time
        return items
    
    overall_order = get_ordered_concepts(overall_stats)
    cluster_orders = {label: get_ordered_concepts(cluster_stats[label]) for label in cluster_labels}
    
    # Find max concepts in any column for height
    max_concepts = max(
        len(overall_order),
        max(len(cluster_orders[label]) for label in cluster_labels) if cluster_labels else 0
    )
    
    if max_concepts == 0:
        fig = go.Figure()
        fig.update_layout(
            height=200,
            showlegend=False,
            annotations=[dict(
                text=f"No cells with ≥{min_prevalence_pct}% prevalence",
                x=0.5, y=0.5,
                xref="paper", yref="paper",
                showarrow=False,
                font=dict(size=14, color="#666")
            )]
        )
        return fig
    
    # Build overall position map for delta calculation
    overall_position = {}
    for pos, (cid, _, _) in enumerate(overall_order):
        overall_position[cid] = pos
    
    # Layout parameters (same as patient mode)
    cell_height = 72
    cell_width = 308
    col_gap = 20
    num_cols = 1 + len(cluster_labels)
    
    plot_height = max(400, max_concepts * cell_height + 120)
    total_width = num_cols * cell_width + (num_cols - 1) * col_gap + 50
    
    fig = go.Figure()
    annotations = []
    shapes = []
    
    # Add vertical dashed lines between columns
    for col_idx in range(1, num_cols):
        x_line = col_idx * (cell_width + col_gap) - col_gap / 2
        shapes.append(dict(
            type="line",
            x0=x_line, x1=x_line,
            y0=-10, y1=max_concepts * cell_height + 40,
            line=dict(color='#999', width=1, dash='dash'),
            layer="below"
        ))
    
    # Column headers
    column_labels = ["Overall"] + [f"Cluster {i+1}\n(n={cluster_counts.get(label, '?')})" 
                                    for i, label in enumerate(cluster_labels)]
    
    for col_idx, label in enumerate(column_labels):
        x_center = col_idx * (cell_width + col_gap) + cell_width / 2
        annotations.append(dict(
            x=x_center, y=max_concepts * cell_height + 30,
            text=f"<b>{label}</b>",
            showarrow=False,
            font=dict(size=12, color='#2c3e50'),
            xref="x", yref="y",
            xanchor="center"
        ))
    
    # Helper to wrap text
    def wrap_text(text: str, max_chars: int = 50) -> str:
        if len(text) <= max_chars:
            return text
        break_point = text.rfind(' ', 0, max_chars)
        if break_point == -1:
            break_point = max_chars
        return text[:break_point] + '<br>' + text[break_point:].strip()
    
    # Draw cells for each column
    def draw_column(col_idx: int, ordered_concepts: List[Tuple[str, float, float]], is_overall: bool = False):
        x_start = col_idx * (cell_width + col_gap)
        
        for row_idx, (cid, median_time, prevalence) in enumerate(ordered_concepts):
            info = concept_info.get(cid, {})
            name = info.get('name', cid)
            heritage = info.get('heritage', 'unknown')
            is_ordinal = info.get('is_ordinal', False)
            ordinal_num = info.get('ordinal_num')
            color = HERITAGE_COLORS.get(heritage, '#cccccc')
            
            # Y position (top to bottom, earliest at top)
            y_top = max_concepts * cell_height - row_idx * cell_height
            y_bottom = y_top - cell_height + 4
            
            # Draw cell background
            shapes.append(dict(
                type="rect",
                x0=x_start, x1=x_start + cell_width,
                y0=y_bottom, y1=y_top,
                fillcolor=color,
                line=dict(color='white', width=2),
                layer="below"
            ))
            
            # Format display name
            if is_ordinal and ordinal_num:
                ordinal_suffix = {1: "1st", 2: "2nd", 3: "3rd"}.get(ordinal_num, f"{ordinal_num}th")
                full_name = f"{name} ({ordinal_suffix})"
            else:
                full_name = name
            
            display_name = wrap_text(full_name, 45)
            
            # Calculate delta from overall position
            delta_text = ""
            if not is_overall and cid in overall_position:
                overall_pos = overall_position[cid]
                delta = row_idx - overall_pos
                if delta > 0:
                    delta_text = f" <span style='color:#d62728'>▼{delta}</span>"
                elif delta < 0:
                    delta_text = f" <span style='color:#1a7a1a'>▲{abs(delta)}</span>"
            elif not is_overall and cid not in overall_position:
                delta_text = " <span style='color:#999'>●</span>"
            
            # Cell text
            annotations.append(dict(
                x=x_start + cell_width / 2,
                y=(y_top + y_bottom) / 2,
                text=f"<b>{display_name}</b><br><span style='font-size:10px'>{median_time:.0f}d | {prevalence:.0f}%{delta_text}</span>",
                showarrow=False,
                font=dict(size=10, color='black'),
                xref="x", yref="y",
                xanchor="center", yanchor="middle",
                align="center"
            ))
    
    # Draw Overall column
    draw_column(0, overall_order, is_overall=True)
    
    # Draw Cluster columns
    for i, cluster_label in enumerate(cluster_labels):
        draw_column(i + 1, cluster_orders[cluster_label], is_overall=False)
    
    # Set axis ranges
    x_max = num_cols * (cell_width + col_gap)
    y_max = max_concepts * cell_height + 50
    
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
    
    # Create combined matrix: Upper = Correlation, Lower = Jaccard, Diagonal = 1
    combined_matrix = np.zeros((n_concepts, n_concepts))
    for i in range(n_concepts):
        for j in range(n_concepts):
            if i == j:
                combined_matrix[i, j] = 1.0
            elif i < j:
                # Upper triangle: Correlation (scale from -1,1 to 0,1)
                combined_matrix[i, j] = (corr_matrix[i, j] + 1) / 2
            else:
                # Lower triangle: Jaccard - map 0..1 to 0.5..1 (white to blue only, no red)
                combined_matrix[i, j] = 0.5 + jaccard_matrix[i, j] / 2
    
    # Create labels
    labels = [concept_info[cid]['display_name'] for cid in concept_ids]
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
                cooccur = co_occurrence.get((cid_i, cid_j), co_occurrence.get((cid_j, cid_i), 0))
                union = union_counts.get((cid_i, cid_j), union_counts.get((cid_j, cid_i), 0))
                
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
    
    # Custom colorscale
    custom_colorscale = [
        [0.0, 'rgb(178, 24, 43)'],
        [0.25, 'rgb(239, 138, 98)'],
        [0.5, 'rgb(255, 255, 255)'],
        [0.75, 'rgb(103, 169, 207)'],
        [1.0, 'rgb(33, 102, 172)']
    ]
    
    fig = go.Figure(data=go.Heatmap(
        z=combined_matrix,
        x=labels,
        y=labels,
        colorscale=custom_colorscale,
        zmin=0,
        zmax=1,
        text=hover_text,
        hoverinfo='text',
        showscale=False
    ))
    
    # Add value annotations
    def get_text_color(val):
        if val < 0.3 or val > 0.7:
            return 'white'
        return 'black'
    
    annotations = []
    for i in range(n_concepts):
        for j in range(n_concepts):
            display_val = combined_matrix[i, j]
            
            if i == j:
                prevalence = prevalence_diag.get(concept_ids[i], 0)
                text_color = get_text_color(display_val)
                annotations.append(dict(
                    x=labels[j],
                    y=labels[i],
                    text=f"{prevalence:.0%}",
                    showarrow=False,
                    font=dict(size=9, color=text_color)
                ))
            elif i < j:
                corr_val = corr_matrix[i, j]
                text_color = get_text_color(display_val)
                annotations.append(dict(
                    x=labels[j],
                    y=labels[i],
                    text=f"{corr_val:.2f}",
                    showarrow=False,
                    font=dict(size=9, color=text_color)
                ))
            else:
                jaccard_val = jaccard_matrix[i, j]
                text_color = get_text_color(display_val)
                annotations.append(dict(
                    x=labels[j],
                    y=labels[i],
                    text=f"{jaccard_val:.2f}",
                    showarrow=False,
                    font=dict(size=9, color=text_color)
                ))
    
    plot_size = max(400, n_concepts * 45 + 150)
    
    title_text = "Concept Co-occurrence"
    if selected_group != "overall":
        title_text += f" - Cluster {selected_group[1:]}"
    else:
        title_text += " - Overall"
    title_text += "<br><sup>Upper △: Correlation (Phi) | Lower △: Jaccard Index | Diagonal: Prevalence</sup>"
    
    fig.update_layout(
        title=dict(text=title_text, x=0.5, font=dict(size=14)),
        height=plot_size,
        width=plot_size + 100,
        xaxis=dict(
            tickangle=45,
            tickfont=dict(size=10),
            side="bottom"
        ),
        yaxis=dict(
            tickfont=dict(size=10),
            autorange="reversed"
        ),
        margin=dict(l=150, r=50, t=60, b=150),
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
            
            cooccur = co_occurrence.get((cid_i, cid_j), co_occurrence.get((cid_j, cid_i), 0))
            union = union_counts.get((cid_i, cid_j), union_counts.get((cid_j, cid_i), 0))
            
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
