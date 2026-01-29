"""
Prevalence/Enrichment plot visualization.
"""

from typing import Dict, List, Optional
import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots

from config.constants import (
    HERITAGE_ORDER,
    MAX_ENRICHMENT_VALUE,
    MAX_ENRICHMENT_LOG,
)
from utils.helpers import normalize_concept_id as _normalize_concept_id, format_heritage_label


def create_prevalence_plot(
    active_concepts: List[Dict],
    dashboard_data: List[Dict],
    y_labels_order: List[str],
    heritage_groups_order: Dict[str, List[Dict]],
    cluster_prevalence_data: Optional[Dict] = None,
    show_all_background: bool = False
) -> go.Figure:
    """
    Create the prevalence/enrichment plot showing prevalence and enrichment for active concepts.
    This plot is linked to the composite plot by sharing the same y-axis ordering.
    
    Args:
        active_concepts: List of active concept dictionaries from dashboard table (with _show=True)
        dashboard_data: Full dashboard data to get concept names and enrichment data
        y_labels_order: List of y-axis labels in the order they appear in the composite plot
        heritage_groups_order: Dictionary mapping heritage to list of items in display order
        cluster_prevalence_data: Optional dict mapping (heritage, concept_id, ordinal) to cluster prevalence
        show_all_background: If True and cluster_prevalence_data provided, show all as background
        
    Returns:
        Plotly figure with bar charts showing prevalence and enrichment
    """
    if not active_concepts:
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
        return fig
    
    # Create concept lookup maps for efficient access
    concept_map = {}
    ordinal_map = {}  # Map (heritage, original_concept_id, ordinal) -> row
    
    for row in dashboard_data:
        concept_id = row.get("_concept_id") or row.get("CONCEPT_ID")
        if concept_id is not None:
            concept_id_normalized = _normalize_concept_id(concept_id)
            concept_map[concept_id_normalized] = row
            concept_map[str(concept_id)] = row  # Backward compatibility
        
        # Build ordinal lookup map for efficient ordinal searches
        if row.get("IS_ORDINAL", False):
            original_id = row.get("ORIGINAL_CONCEPT_ID")
            ordinal = row.get("ORDINAL", 0)
            heritage = row.get("HERITAGE")
            if original_id is not None and heritage is not None:
                key = (str(heritage), _normalize_concept_id(original_id), ordinal)
                ordinal_map[key] = row
    
    # Prepare plot data by iterating through heritage_groups_order
    # This preserves the exact order and avoids ambiguous matches
    plot_data = []
    default_concept_info = {
        "TARGET_SUBJECT_PREVALENCE": 0,
        "PREVALENCE_DIFFERENCE_RATIO": 1.0
    }
    
    for heritage in HERITAGE_ORDER:
        if heritage not in heritage_groups_order:
            continue
        
        for item in heritage_groups_order[heritage]:
            y_label = item.get("concept_name")
            if not y_label:
                continue
            
            concept_id = item.get("concept_id") or item.get("_concept_id")
            if concept_id is None:
                continue
            
            # Look up concept info
            concept_id_normalized = _normalize_concept_id(concept_id)
            concept_info = concept_map.get(concept_id_normalized)
            
            # For ordinals, try ordinal_map if direct lookup failed
            if not concept_info and item.get("is_ordinal", False):
                original_concept_id = item.get("original_concept_id")
                ordinal = item.get("ordinal", 0)
                item_heritage = item.get("heritage")
                if original_concept_id is not None and item_heritage is not None:
                    ordinal_key = (str(item_heritage), _normalize_concept_id(original_concept_id), ordinal)
                    concept_info = ordinal_map.get(ordinal_key)
            
            concept_info = concept_info or default_concept_info
            
            # Extract and validate prevalence and enrichment
            target_prevalence = concept_info.get("TARGET_SUBJECT_PREVALENCE", 0)
            if target_prevalence is None or pd.isna(target_prevalence):
                target_prevalence = 0
            
            enrichment_ratio = concept_info.get("PREVALENCE_DIFFERENCE_RATIO", 1.0)
            if enrichment_ratio is None or pd.isna(enrichment_ratio) or not np.isfinite(enrichment_ratio):
                enrichment_ratio = 1.0
            enrichment_ratio = min(enrichment_ratio, MAX_ENRICHMENT_VALUE)
            enrichment_log = np.log10(max(enrichment_ratio, 1.0))
            
            # Store plot data
            is_ordinal = item.get("is_ordinal", False)
            item_heritage = item.get("heritage", "unknown")
            ordinal = item.get("ordinal", 0)
            lookup_concept_id = item.get("original_concept_id") if is_ordinal else concept_id
            
            plot_data.append({
                "y_label": y_label,
                "prevalence": target_prevalence,
                "enrichment": enrichment_ratio,
                "enrichment_log": enrichment_log,
                "is_ordinal": is_ordinal,
                "heritage": item_heritage,
                "concept_id": lookup_concept_id,
                "ordinal": ordinal
            })
    
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
        return fig
    
    # Create lookup map from (heritage, concept_id, ordinal) to plot_data item
    plot_data_map = {}
    for item in plot_data:
        heritage_key = item["heritage"] or "unknown"
        concept_id_key = _normalize_concept_id(item["concept_id"]) if item["concept_id"] is not None else None
        ordinal_key = item["ordinal"]
        plot_data_map[(heritage_key, concept_id_key, ordinal_key)] = item
    
    # Get heritages with data
    heritages_with_data = [h for h in HERITAGE_ORDER if h in heritage_groups_order and heritage_groups_order[h]]
    
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
        return fig
    
    # Calculate row heights: 30px per concept per heritage panel
    row_heights = [len(heritage_groups_order[h]) * 30 for h in heritages_with_data]
    
    # Create subplots - one row per heritage with proportional heights
    fig = make_subplots(
        rows=len(heritages_with_data),
        cols=1,
        shared_xaxes=True,
        vertical_spacing=0.05,
        row_heights=row_heights,
        subplot_titles=[format_heritage_label(h) for h in heritages_with_data]
    )
    
    # Prepare colorbar configuration (only for first bar)
    break_values = [0, 5, 10, 50, 100]
    log_tick_positions = [0 if val == 0 else np.log10(val) for val in break_values]
    tick_texts = [str(val) for val in break_values]
    
    # Add bars for each heritage - use heritage_groups_order to preserve exact order
    for row_idx, heritage in enumerate(heritages_with_data, start=1):
        items_from_order = heritage_groups_order[heritage]
        
        for y_idx, item_from_order in enumerate(items_from_order):
            # Build lookup key using (heritage, concept_id, ordinal) for unambiguous mapping
            heritage_key = heritage or "unknown"
            is_ordinal_item = item_from_order.get("is_ordinal", False)
            
            if is_ordinal_item:
                concept_id_key = _normalize_concept_id(item_from_order.get("original_concept_id"))
            else:
                concept_id_key = _normalize_concept_id(
                    item_from_order.get("concept_id") or item_from_order.get("_concept_id")
                )
            
            lookup_key = (heritage_key, concept_id_key, item_from_order.get("ordinal", 0))
            plot_item = plot_data_map.get(lookup_key)
            
            if not plot_item:
                continue  # Skip if not found (shouldn't happen, but handle gracefully)
            
            # Extract plot data
            prevalence = plot_item["prevalence"]
            enrichment = plot_item["enrichment"]
            enrichment_log = plot_item["enrichment_log"]
            is_ordinal = plot_item["is_ordinal"]
            
            # Configure visual properties
            base_alpha = 0.4 if is_ordinal else 1.0
            border_color = "#666666" if is_ordinal else "rgba(0,0,0,0)"
            enrichment_log_for_color = min(max(enrichment_log, 0), MAX_ENRICHMENT_LOG)
            
            # Determine if we're showing cluster overlay
            if show_all_background and cluster_prevalence_data:
                bg_alpha = base_alpha * 0.35  # Reduced for background
                fg_alpha = base_alpha * 0.9   # Nearly full for cluster
            else:
                bg_alpha = base_alpha
                fg_alpha = None
            
            # Add "All" bar trace (background when cluster selected, only bar otherwise)
            fig.add_trace(
                go.Bar(
                    x=[prevalence],
                    y=[y_idx],
                    orientation='h',
                    width=0.8,  # Full width
                    marker=dict(
                        color=[enrichment_log_for_color],
                        colorscale='Viridis',
                        cmin=0,
                        cmax=MAX_ENRICHMENT_LOG,
                        colorbar=None,
                        line=dict(color=border_color, width=0.8 if is_ordinal else 0),
                        opacity=bg_alpha
                    ),
                    showlegend=False,
                    text=[f"{enrichment:.2f}" if enrichment < MAX_ENRICHMENT_VALUE else "Inf"] if not show_all_background else None,
                    textposition="outside" if not show_all_background else None,
                    textfont=dict(size=10, color="black") if not show_all_background else None,
                    hovertemplate=f"All - Prevalence: {prevalence:.2%}<br>Enrichment: {enrichment:.2f}<extra></extra>"
                ),
                row=row_idx,
                col=1
            )
            
            # Add cluster overlay circle if we have cluster data
            if show_all_background and cluster_prevalence_data:
                # Get cluster prevalence for this concept
                cluster_data = cluster_prevalence_data.get(lookup_key, {})
                
                # Debug: if not found, try alternative keys
                if not cluster_data:
                    # Try with concept_id directly (not original_concept_id)
                    direct_concept_id = _normalize_concept_id(
                        item_from_order.get("concept_id") or item_from_order.get("_concept_id")
                    )
                    alt_key = (heritage_key, direct_concept_id, item_from_order.get("ordinal", 0))
                    cluster_data = cluster_prevalence_data.get(alt_key, {})
                    
                cluster_prevalence = cluster_data.get("prevalence", 0)
                cluster_enrichment = cluster_data.get("enrichment", 1.0)
                
                # Skip drawing circle if prevalence is 0
                if cluster_prevalence <= 0:
                    continue
                
                if cluster_enrichment is None or pd.isna(cluster_enrichment) or not np.isfinite(cluster_enrichment):
                    cluster_enrichment = 1.0
                cluster_enrichment = min(cluster_enrichment, MAX_ENRICHMENT_VALUE)
                cluster_enrichment_log = np.log10(max(cluster_enrichment, 1.0))
                cluster_enrichment_log_for_color = min(max(cluster_enrichment_log, 0), MAX_ENRICHMENT_LOG)
                
                # Compute color from Viridis colorscale - same as bars
                # Normalize to 0-1 range based on log enrichment
                color_norm = cluster_enrichment_log_for_color / MAX_ENRICHMENT_LOG if MAX_ENRICHMENT_LOG > 0 else 0
                color_norm = min(max(color_norm, 0), 1)
                
                # Use plotly.colors to sample from Viridis
                import plotly.colors as pc
                cluster_marker_color = pc.sample_colorscale('Viridis', [color_norm])[0]
                
                # Add circle marker at cluster prevalence position
                # Label position: left if prevalence >= 50%, right if < 50%
                label_position = "middle left" if cluster_prevalence >= 0.5 else "middle right"
                
                fig.add_trace(
                    go.Scatter(
                        x=[cluster_prevalence],
                        y=[y_idx],
                        mode='markers+text',
                        marker=dict(
                            size=14,
                            color=cluster_marker_color,
                            line=dict(color='#E74C3C', width=2),  # Red border
                            symbol='circle'
                        ),
                        text=[f"{cluster_enrichment:.2f}"],
                        textposition=label_position,
                        textfont=dict(size=9, color="#E74C3C"),  # Red text for cluster
                        showlegend=False,
                        hovertemplate=f"Cluster - Prevalence: {cluster_prevalence:.2%}<br>Enrichment: {cluster_enrichment:.2f}<extra></extra>"
                    ),
                    row=row_idx,
                    col=1
                )
            
    # Calculate total plot height
    plot_height = sum(row_heights) + (len(heritages_with_data) - 1) * 10 + 160
    
    # Update layout (colorbar is created separately in combined figure)
    fig.update_layout(
        title="Prevalence/Enrichment",
        height=plot_height,
        showlegend=False,
        plot_bgcolor="white",
        paper_bgcolor="white",
        margin=dict(l=50, r=50, t=80, b=25),  # Standard bottom margin
        dragmode=False,
        hovermode=False
    )
    
    # Update x-axes (shared, show percentage)
    for row_idx in range(1, len(heritages_with_data) + 1):
        fig.update_xaxes(
            title_text="Prevalence" if row_idx == len(heritages_with_data) else "",
            tickformat=".0%",
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


def create_enrichment_colorbar_figure() -> go.Figure:
    """
    Create a standalone figure showing only the enrichment colorbar legend.
    
    Returns:
        Plotly figure with colorbar showing enrichment scale
    """
    import plotly.colors as pc
    
    MAX_ENRICHMENT_VALUE = 1000.0
    MAX_ENRICHMENT_LOG = np.log10(MAX_ENRICHMENT_VALUE) if MAX_ENRICHMENT_VALUE > 0 else 0
    
    # Create a minimal figure
    fig = go.Figure()
    
    # Colorbar dimensions
    colorbar_width = 0.8  # 80% of figure width
    colorbar_height = 0.3  # 30% of figure height
    colorbar_x_center = 0.5
    colorbar_x_start = colorbar_x_center - colorbar_width / 2
    colorbar_y_center = 0.5
    
    # Get Viridis colors
    num_segments = 100
    viridis_colors = pc.sample_colorscale('Viridis', [i/num_segments for i in range(num_segments+1)])
    
    # Create gradient rectangles
    shapes = []
    for i in range(num_segments):
        x_start = colorbar_x_start + (i / num_segments) * colorbar_width
        x_end = colorbar_x_start + ((i + 1) / num_segments) * colorbar_width
        shapes.append(dict(
            type="rect",
            xref="paper",
            yref="paper",
            x0=x_start,
            y0=colorbar_y_center - colorbar_height / 2,
            x1=x_end,
            y1=colorbar_y_center + colorbar_height / 2,
            fillcolor=viridis_colors[i],
            line=dict(width=0),
            layer="above"
        ))
    
    # Add tick marks and labels
    break_values = [0, 5, 10, 50, 100]
    log_tick_positions = [0 if val == 0 else np.log10(val) for val in break_values]
    tick_texts = [str(val) for val in break_values]
    
    annotations = []
    for tick_val, tick_text in zip(log_tick_positions, tick_texts):
        # Calculate x position for this tick
        norm_tick = tick_val / MAX_ENRICHMENT_LOG if MAX_ENRICHMENT_LOG > 0 else 0
        tick_x = colorbar_x_start + norm_tick * colorbar_width
        
        # Add tick mark (small vertical line below colorbar)
        tick_height = 0.05
        shapes.append(dict(
            type="line",
            xref="paper",
            yref="paper",
            x0=tick_x,
            y0=colorbar_y_center - colorbar_height / 2,
            x1=tick_x,
            y1=colorbar_y_center - colorbar_height / 2 - tick_height,
            line=dict(color="black", width=1),
            layer="above"
        ))
        
        # Add tick label
        label_y = colorbar_y_center - colorbar_height / 2 - tick_height - 0.05
        annotations.append(dict(
            x=tick_x,
            y=label_y,
            xref="paper",
            yref="paper",
            text=tick_text,
            showarrow=False,
            font=dict(size=10, color="black"),
            xanchor="center",
            yanchor="top"
        ))
    
    # Add colorbar title
    title_y = colorbar_y_center + colorbar_height / 2 + 0.1
    annotations.append(dict(
        x=colorbar_x_center,
        y=title_y,
        xref="paper",
        yref="paper",
        text="Enrichment",
        showarrow=False,
        font=dict(size=12, color="black"),
        xanchor="center",
        yanchor="bottom"
    ))
    
    # Add significance legend (blue = significant, gray = not significant)
    # Blue circle (filled) for significant
    shapes.append(dict(
        type="circle",
        xref="paper",
        yref="paper",
        x0=0.05,
        y0=0.15,
        x1=0.12,
        y1=0.22,
        fillcolor="#2E86AB",
        line=dict(width=1, color="#2E86AB"),
        layer="above"
    ))
    annotations.append(dict(
        x=0.13,
        y=0.185,
        xref="paper",
        yref="paper",
        text="Significant",
        showarrow=False,
        font=dict(size=10, color="black"),
        xanchor="left",
        yanchor="middle"
    ))
    
    # Gray circle (filled) for not significant
    shapes.append(dict(
        type="circle",
        xref="paper",
        yref="paper",
        x0=0.25,
        y0=0.15,
        x1=0.32,
        y1=0.22,
        fillcolor="#666666",
        line=dict(width=1, color="#666666"),
        layer="above"
    ))
    annotations.append(dict(
        x=0.33,
        y=0.185,
        xref="paper",
        yref="paper",
        text="Not significant",
        showarrow=False,
        font=dict(size=10, color="black"),
        xanchor="left",
        yanchor="middle"
    ))
    
    # Hollow square for ordinals
    shapes.append(dict(
        type="rect",
        xref="paper",
        yref="paper",
        x0=0.50,
        y0=0.15,
        x1=0.57,
        y1=0.22,
        fillcolor="white",
        line=dict(width=2, color="#2E86AB"),
        layer="above"
    ))
    annotations.append(dict(
        x=0.58,
        y=0.185,
        xref="paper",
        yref="paper",
        text="Ordinal",
        showarrow=False,
        font=dict(size=10, color="black"),
        xanchor="left",
        yanchor="middle"
    ))
    
    # Update layout
    fig.update_layout(
        height=100,  # Increased height to accommodate significance legend
        showlegend=False,
        plot_bgcolor="white",
        paper_bgcolor="white",
        margin=dict(l=50, r=50, t=40, b=20),
        dragmode=False,
        hovermode=False,
        xaxis=dict(visible=False, range=[0, 1]),
        yaxis=dict(visible=False, range=[0, 1]),
        shapes=shapes,
        annotations=annotations
    )
    
    return fig


