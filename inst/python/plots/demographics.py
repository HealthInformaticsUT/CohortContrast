"""
Demographics plots - Age and Sex distributions.
Shows overall and per-cluster comparisons.
"""

from typing import Dict, List, Optional, Tuple, Set
import json
import logging
import numpy as np
import pandas as pd
import plotly.graph_objects as go
from plotly.subplots import make_subplots

logger = logging.getLogger('ContrastViewer.demographics')

# Cluster colors (matching clustering_plots.py)
CLUSTER_COLORS = {
    "C1": "#1f77b4",  # Blue
    "C2": "#ff7f0e",  # Orange
    "C3": "#2ca02c",  # Green
    "C4": "#d62728",  # Red
    "C5": "#9467bd",  # Purple
    "C6": "#8c564b",  # Brown
    "C7": "#e377c2",  # Pink
    "C8": "#7f7f7f",  # Gray
    "Overall": "#2c3e50"  # Dark blue-gray for overall
}


def _calculate_patient_demographics(
    data_person: pd.DataFrame,
    data_initial: pd.DataFrame,
    data_patients: pd.DataFrame,
    patient_ids: Optional[Set] = None
) -> Dict:
    """
    Calculate demographic statistics for a set of patients.
    
    Returns dict with age_values, sex_counts.
    """
    result = {
        "age_values": [],
        "male_count": 0,
        "female_count": 0,
        "other_count": 0,
        "total": 0
    }
    
    # Get target patient data
    target_person_ids = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"]["PERSON_ID"].unique()
    
    # Filter to specific patients if provided
    if patient_ids is not None:
        target_person_ids = [pid for pid in target_person_ids if pid in patient_ids]
    
    if len(target_person_ids) == 0:
        return result
    
    target_persons = data_person[data_person["PERSON_ID"].isin(target_person_ids)].copy()
    
    # Get target cohort start dates
    target_initial = data_initial[data_initial["COHORT_DEFINITION_ID"] == "target"].copy()
    target_initial["COHORT_START_DATE"] = pd.to_datetime(target_initial["COHORT_START_DATE"])
    
    # Merge to get cohort dates for each patient
    person_with_dates = target_persons.merge(
        target_initial[["SUBJECT_ID", "COHORT_START_DATE"]],
        left_on="PERSON_ID",
        right_on="SUBJECT_ID",
        how="inner"
    )
    
    result["total"] = len(person_with_dates)
    
    # Calculate ages
    if "YEAR_OF_BIRTH" in person_with_dates.columns and len(person_with_dates) > 0:
        person_with_dates["AGE"] = person_with_dates["COHORT_START_DATE"].dt.year - person_with_dates["YEAR_OF_BIRTH"]
        valid_ages = person_with_dates[(person_with_dates["AGE"] >= 0) & (person_with_dates["AGE"] <= 150)]["AGE"]
        result["age_values"] = valid_ages.dropna().tolist()
    
    # Sex distribution
    if "GENDER_CONCEPT_ID" in person_with_dates.columns:
        result["male_count"] = int((person_with_dates["GENDER_CONCEPT_ID"] == 8507).sum())
        result["female_count"] = int((person_with_dates["GENDER_CONCEPT_ID"] == 8532).sum())
        result["other_count"] = int(len(person_with_dates) - result["male_count"] - result["female_count"])
    
    return result


def create_demographics_plots_from_patient_data(
    data_person: pd.DataFrame,
    data_initial: pd.DataFrame,
    data_patients: pd.DataFrame,
    clustering_results: Optional[Dict] = None
) -> go.Figure:
    """
    Create demographics plots from patient-level data.
    Shows overall and per-cluster comparisons.
    """
    # Get overall demographics
    overall_demo = _calculate_patient_demographics(data_person, data_initial, data_patients)
    
    if overall_demo["total"] == 0:
        return create_empty_demographics_plot()
    
    # Get cluster assignments and compute per-cluster demographics
    cluster_demos = {}
    cluster_labels = ["Overall"]
    
    if clustering_results:
        patient_assignments = clustering_results.get('patient_assignments', [])
        if patient_assignments:
            if isinstance(patient_assignments, list):
                df_assignments = pd.DataFrame(patient_assignments)
            else:
                df_assignments = patient_assignments
            
            # Check for patient_id column (lowercase, used by clustering)
            patient_id_col = None
            if "patient_id" in df_assignments.columns:
                patient_id_col = "patient_id"
            elif "PERSON_ID" in df_assignments.columns:
                patient_id_col = "PERSON_ID"
            
            if not df_assignments.empty and "cluster" in df_assignments.columns and patient_id_col:
                clusters = sorted(df_assignments["cluster"].dropna().unique())
                
                for cluster in clusters:
                    cluster_patient_ids = set(df_assignments[df_assignments["cluster"] == cluster][patient_id_col])
                    if cluster_patient_ids:
                        cluster_demos[cluster] = _calculate_patient_demographics(
                            data_person, data_initial, data_patients, cluster_patient_ids
                        )
                        cluster_labels.append(cluster)
    
    # Create figure with 2 subplots (Age and Sex)
    fig = make_subplots(
        rows=1, cols=2,
        subplot_titles=(
            "<b>Age Distribution</b>",
            "<b>Sex Distribution</b>"
        ),
        horizontal_spacing=0.12,
        column_widths=[0.5, 0.5]
    )
    
    # === 1. Age Distribution (Box plots for cluster comparison) ===
    if overall_demo["age_values"]:
        # Add overall box plot (visible by default)
        fig.add_trace(
            go.Box(
                y=overall_demo["age_values"],
                name="Overall",
                marker_color=CLUSTER_COLORS["Overall"],
                boxmean='sd',
                showlegend=True,
                legendgroup="Overall",
                visible=True
            ),
            row=1, col=1
        )
        
        # Add cluster box plots (only C1 visible by default)
        for cluster in sorted(cluster_demos.keys()):
            if cluster_demos[cluster]["age_values"]:
                # Only Overall and C1 visible by default
                is_visible = True if cluster == "C1" else "legendonly"
                fig.add_trace(
                    go.Box(
                        y=cluster_demos[cluster]["age_values"],
                        name=cluster,
                        marker_color=CLUSTER_COLORS.get(cluster, "#888"),
                        boxmean='sd',
                        showlegend=True,
                        legendgroup=cluster,
                        visible=is_visible
                    ),
                    row=1, col=1
                )
    
    # === 2. Sex Distribution (Grouped bar chart) ===
    categories = ["Male", "Female", "Other"]
    
    # Overall
    overall_sex = [overall_demo["male_count"], overall_demo["female_count"], overall_demo["other_count"]]
    overall_pct = [round(100 * v / overall_demo["total"], 2) if overall_demo["total"] > 0 else 0 for v in overall_sex]
    
    fig.add_trace(
        go.Bar(
            x=categories,
            y=overall_pct,
            name="Overall",
            marker_color=CLUSTER_COLORS["Overall"],
            text=[f"{p:.2f}%<br>(n={n})" for p, n in zip(overall_pct, overall_sex)],
            textposition="auto",
            showlegend=False,
            legendgroup="Overall",
            hovertemplate="Overall<br>%{x}: %{y:.2f}%<extra></extra>"
        ),
        row=1, col=2
    )
    
    # Cluster sex distributions (overlay as points)
    for cluster in sorted(cluster_demos.keys()):
        demo = cluster_demos[cluster]
        if demo["total"] > 0:
            cluster_sex = [demo["male_count"], demo["female_count"], demo["other_count"]]
            cluster_pct = [round(100 * v / demo["total"], 2) for v in cluster_sex]
            
            # Only C1 visible by default
            is_visible = True if cluster == "C1" else "legendonly"
            
            # Add as scatter points on top of bars for comparison
            fig.add_trace(
                go.Scatter(
                    x=categories,
                    y=cluster_pct,
                    mode="markers+text",
                    name=cluster,
                    marker=dict(
                        color=CLUSTER_COLORS.get(cluster, "#888"),
                        size=12,
                        symbol="diamond",
                        line=dict(width=2, color="white")
                    ),
                    text=[f"{p:.2f}%" for p in cluster_pct],
                    textposition="top center",
                    textfont=dict(size=10, color=CLUSTER_COLORS.get(cluster, "#888")),
                    showlegend=False,
                    legendgroup=cluster,
                    visible=is_visible,
                    hovertemplate=f"{cluster}<br>%{{x}}: %{{y:.2f}}%<extra></extra>"
                ),
                row=1, col=2
            )
    
    # Update layout
    fig.update_layout(
        height=450,
        showlegend=True,
        legend=dict(
            orientation="h",
            yanchor="bottom",
            y=1.08,
            xanchor="center",
            x=0.5,
            bgcolor="rgba(255,255,255,0.8)"
        ),
        barmode="overlay",
        plot_bgcolor="white",
        paper_bgcolor="white",
        margin=dict(t=100, b=60, l=60, r=40),
        font=dict(family="Arial, sans-serif")
    )
    
    # Update axes
    fig.update_yaxes(title_text="Age (years)", row=1, col=1, gridcolor="#eee")
    fig.update_yaxes(title_text="Percentage (%)", row=1, col=2, gridcolor="#eee", range=[0, 105])
    
    fig.update_xaxes(title_text="", row=1, col=1, showgrid=False)
    fig.update_xaxes(title_text="", row=1, col=2, showgrid=False)
    
    # Add summary statistics as annotation
    if overall_demo["age_values"]:
        ages = np.array(overall_demo["age_values"])
        age_summary = f"Overall: Mean={np.mean(ages):.1f}, Median={np.median(ages):.1f}, N={len(ages)}"
        
        # Add cluster summaries
        for cluster in sorted(cluster_demos.keys()):
            if cluster_demos[cluster]["age_values"]:
                c_ages = np.array(cluster_demos[cluster]["age_values"])
                age_summary += f" | {cluster}: Mean={np.mean(c_ages):.1f}, N={len(c_ages)}"
        
        fig.add_annotation(
            x=0.5, y=-0.12,
            xref="paper", yref="paper",
            text=age_summary,
            showarrow=False,
            font=dict(size=10, color="#666"),
            bgcolor="rgba(255,255,255,0.9)"
        )
    
    # Add note if no clusters available
    if not cluster_demos:
        has_clustering = clustering_results is not None
        has_assignments = False
        if has_clustering:
            pa = clustering_results.get('patient_assignments', [])
            has_assignments = len(pa) > 0 if pa else False
        
        note = "Cluster comparison available after clustering completes (click 'Re-cluster' on Dashboard)"
        if has_clustering and not has_assignments:
            note = "Clustering complete but patient assignments not available (summary mode)"
        
        fig.add_annotation(
            x=0.5, y=1.12,
            xref="paper", yref="paper",
            text=note,
            showarrow=False,
            font=dict(size=10, color="#888", style="italic"),
            bgcolor="rgba(255,255,255,0.8)"
        )
    
    return fig


def create_demographics_plots_from_summary(
    metadata: Dict,
    demographics_data: Optional[pd.DataFrame] = None,
    clustering_results: Optional[Dict] = None
) -> go.Figure:
    """
    Create demographics plots from pre-computed summary data.
    Shows overall and per-cluster comparisons when available.
    Uses box plots similar to patient mode.
    """
    demo = metadata.get("demographics", {})
    distributions = demo.get("distributions", {})
    overall = distributions.get("overall", {})
    clusters = distributions.get("clusters", {})
    
    # Create figure with 2 subplots (Age and Sex)
    fig = make_subplots(
        rows=1, cols=2,
        subplot_titles=(
            "<b>Age Distribution</b>",
            "<b>Sex Distribution</b>"
        ),
        horizontal_spacing=0.12,
        column_widths=[0.5, 0.5]
    )
    
    # Track if we have any data to show
    has_data = False
    
    # === 1. Age Distribution (Box plots from summary stats) ===
    age_hist = overall.get("age_histogram", {})
    
    # Check if we have quartile data for box plots
    has_quartiles = age_hist.get("q1") is not None and age_hist.get("q3") is not None
    
    if has_quartiles:
        has_data = True
        
        # Create box plot from summary statistics for Overall
        # Use x="Overall" to position boxes side by side
        fig.add_trace(
            go.Box(
                x=["Overall"],
                name="Overall",
                q1=[age_hist["q1"]],
                median=[age_hist["median"]],
                q3=[age_hist["q3"]],
                lowerfence=[age_hist.get("whisker_low", age_hist.get("min", age_hist["q1"]))],
                upperfence=[age_hist.get("whisker_high", age_hist.get("max", age_hist["q3"]))],
                mean=[age_hist["mean"]],
                sd=[age_hist.get("std", 0)],
                marker_color=CLUSTER_COLORS["Overall"],
                boxmean='sd',
                showlegend=True,
                legendgroup="Overall",
                visible=True,
                hovertemplate=(
                    f"<b>Overall</b><br>" +
                    f"Mean: {age_hist['mean']:.1f}<br>" +
                    f"Median: {age_hist['median']:.1f}<br>" +
                    f"Q1-Q3: {age_hist['q1']:.1f}-{age_hist['q3']:.1f}<br>" +
                    f"N: {age_hist.get('n', 'N/A')}<extra></extra>"
                )
            ),
            row=1, col=1
        )
        
        # Add cluster box plots
        # Use overall std for estimating cluster quartiles if not available
        overall_std = age_hist.get("std", 10)
        
        for cluster_name, cluster_data in sorted(clusters.items()):
            if cluster_data.get("age_mean") is not None:
                is_visible = True if cluster_name == "C1" else "legendonly"
                
                c_mean = cluster_data["age_mean"]
                
                # Use actual quartiles if available, otherwise estimate from mean
                if cluster_data.get("age_q1") is not None and cluster_data.get("age_q3") is not None:
                    c_q1 = cluster_data["age_q1"]
                    c_q3 = cluster_data["age_q3"]
                    c_median = cluster_data.get("age_median", c_mean)
                    c_whisker_low = cluster_data.get("whisker_low", cluster_data.get("age_min", c_q1))
                    c_whisker_high = cluster_data.get("whisker_high", cluster_data.get("age_max", c_q3))
                    c_std = cluster_data.get("age_std", overall_std * 0.8)
                else:
                    # Estimate quartiles from mean using overall distribution shape
                    # Use overall IQR ratio to estimate cluster quartiles
                    overall_iqr = age_hist["q3"] - age_hist["q1"]
                    overall_range = age_hist.get("max", 100) - age_hist.get("min", 0)
                    
                    # Scale by cluster patient count relative to overall
                    c_std = overall_std * 0.9  # Slightly smaller std for clusters
                    c_q1 = c_mean - 0.675 * c_std  # Approximate for normal distribution
                    c_q3 = c_mean + 0.675 * c_std
                    c_median = c_mean  # Use mean as median estimate
                    c_whisker_low = max(age_hist.get("min", 0), c_mean - 2 * c_std)
                    c_whisker_high = min(age_hist.get("max", 100), c_mean + 2 * c_std)
                
                fig.add_trace(
                    go.Box(
                        x=[cluster_name],
                        name=cluster_name,
                        q1=[c_q1],
                        median=[c_median],
                        q3=[c_q3],
                        lowerfence=[c_whisker_low],
                        upperfence=[c_whisker_high],
                        mean=[c_mean],
                        sd=[c_std],
                        marker_color=CLUSTER_COLORS.get(cluster_name, "#888"),
                        boxmean='sd',
                        showlegend=True,
                        legendgroup=cluster_name,
                        visible=is_visible,
                        hovertemplate=(
                            f"<b>{cluster_name}</b><br>" +
                            f"Mean: {c_mean:.1f}<br>" +
                            f"N: {cluster_data.get('n', cluster_data.get('patient_count', 'N/A'))}<extra></extra>"
                        )
                    ),
                    row=1, col=1
                )
    elif age_hist and age_hist.get("mean") is not None:
        # Fallback: create simple box-like visualization from mean/std
        has_data = True
        mean_age = age_hist.get("mean")
        std_age = age_hist.get("std", 0)
        median_age = age_hist.get("median", mean_age)
        min_age = age_hist.get("min", mean_age - 2*std_age)
        max_age = age_hist.get("max", mean_age + 2*std_age)
        
        # Estimate quartiles if not available
        q1 = mean_age - 0.675 * std_age  # Approximate for normal distribution
        q3 = mean_age + 0.675 * std_age
        
        fig.add_trace(
            go.Box(
                x=["Overall"],
                name="Overall",
                q1=[q1],
                median=[median_age],
                q3=[q3],
                lowerfence=[min_age],
                upperfence=[max_age],
                mean=[mean_age],
                sd=[std_age],
                marker_color=CLUSTER_COLORS["Overall"],
                boxmean='sd',
                showlegend=True,
                legendgroup="Overall"
            ),
            row=1, col=1
        )
        
        # Add clusters if they have mean data
        for cluster_name, cluster_data in sorted(clusters.items()):
            if cluster_data.get("age_mean") is not None:
                is_visible = True if cluster_name == "C1" else "legendonly"
                c_mean = cluster_data["age_mean"]
                c_std = cluster_data.get("age_std", std_age * 0.8)  # Fallback estimate
                
                fig.add_trace(
                    go.Box(
                        x=[cluster_name],
                        name=cluster_name,
                        q1=[c_mean - 0.675 * c_std],
                        median=[cluster_data.get("age_median", c_mean)],
                        q3=[c_mean + 0.675 * c_std],
                        lowerfence=[cluster_data.get("age_min", c_mean - 2*c_std)],
                        upperfence=[cluster_data.get("age_max", c_mean + 2*c_std)],
                        mean=[c_mean],
                        marker_color=CLUSTER_COLORS.get(cluster_name, "#888"),
                        boxmean='sd',
                        showlegend=True,
                        legendgroup=cluster_name,
                        visible=is_visible
                    ),
                    row=1, col=1
                )
    else:
        # Ultimate fallback: use basic mean from metadata
        age_mean = demo.get("age_mean")
        if age_mean is not None:
            has_data = True
            age_std = demo.get("age_std", 10)
            age_median = demo.get("age_median", age_mean)
            
            fig.add_trace(
                go.Box(
                    x=["Overall"],
                    name="Overall",
                    q1=[age_mean - 0.675 * age_std],
                    median=[age_median],
                    q3=[age_mean + 0.675 * age_std],
                    lowerfence=[age_mean - 2*age_std],
                    upperfence=[age_mean + 2*age_std],
                    mean=[age_mean],
                    sd=[age_std],
                    marker_color=CLUSTER_COLORS["Overall"],
                    boxmean='sd',
                    showlegend=True,
                    legendgroup="Overall"
                ),
                row=1, col=1
            )
    
    # === 2. Sex Distribution ===
    sex_dist = overall.get("sex_distribution", {})
    if sex_dist:
        has_data = True
        male = sex_dist.get("male", 0)
        female = sex_dist.get("female", 0)
        other = sex_dist.get("other", 0)
        total = sex_dist.get("total", male + female + other)
        
        male_pct = round(100 * male / total, 2) if total > 0 else 0
        female_pct = round(100 * female / total, 2) if total > 0 else 0
        other_pct = round(100 * other / total, 2) if total > 0 else 0
        
        fig.add_trace(
            go.Bar(
                x=["Male", "Female", "Other"],
                y=[male_pct, female_pct, other_pct],
                name="Overall",
                marker_color=CLUSTER_COLORS["Overall"],
                text=[f"{male_pct:.2f}%<br>(n={male})", 
                      f"{female_pct:.2f}%<br>(n={female})", 
                      f"{other_pct:.2f}%<br>(n={other})"],
                textposition="auto",
                showlegend=False,
                legendgroup="Overall",
                hovertemplate="Overall<br>%{x}: %{y:.2f}%<extra></extra>"
            ),
            row=1, col=2
        )
        
        # Add cluster comparisons as points (only C1 visible by default)
        for cluster_name, cluster_data in sorted(clusters.items()):
            male_prop = cluster_data.get("male_proportion")
            if male_prop is not None:
                is_visible = True if cluster_name == "C1" else "legendonly"
                male_pct_cluster = round(male_prop * 100, 2)
                female_pct_cluster = round((1 - male_prop) * 100, 2)
                fig.add_trace(
                    go.Scatter(
                        x=["Male", "Female"],
                        y=[male_pct_cluster, female_pct_cluster],
                        mode="markers+text",
                        name=cluster_name,
                        marker=dict(
                            color=CLUSTER_COLORS.get(cluster_name, "#888"),
                            size=12,
                            symbol="diamond",
                            line=dict(width=2, color="white")
                        ),
                        text=[f"{male_pct_cluster:.2f}%", f"{female_pct_cluster:.2f}%"],
                        textposition="top center",
                        textfont=dict(size=10, color=CLUSTER_COLORS.get(cluster_name, "#888")),
                        showlegend=False,
                        legendgroup=cluster_name,
                        visible=is_visible,
                        hovertemplate=f"{cluster_name}<br>%{{x}}: %{{y:.2f}}%<extra></extra>"
                    ),
                    row=1, col=2
                )
    else:
        # Fallback: use overall male_proportion
        male_prop = demo.get("male_proportion")
        target_patients = demo.get("target_patients", 0)
        
        if male_prop is not None and target_patients > 0:
            has_data = True
            male_count = int(male_prop * target_patients)
            female_count = target_patients - male_count
            male_pct = round(male_prop * 100, 2)
            female_pct = round((1 - male_prop) * 100, 2)
            
            fig.add_trace(
                go.Bar(
                    x=["Male", "Female"],
                    y=[male_pct, female_pct],
                    name="Overall",
                    marker_color=CLUSTER_COLORS["Overall"],
                    text=[f"{male_pct:.2f}%<br>(n≈{male_count})", 
                          f"{female_pct:.2f}%<br>(n≈{female_count})"],
                    textposition="auto",
                    showlegend=False,
                    legendgroup="Overall",
                    hovertemplate="Overall<br>%{x}: %{y:.2f}%<extra></extra>"
                ),
                row=1, col=2
            )
            
            # Add cluster points (only C1 visible by default)
            for cluster_name, cluster_data in sorted(clusters.items()):
                c_male_prop = cluster_data.get("male_proportion")
                if c_male_prop is not None:
                    is_visible = True if cluster_name == "C1" else "legendonly"
                    c_male_pct = round(c_male_prop * 100, 2)
                    c_female_pct = round((1 - c_male_prop) * 100, 2)
                    fig.add_trace(
                        go.Scatter(
                            x=["Male", "Female"],
                            y=[c_male_pct, c_female_pct],
                            mode="markers+text",
                            name=cluster_name,
                            marker=dict(
                                color=CLUSTER_COLORS.get(cluster_name, "#888"),
                                size=12,
                                symbol="diamond",
                                line=dict(width=2, color="white")
                            ),
                            text=[f"{c_male_pct:.2f}%", f"{c_female_pct:.2f}%"],
                            textposition="top center",
                            textfont=dict(size=10, color=CLUSTER_COLORS.get(cluster_name, "#888")),
                            showlegend=False,
                            legendgroup=cluster_name,
                            visible=is_visible,
                            hovertemplate=f"{cluster_name}<br>%{{x}}: %{{y:.2f}}%<extra></extra>"
                        ),
                        row=1, col=2
                    )
    
    if not has_data:
        return create_empty_demographics_plot()
    
    # Update layout
    fig.update_layout(
        height=450,
        showlegend=True,
        legend=dict(
            orientation="h",
            yanchor="bottom",
            y=1.08,
            xanchor="center",
            x=0.5,
            bgcolor="rgba(255,255,255,0.8)"
        ),
        barmode="group",
        plot_bgcolor="white",
        paper_bgcolor="white",
        margin=dict(t=100, b=80, l=60, r=40),
        font=dict(family="Arial, sans-serif")
    )
    
    # Update axes
    fig.update_yaxes(title_text="Age (years)", row=1, col=1, gridcolor="#eee")
    fig.update_yaxes(title_text="Percentage (%)", row=1, col=2, gridcolor="#eee", range=[0, 105])
    
    fig.update_xaxes(title_text="", row=1, col=1, showgrid=False)
    fig.update_xaxes(title_text="", row=1, col=2, showgrid=False)
    
    return fig


def create_empty_demographics_plot() -> go.Figure:
    """Create an empty demographics plot with a message."""
    fig = go.Figure()
    fig.add_annotation(
        x=0.5, y=0.5,
        xref="paper", yref="paper",
        text="Demographics data not available.<br><br>" +
             "For patient-level data: Requires data_person.parquet<br>" +
             "For summary data: Re-run precompute with updated code.",
        showarrow=False,
        font=dict(size=14, color="#666"),
        align="center"
    )
    fig.update_layout(
        height=400,
        xaxis=dict(visible=False),
        yaxis=dict(visible=False),
        plot_bgcolor="white",
        paper_bgcolor="white"
    )
    return fig
