"""Layout helpers for combined composite figure."""

from typing import Dict, List, Optional

import pandas as pd


def add_combined_column_titles(
    fig_combined,
    column_widths: List[float],
    *,
    cluster_data_available: bool,
    optimal_k: int,
    clustering_results: Optional[Dict],
) -> None:
    base_col_titles = ["Event occurrences", "Prevalence/Enrichment", "Age", "Male %"]

    if cluster_data_available:
        cluster_counts_for_titles = {}
        if clustering_results:
            if "cluster_counts" in clustering_results and clustering_results["cluster_counts"]:
                cluster_counts_for_titles = clustering_results["cluster_counts"]
            else:
                patient_assignments_data = clustering_results.get("patient_assignments", [])
                if isinstance(patient_assignments_data, list):
                    patient_assignments_df = (
                        pd.DataFrame(patient_assignments_data) if patient_assignments_data else pd.DataFrame()
                    )
                else:
                    patient_assignments_df = (
                        patient_assignments_data
                        if isinstance(patient_assignments_data, pd.DataFrame)
                        else pd.DataFrame()
                    )
                if not patient_assignments_df.empty and "cluster" in patient_assignments_df.columns:
                    cluster_counts_for_titles = patient_assignments_df["cluster"].value_counts().to_dict()

        cluster_titles = []
        for i in range(optimal_k):
            cluster_label = f"C{i + 1}"
            count = cluster_counts_for_titles.get(cluster_label, 0)
            cluster_titles.append(f"{cluster_label} (n={count})")
        col_titles = base_col_titles + cluster_titles
    else:
        col_titles = base_col_titles

    total_width = sum(column_widths)
    col_x_positions = []
    cumulative_width = 0
    for width in column_widths:
        center = (cumulative_width + width / 2) / total_width
        col_x_positions.append(center)
        cumulative_width += width

    for title, x_pos in zip(col_titles, col_x_positions):
        fig_combined.add_annotation(
            text=title,
            xref="paper",
            yref="paper",
            x=x_pos,
            y=1.0,
            showarrow=False,
            font=dict(size=10 if cluster_data_available else 12, color="black"),
            xanchor="center",
            yanchor="bottom",
        )


def add_base_column_backgrounds(fig_combined, *, base_cols: int, column_widths: List[float]) -> None:
    column_domains = {}
    try:
        for col_idx in range(1, base_cols + 1):
            xaxis_key = "xaxis" if col_idx == 1 else f"xaxis{col_idx}"
            if hasattr(fig_combined.layout, xaxis_key):
                xaxis = getattr(fig_combined.layout, xaxis_key)
                if hasattr(xaxis, "domain") and xaxis.domain:
                    domain = xaxis.domain
                    if domain and len(domain) >= 2:
                        column_domains[col_idx] = domain
    except (AttributeError, IndexError, TypeError):
        pass

    grey_color = "#f8f8f8"
    for col_idx in range(1, base_cols + 1):
        is_grey = col_idx in [2, 4]
        bg_color = grey_color if is_grey else "white"

        if col_idx in column_domains:
            domain = column_domains[col_idx]
            if is_grey:
                left_extend = 0.3
                right_extend = 0.7 if col_idx == 2 else 0.3

                if col_idx > 1 and (col_idx - 1) in column_domains:
                    prev_domain = column_domains[col_idx - 1]
                    x0 = prev_domain[1] + (domain[0] - prev_domain[1]) * left_extend
                else:
                    x0 = max(0, domain[0] - 0.02)

                if col_idx < base_cols and (col_idx + 1) in column_domains:
                    next_domain = column_domains[col_idx + 1]
                    x1 = domain[1] + (next_domain[0] - domain[1]) * right_extend
                else:
                    x1 = min(1, domain[1] + 0.02)
            else:
                x0, x1 = domain[0], domain[1]

            fig_combined.add_shape(
                type="rect",
                x0=x0,
                x1=x1,
                y0=0,
                y1=1,
                xref="paper",
                yref="paper",
                fillcolor=bg_color,
                line=dict(width=0),
                layer="below",
            )
        elif len(column_widths) >= col_idx:
            total_width = sum(column_widths)
            cumulative_before = sum(column_widths[: col_idx - 1]) / total_width
            cumulative_after = sum(column_widths[:col_idx]) / total_width
            plot_left, plot_width = 0.15, 0.82
            x0_base = plot_left + cumulative_before * plot_width
            x1_base = plot_left + cumulative_after * plot_width

            if is_grey:
                left_extend = 0.3
                right_extend = 0.7 if col_idx == 2 else 0.3

                if col_idx > 1:
                    prev_right = plot_left + sum(column_widths[: col_idx - 1]) / total_width * plot_width
                    x0 = prev_right + (x0_base - prev_right) * left_extend
                else:
                    x0 = max(0, x0_base - 0.02)

                if col_idx < len(column_widths):
                    next_left = plot_left + sum(column_widths[: col_idx + 1]) / total_width * plot_width
                    x1 = x1_base + (next_left - x1_base) * right_extend
                else:
                    x1 = min(1, x1_base + 0.02)
            else:
                x0, x1 = x0_base, x1_base

            fig_combined.add_shape(
                type="rect",
                x0=x0,
                x1=x1,
                y0=0,
                y1=1,
                xref="paper",
                yref="paper",
                fillcolor=bg_color,
                line=dict(width=0),
                layer="below",
            )


def update_combined_x_axes(
    fig_combined,
    *,
    num_rows: int,
    time_x_min,
    time_x_max,
    prevalence_x_min: float,
    prevalence_x_max: float,
    age_x_min,
    age_x_max,
    male_prop_x_min,
    male_prop_x_max,
    cluster_data_available: bool,
    optimal_k: int,
    base_cols: int,
) -> None:
    for row_idx in range(1, num_rows + 1):
        fig_combined.update_xaxes(
            range=[time_x_min, time_x_max] if time_x_min is not None and time_x_max is not None else None,
            title_text="Time to Event (days)" if row_idx == num_rows else "",
            fixedrange=True,
            tickangle=0,
            row=row_idx,
            col=1,
        )

        fig_combined.update_xaxes(
            range=[prevalence_x_min, prevalence_x_max],
            title_text="Prevalence" if row_idx == num_rows else "",
            tickformat=".0%",
            fixedrange=True,
            tickangle=0,
            showgrid=False,
            showline=False,
            zeroline=False,
            row=row_idx,
            col=2,
        )

        fig_combined.update_xaxes(
            range=[age_x_min, age_x_max] if age_x_min is not None and age_x_max is not None else None,
            title_text="Age (years)" if row_idx == num_rows else "",
            fixedrange=True,
            tickangle=0,
            row=row_idx,
            col=3,
        )

        male_prop_range = (
            [male_prop_x_min, male_prop_x_max]
            if male_prop_x_min is not None and male_prop_x_max is not None
            else [0.0, 1.0]
        )
        fig_combined.update_xaxes(
            range=male_prop_range,
            title_text="Male Proportion" if row_idx == num_rows else "",
            tickformat=".0%",
            fixedrange=True,
            tickangle=0,
            showgrid=False,
            showline=False,
            zeroline=False,
            row=row_idx,
            col=4,
        )

        if cluster_data_available:
            for cluster_col_idx in range(optimal_k):
                col_idx = base_cols + cluster_col_idx + 1
                fig_combined.update_xaxes(
                    showticklabels=False,
                    showgrid=False,
                    zeroline=False,
                    fixedrange=True,
                    row=row_idx,
                    col=col_idx,
                )


def add_overall_male_reference_lines(
    fig_combined,
    *,
    overall_avg_male_prop,
    heritages_with_data: List[str],
    heritage_groups_order: Dict[str, List[Dict]],
) -> None:
    if overall_avg_male_prop is None:
        return
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
            col=4,
        )


def update_combined_y_axes(
    fig_combined,
    fig_composite,
    *,
    num_rows: int,
    cluster_data_available: bool,
    optimal_k: int,
    base_cols: int,
) -> None:
    for row_idx in range(1, num_rows + 1):
        yaxis_key = f"yaxis{row_idx if row_idx > 1 else ''}"
        yaxis_layout = getattr(fig_composite.layout, yaxis_key, None) if hasattr(fig_composite.layout, yaxis_key) else None

        tickvals = yaxis_layout.tickvals if yaxis_layout and hasattr(yaxis_layout, "tickvals") else None
        ticktext = yaxis_layout.ticktext if yaxis_layout and hasattr(yaxis_layout, "ticktext") else None
        y_range = yaxis_layout.range if yaxis_layout and hasattr(yaxis_layout, "range") else None

        fig_combined.update_yaxes(
            tickmode="array",
            tickvals=tickvals,
            ticktext=ticktext,
            range=y_range,
            fixedrange=True,
            row=row_idx,
            col=1,
        )

        fig_combined.update_yaxes(
            tickmode="array",
            tickvals=tickvals,
            ticktext=[""] * len(ticktext) if ticktext else [],
            range=y_range,
            showticklabels=False,
            showgrid=False,
            showline=False,
            zeroline=False,
            fixedrange=True,
            row=row_idx,
            col=2,
        )

        fig_combined.update_yaxes(
            tickmode="array",
            tickvals=tickvals,
            ticktext=[""] * len(ticktext) if ticktext else [],
            range=y_range,
            showticklabels=False,
            showgrid=False,
            fixedrange=True,
            row=row_idx,
            col=3,
        )

        fig_combined.update_yaxes(
            tickmode="array",
            tickvals=tickvals,
            ticktext=[""] * len(ticktext) if ticktext else [],
            range=y_range,
            showticklabels=False,
            showgrid=False,
            showline=False,
            zeroline=False,
            fixedrange=True,
            row=row_idx,
            col=4,
        )

        if cluster_data_available:
            for cluster_col_idx in range(optimal_k):
                col_idx = base_cols + cluster_col_idx + 1
                fig_combined.update_yaxes(
                    tickmode="array",
                    tickvals=tickvals,
                    ticktext=[""] * len(ticktext) if ticktext else [],
                    range=y_range,
                    showticklabels=False,
                    showgrid=False,
                    fixedrange=True,
                    row=row_idx,
                    col=col_idx,
                )


def add_overall_age_reference_lines(
    fig_combined,
    *,
    overall_avg_age,
    heritages_with_data: List[str],
    heritage_groups_order: Dict[str, List[Dict]],
) -> None:
    if overall_avg_age is None:
        return
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
            col=3,
        )
