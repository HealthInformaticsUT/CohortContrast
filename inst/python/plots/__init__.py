"""Plotting module for ContrastViewer."""

from plots.composite import create_composite_plot
from plots.prevalence import create_prevalence_plot, create_enrichment_colorbar_figure
from plots.age import calculate_age_stats, create_age_plot
from plots.male_proportion import calculate_male_prop_stats, create_male_prop_plot
from plots.clustering_plots import create_clustering_heatmap, create_cluster_prevalence_plots

__all__ = [
    'create_composite_plot',
    'create_prevalence_plot',
    'create_enrichment_colorbar_figure',
    'calculate_age_stats',
    'create_age_plot',
    'calculate_male_prop_stats',
    'create_male_prop_plot',
    'create_clustering_heatmap',
    'create_cluster_prevalence_plots',
]
