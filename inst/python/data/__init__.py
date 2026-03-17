"""Data loading and processing module for ContrastViewer."""

from data.loader import (
    load_parquet_files,
    load_study_summaries,
)
from data.occurrence_metrics import (
    calculate_median_first_occurrence,
    calculate_ordinal_medians,
)
from data.processing import (
    calculate_concept_metrics_from_patients,
    create_ordinal_concepts,
)
from data.dashboard_filters import (
    apply_filters_to_data,
    filter_ordinal_concepts_for_filtered_mains,
)

__all__ = [
    'load_parquet_files',
    'load_study_summaries',
    'calculate_median_first_occurrence',
    'calculate_ordinal_medians',
    'calculate_concept_metrics_from_patients',
    'create_ordinal_concepts',
    'apply_filters_to_data',
    'filter_ordinal_concepts_for_filtered_mains',
]
