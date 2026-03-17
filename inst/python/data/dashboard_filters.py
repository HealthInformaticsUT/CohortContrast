"""Dashboard filter helpers for concept visibility state."""

from typing import Dict, List, Optional, Set

import pandas as pd

from utils.helpers import is_ordinal_concept as _is_ordinal_concept
from utils.helpers import normalize_concept_id as _normalize_concept_id

_dashboard_show_state: Optional[Dict[str, Dict[int, bool]]] = None


def configure_dashboard_show_state(store: Dict[str, Dict[int, bool]]) -> None:
    """Set shared dashboard show-state storage used by helper functions."""
    global _dashboard_show_state
    _dashboard_show_state = store


def _get_store() -> Dict[str, Dict[int, bool]]:
    if _dashboard_show_state is None:
        return {}
    return _dashboard_show_state


def apply_filters_to_data(
    updated_data: List[Dict],
    target_min: float,
    target_max: float,
    ratio_min: float,
    ratio_max: float,
    selected_heritages_set: set,
    selected_study: str,
) -> None:
    """Apply filter criteria to dashboard data and update _show column."""
    if not updated_data:
        return

    dashboard_show_state = _get_store()

    if len(updated_data) < 100:
        for row in updated_data:
            target_prevalence = row.get("TARGET_SUBJECT_PREVALENCE_PCT", 0)
            if target_prevalence is None or pd.isna(target_prevalence):
                target_prevalence = 0

            ratio = row.get("PREVALENCE_DIFFERENCE_RATIO_DISPLAY", 0)
            if ratio is None or pd.isna(ratio):
                ratio = 0

            heritage = row.get("HERITAGE")
            heritage_str = str(heritage) if heritage is not None and pd.notna(heritage) else ""
            heritage_selected = heritage_str in selected_heritages_set if heritage_str else False

            matches_target = target_min <= target_prevalence <= target_max
            matches_ratio = ratio_min <= ratio <= ratio_max
            matches_heritage = heritage_selected

            row["_show"] = matches_target and matches_ratio and matches_heritage

            concept_id = row.get("_concept_id")
            if concept_id is not None:
                if selected_study not in dashboard_show_state:
                    dashboard_show_state[selected_study] = {}
                dashboard_show_state[selected_study][concept_id] = row["_show"]
    else:
        df = pd.DataFrame(updated_data)

        target_prev = df["TARGET_SUBJECT_PREVALENCE_PCT"].fillna(0)
        ratio = df["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"].fillna(0)
        heritage_str = df["HERITAGE"].fillna("").astype(str)

        matches_target = (target_prev >= target_min) & (target_prev <= target_max)
        matches_ratio = (ratio >= ratio_min) & (ratio <= ratio_max)
        matches_heritage = heritage_str.isin(selected_heritages_set)

        df["_show"] = matches_target & matches_ratio & matches_heritage

        if selected_study not in dashboard_show_state:
            dashboard_show_state[selected_study] = {}

        concept_ids = df["_concept_id"].dropna()
        show_values = df.loc[concept_ids.index, "_show"]
        for idx, concept_id in concept_ids.items():
            dashboard_show_state[selected_study][concept_id] = bool(show_values.loc[idx])

        for idx, row in enumerate(updated_data):
            row["_show"] = df.loc[idx, "_show"]


def filter_ordinal_concepts_for_filtered_mains(
    updated_data: List[Dict],
    selected_study: str,
    manually_set_concepts: Optional[Set[int]] = None,
) -> None:
    """Keep ordinal concepts active only when linked main concepts are active."""
    if manually_set_concepts is None:
        manually_set_concepts = set()

    dashboard_show_state = _get_store()
    active_main_concepts = {}

    for row in updated_data:
        if not _is_ordinal_concept(row) and row.get("_show", False):
            main_concept_id = row.get("CONCEPT_ID")
            main_heritage = row.get("HERITAGE")

            if main_concept_id is not None:
                main_concept_id_normalized = _normalize_concept_id(main_concept_id)
                heritage_key = str(main_heritage) if main_heritage is not None and pd.notna(main_heritage) else None
                key = (main_concept_id_normalized, heritage_key)
                active_main_concepts[key] = True

    for ordinal_row in updated_data:
        if _is_ordinal_concept(ordinal_row):
            ordinal_concept_id = ordinal_row.get("_concept_id")
            if ordinal_concept_id in manually_set_concepts:
                continue

            original_concept_id = ordinal_row.get("ORIGINAL_CONCEPT_ID")
            ordinal_heritage = ordinal_row.get("HERITAGE")
            ordinal_passes_base_filters = bool(ordinal_row.get("_show", False))

            ordinal_allowed = False
            if ordinal_passes_base_filters and original_concept_id is not None:
                original_concept_id_normalized = _normalize_concept_id(original_concept_id)
                heritage_key = str(ordinal_heritage) if ordinal_heritage is not None and pd.notna(ordinal_heritage) else None
                key = (original_concept_id_normalized, heritage_key)
                ordinal_allowed = key in active_main_concepts

            if not ordinal_allowed:
                ordinal_row["_show"] = False
                ordinal_concept_id_for_state = ordinal_row.get("_concept_id")
                if ordinal_concept_id_for_state is not None:
                    if selected_study not in dashboard_show_state:
                        dashboard_show_state[selected_study] = {}
                    dashboard_show_state[selected_study][ordinal_concept_id_for_state] = False


def show_ordinals_for_active_mains(
    updated_data: List[Dict],
    selected_study: str,
    manually_set_concepts: Optional[Set[int]] = None,
) -> None:
    """Show ordinal concepts when their main concepts are active and they passed base filters."""
    if manually_set_concepts is None:
        manually_set_concepts = set()

    dashboard_show_state = _get_store()
    active_main_concepts = {}

    for row in updated_data:
        if not _is_ordinal_concept(row) and row.get("_show", False):
            main_concept_id = row.get("CONCEPT_ID")
            main_heritage = row.get("HERITAGE")

            if main_concept_id is not None:
                main_concept_id_normalized = _normalize_concept_id(main_concept_id)
                heritage_key = str(main_heritage) if main_heritage is not None and pd.notna(main_heritage) else None
                key = (main_concept_id_normalized, heritage_key)
                active_main_concepts[key] = True

    for ordinal_row in updated_data:
        if _is_ordinal_concept(ordinal_row):
            ordinal_concept_id = ordinal_row.get("_concept_id")
            if ordinal_concept_id in manually_set_concepts:
                continue

            if not ordinal_row.get("_show", False):
                continue

            original_concept_id = ordinal_row.get("ORIGINAL_CONCEPT_ID")
            ordinal_heritage = ordinal_row.get("HERITAGE")

            if original_concept_id is not None:
                original_concept_id_normalized = _normalize_concept_id(original_concept_id)
                heritage_key = str(ordinal_heritage) if ordinal_heritage is not None and pd.notna(ordinal_heritage) else None
                key = (original_concept_id_normalized, heritage_key)

                if key in active_main_concepts:
                    ordinal_row["_show"] = True
                    ordinal_concept_id_for_state = ordinal_row.get("_concept_id")
                    if ordinal_concept_id_for_state is not None:
                        if selected_study not in dashboard_show_state:
                            dashboard_show_state[selected_study] = {}
                        dashboard_show_state[selected_study][ordinal_concept_id_for_state] = True


def hide_ordinals_for_active_mains(
    updated_data: List[Dict],
    selected_study: str,
    manually_set_concepts: Optional[Set[int]] = None,
) -> None:
    """Hide all ordinal concepts."""
    if manually_set_concepts is None:
        manually_set_concepts = set()

    dashboard_show_state = _get_store()

    for ordinal_row in updated_data:
        if _is_ordinal_concept(ordinal_row):
            ordinal_concept_id = ordinal_row.get("_concept_id")
            if ordinal_concept_id in manually_set_concepts:
                continue

            ordinal_row["_show"] = False
            ordinal_concept_id_for_state = ordinal_row.get("_concept_id")
            if ordinal_concept_id_for_state is not None:
                if selected_study not in dashboard_show_state:
                    dashboard_show_state[selected_study] = {}
                dashboard_show_state[selected_study][ordinal_concept_id_for_state] = False
