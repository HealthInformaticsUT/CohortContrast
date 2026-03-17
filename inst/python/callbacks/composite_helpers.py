"""Helper utilities for composite plot callbacks."""

from typing import Dict, List, Optional, Tuple

import numpy as np
import pandas as pd

from utils.helpers import normalize_concept_id as _normalize_concept_id


def build_passed_concept_ids(active_concepts: List[Dict]) -> set:
    passed_concept_ids = set()
    for concept in active_concepts:
        concept_id = concept.get("_concept_id") or concept.get("CONCEPT_ID")
        if concept_id is None:
            continue
        passed_concept_ids.add(_normalize_concept_id(concept_id))
        passed_concept_ids.add(str(concept_id))
        passed_concept_ids.add(str(concept_id).replace(".0", ""))
    return passed_concept_ids


def apply_passed_concept_filter_to_table(data_to_use: List[Dict], passed_concept_ids: set) -> List[Dict]:
    updated_table_data = [row.copy() for row in data_to_use]
    for row in updated_table_data:
        concept_id = row.get("_concept_id") or row.get("CONCEPT_ID")
        if concept_id is None:
            continue
        norm_id = _normalize_concept_id(concept_id)
        row["_show"] = row.get("_show", False) and (
            norm_id in passed_concept_ids
            or str(concept_id) in passed_concept_ids
            or str(concept_id).replace(".0", "") in passed_concept_ids
        )
    return updated_table_data


def get_trace_row(trace, default_row: int = 1) -> int:
    if hasattr(trace, "yaxis") and trace.yaxis:
        yaxis_str = str(trace.yaxis).lower()
        row_str = yaxis_str.replace("y", "").replace("axis", "").strip()
        if row_str.isdigit():
            return int(row_str)
        if row_str == "":
            return 1
    return default_row


def expand_id_set(values, *, include_float: bool = False) -> set:
    expanded = set()
    for value in values or []:
        expanded.add(value)
        expanded.add(str(value))
        try:
            expanded.add(int(value))
        except (ValueError, TypeError):
            pass
        if include_float:
            try:
                expanded.add(float(value))
            except (ValueError, TypeError):
                pass
    return expanded


def normalize_id_loose(value):
    if value is None:
        return None
    value_str = str(value).replace(".0", "")
    try:
        return int(float(value_str))
    except (ValueError, TypeError):
        return value_str


def match_series_id(series: pd.Series, target_id) -> pd.Series:
    if target_id is None:
        return pd.Series([False] * len(series), index=series.index)
    target_normalized = normalize_id_loose(target_id)
    matches = (series == target_id) | (series == target_normalized)
    try:
        matches = matches | (series == str(target_id))
    except Exception:
        pass
    try:
        matches = matches | (series == int(float(str(target_id).replace(".0", ""))))
    except Exception:
        pass
    return matches


def count_occurrences(time_values) -> int:
    if time_values is None:
        return 0
    if isinstance(time_values, np.ndarray):
        return len(time_values[~np.isnan(time_values)])
    if isinstance(time_values, (list, tuple)):
        valid = [
            t
            for t in time_values
            if t is not None and not (isinstance(t, (int, float)) and pd.isna(t))
        ]
        return len(valid)
    return 1 if not pd.isna(time_values) else 0


def count_unique_occurrences(time_values) -> int:
    if time_values is None:
        return 0
    if isinstance(time_values, np.ndarray):
        if time_values.size == 0:
            return 0
        valid = time_values[~np.isnan(time_values)]
        return len(np.unique(valid))
    if isinstance(time_values, (list, tuple)):
        valid = [
            t
            for t in time_values
            if t is not None and not (isinstance(t, (int, float)) and pd.isna(t))
        ]
        return len(set(valid))
    if isinstance(time_values, (int, float)) and not pd.isna(time_values):
        return 1
    return 0


def extract_first_time(time_values):
    if time_values is None:
        return np.nan
    if isinstance(time_values, np.ndarray):
        if time_values.size == 0:
            return np.nan
        valid = time_values[~np.isnan(time_values)]
        return float(np.min(valid)) if len(valid) > 0 else np.nan
    if isinstance(time_values, (list, tuple)):
        valid = [
            t
            for t in time_values
            if t is not None and not (isinstance(t, (int, float)) and pd.isna(t))
        ]
        return float(min(valid)) if valid else np.nan
    if isinstance(time_values, (int, float)) and not pd.isna(time_values):
        return float(time_values)
    return np.nan


def compute_age_axis_range(mean_ages: List[float]) -> Tuple[Optional[float], Optional[float]]:
    if not mean_ages:
        return None, None
    min_age = min(mean_ages)
    max_age = max(mean_ages)
    age_range = max_age - min_age
    if age_range == 0:
        return min_age - 5, max_age + 5
    return min_age - age_range * 0.1, max_age + age_range * 0.1


def compute_male_prop_axis_range(
    mean_props: List[float], ci_lows: List[float], ci_highs: List[float]
) -> Tuple[float, float]:
    if not mean_props:
        return 0.0, 1.0
    min_prop = min(min(mean_props), min(ci_lows)) if ci_lows else min(mean_props)
    max_prop = max(max(mean_props), max(ci_highs)) if ci_highs else max(mean_props)
    prop_range = max_prop - min_prop
    if prop_range == 0:
        if min_prop == 1.0:
            return 0.95, 1.0
        if min_prop == 0.0:
            return 0.0, 0.05
        return max(0, min_prop - 0.1), min(1, max_prop + 0.1)
    x_min = max(0, min_prop - prop_range * 0.1)
    x_max = min(1, max_prop + prop_range * 0.1)
    if max_prop >= 0.99 and min_prop >= 0.99:
        return 0.95, 1.0
    return x_min, x_max
