"""Mappings tab callbacks for patient-mode concept merges."""

import json
import shutil
from pathlib import Path
from typing import Dict, List, Optional, Tuple
import time
import re

import numpy as np
import pandas as pd
from dash import Input, Output, State, callback_context, no_update

from data.cache import get_or_load_parquet_data
from utils.helpers import (
    convert_list_columns_to_strings,
    get_min_time,
    get_unique_occurrences,
    normalize_concept_id,
    normalize_concept_id_series,
)


_TARGET_COHORT_LABELS = {"target", "2"}
_CONTROL_COHORT_LABELS = {"control", "1"}


def _safe_float(value, default=0.0) -> float:
    try:
        if value is None or (isinstance(value, float) and np.isnan(value)):
            return float(default)
        return float(value)
    except (TypeError, ValueError):
        return float(default)


def _base_concept_id_int(value) -> Optional[int]:
    """
    Parse concept ID into base integer ID.

    Rules:
    - Strip float suffixes (e.g. 9202.0 -> 9202)
    - Strip generated suffixes after "." or "_" if present
    - Return None when no integer-like base ID is available
    """
    if value is None:
        return None
    try:
        if pd.isna(value):
            return None
    except Exception:
        pass

    raw = normalize_concept_id(value).strip()
    if not raw:
        return None

    token = re.split(r"[._]", raw, maxsplit=1)[0].strip()
    if not token:
        return None

    try:
        return int(token)
    except (TypeError, ValueError):
        try:
            return int(float(token))
        except (TypeError, ValueError):
            return None


def _is_ordinal(row: Dict) -> bool:
    flag = row.get("IS_ORDINAL", False)
    if isinstance(flag, str):
        return flag.strip().lower() in {"true", "1", "yes", "y"}
    try:
        return bool(flag) and not pd.isna(flag)
    except Exception:
        return bool(flag)


def _build_candidate_rows(dashboard_data: Optional[List[Dict]]) -> List[Dict]:
    if not isinstance(dashboard_data, list):
        return []
    rows: List[Dict] = []
    for row in dashboard_data:
        if _is_ordinal(row):
            continue
        candidate = row.copy()
        candidate["_map"] = False
        rows.append(candidate)
    return rows


def _build_heritage_options(dashboard_data: Optional[List[Dict]]) -> List[Dict]:
    if not isinstance(dashboard_data, list):
        return []
    values = sorted({str(row.get("HERITAGE")) for row in dashboard_data if row.get("HERITAGE") is not None and str(row.get("HERITAGE")).strip()})
    return [{"label": v, "value": v} for v in values]


def _selected_map_rows(candidate_rows: Optional[List[Dict]]) -> List[Dict]:
    if not isinstance(candidate_rows, list):
        return []
    return [row for row in candidate_rows if bool(row.get("_map", False))]


def _active_main_rows(dashboard_data: Optional[List[Dict]]) -> List[Dict]:
    if not isinstance(dashboard_data, list):
        return []
    mains = [row for row in dashboard_data if not _is_ordinal(row)]
    active = [row for row in mains if bool(row.get("_show", False))]
    return active if active else mains


def _cohort_filter_target(df: pd.DataFrame) -> pd.DataFrame:
    if df.empty:
        return df
    if "COHORT_DEFINITION_ID" not in df.columns:
        return df
    cohort_norm = df["COHORT_DEFINITION_ID"].astype(str).str.strip().str.lower()
    return df[cohort_norm.isin(_TARGET_COHORT_LABELS)].copy()


def _to_float(value, default=0.0) -> float:
    try:
        if value is None or (isinstance(value, float) and np.isnan(value)):
            return float(default)
        return float(value)
    except Exception:
        return float(default)


def _parse_source_concept_ids(value) -> List[str]:
    if value is None:
        return []
    text = str(value).strip()
    if not text:
        return []
    parts = [part.strip() for part in text.split(",") if part.strip()]
    return [normalize_concept_id(part) for part in parts]


def _load_concept_ancestor_table(parquet_data: Dict, selected_study: str, data_dir: Path) -> pd.DataFrame:
    cached = parquet_data.get("concepts_concept_ancestor", pd.DataFrame())
    if isinstance(cached, pd.DataFrame) and not cached.empty:
        return cached

    path = data_dir / selected_study / "concepts_concept_ancestor.parquet"
    if not path.exists():
        return pd.DataFrame()
    try:
        loaded = pd.read_parquet(path)
    except Exception:
        return pd.DataFrame()

    parquet_data["concepts_concept_ancestor"] = loaded
    return loaded


def _build_hierarchy_suggestions(
    dashboard_data: Optional[List[Dict]],
    concept_ancestor_df: pd.DataFrame,
    min_depth: int = 1,
    allow_only_minors: bool = True,
) -> List[Dict]:
    if not isinstance(dashboard_data, list) or concept_ancestor_df.empty:
        return []

    candidates = _active_main_rows(dashboard_data)
    if len(candidates) < 2:
        return []

    concept_meta: Dict[str, Dict] = {}
    for row in candidates:
        cid = normalize_concept_id(row.get("CONCEPT_ID") or row.get("_concept_id"))
        if not cid:
            continue
        prev = _to_float(row.get("TARGET_SUBJECT_PREVALENCE"), default=0.0)
        existing = concept_meta.get(cid)
        if existing is None or prev > _to_float(existing.get("TARGET_SUBJECT_PREVALENCE"), default=0.0):
            concept_meta[cid] = row

    if len(concept_meta) < 2:
        return []

    ancestor_col = None
    desc_col = None
    depth_col = None
    for col in ["ancestor_concept_id", "ANCESTOR_CONCEPT_ID"]:
        if col in concept_ancestor_df.columns:
            ancestor_col = col
            break
    for col in ["descendant_concept_id", "DESCENDANT_CONCEPT_ID"]:
        if col in concept_ancestor_df.columns:
            desc_col = col
            break
    for col in ["min_levels_of_separation", "MIN_LEVELS_OF_SEPARATION", "max_levels_of_separation", "MAX_LEVELS_OF_SEPARATION"]:
        if col in concept_ancestor_df.columns:
            depth_col = col
            break

    if ancestor_col is None or desc_col is None:
        return []

    working = concept_ancestor_df[[ancestor_col, desc_col] + ([depth_col] if depth_col else [])].copy()
    working["__ancestor"] = normalize_concept_id_series(working[ancestor_col])
    working["__desc"] = normalize_concept_id_series(working[desc_col])

    active_ids = set(concept_meta.keys())
    working = working[
        working["__ancestor"].isin(active_ids)
        & working["__desc"].isin(active_ids)
        & (working["__ancestor"] != working["__desc"])
    ].copy()
    if working.empty:
        return []

    if depth_col is not None:
        depth_min = max(int(_to_float(min_depth, default=1)), 1)
        depth_vals = pd.to_numeric(working[depth_col], errors="coerce").fillna(0)
        working = working[depth_vals >= depth_min].copy()
        if working.empty:
            return []

    suggestions = []
    for ancestor_id, group in working.groupby("__ancestor", sort=False):
        parent_row = concept_meta.get(ancestor_id)
        if parent_row is None:
            continue

        selected_ids = [ancestor_id] + sorted(set(group["__desc"].tolist()))
        parent_prev = _to_float(parent_row.get("TARGET_SUBJECT_PREVALENCE"), default=0.0)
        parent_heritage = str(parent_row.get("HERITAGE") or "")

        selected_ids_filtered: List[str] = []
        for cid in selected_ids:
            row = concept_meta.get(cid)
            if row is None:
                continue
            if parent_heritage and str(row.get("HERITAGE") or "") != parent_heritage:
                continue
            if allow_only_minors and cid != ancestor_id:
                if _to_float(row.get("TARGET_SUBJECT_PREVALENCE"), default=0.0) > parent_prev:
                    continue
            selected_ids_filtered.append(cid)

        selected_ids_filtered = list(dict.fromkeys(selected_ids_filtered))
        if len(selected_ids_filtered) < 2:
            continue

        selected_rows = [concept_meta[cid] for cid in selected_ids_filtered if cid in concept_meta]
        names = [str(row.get("CONCEPT_NAME") or row.get("CONCEPT_ID")) for row in selected_rows]
        source_names_text = " | ".join(names[:8]) + (" | ..." if len(names) > 8 else "")

        new_concept_id_int = _base_concept_id_int(ancestor_id)
        if new_concept_id_int is None:
            continue
        new_concept_name = str(parent_row.get("CONCEPT_NAME") or f"Concept {ancestor_id}")

        suggestions.append(
            {
                "_map": False,
                "NEW_CONCEPT_ID": int(new_concept_id_int),
                "NEW_CONCEPT_NAME": new_concept_name,
                "HERITAGE": parent_heritage,
                "SOURCE_CONCEPT_IDS": ",".join(selected_ids_filtered),
                "SOURCE_CONCEPT_NAMES": source_names_text,
                "N_CONCEPTS": len(selected_ids_filtered),
                "SCORE": round(parent_prev * 100.0, 2),
                "AUX_VALUE": int(len(selected_ids_filtered) - 1),
            }
        )

    suggestions.sort(key=lambda row: (int(row.get("N_CONCEPTS", 0)), float(row.get("SCORE", 0.0))), reverse=True)
    return suggestions[:250]


def _build_correlation_suggestions(
    parquet_data: Dict,
    dashboard_data: Optional[List[Dict]],
    min_correlation: float = 0.7,
    max_days_inbetween: float = 1.0,
    heritage_drift_allowed: bool = False,
) -> List[Dict]:
    if not isinstance(dashboard_data, list):
        return []

    candidates = _active_main_rows(dashboard_data)
    if len(candidates) < 2:
        return []

    data_patients = parquet_data.get("data_patients", pd.DataFrame())
    if not isinstance(data_patients, pd.DataFrame) or data_patients.empty:
        return []
    if "CONCEPT_ID" not in data_patients.columns:
        return []
    person_col = "PERSON_ID" if "PERSON_ID" in data_patients.columns else ("SUBJECT_ID" if "SUBJECT_ID" in data_patients.columns else None)
    if person_col is None:
        return []

    concept_meta = {}
    active_ids = set()
    for row in candidates:
        cid = normalize_concept_id(row.get("CONCEPT_ID") or row.get("_concept_id"))
        if not cid:
            continue
        active_ids.add(cid)
        concept_meta[cid] = row

    if len(active_ids) < 2:
        return []

    target_rows = _cohort_filter_target(data_patients)
    if target_rows.empty:
        return []
    target_rows = target_rows.copy()
    target_rows["__concept_norm"] = normalize_concept_id_series(target_rows["CONCEPT_ID"])
    target_rows = target_rows[target_rows["__concept_norm"].isin(active_ids)].copy()
    if target_rows.empty:
        return []
    target_rows = target_rows[_presence_mask(target_rows)].copy()
    if target_rows.empty:
        return []

    presence = target_rows[[person_col, "__concept_norm"]].drop_duplicates().copy()
    presence["__present"] = 1.0
    binary = presence.pivot_table(index=person_col, columns="__concept_norm", values="__present", aggfunc="max", fill_value=0.0)
    if binary.shape[1] < 2:
        return []

    corr_matrix = binary.corr(method="pearson").fillna(0.0)
    corr_matrix.index = corr_matrix.index.astype(str)
    corr_matrix.columns = corr_matrix.columns.astype(str)
    corr_matrix.index.name = "cid1"
    corr_matrix.columns.name = "cid2"
    upper_mask = np.triu(np.ones(corr_matrix.shape), k=1).astype(bool)
    corr_pairs = corr_matrix.where(upper_mask).stack().reset_index(name="CORRELATION")
    if corr_pairs.empty:
        return []

    min_corr = max(0.0, min(1.0, _to_float(min_correlation, default=0.7)))
    corr_pairs = corr_pairs[pd.to_numeric(corr_pairs["CORRELATION"], errors="coerce").fillna(0.0) >= min_corr].copy()
    if corr_pairs.empty:
        return []

    if "TIME_TO_EVENT" in target_rows.columns:
        first_times = target_rows[[person_col, "__concept_norm", "TIME_TO_EVENT"]].copy()
        first_times["__first"] = first_times["TIME_TO_EVENT"].apply(get_min_time)
    else:
        first_times = target_rows[[person_col, "__concept_norm"]].copy()
        first_times["__first"] = np.nan
    first_per_person = (
        first_times.dropna(subset=["__first"])
        .groupby([person_col, "__concept_norm"], as_index=False)["__first"]
        .min()
        .pivot(index=person_col, columns="__concept_norm", values="__first")
    )

    suggestions = []
    max_days = max(0.0, _to_float(max_days_inbetween, default=1.0))
    for _, pair in corr_pairs.iterrows():
        cid1 = normalize_concept_id(pair["cid1"])
        cid2 = normalize_concept_id(pair["cid2"])
        row1 = concept_meta.get(cid1)
        row2 = concept_meta.get(cid2)
        if row1 is None or row2 is None:
            continue

        heritage1 = str(row1.get("HERITAGE") or "")
        heritage2 = str(row2.get("HERITAGE") or "")
        if not heritage_drift_allowed and heritage1 != heritage2:
            continue

        median_days = np.nan
        if not first_per_person.empty and cid1 in first_per_person.columns and cid2 in first_per_person.columns:
            pair_times = first_per_person[[cid1, cid2]].dropna()
            if not pair_times.empty:
                median_days = float(np.median(np.abs(pair_times[cid1] - pair_times[cid2])))
        if not np.isnan(median_days) and median_days > max_days:
            continue

        prev1 = _to_float(row1.get("TARGET_SUBJECT_PREVALENCE"), default=0.0)
        prev2 = _to_float(row2.get("TARGET_SUBJECT_PREVALENCE"), default=0.0)
        primary_row = row1 if prev1 >= prev2 else row2
        primary_id = cid1 if prev1 >= prev2 else cid2

        new_concept_id_int = _base_concept_id_int(primary_id)
        if new_concept_id_int is None:
            continue
        new_concept_name = f"{str(row1.get('CONCEPT_NAME') or cid1)} + {str(row2.get('CONCEPT_NAME') or cid2)}"
        corr_value = _to_float(pair["CORRELATION"], default=0.0)
        aux_value = round(float(median_days), 2) if not np.isnan(median_days) else np.nan

        suggestions.append(
            {
                "_map": False,
                "NEW_CONCEPT_ID": int(new_concept_id_int),
                "NEW_CONCEPT_NAME": new_concept_name,
                "HERITAGE": str(primary_row.get("HERITAGE") or heritage1 or heritage2),
                "SOURCE_CONCEPT_IDS": f"{cid1},{cid2}",
                "SOURCE_CONCEPT_NAMES": f"{str(row1.get('CONCEPT_NAME') or cid1)} | {str(row2.get('CONCEPT_NAME') or cid2)}",
                "N_CONCEPTS": 2,
                "SCORE": round(corr_value, 4),
                "AUX_VALUE": aux_value,
            }
        )

    suggestions.sort(
        key=lambda row: (
            float(row.get("SCORE", 0.0)),
            -float(row.get("AUX_VALUE", np.inf)) if not pd.isna(row.get("AUX_VALUE")) else -999999.0,
        ),
        reverse=True,
    )
    return suggestions[:250]


def _execute_single_merge(
    parquet_data: Dict,
    existing_rows: List[Dict],
    selected_rows: List[Dict],
    new_concept_id_str: str,
    new_concept_name_str: str,
    new_heritage_str: str,
) -> List[Dict]:
    metrics = _compute_concept_metrics(parquet_data, new_concept_id_str, new_heritage_str)
    _apply_merge_to_patient_tables(
        parquet_data,
        selected_rows,
        new_concept_id_str,
        new_concept_name_str,
        new_heritage_str,
        metrics,
    )
    metrics = _compute_concept_metrics(parquet_data, new_concept_id_str, new_heritage_str)
    _sync_data_features_metrics(parquet_data, new_concept_id_str, new_heritage_str, metrics)
    updated_dashboard = _build_updated_dashboard_rows(
        existing_rows,
        selected_rows,
        new_concept_id_str,
        new_concept_name_str,
        new_heritage_str,
        metrics,
    )
    _append_mapping_history(parquet_data, selected_rows, new_concept_id_str, new_concept_name_str)
    return updated_dashboard


def _resolve_suggestion_source_rows(
    dashboard_rows: List[Dict],
    source_concept_ids: List[str],
) -> List[Dict]:
    main_rows = _active_main_rows(dashboard_rows)
    if not main_rows or not source_concept_ids:
        return []

    exact_lookup: Dict[str, Dict] = {}
    base_lookup: Dict[str, Dict] = {}
    for row in main_rows:
        cid = normalize_concept_id(row.get("CONCEPT_ID") or row.get("_concept_id"))
        if not cid:
            continue
        if cid not in exact_lookup:
            exact_lookup[cid] = row
        base_id_int = _base_concept_id_int(cid)
        if base_id_int is not None:
            base_key = str(base_id_int)
            if base_key not in base_lookup:
                base_lookup[base_key] = row

    selected_rows: List[Dict] = []
    seen_norm_ids = set()
    for source_id in source_concept_ids:
        source_norm = normalize_concept_id(source_id)
        if not source_norm:
            continue
        match = exact_lookup.get(source_norm)
        if match is None:
            source_base = _base_concept_id_int(source_norm)
            if source_base is not None:
                match = base_lookup.get(str(source_base))
        if match is None:
            continue
        match_norm = normalize_concept_id(match.get("CONCEPT_ID") or match.get("_concept_id"))
        if match_norm in seen_norm_ids:
            continue
        seen_norm_ids.add(match_norm)
        selected_rows.append(match)
    return selected_rows


def _derive_primary_merge_defaults(selected_rows: List[Dict]) -> Tuple[str, str]:
    if not selected_rows:
        return "", ""

    def _score(row: Dict) -> float:
        pct = row.get("TARGET_SUBJECT_PREVALENCE_PCT")
        if pct is not None:
            return _safe_float(pct, default=0.0)
        return _safe_float(row.get("TARGET_SUBJECT_PREVALENCE"), default=0.0) * 100.0

    primary = max(selected_rows, key=_score)
    default_name = str(primary.get("CONCEPT_NAME") or "")
    default_heritage = str(primary.get("HERITAGE") or "")
    return default_name, default_heritage


def _get_target_control_totals(parquet_data: Dict) -> Tuple[int, int]:
    data_initial = parquet_data.get("data_initial", pd.DataFrame())
    if isinstance(data_initial, pd.DataFrame) and not data_initial.empty:
        if "COHORT_DEFINITION_ID" in data_initial.columns:
            cohort_norm = data_initial["COHORT_DEFINITION_ID"].astype(str).str.strip().str.lower()
        else:
            cohort_norm = pd.Series(["target"] * len(data_initial), index=data_initial.index)

        subject_col = "SUBJECT_ID" if "SUBJECT_ID" in data_initial.columns else ("PERSON_ID" if "PERSON_ID" in data_initial.columns else None)
        if subject_col is not None:
            target_total = int(data_initial.loc[cohort_norm.isin(_TARGET_COHORT_LABELS), subject_col].nunique())
            control_total = int(data_initial.loc[cohort_norm.isin(_CONTROL_COHORT_LABELS), subject_col].nunique())
            return target_total, control_total

    data_patients = parquet_data.get("data_patients", pd.DataFrame())
    if isinstance(data_patients, pd.DataFrame) and not data_patients.empty and "PERSON_ID" in data_patients.columns:
        if "COHORT_DEFINITION_ID" in data_patients.columns:
            cohort_norm = data_patients["COHORT_DEFINITION_ID"].astype(str).str.strip().str.lower()
            target_total = int(data_patients.loc[cohort_norm.isin(_TARGET_COHORT_LABELS), "PERSON_ID"].nunique())
            control_total = int(data_patients.loc[cohort_norm.isin(_CONTROL_COHORT_LABELS), "PERSON_ID"].nunique())
            return target_total, control_total
    return 0, 0


def _presence_mask(df: pd.DataFrame) -> pd.Series:
    if df.empty:
        return pd.Series(dtype=bool)
    if "PREVALENCE" in df.columns:
        prevalence_num = pd.to_numeric(df["PREVALENCE"], errors="coerce").fillna(0)
        return prevalence_num > 0
    if "TIME_TO_EVENT" in df.columns:
        return df["TIME_TO_EVENT"].apply(lambda x: len(get_unique_occurrences(x)) > 0)
    return pd.Series([True] * len(df), index=df.index)


def _compute_concept_metrics(parquet_data: Dict, concept_id: str, heritage: Optional[str]) -> Dict:
    data_patients = parquet_data.get("data_patients", pd.DataFrame())
    if not isinstance(data_patients, pd.DataFrame) or data_patients.empty or "CONCEPT_ID" not in data_patients.columns:
        return {
            "target_subject_count": 0,
            "control_subject_count": 0,
            "target_subject_prevalence": 0.0,
            "control_subject_prevalence": 0.0,
            "prevalence_difference_ratio": 0.0,
            "median_first_occurrence": None,
        }

    working = data_patients.copy()
    working["__concept_norm"] = normalize_concept_id_series(working["CONCEPT_ID"])
    concept_norm = normalize_concept_id(concept_id)
    mask = working["__concept_norm"] == concept_norm
    if "HERITAGE" in working.columns and heritage is not None and str(heritage).strip():
        mask = mask & (working["HERITAGE"].astype(str) == str(heritage))

    concept_rows = working[mask].copy()
    if concept_rows.empty:
        return {
            "target_subject_count": 0,
            "control_subject_count": 0,
            "target_subject_prevalence": 0.0,
            "control_subject_prevalence": 0.0,
            "prevalence_difference_ratio": 0.0,
            "median_first_occurrence": None,
        }

    if "COHORT_DEFINITION_ID" in concept_rows.columns:
        cohort_norm = concept_rows["COHORT_DEFINITION_ID"].astype(str).str.strip().str.lower()
    else:
        cohort_norm = pd.Series(["target"] * len(concept_rows), index=concept_rows.index)

    present = _presence_mask(concept_rows)
    person_col = "PERSON_ID" if "PERSON_ID" in concept_rows.columns else ("SUBJECT_ID" if "SUBJECT_ID" in concept_rows.columns else None)
    if person_col is None:
        return {
            "target_subject_count": 0,
            "control_subject_count": 0,
            "target_subject_prevalence": 0.0,
            "control_subject_prevalence": 0.0,
            "prevalence_difference_ratio": 0.0,
            "median_first_occurrence": None,
        }

    target_rows = concept_rows[present & cohort_norm.isin(_TARGET_COHORT_LABELS)].copy()
    control_rows = concept_rows[present & cohort_norm.isin(_CONTROL_COHORT_LABELS)].copy()

    target_subject_count = int(target_rows[person_col].nunique()) if not target_rows.empty else 0
    control_subject_count = int(control_rows[person_col].nunique()) if not control_rows.empty else 0

    target_total, control_total = _get_target_control_totals(parquet_data)
    target_prev = (target_subject_count / target_total) if target_total > 0 else 0.0
    control_prev = (control_subject_count / control_total) if control_total > 0 else 0.0

    if control_prev > 0:
        ratio = target_prev / control_prev
    else:
        ratio = 100.0 if target_prev > 0 else 0.0

    median_first_occurrence = None
    if not target_rows.empty and "TIME_TO_EVENT" in target_rows.columns:
        target_rows["__first"] = target_rows["TIME_TO_EVENT"].apply(get_min_time)
        target_rows = target_rows[target_rows["__first"].notna()].copy()
        if not target_rows.empty:
            person_first = target_rows.groupby(person_col)["__first"].min()
            if not person_first.empty:
                median_first_occurrence = float(person_first.median())

    return {
        "target_subject_count": target_subject_count,
        "control_subject_count": control_subject_count,
        "target_subject_prevalence": float(target_prev),
        "control_subject_prevalence": float(control_prev),
        "prevalence_difference_ratio": float(ratio),
        "median_first_occurrence": median_first_occurrence,
    }


def _append_mapping_history(parquet_data: Dict, selected_rows: List[Dict], new_concept_id: str, new_concept_name: str) -> pd.DataFrame:
    history_df = parquet_data.get("complementaryMappingTable", pd.DataFrame())
    if not isinstance(history_df, pd.DataFrame):
        history_df = pd.DataFrame()

    new_rows = []
    for row in selected_rows:
        source_id_raw = row.get("ORIGINAL_CONCEPT_ID") or row.get("CONCEPT_ID") or row.get("_concept_id")
        source_id = _base_concept_id_int(source_id_raw)
        source_name = row.get("CONCEPT_NAME") or str(source_id)
        source_heritage = row.get("HERITAGE")
        new_rows.append(
            {
                "CONCEPT_ID": source_id,
                "CONCEPT_NAME": str(source_name),
                "NEW_CONCEPT_ID": _base_concept_id_int(new_concept_id),
                "NEW_CONCEPT_NAME": str(new_concept_name),
                "TYPE": "manual",
                "HERITAGE": source_heritage,
            }
        )

    new_rows_df = pd.DataFrame(new_rows)
    if history_df.empty:
        merged_history = new_rows_df
    else:
        merged_history = pd.concat([history_df, new_rows_df], ignore_index=True, sort=False)

    parquet_data["complementaryMappingTable"] = merged_history
    return merged_history


def _apply_merge_to_patient_tables(
    parquet_data: Dict,
    selected_rows: List[Dict],
    new_concept_id: str,
    new_concept_name: str,
    new_heritage: str,
    metrics: Dict,
) -> None:
    selected_concept_ids = {
        normalize_concept_id(row.get("CONCEPT_ID") or row.get("_concept_id"))
        for row in selected_rows
        if (row.get("CONCEPT_ID") or row.get("_concept_id")) is not None
    }

    data_patients = parquet_data.get("data_patients", pd.DataFrame())
    if isinstance(data_patients, pd.DataFrame) and not data_patients.empty and "CONCEPT_ID" in data_patients.columns:
        updated_patients = data_patients.copy()
        concept_norm = normalize_concept_id_series(updated_patients["CONCEPT_ID"])
        mask = concept_norm.isin(selected_concept_ids)
        if mask.any():
            updated_patients.loc[mask, "CONCEPT_ID"] = str(new_concept_id)
            if "HERITAGE" in updated_patients.columns:
                updated_patients.loc[mask, "HERITAGE"] = str(new_heritage)
        parquet_data["data_patients"] = updated_patients

    data_features = parquet_data.get("data_features", pd.DataFrame())
    if isinstance(data_features, pd.DataFrame) and not data_features.empty and "CONCEPT_ID" in data_features.columns:
        updated_features = data_features.copy()
        concept_norm = normalize_concept_id_series(updated_features["CONCEPT_ID"])
        drop_mask = concept_norm.isin(selected_concept_ids)

        if "IS_ORDINAL" in updated_features.columns and "ORIGINAL_CONCEPT_ID" in updated_features.columns:
            is_ordinal = updated_features["IS_ORDINAL"].fillna(False).astype(bool)
            original_norm = normalize_concept_id_series(updated_features["ORIGINAL_CONCEPT_ID"])
            drop_mask = drop_mask | (is_ordinal & original_norm.isin(selected_concept_ids))

        updated_features = updated_features[~drop_mask].copy()

        template = selected_rows[0] if selected_rows else {}
        new_feature = {col: pd.NA for col in data_features.columns}
        if "CONCEPT_ID" in new_feature:
            new_feature["CONCEPT_ID"] = str(new_concept_id)
        if "CONCEPT_NAME" in new_feature:
            new_feature["CONCEPT_NAME"] = str(new_concept_name)
        if "HERITAGE" in new_feature:
            new_feature["HERITAGE"] = str(new_heritage)
        if "IS_ORDINAL" in new_feature:
            new_feature["IS_ORDINAL"] = False
        if "ORIGINAL_CONCEPT_ID" in new_feature:
            new_feature["ORIGINAL_CONCEPT_ID"] = str(new_concept_id)
        if "ORDINAL" in new_feature:
            new_feature["ORDINAL"] = 0
        if "ABSTRACTION_LEVEL" in new_feature:
            new_feature["ABSTRACTION_LEVEL"] = -1
        if "TARGET_SUBJECT_PREVALENCE" in new_feature:
            new_feature["TARGET_SUBJECT_PREVALENCE"] = metrics.get("target_subject_prevalence", 0.0)
        if "PREVALENCE_DIFFERENCE_RATIO" in new_feature:
            new_feature["PREVALENCE_DIFFERENCE_RATIO"] = metrics.get("prevalence_difference_ratio", 0.0)
        if "TARGET_SUBJECT_COUNT" in new_feature:
            new_feature["TARGET_SUBJECT_COUNT"] = metrics.get("target_subject_count", 0)
        if "CONTROL_SUBJECT_COUNT" in new_feature:
            new_feature["CONTROL_SUBJECT_COUNT"] = metrics.get("control_subject_count", 0)
        if "MEDIAN_FIRST_OCCURRENCE" in new_feature:
            new_feature["MEDIAN_FIRST_OCCURRENCE"] = metrics.get("median_first_occurrence")

        if "CONCEPT_NAME" in template and "CONCEPT_NAME" in new_feature and (new_feature["CONCEPT_NAME"] is pd.NA or not str(new_feature["CONCEPT_NAME"]).strip()):
            new_feature["CONCEPT_NAME"] = template.get("CONCEPT_NAME")

        updated_features = pd.concat([updated_features, pd.DataFrame([new_feature])], ignore_index=True)
        parquet_data["data_features"] = updated_features


def _sync_data_features_metrics(parquet_data: Dict, new_concept_id: str, new_heritage: str, metrics: Dict) -> None:
    """Update merged concept metrics in data_features using post-merge values."""
    data_features = parquet_data.get("data_features", pd.DataFrame())
    if not isinstance(data_features, pd.DataFrame) or data_features.empty or "CONCEPT_ID" not in data_features.columns:
        return

    updated = data_features.copy()
    concept_norm = normalize_concept_id_series(updated["CONCEPT_ID"])
    mask = concept_norm == normalize_concept_id(new_concept_id)
    if "HERITAGE" in updated.columns and new_heritage:
        mask = mask & (updated["HERITAGE"].astype(str) == str(new_heritage))
    if not mask.any():
        return

    if "TARGET_SUBJECT_PREVALENCE" in updated.columns:
        updated.loc[mask, "TARGET_SUBJECT_PREVALENCE"] = float(metrics.get("target_subject_prevalence", 0.0))
    if "PREVALENCE_DIFFERENCE_RATIO" in updated.columns:
        updated.loc[mask, "PREVALENCE_DIFFERENCE_RATIO"] = float(metrics.get("prevalence_difference_ratio", 0.0))
    if "TARGET_SUBJECT_COUNT" in updated.columns:
        updated.loc[mask, "TARGET_SUBJECT_COUNT"] = int(metrics.get("target_subject_count", 0))
    if "CONTROL_SUBJECT_COUNT" in updated.columns:
        updated.loc[mask, "CONTROL_SUBJECT_COUNT"] = int(metrics.get("control_subject_count", 0))
    if "MEDIAN_FIRST_OCCURRENCE" in updated.columns:
        updated.loc[mask, "MEDIAN_FIRST_OCCURRENCE"] = metrics.get("median_first_occurrence")

    parquet_data["data_features"] = updated


def _build_updated_dashboard_rows(
    dashboard_data: List[Dict],
    selected_rows: List[Dict],
    new_concept_id: str,
    new_concept_name: str,
    new_heritage: str,
    metrics: Dict,
) -> List[Dict]:
    selected_concept_ids = {
        normalize_concept_id(row.get("CONCEPT_ID") or row.get("_concept_id"))
        for row in selected_rows
        if (row.get("CONCEPT_ID") or row.get("_concept_id")) is not None
    }

    kept_rows: List[Dict] = []
    for row in dashboard_data:
        row_copy = row.copy()
        concept_norm = normalize_concept_id(row_copy.get("CONCEPT_ID") or row_copy.get("_concept_id"))
        if concept_norm in selected_concept_ids:
            continue

        if _is_ordinal(row_copy):
            original_norm = normalize_concept_id(row_copy.get("ORIGINAL_CONCEPT_ID"))
            if original_norm in selected_concept_ids:
                continue

        row_copy["_map"] = False
        kept_rows.append(row_copy)

    def _prev_score(row: Dict) -> float:
        pct = row.get("TARGET_SUBJECT_PREVALENCE_PCT")
        if pct is not None:
            return _safe_float(pct, default=0.0)
        return _safe_float(row.get("TARGET_SUBJECT_PREVALENCE"), default=0.0) * 100.0

    primary_row = max(selected_rows, key=_prev_score)
    merged_row = primary_row.copy()

    ratio_value = _safe_float(metrics.get("prevalence_difference_ratio"), default=0.0)
    ratio_display = ratio_value
    if not np.isfinite(ratio_display) or ratio_display > 100.0:
        ratio_display = 100.0

    merged_row.update(
        {
            "CONCEPT_ID": str(new_concept_id),
            "_concept_id": str(new_concept_id),
            "CONCEPT_NAME": str(new_concept_name),
            "HERITAGE": str(new_heritage),
            "IS_ORDINAL": False,
            "ORIGINAL_CONCEPT_ID": str(new_concept_id),
            "ORDINAL": 0,
            "TARGET_SUBJECT_PREVALENCE": float(metrics.get("target_subject_prevalence", 0.0)),
            "TARGET_SUBJECT_PREVALENCE_PCT": float(metrics.get("target_subject_prevalence", 0.0)) * 100.0,
            "PREVALENCE_DIFFERENCE_RATIO": ratio_value,
            "PREVALENCE_DIFFERENCE_RATIO_DISPLAY": ratio_display,
            "MEDIAN_FIRST_OCCURRENCE": metrics.get("median_first_occurrence"),
            "_show": True,
            "_map": False,
        }
    )

    kept_rows.append(merged_row)
    return kept_rows


def _save_patient_study_state_copy(
    parquet_data: Dict,
    source_study_dir: Path,
    output_dir_raw: str,
) -> Tuple[Path, int]:
    output_dir = Path(output_dir_raw).expanduser()
    if not output_dir.is_absolute():
        output_dir = (Path.cwd() / output_dir).resolve()
    else:
        output_dir = output_dir.resolve()

    source_study_dir = source_study_dir.resolve()
    if output_dir == source_study_dir:
        raise ValueError("Output path must be different from the source study folder.")

    if output_dir.exists() and any(output_dir.iterdir()):
        raise ValueError("Output path already exists and is not empty.")
    output_dir.mkdir(parents=True, exist_ok=True)

    component_keys = [
        "data_initial",
        "data_person",
        "data_patients",
        "data_features",
        "complementaryMappingTable",
        "concepts_concept_ancestor",
    ]
    written_count = 0
    written_names = set()
    for key in component_keys:
        frame = parquet_data.get(key)
        if isinstance(frame, pd.DataFrame):
            target_path = output_dir / f"{key}.parquet"
            frame.to_parquet(target_path, index=False)
            written_count += 1
            written_names.add(target_path.name)

    # Copy any additional parquet artifacts that may exist in the source study.
    if source_study_dir.exists():
        for source_file in source_study_dir.glob("*.parquet"):
            if source_file.name in written_names:
                continue
            shutil.copy2(source_file, output_dir / source_file.name)
            written_count += 1

    metadata = parquet_data.get("_metadata")
    if isinstance(metadata, dict):
        with open(output_dir / "metadata.json", "w", encoding="utf-8") as f:
            json.dump(metadata, f, indent=2)
    elif (source_study_dir / "metadata.json").exists():
        shutil.copy2(source_study_dir / "metadata.json", output_dir / "metadata.json")

    if (source_study_dir / "desc.txt").exists():
        shutil.copy2(source_study_dir / "desc.txt", output_dir / "desc.txt")

    return output_dir, written_count


def register_mappings_callbacks(
    app,
    *,
    logger_obj,
    data_dir: Path,
    cache_store,
    loaded_parquet_data_store: Dict[str, Dict[str, pd.DataFrame]],
) -> None:
    """Register mappings tab callbacks."""
    logger = logger_obj
    DATA_DIR = data_dir
    cache = cache_store
    loaded_parquet_data = loaded_parquet_data_store

    @app.callback(
        [Output("mappings-candidate-table", "rowData"),
         Output("merge-new-heritage", "options"),
         Output("mappings-history-table", "rowData"),
         Output("mappings-history-count", "children"),
         Output("mappings-candidate-section", "style"),
         Output("mappings-summary-note", "style")],
        [Input("selected-study-store", "data"),
         Input("dashboard-data-store", "data"),
         Input("data-mode-store", "data")],
        prevent_initial_call=False,
    )
    def refresh_mappings_tab(
        selected_study: Optional[str],
        dashboard_data: Optional[List[Dict]],
        data_mode: Optional[str],
    ):
        is_patient_mode = (data_mode == "patient")
        candidate_style = {"display": "block" if is_patient_mode else "none"}
        summary_note_style = {"display": "none" if is_patient_mode else "block"}

        candidate_rows = _build_candidate_rows(dashboard_data if is_patient_mode else [])
        heritage_options = _build_heritage_options(dashboard_data if is_patient_mode else [])

        history_rows: List[Dict] = []
        if selected_study:
            parquet_data = loaded_parquet_data.get(selected_study)
            if parquet_data is None:
                parquet_data = get_or_load_parquet_data(selected_study, DATA_DIR, cache)
                if parquet_data is not None:
                    loaded_parquet_data[selected_study] = parquet_data
            if parquet_data is not None:
                history_df = parquet_data.get("complementaryMappingTable", pd.DataFrame())
                if isinstance(history_df, pd.DataFrame) and not history_df.empty:
                    history_rows = convert_list_columns_to_strings(history_df.copy()).to_dict("records")

        history_count = f"Showing {len(history_rows)} mapping entries"
        return candidate_rows, heritage_options, history_rows, history_count, candidate_style, summary_note_style

    @app.callback(
        [Output("hierarchy-suggestion-table", "rowData"),
         Output("hierarchy-suggestion-note", "children")],
        [Input("selected-study-store", "data"),
         Input("data-mode-store", "data"),
         Input("dashboard-data-store", "data"),
         Input("refresh-hierarchy-suggestions-btn", "n_clicks")],
        [State("hierarchy-min-depth-input", "value"),
         State("hierarchy-only-minors-checkbox", "value")],
        prevent_initial_call=False,
    )
    def refresh_hierarchy_suggestions(
        selected_study: Optional[str],
        data_mode: Optional[str],
        dashboard_data: Optional[List[Dict]],
        refresh_clicks: Optional[int],
        min_depth_value,
        hierarchy_flags: Optional[List[str]],
    ):
        ctx = callback_context
        trigger = getattr(ctx, "triggered_id", None)
        if trigger is None and ctx.triggered:
            trigger = str(ctx.triggered[0].get("prop_id", "")).split(".")[0]

        if data_mode != "patient":
            return [], "Hierarchy suggestions are available only in patient-level mode."
        if not selected_study:
            return [], "Select a study to generate hierarchy suggestions."
        if trigger != "refresh-hierarchy-suggestions-btn":
            return [], "Set hierarchy parameters, then click 'Update Hierarchy Suggestions'."
        if not refresh_clicks:
            return [], "Set hierarchy parameters, then click 'Update Hierarchy Suggestions'."

        parquet_data = loaded_parquet_data.get(selected_study)
        if parquet_data is None:
            parquet_data = get_or_load_parquet_data(selected_study, DATA_DIR, cache)
            if parquet_data is not None:
                loaded_parquet_data[selected_study] = parquet_data
        if parquet_data is None:
            return [], "Unable to load study data."

        concept_ancestor_df = _load_concept_ancestor_table(parquet_data, selected_study, DATA_DIR)
        if concept_ancestor_df.empty:
            return [], "No hierarchy source file found (concepts_concept_ancestor.parquet)."

        min_depth = int(max(1, _to_float(min_depth_value, default=1)))
        allow_only_minors = isinstance(hierarchy_flags, list) and ("only_minors" in hierarchy_flags)

        suggestions = _build_hierarchy_suggestions(
            dashboard_data=dashboard_data,
            concept_ancestor_df=concept_ancestor_df,
            min_depth=min_depth,
            allow_only_minors=allow_only_minors,
        )
        note = f"Showing {len(suggestions)} hierarchy suggestion(s) (min depth: {min_depth}, only minors: {allow_only_minors})."
        return suggestions, note

    @app.callback(
        [Output("correlation-suggestion-table", "rowData"),
         Output("correlation-suggestion-note", "children")],
        [Input("selected-study-store", "data"),
         Input("data-mode-store", "data"),
         Input("dashboard-data-store", "data"),
         Input("refresh-correlation-suggestions-btn", "n_clicks")],
        [State("correlation-min-input", "value"),
         State("correlation-max-days-input", "value"),
         State("correlation-heritage-drift-checkbox", "value")],
        prevent_initial_call=False,
    )
    def refresh_correlation_suggestions(
        selected_study: Optional[str],
        data_mode: Optional[str],
        dashboard_data: Optional[List[Dict]],
        refresh_clicks: Optional[int],
        min_corr_value,
        max_days_value,
        correlation_flags: Optional[List[str]],
    ):
        ctx = callback_context
        trigger = getattr(ctx, "triggered_id", None)
        if trigger is None and ctx.triggered:
            trigger = str(ctx.triggered[0].get("prop_id", "")).split(".")[0]

        if data_mode != "patient":
            return [], "Correlation suggestions are available only in patient-level mode."
        if not selected_study:
            return [], "Select a study to generate correlation suggestions."
        if trigger != "refresh-correlation-suggestions-btn":
            return [], "Set correlation parameters, then click 'Update Correlation Suggestions'."
        if not refresh_clicks:
            return [], "Set correlation parameters, then click 'Update Correlation Suggestions'."

        parquet_data = loaded_parquet_data.get(selected_study)
        if parquet_data is None:
            parquet_data = get_or_load_parquet_data(selected_study, DATA_DIR, cache)
            if parquet_data is not None:
                loaded_parquet_data[selected_study] = parquet_data
        if parquet_data is None:
            return [], "Unable to load study data."

        min_corr = max(0.0, min(1.0, _to_float(min_corr_value, default=0.7)))
        max_days = max(0.0, _to_float(max_days_value, default=1.0))
        allow_drift = isinstance(correlation_flags, list) and ("allow_drift" in correlation_flags)

        suggestions = _build_correlation_suggestions(
            parquet_data=parquet_data,
            dashboard_data=dashboard_data,
            min_correlation=min_corr,
            max_days_inbetween=max_days,
            heritage_drift_allowed=allow_drift,
        )
        note = f"Showing {len(suggestions)} correlation suggestion(s) (min corr: {min_corr:.2f}, max days: {max_days:.2f}, heritage drift: {allow_drift})."
        return suggestions, note

    @app.callback(
        [Output("mappings-merge-form", "style"),
         Output("merge-new-concept-id", "value"),
         Output("merge-new-concept-name", "value"),
         Output("merge-new-heritage", "value"),
         Output("mappings-action-status", "children"),
         Output("mappings-action-status", "style")],
        Input("open-merge-mapping-btn", "n_clicks"),
        [State("mappings-candidate-table", "rowData"),
         State("data-mode-store", "data")],
        prevent_initial_call=True,
    )
    def open_merge_form(
        n_clicks: Optional[int],
        candidate_rows: Optional[List[Dict]],
        data_mode: Optional[str],
    ):
        default_style = {"fontSize": "13px", "marginBottom": "10px", "color": "#4a4a4a"}
        if not n_clicks:
            return no_update, no_update, no_update, no_update, no_update, no_update

        if data_mode != "patient":
            return (
                {"display": "none"},
                "",
                "",
                None,
                "Concept merge is available only in patient-level mode.",
                {**default_style, "color": "#a94442"},
            )

        selected_rows = _selected_map_rows(candidate_rows)
        selected_main_rows = [row for row in selected_rows if not _is_ordinal(row)]

        if len(selected_main_rows) < 2:
            return (
                {"display": "none"},
                "",
                "",
                None,
                "Select at least two non-ordinal concepts in the Map column.",
                {**default_style, "color": "#a94442"},
            )

        def _score(row: Dict) -> float:
            pct = row.get("TARGET_SUBJECT_PREVALENCE_PCT")
            if pct is not None:
                return _safe_float(pct, default=0.0)
            return _safe_float(row.get("TARGET_SUBJECT_PREVALENCE"), default=0.0) * 100.0

        primary = max(selected_main_rows, key=_score)
        prefill_id_int = _base_concept_id_int(primary.get("CONCEPT_ID") or primary.get("_concept_id"))
        prefill_id = str(prefill_id_int) if prefill_id_int is not None else str(primary.get("CONCEPT_ID") or primary.get("_concept_id") or "")
        prefill_name = str(primary.get("CONCEPT_NAME") or "")
        prefill_heritage = primary.get("HERITAGE")

        return (
            {
                "display": "block",
                "border": "1px solid #d9e2ec",
                "borderRadius": "8px",
                "padding": "12px",
                "backgroundColor": "#f8fbff",
                "marginBottom": "12px",
            },
            prefill_id,
            prefill_name,
            prefill_heritage,
            f"Prepared merge for {len(selected_main_rows)} concepts. Review and apply.",
            {**default_style, "color": "#3c763d"},
        )

    @app.callback(
        [Output("mappings-action-status", "children", allow_duplicate=True),
         Output("mappings-action-status", "style", allow_duplicate=True),
         Output("mappings-merge-form", "style", allow_duplicate=True),
         Output("mappings-candidate-table", "rowData", allow_duplicate=True),
         Output("mappings-history-table", "rowData", allow_duplicate=True),
         Output("mappings-history-count", "children", allow_duplicate=True),
         Output("dashboard-data-store", "data", allow_duplicate=True),
         Output("dashboard-table", "rowData", allow_duplicate=True),
         Output("plots-update-trigger-store", "data", allow_duplicate=True),
         Output("clustering-results-store", "data", allow_duplicate=True),
         Output("clustering-concepts-store", "data", allow_duplicate=True),
         Output("cluster-view-store", "data", allow_duplicate=True)],
        Input("execute-merge-mapping-btn", "n_clicks"),
        [State("selected-study-store", "data"),
         State("data-mode-store", "data"),
         State("mappings-candidate-table", "rowData"),
         State("dashboard-data-store", "data"),
         State("merge-new-concept-id", "value"),
         State("merge-new-concept-name", "value"),
         State("merge-new-heritage", "value")],
        prevent_initial_call='initial_duplicate',
    )
    def execute_merge_mapping(
        n_clicks: Optional[int],
        selected_study: Optional[str],
        data_mode: Optional[str],
        candidate_rows: Optional[List[Dict]],
        dashboard_data: Optional[List[Dict]],
        new_concept_id: Optional[str],
        new_concept_name: Optional[str],
        new_heritage: Optional[str],
    ):
        default_style = {"fontSize": "13px", "marginBottom": "10px", "color": "#4a4a4a"}
        if not n_clicks:
            return (no_update,) * 12

        if data_mode != "patient":
            return (
                "Concept merge is available only in patient-level mode.",
                {**default_style, "color": "#a94442"},
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
            )

        if not selected_study:
            return (
                "No study selected.",
                {**default_style, "color": "#a94442"},
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
            )

        selected_rows = [row for row in _selected_map_rows(candidate_rows) if not _is_ordinal(row)]
        if len(selected_rows) < 2:
            return (
                "Select at least two non-ordinal concepts before applying merge.",
                {**default_style, "color": "#a94442"},
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
            )

        new_concept_id_str = str(new_concept_id).strip() if new_concept_id is not None else ""
        new_concept_name_str = str(new_concept_name).strip() if new_concept_name is not None else ""
        new_heritage_str = str(new_heritage).strip() if new_heritage is not None else ""

        if not new_concept_id_str or not new_concept_name_str or not new_heritage_str:
            return (
                "New concept ID, name, and heritage are required.",
                {**default_style, "color": "#a94442"},
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
            )

        new_concept_id_int = _base_concept_id_int(new_concept_id_str)
        if new_concept_id_int is None:
            return (
                "New concept ID must be an integer-like concept ID.",
                {**default_style, "color": "#a94442"},
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
            )
        new_concept_id_str = str(new_concept_id_int)

        invalid_sources = []
        for row in selected_rows:
            source_id_raw = row.get("ORIGINAL_CONCEPT_ID") or row.get("CONCEPT_ID") or row.get("_concept_id")
            if _base_concept_id_int(source_id_raw) is None:
                invalid_sources.append(str(row.get("CONCEPT_NAME") or source_id_raw))
        if invalid_sources:
            return (
                f"Selected concepts contain non-integer IDs: {', '.join(invalid_sources[:3])}",
                {**default_style, "color": "#a94442"},
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
            )

        selected_ids_norm = {
            normalize_concept_id(row.get("CONCEPT_ID") or row.get("_concept_id"))
            for row in selected_rows
        }
        existing_rows = dashboard_data if isinstance(dashboard_data, list) else []
        existing_non_selected = {
            normalize_concept_id(row.get("CONCEPT_ID") or row.get("_concept_id"))
            for row in existing_rows
            if normalize_concept_id(row.get("CONCEPT_ID") or row.get("_concept_id")) not in selected_ids_norm
        }
        if normalize_concept_id(new_concept_id_str) in existing_non_selected:
            return (
                f"Concept ID {new_concept_id_str} already exists. Choose a different ID.",
                {**default_style, "color": "#a94442"},
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
            )

        parquet_data = loaded_parquet_data.get(selected_study)
        if parquet_data is None:
            parquet_data = get_or_load_parquet_data(selected_study, DATA_DIR, cache)
            if parquet_data is not None:
                loaded_parquet_data[selected_study] = parquet_data
        if parquet_data is None:
            return (
                "Unable to load study data for merge.",
                {**default_style, "color": "#a94442"},
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
            )

        metrics = _compute_concept_metrics(parquet_data, new_concept_id_str, new_heritage_str)
        _apply_merge_to_patient_tables(
            parquet_data,
            selected_rows,
            new_concept_id_str,
            new_concept_name_str,
            new_heritage_str,
            metrics,
        )
        metrics = _compute_concept_metrics(parquet_data, new_concept_id_str, new_heritage_str)
        _sync_data_features_metrics(parquet_data, new_concept_id_str, new_heritage_str, metrics)

        updated_dashboard = _build_updated_dashboard_rows(
            existing_rows,
            selected_rows,
            new_concept_id_str,
            new_concept_name_str,
            new_heritage_str,
            metrics,
        )

        history_df = _append_mapping_history(parquet_data, selected_rows, new_concept_id_str, new_concept_name_str)
        history_rows = convert_list_columns_to_strings(history_df.copy()).to_dict("records") if not history_df.empty else []

        loaded_parquet_data[selected_study] = parquet_data

        candidate_rows_updated = _build_candidate_rows(updated_dashboard)
        history_count_text = f"Showing {len(history_rows)} mapping entries"
        trigger = time.time()

        logger.info(
            "Applied manual merge in patient mode for study %s: %s -> %s",
            selected_study,
            ", ".join(sorted(selected_ids_norm)),
            new_concept_id_str,
        )

        return (
            f"Merged {len(selected_rows)} concepts into '{new_concept_name_str}' ({new_concept_id_str}) in heritage '{new_heritage_str}'. Clustering was reset.",
            {**default_style, "color": "#3c763d"},
            {"display": "none"},
            candidate_rows_updated,
            history_rows,
            history_count_text,
            updated_dashboard,
            updated_dashboard,
            trigger,
            None,
            None,
            "all",
        )

    @app.callback(
        [Output("mappings-action-status", "children", allow_duplicate=True),
         Output("mappings-action-status", "style", allow_duplicate=True),
         Output("hierarchy-suggestion-status", "children"),
         Output("correlation-suggestion-status", "children"),
         Output("mappings-candidate-table", "rowData", allow_duplicate=True),
         Output("mappings-history-table", "rowData", allow_duplicate=True),
         Output("mappings-history-count", "children", allow_duplicate=True),
         Output("dashboard-data-store", "data", allow_duplicate=True),
         Output("dashboard-table", "rowData", allow_duplicate=True),
         Output("plots-update-trigger-store", "data", allow_duplicate=True),
         Output("clustering-results-store", "data", allow_duplicate=True),
         Output("clustering-concepts-store", "data", allow_duplicate=True),
         Output("cluster-view-store", "data", allow_duplicate=True)],
        [Input("execute-hierarchy-suggestions-btn", "n_clicks"),
         Input("execute-correlation-suggestions-btn", "n_clicks")],
        [State("selected-study-store", "data"),
         State("data-mode-store", "data"),
         State("dashboard-data-store", "data"),
         State("hierarchy-suggestion-table", "rowData"),
         State("correlation-suggestion-table", "rowData")],
        prevent_initial_call="initial_duplicate",
    )
    def execute_suggestion_merges(
        hierarchy_clicks: Optional[int],
        correlation_clicks: Optional[int],
        selected_study: Optional[str],
        data_mode: Optional[str],
        dashboard_data: Optional[List[Dict]],
        hierarchy_rows: Optional[List[Dict]],
        correlation_rows: Optional[List[Dict]],
    ):
        default_style = {"fontSize": "13px", "marginBottom": "10px", "color": "#4a4a4a"}
        ctx = callback_context
        trigger = getattr(ctx, "triggered_id", None)
        if trigger is None and ctx.triggered:
            trigger = str(ctx.triggered[0].get("prop_id", "")).split(".")[0]

        if trigger not in {"execute-hierarchy-suggestions-btn", "execute-correlation-suggestions-btn"}:
            return (no_update,) * 13

        is_hierarchy = trigger == "execute-hierarchy-suggestions-btn"
        suggestion_rows = hierarchy_rows if is_hierarchy else correlation_rows
        suggestion_type = "hierarchy" if is_hierarchy else "correlation"

        if (is_hierarchy and not hierarchy_clicks) or ((not is_hierarchy) and not correlation_clicks):
            return (no_update,) * 13

        if data_mode != "patient":
            tab_msg = "Suggestion merges are available only in patient-level mode."
            return (
                tab_msg,
                {**default_style, "color": "#a94442"},
                tab_msg if is_hierarchy else no_update,
                tab_msg if not is_hierarchy else no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
            )

        if not selected_study:
            tab_msg = "No study selected."
            return (
                tab_msg,
                {**default_style, "color": "#a94442"},
                tab_msg if is_hierarchy else no_update,
                tab_msg if not is_hierarchy else no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
            )

        selected_suggestions = [
            row for row in (suggestion_rows or [])
            if bool(row.get("_map", False))
        ]
        if not selected_suggestions:
            tab_msg = f"Select at least one {suggestion_type} suggestion in the Map column."
            return (
                tab_msg,
                {**default_style, "color": "#a94442"},
                tab_msg if is_hierarchy else no_update,
                tab_msg if not is_hierarchy else no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
            )

        parquet_data = loaded_parquet_data.get(selected_study)
        if parquet_data is None:
            parquet_data = get_or_load_parquet_data(selected_study, DATA_DIR, cache)
            if parquet_data is not None:
                loaded_parquet_data[selected_study] = parquet_data
        if parquet_data is None:
            tab_msg = "Unable to load study data for suggestion merges."
            return (
                tab_msg,
                {**default_style, "color": "#a94442"},
                tab_msg if is_hierarchy else no_update,
                tab_msg if not is_hierarchy else no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
            )

        updated_dashboard = list(dashboard_data or [])
        applied_count = 0
        skipped_count = 0
        for suggestion in selected_suggestions:
            source_ids = _parse_source_concept_ids(suggestion.get("SOURCE_CONCEPT_IDS"))
            selected_rows = _resolve_suggestion_source_rows(updated_dashboard, source_ids)
            if len(selected_rows) < 2:
                skipped_count += 1
                continue

            default_name, default_heritage = _derive_primary_merge_defaults(selected_rows)
            new_concept_id_raw = suggestion.get("NEW_CONCEPT_ID")
            new_concept_name = str(suggestion.get("NEW_CONCEPT_NAME") or default_name).strip()
            new_heritage = str(suggestion.get("HERITAGE") or default_heritage).strip()
            new_concept_id_int = _base_concept_id_int(new_concept_id_raw)
            if new_concept_id_int is None or not new_concept_name or not new_heritage:
                skipped_count += 1
                continue
            new_concept_id_str = str(new_concept_id_int)

            selected_ids_norm = {
                normalize_concept_id(row.get("CONCEPT_ID") or row.get("_concept_id"))
                for row in selected_rows
            }
            existing_non_selected = {
                normalize_concept_id(row.get("CONCEPT_ID") or row.get("_concept_id"))
                for row in updated_dashboard
                if normalize_concept_id(row.get("CONCEPT_ID") or row.get("_concept_id")) not in selected_ids_norm
            }
            if normalize_concept_id(new_concept_id_str) in existing_non_selected:
                skipped_count += 1
                continue

            updated_dashboard = _execute_single_merge(
                parquet_data,
                updated_dashboard,
                selected_rows,
                new_concept_id_str,
                new_concept_name,
                new_heritage,
            )
            applied_count += 1

        if applied_count == 0:
            tab_msg = f"No {suggestion_type} suggestions were applied (all selected suggestions were invalid or unavailable)."
            return (
                tab_msg,
                {**default_style, "color": "#a94442"},
                tab_msg if is_hierarchy else no_update,
                tab_msg if not is_hierarchy else no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
                no_update,
            )

        history_df = parquet_data.get("complementaryMappingTable", pd.DataFrame())
        history_rows = convert_list_columns_to_strings(history_df.copy()).to_dict("records") if isinstance(history_df, pd.DataFrame) and not history_df.empty else []
        loaded_parquet_data[selected_study] = parquet_data

        candidate_rows_updated = _build_candidate_rows(updated_dashboard)
        history_count_text = f"Showing {len(history_rows)} mapping entries"
        now_trigger = time.time()
        base_msg = (
            f"Applied {applied_count} {suggestion_type} suggestion merge(s)"
            + (f"; skipped {skipped_count}." if skipped_count > 0 else ".")
            + " Clustering was reset."
        )

        logger.info(
            "Applied %d %s suggestion merge(s) for study %s (skipped=%d)",
            applied_count,
            suggestion_type,
            selected_study,
            skipped_count,
        )

        return (
            base_msg,
            {**default_style, "color": "#3c763d"},
            base_msg if is_hierarchy else no_update,
            base_msg if not is_hierarchy else no_update,
            candidate_rows_updated,
            history_rows,
            history_count_text,
            updated_dashboard,
            updated_dashboard,
            now_trigger,
            None,
            None,
            "all",
        )

    @app.callback(
        [Output("mappings-save-state-status", "children"),
         Output("mappings-save-state-status", "style")],
        Input("save-study-state-btn", "n_clicks"),
        [State("selected-study-store", "data"),
         State("data-mode-store", "data"),
         State("save-study-path-input", "value")],
        prevent_initial_call=True,
    )
    def save_study_state_copy(
        n_clicks: Optional[int],
        selected_study: Optional[str],
        data_mode: Optional[str],
        output_path: Optional[str],
    ):
        default_style = {"fontSize": "13px", "marginBottom": "8px", "color": "#4a4a4a"}
        if not n_clicks:
            return no_update, no_update

        if data_mode != "patient":
            return (
                "Save Study State is available only in patient-level mode.",
                {**default_style, "color": "#a94442"},
            )
        if not selected_study:
            return ("No study selected.", {**default_style, "color": "#a94442"})

        output_path_str = str(output_path or "").strip()
        if not output_path_str:
            return (
                "Provide an output folder path before saving.",
                {**default_style, "color": "#a94442"},
            )

        parquet_data = loaded_parquet_data.get(selected_study)
        if parquet_data is None:
            parquet_data = get_or_load_parquet_data(selected_study, DATA_DIR, cache)
            if parquet_data is not None:
                loaded_parquet_data[selected_study] = parquet_data
        if parquet_data is None:
            return (
                "Unable to load study data.",
                {**default_style, "color": "#a94442"},
            )

        source_study_dir = DATA_DIR / selected_study
        try:
            output_dir, written_count = _save_patient_study_state_copy(
                parquet_data=parquet_data,
                source_study_dir=source_study_dir,
                output_dir_raw=output_path_str,
            )
        except Exception as e:
            return (
                f"Failed to save study state: {e}",
                {**default_style, "color": "#a94442"},
            )

        logger.info(
            "Saved patient study state copy for %s to %s (%d files)",
            selected_study,
            str(output_dir),
            written_count,
        )
        return (
            f"Saved study state copy to {output_dir} ({written_count} file(s) written).",
            {**default_style, "color": "#3c763d"},
        )
