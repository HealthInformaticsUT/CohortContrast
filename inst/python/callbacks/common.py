"""Shared callback helpers used across GUI callback modules."""

from pathlib import Path
from typing import Callable, Dict, List, Optional, Set, Tuple

import pandas as pd

from data.cache import get_or_load_parquet_data


def scoped_study_key(selected_study: str, session_id: Optional[str]) -> str:
    """Build a per-session study key for state stores."""
    session_key = session_id or "anonymous-session"
    return f"{session_key}::{selected_study}"


def load_study_parquet_data(
    selected_study: Optional[str],
    data_dir: Path,
    cache_store,
    loaded_parquet_data_store: Dict[str, Dict[str, pd.DataFrame]],
) -> Optional[Dict[str, pd.DataFrame]]:
    """
    Load study parquet payload from centralized cache and mirror in process memory.
    """
    if not selected_study:
        return None
    parquet_data = get_or_load_parquet_data(selected_study, data_dir, cache_store)
    if parquet_data is None:
        return None
    loaded_parquet_data_store[selected_study] = parquet_data
    return parquet_data


def extract_active_concepts_and_heritage_groups(
    row_data: Optional[List[Dict]],
    normalize_concept_id_fn: Callable,
) -> Tuple[Set[str], Dict[str, List[Dict]]]:
    """Collect active concept IDs and preserve row ordering per heritage."""
    active_concepts: Set[str] = set()
    heritage_groups_order: Dict[str, List[Dict]] = {}

    if not row_data:
        return active_concepts, heritage_groups_order

    for row in row_data:
        if not row.get("_show", False):
            continue
        concept_id = row.get("_concept_id") or row.get("CONCEPT_ID")
        if not concept_id:
            continue
        norm_id = normalize_concept_id_fn(concept_id)
        active_concepts.add(norm_id)

        heritage = row.get("HERITAGE", "unknown")
        if heritage not in heritage_groups_order:
            heritage_groups_order[heritage] = []
        heritage_groups_order[heritage].append(row)

    return active_concepts, heritage_groups_order
