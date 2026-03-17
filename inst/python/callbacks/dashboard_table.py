"""Dashboard table callback registration."""

from typing import Dict, List, Optional, Tuple

import pandas as pd
from dash import Input, Output, State, callback_context, no_update

from callbacks.session_state import touch_session


def _scoped_study_key(selected_study: str, session_id: Optional[str]) -> str:
    session_key = session_id or "anonymous-session"
    return f"{session_key}::{selected_study}"


def register_dashboard_table_callbacks(
    app,
    *,
    dashboard_show_state_store: Dict[str, Dict[int, bool]],
    normalize_concept_id_fn,
    apply_filters_to_data_fn,
    filter_ordinal_concepts_for_filtered_mains_fn,
    show_ordinals_for_active_mains_fn,
    hide_ordinals_for_active_mains_fn,
) -> None:
    """Register dashboard table callbacks for filter and table-selection actions."""
    dashboard_show_state = dashboard_show_state_store
    _normalize_concept_id = normalize_concept_id_fn
    _apply_filters_to_data = apply_filters_to_data_fn
    _filter_ordinal_concepts_for_filtered_mains = filter_ordinal_concepts_for_filtered_mains_fn
    _show_ordinals_for_active_mains = show_ordinals_for_active_mains_fn
    _hide_ordinals_for_active_mains = hide_ordinals_for_active_mains_fn

    @app.callback(
        [Output("dashboard-table", "rowData"),
         Output("plots-update-trigger-store", "data")],
        [Input("apply-filters-btn", "n_clicks"),
         Input("apply-table-selection-btn", "n_clicks")],
        [State("dashboard-table", "rowData"),
         State("dashboard-data-store", "data"),
         State("target-prevalence-range", "value"),
         State("ratio-range", "value"),
         State("heritage-selection-store", "data"),
         State("selected-study-store", "data"),
         State("show-ordinal-checkbox", "value"),
         State("session-id-store", "data"),
         State("plots-update-trigger-store", "data")],
        background=True,
        running=[
            (Output("loading-message-store", "data"), "Applying filters to concepts...", "")
        ],
        prevent_initial_call=True
    )
    def update_dashboard_table(
        apply_n_clicks: Optional[int],
        apply_table_n_clicks: Optional[int],
        row_data: Optional[List[Dict]],
        dashboard_data: Optional[List[Dict]],
        target_prevalence_range: List[float],
        ratio_range: List[float],
        selected_heritages: List[str],
        selected_study: Optional[str],
        show_ordinal_checkbox: Optional[List[str]],
        session_id: Optional[str],
        current_trigger: Optional[int],
    ) -> Tuple[List[Dict], Optional[int]]:
        ctx = callback_context
        if not ctx.triggered:
            return row_data or [], no_update

        touch_session(session_id, dashboard_show_state)

        trigger_id = ctx.triggered[0]["prop_id"].split(".")[0]

        if trigger_id == "apply-filters-btn" and apply_n_clicks is not None and apply_n_clicks > 0:
            if not dashboard_data or not selected_study:
                return row_data or [], no_update
            scoped_study_key = _scoped_study_key(selected_study, session_id)

            updated_data = [row.copy() for row in dashboard_data]

            if not selected_heritages:
                all_heritages = set()
                for row in updated_data:
                    heritage = row.get("HERITAGE")
                    if heritage is not None and pd.notna(heritage) and str(heritage).strip():
                        all_heritages.add(str(heritage))
                selected_heritages = list(all_heritages)

            selected_heritages_set = set(selected_heritages)

            target_min = target_prevalence_range[0] if target_prevalence_range and len(target_prevalence_range) >= 1 else 0
            target_max = target_prevalence_range[1] if target_prevalence_range and len(target_prevalence_range) >= 2 else 100
            ratio_min = ratio_range[0] if ratio_range and len(ratio_range) >= 1 else 0
            ratio_max = ratio_range[1] if ratio_range and len(ratio_range) >= 2 else 100

            _apply_filters_to_data(
                updated_data, target_min, target_max, ratio_min, ratio_max,
                selected_heritages_set, scoped_study_key
            )

            _filter_ordinal_concepts_for_filtered_mains(updated_data, scoped_study_key)

            show_ordinals = show_ordinal_checkbox and "show_ordinals" in show_ordinal_checkbox
            if show_ordinals:
                _show_ordinals_for_active_mains(updated_data, scoped_study_key)
            else:
                _hide_ordinals_for_active_mains(updated_data, scoped_study_key)

            import time
            return updated_data, time.time()

        if trigger_id == "apply-table-selection-btn" and apply_table_n_clicks is not None and apply_table_n_clicks > 0:
            if not row_data or not selected_study:
                return row_data or [], no_update
            scoped_study_key = _scoped_study_key(selected_study, session_id)

            updated_row_data = [row.copy() for row in row_data]

            main_concept_lookup = {}
            for idx, row in enumerate(updated_row_data):
                if row.get("IS_ORDINAL", False):
                    continue
                concept_id = row.get("CONCEPT_ID")
                heritage = row.get("HERITAGE")
                if concept_id is None:
                    continue
                key = (_normalize_concept_id(concept_id), heritage)
                main_concept_lookup[key] = idx

            for row in updated_row_data:
                if not row.get("IS_ORDINAL", False) or not row.get("_show", False):
                    continue
                original_concept_id = row.get("ORIGINAL_CONCEPT_ID")
                heritage = row.get("HERITAGE")
                if original_concept_id is None:
                    continue
                key = (_normalize_concept_id(original_concept_id), heritage)
                main_idx = main_concept_lookup.get(key)
                if main_idx is not None and not updated_row_data[main_idx].get("_show", False):
                    updated_row_data[main_idx]["_show"] = True

            if scoped_study_key not in dashboard_show_state:
                dashboard_show_state[scoped_study_key] = {}
            for row in updated_row_data:
                concept_id = row.get("_concept_id")
                if concept_id is not None:
                    dashboard_show_state[scoped_study_key][concept_id] = bool(row.get("_show", False))

            import time
            return updated_row_data, time.time()

        return row_data or [], no_update
