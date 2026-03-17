"""Data loading callback registration for ContrastViewer."""

from pathlib import Path
from typing import Callable, Dict, List, Optional, Tuple

import numpy as np
import pandas as pd
from dash import Input, Output, State, dcc, html
from dash_ag_grid import AgGrid

from callbacks.session_state import touch_session
from data.cache import get_or_load_parquet_data
from data.processing import calculate_concept_metrics_from_patients, create_ordinal_concepts
from layout.dashboard_tab import build_dashboard_tab, build_dashboard_table
from utils.helpers import convert_list_columns_to_strings, format_column_name


def _merge_median_column(df: pd.DataFrame) -> pd.DataFrame:
    if "MEDIAN_FIRST_OCCURRENCE_median" in df.columns:
        df["MEDIAN_FIRST_OCCURRENCE"] = df["MEDIAN_FIRST_OCCURRENCE_median"].fillna(
            df.get("MEDIAN_FIRST_OCCURRENCE")
        )
        df = df.drop(columns=["MEDIAN_FIRST_OCCURRENCE_median"])
    return df


def _scoped_study_key(selected_study: str, session_id: Optional[str]) -> str:
    session_key = session_id or "anonymous-session"
    return f"{session_key}::{selected_study}"


def register_data_loading_callbacks(
    app,
    *,
    data_dir: Path,
    cache_store,
    loaded_parquet_data_store: Dict[str, Dict[str, pd.DataFrame]],
    dashboard_show_state_store: Dict[str, Dict[int, bool]],
    build_help_tab_content_fn: Callable[[], html.Div],
    logger_obj,
    calculate_median_first_occurrence_fn: Callable[[pd.DataFrame], pd.DataFrame],
    calculate_ordinal_medians_fn: Callable[[pd.DataFrame, pd.DataFrame], pd.DataFrame],
    apply_filters_to_data_fn: Callable,
    filter_ordinal_concepts_for_filtered_mains_fn: Callable,
    show_ordinals_for_active_mains_fn: Callable,
    hide_ordinals_for_active_mains_fn: Callable,
) -> None:
    """Register callbacks for loading study data and building tab content."""
    DATA_DIR = data_dir
    cache = cache_store
    loaded_parquet_data = loaded_parquet_data_store
    dashboard_show_state = dashboard_show_state_store
    build_help_tab_content = build_help_tab_content_fn
    logger = logger_obj
    calculate_median_first_occurrence = calculate_median_first_occurrence_fn
    calculate_ordinal_medians = calculate_ordinal_medians_fn
    _apply_filters_to_data = apply_filters_to_data_fn
    _filter_ordinal_concepts_for_filtered_mains = filter_ordinal_concepts_for_filtered_mains_fn
    _show_ordinals_for_active_mains = show_ordinals_for_active_mains_fn
    _hide_ordinals_for_active_mains = hide_ordinals_for_active_mains_fn
    @app.callback(
        [Output("table-container", "children"),
         Output("dashboard-data-store", "data"),
         Output("clustering-trigger-store", "data"),
         Output("data-mode-store", "data")],
        Input("selected-study-store", "data"),
        [State("target-prevalence-range", "value"),
         State("ratio-range", "value"),
         State("heritage-selection-store", "data"),
         State("show-ordinal-checkbox", "value"),
         State("session-id-store", "data")],
        background=True,
        running=[
            (Output("loading-message-store", "data"), "Loading study data from parquet files...", "")
        ],
        prevent_initial_call=False
    )
    def load_study_data(
        selected_study: Optional[str],
        target_prevalence_range: Optional[List[float]],
        ratio_range: Optional[List[float]],
        selected_heritages: Optional[List[str]],
        show_ordinal_checkbox: Optional[List[str]],
        session_id: Optional[str],
    ) -> Tuple[html.Div, Optional[List[Dict]], Optional[float], str]:
        """
        Load parquet files for the selected study and display them in tables.
        Only loads parquet files when a study is selected (clicked).
        
        Args:
            selected_study: Name of the selected study
            
        Returns:
            HTML Div containing tables for all parquet files
        """
        logger.info(f"load_study_data called with: {selected_study}")
        
        if selected_study is None:
            logger.info("No study selected, returning welcome message")
            welcome_content = html.Div([
                html.H3("Welcome to Contrast Viewer", style={
                    "marginBottom": "20px",
                    "color": "#2c3e50",
                    "fontWeight": "600"
                }),
                html.P(
                    "Select a study from the left sidebar to begin exploring the data.",
                    style={
                        "fontSize": "16px",
                        "color": "#666",
                        "marginTop": "10px",
                        "lineHeight": "1.6"
                    }
                )
            ], style={
                "padding": "40px",
                "textAlign": "left"
            })
            return (html.Div([welcome_content]), None, None, "patient")

        scoped_study_key = _scoped_study_key(selected_study, session_id)
        touch_session(session_id, dashboard_show_state, logger_obj=logger)
        
        # Load parquet files using centralized cache (works across background callbacks)
        parquet_data = get_or_load_parquet_data(selected_study, DATA_DIR, cache)
        if parquet_data is None:
            logger.error(f"Study folder not found or error loading: {selected_study}")
            return (html.Div(f"Study folder {selected_study} not found."), None, None, "patient")
        
        logger.info(f"Loaded keys: {list(parquet_data.keys())}")
        # Also update in-memory cache for this process
        loaded_parquet_data[selected_study] = parquet_data
        logger.info(f"Data mode: {parquet_data.get('_mode', 'unknown')}")
        
        if not parquet_data:
            return (html.Div(f"No parquet files found for {selected_study}."), None, None, "patient")
        
        # Detect data mode from loaded data
        data_mode = parquet_data.get("_mode", "patient")
        logger.info(f"Processing study in {data_mode} mode")
        
        # Load study description if available
        study_folder = DATA_DIR / selected_study
        desc_file = study_folder / "desc.txt"
        study_description = None
        if desc_file.exists():
            try:
                study_description = desc_file.read_text().strip()
            except Exception:
                pass
        
        # Create tabs for different views
        tabs_content = []
        
        # SUMMARY MODE: Use pre-computed concept_summaries directly
        if data_mode == "summary" and "concept_summaries" in parquet_data:
            logger.info("Using SUMMARY mode data processing")
            
            df_features = parquet_data["concept_summaries"].copy()
            
            # Ensure required columns exist
            if "TARGET_SUBJECT_PREVALENCE" not in df_features.columns:
                df_features["TARGET_SUBJECT_PREVALENCE"] = 0
            if "PREVALENCE_DIFFERENCE_RATIO" not in df_features.columns:
                df_features["PREVALENCE_DIFFERENCE_RATIO"] = 0
            if "MEDIAN_FIRST_OCCURRENCE" not in df_features.columns:
                df_features["MEDIAN_FIRST_OCCURRENCE"] = df_features.get("time_median", None)
            if "IS_ORDINAL" not in df_features.columns:
                df_features["IS_ORDINAL"] = False
                
            # Add ordinal summaries if available
            if "ordinal_summaries" in parquet_data:
                ordinal_df = parquet_data["ordinal_summaries"].copy()
                if "IS_ORDINAL" not in ordinal_df.columns:
                    ordinal_df["IS_ORDINAL"] = True
                if "MEDIAN_FIRST_OCCURRENCE" not in ordinal_df.columns:
                    ordinal_df["MEDIAN_FIRST_OCCURRENCE"] = ordinal_df.get("time_median", None)
                df_features = pd.concat([df_features, ordinal_df], ignore_index=True)
            
            # Create df_dashboard directly from df_features for summary mode
            required_cols = ["CONCEPT_NAME", "HERITAGE", "TARGET_SUBJECT_PREVALENCE", "PREVALENCE_DIFFERENCE_RATIO", "CONCEPT_ID"]
            available_cols = [col for col in required_cols if col in df_features.columns]
            df_dashboard = df_features[available_cols].copy()
            
            # Preserve linking columns
            for col in ["ORIGINAL_CONCEPT_ID", "ORDINAL", "IS_ORDINAL", "MEDIAN_FIRST_OCCURRENCE"]:
                if col in df_features.columns:
                    df_dashboard[col] = df_features[col]
            
            # Common processing for summary mode - create the rest of dashboard
            # Get or initialize show state for this study
            if scoped_study_key not in dashboard_show_state:
                dashboard_show_state[scoped_study_key] = {}
            
            # Add a "show" column based on stored state
            concept_ids = df_dashboard.get("CONCEPT_ID", range(len(df_dashboard))).tolist()
            df_dashboard["_concept_id"] = concept_ids
            df_dashboard["_show"] = [
                dashboard_show_state[scoped_study_key].get(cid, True) for cid in concept_ids
            ]
            
            # Scale TARGET_SUBJECT_PREVALENCE to 0-100%
            if "TARGET_SUBJECT_PREVALENCE" in df_dashboard.columns:
                df_dashboard["TARGET_SUBJECT_PREVALENCE_PCT"] = df_dashboard["TARGET_SUBJECT_PREVALENCE"] * 100
            else:
                df_dashboard["TARGET_SUBJECT_PREVALENCE_PCT"] = 0
            
            # Handle PREVALENCE_DIFFERENCE_RATIO
            if "PREVALENCE_DIFFERENCE_RATIO" in df_dashboard.columns:
                df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"] = df_dashboard["PREVALENCE_DIFFERENCE_RATIO"].copy()
                df_dashboard.loc[
                    (df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"] > 100) | 
                    (df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"].isna()) |
                    (~np.isfinite(df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"])),
                    "PREVALENCE_DIFFERENCE_RATIO_DISPLAY"
                ] = 100
            else:
                df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"] = 0
            
            # Convert list columns to strings
            df_dashboard = convert_list_columns_to_strings(df_dashboard)
            
            # Apply default filters
            default_heritages = ["procedure_occurrence", "measurement", "drug_exposure"]
            available_heritages = set()
            if "HERITAGE" in df_dashboard.columns:
                for heritage in df_dashboard["HERITAGE"].dropna().unique():
                    if heritage is not None and str(heritage).strip():
                        available_heritages.add(str(heritage))
            
            selected_heritages = [h for h in default_heritages if h in available_heritages]
            if not selected_heritages and available_heritages:
                selected_heritages = list(available_heritages)
            
            selected_heritages_set = set(selected_heritages)
            target_min, target_max = 10, 100
            ratio_min, ratio_max = 5, 100
            
            # Vectorized filtering
            target_prev = df_dashboard["TARGET_SUBJECT_PREVALENCE_PCT"].fillna(0)
            ratio = df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"].fillna(0)
            heritage_str = df_dashboard["HERITAGE"].fillna("").astype(str)
            
            matches_target = (target_prev >= target_min) & (target_prev <= target_max)
            matches_ratio = (ratio >= ratio_min) & (ratio <= ratio_max)
            matches_heritage = heritage_str.isin(selected_heritages_set)
            
            df_dashboard["_show"] = matches_target & matches_ratio & matches_heritage
    
            # Apply the same ordinal activation rules as the Apply Filters flow.
            dashboard_data_list = df_dashboard.to_dict("records")
            _filter_ordinal_concepts_for_filtered_mains(dashboard_data_list, scoped_study_key)
    
            show_ordinals = show_ordinal_checkbox and "show_ordinals" in show_ordinal_checkbox
            if show_ordinals:
                _show_ordinals_for_active_mains(dashboard_data_list, scoped_study_key)
            else:
                _hide_ordinals_for_active_mains(dashboard_data_list, scoped_study_key)
    
            # Sync DataFrame _show values from filtered list.
            concept_id_to_show = {
                row.get("_concept_id"): bool(row.get("_show", False))
                for row in dashboard_data_list
                if row.get("_concept_id") is not None
            }
            for idx in df_dashboard.index:
                cid = df_dashboard.loc[idx, "_concept_id"]
                if cid in concept_id_to_show:
                    df_dashboard.loc[idx, "_show"] = concept_id_to_show[cid]
    
            # Update stored state
            dashboard_show_state[scoped_study_key] = {
                row["_concept_id"]: bool(row.get("_show", True))
                for row in dashboard_data_list
                if row.get("_concept_id") is not None
            }
    
            dashboard_data = dashboard_data_list
            dashboard_table = build_dashboard_table(dashboard_data, summary_mode=True)
            tabs_content.append(
                build_dashboard_tab(
                    dashboard_table,
                    study_description=study_description,
                    concept_count=len(df_dashboard),
                    summary_mode=True,
                )
            )
            
        # PATIENT MODE: Process data_features with patient-level data
        elif "data_features" in parquet_data:
            df_features = parquet_data["data_features"].copy()
            
            # Filter for ABSTRACTION_LEVEL = -1
            if "ABSTRACTION_LEVEL" in df_features.columns:
                df_features = df_features[df_features["ABSTRACTION_LEVEL"] == -1].copy()
            
            # Filter out concepts that are never present in target patients
            # Check if TARGET_SUBJECT_COUNT or TARGET_SUBJECT_PREVALENCE is available
            if "TARGET_SUBJECT_COUNT" in df_features.columns:
                # Filter where TARGET_SUBJECT_COUNT > 0
                df_features = df_features[df_features["TARGET_SUBJECT_COUNT"] > 0].copy()
            elif "TARGET_SUBJECT_PREVALENCE" in df_features.columns:
                # Filter where TARGET_SUBJECT_PREVALENCE > 0
                df_features = df_features[df_features["TARGET_SUBJECT_PREVALENCE"] > 0].copy()
            elif "data_patients" in parquet_data:
                # Fallback: check data_patients directly
                df_patients = parquet_data["data_patients"]
                target_patients = df_patients[
                    (df_patients["COHORT_DEFINITION_ID"] == "target") & 
                    (df_patients["PREVALENCE"] > 0)
                ]
                # Use both CONCEPT_ID and HERITAGE for filtering
                if "HERITAGE" in target_patients.columns and "HERITAGE" in df_features.columns:
                    target_concept_heritage = target_patients[["CONCEPT_ID", "HERITAGE"]].drop_duplicates()
                    df_features = df_features.merge(
                        target_concept_heritage,
                        on=["CONCEPT_ID", "HERITAGE"],
                        how="inner"
                    )
                else:
                    # Fallback to CONCEPT_ID only if HERITAGE is missing
                    target_concepts = target_patients["CONCEPT_ID"].unique()
                    df_features = df_features[df_features["CONCEPT_ID"].isin(target_concepts)].copy()
            
            # Create ordinal concept variants (1st, 2nd, 3rd, etc.)
            if "data_patients" in parquet_data:
                df_features = create_ordinal_concepts(parquet_data["data_patients"], df_features)
            
            # Recalculate metrics from data_patients for all concepts (main + ordinals)
            if "data_patients" in parquet_data and "data_initial" in parquet_data:
                # Calculate metrics for all concepts
                metrics_df = calculate_concept_metrics_from_patients(
                    parquet_data["data_patients"],
                    parquet_data["data_initial"],
                    df_features
                )
                
                # Merge metrics back to df_features, using main concept values as fallback
                if "HERITAGE" in df_features.columns and "HERITAGE" in metrics_df.columns:
                    df_features = df_features.merge(
                        metrics_df,
                        on=["CONCEPT_ID", "HERITAGE"],
                        how="left",
                        suffixes=("", "_new")
                    )
                else:
                    df_features = df_features.merge(
                        metrics_df,
                        on="CONCEPT_ID",
                        how="left",
                        suffixes=("", "_new")
                    )
                
                # Update with new calculated values, fallback to original for missing values
                # This is 3-5x faster than iterrows() for datasets with many ordinal concepts
                if "TARGET_SUBJECT_PREVALENCE_new" in df_features.columns:
                    # For ordinal concepts, use new value if available, otherwise use main concept value
                    # Vectorized approach: create lookup map for main concepts first
                    if "IS_ORDINAL" in df_features.columns and "ORIGINAL_CONCEPT_ID" in df_features.columns:
                        # Build lookup map for main concepts (non-ordinals) - much faster than repeated lookups
                        main_concepts = df_features[df_features.get("IS_ORDINAL", pd.Series([False] * len(df_features))) == False]
                        if not main_concepts.empty:
                            # Create multi-index lookup using vectorized operations
                            # Create multi-index lookup: (CONCEPT_ID, HERITAGE) -> (prevalence, ratio)
                            main_lookup = {}
                            concept_ids = main_concepts["CONCEPT_ID"].values
                            heritages = main_concepts["HERITAGE"].values
                            prevalences = main_concepts["TARGET_SUBJECT_PREVALENCE"].values
                            ratios = main_concepts["PREVALENCE_DIFFERENCE_RATIO"].values
                            for concept_id, heritage, prev, ratio in zip(concept_ids, heritages, prevalences, ratios):
                                key = (concept_id, heritage)
                                main_lookup[key] = (prev, ratio)
                            
                            # Vectorized: find all ordinal concepts that need fallback values
                            ordinal_mask = (
                                df_features.get("IS_ORDINAL", pd.Series([False] * len(df_features))) == True
                            ) & df_features["TARGET_SUBJECT_PREVALENCE_new"].isna()
                            
                            if ordinal_mask.any():
                                # Update only the rows that need fallback
                                for idx in df_features[ordinal_mask].index:
                                    row = df_features.loc[idx]
                                    original_id = row.get("ORIGINAL_CONCEPT_ID")
                                    heritage = row.get("HERITAGE")
                                    if original_id is not None:
                                        lookup_key = (original_id, heritage)
                                        if lookup_key in main_lookup:
                                            prev, ratio = main_lookup[lookup_key]
                                            df_features.loc[idx, "TARGET_SUBJECT_PREVALENCE_new"] = prev
                                            df_features.loc[idx, "PREVALENCE_DIFFERENCE_RATIO_new"] = ratio
                    
                    df_features["TARGET_SUBJECT_PREVALENCE"] = df_features["TARGET_SUBJECT_PREVALENCE_new"].fillna(df_features["TARGET_SUBJECT_PREVALENCE"])
                    df_features["PREVALENCE_DIFFERENCE_RATIO"] = df_features["PREVALENCE_DIFFERENCE_RATIO_new"].fillna(df_features["PREVALENCE_DIFFERENCE_RATIO"])
                    df_features = df_features.drop(columns=["TARGET_SUBJECT_PREVALENCE_new", "PREVALENCE_DIFFERENCE_RATIO_new"])
            
            # Select only the required columns, but also include CONCEPT_ID for merging
            required_cols = ["CONCEPT_NAME", "HERITAGE", "TARGET_SUBJECT_PREVALENCE", "PREVALENCE_DIFFERENCE_RATIO"]
            available_cols = [col for col in required_cols if col in df_features.columns]
            # Always include CONCEPT_ID if it exists
            if "CONCEPT_ID" in df_features.columns:
                available_cols.append("CONCEPT_ID")
            df_dashboard = df_features[available_cols].copy()
            
            # Preserve linking columns if they exist
            if "ORIGINAL_CONCEPT_ID" in df_features.columns:
                df_dashboard["ORIGINAL_CONCEPT_ID"] = df_features["ORIGINAL_CONCEPT_ID"]
            if "ORDINAL" in df_features.columns:
                df_dashboard["ORDINAL"] = df_features["ORDINAL"]
            if "IS_ORDINAL" in df_features.columns:
                df_dashboard["IS_ORDINAL"] = df_features["IS_ORDINAL"]
            
            # Calculate median occurrence time for each concept (including ordinals)
            if "data_patients" in parquet_data:
                # Calculate for main concepts (1st occurrence)
                median_first_occurrence_df = calculate_median_first_occurrence(parquet_data["data_patients"])
                
                # For ordinal concepts, calculate median for the specific ordinal
                if "IS_ORDINAL" in df_dashboard.columns and df_dashboard["IS_ORDINAL"].any():
                    ordinal_concepts_df = df_dashboard[df_dashboard["IS_ORDINAL"] == True].copy()
                    if not ordinal_concepts_df.empty:
                        # Ensure we have CONCEPT_ID in ordinal_concepts_df for the calculation
                        if "CONCEPT_ID" not in ordinal_concepts_df.columns and "CONCEPT_ID" in df_features.columns:
                            # Get CONCEPT_IDs from df_features for ordinal rows
                            ordinal_indices = ordinal_concepts_df.index
                            ordinal_concepts_df["CONCEPT_ID"] = df_features.loc[ordinal_indices, "CONCEPT_ID"].values
                        
                        ordinal_medians = calculate_ordinal_medians(
                            parquet_data["data_patients"],
                            ordinal_concepts_df
                        )
                        # Combine main and ordinal medians
                        if not ordinal_medians.empty:
                            # Ensure CONCEPT_ID types match before concatenating
                            if "CONCEPT_ID" in median_first_occurrence_df.columns:
                                median_first_occurrence_df["CONCEPT_ID"] = median_first_occurrence_df["CONCEPT_ID"].astype(str)
                            if "CONCEPT_ID" in ordinal_medians.columns:
                                ordinal_medians["CONCEPT_ID"] = ordinal_medians["CONCEPT_ID"].astype(str)
                            median_first_occurrence_df = pd.concat([median_first_occurrence_df, ordinal_medians], ignore_index=True)
                
                # Ensure CONCEPT_ID and HERITAGE are in df_dashboard for merging
                if "CONCEPT_ID" not in df_dashboard.columns:
                    concept_ids = df_features.get("CONCEPT_ID", range(len(df_features))).tolist()
                    df_dashboard["CONCEPT_ID"] = concept_ids
                if "HERITAGE" not in df_dashboard.columns and "HERITAGE" in df_features.columns:
                    df_dashboard["HERITAGE"] = df_features["HERITAGE"].values
                
                # Ensure CONCEPT_ID types match (convert to same type for merge)
                if "CONCEPT_ID" in df_dashboard.columns and "CONCEPT_ID" in median_first_occurrence_df.columns:
                    df_dashboard["CONCEPT_ID"] = df_dashboard["CONCEPT_ID"].astype(str)
                    median_first_occurrence_df["CONCEPT_ID"] = median_first_occurrence_df["CONCEPT_ID"].astype(str)
                
                # Merge on both CONCEPT_ID and HERITAGE if both are available
                if "HERITAGE" in df_dashboard.columns and "HERITAGE" in median_first_occurrence_df.columns:
                    # Convert HERITAGE to string for consistent merging
                    df_dashboard["HERITAGE"] = df_dashboard["HERITAGE"].astype(str)
                    median_first_occurrence_df["HERITAGE"] = median_first_occurrence_df["HERITAGE"].astype(str)
                    
                    df_dashboard = df_dashboard.merge(
                        median_first_occurrence_df,
                        on=["CONCEPT_ID", "HERITAGE"],
                        how="left",
                        suffixes=("", "_median")
                    )
                    df_dashboard = _merge_median_column(df_dashboard)
                else:
                    df_dashboard = df_dashboard.merge(
                        median_first_occurrence_df,
                        on="CONCEPT_ID",
                        how="left",
                        suffixes=("", "_median")
                    )
                    df_dashboard = _merge_median_column(df_dashboard)
                
                # The merge should have worked, but let's verify ordinal concepts got their values
                # Only use fallback if median is actually missing (not just checking isna, but also checking if it's the same as main)
                
                # For ordinal concepts with missing median (only if merge failed), use main concept value as fallback
                if "IS_ORDINAL" in df_dashboard.columns and "ORIGINAL_CONCEPT_ID" in df_dashboard.columns:
                    missing_ordinals = df_dashboard[
                        (df_dashboard["IS_ORDINAL"] == True) & 
                        (df_dashboard["MEDIAN_FIRST_OCCURRENCE"].isna())
                    ]
                    
                    for idx in missing_ordinals.index:
                        ordinal_row = df_dashboard.loc[idx]
                        original_id = ordinal_row.get("ORIGINAL_CONCEPT_ID")
                        heritage = ordinal_row.get("HERITAGE")
                        
                        # Find main concept row
                        if "HERITAGE" in df_dashboard.columns and heritage is not None:
                            main_row = df_dashboard[
                                (df_dashboard["CONCEPT_ID"] == original_id) &
                                (df_dashboard["HERITAGE"] == heritage) &
                                (df_dashboard["IS_ORDINAL"] == False)
                            ]
                        else:
                            main_row = df_dashboard[
                                (df_dashboard["CONCEPT_ID"] == original_id) &
                                (df_dashboard["IS_ORDINAL"] == False)
                            ]
                        
                        # Only use fallback if median is actually missing (NaN)
                        if not main_row.empty and not pd.isna(main_row.iloc[0].get("MEDIAN_FIRST_OCCURRENCE")):
                            df_dashboard.loc[idx, "MEDIAN_FIRST_OCCURRENCE"] = main_row.iloc[0]["MEDIAN_FIRST_OCCURRENCE"]
            else:
                df_dashboard["MEDIAN_FIRST_OCCURRENCE"] = None
            
            # Get or initialize show state for this study
            if scoped_study_key not in dashboard_show_state:
                dashboard_show_state[scoped_study_key] = {}
            
            # Add a "show" column based on stored state
            concept_ids = df_dashboard.get("CONCEPT_ID", range(len(df_dashboard))).tolist()
            df_dashboard["_concept_id"] = concept_ids
            df_dashboard["_show"] = [
                dashboard_show_state[scoped_study_key].get(cid, True) for cid in concept_ids
            ]
            
            # Scale TARGET_SUBJECT_PREVALENCE to 0-100% (multiply by 100)
            if "TARGET_SUBJECT_PREVALENCE" in df_dashboard.columns:
                df_dashboard["TARGET_SUBJECT_PREVALENCE_PCT"] = df_dashboard["TARGET_SUBJECT_PREVALENCE"] * 100
            
            # Handle PREVALENCE_DIFFERENCE_RATIO: cap at 100 for values > 100 or NA/0 in control
            if "PREVALENCE_DIFFERENCE_RATIO" in df_dashboard.columns:
                # Replace inf, very large values, or NaN with 100
                df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"] = df_dashboard["PREVALENCE_DIFFERENCE_RATIO"].copy()
                # Replace infinite or very large values with 100
                df_dashboard.loc[
                    (df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"] > 100) | 
                    (df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"].isna()) |
                    (~np.isfinite(df_dashboard["PREVALENCE_DIFFERENCE_RATIO_DISPLAY"])),
                    "PREVALENCE_DIFFERENCE_RATIO_DISPLAY"
                ] = 100
            
            # Convert list columns to strings
            df_dashboard = convert_list_columns_to_strings(df_dashboard)
            
            # Apply filters - use provided values or defaults
            # Default heritage selections: procedures, measurements, drugs
            default_heritages = ["procedure_occurrence", "measurement", "drug_exposure"]
            # Get available heritages from data
            available_heritages = set()
            if "HERITAGE" in df_dashboard.columns:
                for heritage in df_dashboard["HERITAGE"].dropna().unique():
                    if heritage is not None and str(heritage).strip():
                        available_heritages.add(str(heritage))
            
            # Use provided heritages or defaults
            if selected_heritages:
                # Use provided heritages that exist in the data
                selected_heritages_list = [h for h in selected_heritages if h in available_heritages]
                if not selected_heritages_list and available_heritages:
                    selected_heritages_list = list(available_heritages)
            else:
                # Use default heritages that exist in the data, or all if none match
                selected_heritages_list = [h for h in default_heritages if h in available_heritages]
                if not selected_heritages_list and available_heritages:
                    selected_heritages_list = list(available_heritages)
            
            selected_heritages_set = set(selected_heritages_list)
            
            # Use provided filter ranges or defaults
            target_min = target_prevalence_range[0] if target_prevalence_range and len(target_prevalence_range) >= 1 else 10
            target_max = target_prevalence_range[1] if target_prevalence_range and len(target_prevalence_range) >= 2 else 100
            ratio_min = ratio_range[0] if ratio_range and len(ratio_range) >= 1 else 5
            ratio_max = ratio_range[1] if ratio_range and len(ratio_range) >= 2 else 100
            
            # Convert to list format for helper functions
            dashboard_data_list = df_dashboard.to_dict("records")
            
            # Apply filters using the same logic as "Apply Filters"
            _apply_filters_to_data(
                dashboard_data_list, target_min, target_max, ratio_min, ratio_max,
                selected_heritages_set, scoped_study_key
            )
            
            _filter_ordinal_concepts_for_filtered_mains(dashboard_data_list, scoped_study_key)
            
            # Apply ordinal checkbox logic (same as "Apply Filters")
            show_ordinals = show_ordinal_checkbox and "show_ordinals" in show_ordinal_checkbox
            if show_ordinals:
                _show_ordinals_for_active_mains(dashboard_data_list, scoped_study_key)
            else:
                _hide_ordinals_for_active_mains(dashboard_data_list, scoped_study_key)
            
            # Update DataFrame with filtered results using _concept_id for matching
            concept_id_to_show = {}
            for row in dashboard_data_list:
                concept_id = row.get("_concept_id")
                if concept_id is not None:
                    concept_id_to_show[concept_id] = row.get("_show", False)
            
            for idx in df_dashboard.index:
                concept_id = df_dashboard.loc[idx, "_concept_id"]
                if concept_id in concept_id_to_show:
                    df_dashboard.loc[idx, "_show"] = concept_id_to_show[concept_id]
            
            # Update stored state using filtered list
            if scoped_study_key not in dashboard_show_state:
                dashboard_show_state[scoped_study_key] = {}
            dashboard_show_state[scoped_study_key] = {
                row["_concept_id"]: bool(row.get("_show", False))
                for row in dashboard_data_list
                if row.get("_concept_id") is not None
            }
            
            dashboard_data = dashboard_data_list
            dashboard_table = build_dashboard_table(dashboard_data, summary_mode=False)
            tabs_content.append(
                build_dashboard_tab(
                    dashboard_table,
                    study_description=study_description,
                    concept_count=len(df_dashboard),
                    summary_mode=False,
                )
            )
        
        # Add Mappings tab: patient-mode concept merge table + mapping history table
        mapping_df = pd.DataFrame()
        if parquet_data and "complementaryMappingTable" in parquet_data:
            mapping_df = parquet_data["complementaryMappingTable"].copy()

        if "ABSTRACTION_LEVEL" in mapping_df.columns:
            mapping_df = mapping_df.drop(columns=["ABSTRACTION_LEVEL"])
        mapping_df = convert_list_columns_to_strings(mapping_df)

        mapping_column_display_names = {
            "CONCEPT_ID": "Source Concept ID",
            "CONCEPT_NAME": "Source Concept Name",
            "NEW_CONCEPT_ID": "Mapped Concept ID",
            "NEW_CONCEPT_NAME": "Mapped Concept Name",
            "TYPE": "Mapping Type"
        }

        if mapping_df.empty:
            mapping_history_columns = [
                {"field": "CONCEPT_ID", "headerName": "Source Concept ID", "sortable": True, "filter": True, "resizable": True, "minWidth": 120},
                {"field": "CONCEPT_NAME", "headerName": "Source Concept Name", "sortable": True, "filter": True, "resizable": True, "minWidth": 250, "flex": 2},
                {"field": "NEW_CONCEPT_ID", "headerName": "Mapped Concept ID", "sortable": True, "filter": True, "resizable": True, "minWidth": 120},
                {"field": "NEW_CONCEPT_NAME", "headerName": "Mapped Concept Name", "sortable": True, "filter": True, "resizable": True, "minWidth": 250, "flex": 2},
                {"field": "TYPE", "headerName": "Mapping Type", "sortable": True, "filter": True, "resizable": True, "minWidth": 130},
            ]
        else:
            mapping_history_columns = []
            for col in mapping_df.columns:
                col_def = {
                    "field": col,
                    "headerName": mapping_column_display_names.get(col, format_column_name(col)),
                    "sortable": True,
                    "filter": True,
                    "resizable": True
                }
                if "NAME" in col:
                    col_def["minWidth"] = 250
                    col_def["flex"] = 2
                elif "ID" in col:
                    col_def["minWidth"] = 120
                else:
                    col_def["minWidth"] = 100
                mapping_history_columns.append(col_def)

        mapping_candidates = []
        if data_mode == "patient" and 'dashboard_data' in locals() and dashboard_data:
            for row in dashboard_data:
                candidate_row = row.copy()
                candidate_row["_map"] = False
                mapping_candidates.append(candidate_row)

        mapping_candidates_columns = [
            {
                "field": "_map",
                "headerName": "Map",
                "width": 90,
                "cellRenderer": "agCheckboxCellRenderer",
                "cellEditor": "agCheckboxCellEditor",
                "editable": True,
                "sortable": True,
                "filter": "agSetColumnFilter",
                "filterParams": {"values": [True, False], "buttons": ["apply", "reset"]},
                "cellStyle": {"textAlign": "center"},
            },
            {
                "field": "CONCEPT_NAME",
                "headerName": "Concept Name",
                "flex": 3,
                "minWidth": 220,
                "filter": "agTextColumnFilter",
                "sortable": True,
                "resizable": True,
            },
            {
                "field": "HERITAGE",
                "headerName": "Heritage",
                "flex": 2,
                "minWidth": 170,
                "filter": "agTextColumnFilter",
                "sortable": True,
                "resizable": True,
            },
            {
                "field": "MEDIAN_FIRST_OCCURRENCE",
                "headerName": "Median First Occurrence (days)",
                "flex": 2,
                "minWidth": 210,
                "filter": "agNumberColumnFilter",
                "sortable": True,
                "resizable": True,
                "valueFormatter": {"function": "params.value != null ? params.value.toFixed(1) + ' days' : ''"},
            },
            {
                "field": "TARGET_SUBJECT_PREVALENCE_PCT",
                "headerName": "Target Subject Prevalence (%)",
                "flex": 2,
                "minWidth": 200,
                "filter": "agNumberColumnFilter",
                "sortable": True,
                "resizable": True,
                "valueFormatter": {"function": "params.value != null ? params.value.toFixed(2) + '%' : ''"},
            },
            {
                "field": "PREVALENCE_DIFFERENCE_RATIO_DISPLAY",
                "headerName": "Prevalence Difference Ratio",
                "flex": 2,
                "minWidth": 200,
                "filter": "agNumberColumnFilter",
                "sortable": True,
                "resizable": True,
                "valueFormatter": {"function": "params.value != null ? params.value.toFixed(2) : ''"},
            },
        ]

        suggestion_column_defs = [
            {
                "field": "_map",
                "headerName": "Map",
                "width": 90,
                "cellRenderer": "agCheckboxCellRenderer",
                "cellEditor": "agCheckboxCellEditor",
                "editable": True,
                "sortable": True,
                "filter": "agSetColumnFilter",
                "filterParams": {"values": [True, False], "buttons": ["apply", "reset"]},
                "cellStyle": {"textAlign": "center"},
            },
            {
                "field": "NEW_CONCEPT_NAME",
                "headerName": "Suggested Merged Concept",
                "flex": 2,
                "minWidth": 220,
                "filter": "agTextColumnFilter",
                "sortable": True,
                "editable": True,
                "resizable": True,
            },
            {
                "field": "NEW_CONCEPT_ID",
                "headerName": "Merged Concept ID",
                "minWidth": 140,
                "filter": "agNumberColumnFilter",
                "sortable": True,
                "editable": True,
                "resizable": True,
            },
            {
                "field": "HERITAGE",
                "headerName": "Heritage",
                "minWidth": 180,
                "filter": "agTextColumnFilter",
                "sortable": True,
                "editable": True,
                "resizable": True,
            },
            {
                "field": "SOURCE_CONCEPT_NAMES",
                "headerName": "Source Concepts",
                "flex": 3,
                "minWidth": 360,
                "filter": "agTextColumnFilter",
                "sortable": True,
                "resizable": True,
            },
            {
                "field": "N_CONCEPTS",
                "headerName": "N",
                "minWidth": 80,
                "filter": "agNumberColumnFilter",
                "sortable": True,
                "resizable": True,
            },
            {
                "field": "SCORE",
                "headerName": "Score",
                "minWidth": 110,
                "filter": "agNumberColumnFilter",
                "sortable": True,
                "resizable": True,
            },
            {
                "field": "AUX_VALUE",
                "headerName": "Aux",
                "minWidth": 110,
                "filter": "agNumberColumnFilter",
                "sortable": True,
                "resizable": True,
            },
            {
                "field": "SOURCE_CONCEPT_IDS",
                "headerName": "Source IDs",
                "minWidth": 190,
                "filter": "agTextColumnFilter",
                "sortable": True,
                "resizable": True,
            },
        ]

        mapping_table_content = html.Div([
            html.H3("Concept Mappings", style={"marginBottom": "10px", "color": "#2c3e50"}),
            html.Div(
                id="mappings-candidate-section",
                style={"display": "block" if data_mode == "patient" else "none"},
                children=[
                    dcc.Tabs(
                        id="mappings-mode-tabs",
                        value="manual",
                        children=[
                            dcc.Tab(
                                label="Manual Merge",
                                value="manual",
                                children=[
                                    html.Div(
                                        [
                                            html.H4("Create Mapping", style={"marginBottom": "8px", "color": "#35506b", "fontSize": "18px"}),
                                            html.P(
                                                "Select two or more concepts in the Map column, then create a merged concept.",
                                                style={"color": "#666", "fontSize": "13px", "marginBottom": "10px"},
                                            ),
                                            AgGrid(
                                                id="mappings-candidate-table",
                                                rowData=mapping_candidates,
                                                columnDefs=mapping_candidates_columns,
                                                defaultColDef={
                                                    "sortable": True,
                                                    "filter": True,
                                                    "resizable": True,
                                                    "floatingFilter": True
                                                },
                                                dashGridOptions={
                                                    "pagination": True,
                                                    "paginationPageSize": 10,
                                                    "animateRows": True,
                                                    "rowSelection": "single"
                                                },
                                                style={"height": "460px", "width": "100%", "marginBottom": "10px"},
                                                className="ag-theme-alpine",
                                            ),
                                            html.Div(
                                                [
                                                    html.Button(
                                                        "Merge Selected Concepts",
                                                        id="open-merge-mapping-btn",
                                                        n_clicks=0,
                                                        style={
                                                            "padding": "9px 14px",
                                                            "backgroundColor": "#5a7d9a",
                                                            "color": "white",
                                                            "border": "none",
                                                            "borderRadius": "6px",
                                                            "fontSize": "13px",
                                                            "fontWeight": "600",
                                                            "cursor": "pointer",
                                                        },
                                                    ),
                                                ],
                                                style={"marginBottom": "10px"},
                                            ),
                                            html.Div(id="mappings-action-status", style={"fontSize": "13px", "marginBottom": "10px", "color": "#4a4a4a"}),
                                            html.Div(
                                                id="mappings-merge-form",
                                                style={
                                                    "display": "none",
                                                    "border": "1px solid #d9e2ec",
                                                    "borderRadius": "8px",
                                                    "padding": "12px",
                                                    "backgroundColor": "#f8fbff",
                                                    "marginBottom": "12px",
                                                },
                                                children=[
                                                    html.H5("Confirm Merged Concept", style={"marginBottom": "10px", "color": "#2c3e50", "fontSize": "15px"}),
                                                    html.Div(
                                                        [
                                                            html.Div(
                                                                [
                                                                    html.Label("New Concept ID", style={"fontSize": "12px", "fontWeight": "600", "marginBottom": "4px"}),
                                                                    dcc.Input(
                                                                        id="merge-new-concept-id",
                                                                        type="text",
                                                                        value="",
                                                                        style={"width": "100%", "padding": "7px", "borderRadius": "4px", "border": "1px solid #cbd5e0"},
                                                                    ),
                                                                ],
                                                                style={"flex": "1 1 180px"},
                                                            ),
                                                            html.Div(
                                                                [
                                                                    html.Label("New Concept Name", style={"fontSize": "12px", "fontWeight": "600", "marginBottom": "4px"}),
                                                                    dcc.Input(
                                                                        id="merge-new-concept-name",
                                                                        type="text",
                                                                        value="",
                                                                        style={"width": "100%", "padding": "7px", "borderRadius": "4px", "border": "1px solid #cbd5e0"},
                                                                    ),
                                                                ],
                                                                style={"flex": "2 1 320px"},
                                                            ),
                                                            html.Div(
                                                                [
                                                                    html.Label("Heritage", style={"fontSize": "12px", "fontWeight": "600", "marginBottom": "4px"}),
                                                                    dcc.Dropdown(
                                                                        id="merge-new-heritage",
                                                                        options=[],
                                                                        value=None,
                                                                        clearable=False,
                                                                    ),
                                                                ],
                                                                style={"flex": "1 1 220px"},
                                                            ),
                                                        ],
                                                        style={"display": "flex", "flexWrap": "wrap", "gap": "10px", "marginBottom": "10px"},
                                                    ),
                                                    html.Button(
                                                        "Apply Merge",
                                                        id="execute-merge-mapping-btn",
                                                        n_clicks=0,
                                                        style={
                                                            "padding": "8px 12px",
                                                            "backgroundColor": "#3f7f4f",
                                                            "color": "white",
                                                            "border": "none",
                                                            "borderRadius": "6px",
                                                            "fontSize": "13px",
                                                            "fontWeight": "600",
                                                            "cursor": "pointer",
                                                        },
                                                    ),
                                                ],
                                            ),
                                            html.Hr(style={"margin": "14px 0"}),
                                            html.H5("Save Study State", style={"marginBottom": "8px", "color": "#2c3e50", "fontSize": "15px"}),
                                            html.P(
                                                "Create a copy of this patient-level study with current in-memory mappings applied.",
                                                style={"color": "#666", "fontSize": "13px", "marginBottom": "8px"},
                                            ),
                                            html.Div(
                                                [
                                                    dcc.Input(
                                                        id="save-study-path-input",
                                                        type="text",
                                                        value="",
                                                        placeholder="Output folder path (for example /tmp/LungCancer_1Y_mapped)",
                                                        style={
                                                            "flex": "1 1 420px",
                                                            "padding": "7px",
                                                            "borderRadius": "4px",
                                                            "border": "1px solid #cbd5e0",
                                                        },
                                                    ),
                                                    html.Button(
                                                        "Save Study State Copy",
                                                        id="save-study-state-btn",
                                                        n_clicks=0,
                                                        style={
                                                            "padding": "8px 12px",
                                                            "backgroundColor": "#3d6a9f",
                                                            "color": "white",
                                                            "border": "none",
                                                            "borderRadius": "6px",
                                                            "fontSize": "13px",
                                                            "fontWeight": "600",
                                                            "cursor": "pointer",
                                                        },
                                                    ),
                                                ],
                                                style={"display": "flex", "flexWrap": "wrap", "gap": "10px", "marginBottom": "8px"},
                                            ),
                                            html.Div(
                                                id="mappings-save-state-status",
                                                style={"fontSize": "13px", "marginBottom": "8px", "color": "#4a4a4a"},
                                            ),
                                        ],
                                        style={"paddingTop": "8px"},
                                    )
                                ],
                            ),
                            dcc.Tab(
                                label="Hierarchy Suggestions",
                                value="hierarchy",
                                children=[
                                    html.Div(
                                        [
                                            html.P(
                                                "Review hierarchy-based merge suggestions, tweak parameters, and merge selected suggestions.",
                                                style={"color": "#666", "fontSize": "13px", "marginBottom": "10px"},
                                            ),
                                            html.Div(
                                                [
                                                    html.Div(
                                                        [
                                                            html.Label("Minimum hierarchy depth", style={"fontSize": "12px", "fontWeight": "600"}),
                                                            dcc.Input(id="hierarchy-min-depth-input", type="number", min=1, step=1, value=1, style={"width": "120px"}),
                                                        ],
                                                        style={"display": "flex", "flexDirection": "column", "gap": "5px"},
                                                    ),
                                                    html.Div(
                                                        [
                                                            html.Label("Options", style={"fontSize": "12px", "fontWeight": "600"}),
                                                            dcc.Checklist(
                                                                id="hierarchy-only-minors-checkbox",
                                                                options=[{"label": "Allow only minors", "value": "only_minors"}],
                                                                value=["only_minors"],
                                                            ),
                                                        ],
                                                        style={"display": "flex", "flexDirection": "column", "gap": "5px"},
                                                    ),
                                                    html.Button(
                                                        "Update Hierarchy Suggestions",
                                                        id="refresh-hierarchy-suggestions-btn",
                                                        n_clicks=0,
                                                        style={
                                                            "padding": "8px 12px",
                                                            "backgroundColor": "#5a7d9a",
                                                            "color": "white",
                                                            "border": "none",
                                                            "borderRadius": "6px",
                                                            "fontSize": "13px",
                                                            "fontWeight": "600",
                                                            "cursor": "pointer",
                                                        },
                                                    ),
                                                ],
                                                style={"display": "flex", "gap": "30px", "alignItems": "flex-end", "marginBottom": "10px"},
                                            ),
                                            html.P(id="hierarchy-suggestion-note", style={"color": "#666", "fontSize": "13px", "marginBottom": "10px"}),
                                            AgGrid(
                                                id="hierarchy-suggestion-table",
                                                rowData=[],
                                                columnDefs=suggestion_column_defs,
                                                defaultColDef={
                                                    "sortable": True,
                                                    "filter": True,
                                                    "resizable": True,
                                                    "floatingFilter": True
                                                },
                                                dashGridOptions={
                                                    "pagination": True,
                                                    "paginationPageSize": 10,
                                                    "animateRows": True,
                                                    "rowSelection": "single"
                                                },
                                                style={"height": "460px", "width": "100%", "marginBottom": "10px"},
                                                className="ag-theme-alpine",
                                            ),
                                            html.Button(
                                                "Merge Selected Hierarchy Suggestions",
                                                id="execute-hierarchy-suggestions-btn",
                                                n_clicks=0,
                                                style={
                                                    "padding": "8px 12px",
                                                    "backgroundColor": "#3f7f4f",
                                                    "color": "white",
                                                    "border": "none",
                                                    "borderRadius": "6px",
                                                    "fontSize": "13px",
                                                    "fontWeight": "600",
                                                    "cursor": "pointer",
                                                },
                                            ),
                                            html.Div(id="hierarchy-suggestion-status", style={"fontSize": "13px", "marginTop": "10px", "color": "#4a4a4a"}),
                                        ],
                                        style={"paddingTop": "8px"},
                                    )
                                ],
                            ),
                            dcc.Tab(
                                label="Correlation Suggestions",
                                value="correlation",
                                children=[
                                    html.Div(
                                        [
                                            html.P(
                                                "Review correlation-based merge suggestions, tweak parameters, and merge selected suggestions.",
                                                style={"color": "#666", "fontSize": "13px", "marginBottom": "10px"},
                                            ),
                                            html.Div(
                                                [
                                                    html.Div(
                                                        [
                                                            html.Label("Minimum correlation", style={"fontSize": "12px", "fontWeight": "600"}),
                                                            dcc.Input(id="correlation-min-input", type="number", min=0, max=1, step=0.05, value=0.7, style={"width": "120px"}),
                                                        ],
                                                        style={"display": "flex", "flexDirection": "column", "gap": "5px"},
                                                    ),
                                                    html.Div(
                                                        [
                                                            html.Label("Max median days in between", style={"fontSize": "12px", "fontWeight": "600"}),
                                                            dcc.Input(id="correlation-max-days-input", type="number", min=0, step=1, value=1, style={"width": "140px"}),
                                                        ],
                                                        style={"display": "flex", "flexDirection": "column", "gap": "5px"},
                                                    ),
                                                    html.Div(
                                                        [
                                                            html.Label("Options", style={"fontSize": "12px", "fontWeight": "600"}),
                                                            dcc.Checklist(
                                                                id="correlation-heritage-drift-checkbox",
                                                                options=[{"label": "Allow heritage drift", "value": "allow_drift"}],
                                                                value=[],
                                                            ),
                                                        ],
                                                        style={"display": "flex", "flexDirection": "column", "gap": "5px"},
                                                    ),
                                                    html.Button(
                                                        "Update Correlation Suggestions",
                                                        id="refresh-correlation-suggestions-btn",
                                                        n_clicks=0,
                                                        style={
                                                            "padding": "8px 12px",
                                                            "backgroundColor": "#5a7d9a",
                                                            "color": "white",
                                                            "border": "none",
                                                            "borderRadius": "6px",
                                                            "fontSize": "13px",
                                                            "fontWeight": "600",
                                                            "cursor": "pointer",
                                                        },
                                                    ),
                                                ],
                                                style={"display": "flex", "gap": "30px", "alignItems": "flex-end", "marginBottom": "10px"},
                                            ),
                                            html.P(id="correlation-suggestion-note", style={"color": "#666", "fontSize": "13px", "marginBottom": "10px"}),
                                            AgGrid(
                                                id="correlation-suggestion-table",
                                                rowData=[],
                                                columnDefs=suggestion_column_defs,
                                                defaultColDef={
                                                    "sortable": True,
                                                    "filter": True,
                                                    "resizable": True,
                                                    "floatingFilter": True
                                                },
                                                dashGridOptions={
                                                    "pagination": True,
                                                    "paginationPageSize": 10,
                                                    "animateRows": True,
                                                    "rowSelection": "single"
                                                },
                                                style={"height": "460px", "width": "100%", "marginBottom": "10px"},
                                                className="ag-theme-alpine",
                                            ),
                                            html.Button(
                                                "Merge Selected Correlation Suggestions",
                                                id="execute-correlation-suggestions-btn",
                                                n_clicks=0,
                                                style={
                                                    "padding": "8px 12px",
                                                    "backgroundColor": "#3f7f4f",
                                                    "color": "white",
                                                    "border": "none",
                                                    "borderRadius": "6px",
                                                    "fontSize": "13px",
                                                    "fontWeight": "600",
                                                    "cursor": "pointer",
                                                },
                                            ),
                                            html.Div(id="correlation-suggestion-status", style={"fontSize": "13px", "marginTop": "10px", "color": "#4a4a4a"}),
                                        ],
                                        style={"paddingTop": "8px"},
                                    )
                                ],
                            ),
                        ],
                    ),
                ],
            ),
            html.Div(
                id="mappings-summary-note",
                style={"display": "block" if data_mode == "summary" else "none"},
                children=[
                    html.P(
                        "Concept merge actions are available only in patient-level mode. Mapping history is shown below.",
                        style={"color": "#666", "fontSize": "13px", "marginBottom": "10px"},
                    )
                ],
            ),
            html.H4("Mapping History", style={"marginBottom": "6px", "color": "#35506b", "fontSize": "18px"}),
            html.P(
                id="mappings-history-count",
                children=f"Showing {len(mapping_df)} mapping entries",
                style={"color": "#666", "fontSize": "14px", "marginBottom": "10px"},
            ),
            AgGrid(
                id="mappings-history-table",
                rowData=mapping_df.to_dict("records"),
                columnDefs=mapping_history_columns,
                defaultColDef={
                    "sortable": True,
                    "filter": True,
                    "resizable": True,
                    "floatingFilter": True
                },
                dashGridOptions={
                    "pagination": True,
                    "paginationPageSize": 10,
                    "animateRows": True,
                    "rowSelection": "single"
                },
                style={"height": "460px", "width": "100%"},
                className="ag-theme-alpine"
            ),
        ], style={"padding": "20px"})
        
        # Trajectories Tab - Show concept ordering by median time per cluster
        trajectories_content = html.Div([
            html.H3("Concept Trajectories", className="mb-3", style={"color": "#2c3e50", "fontWeight": "600"}),
            html.P("Compare concept rank shifts vs overall ordering. Cells show median day and prevalence.", 
                   style={"color": "#666", "marginBottom": "10px"}),
            
            # Info note about cluster prevalence filter
            html.Div([
                html.Span("Use the ", style={"color": "#666", "fontSize": "13px"}),
                html.Span("Cluster Prevalence", style={"color": "#5a7d9a", "fontWeight": "600", "fontSize": "13px"}),
                html.Span(" slider in the sidebar Filters section to filter concepts by minimum cluster prevalence.", 
                         style={"color": "#666", "fontSize": "13px"})
            ], style={
                "backgroundColor": "#f0f7ff", 
                "padding": "10px 15px", 
                "borderRadius": "6px", 
                "marginBottom": "15px",
                "border": "1px solid #d0e3ff"
            }),
    
            html.Div([
                html.Label("Row ordering:", style={"fontWeight": "600", "marginRight": "10px", "color": "#2c3e50"}),
                dcc.RadioItems(
                    id="trajectory-order-mode",
                    options=[
                        {"label": "Overall order", "value": "order"},
                        {"label": "Top movers", "value": "movers"},
                        {"label": "Most stable", "value": "stable"},
                    ],
                    value="order",
                    inline=True,
                    inputStyle={"marginRight": "5px", "marginLeft": "12px"},
                    style={"display": "inline-flex", "gap": "6px"}
                )
            ], style={
                "display": "flex",
                "alignItems": "center",
                "padding": "10px 12px",
                "backgroundColor": "#f8f9fa",
                "borderRadius": "6px",
                "marginBottom": "12px"
            }),
            
            dcc.Loading(
                id="trajectories-loading",
                type="circle",
                children=[
                    html.Div(id="trajectories-plot-container", children=[
                        html.P("Select a study and wait for clustering to complete.", 
                               style={"color": "#999", "textAlign": "center", "padding": "50px"})
                    ])
                ]
            )
        ], style={"padding": "20px"})
        
        tabs_content.append(
            dcc.Tab(label="Trajectories", value="trajectories", children=[trajectories_content])
        )
        
        # Overlap Tab - Show concept co-occurrence matrix
        overlap_content = html.Div([
            html.H3("Concept Overlap", className="mb-3", style={"color": "#2c3e50", "fontWeight": "600"}),
            html.P("Shows how often concepts co-occur in the same patients. Higher values indicate concepts appear together.", 
                   style={"color": "#666", "marginBottom": "15px"}),
            
            # Controls row
            html.Div([
                # Group selector
                html.Div([
                    html.Label("Group:", style={"marginRight": "10px", "fontWeight": "500"}),
                    dcc.RadioItems(
                        id="overlap-group-selector",
                        options=[{"label": "Overall", "value": "overall"}],
                        value="overall",
                        inline=True,
                        style={"display": "inline-flex", "gap": "15px"},
                        inputStyle={"marginRight": "5px"}
                    )
                ], style={"display": "flex", "alignItems": "center"}),
                # Hidden metric selector (kept for callback compatibility)
                dcc.RadioItems(
                    id="overlap-metric-selector",
                    options=[{"label": "Combined", "value": "combined"}],
                    value="combined",
                    style={"display": "none"}
                )
            ], style={"display": "flex", "flexWrap": "wrap", "marginBottom": "20px", "padding": "10px", 
                      "backgroundColor": "#f8f9fa", "borderRadius": "8px"}),
            
            # Plot container
            html.Div(id="overlap-plot-container", style={"marginTop": "10px"})
        ], style={"padding": "20px"})
        
        tabs_content.append(
            dcc.Tab(label="Overlap", value="overlap", children=[overlap_content])
        )
        
        # Demographics Tab
        demographics_content = html.Div([
            html.H3("Demographics", style={"marginBottom": "15px", "color": "#2c3e50", "fontWeight": "600"}),
            html.P("Age and sex distributions for the target cohort.", 
                   style={"color": "#666", "marginBottom": "20px", "fontSize": "14px"}),
            html.Div(id="demographics-plot-container", style={"marginTop": "10px"})
        ], style={"padding": "20px"})
        
        tabs_content.append(
            dcc.Tab(label="Demographics", value="demographics", children=[demographics_content])
        )
        
        tabs_content.append(
            dcc.Tab(label="Mappings", value="mappings", children=[mapping_table_content])
        )
    
        tabs_content.append(
            dcc.Tab(label="Help", value="help", children=[build_help_tab_content()])
        )
        
        
        # Return (loading message is handled by running parameter)
        # Include a timestamp trigger to initiate clustering after data load
        import time
        clustering_trigger = time.time() if 'dashboard_data' in locals() and dashboard_data else None
        return (html.Div([
            dcc.Tabs(id="main-tabs", value="dashboard", children=tabs_content)
        ]), dashboard_data if 'dashboard_data' in locals() else None, clustering_trigger, data_mode)
    
    
