"""
Data loading functions for ContrastViewer.

Supports two modes:
- Patient mode: Uses patient-level data (data_patients.parquet)
- Summary mode: Uses pre-computed aggregated data (concept_summaries.parquet)

Tries fastparquet first, then falls back to pyarrow if not available.

Note: When parquet files are written by nanoparquet (R), list columns are
serialized to JSON strings. This module automatically deserializes them back.
"""

from pathlib import Path
from typing import Dict, Optional, List
import json
import warnings
import logging

import pandas as pd
import numpy as np

# Detect available parquet engine (prefer fastparquet, fall back to pyarrow)
def _detect_parquet_engine():
    """Detect which parquet engine is available."""
    try:
        import fastparquet
        return 'fastparquet'
    except ImportError:
        pass
    try:
        import pyarrow
        return 'pyarrow'
    except ImportError:
        pass
    # Return None and let pandas use its default (will error if neither available)
    return None

PARQUET_ENGINE = _detect_parquet_engine()

# Columns that may contain JSON-serialized lists (from nanoparquet conversion)
# Only TIME_TO_EVENT is a list column; PREVALENCE is a scalar
JSON_LIST_COLUMNS = ['TIME_TO_EVENT']

logger = logging.getLogger('ContrastViewer.loader')

# Log which engine is being used at module load time
if PARQUET_ENGINE:
    logger.info(f"Using parquet engine: {PARQUET_ENGINE}")
else:
    logger.warning("No parquet engine found! Install fastparquet or pyarrow.")


def deserialize_json_columns(df: pd.DataFrame, columns: List[str] = None) -> pd.DataFrame:
    """
    Deserialize JSON string columns back to Python lists/arrays.
    
    When parquet files are written by nanoparquet (R), list columns are
    serialized to JSON strings. This function converts them back.
    
    Args:
        df: DataFrame to process
        columns: List of column names to check. If None, uses known list columns.
        
    Returns:
        DataFrame with deserialized columns
    """
    if df.empty:
        return df
    
    # If no specific columns provided, use known list columns
    if columns is None:
        columns = JSON_LIST_COLUMNS
    
    for col in columns:
        if col not in df.columns:
            continue
        
        # Check if column contains JSON strings (starts with '[')
        sample = df[col].dropna().head(5)
        if sample.empty:
            continue
        
        # Check if the first value is a string that looks like a JSON array
        first_val = sample.iloc[0]
        if not isinstance(first_val, str):
            continue  # Already a list/array or other type, skip
        
        # Only deserialize if it clearly looks like a JSON array
        if not first_val.startswith('['):
            continue
        
        # Deserialize JSON strings to lists
        def parse_json_safe(val):
            if pd.isna(val) or val is None:
                return np.array([])
            if isinstance(val, str):
                if len(val) == 0 or val == '[]':
                    return np.array([])
                try:
                    parsed = json.loads(val)
                    if isinstance(parsed, list):
                        return np.array(parsed)
                    return parsed
                except (json.JSONDecodeError, TypeError):
                    return np.array([])
            # Already a list/array, return as is
            if isinstance(val, (list, np.ndarray)):
                return np.array(val)
            return val
        
        df = df.copy()  # Avoid SettingWithCopyWarning
        df[col] = df[col].apply(parse_json_safe)
        logger.info(f"  Deserialized JSON column: {col}")
    
    return df


def detect_data_mode(disease_folder: Path) -> str:
    """
    Detect whether a folder contains patient-level or summary data.
    
    Args:
        disease_folder: Path to the disease subfolder
        
    Returns:
        "patient" or "summary"
    """
    has_metadata = (disease_folder / "metadata.json").exists()
    has_concept_summaries = (disease_folder / "concept_summaries.parquet").exists()
    has_data_patients = (disease_folder / "data_patients.parquet").exists()
    
    if has_metadata and has_concept_summaries:
        return "summary"
    elif has_data_patients:
        return "patient"
    else:
        # Check for any parquet files
        if list(disease_folder.glob("*.parquet")):
            return "patient"  # Assume patient mode
        return "unknown"


def load_summary_metadata(disease_folder: Path) -> Optional[Dict]:
    """
    Load summary metadata from metadata.json.
    
    Args:
        disease_folder: Path to the disease subfolder
        
    Returns:
        Dictionary with metadata or None
    """
    metadata_path = disease_folder / "metadata.json"
    if metadata_path.exists():
        try:
            with open(metadata_path, "r") as f:
                return json.load(f)
        except Exception as e:
            warnings.warn(f"Failed to load metadata: {e}")
    return None


def _na_safe(value):
    """Convert None/NaN values to 'NA' to keep table rendering stable."""
    if value is None:
        return "NA"
    if isinstance(value, (float, np.floating)) and np.isnan(value):
        return "NA"
    return value


def _compact_mode_label(mode: str) -> str:
    """Convert internal mode string to a concise one-word UI label."""
    if mode == "summary":
        return "Summary"
    if mode == "patient":
        return "Patient"
    return "Unknown"


def _metadata_to_study_row(metadata: Dict, disease_folder: Path, mode: str) -> Dict:
    """
    Normalize metadata.json (summary or patient) to study table row format.
    """
    if not isinstance(metadata, dict):
        return {}

    demographics = metadata.get("demographics", {})
    if not isinstance(demographics, dict):
        demographics = {}

    study_name = (
        metadata.get("study")
        or metadata.get("study_name")
        or metadata.get("original_study_name")
        or disease_folder.name
    )

    target_patients = metadata.get("target_patients", demographics.get("target_patients"))
    control_patients = metadata.get("control_patients", demographics.get("control_patients"))

    # Keep chi2y_count as canonical and expose z_count alias for UI compatibility.
    concept_count = metadata.get("chi2y_count")
    if concept_count is None:
        concept_count = metadata.get("z_count")
    if concept_count is None:
        concept_count = metadata.get("significant_concepts")

    return {
        "study": study_name,
        "mode": _compact_mode_label(mode),
        "target_patients": _na_safe(target_patients),
        "control_patients": _na_safe(control_patients),
        "chi2y_count": _na_safe(concept_count),
        "z_count": _na_safe(concept_count),
    }


def _csv_to_study_row(row: Dict, disease_folder: Path, mode: str) -> Dict:
    """Normalize CSV first-row dict to study table row format."""
    study_name = row.get("study") or row.get("study_name") or disease_folder.name

    target_patients = row.get("target_patients")
    control_patients = row.get("control_patients")

    concept_count = row.get("chi2y_count")
    if concept_count is None or pd.isna(concept_count):
        concept_count = row.get("z_count")
    if concept_count is None or pd.isna(concept_count):
        concept_count = row.get("significant_concepts")

    return {
        "study": study_name,
        "mode": _compact_mode_label(mode),
        "target_patients": _na_safe(target_patients),
        "control_patients": _na_safe(control_patients),
        "chi2y_count": _na_safe(concept_count),
        "z_count": _na_safe(concept_count),
    }


def load_parquet_files(disease_folder: Path) -> Dict[str, pd.DataFrame]:
    """
    Load all parquet files from a disease subfolder.
    Automatically detects patient mode vs summary mode.
    
    Args:
        disease_folder: Path to the disease subfolder
        
    Returns:
        Dictionary mapping parquet file names (without extension) to DataFrames
        Includes "_mode" key indicating "patient" or "summary"
    """
    parquet_data = {}
    disease_folder = Path(disease_folder)
    
    logger.info(f"load_parquet_files called for: {disease_folder}")
    
    # Detect data mode
    mode = detect_data_mode(disease_folder)
    parquet_data["_mode"] = mode
    logger.info(f"  Detected mode: {mode}")
    
    if mode == "summary":
        # Summary mode - load pre-computed data
        # NOTE: data_features.parquet is NOT loaded in summary mode
        # concept_summaries.parquet contains all needed data with privacy protections
        logger.info("  Loading SUMMARY mode files...")
        summary_files = [
            "complementaryMappingTable.parquet",
            "concept_summaries.parquet",
            "ordinal_summaries.parquet",
        ]
        
        # Clustering files will be loaded when needed via load_clustering_file()
        
        for parquet_file in summary_files:
            parquet_path = disease_folder / parquet_file
            if parquet_path.exists():
                try:
                    df = pd.read_parquet(parquet_path, engine=PARQUET_ENGINE)
                    key = parquet_path.stem
                    
                    # Pre-parse KDE JSON columns to avoid on-demand parsing during filter changes
                    # This improves filter performance significantly (85ms -> 0ms per filter change)
                    if parquet_file in ["concept_summaries.parquet", "ordinal_summaries.parquet"]:
                        df = deserialize_json_columns(df, columns=['time_kde_x', 'time_kde_y'])
                    
                    parquet_data[key] = df
                    logger.info(f"    Loaded {parquet_file}: {len(df)} rows")
                except Exception as e:
                    logger.error(f"    Failed to load {parquet_file}: {e}")
                    warnings.warn(f"Failed to load {parquet_path}: {e}")
        
        # Load metadata
        metadata = load_summary_metadata(disease_folder)
        if metadata:
            parquet_data["_metadata"] = metadata
        
        # Store disease_folder path for lazy loading of clustering files
        parquet_data["_disease_folder"] = disease_folder
        
        # Pre-load all available clustering files in parallel to avoid on-demand loading delays
        # This significantly improves filter/view switching performance (45s -> 0s per switch)
        try:
            from concurrent.futures import ThreadPoolExecutor, as_completed
            
            # Find available clustering files on disk
            available_k = []
            for k in range(2, 11):  # Check k values from 2 to 10
                clustering_path = disease_folder / f"clustering_k{k}_summary.parquet"
                if clustering_path.exists():
                    available_k.append(k)
            
            if available_k:
                logger.info(f"  Pre-loading {len(available_k)} clustering files in parallel: k={available_k}")
                
                def load_clustering_file_safe(k_value):
                    """Load a single clustering file, return (k, data) or (k, None) on error."""
                    try:
                        clustering_data = load_clustering_file(disease_folder, k_value, "summary")
                        return (k_value, clustering_data)
                    except Exception as e:
                        logger.warning(f"    Failed to load clustering_k{k_value}_summary.parquet: {e}")
                        return (k_value, None)
                
                # Load clustering files in parallel (I/O bound, so threads are efficient)
                with ThreadPoolExecutor(max_workers=min(4, len(available_k))) as executor:
                    futures = {executor.submit(load_clustering_file_safe, k): k for k in available_k}
                    for future in as_completed(futures):
                        k_value, clustering_data = future.result()
                        if clustering_data is not None:
                            summary_key = f"clustering_k{k_value}_summary"
                            parquet_data[summary_key] = clustering_data
                            logger.info(f"    Loaded clustering_k{k_value}_summary.parquet: {len(clustering_data)} rows")
        except ImportError:
            # Fall back to sequential loading if concurrent.futures not available
            logger.warning("  concurrent.futures not available, loading clustering files sequentially")
            for k in range(2, 11):
                clustering_path = disease_folder / f"clustering_k{k}_summary.parquet"
                if clustering_path.exists():
                    try:
                        clustering_data = load_clustering_file(disease_folder, k, "summary")
                        if clustering_data is not None:
                            parquet_data[f"clustering_k{k}_summary"] = clustering_data
                            logger.info(f"    Loaded clustering_k{k}_summary.parquet: {len(clustering_data)} rows")
                    except Exception as e:
                        logger.warning(f"    Failed to load clustering_k{k}_summary.parquet: {e}")
        except Exception as e:
            logger.warning(f"  Error pre-loading clustering files: {e}")
            # Continue without clustering files - they can be loaded on-demand if needed
            
    else:
        # Patient mode - load patient-level data
        patient_files = [
            "data_initial.parquet",
            "data_person.parquet",
            "data_patients.parquet",
            "data_features.parquet",
            "complementaryMappingTable.parquet"
        ]
        
        for parquet_file in patient_files:
            parquet_path = disease_folder / parquet_file
            if parquet_path.exists():
                try:
                    df = pd.read_parquet(parquet_path, engine=PARQUET_ENGINE)
                    key = parquet_path.stem
                    
                    # Deserialize JSON columns for data_patients only
                    # (nanoparquet serializes list columns like TIME_TO_EVENT to JSON strings)
                    if key == "data_patients":
                        df = deserialize_json_columns(df)
                    
                    logger.info(f"  Loaded {parquet_file}: {len(df)} rows")
                    parquet_data[key] = df
                except Exception as e:
                    logger.error(f"  Failed to load {parquet_file}: {e}")
                    warnings.warn(f"Failed to load {parquet_path}: {e}")
    
    return parquet_data


def load_clustering_file(disease_folder: Path, k_value: int, file_type: str = "summary") -> Optional[pd.DataFrame]:
    """
    Load a specific clustering file on demand.
    
    Args:
        disease_folder: Path to the disease subfolder
        k_value: Cluster count (k value)
        file_type: Type of file to load - "summary" or "pairwise_overlap"
        
    Returns:
        DataFrame if file exists and loads successfully, None otherwise
    """
    if file_type == "summary":
        filename = f"clustering_k{k_value}_summary.parquet"
    elif file_type == "pairwise_overlap":
        filename = f"clustering_k{k_value}_pairwise_overlap.parquet"
    else:
        return None
    
    parquet_path = disease_folder / filename
    if parquet_path.exists():
        try:
            df = pd.read_parquet(parquet_path, engine=PARQUET_ENGINE)
            logger.info(f"    Loaded {filename}: {len(df)} rows")
            return df
        except Exception as e:
            logger.error(f"    Failed to load {filename}: {e}")
            warnings.warn(f"Failed to load {parquet_path}: {e}")
            return None
    return None


def get_available_cluster_k_values(parquet_data: Dict) -> list:
    """
    Get list of available pre-computed cluster k values.
    Checks both loaded files and files on disk.
    
    Args:
        parquet_data: Dictionary from load_parquet_files
        
    Returns:
        List of available k values (empty for patient mode)
    """
    if parquet_data.get("_mode") != "summary":
        return []
    
    # First check already loaded files
    k_values = set()
    for key in parquet_data.keys():
        if key.startswith("clustering_k") and key.endswith("_summary"):
            try:
                k = int(key.replace("clustering_k", "").replace("_summary", ""))
                k_values.add(k)
            except ValueError:
                pass
    
    # Also check disk for files that weren't loaded yet
    disease_folder = parquet_data.get("_disease_folder")
    if disease_folder and Path(disease_folder).exists():
        for k in range(2, 11):
            if (Path(disease_folder) / f"clustering_k{k}_summary.parquet").exists():
                k_values.add(k)
    
    return sorted(k_values)


def load_study_summaries(data_dir: Path) -> pd.DataFrame:
    """
    Load study summary rows from all disease subfolders.
    Uses metadata.json when available (summary and patient mode), with CSV fallback.
    
    Args:
        data_dir: Path to the results_parquet/ directory
        
    Returns:
        DataFrame with columns: study, mode, target_patients, control_patients,
        chi2y_count, z_count
    """
    study_data = []
    out_cols = ["study", "mode", "target_patients", "control_patients", "chi2y_count", "z_count"]
    
    if not data_dir.exists():
        warnings.warn(f"Data directory {data_dir} does not exist!")
        return pd.DataFrame(columns=out_cols)
    
    # Iterate through immediate subdirectories only
    for disease_folder in data_dir.iterdir():
        if not disease_folder.is_dir():
            continue
        
        # Use detected data mode, not just metadata existence.
        mode = detect_data_mode(disease_folder)
        metadata_path = disease_folder / "metadata.json"

        # Prefer metadata.json when present (both summary and patient modes).
        if metadata_path.exists():
            try:
                metadata = load_summary_metadata(disease_folder)
                if metadata:
                    row_data = _metadata_to_study_row(metadata, disease_folder, mode)
                    if row_data:
                        study_data.append(row_data)
                        continue
            except Exception as e:
                warnings.warn(f"Failed to load metadata from {metadata_path}: {e}. Falling back to CSV.")
        
        # Fallback to CSV file
        csv_files = list(disease_folder.glob("*.csv"))
        
        if not csv_files:
            warnings.warn(f"No study metadata found in {disease_folder} (mode: {mode}). Skipping.")
            continue
        
        # Use the first CSV file found
        csv_path = csv_files[0]
        
        try:
            df = pd.read_csv(csv_path)
            row = _csv_to_study_row(df.iloc[0].to_dict(), disease_folder, mode)
            study_data.append(row)
        except Exception as e:
            warnings.warn(f"Failed to load CSV from {csv_path}: {e}")
            continue
    
    if not study_data:
        return pd.DataFrame(columns=out_cols)
    
    return pd.DataFrame(study_data)
