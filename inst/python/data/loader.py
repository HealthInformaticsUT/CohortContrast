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
        
        # Also load clustering files
        for k in [2, 3, 4, 5, 6, 7, 8, 9, 10]:
            summary_files.append(f"clustering_k{k}_summary.parquet")
            summary_files.append(f"clustering_k{k}_pairwise_overlap.parquet")
        
        for parquet_file in summary_files:
            parquet_path = disease_folder / parquet_file
            if parquet_path.exists():
                try:
                    df = pd.read_parquet(parquet_path, engine=PARQUET_ENGINE)
                    key = parquet_path.stem
                    parquet_data[key] = df
                    logger.info(f"    Loaded {parquet_file}: {len(df)} rows")
                except Exception as e:
                    logger.error(f"    Failed to load {parquet_file}: {e}")
                    warnings.warn(f"Failed to load {parquet_path}: {e}")
        
        # Load metadata
        metadata = load_summary_metadata(disease_folder)
        if metadata:
            parquet_data["_metadata"] = metadata
            
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


def get_available_cluster_k_values(parquet_data: Dict) -> list:
    """
    Get list of available pre-computed cluster k values.
    
    Args:
        parquet_data: Dictionary from load_parquet_files
        
    Returns:
        List of available k values (empty for patient mode)
    """
    if parquet_data.get("_mode") != "summary":
        return []
    
    k_values = []
    for key in parquet_data.keys():
        if key.startswith("clustering_k") and key.endswith("_summary"):
            try:
                k = int(key.replace("clustering_k", "").replace("_summary", ""))
                k_values.append(k)
            except ValueError:
                pass
    
    return sorted(k_values)


def load_study_summaries(data_dir: Path) -> pd.DataFrame:
    """
    Load CSV summary files from all disease subfolders.
    
    Args:
        data_dir: Path to the results_parquet/ directory
        
    Returns:
        DataFrame with columns: study, target_patients, control_patients, z_count
    """
    study_data = []
    
    if not data_dir.exists():
        warnings.warn(f"Data directory {data_dir} does not exist!")
        return pd.DataFrame(columns=["study", "target_patients", "control_patients", "z_count"])
    
    # Iterate through immediate subdirectories only
    for disease_folder in data_dir.iterdir():
        if not disease_folder.is_dir():
            continue
            
        # Find the first CSV file in this subfolder
        csv_files = list(disease_folder.glob("*.csv"))
        
        if not csv_files:
            warnings.warn(f"No CSV file found in {disease_folder}. Skipping.")
            continue
        
        # Use the first CSV file found
        csv_path = csv_files[0]
        
        try:
            df = pd.read_csv(csv_path)
            # Ensure the CSV has the expected columns
            if "study" in df.columns:
                study_data.append(df.iloc[0].to_dict())
            else:
                # If no 'study' column, use folder name
                row = df.iloc[0].to_dict()
                row["study"] = disease_folder.name
                study_data.append(row)
        except Exception as e:
            warnings.warn(f"Failed to load CSV from {csv_path}: {e}")
            continue
    
    if not study_data:
        return pd.DataFrame(columns=["study", "target_patients", "control_patients", "z_count"])
    
    return pd.DataFrame(study_data)

