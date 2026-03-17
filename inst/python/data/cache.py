"""
Data caching utilities for ContrastViewer.

Provides centralized data loading with multi-level caching:
1. In-memory cache (fastest, process-local)
2. Disk cache (shared across background callback processes)
3. File system (fallback, loads from parquet files)

This solves the problem where background callbacks run in separate processes
and cannot share the in-memory `loaded_parquet_data` dictionary.
"""

from pathlib import Path
from typing import Dict, Optional
import logging
import hashlib

import pandas as pd
import diskcache

from data.loader import load_parquet_files, load_clustering_file

logger = logging.getLogger('ContrastViewer.cache')

# Global in-memory cache (process-local, fastest)
_in_memory_cache: Dict[str, Dict[str, pd.DataFrame]] = {}
_in_memory_signatures: Dict[str, str] = {}

# Disk cache instance (shared across processes)
_disk_cache: Optional[diskcache.Cache] = None

# Cache key version for forward-compatible key migrations.
_CACHE_KEY_VERSION = "v2"


def initialize_disk_cache(cache_dir: str = "./cache") -> diskcache.Cache:
    """
    Initialize the disk cache for data sharing across processes.
    
    Args:
        cache_dir: Directory for disk cache
        
    Returns:
        DiskCache instance
    """
    global _disk_cache
    if _disk_cache is None:
        _disk_cache = diskcache.Cache(cache_dir)
        logger.info(f"Initialized disk cache at {cache_dir}")
    return _disk_cache


def _normalize_data_dir(data_dir: Optional[Path]) -> str:
    """
    Normalize data directory into a stable absolute path string.

    Args:
        data_dir: Data directory path

    Returns:
        Absolute normalized path string
    """
    if data_dir is None:
        return "default-data-dir"
    try:
        return str(Path(data_dir).expanduser().resolve())
    except Exception:
        return str(data_dir)


def _compute_data_dir_namespace(data_dir: Optional[Path]) -> str:
    """
    Compute a short namespace hash for a data directory.

    This prevents cache collisions when the same study name exists in
    multiple directories.
    """
    normalized = _normalize_data_dir(data_dir)
    return hashlib.md5(normalized.encode()).hexdigest()[:12]


def _compute_memory_key(study_name: str, data_dir: Optional[Path]) -> str:
    """
    Compute in-memory cache key scoped by study name and data directory.
    """
    namespace = _compute_data_dir_namespace(data_dir)
    return f"{namespace}:{study_name}"


def _compute_cache_key(study_name: str, file_type: str = "all", data_dir: Optional[Path] = None) -> str:
    """
    Compute a cache key for a study's data.
    
    Args:
        study_name: Name of the study
        file_type: Type of data ("all", "clustering_k3", etc.)
        
    Returns:
        Cache key string
    """
    namespace = _compute_data_dir_namespace(data_dir)
    return f"parquet_data:{_CACHE_KEY_VERSION}:{namespace}:{study_name}:{file_type}"


def _compute_study_signature(study_folder: Path) -> str:
    """
    Compute a deterministic signature of parquet files in a study folder.

    Uses relative path, size, and nanosecond mtime to invalidate stale cache
    entries when source data changes.
    """
    parquet_files = sorted(study_folder.rglob("*.parquet"))
    hasher = hashlib.md5()
    for parquet_file in parquet_files:
        try:
            stat = parquet_file.stat()
            rel_path = parquet_file.relative_to(study_folder)
            hasher.update(str(rel_path).encode())
            hasher.update(str(stat.st_size).encode())
            hasher.update(str(stat.st_mtime_ns).encode())
        except OSError:
            # If file disappears mid-scan, continue and let subsequent reload handle it.
            continue
    return hasher.hexdigest()


def get_or_load_parquet_data(
    study_name: str,
    data_dir: Path,
    disk_cache: Optional[diskcache.Cache] = None,
    force_reload: bool = False
) -> Optional[Dict[str, pd.DataFrame]]:
    """
    Get parquet data from cache or load from disk.
    
    Uses a three-tier caching strategy:
    1. Check in-memory cache (fastest, process-local)
    2. Check disk cache (shared across processes, for background callbacks)
    3. Load from file system and cache
    
    Args:
        study_name: Name of the study to load
        data_dir: Base directory containing study folders
        disk_cache: Optional diskcache instance (if None, uses global)
        force_reload: If True, bypass cache and reload from disk
        
    Returns:
        Dictionary of DataFrames keyed by file type, or None if study not found
    """
    global _in_memory_cache, _in_memory_signatures, _disk_cache
    
    # Initialize disk cache if not provided
    if disk_cache is None:
        disk_cache = _disk_cache
        if disk_cache is None:
            disk_cache = initialize_disk_cache()
    
    study_folder = data_dir / study_name
    if not study_folder.exists():
        logger.warning(f"Study folder not found: {study_folder}")
        return None

    study_signature = _compute_study_signature(study_folder)
    memory_key = _compute_memory_key(study_name, data_dir)
    cache_key = _compute_cache_key(study_name, "all", data_dir=data_dir)
    
    # Step 1: Check in-memory cache (fastest)
    if not force_reload and memory_key in _in_memory_cache:
        cached_signature = _in_memory_signatures.get(memory_key)
        if cached_signature == study_signature:
            return _in_memory_cache[memory_key]
        logger.info(f"Cache invalidated (memory): {study_name} (source data changed)")
        _in_memory_cache.pop(memory_key, None)
        _in_memory_signatures.pop(memory_key, None)
    
    # Step 2: Check disk cache (shared across processes)
    if not force_reload:
        try:
            cached_data = disk_cache.get(cache_key)
            if cached_data is not None:
                cached_payload = cached_data.get("data") if isinstance(cached_data, dict) and "data" in cached_data else cached_data
                cached_signature = cached_data.get("signature") if isinstance(cached_data, dict) else None
                if cached_signature is None or cached_signature == study_signature:
                    logger.info(f"Cache hit (disk): {study_name}")
                    # Also store in in-memory cache for faster future access
                    _in_memory_cache[memory_key] = cached_payload
                    _in_memory_signatures[memory_key] = study_signature
                    return cached_payload
                logger.info(f"Cache invalidated (disk): {study_name} (source data changed)")
        except Exception as e:
            logger.warning(f"Error reading from disk cache: {e}")
    
    # Step 3: Load from file system
    logger.info(f"Loading parquet files from disk: {study_name}")
    try:
        parquet_data = load_parquet_files(study_folder)
        
        # Store in both caches
        _in_memory_cache[memory_key] = parquet_data
        _in_memory_signatures[memory_key] = study_signature
        try:
            # Store in disk cache with a reasonable expiration (24 hours)
            disk_cache.set(
                cache_key,
                {"data": parquet_data, "signature": study_signature},
                expire=86400
            )
        except Exception as e:
            logger.warning(f"Error writing to disk cache: {e}")
        
        return parquet_data
    except Exception as e:
        logger.error(f"Error loading parquet files for {study_name}: {e}")
        return None


def get_or_load_clustering_file(
    study_name: str,
    data_dir: Path,
    k_value: int,
    file_type: str = "summary",
    disk_cache: Optional[diskcache.Cache] = None,
    force_reload: bool = False
) -> Optional[pd.DataFrame]:
    """
    Get clustering file from cache or load from disk.
    
    Args:
        study_name: Name of the study
        data_dir: Base directory containing study folders
        k_value: Cluster count (k value)
        file_type: Type of clustering file ("summary" or "pairwise_overlap")
        disk_cache: Optional diskcache instance
        force_reload: If True, bypass cache and reload from disk
        
    Returns:
        DataFrame with clustering data, or None if not found
    """
    global _in_memory_cache, _in_memory_signatures, _disk_cache
    
    if disk_cache is None:
        disk_cache = _disk_cache
        if disk_cache is None:
            disk_cache = initialize_disk_cache()
    
    study_folder = data_dir / study_name
    if not study_folder.exists():
        return None

    study_signature = _compute_study_signature(study_folder)
    memory_key = _compute_memory_key(study_name, data_dir)
    summary_key = f"clustering_k{k_value}_{file_type}"

    # Check if study data is already loaded in-memory with matching signature
    if memory_key in _in_memory_cache:
        if _in_memory_signatures.get(memory_key) == study_signature:
            parquet_data = _in_memory_cache[memory_key]
            if summary_key in parquet_data:
                return parquet_data[summary_key]
        else:
            _in_memory_cache.pop(memory_key, None)
            _in_memory_signatures.pop(memory_key, None)
    
    # Check disk cache
    cache_key = _compute_cache_key(study_name, f"clustering_k{k_value}_{file_type}", data_dir=data_dir)
    if not force_reload:
        try:
            cached_data = disk_cache.get(cache_key)
            if cached_data is not None:
                cached_payload = cached_data.get("data") if isinstance(cached_data, dict) and "data" in cached_data else cached_data
                cached_signature = cached_data.get("signature") if isinstance(cached_data, dict) else None
                if cached_signature is None or cached_signature == study_signature:
                    # Also add to in-memory cache if study data exists
                    if memory_key in _in_memory_cache:
                        _in_memory_cache[memory_key][summary_key] = cached_payload
                        _in_memory_signatures[memory_key] = study_signature
                    return cached_payload
        except Exception as e:
            logger.warning(f"Error reading clustering file from disk cache: {e}")
    
    # Load from file system
    logger.info(f"Loading clustering file from disk: {study_name}, k={k_value}, type={file_type}")
    try:
        clustering_data = load_clustering_file(study_folder, k_value, file_type)
        
        if clustering_data is not None:
            # Store in in-memory cache
            if memory_key not in _in_memory_cache:
                _in_memory_cache[memory_key] = {}
            _in_memory_cache[memory_key][summary_key] = clustering_data
            _in_memory_signatures[memory_key] = study_signature
            
            # Store in disk cache
            try:
                disk_cache.set(
                    cache_key,
                    {"data": clustering_data, "signature": study_signature},
                    expire=86400
                )
            except Exception as e:
                logger.warning(f"Error writing clustering file to disk cache: {e}")
        
        return clustering_data
    except Exception as e:
        logger.error(f"Error loading clustering file: {e}")
        return None


def update_cache(
    study_name: str,
    parquet_data: Dict[str, pd.DataFrame],
    disk_cache: Optional[diskcache.Cache] = None,
    data_dir: Optional[Path] = None,
) -> None:
    """
    Update both in-memory and disk caches with new data.
    
    Useful when data is modified or when clustering files are loaded on demand.
    
    Args:
        study_name: Name of the study
        parquet_data: Dictionary of DataFrames to cache
        disk_cache: Optional diskcache instance
    """
    global _in_memory_cache, _in_memory_signatures, _disk_cache
    
    if disk_cache is None:
        disk_cache = _disk_cache
        if disk_cache is None:
            disk_cache = initialize_disk_cache()
    
    memory_key = _compute_memory_key(study_name, data_dir)

    # Update in-memory cache
    _in_memory_cache[memory_key] = parquet_data
    study_signature = None
    if data_dir is not None:
        study_folder = Path(data_dir) / study_name
        if study_folder.exists():
            study_signature = _compute_study_signature(study_folder)
            _in_memory_signatures[memory_key] = study_signature
    
    # Update disk cache
    cache_key = _compute_cache_key(study_name, "all", data_dir=data_dir)
    try:
        disk_cache.set(
            cache_key,
            {"data": parquet_data, "signature": study_signature},
            expire=86400
        )
    except Exception as e:
        logger.warning(f"Error updating disk cache: {e}")


def clear_cache(study_name: Optional[str] = None, data_dir: Optional[Path] = None) -> None:
    """
    Clear cache for a specific study or all studies.
    
    Args:
        study_name: If provided, clear only this study. If None, clear all.
    """
    global _in_memory_cache, _in_memory_signatures, _disk_cache
    
    if study_name:
        memory_key = _compute_memory_key(study_name, data_dir)
        if memory_key in _in_memory_cache:
            del _in_memory_cache[memory_key]
        _in_memory_signatures.pop(memory_key, None)
        
        if _disk_cache:
            try:
                _disk_cache.delete(_compute_cache_key(study_name, "all", data_dir=data_dir))
            except Exception:
                pass
    else:
        _in_memory_cache.clear()
        _in_memory_signatures.clear()
        if _disk_cache:
            _disk_cache.clear()
