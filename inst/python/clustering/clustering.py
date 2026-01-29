"""
Patient clustering functions.
"""

from typing import Dict, List, Optional, Tuple
import hashlib
import json
import numpy as np
import pandas as pd
from sklearn.decomposition import PCA
from sklearn.preprocessing import StandardScaler
from sklearn_extra.cluster import KMedoids
from sklearn.metrics import silhouette_score

from config.constants import CLUSTER_COLORS
from utils.helpers import normalize_concept_id, get_unique_occurrences
from models.registry import concept_registry


# =============================================================================
# Clustering Cache
# =============================================================================

_clustering_cache: Dict[str, Dict] = {}
_MAX_CACHE_SIZE = 10  # Keep last 10 clustering results


def _compute_cache_key(
    data_patients: pd.DataFrame,
    dashboard_data: List[Dict],
    concept_limit: int,
    cluster_range: Tuple[int, int],
    time_window: int,
    k_value: Optional[int]
) -> str:
    """
    Compute a hash key for caching clustering results.
    
    Args:
        data_patients: Patient data DataFrame
        dashboard_data: List of concept dictionaries
        concept_limit: Number of concepts to use
        cluster_range: Range of cluster counts to try
        time_window: Time window for events
        k_value: Fixed k value (if any)
        
    Returns:
        Hash string for cache lookup
    """
    # Create a hashable representation of inputs
    hash_parts = []
    
    # Hash data_patients shape and sample of content
    hash_parts.append(f"patients:{len(data_patients)}")
    if not data_patients.empty:
        # Use sorted unique concept IDs and patient count as proxy
        concept_ids = sorted(data_patients['CONCEPT_ID'].astype(str).unique()[:100].tolist())
        hash_parts.append(f"concepts:{','.join(concept_ids)}")
        patient_count = data_patients['PERSON_ID'].nunique()
        hash_parts.append(f"patient_count:{patient_count}")
    
    # Hash dashboard_data (concept IDs that are active)
    active_ids = sorted([
        str(item.get('CONCEPT_ID') or item.get('_concept_id', ''))
        for item in dashboard_data
        if item.get('_show', True)
    ][:100])
    hash_parts.append(f"active:{','.join(active_ids)}")
    
    # Hash parameters
    hash_parts.append(f"limit:{concept_limit}")
    hash_parts.append(f"range:{cluster_range}")
    hash_parts.append(f"window:{time_window}")
    hash_parts.append(f"k:{k_value}")
    
    # Compute hash
    hash_string = "|".join(hash_parts)
    return hashlib.md5(hash_string.encode()).hexdigest()


def clear_clustering_cache():
    """Clear the clustering cache."""
    global _clustering_cache
    _clustering_cache.clear()


def perform_patient_clustering(
    data_patients: pd.DataFrame,
    data_initial: pd.DataFrame,
    dashboard_data: List[Dict],
    concept_limit: int = 60,
    cluster_range: Tuple[int, int] = (2, 5),
    pca_components: int = 20,
    time_window: int = 180,  # Kept for API compatibility but no longer used for filtering
    k_value: Optional[int] = None
) -> Dict:
    """
    Perform patient clustering based on concept occurrence patterns.
    
    Results are cached based on input parameters to avoid redundant computation.
    
    NOTE: All events are included regardless of time - no time_window filtering
    is applied. This ensures prevalence calculations are consistent across
    the application (composite plot, trajectories tab, etc.).
    
    Args:
        data_patients: DataFrame with patient-concept data
        data_initial: DataFrame with patient index dates
        dashboard_data: List of concept dictionaries with metadata
        concept_limit: Number of most prevalent concepts to use
        cluster_range: (min_k, max_k) for cluster count optimization
        pca_components: Number of PCA components
        time_window: Deprecated - kept for API compatibility, no longer used
        k_value: Fixed k value (if None, optimize using silhouette)
        
    Returns:
        Dictionary with summary_matrix, patient_assignments, best_silhouette_score, optimal_cluster_count
    """
    global _clustering_cache
    
    # Check cache first
    cache_key = _compute_cache_key(
        data_patients, dashboard_data, concept_limit, 
        cluster_range, time_window, k_value
    )
    
    if cache_key in _clustering_cache:
        return _clustering_cache[cache_key]
    
    from sklearn.decomposition import PCA
    from sklearn.preprocessing import StandardScaler
    from sklearn_extra.cluster import KMedoids
    from sklearn.metrics import silhouette_score
    
    # Step 1: Expand event times (vectorized)
    # IMPORTANT: Only use target cohort patients for clustering
    # NOTE: No time_window filtering - use ALL events for consistent prevalence calculations
    df_target_clustering = data_patients[data_patients["COHORT_DEFINITION_ID"] == "target"].copy()
    
    if df_target_clustering.empty:
        return {
            'summary_matrix': pd.DataFrame(),
            'patient_assignments': pd.DataFrame(),
            'best_silhouette_score': 0.0,
            'optimal_cluster_count': 2
        }
    
    # Extract all unique occurrences (no time window filtering)
    df_target_clustering["VALID_TIMES"] = df_target_clustering["TIME_TO_EVENT"].apply(get_unique_occurrences)
    df_target_clustering["CONCEPT_ID"] = df_target_clustering["CONCEPT_ID"].astype(str)
    
    # Filter out rows with no valid times
    df_target_clustering = df_target_clustering[df_target_clustering["VALID_TIMES"].apply(len) > 0].copy()
    
    if df_target_clustering.empty:
        return {
            'summary_matrix': pd.DataFrame(),
            'patient_assignments': pd.DataFrame(),
            'best_silhouette_score': 0.0,
            'optimal_cluster_count': 2
        }
    
    # Explode to get one row per event (vectorized)
    events_df = df_target_clustering[["PERSON_ID", "CONCEPT_ID", "VALID_TIMES"]].explode("VALID_TIMES")
    events_df = events_df.rename(columns={"VALID_TIMES": "TIME_OFFSET"})
    events_df["TIME_OFFSET"] = events_df["TIME_OFFSET"].astype(float)
    events_df["PERSON_ID"] = events_df["PERSON_ID"].astype(int)
    events_df = events_df.dropna(subset=["TIME_OFFSET"])
    
    if events_df.empty:
        return {
            'summary_matrix': pd.DataFrame(),
            'patient_assignments': pd.DataFrame(),
            'best_silhouette_score': 0.0,
            'optimal_cluster_count': 2
        }
    
    # Step 2: Filter to only include concepts that are in dashboard_data (active concepts)
    # Get concept IDs from dashboard_data
    active_concept_ids = set()
    for item in dashboard_data:
        concept_id = item.get('CONCEPT_ID') or item.get('_concept_id')
        if concept_id is not None:
            # Normalize to remove .0 suffix for consistent matching
            norm_id = str(concept_id).replace('.0', '')
            active_concept_ids.add(norm_id)
    
    
    # Filter events to only include active concepts
    if active_concept_ids:
        # Convert CONCEPT_ID to string for comparison
        events_df['CONCEPT_ID_STR'] = events_df['CONCEPT_ID'].astype(str).str.replace('.0', '', regex=False)
        events_df = events_df[events_df['CONCEPT_ID_STR'].isin(active_concept_ids)].copy()
    
    if events_df.empty:
        return {
            'summary_matrix': pd.DataFrame(),
            'patient_assignments': pd.DataFrame(),
            'best_silhouette_score': 0.0,
            'optimal_cluster_count': 2
        }
    
    # Select top N most prevalent concepts from the active ones
    concept_counts = events_df.groupby('CONCEPT_ID')['PERSON_ID'].nunique().sort_values(ascending=False)
    top_concepts = concept_counts.head(concept_limit).index.tolist()
    
    if not top_concepts:
        return {
            'summary_matrix': pd.DataFrame(),
            'patient_assignments': pd.DataFrame(),
            'best_silhouette_score': 0.0,
            'optimal_cluster_count': 2
        }
    
    # Step 3: Build patient × concept feature matrix (vectorized)
    all_patients = sorted(events_df['PERSON_ID'].unique())
    
    # Get concept weights from dashboard_data
    concept_weights = {}
    for item in dashboard_data:
        concept_id = str(item.get('CONCEPT_ID') or item.get('_concept_id', ''))
        if concept_id:
            target_prev = item.get('TARGET_SUBJECT_PREVALENCE', 0) / 100.0 if item.get('TARGET_SUBJECT_PREVALENCE') else 0
            ratio = item.get('PREVALENCE_DIFFERENCE_RATIO', 1.0) if item.get('PREVALENCE_DIFFERENCE_RATIO') else 1.0
            
            # Normalize ratio
            if ratio < 1:
                norm_ratio = 0
            elif ratio > 100:
                norm_ratio = 2
            else:
                norm_ratio = np.log10(ratio) if ratio > 0 else 0
            
            weight = target_prev * (1 + norm_ratio) if target_prev > 0 else 1.0
            concept_weights[concept_id] = weight
    
    # Filter events to top concepts only
    top_concepts_set = set(top_concepts)
    events_top = events_df[events_df['CONCEPT_ID'].isin(top_concepts_set)].copy()
    
    # Calculate aggregates using groupby (vectorized)
    # Group by patient and concept
    grouped = events_top.groupby(['PERSON_ID', 'CONCEPT_ID'])
    
    # Count: number of events
    count_df = grouped.size().reset_index(name='count')
    
    # First time: minimum TIME_OFFSET
    first_time_df = grouped['TIME_OFFSET'].min().reset_index(name='first_time')
    
    # Merge counts and first times
    stats_df = count_df.merge(first_time_df, on=['PERSON_ID', 'CONCEPT_ID'])
    
    # Calculate derived features
    stats_df['log_count'] = np.log1p(stats_df['count'])
    stats_df['earliness'] = 1.0 / (1.0 + stats_df['first_time'] / 30.0)
    stats_df['presence'] = 1.0
    
    # Apply concept weights
    stats_df['weight'] = stats_df['CONCEPT_ID'].map(concept_weights).fillna(1.0)
    stats_df['presence_weighted'] = stats_df['presence'] * stats_df['weight']
    stats_df['log_count_weighted'] = stats_df['log_count'] * stats_df['weight']
    stats_df['earliness_weighted'] = stats_df['earliness'] * stats_df['weight']
    
    # Pivot to create feature matrix
    # Create separate pivots for each feature type
    presence_pivot = stats_df.pivot_table(
        index='PERSON_ID', 
        columns='CONCEPT_ID', 
        values='presence_weighted',
        fill_value=0.0
    )
    presence_pivot.columns = [f'presence_{c}' for c in presence_pivot.columns]
    
    logcount_pivot = stats_df.pivot_table(
        index='PERSON_ID', 
        columns='CONCEPT_ID', 
        values='log_count_weighted',
        fill_value=0.0
    )
    logcount_pivot.columns = [f'logcount_{c}' for c in logcount_pivot.columns]
    
    earliness_pivot = stats_df.pivot_table(
        index='PERSON_ID', 
        columns='CONCEPT_ID', 
        values='earliness_weighted',
        fill_value=0.0
    )
    earliness_pivot.columns = [f'earliness_{c}' for c in earliness_pivot.columns]
    
    # Combine all features
    feature_df = pd.concat([presence_pivot, logcount_pivot, earliness_pivot], axis=1)
    
    # Ensure all patients are included (fill missing with 0)
    all_patients_index = pd.Index(all_patients, name='PERSON_ID')
    feature_df = feature_df.reindex(all_patients_index, fill_value=0.0)
    
    # Remove constant/zero-variance columns
    feature_df = feature_df.loc[:, feature_df.std() > 1e-10]
    
    if feature_df.empty or len(feature_df.columns) == 0:
        return {
            'summary_matrix': pd.DataFrame(),
            'patient_assignments': pd.DataFrame(),
            'best_silhouette_score': 0.0,
            'optimal_cluster_count': 2
        }
    
    # Step 4: PCA dimensionality reduction
    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(feature_df)
    
    n_components = min(pca_components, len(feature_df.columns), len(feature_df) - 1)
    if n_components < 1:
        n_components = 1
    
    pca = PCA(n_components=n_components)
    X_pca = pca.fit_transform(X_scaled)
    
    # Step 5: Find optimal k or use provided k
    if k_value is not None:
        optimal_k = k_value
        best_silhouette = None
    else:
        min_k, max_k = cluster_range
        silhouette_scores = []
        
        for k in range(min_k, max_k + 1):
            if k >= len(all_patients):
                break
            try:
                kmedoids = KMedoids(n_clusters=k, random_state=42, method='pam')
                labels = kmedoids.fit_predict(X_pca)
                if len(set(labels)) > 1:  # Need at least 2 clusters
                    sil_score = silhouette_score(X_pca, labels)
                    silhouette_scores.append((k, sil_score))
            except Exception:
                continue
        
        if silhouette_scores:
            optimal_k, best_silhouette = max(silhouette_scores, key=lambda x: x[1])
        else:
            optimal_k = min_k
            best_silhouette = 0.0
    
    # Step 6: Final clustering
    if optimal_k >= len(all_patients):
        optimal_k = max(2, len(all_patients) - 1)
    
    kmedoids = KMedoids(n_clusters=optimal_k, random_state=42, method='pam')
    cluster_labels = kmedoids.fit_predict(X_pca)
    
    # Create patient assignments
    patient_assignments = pd.DataFrame({
        'patient_id': all_patients,
        'cluster': [f'C{i+1}' for i in cluster_labels]
    })
    
    # Step 7: Build summary matrix (vectorized)
    # Build concept name lookup from dashboard_data
    concept_name_map = {}
    for item in dashboard_data:
        cid = str(item.get('CONCEPT_ID') or item.get('_concept_id', ''))
        if cid:
            concept_name_map[cid] = item.get('CONCEPT_NAME', cid)
    
    # Add cluster assignment to events
    events_with_cluster = events_df.merge(
        patient_assignments,
        left_on='PERSON_ID',
        right_on='patient_id',
        how='left'
    )
    
    # Filter to top concepts
    events_with_cluster = events_with_cluster[events_with_cluster['CONCEPT_ID'].isin(top_concepts)]
    
    # Calculate statistics per concept-cluster combination
    if events_with_cluster.empty:
        summary_matrix = pd.DataFrame(columns=['concept_id', 'concept_name', 'cluster', 'prevalence', 'count_category', 'median_days'])
    else:
        # Group by concept and cluster
        grouped = events_with_cluster.groupby(['CONCEPT_ID', 'cluster'])
        
        # Count unique patients per concept-cluster
        patient_counts = grouped['PERSON_ID'].nunique().reset_index(name='patients_with_concept')
        
        # Calculate first occurrence per patient, then median
        first_occurrence = events_with_cluster.groupby(['CONCEPT_ID', 'cluster', 'PERSON_ID'])['TIME_OFFSET'].min()
        median_days_df = first_occurrence.groupby(['CONCEPT_ID', 'cluster']).median().reset_index(name='median_days')
        
        # Calculate occurrence count per patient, then median
        occurrence_counts = events_with_cluster.groupby(['CONCEPT_ID', 'cluster', 'PERSON_ID']).size()
        median_count_df = occurrence_counts.groupby(['CONCEPT_ID', 'cluster']).median().reset_index(name='median_count')
        
        # Merge all statistics
        summary_stats = patient_counts.merge(median_days_df, on=['CONCEPT_ID', 'cluster'], how='outer')
        summary_stats = summary_stats.merge(median_count_df, on=['CONCEPT_ID', 'cluster'], how='outer')
        
        # Calculate total patients per cluster
        cluster_totals = patient_assignments.groupby('cluster')['patient_id'].count().reset_index(name='total_patients')
        summary_stats = summary_stats.merge(cluster_totals, on='cluster', how='left')
        
        # Calculate prevalence
        summary_stats['prevalence'] = summary_stats['patients_with_concept'] / summary_stats['total_patients']
        summary_stats['prevalence'] = summary_stats['prevalence'].fillna(0.0)
        
        # Calculate count category
        def get_count_category(median_count):
            if pd.isna(median_count) or median_count <= 1:
                return "1"
            elif median_count <= 2:
                return "2"
            else:
                return "3+"
        
        summary_stats['count_category'] = summary_stats['median_count'].apply(get_count_category)
        
        # Add concept names
        summary_stats['concept_id'] = summary_stats['CONCEPT_ID']
        summary_stats['concept_name'] = summary_stats['CONCEPT_ID'].map(concept_name_map).fillna(summary_stats['CONCEPT_ID'])
        
        # Ensure all concept-cluster combinations exist
        all_combinations = pd.DataFrame([
            {'CONCEPT_ID': cid, 'cluster': f'C{i+1}'} 
            for cid in top_concepts 
            for i in range(optimal_k)
        ])
        
        summary_matrix = all_combinations.merge(
            summary_stats[['CONCEPT_ID', 'cluster', 'prevalence', 'count_category', 'median_days', 'concept_name']],
            on=['CONCEPT_ID', 'cluster'],
            how='left'
        )
        summary_matrix['prevalence'] = summary_matrix['prevalence'].fillna(0.0)
        summary_matrix['count_category'] = summary_matrix['count_category'].fillna("1")
        summary_matrix['concept_id'] = summary_matrix['CONCEPT_ID']
        summary_matrix['concept_name'] = summary_matrix['CONCEPT_ID'].map(concept_name_map).fillna(summary_matrix['CONCEPT_ID'])
        summary_matrix = summary_matrix[['concept_id', 'concept_name', 'cluster', 'prevalence', 'count_category', 'median_days']]
    
    # Step 8: Add ALL main concepts from dashboard_data to summary matrix (not just top_concepts)
    # This ensures all active main concepts are included in the clustering plot
    # Note: Ordinals are handled separately after this step
    # Normalize concept IDs (remove .0 suffix) for consistent comparison
    all_concept_ids_in_summary = set(str(cid).replace('.0', '') for cid in summary_matrix['concept_id'].unique())
    
    # Get all unique MAIN concept IDs from dashboard_data (skip ordinals)
    all_dashboard_concept_ids = set()
    for item in dashboard_data:
        is_ordinal = item.get('IS_ORDINAL', False)
        if is_ordinal:
            continue  # Skip ordinals - they're handled separately
        concept_id = item.get('CONCEPT_ID') or item.get('_concept_id')
        if concept_id is not None:
            # Normalize to remove .0 suffix
            norm_id = str(concept_id).replace('.0', '')
            all_dashboard_concept_ids.add(norm_id)
    
    
    # Add missing main concepts to summary matrix
    missing_concept_ids = all_dashboard_concept_ids - all_concept_ids_in_summary
    
    if missing_concept_ids:
        # Collect additional rows for missing concepts
        additional_rows = []
        
        for concept_id in missing_concept_ids:
            # Try multiple ID formats for matching
            concept_events = events_df[events_df['CONCEPT_ID'].astype(str) == str(concept_id)]
            if concept_events.empty:
                # Try without .0
                norm_id = str(concept_id).replace('.0', '')
                concept_events = events_df[events_df['CONCEPT_ID'].astype(str).str.replace('.0', '', regex=False) == norm_id]
            
            
            for cluster_label in [f'C{i+1}' for i in range(optimal_k)]:
                cluster_patients = patient_assignments[patient_assignments['cluster'] == cluster_label]['patient_id'].tolist()
                cluster_events = concept_events[concept_events['PERSON_ID'].isin(cluster_patients)]
                
                # Prevalence
                patients_with_concept = cluster_events['PERSON_ID'].nunique()
                total_patients = len(cluster_patients)
                prevalence = patients_with_concept / total_patients if total_patients > 0 else 0.0
                
                # Median days and count
                if len(cluster_events) > 0:
                    median_days = cluster_events.groupby('PERSON_ID')['TIME_OFFSET'].min().median()
                    median_count = cluster_events.groupby('PERSON_ID').size().median()
                    
                    if median_count <= 1:
                        count_category = "1"
                    elif median_count <= 2:
                        count_category = "2"
                    else:
                        count_category = "3+"
                else:
                    median_days = np.nan
                    median_count = 0
                    count_category = "1"
                
                # Get concept name from dashboard_data
                concept_name = concept_id
                for item in dashboard_data:
                    item_concept_id = str(item.get('CONCEPT_ID') or item.get('_concept_id', '')).replace('.0', '')
                    if item_concept_id == concept_id:
                        concept_name = item.get('CONCEPT_NAME', concept_id)
                        break
                
                additional_rows.append({
                    'concept_id': concept_id,
                    'concept_name': concept_name,
                    'cluster': cluster_label,
                    'prevalence': prevalence,
                    'count_category': count_category,
                    'median_days': median_days
                })
        
        # Append additional rows to summary matrix
        if additional_rows:
            additional_df = pd.DataFrame(additional_rows)
            summary_matrix = pd.concat([summary_matrix, additional_df], ignore_index=True)
    
    # Populate the global concept registry with cluster data
    if not summary_matrix.empty:
        concept_registry.populate_cluster_data_from_matrix(summary_matrix.to_dict('records'))
    
    result = {
        'summary_matrix': summary_matrix,
        'patient_assignments': patient_assignments,
        'best_silhouette_score': best_silhouette if best_silhouette is not None else 0.0,
        'optimal_cluster_count': optimal_k
    }
    
    # Store in cache (with size limit)
    if len(_clustering_cache) >= _MAX_CACHE_SIZE:
        # Remove oldest entry (first key)
        oldest_key = next(iter(_clustering_cache))
        del _clustering_cache[oldest_key]
    
    _clustering_cache[cache_key] = result
    
    return result


