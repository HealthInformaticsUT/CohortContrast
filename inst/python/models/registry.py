"""
Concept Registry - Centralized registry for all concepts (main and ordinals).
"""

from typing import Dict, List, Optional, Tuple
import pandas as pd


class ConceptRegistry:
    """
    Centralized registry for all concepts (main and ordinals).
    Provides consistent lookup by various keys from anywhere in the app.
    """
    
    def __init__(self):
        self.clear()
    
    def clear(self):
        """Reset all maps."""
        # Main concept maps
        self._by_id: Dict[str, Dict] = {}  # normalized_id -> concept_info
        self._by_name: Dict[str, Dict] = {}  # concept_name -> concept_info
        
        # Ordinal maps
        self._ordinals_by_id: Dict[str, Dict] = {}  # normalized_id -> ordinal_info
        self._ordinals_by_key: Dict[Tuple, Dict] = {}  # (heritage, original_id, ordinal_num) -> ordinal_info
        
        # Combined map (both main and ordinals)
        self._all_by_id: Dict[str, Dict] = {}  # normalized_id -> info
        
        # Cluster data maps (populated separately)
        self._cluster_data: Dict[Tuple[str, str], Dict] = {}  # (normalized_id, cluster) -> {prevalence, median_days, count_category}
        
        # Mapping table data
        self._concept_mappings: Dict[str, List[str]] = {}  # target_concept_id -> [source_concept_names]
        
        self._initialized = False
    
    @staticmethod
    def normalize_id(concept_id) -> str:
        """Normalize concept ID to string, removing .0 suffix from floats."""
        if concept_id is None:
            return ""
        if isinstance(concept_id, float):
            if pd.isna(concept_id):
                return ""
            if concept_id.is_integer():
                return str(int(concept_id))
        s = str(concept_id)
        # Remove .0 suffix
        if s.endswith('.0'):
            return s[:-2]
        return s
    
    def register_concept(self, concept_info: Dict, is_ordinal: bool = False):
        """
        Register a concept (main or ordinal) in the registry.
        
        Args:
            concept_info: Dict with CONCEPT_ID, CONCEPT_NAME, heritage, etc.
            is_ordinal: Whether this is an ordinal concept
        """
        concept_id = concept_info.get('CONCEPT_ID') or concept_info.get('_concept_id') or concept_info.get('concept_id')
        norm_id = self.normalize_id(concept_id)
        
        if not norm_id:
            return
        
        concept_name = concept_info.get('CONCEPT_NAME') or concept_info.get('concept_name', '')
        heritage = concept_info.get('heritage') or concept_info.get('HERITAGE', '')
        
        # Store in all_by_id
        self._all_by_id[norm_id] = concept_info
        
        if is_ordinal or concept_info.get('IS_ORDINAL', False) or concept_info.get('is_ordinal', False):
            # Ordinal concept
            self._ordinals_by_id[norm_id] = concept_info
            
            original_id = concept_info.get('ORIGINAL_CONCEPT_ID') or concept_info.get('original_concept_id')
            ordinal_num = concept_info.get('ORDINAL') or concept_info.get('ordinal', 0)
            
            if original_id is not None and ordinal_num:
                norm_original_id = self.normalize_id(original_id)
                key = (str(heritage), norm_original_id, int(ordinal_num))
                self._ordinals_by_key[key] = concept_info
        else:
            # Main concept
            self._by_id[norm_id] = concept_info
            if concept_name:
                self._by_name[concept_name] = concept_info
        
        self._initialized = True
    
    def register_from_dashboard_data(self, dashboard_data: List[Dict]):
        """
        Populate registry from dashboard data list.
        
        Args:
            dashboard_data: List of concept dictionaries from dashboard
        """
        self.clear()
        for item in dashboard_data:
            is_ordinal = item.get('IS_ORDINAL', False) or item.get('is_ordinal', False)
            self.register_concept(item, is_ordinal)
    
    def register_mappings(self, mapping_df: pd.DataFrame):
        """
        Register concept mappings from complementaryMappingTable.
        
        Args:
            mapping_df: DataFrame with columns CONCEPT_ID, CONCEPT_NAME, NEW_CONCEPT_ID, etc.
        """
        self._concept_mappings.clear()
        
        if mapping_df is None or mapping_df.empty:
            return
            
        for _, row in mapping_df.iterrows():
            target_id = self.normalize_id(row.get('NEW_CONCEPT_ID'))
            source_name = row.get('CONCEPT_NAME', '')
            
            if target_id and source_name:
                if target_id not in self._concept_mappings:
                    self._concept_mappings[target_id] = []
                if source_name not in self._concept_mappings[target_id]:
                    self._concept_mappings[target_id].append(source_name)
    
    def get_mapped_from_concepts(self, concept_id, original_concept_id=None, ordinal=None) -> List[str]:
        """
        Get list of source concept names that were mapped to this concept.
        
        Args:
            concept_id: The concept ID to look up
            original_concept_id: For ordinals, the original (main) concept ID
            ordinal: For ordinals, the ordinal number (1, 2, 3, etc.)
            
        Returns:
            List of source concept names
        """
        # For ordinals, use the original concept ID for mapping lookup
        lookup_id = original_concept_id if ordinal and original_concept_id else concept_id
        norm_id = self.normalize_id(lookup_id)
        
        return self._concept_mappings.get(norm_id, [])
    
    def get_by_id(self, concept_id) -> Optional[Dict]:
        """Get concept by ID (works for both main and ordinal)."""
        norm_id = self.normalize_id(concept_id)
        return self._all_by_id.get(norm_id)
    
    def get_main_by_id(self, concept_id) -> Optional[Dict]:
        """Get main concept by ID."""
        norm_id = self.normalize_id(concept_id)
        return self._by_id.get(norm_id)
    
    def get_by_name(self, concept_name: str) -> Optional[Dict]:
        """Get main concept by name."""
        return self._by_name.get(concept_name)
    
    def get_ordinal(self, heritage: str, original_concept_id, ordinal_num: int) -> Optional[Dict]:
        """Get ordinal concept by heritage, original concept ID, and ordinal number."""
        norm_original_id = self.normalize_id(original_concept_id)
        key = (str(heritage), norm_original_id, int(ordinal_num))
        return self._ordinals_by_key.get(key)
    
    def get_ordinal_by_id(self, concept_id) -> Optional[Dict]:
        """Get ordinal concept by its own concept ID."""
        norm_id = self.normalize_id(concept_id)
        return self._ordinals_by_id.get(norm_id)
    
    def set_cluster_data(self, concept_id, cluster: str, prevalence: float, 
                         median_days: Optional[float] = None, count_category: str = "1"):
        """Store cluster prevalence data for a concept."""
        norm_id = self.normalize_id(concept_id)
        key = (norm_id, cluster)
        self._cluster_data[key] = {
            'prevalence': prevalence,
            'median_days': median_days,
            'count_category': count_category
        }
    
    def get_cluster_data(self, concept_id, cluster: str) -> Dict:
        """Get cluster prevalence data for a concept."""
        norm_id = self.normalize_id(concept_id)
        key = (norm_id, cluster)
        return self._cluster_data.get(key, {'prevalence': 0.0, 'median_days': None, 'count_category': '1'})
    
    def has_cluster_data(self, concept_id, cluster: str) -> bool:
        """Check if cluster data exists for a concept."""
        norm_id = self.normalize_id(concept_id)
        key = (norm_id, cluster)
        return key in self._cluster_data
    
    def populate_cluster_data_from_matrix(self, summary_matrix_data: List[Dict]):
        """
        Populate cluster data from summary matrix (from clustering results).
        
        Args:
            summary_matrix_data: List of dicts with concept_id, cluster, prevalence, etc.
                                 Handles both uppercase (from parquet) and lowercase column names.
        """
        for row in summary_matrix_data:
            # Handle both uppercase (from precomputed parquet) and lowercase column names
            concept_id = row.get('CONCEPT_ID') or row.get('concept_id')
            cluster = row.get('cluster')
            if concept_id is not None and cluster:
                # Get median_days from either column name format
                median_days = row.get('time_median') or row.get('median_days')
                self.set_cluster_data(
                    concept_id,
                    cluster,
                    row.get('prevalence', 0.0),
                    median_days,
                    row.get('count_category', '1')
                )
    
    def get_all_main_ids(self) -> List[str]:
        """Get all registered main concept IDs (normalized)."""
        return list(self._by_id.keys())
    
    def get_all_ordinal_ids(self) -> List[str]:
        """Get all registered ordinal concept IDs (normalized)."""
        return list(self._ordinals_by_id.keys())
    
    def is_initialized(self) -> bool:
        """Check if registry has been populated."""
        return self._initialized
    
    def __repr__(self):
        return f"ConceptRegistry(main={len(self._by_id)}, ordinals={len(self._ordinals_by_id)}, cluster_data={len(self._cluster_data)})"


# Global concept registry instance
concept_registry = ConceptRegistry()

