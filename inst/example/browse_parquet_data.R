# =============================================================================
# Browse Example Parquet Data
# =============================================================================
#
# This script loads all parquet files from the example data folder into
# data frames for exploration and inspection.
#
# Requirements:
#   - nanoparquet package (or arrow package)
#
# =============================================================================

# Install nanoparquet if not available
if (!requireNamespace("nanoparquet", quietly = TRUE)) {
  message("Installing nanoparquet package...")
  install.packages("nanoparquet")
}

library(nanoparquet)

# -----------------------------------------------------------------------------
# Get Path to Example Data
# -----------------------------------------------------------------------------
example_dir <- system.file("example", "parquet", "Prostate_cancer_summary",
                           package = "CohortContrast")

# If running from development mode (not installed package)
if (example_dir == "") {
  example_dir <- "inst/example/parquet/Prostate_cancer_summary"
}

cat("Loading parquet files from:\n", example_dir, "\n\n")

# -----------------------------------------------------------------------------
# List Available Files
# -----------------------------------------------------------------------------
parquet_files <- list.files(example_dir, pattern = "\\.parquet$", full.names = TRUE)
cat("Found", length(parquet_files), "parquet files:\n")
for (f in parquet_files) {
  cat("  -", basename(f), "\n")
}
cat("\n")

# -----------------------------------------------------------------------------
# Load All Parquet Files
# -----------------------------------------------------------------------------

# NOTE: data_features.parquet is NOT included in summary mode outputs
# because it contains patient-level data. All needed aggregated data
# is in concept_summaries.parquet with privacy protections applied.

# Concept Summaries - aggregated time distributions per concept
concept_summaries <- nanoparquet::read_parquet(
  file.path(example_dir, "concept_summaries.parquet")
)
cat("concept_summaries:", nrow(concept_summaries), "rows x", ncol(concept_summaries), "cols\n")

# Ordinal Summaries - 1st, 2nd, 3rd occurrence statistics
ordinal_summaries <- nanoparquet::read_parquet(
  file.path(example_dir, "ordinal_summaries.parquet")
)
cat("ordinal_summaries:", nrow(ordinal_summaries), "rows x", ncol(ordinal_summaries), "cols\n")

# Complementary Mapping Table - concept hierarchy/groupings
complementary_mapping <- nanoparquet::read_parquet(
  file.path(example_dir, "complementaryMappingTable.parquet")
)
cat("complementary_mapping:", nrow(complementary_mapping), "rows x", ncol(complementary_mapping), "cols\n")

# Clustering Results (k=2,3,4,5)
clustering_k2 <- nanoparquet::read_parquet(
  file.path(example_dir, "clustering_k2_summary.parquet")
)
cat("clustering_k2:", nrow(clustering_k2), "rows x", ncol(clustering_k2), "cols\n")

clustering_k3 <- nanoparquet::read_parquet(
  file.path(example_dir, "clustering_k3_summary.parquet")
)
cat("clustering_k3:", nrow(clustering_k3), "rows x", ncol(clustering_k3), "cols\n")

clustering_k4 <- nanoparquet::read_parquet(
  file.path(example_dir, "clustering_k4_summary.parquet")
)
cat("clustering_k4:", nrow(clustering_k4), "rows x", ncol(clustering_k4), "cols\n")

clustering_k5 <- nanoparquet::read_parquet(
  file.path(example_dir, "clustering_k5_summary.parquet")
)
cat("clustering_k5:", nrow(clustering_k5), "rows x", ncol(clustering_k5), "cols\n")

# Pairwise Cluster Overlap
overlap_k2 <- nanoparquet::read_parquet(
  file.path(example_dir, "clustering_k2_pairwise_overlap.parquet")
)
cat("overlap_k2:", nrow(overlap_k2), "rows x", ncol(overlap_k2), "cols\n")

overlap_k3 <- nanoparquet::read_parquet(
  file.path(example_dir, "clustering_k3_pairwise_overlap.parquet")
)
cat("overlap_k3:", nrow(overlap_k3), "rows x", ncol(overlap_k3), "cols\n")

overlap_k4 <- nanoparquet::read_parquet(
  file.path(example_dir, "clustering_k4_pairwise_overlap.parquet")
)
cat("overlap_k4:", nrow(overlap_k4), "rows x", ncol(overlap_k4), "cols\n")

overlap_k5 <- nanoparquet::read_parquet(
  file.path(example_dir, "clustering_k5_pairwise_overlap.parquet")
)
cat("overlap_k5:", nrow(overlap_k5), "rows x", ncol(overlap_k5), "cols\n")

# -----------------------------------------------------------------------------
# Load Metadata (JSON)
# -----------------------------------------------------------------------------
metadata <- jsonlite::fromJSON(file.path(example_dir, "metadata.json"))
cat("\nMetadata loaded. Keys:", paste(names(metadata), collapse = ", "), "\n")

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------
cat("\n")
cat("=============================================================================\n")
cat("All data loaded! Available data frames:\n")
cat("=============================================================================\n")
cat("\n")
cat("Main Data:\n")
cat("  concept_summaries    - Time distribution summaries per concept\n")
cat("  ordinal_summaries    - 1st/2nd/3rd occurrence statistics\n")
cat("  complementary_mapping - Concept hierarchy and groupings\n")
cat("\n")
cat("Clustering (k = number of clusters):\n")
cat("  clustering_k2, clustering_k3, clustering_k4, clustering_k5\n")
cat("  overlap_k2, overlap_k3, overlap_k4, overlap_k5\n")
cat("\n")
cat("Metadata:\n")
cat("  metadata             - Study metadata (list)\n")
cat("\n")
cat("=============================================================================\n")
cat("Example usage:\n")
cat("  View(concept_summaries)       # Open in RStudio viewer\n")
cat("  head(concept_summaries)       # First 6 rows\n")
cat("  str(clustering_k3)            # Structure of data frame\n")
cat("  names(metadata)               # Metadata fields\n")
cat("=============================================================================\n")

