# Pre-compute Summary Data for a Study

Generates aggregated summary data from patient-level parquet files,
removing all individual patient information. This creates a "summary
mode" dataset that can be shared without privacy concerns.

## Usage

``` r
precomputeSummary(
  studyPath,
  outputPath = NULL,
  clusterKValues = c(2, 3, 4, 5),
  conceptLimit = 60,
  minCellCount = 0,
  maxParallelJobs = 1,
  clusterFeatureMatrixCellThreshold = 5e+07,
  pairwiseOverlapMaxConcepts = 500,
  ...
)
```

## Arguments

- studyPath:

  Path to directory containing patient-level parquet files
  (data_patients.parquet, data_features.parquet, etc.)

- outputPath:

  Path for output summary files. Default is studyPath + "\_summary"

- clusterKValues:

  Vector of k values to pre-compute clustering for. Default is c(2, 3,
  4, 5).

- conceptLimit:

  Maximum number of concepts to use for clustering. Default is 60.

- minCellCount:

  Minimum patient count for small cell suppression (privacy). Counts
  between 1 and (minCellCount-1) are rounded up to minCellCount. Default
  is 0 (disabled). Set to e.g. 5 to apply suppression.

- maxParallelJobs:

  Maximum number of parallel clustering jobs. Default is 1 (sequential)
  to avoid out-of-memory errors on servers. Set to 2-4 on machines with
  ample RAM for faster execution.

- clusterFeatureMatrixCellThreshold:

  Maximum allowed size for the clustering feature matrix, measured as
  patients x features (features = 3 x clustering concepts). If exceeded,
  clustering concepts are auto-capped for memory safety. Default is
  50000000.

- pairwiseOverlapMaxConcepts:

  Maximum number of concepts used when computing pairwise overlap
  matrices. Higher values increase memory/time quadratically. Default is
  500.

- ...:

  Backward-compatible aliases: \`study_path\`, \`output_path\`,
  \`cluster_k_values\`, \`concept_limit\`, \`min_cell_count\`,
  \`max_parallel_jobs\`, \`cluster_feature_matrix_cell_threshold\`,
  \`pairwise_overlap_max_concepts\`.

## Value

A list with:

- outputPath:

  Path to the generated summary directory (canonical name; legacy
  \`output_path\` may also be present depending on Python output).

- files:

  Named list of generated file paths

- metadata:

  Study metadata including demographics and clustering info

## Details

The function pre-computes: - Concept-level time distribution statistics
(for violin/box plots) - Age and gender statistics per concept - Ordinal
concept summaries (1st, 2nd, 3rd occurrences) - Clustering results for
k=2,3,4,5 clusters - Cluster overlap and differentiation metrics

## Examples

``` r
if (FALSE) { # \dontrun{
# Generate summary data for a study (no suppression)
result <- precomputeSummary(
  studyPath = "results_parquet/Breast_cancer",
  outputPath = "summaries/Breast_cancer"
)

# With small cell suppression (counts 1-4 become 5)
result <- precomputeSummary(
  studyPath = "results_parquet/Breast_cancer",
  minCellCount = 5
)

# View generated files
print(result$files)

# Run viewer with summary data (no patient data needed)
runCohortContrastViewer(dataDir = "summaries")
} # }
```
