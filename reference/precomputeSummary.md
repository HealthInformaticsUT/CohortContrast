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
  minibatchKMeansCutoffPatients = 50000
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

- minibatchKMeansCutoffPatients:

  If target patient count is greater than this value, clustering uses
  MiniBatchKMeans instead of KMedoids. Default is 50000.

## Value

A list with:

- outputPath:

  Path to the generated summary directory.

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
# \donttest{
if (requireNamespace("reticulate", quietly = TRUE) &&
    (nzchar(Sys.which("python3")) || nzchar(Sys.which("python")))) {
  configurePython(createVenv = FALSE)
  deps <- checkPythonDeps()
  if (all(deps$installed)) {
    studyPath <- system.file("example", "st", "lc500", package = "CohortContrast")
    outputPath <- file.path(tempdir(), "lc500_summary")
    result <- precomputeSummary(
      studyPath = studyPath,
      outputPath = outputPath,
      clusterKValues = c(2, 3),
      conceptLimit = 20,
      maxParallelJobs = 1
    )
    result$outputPath
  }
}
#> Configuring Python environment for CohortContrast Viewer...
#> Using system Python: /Users/markushaug/.virtualenvs/r-cohortcontrast-viewer/bin/python3
#> Python configuration complete.
#> Python version: 3.13
#> Python path: /Users/markushaug/.virtualenvs/r-cohortcontrast-viewer/bin/python
#> All required Python packages are installed.
#> Pre-computing summary data for: /Users/markushaug/UT/R-packages/Develop/CohortContrast/inst/example/st/lc500
#> This may take a few minutes for large datasets...
#> 10:39:59 [INFO] ============================================================
#> 10:39:59 [INFO] Pre-computing summary for: /Users/markushaug/UT/R-packages/Develop/CohortContrast/inst/example/st/lc500
#> 10:39:59 [INFO] Output directory: /var/folders/04/_j3s9x5j6mvf0cbxxbbgy5_00000gn/T/RtmpeYfpSH/lc500_summary
#> 10:39:59 [INFO] Concept limit: 20
#> 10:39:59 [INFO] Min cell count: 0
#> 10:39:59 [INFO] MiniBatchKMeans cutoff patients: 50000
#> 10:39:59 [INFO] ============================================================
#> 10:39:59 [INFO] Starting: Loading parquet files
#> 10:40:00 [INFO]   data_patients: 4,656 rows
#> 10:40:00 [INFO]   data_features: 16 rows
#> 10:40:00 [INFO]   data_initial: 1,000 rows
#> 10:40:00 [INFO]   data_person: 1,000 rows
#> 10:40:00 [INFO]   complementaryMappingTable: 0 rows
#> 10:40:00 [INFO] Completed: Loading parquet files (0.08s)
#> 10:40:00 [INFO] Starting: Pre-processing data
#> 10:40:00 [INFO] Filtering target cohort...
#> 10:40:00 [INFO] Target cohort: 500 patients, 4,183 rows
#> 10:40:00 [INFO] Exploding TIME_TO_EVENT arrays...
#> 10:40:00 [INFO] Filtered to 16 concepts from data_features (8,139 -> 8,139 events)
#> 10:40:00 [INFO] Exploded data: 8,139 individual events
#> 10:40:00 [INFO] Computing patient ages...
#> 10:40:00 [INFO] Computed ages for 500 patients
#> 10:40:00 [INFO] Building concept lookups...
#> 10:40:00 [INFO] From data_initial - Target: 500, Control: 500 patients
#> 10:40:00 [INFO] Completed: Pre-processing data (0.01s)
#> 10:40:00 [INFO] ✓ Copied complementaryMappingTable.parquet
#> 10:40:00 [INFO] Starting: Computing concept summaries (vectorized)
#> 10:40:00 [INFO] Computing summaries for 16 concepts...
#> 10:40:00 [INFO] Computing histograms and KDE distributions...
#> 10:40:00 [INFO] Computing demographics per concept...
#> 10:40:00 [INFO] Generated 16 concept summaries
#> 10:40:00 [INFO] Completed: Computing concept summaries (vectorized) (0.03s)
#> 10:40:00 [INFO] ✓ Generated concept_summaries.parquet: 16 rows
#> 10:40:00 [INFO] Starting: Computing ordinal summaries (vectorized)
#> 10:40:00 [INFO] Assigning ordinals to events...
#> 10:40:00 [INFO] Found 6 max ordinal across all concepts
#> 10:40:00 [INFO] Computing stats for 17 ordinal groups
#> 10:40:00 [INFO] Computing demographics per ordinal group...
#> 10:40:00 [INFO] Calculating target and control ordinal occurrence counts...
#> 10:40:00 [INFO] Processing control cohort ordinals (counting all ordinals, no 50% threshold)...
#> 10:40:00 [INFO] Found 9 control ordinal groups (all ordinals counted)
#> 10:40:00 [INFO] Matched 8 target ordinal groups with control data
#> 10:40:00 [INFO] Generated 17 ordinal summaries
#> 10:40:00 [INFO] Completed: Computing ordinal summaries (vectorized) (0.04s)
#> 10:40:00 [INFO] ✓ Generated ordinal_summaries.parquet: 17 rows
#> 10:40:00 [INFO] Starting: Computing study demographics
#> 10:40:00 [INFO] Completed: Computing study demographics (0.00s)
#> 10:40:00 [INFO] Starting: Computing clustering for k=[2, 3]
#> 10:40:00 [INFO] Computing clustering for k=2...
#> 10:40:00 [INFO] Computing clustering for k=3...
#> 10:40:00 [INFO] ✓ k=2: silhouette=0.298, patients=500
#> 10:40:00 [INFO] ✓ k=3: silhouette=0.418, patients=500
#> 10:40:00 [INFO] Completed: Computing clustering for k=[2, 3] (0.16s)
#> 10:40:00 [INFO] Starting: Computing demographic distributions
#> 10:40:00 [INFO] Completed: Computing demographic distributions (0.00s)
#> 10:40:00 [INFO] Counted 16 unique main concepts (significant concepts)
#> 10:40:00 [INFO] Starting: Saving metadata
#> 10:40:00 [INFO] ✓ Generated metadata.json
#> 10:40:00 [INFO] Completed: Saving metadata (0.00s)
#> 10:40:00 [INFO] ✓ Copied desc.txt
#> 10:40:00 [INFO] ============================================================
#> 10:40:00 [INFO] Summary generation complete!
#> 10:40:00 [INFO] Output: /var/folders/04/_j3s9x5j6mvf0cbxxbbgy5_00000gn/T/RtmpeYfpSH/lc500_summary
#> 10:40:00 [INFO] Files generated: 9
#> 10:40:00 [INFO] Total time: 0.33s
#> 10:40:00 [INFO] ============================================================
#> Pre-computing summary for: /Users/markushaug/UT/R-packages/Develop/CohortContrast/inst/example/st/lc500
#> Output directory: /var/folders/04/_j3s9x5j6mvf0cbxxbbgy5_00000gn/T/RtmpeYfpSH/lc500_summary 
#> 
#> Summary generation complete!
#> Output directory: /var/folders/04/_j3s9x5j6mvf0cbxxbbgy5_00000gn/T/RtmpeYfpSH/lc500_summary
#> Files generated: 9
#> [1] "/var/folders/04/_j3s9x5j6mvf0cbxxbbgy5_00000gn/T/RtmpeYfpSH/lc500_summary"
# }
```
