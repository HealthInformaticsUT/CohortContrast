# Get Top Concepts That Best Separate Clusters

Returns the top \`n\` main concepts ranked by standard deviation of
concept prevalence across clusters (same ranking idea as the UI "Top N
by SD" filter).

## Usage

``` r
getTopSeparatingConcepts(
  studyPath,
  n = 10,
  k = "auto",
  precomputeIfNeeded = TRUE,
  clusterKValuesWhenAuto = c(2, 3, 4, 5),
  conceptLimit = 60,
  ...
)
```

## Arguments

- studyPath:

  Path to a CohortContrast study folder (summary or patient).

- n:

  Number of top concepts to return.

- k:

  Cluster count as an integer, or \`"auto"\` to select the best
  precomputed \`k\` from \`metadata.json\`.

- precomputeIfNeeded:

  Logical; if \`TRUE\` and clustering summaries are not available, run
  \[precomputeSummary()\] into a temporary directory.

- clusterKValuesWhenAuto:

  Integer vector of \`k\` values to precompute when \`k = "auto"\` and
  precomputation is needed.

- conceptLimit:

  Maximum concept count passed to \[precomputeSummary()\] when
  precomputation is needed.

- ...:

  Additional arguments forwarded to \[precomputeSummary()\] when
  precomputation is needed.

## Value

A \`data.frame\` with columns:

- id:

  Concept ID

- name:

  Concept name

- enrichment:

  Target/control prevalence ratio

- target_prevalence:

  Target cohort prevalence

The selected \`k\` is attached as \`attr(result, "k")\`.

## Details

The function accepts either: - A summary-mode directory (with
\`metadata.json\` and \`concept_summaries.parquet\`) - A patient-level
study directory (with \`data_patients.parquet\`)

For patient-level inputs, clustering summaries are generated on the fly
with \[precomputeSummary()\] when needed.

## Examples

``` r
if (requireNamespace("nanoparquet", quietly = TRUE)) {
  summaryPath <- system.file("example", "st", "lc500s", package = "CohortContrast")

  top_auto <- getTopSeparatingConcepts(summaryPath, n = 5, k = "auto")
  top_k3 <- getTopSeparatingConcepts(summaryPath, n = 5, k = 3)

  head(top_auto)
  head(top_k3)
}
#>        id                                                   name enrichment
#> 1   32280                                                  Death      100.0
#> 2   32815                                      Death Certificate      100.0
#> 3 4008211                                           Radiotherapy       34.2
#> 4 4008226                                           Chemotherapy       32.8
#> 5 4176729 Treatment planning for external beam radiation therapy       32.8
#>   target_prevalence
#> 1             0.312
#> 2             0.294
#> 3             0.342
#> 4             0.328
#> 5             0.328
```
