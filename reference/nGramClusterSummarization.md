# Function for summarizing ngrams from nGramDiscovery output

Function for summarizing ngrams from nGramDiscovery output

## Usage

``` r
nGramClusterSummarization(result, top_n = 5)
```

## Arguments

- result:

  output from nGramDiscovery

- top_n:

  integer for number of most frequent concept to show per cluster

## Value

A data frame with one row per n-gram cluster. The table reports the
number of n-grams in each cluster, average number of persons per n-gram,
total unique patients represented in the cluster, average timing, and a
concatenated label of the most frequent concepts in that cluster.

## Examples

``` r
if (requireNamespace("nanoparquet", quietly = TRUE) &&
    requireNamespace("Matrix", quietly = TRUE) &&
    requireNamespace("vegan", quietly = TRUE) &&
    requireNamespace("cluster", quietly = TRUE)) {
  studyDir <- system.file("example", "st", package = "CohortContrast")
  study <- loadCohortContrastStudy("lc500", pathToResults = studyDir)
  ngrams <- nGramDiscovery(study)
  clusterSummary <- suppressWarnings(nGramClusterSummarization(ngrams))
  head(clusterSummary)
}
#> ! Running n-gram discovery...
#> ! Running n = 2
#> ! Running n = 3
#> ! Running n = 4
#> ! Running n = 5
#> ✔ Running n-gram discovery.
#> ! Running n-gram clustering...
#> ✔ Running n-gram clustering.
#> # A tibble: 4 × 6
#>   ngram_cluster n_phases avg_persons total_unique_patients avg_time top_concepts
#>           <int>    <int>       <dbl>                 <int>    <dbl> <chr>       
#> 1             3        6        172.                   437     6.99 Malignant t…
#> 2             4        2        232.                   355    31.7  Outpatient …
#> 3             2        2        126.                   149    32.3  Chemotherap…
#> 4             1        3        118                    155   216.   Death + Pal…
```
