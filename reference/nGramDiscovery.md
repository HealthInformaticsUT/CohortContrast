# Function for discovering ngrams from the extracted data from CohortContrast function

Function for discovering ngrams from the extracted data from
CohortContrast function

## Usage

``` r
nGramDiscovery(data, collapse_size = 0)
```

## Arguments

- data:

  CohortContrastObject returned by CohortContrast or GUI snapshot

- collapse_size:

  integer for days to use for collapse same concept ids

## Value

A data frame of enriched n-grams, or NULL if no significant n-grams are
found. The returned table contains one row per detected n-gram with
cluster assignment, concept identifiers and names, observed and expected
counts, over-representation statistics, person counts, timing summaries,
and p-values used to flag significant patterns.

## Examples

``` r
# \donttest{
if (requireNamespace("nanoparquet", quietly = TRUE) &&
    requireNamespace("Matrix", quietly = TRUE) &&
    requireNamespace("vegan", quietly = TRUE) &&
    requireNamespace("cluster", quietly = TRUE)) {
  studyDir <- system.file("example", "st", package = "CohortContrast")
  study <- loadCohortContrastStudy("lc500", pathToResults = studyDir)
  ngrams <- nGramDiscovery(study)
  head(ngrams)
}
#> ! Running n-gram discovery...
#> ! Running n = 2
#> ! Running n = 3
#> ! Running n = 4
#> ! Running n = 5
#> ✔ Running n-gram discovery.
#> ! Running n-gram clustering...
#> ✔ Running n-gram clustering.
#> # A tibble: 6 × 15
#>   ngram_label           ngram_cluster ngram_ids ngram_names observed_ngram_count
#>   <chr>                         <int> <list>    <list>                     <int>
#> 1 Death + Death Certif…             1 <chr [2]> <chr [2]>                    156
#> 2 Death + Palliative c…             1 <chr [2]> <chr [2]>                    185
#> 3 Death + Death                     1 <chr [2]> <chr [2]>                    186
#> 4 Radiotherapy + Chemo…             2 <chr [2]> <chr [2]>                    322
#> 5 Bronchoscopy + Needl…             3 <chr [2]> <chr [2]>                    228
#> 6 Malignant tumor of l…             3 <chr [2]> <chr [2]>                    276
#> # ℹ 10 more variables: expected_ngram_count <dbl>, overrep_ratio <dbl>,
#> #   n_unique_persons <int>, unique_persons_ids <list>, ngram_time_diff <dbl>,
#> #   ngram_time <dbl>, observed_count_p_value <dbl>, observed_test <lgl>,
#> #   persons_count_p_value <dbl>, persons_test <lgl>
# }
```
