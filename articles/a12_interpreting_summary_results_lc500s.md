# Interpreting Summary Results with lc500s

## Goal

This vignette is the summary-mode companion to the patient-level
interpretation guide. It explains, in detail, what is stored in each
summary artifact using the bundled `lc500s` example.

The focus is exhaustive schema interpretation:

- `metadata.json`
- `concept_summaries.parquet`
- `ordinal_summaries.parquet`
- `clustering_k*_summary.parquet`
- `clustering_k*_pairwise_overlap.parquet`
- `complementaryMappingTable.parquet`

## Load the Example Summary Study

``` r

isMissingOrEmpty <- function(x) {
  length(x) == 0 || is.na(x[1]) || !nzchar(x[1])
}

exampleRoot <- system.file("example", "st", package = "CohortContrast")
if (isMissingOrEmpty(exampleRoot) && dir.exists("inst/example/st")) {
  exampleRoot <- "inst/example/st"
}

studyPath <- file.path(exampleRoot, "lc500s")

if (isMissingOrEmpty(exampleRoot) || !dir.exists(studyPath)) {
  cat("Bundled summary example 'lc500s' is not available in this build.\n")
  knitr::knit_exit()
}

stopifnot(requireNamespace("nanoparquet", quietly = TRUE))
stopifnot(requireNamespace("jsonlite", quietly = TRUE))

modeInfo <- CohortContrast::checkDataMode(studyPath)
modeInfo
#> $mode
#> [1] "summary"
#> 
#> $has_clustering
#> [1] TRUE
#> 
#> $clusterKValues
#> [1] 2 3 4 5
#> 
#> $demographics
#> $demographics$target_patients
#> [1] 500
#> 
#> $demographics$control_patients
#> [1] 346
#> 
#> $demographics$age_mean
#> [1] 66.986
#> 
#> $demographics$age_median
#> [1] 66
#> 
#> $demographics$age_std
#> [1] 10.38564
#> 
#> $demographics$male_proportion
#> [1] 0.562
#> 
#> $demographics$distributions
#> $demographics$distributions$overall
#> $demographics$distributions$overall$age_histogram
#> $demographics$distributions$overall$age_histogram$bins
#>  [1] "0-10"   "10-20"  "20-30"  "30-40"  "40-50"  "50-60"  "60-70"  "70-80" 
#>  [9] "80-90"  "90-100"
#> 
#> $demographics$distributions$overall$age_histogram$counts
#>  [1]   0   0   0   0  23 108 160 144  65   0
#> 
#> $demographics$distributions$overall$age_histogram$mean
#> [1] 66.986
#> 
#> $demographics$distributions$overall$age_histogram$median
#> [1] 66
#> 
#> $demographics$distributions$overall$age_histogram$std
#> [1] 10.37525
#> 
#> $demographics$distributions$overall$age_histogram$q1
#> [1] 59
#> 
#> $demographics$distributions$overall$age_histogram$q3
#> [1] 74
#> 
#> $demographics$distributions$overall$age_histogram$n
#> [1] 500
#> 
#> 
#> $demographics$distributions$overall$sex_distribution
#> $demographics$distributions$overall$sex_distribution$male
#> [1] 281
#> 
#> $demographics$distributions$overall$sex_distribution$female
#> [1] 219
#> 
#> $demographics$distributions$overall$sex_distribution$total
#> [1] 500
#> 
#> 
#> 
#> $demographics$distributions$clusters
#> $demographics$distributions$clusters$C1
#> $demographics$distributions$clusters$C1$patient_count
#> [1] 170
#> 
#> $demographics$distributions$clusters$C1$age_mean
#> [1] 56.64118
#> 
#> $demographics$distributions$clusters$C1$age_median
#> [1] 56
#> 
#> $demographics$distributions$clusters$C1$age_std
#> [1] 5.501396
#> 
#> $demographics$distributions$clusters$C1$age_q1
#> [1] 52
#> 
#> $demographics$distributions$clusters$C1$age_q3
#> [1] 62
#> 
#> $demographics$distributions$clusters$C1$n
#> [1] 170
#> 
#> $demographics$distributions$clusters$C1$male_proportion
#> [1] 0.5823529
#> 
#> 
#> $demographics$distributions$clusters$C2
#> $demographics$distributions$clusters$C2$patient_count
#> [1] 160
#> 
#> $demographics$distributions$clusters$C2$age_mean
#> [1] 78.24375
#> 
#> $demographics$distributions$clusters$C2$age_median
#> [1] 78
#> 
#> $demographics$distributions$clusters$C2$age_std
#> [1] 6.020535
#> 
#> $demographics$distributions$clusters$C2$age_q1
#> [1] 73
#> 
#> $demographics$distributions$clusters$C2$age_q3
#> [1] 84
#> 
#> $demographics$distributions$clusters$C2$n
#> [1] 160
#> 
#> $demographics$distributions$clusters$C2$male_proportion
#> [1] 0.5125
#> 
#> 
#> $demographics$distributions$clusters$C3
#> $demographics$distributions$clusters$C3$patient_count
#> [1] 170
#> 
#> $demographics$distributions$clusters$C3$age_mean
#> [1] 66.73529
#> 
#> $demographics$distributions$clusters$C3$age_median
#> [1] 67
#> 
#> $demographics$distributions$clusters$C3$age_std
#> [1] 5.085787
#> 
#> $demographics$distributions$clusters$C3$age_q1
#> [1] 63
#> 
#> $demographics$distributions$clusters$C3$age_q3
#> [1] 71
#> 
#> $demographics$distributions$clusters$C3$n
#> [1] 170
#> 
#> $demographics$distributions$clusters$C3$male_proportion
#> [1] 0.5882353
#> 
#> 
#> 
#> $demographics$distributions$clusters_by_k
#> $demographics$distributions$clusters_by_k$`2`
#> $demographics$distributions$clusters_by_k$`2`$C1
#> $demographics$distributions$clusters_by_k$`2`$C1$patient_count
#> [1] 336
#> 
#> $demographics$distributions$clusters_by_k$`2`$C1$age_mean
#> [1] 61.62202
#> 
#> $demographics$distributions$clusters_by_k$`2`$C1$age_median
#> [1] 62
#> 
#> $demographics$distributions$clusters_by_k$`2`$C1$age_std
#> [1] 7.325956
#> 
#> $demographics$distributions$clusters_by_k$`2`$C1$age_q1
#> [1] 56
#> 
#> $demographics$distributions$clusters_by_k$`2`$C1$age_q3
#> [1] 67
#> 
#> $demographics$distributions$clusters_by_k$`2`$C1$n
#> [1] 336
#> 
#> $demographics$distributions$clusters_by_k$`2`$C1$male_proportion
#> [1] 0.5833333
#> 
#> 
#> $demographics$distributions$clusters_by_k$`2`$C2
#> $demographics$distributions$clusters_by_k$`2`$C2$patient_count
#> [1] 164
#> 
#> $demographics$distributions$clusters_by_k$`2`$C2$age_mean
#> [1] 77.97561
#> 
#> $demographics$distributions$clusters_by_k$`2`$C2$age_median
#> [1] 78
#> 
#> $demographics$distributions$clusters_by_k$`2`$C2$age_std
#> [1] 6.205772
#> 
#> $demographics$distributions$clusters_by_k$`2`$C2$age_q1
#> [1] 73
#> 
#> $demographics$distributions$clusters_by_k$`2`$C2$age_q3
#> [1] 83.25
#> 
#> $demographics$distributions$clusters_by_k$`2`$C2$n
#> [1] 164
#> 
#> $demographics$distributions$clusters_by_k$`2`$C2$male_proportion
#> [1] 0.5182927
#> 
#> 
#> 
#> $demographics$distributions$clusters_by_k$`3`
#> $demographics$distributions$clusters_by_k$`3`$C1
#> $demographics$distributions$clusters_by_k$`3`$C1$patient_count
#> [1] 170
#> 
#> $demographics$distributions$clusters_by_k$`3`$C1$age_mean
#> [1] 56.64118
#> 
#> $demographics$distributions$clusters_by_k$`3`$C1$age_median
#> [1] 56
#> 
#> $demographics$distributions$clusters_by_k$`3`$C1$age_std
#> [1] 5.501396
#> 
#> $demographics$distributions$clusters_by_k$`3`$C1$age_q1
#> [1] 52
#> 
#> $demographics$distributions$clusters_by_k$`3`$C1$age_q3
#> [1] 62
#> 
#> $demographics$distributions$clusters_by_k$`3`$C1$n
#> [1] 170
#> 
#> $demographics$distributions$clusters_by_k$`3`$C1$male_proportion
#> [1] 0.5823529
#> 
#> 
#> $demographics$distributions$clusters_by_k$`3`$C2
#> $demographics$distributions$clusters_by_k$`3`$C2$patient_count
#> [1] 160
#> 
#> $demographics$distributions$clusters_by_k$`3`$C2$age_mean
#> [1] 78.24375
#> 
#> $demographics$distributions$clusters_by_k$`3`$C2$age_median
#> [1] 78
#> 
#> $demographics$distributions$clusters_by_k$`3`$C2$age_std
#> [1] 6.020535
#> 
#> $demographics$distributions$clusters_by_k$`3`$C2$age_q1
#> [1] 73
#> 
#> $demographics$distributions$clusters_by_k$`3`$C2$age_q3
#> [1] 84
#> 
#> $demographics$distributions$clusters_by_k$`3`$C2$n
#> [1] 160
#> 
#> $demographics$distributions$clusters_by_k$`3`$C2$male_proportion
#> [1] 0.5125
#> 
#> 
#> $demographics$distributions$clusters_by_k$`3`$C3
#> $demographics$distributions$clusters_by_k$`3`$C3$patient_count
#> [1] 170
#> 
#> $demographics$distributions$clusters_by_k$`3`$C3$age_mean
#> [1] 66.73529
#> 
#> $demographics$distributions$clusters_by_k$`3`$C3$age_median
#> [1] 67
#> 
#> $demographics$distributions$clusters_by_k$`3`$C3$age_std
#> [1] 5.085787
#> 
#> $demographics$distributions$clusters_by_k$`3`$C3$age_q1
#> [1] 63
#> 
#> $demographics$distributions$clusters_by_k$`3`$C3$age_q3
#> [1] 71
#> 
#> $demographics$distributions$clusters_by_k$`3`$C3$n
#> [1] 170
#> 
#> $demographics$distributions$clusters_by_k$`3`$C3$male_proportion
#> [1] 0.5882353
#> 
#> 
#> 
#> $demographics$distributions$clusters_by_k$`4`
#> $demographics$distributions$clusters_by_k$`4`$C1
#> $demographics$distributions$clusters_by_k$`4`$C1$patient_count
#> [1] 51
#> 
#> $demographics$distributions$clusters_by_k$`4`$C1$age_mean
#> [1] 65.98039
#> 
#> $demographics$distributions$clusters_by_k$`4`$C1$age_median
#> [1] 66
#> 
#> $demographics$distributions$clusters_by_k$`4`$C1$age_std
#> [1] 5.355921
#> 
#> $demographics$distributions$clusters_by_k$`4`$C1$age_q1
#> [1] 61.5
#> 
#> $demographics$distributions$clusters_by_k$`4`$C1$age_q3
#> [1] 70.5
#> 
#> $demographics$distributions$clusters_by_k$`4`$C1$n
#> [1] 51
#> 
#> $demographics$distributions$clusters_by_k$`4`$C1$male_proportion
#> [1] 0.6666667
#> 
#> 
#> $demographics$distributions$clusters_by_k$`4`$C2
#> $demographics$distributions$clusters_by_k$`4`$C2$patient_count
#> [1] 170
#> 
#> $demographics$distributions$clusters_by_k$`4`$C2$age_mean
#> [1] 56.64118
#> 
#> $demographics$distributions$clusters_by_k$`4`$C2$age_median
#> [1] 56
#> 
#> $demographics$distributions$clusters_by_k$`4`$C2$age_std
#> [1] 5.501396
#> 
#> $demographics$distributions$clusters_by_k$`4`$C2$age_q1
#> [1] 52
#> 
#> $demographics$distributions$clusters_by_k$`4`$C2$age_q3
#> [1] 62
#> 
#> $demographics$distributions$clusters_by_k$`4`$C2$n
#> [1] 170
#> 
#> $demographics$distributions$clusters_by_k$`4`$C2$male_proportion
#> [1] 0.5823529
#> 
#> 
#> $demographics$distributions$clusters_by_k$`4`$C3
#> $demographics$distributions$clusters_by_k$`4`$C3$patient_count
#> [1] 119
#> 
#> $demographics$distributions$clusters_by_k$`4`$C3$age_mean
#> [1] 67.05882
#> 
#> $demographics$distributions$clusters_by_k$`4`$C3$age_median
#> [1] 67
#> 
#> $demographics$distributions$clusters_by_k$`4`$C3$age_std
#> [1] 4.93026
#> 
#> $demographics$distributions$clusters_by_k$`4`$C3$age_q1
#> [1] 63
#> 
#> $demographics$distributions$clusters_by_k$`4`$C3$age_q3
#> [1] 71
#> 
#> $demographics$distributions$clusters_by_k$`4`$C3$n
#> [1] 119
#> 
#> $demographics$distributions$clusters_by_k$`4`$C3$male_proportion
#> [1] 0.5546218
#> 
#> 
#> $demographics$distributions$clusters_by_k$`4`$C4
#> $demographics$distributions$clusters_by_k$`4`$C4$patient_count
#> [1] 160
#> 
#> $demographics$distributions$clusters_by_k$`4`$C4$age_mean
#> [1] 78.24375
#> 
#> $demographics$distributions$clusters_by_k$`4`$C4$age_median
#> [1] 78
#> 
#> $demographics$distributions$clusters_by_k$`4`$C4$age_std
#> [1] 6.020535
#> 
#> $demographics$distributions$clusters_by_k$`4`$C4$age_q1
#> [1] 73
#> 
#> $demographics$distributions$clusters_by_k$`4`$C4$age_q3
#> [1] 84
#> 
#> $demographics$distributions$clusters_by_k$`4`$C4$n
#> [1] 160
#> 
#> $demographics$distributions$clusters_by_k$`4`$C4$male_proportion
#> [1] 0.5125
#> 
#> 
#> 
#> $demographics$distributions$clusters_by_k$`5`
#> $demographics$distributions$clusters_by_k$`5`$C1
#> $demographics$distributions$clusters_by_k$`5`$C1$patient_count
#> [1] 126
#> 
#> $demographics$distributions$clusters_by_k$`5`$C1$age_mean
#> [1] 56.46032
#> 
#> $demographics$distributions$clusters_by_k$`5`$C1$age_median
#> [1] 55.5
#> 
#> $demographics$distributions$clusters_by_k$`5`$C1$age_std
#> [1] 5.542979
#> 
#> $demographics$distributions$clusters_by_k$`5`$C1$age_q1
#> [1] 52
#> 
#> $demographics$distributions$clusters_by_k$`5`$C1$age_q3
#> [1] 62
#> 
#> $demographics$distributions$clusters_by_k$`5`$C1$n
#> [1] 126
#> 
#> $demographics$distributions$clusters_by_k$`5`$C1$male_proportion
#> [1] 0.5873016
#> 
#> 
#> $demographics$distributions$clusters_by_k$`5`$C2
#> $demographics$distributions$clusters_by_k$`5`$C2$patient_count
#> [1] 119
#> 
#> $demographics$distributions$clusters_by_k$`5`$C2$age_mean
#> [1] 67.05882
#> 
#> $demographics$distributions$clusters_by_k$`5`$C2$age_median
#> [1] 67
#> 
#> $demographics$distributions$clusters_by_k$`5`$C2$age_std
#> [1] 4.93026
#> 
#> $demographics$distributions$clusters_by_k$`5`$C2$age_q1
#> [1] 63
#> 
#> $demographics$distributions$clusters_by_k$`5`$C2$age_q3
#> [1] 71
#> 
#> $demographics$distributions$clusters_by_k$`5`$C2$n
#> [1] 119
#> 
#> $demographics$distributions$clusters_by_k$`5`$C2$male_proportion
#> [1] 0.5546218
#> 
#> 
#> $demographics$distributions$clusters_by_k$`5`$C3
#> $demographics$distributions$clusters_by_k$`5`$C3$patient_count
#> [1] 44
#> 
#> $demographics$distributions$clusters_by_k$`5`$C3$age_mean
#> [1] 57.15909
#> 
#> $demographics$distributions$clusters_by_k$`5`$C3$age_median
#> [1] 57
#> 
#> $demographics$distributions$clusters_by_k$`5`$C3$age_std
#> [1] 5.346805
#> 
#> $demographics$distributions$clusters_by_k$`5`$C3$age_q1
#> [1] 53
#> 
#> $demographics$distributions$clusters_by_k$`5`$C3$age_q3
#> [1] 62.25
#> 
#> $demographics$distributions$clusters_by_k$`5`$C3$n
#> [1] 44
#> 
#> $demographics$distributions$clusters_by_k$`5`$C3$male_proportion
#> [1] 0.5681818
#> 
#> 
#> $demographics$distributions$clusters_by_k$`5`$C4
#> $demographics$distributions$clusters_by_k$`5`$C4$patient_count
#> [1] 160
#> 
#> $demographics$distributions$clusters_by_k$`5`$C4$age_mean
#> [1] 78.24375
#> 
#> $demographics$distributions$clusters_by_k$`5`$C4$age_median
#> [1] 78
#> 
#> $demographics$distributions$clusters_by_k$`5`$C4$age_std
#> [1] 6.020535
#> 
#> $demographics$distributions$clusters_by_k$`5`$C4$age_q1
#> [1] 73
#> 
#> $demographics$distributions$clusters_by_k$`5`$C4$age_q3
#> [1] 84
#> 
#> $demographics$distributions$clusters_by_k$`5`$C4$n
#> [1] 160
#> 
#> $demographics$distributions$clusters_by_k$`5`$C4$male_proportion
#> [1] 0.5125
#> 
#> 
#> $demographics$distributions$clusters_by_k$`5`$C5
#> $demographics$distributions$clusters_by_k$`5`$C5$patient_count
#> [1] 51
#> 
#> $demographics$distributions$clusters_by_k$`5`$C5$age_mean
#> [1] 65.98039
#> 
#> $demographics$distributions$clusters_by_k$`5`$C5$age_median
#> [1] 66
#> 
#> $demographics$distributions$clusters_by_k$`5`$C5$age_std
#> [1] 5.355921
#> 
#> $demographics$distributions$clusters_by_k$`5`$C5$age_q1
#> [1] 61.5
#> 
#> $demographics$distributions$clusters_by_k$`5`$C5$age_q3
#> [1] 70.5
#> 
#> $demographics$distributions$clusters_by_k$`5`$C5$n
#> [1] 51
#> 
#> $demographics$distributions$clusters_by_k$`5`$C5$male_proportion
#> [1] 0.6666667
#> 
#> 
#> 
#> 
#> $demographics$distributions$clusters_best_k
#> [1] 3
#> 
#> 
#> 
#> $metadata
#> $metadata$study_name
#> [1] "lc500s"
#> 
#> $metadata$original_study_name
#> [1] "lc500"
#> 
#> $metadata$source_path
#> [1] "inst/example/st/lc500"
#> 
#> $metadata$mode
#> [1] "summary"
#> 
#> $metadata$demographics
#> $metadata$demographics$target_patients
#> [1] 500
#> 
#> $metadata$demographics$control_patients
#> [1] 346
#> 
#> $metadata$demographics$age_mean
#> [1] 66.986
#> 
#> $metadata$demographics$age_median
#> [1] 66
#> 
#> $metadata$demographics$age_std
#> [1] 10.38564
#> 
#> $metadata$demographics$male_proportion
#> [1] 0.562
#> 
#> $metadata$demographics$distributions
#> $metadata$demographics$distributions$overall
#> $metadata$demographics$distributions$overall$age_histogram
#> $metadata$demographics$distributions$overall$age_histogram$bins
#>  [1] "0-10"   "10-20"  "20-30"  "30-40"  "40-50"  "50-60"  "60-70"  "70-80" 
#>  [9] "80-90"  "90-100"
#> 
#> $metadata$demographics$distributions$overall$age_histogram$counts
#>  [1]   0   0   0   0  23 108 160 144  65   0
#> 
#> $metadata$demographics$distributions$overall$age_histogram$mean
#> [1] 66.986
#> 
#> $metadata$demographics$distributions$overall$age_histogram$median
#> [1] 66
#> 
#> $metadata$demographics$distributions$overall$age_histogram$std
#> [1] 10.37525
#> 
#> $metadata$demographics$distributions$overall$age_histogram$q1
#> [1] 59
#> 
#> $metadata$demographics$distributions$overall$age_histogram$q3
#> [1] 74
#> 
#> $metadata$demographics$distributions$overall$age_histogram$n
#> [1] 500
#> 
#> 
#> $metadata$demographics$distributions$overall$sex_distribution
#> $metadata$demographics$distributions$overall$sex_distribution$male
#> [1] 281
#> 
#> $metadata$demographics$distributions$overall$sex_distribution$female
#> [1] 219
#> 
#> $metadata$demographics$distributions$overall$sex_distribution$total
#> [1] 500
#> 
#> 
#> 
#> $metadata$demographics$distributions$clusters
#> $metadata$demographics$distributions$clusters$C1
#> $metadata$demographics$distributions$clusters$C1$patient_count
#> [1] 170
#> 
#> $metadata$demographics$distributions$clusters$C1$age_mean
#> [1] 56.64118
#> 
#> $metadata$demographics$distributions$clusters$C1$age_median
#> [1] 56
#> 
#> $metadata$demographics$distributions$clusters$C1$age_std
#> [1] 5.501396
#> 
#> $metadata$demographics$distributions$clusters$C1$age_q1
#> [1] 52
#> 
#> $metadata$demographics$distributions$clusters$C1$age_q3
#> [1] 62
#> 
#> $metadata$demographics$distributions$clusters$C1$n
#> [1] 170
#> 
#> $metadata$demographics$distributions$clusters$C1$male_proportion
#> [1] 0.5823529
#> 
#> 
#> $metadata$demographics$distributions$clusters$C2
#> $metadata$demographics$distributions$clusters$C2$patient_count
#> [1] 160
#> 
#> $metadata$demographics$distributions$clusters$C2$age_mean
#> [1] 78.24375
#> 
#> $metadata$demographics$distributions$clusters$C2$age_median
#> [1] 78
#> 
#> $metadata$demographics$distributions$clusters$C2$age_std
#> [1] 6.020535
#> 
#> $metadata$demographics$distributions$clusters$C2$age_q1
#> [1] 73
#> 
#> $metadata$demographics$distributions$clusters$C2$age_q3
#> [1] 84
#> 
#> $metadata$demographics$distributions$clusters$C2$n
#> [1] 160
#> 
#> $metadata$demographics$distributions$clusters$C2$male_proportion
#> [1] 0.5125
#> 
#> 
#> $metadata$demographics$distributions$clusters$C3
#> $metadata$demographics$distributions$clusters$C3$patient_count
#> [1] 170
#> 
#> $metadata$demographics$distributions$clusters$C3$age_mean
#> [1] 66.73529
#> 
#> $metadata$demographics$distributions$clusters$C3$age_median
#> [1] 67
#> 
#> $metadata$demographics$distributions$clusters$C3$age_std
#> [1] 5.085787
#> 
#> $metadata$demographics$distributions$clusters$C3$age_q1
#> [1] 63
#> 
#> $metadata$demographics$distributions$clusters$C3$age_q3
#> [1] 71
#> 
#> $metadata$demographics$distributions$clusters$C3$n
#> [1] 170
#> 
#> $metadata$demographics$distributions$clusters$C3$male_proportion
#> [1] 0.5882353
#> 
#> 
#> 
#> $metadata$demographics$distributions$clusters_by_k
#> $metadata$demographics$distributions$clusters_by_k$`2`
#> $metadata$demographics$distributions$clusters_by_k$`2`$C1
#> $metadata$demographics$distributions$clusters_by_k$`2`$C1$patient_count
#> [1] 336
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C1$age_mean
#> [1] 61.62202
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C1$age_median
#> [1] 62
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C1$age_std
#> [1] 7.325956
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C1$age_q1
#> [1] 56
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C1$age_q3
#> [1] 67
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C1$n
#> [1] 336
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C1$male_proportion
#> [1] 0.5833333
#> 
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C2
#> $metadata$demographics$distributions$clusters_by_k$`2`$C2$patient_count
#> [1] 164
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C2$age_mean
#> [1] 77.97561
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C2$age_median
#> [1] 78
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C2$age_std
#> [1] 6.205772
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C2$age_q1
#> [1] 73
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C2$age_q3
#> [1] 83.25
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C2$n
#> [1] 164
#> 
#> $metadata$demographics$distributions$clusters_by_k$`2`$C2$male_proportion
#> [1] 0.5182927
#> 
#> 
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`
#> $metadata$demographics$distributions$clusters_by_k$`3`$C1
#> $metadata$demographics$distributions$clusters_by_k$`3`$C1$patient_count
#> [1] 170
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C1$age_mean
#> [1] 56.64118
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C1$age_median
#> [1] 56
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C1$age_std
#> [1] 5.501396
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C1$age_q1
#> [1] 52
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C1$age_q3
#> [1] 62
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C1$n
#> [1] 170
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C1$male_proportion
#> [1] 0.5823529
#> 
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C2
#> $metadata$demographics$distributions$clusters_by_k$`3`$C2$patient_count
#> [1] 160
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C2$age_mean
#> [1] 78.24375
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C2$age_median
#> [1] 78
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C2$age_std
#> [1] 6.020535
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C2$age_q1
#> [1] 73
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C2$age_q3
#> [1] 84
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C2$n
#> [1] 160
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C2$male_proportion
#> [1] 0.5125
#> 
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C3
#> $metadata$demographics$distributions$clusters_by_k$`3`$C3$patient_count
#> [1] 170
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C3$age_mean
#> [1] 66.73529
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C3$age_median
#> [1] 67
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C3$age_std
#> [1] 5.085787
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C3$age_q1
#> [1] 63
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C3$age_q3
#> [1] 71
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C3$n
#> [1] 170
#> 
#> $metadata$demographics$distributions$clusters_by_k$`3`$C3$male_proportion
#> [1] 0.5882353
#> 
#> 
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`
#> $metadata$demographics$distributions$clusters_by_k$`4`$C1
#> $metadata$demographics$distributions$clusters_by_k$`4`$C1$patient_count
#> [1] 51
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C1$age_mean
#> [1] 65.98039
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C1$age_median
#> [1] 66
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C1$age_std
#> [1] 5.355921
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C1$age_q1
#> [1] 61.5
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C1$age_q3
#> [1] 70.5
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C1$n
#> [1] 51
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C1$male_proportion
#> [1] 0.6666667
#> 
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C2
#> $metadata$demographics$distributions$clusters_by_k$`4`$C2$patient_count
#> [1] 170
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C2$age_mean
#> [1] 56.64118
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C2$age_median
#> [1] 56
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C2$age_std
#> [1] 5.501396
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C2$age_q1
#> [1] 52
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C2$age_q3
#> [1] 62
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C2$n
#> [1] 170
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C2$male_proportion
#> [1] 0.5823529
#> 
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C3
#> $metadata$demographics$distributions$clusters_by_k$`4`$C3$patient_count
#> [1] 119
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C3$age_mean
#> [1] 67.05882
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C3$age_median
#> [1] 67
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C3$age_std
#> [1] 4.93026
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C3$age_q1
#> [1] 63
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C3$age_q3
#> [1] 71
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C3$n
#> [1] 119
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C3$male_proportion
#> [1] 0.5546218
#> 
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C4
#> $metadata$demographics$distributions$clusters_by_k$`4`$C4$patient_count
#> [1] 160
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C4$age_mean
#> [1] 78.24375
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C4$age_median
#> [1] 78
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C4$age_std
#> [1] 6.020535
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C4$age_q1
#> [1] 73
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C4$age_q3
#> [1] 84
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C4$n
#> [1] 160
#> 
#> $metadata$demographics$distributions$clusters_by_k$`4`$C4$male_proportion
#> [1] 0.5125
#> 
#> 
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`
#> $metadata$demographics$distributions$clusters_by_k$`5`$C1
#> $metadata$demographics$distributions$clusters_by_k$`5`$C1$patient_count
#> [1] 126
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C1$age_mean
#> [1] 56.46032
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C1$age_median
#> [1] 55.5
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C1$age_std
#> [1] 5.542979
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C1$age_q1
#> [1] 52
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C1$age_q3
#> [1] 62
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C1$n
#> [1] 126
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C1$male_proportion
#> [1] 0.5873016
#> 
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C2
#> $metadata$demographics$distributions$clusters_by_k$`5`$C2$patient_count
#> [1] 119
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C2$age_mean
#> [1] 67.05882
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C2$age_median
#> [1] 67
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C2$age_std
#> [1] 4.93026
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C2$age_q1
#> [1] 63
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C2$age_q3
#> [1] 71
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C2$n
#> [1] 119
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C2$male_proportion
#> [1] 0.5546218
#> 
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C3
#> $metadata$demographics$distributions$clusters_by_k$`5`$C3$patient_count
#> [1] 44
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C3$age_mean
#> [1] 57.15909
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C3$age_median
#> [1] 57
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C3$age_std
#> [1] 5.346805
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C3$age_q1
#> [1] 53
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C3$age_q3
#> [1] 62.25
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C3$n
#> [1] 44
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C3$male_proportion
#> [1] 0.5681818
#> 
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C4
#> $metadata$demographics$distributions$clusters_by_k$`5`$C4$patient_count
#> [1] 160
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C4$age_mean
#> [1] 78.24375
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C4$age_median
#> [1] 78
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C4$age_std
#> [1] 6.020535
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C4$age_q1
#> [1] 73
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C4$age_q3
#> [1] 84
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C4$n
#> [1] 160
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C4$male_proportion
#> [1] 0.5125
#> 
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C5
#> $metadata$demographics$distributions$clusters_by_k$`5`$C5$patient_count
#> [1] 51
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C5$age_mean
#> [1] 65.98039
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C5$age_median
#> [1] 66
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C5$age_std
#> [1] 5.355921
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C5$age_q1
#> [1] 61.5
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C5$age_q3
#> [1] 70.5
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C5$n
#> [1] 51
#> 
#> $metadata$demographics$distributions$clusters_by_k$`5`$C5$male_proportion
#> [1] 0.6666667
#> 
#> 
#> 
#> 
#> $metadata$demographics$distributions$clusters_best_k
#> [1] 3
#> 
#> 
#> 
#> $metadata$clustering
#> $metadata$clustering$`2`
#> $metadata$clustering$`2`$silhouette_score
#> [1] 0.2979832
#> 
#> $metadata$clustering$`2`$cluster_sizes
#> $metadata$clustering$`2`$cluster_sizes$C1
#> [1] 336
#> 
#> $metadata$clustering$`2`$cluster_sizes$C2
#> [1] 164
#> 
#> 
#> $metadata$clustering$`2`$total_patients
#> [1] 500
#> 
#> $metadata$clustering$`2`$concepts_used_count
#> [1] 16
#> 
#> $metadata$clustering$`2`$sampled
#> [1] FALSE
#> 
#> 
#> $metadata$clustering$`3`
#> $metadata$clustering$`3`$silhouette_score
#> [1] 0.4183669
#> 
#> $metadata$clustering$`3`$cluster_sizes
#> $metadata$clustering$`3`$cluster_sizes$C1
#> [1] 170
#> 
#> $metadata$clustering$`3`$cluster_sizes$C3
#> [1] 170
#> 
#> $metadata$clustering$`3`$cluster_sizes$C2
#> [1] 160
#> 
#> 
#> $metadata$clustering$`3`$total_patients
#> [1] 500
#> 
#> $metadata$clustering$`3`$concepts_used_count
#> [1] 16
#> 
#> $metadata$clustering$`3`$sampled
#> [1] FALSE
#> 
#> 
#> $metadata$clustering$`4`
#> $metadata$clustering$`4`$silhouette_score
#> [1] 0.3281872
#> 
#> $metadata$clustering$`4`$cluster_sizes
#> $metadata$clustering$`4`$cluster_sizes$C2
#> [1] 170
#> 
#> $metadata$clustering$`4`$cluster_sizes$C4
#> [1] 160
#> 
#> $metadata$clustering$`4`$cluster_sizes$C3
#> [1] 119
#> 
#> $metadata$clustering$`4`$cluster_sizes$C1
#> [1] 51
#> 
#> 
#> $metadata$clustering$`4`$total_patients
#> [1] 500
#> 
#> $metadata$clustering$`4`$concepts_used_count
#> [1] 16
#> 
#> $metadata$clustering$`4`$sampled
#> [1] FALSE
#> 
#> 
#> $metadata$clustering$`5`
#> $metadata$clustering$`5`$silhouette_score
#> [1] 0.2505048
#> 
#> $metadata$clustering$`5`$cluster_sizes
#> $metadata$clustering$`5`$cluster_sizes$C4
#> [1] 160
#> 
#> $metadata$clustering$`5`$cluster_sizes$C1
#> [1] 126
#> 
#> $metadata$clustering$`5`$cluster_sizes$C2
#> [1] 119
#> 
#> $metadata$clustering$`5`$cluster_sizes$C5
#> [1] 51
#> 
#> $metadata$clustering$`5`$cluster_sizes$C3
#> [1] 44
#> 
#> 
#> $metadata$clustering$`5`$total_patients
#> [1] 500
#> 
#> $metadata$clustering$`5`$concepts_used_count
#> [1] 16
#> 
#> $metadata$clustering$`5`$sampled
#> [1] FALSE
#> 
#> 
#> 
#> $metadata$cluster_k_values
#> [1] 2 3 4 5
#> 
#> $metadata$concept_limit
#> [1] 60
#> 
#> $metadata$min_cell_count
#> [1] 5
#> 
#> $metadata$significant_concepts
#> [1] 16
#> 
#> $metadata$clustering_guardrails
#> $metadata$clustering_guardrails$cluster_feature_matrix_cell_threshold
#> [1] 50000000
#> 
#> $metadata$clustering_guardrails$pairwise_overlap_max_concepts
#> [1] 500
```

## File Inventory

``` r

allFiles <- list.files(studyPath, full.names = FALSE)
allFiles
#>  [1] "clustering_k2_pairwise_overlap.parquet"
#>  [2] "clustering_k2_summary.parquet"         
#>  [3] "clustering_k3_pairwise_overlap.parquet"
#>  [4] "clustering_k3_summary.parquet"         
#>  [5] "clustering_k4_pairwise_overlap.parquet"
#>  [6] "clustering_k4_summary.parquet"         
#>  [7] "clustering_k5_pairwise_overlap.parquet"
#>  [8] "clustering_k5_summary.parquet"         
#>  [9] "complementaryMappingTable.parquet"     
#> [10] "concept_summaries.parquet"             
#> [11] "desc.txt"                              
#> [12] "metadata.json"                         
#> [13] "ordinal_summaries.parquet"
```

``` r

summaryParquetFiles <- list.files(studyPath, pattern = "[.]parquet$", full.names = TRUE)
summaryParquetFiles <- summaryParquetFiles[order(basename(summaryParquetFiles))]

readSummaryParquet <- function(path) {
  as.data.frame(nanoparquet::read_parquet(path), stringsAsFactors = FALSE)
}

summaryTables <- stats::setNames(
  lapply(summaryParquetFiles, readSummaryParquet),
  basename(summaryParquetFiles)
)

data.frame(
  file = names(summaryTables),
  rows = vapply(summaryTables, nrow, integer(1)),
  cols = vapply(summaryTables, ncol, integer(1)),
  stringsAsFactors = FALSE
)
#>                                                                          file
#> clustering_k2_pairwise_overlap.parquet clustering_k2_pairwise_overlap.parquet
#> clustering_k2_summary.parquet                   clustering_k2_summary.parquet
#> clustering_k3_pairwise_overlap.parquet clustering_k3_pairwise_overlap.parquet
#> clustering_k3_summary.parquet                   clustering_k3_summary.parquet
#> clustering_k4_pairwise_overlap.parquet clustering_k4_pairwise_overlap.parquet
#> clustering_k4_summary.parquet                   clustering_k4_summary.parquet
#> clustering_k5_pairwise_overlap.parquet clustering_k5_pairwise_overlap.parquet
#> clustering_k5_summary.parquet                   clustering_k5_summary.parquet
#> complementaryMappingTable.parquet           complementaryMappingTable.parquet
#> concept_summaries.parquet                           concept_summaries.parquet
#> ordinal_summaries.parquet                           ordinal_summaries.parquet
#>                                        rows cols
#> clustering_k2_pairwise_overlap.parquet  408    9
#> clustering_k2_summary.parquet            58   17
#> clustering_k3_pairwise_overlap.parquet  544    9
#> clustering_k3_summary.parquet            83   17
#> clustering_k4_pairwise_overlap.parquet  680    9
#> clustering_k4_summary.parquet           100   17
#> clustering_k5_pairwise_overlap.parquet  816    9
#> clustering_k5_summary.parquet           123   17
#> complementaryMappingTable.parquet         0    6
#> concept_summaries.parquet                16   27
#> ordinal_summaries.parquet                17   31
```

## Metadata (`metadata.json`)

`metadata.json` is the study-level control plane for summary mode:

- mode and provenance (`study_name`, `original_study_name`,
  `source_path`, `mode`)
- overall demographics
- per-cluster diagnostics for each precomputed `k`
- guardrails and precompute settings

``` r

metadata <- jsonlite::fromJSON(file.path(studyPath, "metadata.json"), simplifyVector = FALSE)
```

### Top-Level Key Dictionary

``` r

metadataKeyDescriptions <- c(
  study_name = "Summary study folder name.",
  original_study_name = "Source patient-level study used to produce summary artifacts.",
  source_path = "Path of source study during precompute execution.",
  mode = "Data mode marker; expected to be 'summary'.",
  demographics = "Nested cohort and age/sex summary block for overall and cluster views.",
  clustering = "Nested clustering quality/size metrics keyed by k.",
  cluster_k_values = "Vector of k values precomputed for summary mode.",
  concept_limit = "Maximum number of concepts used by clustering precompute.",
  min_cell_count = "Suppression threshold used in summary generation.",
  significant_concepts = "Count of significant concepts exported to summary artifacts.",
  clustering_guardrails = "Runtime guardrails for clustering matrix size and overlap computations."
)

metadataTypes <- vapply(metadata, function(x) class(x)[1], character(1))
data.frame(
  key = names(metadata),
  type = unname(metadataTypes[names(metadata)]),
  description = unname(metadataKeyDescriptions[names(metadata)]),
  stringsAsFactors = FALSE
)
#>                      key      type
#> 1             study_name character
#> 2    original_study_name character
#> 3            source_path character
#> 4                   mode character
#> 5           demographics      list
#> 6             clustering      list
#> 7       cluster_k_values      list
#> 8          concept_limit   integer
#> 9         min_cell_count   integer
#> 10  significant_concepts   integer
#> 11 clustering_guardrails      list
#>                                                                description
#> 1                                               Summary study folder name.
#> 2            Source patient-level study used to produce summary artifacts.
#> 3                        Path of source study during precompute execution.
#> 4                              Data mode marker; expected to be 'summary'.
#> 5   Nested cohort and age/sex summary block for overall and cluster views.
#> 6                       Nested clustering quality/size metrics keyed by k.
#> 7                         Vector of k values precomputed for summary mode.
#> 8                Maximum number of concepts used by clustering precompute.
#> 9                        Suppression threshold used in summary generation.
#> 10            Count of significant concepts exported to summary artifacts.
#> 11 Runtime guardrails for clustering matrix size and overlap computations.
```

### Key Nested Structures

``` r

data.frame(
  block = c(
    "demographics",
    "demographics$distributions$overall",
    "demographics$distributions$clusters",
    "demographics$distributions$clusters_by_k",
    "clustering",
    "clustering_guardrails"
  ),
  keys = c(
    paste(names(metadata$demographics), collapse = ", "),
    paste(names(metadata$demographics$distributions$overall), collapse = ", "),
    paste(names(metadata$demographics$distributions$clusters), collapse = ", "),
    paste(names(metadata$demographics$distributions$clusters_by_k), collapse = ", "),
    paste(names(metadata$clustering), collapse = ", "),
    paste(names(metadata$clustering_guardrails), collapse = ", ")
  ),
  stringsAsFactors = FALSE
)
#>                                      block
#> 1                             demographics
#> 2       demographics$distributions$overall
#> 3      demographics$distributions$clusters
#> 4 demographics$distributions$clusters_by_k
#> 5                               clustering
#> 6                    clustering_guardrails
#>                                                                                               keys
#> 1 target_patients, control_patients, age_mean, age_median, age_std, male_proportion, distributions
#> 2                                                                  age_histogram, sex_distribution
#> 3                                                                                       C1, C2, C3
#> 4                                                                                       2, 3, 4, 5
#> 5                                                                                       2, 3, 4, 5
#> 6                             cluster_feature_matrix_cell_threshold, pairwise_overlap_max_concepts
```

``` r

data.frame(
  metric = c(
    "mode",
    "target_patients",
    "control_patients",
    "clusters_best_k",
    "cluster_k_values",
    "significant_concepts"
  ),
  value = c(
    metadata$mode,
    metadata$demographics$target_patients,
    metadata$demographics$control_patients,
    metadata$demographics$distributions$clusters_best_k,
    paste(unlist(metadata$cluster_k_values), collapse = ", "),
    metadata$significant_concepts
  ),
  stringsAsFactors = FALSE
)
#>                 metric      value
#> 1                 mode    summary
#> 2      target_patients        500
#> 3     control_patients        346
#> 4      clusters_best_k          3
#> 5     cluster_k_values 2, 3, 4, 5
#> 6 significant_concepts         16
```

## Helper for Column Dictionaries

``` r

buildColumnDictionary <- function(df, descriptionMap, exampleMap = NULL) {
  cols <- colnames(df)
  out <- data.frame(
    column = cols,
    type = vapply(df, function(x) paste(class(x), collapse = "|"), character(1)),
    description = unname(descriptionMap[cols]),
    stringsAsFactors = FALSE
  )
  out$description[is.na(out$description)] <- "Not documented in this vignette."
  if (!is.null(exampleMap)) {
    out$lc500s_example <- unname(exampleMap[cols])
    out$lc500s_example[is.na(out$lc500s_example)] <- ""
  }
  out
}
```

## `concept_summaries.parquet`

Row grain: one concept (main concept-level summary in summary mode).

### Column Dictionary

``` r

conceptSummaries <- summaryTables[["concept_summaries.parquet"]]

conceptDescriptions <- c(
  CONCEPT_ID = "Concept identifier (string-normalized) used across summary artifacts.",
  HERITAGE = "OMOP domain/heritage for the concept.",
  time_count = "Total number of recorded concept-time observations used for time summary.",
  time_min = "Minimum concept time-to-event (days).",
  time_max = "Maximum concept time-to-event (days).",
  time_mean = "Mean concept time-to-event.",
  time_median = "Median concept time-to-event.",
  time_std = "Standard deviation of concept time-to-event.",
  time_q1 = "First quartile (25th percentile) of concept time-to-event.",
  time_q3 = "Third quartile (75th percentile) of concept time-to-event.",
  time_iqr = "Interquartile range (time_q3 - time_q1).",
  patient_count = "Number of target patients with at least one occurrence of this concept.",
  CONCEPT_NAME = "Human-readable concept name.",
  time_histogram_bins = "JSON-encoded array of histogram bin edges for concept timing distribution.",
  time_histogram_counts = "JSON-encoded array of histogram counts aligned to histogram bins.",
  time_kde_x = "JSON-encoded x-grid for time KDE curve.",
  time_kde_y = "JSON-encoded y-values for time KDE curve.",
  age_mean = "Mean age among target patients with the concept.",
  age_median = "Median age among target patients with the concept.",
  age_std = "Age standard deviation among target patients with the concept.",
  age_q1 = "First quartile of age among target patients with concept.",
  age_q3 = "Third quartile of age among target patients with concept.",
  n_ages = "Number of patients with non-missing age contributing to age statistics.",
  male_proportion = "Proportion male among concept-positive target patients.",
  TARGET_SUBJECT_PREVALENCE = "Proportion of target cohort with this concept.",
  CONTROL_SUBJECT_PREVALENCE = "Proportion of control cohort with this concept.",
  PREVALENCE_DIFFERENCE_RATIO = "Target/control prevalence ratio."
)

conceptExamples <- c(
  CONCEPT_ID = paste("Unique concepts:", dplyr::n_distinct(conceptSummaries$CONCEPT_ID)),
  HERITAGE = paste("Values:", paste(sort(unique(conceptSummaries$HERITAGE)), collapse = ", ")),
  time_count = paste("Range:", paste(range(conceptSummaries$time_count, na.rm = TRUE), collapse = " to ")),
  time_min = paste("Range:", paste(range(conceptSummaries$time_min, na.rm = TRUE), collapse = " to ")),
  time_max = paste("Range:", paste(range(conceptSummaries$time_max, na.rm = TRUE), collapse = " to ")),
  time_mean = paste("Range:", paste(round(range(conceptSummaries$time_mean, na.rm = TRUE), 2), collapse = " to ")),
  time_median = paste("Range:", paste(range(conceptSummaries$time_median, na.rm = TRUE), collapse = " to ")),
  time_std = paste("Range:", paste(round(range(conceptSummaries$time_std, na.rm = TRUE), 2), collapse = " to ")),
  time_q1 = paste("Range:", paste(range(conceptSummaries$time_q1, na.rm = TRUE), collapse = " to ")),
  time_q3 = paste("Range:", paste(range(conceptSummaries$time_q3, na.rm = TRUE), collapse = " to ")),
  time_iqr = paste("Range:", paste(range(conceptSummaries$time_iqr, na.rm = TRUE), collapse = " to ")),
  patient_count = paste("Range:", paste(range(conceptSummaries$patient_count, na.rm = TRUE), collapse = " to ")),
  CONCEPT_NAME = paste("Rows:", nrow(conceptSummaries)),
  time_histogram_bins = paste("JSON string; starts with:", substr(conceptSummaries$time_histogram_bins[1], 1, 30)),
  time_histogram_counts = paste("JSON string; starts with:", substr(conceptSummaries$time_histogram_counts[1], 1, 30)),
  time_kde_x = paste("JSON string; starts with:", substr(conceptSummaries$time_kde_x[1], 1, 30)),
  time_kde_y = paste("JSON string; starts with:", substr(conceptSummaries$time_kde_y[1], 1, 30)),
  age_mean = paste("Range:", paste(round(range(conceptSummaries$age_mean, na.rm = TRUE), 2), collapse = " to ")),
  age_median = paste("Range:", paste(range(conceptSummaries$age_median, na.rm = TRUE), collapse = " to ")),
  age_std = paste("Range:", paste(round(range(conceptSummaries$age_std, na.rm = TRUE), 2), collapse = " to ")),
  age_q1 = paste("Range:", paste(range(conceptSummaries$age_q1, na.rm = TRUE), collapse = " to ")),
  age_q3 = paste("Range:", paste(range(conceptSummaries$age_q3, na.rm = TRUE), collapse = " to ")),
  n_ages = paste("Range:", paste(range(conceptSummaries$n_ages, na.rm = TRUE), collapse = " to ")),
  male_proportion = paste("Range:", paste(round(range(conceptSummaries$male_proportion, na.rm = TRUE), 3), collapse = " to ")),
  TARGET_SUBJECT_PREVALENCE = paste("Range:", paste(range(conceptSummaries$TARGET_SUBJECT_PREVALENCE, na.rm = TRUE), collapse = " to ")),
  CONTROL_SUBJECT_PREVALENCE = paste("Range:", paste(range(conceptSummaries$CONTROL_SUBJECT_PREVALENCE, na.rm = TRUE), collapse = " to ")),
  PREVALENCE_DIFFERENCE_RATIO = paste("Range:", paste(round(range(conceptSummaries$PREVALENCE_DIFFERENCE_RATIO, na.rm = TRUE), 3), collapse = " to "))
)

buildColumnDictionary(conceptSummaries, conceptDescriptions, conceptExamples)
#>                                                  column      type
#> CONCEPT_ID                                   CONCEPT_ID character
#> HERITAGE                                       HERITAGE character
#> time_count                                   time_count   numeric
#> time_min                                       time_min   numeric
#> time_max                                       time_max   numeric
#> time_mean                                     time_mean   numeric
#> time_median                                 time_median   numeric
#> time_std                                       time_std   numeric
#> time_q1                                         time_q1   numeric
#> time_q3                                         time_q3   numeric
#> time_iqr                                       time_iqr   numeric
#> patient_count                             patient_count   numeric
#> CONCEPT_NAME                               CONCEPT_NAME character
#> time_histogram_bins                 time_histogram_bins character
#> time_histogram_counts             time_histogram_counts character
#> time_kde_x                                   time_kde_x character
#> time_kde_y                                   time_kde_y character
#> age_mean                                       age_mean   numeric
#> age_median                                   age_median   numeric
#> age_std                                         age_std   numeric
#> age_q1                                           age_q1   numeric
#> age_q3                                           age_q3   numeric
#> n_ages                                           n_ages   numeric
#> male_proportion                         male_proportion   numeric
#> TARGET_SUBJECT_PREVALENCE     TARGET_SUBJECT_PREVALENCE   numeric
#> CONTROL_SUBJECT_PREVALENCE   CONTROL_SUBJECT_PREVALENCE   numeric
#> PREVALENCE_DIFFERENCE_RATIO PREVALENCE_DIFFERENCE_RATIO   numeric
#>                                                                                            description
#> CONCEPT_ID                       Concept identifier (string-normalized) used across summary artifacts.
#> HERITAGE                                                         OMOP domain/heritage for the concept.
#> time_count                   Total number of recorded concept-time observations used for time summary.
#> time_min                                                         Minimum concept time-to-event (days).
#> time_max                                                         Maximum concept time-to-event (days).
#> time_mean                                                                  Mean concept time-to-event.
#> time_median                                                              Median concept time-to-event.
#> time_std                                                  Standard deviation of concept time-to-event.
#> time_q1                                     First quartile (25th percentile) of concept time-to-event.
#> time_q3                                     Third quartile (75th percentile) of concept time-to-event.
#> time_iqr                                                      Interquartile range (time_q3 - time_q1).
#> patient_count                  Number of target patients with at least one occurrence of this concept.
#> CONCEPT_NAME                                                              Human-readable concept name.
#> time_histogram_bins         JSON-encoded array of histogram bin edges for concept timing distribution.
#> time_histogram_counts                JSON-encoded array of histogram counts aligned to histogram bins.
#> time_kde_x                                                     JSON-encoded x-grid for time KDE curve.
#> time_kde_y                                                   JSON-encoded y-values for time KDE curve.
#> age_mean                                              Mean age among target patients with the concept.
#> age_median                                          Median age among target patients with the concept.
#> age_std                                 Age standard deviation among target patients with the concept.
#> age_q1                                       First quartile of age among target patients with concept.
#> age_q3                                       Third quartile of age among target patients with concept.
#> n_ages                         Number of patients with non-missing age contributing to age statistics.
#> male_proportion                                Proportion male among concept-positive target patients.
#> TARGET_SUBJECT_PREVALENCE                               Proportion of target cohort with this concept.
#> CONTROL_SUBJECT_PREVALENCE                             Proportion of control cohort with this concept.
#> PREVALENCE_DIFFERENCE_RATIO                                           Target/control prevalence ratio.
#>                                                                                                                lc500s_example
#> CONCEPT_ID                                                                                                Unique concepts: 16
#> HERITAGE                    Values: condition_occurrence, death, measurement, observation, procedure_occurrence, visit_detail
#> time_count                                                                                                 Range: 150 to 1565
#> time_min                                                                                                       Range: 0 to 53
#> time_max                                                                                                     Range: 18 to 320
#> time_mean                                                                                               Range: 6.15 to 239.34
#> time_median                                                                                                   Range: 6 to 248
#> time_std                                                                                                 Range: 3.88 to 41.32
#> time_q1                                                                                                       Range: 3 to 231
#> time_q3                                                                                                    Range: 9 to 258.75
#> time_iqr                                                                                                       Range: 6 to 47
#> patient_count                                                                                               Range: 147 to 483
#> CONCEPT_NAME                                                                                                         Rows: 16
#> time_histogram_bins                                                  JSON string; starts with: [0.0, 3.22, 6.44, 9.66, 12.88,
#> time_histogram_counts                                                JSON string; starts with: [5, 5, 5, 13, 26, 39, 37, 32, 
#> time_kde_x                                                           JSON string; starts with: [0.0, 1.6262626262626263, 3.25
#> time_kde_y                                                           JSON string; starts with: [0.0033527565186855495, 0.0046
#> age_mean                                                                                                Range: 56.75 to 78.35
#> age_median                                                                                                    Range: 56 to 78
#> age_std                                                                                                  Range: 5.44 to 10.58
#> age_q1                                                                                                        Range: 52 to 73
#> age_q3                                                                                                        Range: 62 to 84
#> n_ages                                                                                                      Range: 147 to 483
#> male_proportion                                                                                         Range: 0.483 to 0.601
#> TARGET_SUBJECT_PREVALENCE                                                                               Range: 0.294 to 0.966
#> CONTROL_SUBJECT_PREVALENCE                                                                                  Range: 0 to 0.462
#> PREVALENCE_DIFFERENCE_RATIO                                                                                Range: 1.97 to 100
```

### Example Rows

``` r

utils::head(conceptSummaries, 5)
#>   CONCEPT_ID             HERITAGE time_count time_min time_max time_mean
#> 1    2107967 procedure_occurrence        247        0      161  25.30769
#> 2    2107968 procedure_occurrence        171        0      147  22.96491
#> 3    2108158 procedure_occurrence        156        0       89  21.06410
#> 4      32280                death        541       11      320 224.82070
#> 5      32815                death        150       19      299 239.34000
#>   time_median  time_std time_q1 time_q3 time_iqr patient_count
#> 1          22 19.061157      17   28.00    11.00           165
#> 2          18 21.523966      13   24.00    11.00           168
#> 3          21  9.573943      16   26.00    10.00           156
#> 4         224 34.996074     202  249.00    47.00           156
#> 5         248 41.324194     231  258.75    27.75           147
#>            CONCEPT_NAME
#> 1             Lobectomy
#> 2     Lobectomy of lung
#> 3 Partial pneumonectomy
#> 4                 Death
#> 5     Death Certificate
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     time_histogram_bins
#> 1                                                                    [0.0, 3.22, 6.44, 9.66, 12.88, 16.1, 19.32, 22.540000000000003, 25.76, 28.98, 32.2, 35.42, 38.64, 41.86, 45.080000000000005, 48.300000000000004, 51.52, 54.74, 57.96, 61.18000000000001, 64.4, 67.62, 70.84, 74.06, 77.28, 80.5, 83.72, 86.94000000000001, 90.16000000000001, 93.38000000000001, 96.60000000000001, 99.82000000000001, 103.04, 106.26, 109.48, 112.7, 115.92, 119.14, 122.36000000000001, 125.58000000000001, 128.8, 132.02, 135.24, 138.46, 141.68, 144.9, 148.12, 151.34, 154.56, 157.78, 161.0]
#> 2                                                                                                                                                  [0.0, 2.94, 5.88, 8.82, 11.76, 14.7, 17.64, 20.58, 23.52, 26.46, 29.4, 32.339999999999996, 35.28, 38.22, 41.16, 44.1, 47.04, 49.98, 52.92, 55.86, 58.8, 61.74, 64.67999999999999, 67.62, 70.56, 73.5, 76.44, 79.38, 82.32, 85.26, 88.2, 91.14, 94.08, 97.02, 99.96, 102.89999999999999, 105.84, 108.78, 111.72, 114.66, 117.6, 120.53999999999999, 123.48, 126.42, 129.35999999999999, 132.3, 135.24, 138.18, 141.12, 144.06, 147.0]
#> 3                                                                                                                                                                              [0.0, 1.78, 3.56, 5.34, 7.12, 8.9, 10.68, 12.46, 14.24, 16.02, 17.8, 19.580000000000002, 21.36, 23.14, 24.92, 26.7, 28.48, 30.26, 32.04, 33.82, 35.6, 37.38, 39.160000000000004, 40.94, 42.72, 44.5, 46.28, 48.06, 49.84, 51.62, 53.4, 55.18, 56.96, 58.74, 60.52, 62.300000000000004, 64.08, 65.86, 67.64, 69.42, 71.2, 72.98, 74.76, 76.54, 78.32000000000001, 80.1, 81.88, 83.66, 85.44, 87.22, 89.0]
#> 4                                                [11.0, 17.18, 23.36, 29.54, 35.72, 41.9, 48.08, 54.26, 60.44, 66.62, 72.8, 78.97999999999999, 85.16, 91.34, 97.52, 103.69999999999999, 109.88, 116.06, 122.24, 128.42, 134.6, 140.78, 146.95999999999998, 153.14, 159.32, 165.5, 171.68, 177.85999999999999, 184.04, 190.22, 196.39999999999998, 202.57999999999998, 208.76, 214.94, 221.12, 227.29999999999998, 233.48, 239.66, 245.83999999999997, 252.01999999999998, 258.2, 264.38, 270.56, 276.74, 282.91999999999996, 289.09999999999997, 295.28, 301.46, 307.64, 313.82, 320.0]
#> 5 [19.0, 24.6, 30.2, 35.8, 41.4, 47.0, 52.599999999999994, 58.199999999999996, 63.8, 69.4, 75.0, 80.6, 86.19999999999999, 91.8, 97.39999999999999, 103.0, 108.6, 114.19999999999999, 119.8, 125.39999999999999, 131.0, 136.6, 142.2, 147.79999999999998, 153.39999999999998, 159.0, 164.6, 170.2, 175.79999999999998, 181.39999999999998, 187.0, 192.6, 198.2, 203.79999999999998, 209.39999999999998, 215.0, 220.6, 226.2, 231.79999999999998, 237.39999999999998, 243.0, 248.6, 254.2, 259.79999999999995, 265.4, 271.0, 276.59999999999997, 282.2, 287.79999999999995, 293.4, 299.0]
#>                                                                                                                                                       time_histogram_counts
#> 1           [5, 5, 5, 13, 26, 39, 37, 32, 32, 20, 13, 10, 5, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 5, 5, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 5, 0, 5, 0, 0, 0, 0, 5, 0, 0, 5, 0, 0, 0, 5]
#> 2            [5, 5, 10, 14, 21, 30, 22, 18, 21, 10, 6, 5, 5, 0, 5, 5, 0, 0, 0, 0, 5, 0, 5, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 0, 5, 0, 5, 0, 5, 0, 5, 0, 0, 0, 0, 0, 5]
#> 3             [5, 0, 5, 5, 5, 5, 5, 15, 10, 8, 17, 17, 19, 6, 12, 6, 11, 6, 5, 5, 5, 5, 0, 5, 0, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5]
#> 4 [5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 5, 5, 6, 5, 14, 17, 14, 22, 28, 23, 32, 34, 40, 52, 31, 39, 26, 34, 32, 20, 15, 13, 13, 6, 7, 6, 5, 5, 5]
#> 5              [5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 5, 0, 5, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 5, 5, 5, 5, 5, 9, 10, 15, 6, 18, 19, 16, 15, 7, 6, 5, 5, 5, 5]
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           time_kde_x
#> 1          [0.0, 1.6262626262626263, 3.2525252525252526, 4.878787878787879, 6.505050505050505, 8.131313131313131, 9.757575757575758, 11.383838383838384, 13.01010101010101, 14.636363636363637, 16.262626262626263, 17.88888888888889, 19.515151515151516, 21.141414141414142, 22.767676767676768, 24.393939393939394, 26.02020202020202, 27.646464646464647, 29.272727272727273, 30.8989898989899, 32.525252525252526, 34.151515151515156, 35.77777777777778, 37.4040404040404, 39.03030303030303, 40.65656565656566, 42.282828282828284, 43.90909090909091, 45.535353535353536, 47.161616161616166, 48.78787878787879, 50.41414141414141, 52.04040404040404, 53.66666666666667, 55.292929292929294, 56.91919191919192, 58.54545454545455, 60.17171717171718, 61.7979797979798, 63.42424242424242, 65.05050505050505, 66.67676767676768, 68.30303030303031, 69.92929292929293, 71.55555555555556, 73.18181818181819, 74.8080808080808, 76.43434343434343, 78.06060606060606, 79.68686868686869, 81.31313131313132, 82.93939393939394, 84.56565656565657, 86.1919191919192, 87.81818181818181, 89.44444444444444, 91.07070707070707, 92.6969696969697, 94.32323232323233, 95.94949494949495, 97.57575757575758, 99.20202020202021, 100.82828282828282, 102.45454545454545, 104.08080808080808, 105.70707070707071, 107.33333333333334, 108.95959595959596, 110.58585858585859, 112.21212121212122, 113.83838383838383, 115.46464646464646, 117.0909090909091, 118.71717171717172, 120.34343434343435, 121.96969696969697, 123.5959595959596, 125.22222222222223, 126.84848484848484, 128.4747474747475, 130.1010101010101, 131.72727272727272, 133.35353535353536, 134.97979797979798, 136.60606060606062, 138.23232323232324, 139.85858585858585, 141.4848484848485, 143.11111111111111, 144.73737373737373, 146.36363636363637, 147.989898989899, 149.6161616161616, 151.24242424242425, 152.86868686868686, 154.4949494949495, 156.12121212121212, 157.74747474747474, 159.37373737373738, 161.0]
#> 2                                   [0.0, 1.4848484848484849, 2.9696969696969697, 4.454545454545455, 5.9393939393939394, 7.424242424242424, 8.90909090909091, 10.393939393939394, 11.878787878787879, 13.363636363636363, 14.848484848484848, 16.333333333333332, 17.81818181818182, 19.303030303030305, 20.78787878787879, 22.272727272727273, 23.757575757575758, 25.242424242424242, 26.727272727272727, 28.21212121212121, 29.696969696969695, 31.181818181818183, 32.666666666666664, 34.15151515151515, 35.63636363636364, 37.121212121212125, 38.60606060606061, 40.09090909090909, 41.57575757575758, 43.06060606060606, 44.54545454545455, 46.03030303030303, 47.515151515151516, 49.0, 50.484848484848484, 51.96969696969697, 53.45454545454545, 54.93939393939394, 56.42424242424242, 57.90909090909091, 59.39393939393939, 60.87878787878788, 62.36363636363637, 63.84848484848485, 65.33333333333333, 66.81818181818181, 68.3030303030303, 69.78787878787878, 71.27272727272728, 72.75757575757576, 74.24242424242425, 75.72727272727273, 77.21212121212122, 78.6969696969697, 80.18181818181819, 81.66666666666667, 83.15151515151516, 84.63636363636364, 86.12121212121212, 87.60606060606061, 89.0909090909091, 90.57575757575758, 92.06060606060606, 93.54545454545455, 95.03030303030303, 96.51515151515152, 98.0, 99.48484848484848, 100.96969696969697, 102.45454545454545, 103.93939393939394, 105.42424242424242, 106.9090909090909, 108.39393939393939, 109.87878787878788, 111.36363636363636, 112.84848484848484, 114.33333333333333, 115.81818181818181, 117.3030303030303, 118.78787878787878, 120.27272727272728, 121.75757575757576, 123.24242424242425, 124.72727272727273, 126.21212121212122, 127.6969696969697, 129.1818181818182, 130.66666666666666, 132.15151515151516, 133.63636363636363, 135.12121212121212, 136.6060606060606, 138.0909090909091, 139.57575757575756, 141.06060606060606, 142.54545454545456, 144.03030303030303, 145.51515151515153, 147.0]
#> 3                               [0.0, 0.898989898989899, 1.797979797979798, 2.6969696969696972, 3.595959595959596, 4.494949494949495, 5.3939393939393945, 6.292929292929293, 7.191919191919192, 8.090909090909092, 8.98989898989899, 9.88888888888889, 10.787878787878789, 11.686868686868687, 12.585858585858587, 13.484848484848484, 14.383838383838384, 15.282828282828284, 16.181818181818183, 17.08080808080808, 17.97979797979798, 18.87878787878788, 19.77777777777778, 20.67676767676768, 21.575757575757578, 22.474747474747474, 23.373737373737374, 24.272727272727273, 25.171717171717173, 26.070707070707073, 26.96969696969697, 27.86868686868687, 28.767676767676768, 29.666666666666668, 30.565656565656568, 31.464646464646464, 32.36363636363637, 33.26262626262626, 34.16161616161616, 35.06060606060606, 35.95959595959596, 36.85858585858586, 37.75757575757576, 38.656565656565654, 39.55555555555556, 40.45454545454545, 41.35353535353536, 42.25252525252525, 43.151515151515156, 44.05050505050505, 44.94949494949495, 45.84848484848485, 46.74747474747475, 47.64646464646465, 48.54545454545455, 49.44444444444444, 50.343434343434346, 51.24242424242424, 52.141414141414145, 53.04040404040404, 53.93939393939394, 54.83838383838384, 55.73737373737374, 56.63636363636364, 57.535353535353536, 58.43434343434343, 59.333333333333336, 60.23232323232323, 61.131313131313135, 62.03030303030303, 62.92929292929293, 63.82828282828283, 64.72727272727273, 65.62626262626263, 66.52525252525253, 67.42424242424242, 68.32323232323232, 69.22222222222223, 70.12121212121212, 71.02020202020202, 71.91919191919192, 72.81818181818181, 73.71717171717172, 74.61616161616162, 75.51515151515152, 76.41414141414141, 77.31313131313131, 78.21212121212122, 79.11111111111111, 80.01010101010101, 80.9090909090909, 81.8080808080808, 82.70707070707071, 83.60606060606061, 84.5050505050505, 85.4040404040404, 86.30303030303031, 87.20202020202021, 88.1010101010101, 89.0]
#> 4                      [11.0, 14.121212121212121, 17.242424242424242, 20.363636363636363, 23.484848484848484, 26.606060606060606, 29.727272727272727, 32.848484848484844, 35.96969696969697, 39.09090909090909, 42.21212121212121, 45.33333333333333, 48.45454545454545, 51.57575757575758, 54.696969696969695, 57.81818181818181, 60.93939393939394, 64.06060606060606, 67.18181818181819, 70.3030303030303, 73.42424242424242, 76.54545454545455, 79.66666666666666, 82.78787878787878, 85.9090909090909, 89.03030303030303, 92.15151515151516, 95.27272727272727, 98.39393939393939, 101.51515151515152, 104.63636363636363, 107.75757575757575, 110.87878787878788, 114.0, 117.12121212121212, 120.24242424242424, 123.36363636363636, 126.48484848484848, 129.6060606060606, 132.72727272727272, 135.84848484848484, 138.96969696969697, 142.0909090909091, 145.21212121212122, 148.33333333333331, 151.45454545454544, 154.57575757575756, 157.6969696969697, 160.8181818181818, 163.93939393939394, 167.06060606060606, 170.1818181818182, 173.3030303030303, 176.4242424242424, 179.54545454545453, 182.66666666666666, 185.78787878787878, 188.9090909090909, 192.03030303030303, 195.15151515151516, 198.27272727272725, 201.39393939393938, 204.5151515151515, 207.63636363636363, 210.75757575757575, 213.87878787878788, 217.0, 220.12121212121212, 223.24242424242425, 226.36363636363635, 229.48484848484847, 232.6060606060606, 235.72727272727272, 238.84848484848484, 241.96969696969697, 245.0909090909091, 248.2121212121212, 251.33333333333331, 254.45454545454544, 257.57575757575756, 260.6969696969697, 263.8181818181818, 266.93939393939394, 270.06060606060606, 273.1818181818182, 276.3030303030303, 279.42424242424244, 282.54545454545456, 285.66666666666663, 288.78787878787875, 291.9090909090909, 295.030303030303, 298.1515151515151, 301.27272727272725, 304.3939393939394, 307.5151515151515, 310.6363636363636, 313.75757575757575, 316.8787878787879, 320.0]
#> 5 [19.0, 21.828282828282827, 24.656565656565657, 27.484848484848484, 30.313131313131315, 33.141414141414145, 35.96969696969697, 38.7979797979798, 41.62626262626263, 44.45454545454545, 47.282828282828284, 50.111111111111114, 52.93939393939394, 55.76767676767677, 58.5959595959596, 61.42424242424242, 64.25252525252526, 67.08080808080808, 69.9090909090909, 72.73737373737373, 75.56565656565657, 78.39393939393939, 81.22222222222223, 84.05050505050505, 86.87878787878788, 89.70707070707071, 92.53535353535354, 95.36363636363636, 98.1919191919192, 101.02020202020202, 103.84848484848484, 106.67676767676768, 109.5050505050505, 112.33333333333333, 115.16161616161617, 117.98989898989899, 120.81818181818181, 123.64646464646465, 126.47474747474747, 129.3030303030303, 132.13131313131314, 134.95959595959596, 137.78787878787878, 140.6161616161616, 143.44444444444446, 146.27272727272725, 149.1010101010101, 151.92929292929293, 154.75757575757575, 157.58585858585857, 160.41414141414143, 163.24242424242425, 166.07070707070707, 168.8989898989899, 171.72727272727272, 174.55555555555554, 177.3838383838384, 180.21212121212122, 183.04040404040404, 185.86868686868686, 188.6969696969697, 191.5252525252525, 194.35353535353536, 197.1818181818182, 200.010101010101, 202.83838383838383, 205.66666666666666, 208.4949494949495, 211.32323232323233, 214.15151515151516, 216.97979797979798, 219.8080808080808, 222.63636363636363, 225.46464646464648, 228.2929292929293, 231.12121212121212, 233.94949494949495, 236.77777777777777, 239.6060606060606, 242.43434343434345, 245.26262626262627, 248.0909090909091, 250.91919191919192, 253.74747474747474, 256.57575757575756, 259.4040404040404, 262.2323232323232, 265.06060606060606, 267.8888888888889, 270.7171717171717, 273.5454545454545, 276.37373737373736, 279.2020202020202, 282.030303030303, 284.85858585858585, 287.6868686868687, 290.5151515151515, 293.34343434343435, 296.17171717171715, 299.0]
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        time_kde_y
#> 1   [0.0033527565186855495, 0.0046436153530472766, 0.006318206673409978, 0.008439252833052394, 0.01104791427258442, 0.014145600617329057, 0.01767684076296404, 0.021518328142002614, 0.025479067290836432, 0.029314503135691727, 0.0327538744453553, 0.03553597353936435, 0.03744569514405714, 0.03834347507573331, 0.038181970784750024, 0.03700788683661015, 0.034950107656983836, 0.0321973957903463, 0.028970020768758792, 0.025490355216535955, 0.021957732842083886, 0.01853204799742138, 0.015328168243753357, 0.01241978170657254, 0.009848422283611562, 0.007632680722875755, 0.00577431440523572, 0.00426087344860483, 0.0030667212285270897, 0.0021547944105435746, 0.0014802987747787352, 0.0009958875741630038, 0.0006568457438972833, 0.0004248460007677216, 0.00026963727185502003, 0.00016892277075447192, 0.00010718649592436664, 7.421661034157423e-05, 6.374572755708654e-05, 7.226869193412575e-05, 9.790750267638262e-05, 0.0001392185123673798, 0.000194006118068072, 0.0002583922097917421, 0.0003264706696312196, 0.0003907834685989524, 0.000443602631401776, 0.0004786837874414017, 0.0004929099496612076, 0.0004871928520124234, 0.0004661993237039055, 0.00043687785766261065, 0.0004062349201902565, 0.00037915454459066654, 0.00035709303338727803, 0.0003381545246190337, 0.0003184810211581811, 0.00029433558360268675, 0.0002639953388576034, 0.0002287264678514939, 0.00019257974494162563, 0.00016125318107159238, 0.00014056081989119576, 0.00013503387940649284, 0.00014696484733096543, 0.00017597271694866603, 0.00021904221464129597, 0.00027096750442296086, 0.0003251292167517968, 0.00037449437766755365, 0.00041267192663219023, 0.00043484390518311745, 0.0004384488293208706, 0.000423572945511029, 0.00039303741460750685, 0.0003521396118648136, 0.000307976684247803, 0.0002683304824578026, 0.00024024335068813224, 0.0002285923194095612, 0.00023506189131694685, 0.0002578458829802728, 0.0002921842166829792, 0.0003315515673026236, 0.00036909448295726884, 0.0003988744913287916, 0.0004166408273582428, 0.0004201302028125323, 0.00040909790984907345, 0.00038528921214292105, 0.0003523741693215109, 0.00031565445182528716, 0.00028130036149356995, 0.0002550657547779143, 0.00024075680528746115, 0.00023899084894981693, 0.0002467983182314929, 0.0002583527424829857, 0.00026667140722164747, 0.0002657160990393458]
#> 2                        [0.00811882830572124, 0.010323472370181837, 0.0128410816404724, 0.015627196296924002, 0.018609003831562797, 0.021686645385169307, 0.024738016610366103, 0.027626897568954718, 0.030213616776803502, 0.03236692366420628, 0.03397546042226941, 0.03495727672039461, 0.03526620977481574, 0.03489454877737704, 0.03387204539459664, 0.03226184982260856, 0.030154235425771603, 0.02765901024591955, 0.024897373652434277, 0.021993777724144997, 0.01906819969693594, 0.016229173213383408, 0.013567945145335185, 0.011154158111531618, 0.009033434331034308, 0.007227108780352731, 0.005734129247571504, 0.004534852068450439, 0.0035961849563260566, 0.0028773333903515438, 0.0023353450544483768, 0.0019297348100213872, 0.0016256909917926662, 0.0013956622025286772, 0.001219434453218893, 0.0010830631573102677, 0.0009771715573742754, 0.0008951445637019335, 0.0008316465626916817, 0.0007817136841718053, 0.0007404700821721528, 0.0007033464590192607, 0.0006665734004782886, 0.0006276948030061112, 0.0005858885118003347, 0.0005419684664023126, 0.0004980463485272651, 0.00045692500224436735, 0.00042136218641744235, 0.000393371615710478, 0.0003737169142824838, 0.0003617081902197957, 0.00035534148896332985, 0.00035174394019341293, 0.00034781956069101773, 0.00034094833715991277, 0.0003295843701626981, 0.000313628255659822, 0.00029450534917847, 0.0002749487914970116, 0.000258546187604929, 0.00024914797618138173, 0.00025024848080274013, 0.0002644406572485128, 0.00029302120239666505, 0.00033579295442112456, 0.00039108201343210553, 0.00045595916318728446, 0.0005266283254847908, 0.0005989195693506798, 0.0006688047066603958, 0.0007328465354587383, 0.0007885044272598479, 0.0008342503045667823, 0.0008694937625271854, 0.0008943604183306097, 0.0009093990231168408, 0.0009153003040444969, 0.0009126927718615032, 0.0009020463676387502, 0.0008836782380249568, 0.0008578298144787214, 0.0008247775705467155, 0.0007849487233646585, 0.0007390278738692952, 0.0006880498968776601, 0.0006334720097156067, 0.0005772060173243526, 0.0005215796314474043, 0.00046919489638253827, 0.0004226691344144106, 0.000384277735741094, 0.0003555579947491454, 0.0003369636683327109, 0.0003276671116017343, 0.000325583095553146, 0.000327639360019477, 0.0003302567425363384, 0.0003299448565971061, 0.0003238855646736715]
#> 3                                     [0.0026300109337619153, 0.003247309520743016, 0.004015716652271703, 0.004974709162352495, 0.0061469835792674985, 0.00753116760564568, 0.009105625634995318, 0.010843307073340537, 0.012731392738068892, 0.014785699135378699, 0.01705096411816582, 0.01958432589572465, 0.02242766272599795, 0.025580500629848006, 0.02898535684724975, 0.03253134345647718, 0.03607297898568305, 0.039454527471372486, 0.04252938291354708, 0.04516892469949008, 0.0472624270226712, 0.04871467540555673, 0.049448355228746806, 0.04941433539238368, 0.048607161329612465, 0.04707849401310575, 0.044940171563466455, 0.042351672104341515, 0.039492834416863624, 0.036529181576688205, 0.033581176726706824, 0.030708225441043877, 0.027913112299576298, 0.025164765907526233, 0.02243005470233466, 0.01970190995145544, 0.01701304586047246, 0.014431029208464603, 0.01203842677212732, 0.009907540685624452, 0.008080351177400705, 0.006560677911829079, 0.005319498029273096, 0.004308986150195176, 0.003478480367714318, 0.0027866063161570134, 0.0022068343648365425, 0.0017268310541674536, 0.0013437609447668414, 0.0010580188659252213, 0.0008673528151416024, 0.0007626858077921124, 0.0007264588608656391, 0.0007338724929556268, 0.0007567766345870167, 0.0007691516467574879, 0.0007524518279611263, 0.0006989770076282282, 0.0006121081602823681, 0.0005034626272315376, 0.00038821108286017316, 0.0002803634662566427, 0.00018954911934846152, 0.00011994026024671337, 7.102290023268078e-05, 3.9354579328356896e-05, 2.0405253184047026e-05, 9.89991803157598e-06, 4.494284488767302e-06, 1.909092872147715e-06, 7.588049556716899e-07, 2.822080483619622e-07, 9.82074884197269e-08, 3.197842938688815e-08, 9.743907758888645e-09, 2.781260289782113e-09, 7.579820262450146e-10, 2.608329996417923e-10, 3.5995255345666595e-10, 1.2466546671563586e-09, 4.5236157019349405e-09, 1.5463279410038817e-08, 4.9478705362154526e-08, 1.4814258694628277e-07, 4.150282564255573e-07, 1.0879553294882762e-06, 2.668579167274339e-06, 6.124698947704325e-06, 1.315300632507324e-05, 2.6430229801557425e-05, 4.969497506313021e-05, 8.7429831709229e-05, 0.00014392705380426106, 0.00022169753078146539, 0.0003195324021234329, 0.00043092786737938794, 0.0005437883304350244, 0.0006420824721316552, 0.0007093937748884086, 0.0007333639530229585]
#> 4           [7.418787417081405e-05, 7.061902551054395e-05, 6.090993011923666e-05, 4.760275294787866e-05, 3.3709601995407915e-05, 2.1629813142563488e-05, 1.257562266165523e-05, 6.624968458568708e-06, 3.1623921224619315e-06, 1.3678082262980184e-06, 5.36058627973796e-07, 1.903605526077382e-07, 6.125185008055847e-08, 1.7858256798319072e-08, 4.717771541796936e-09, 1.1293442118481366e-09, 2.4523368112824405e-10, 5.013253086647337e-11, 2.0955137258982843e-11, 7.114488175452849e-11, 3.567546995725216e-10, 1.653400452091124e-09, 6.956568543260226e-09, 2.656017967058463e-08, 9.202719333351617e-08, 2.8939313751517303e-07, 8.260133023771088e-07, 2.140212412743172e-06, 5.034444960131148e-06, 1.0753403333868286e-05, 2.0862175709243143e-05, 3.677972201147471e-05, 5.8981124467252765e-05, 8.620412968566323e-05, 0.00011529734363786266, 0.00014230301963238449, 0.00016477712139810488, 0.00018443675171838758, 0.00020861485675866817, 0.0002492763209877868, 0.0003195393392642306, 0.0004291384058470141, 0.0005811865810149178, 0.0007722990836458921, 0.0009965135456673734, 0.0012510662110615548, 0.0015402700712750096, 0.0018740783868115582, 0.002261066681258419, 0.0027000123037135924, 0.0031765596611070144, 0.003668821194312951, 0.004159122386963716, 0.004643517876837212, 0.005131470619400822, 0.005635499410311005, 0.006158793827561642, 0.006690610014521582, 0.007212950407255426, 0.007713069278298135, 0.008192274705936106, 0.008664983400168953, 0.009149069997021418, 0.00965333092323864, 0.010168054844069976, 0.010662266139309174, 0.011088781622816438, 0.011396107313859134, 0.011543639440075805, 0.011514480760418668, 0.011320649363786527, 0.010998973638945073, 0.010599911319816881, 0.010173191917835613, 0.009753970508540852, 0.009353276954270337, 0.008956907476810697, 0.008534868287939161, 0.008058061817530878, 0.0075137312180612046, 0.006911328429225512, 0.006276714271323695, 0.00564005026415244, 0.005025543603959168, 0.0044479227223798695, 0.003914766472439211, 0.0034303197031578755, 0.002997079902033828, 0.002614620913085355, 0.002277880675455972, 0.001977555111677262, 0.0017033702713503385, 0.0014484802248347505, 0.0012120513069018414, 0.0009982020115266814, 0.0008120397999533284, 0.0006555508599646828, 0.0005260017471310104, 0.00041750953071214544, 0.0003242280678834325]
#> 5 [0.00032793852752742957, 0.0003378422373847327, 0.00033703019692098445, 0.00032564087941012245, 0.0003048489176198312, 0.00027671261797640294, 0.00024390637853183536, 0.00020939531672575695, 0.00017611533751334772, 0.00014671132539221285, 0.00012336361673697904, 0.00010770620391270287, 0.00010081760288646062, 0.00010325318025523176, 0.00011508823155834363, 0.00013595217290927932, 0.00016505053820430434, 0.00020118648232633277, 0.00024280156116936928, 0.0002880538234006536, 0.0003349403981097426, 0.0003814556919473767, 0.00042576075025395197, 0.00046632994436168426, 0.0005020416686427854, 0.000532190821818974, 0.0005564198663617595, 0.000574587037212488, 0.0005866085665002836, 0.000592321023029714, 0.00059140668527165, 0.0005834092243886419, 0.0005678424034430927, 0.0005443674487296982, 0.0005129929638298292, 0.0004742415151520458, 0.00042923286713922293, 0.00037965416658855, 0.0003276164568364758, 0.0002754261834576387, 0.00022532128895746037, 0.00017922845249710483, 0.00013859006524203187, 0.0001042903493099848, 7.668626293883081e-05, 5.572745315085563e-05, 4.1135472961466634e-05, 3.260750214453504e-05, 3.001262150294029e-05, 3.3556306450956335e-05, 4.389817441094345e-05, 6.221723294531373e-05, 9.02274757425373e-05, 0.00013015504295265761, 0.00018469636979150575, 0.0002569835032686414, 0.00035058511715208013, 0.00046956588589671083, 0.0006186099359947364, 0.0008031863985887593, 0.0010297017398738322, 0.0013055544073420306, 0.0016389948226317277, 0.0020387087441758865, 0.0025130889906017986, 0.003069233785174234, 0.0037117928653468534, 0.004441850420739716, 0.005256061327932143, 0.006146226572956407, 0.007099404102358839, 0.008098522097182193, 0.009123330256067971, 0.010151436353953857, 0.011159167631652051, 0.012122083319547367, 0.013015126169707684, 0.013812585910458804, 0.014488187909376997, 0.015015655272769077, 0.015369993369741878, 0.015529530446003602, 0.015478479330405455, 0.015209550786417433, 0.014726030608183815, 0.014042776556595643, 0.013185791799465602, 0.0121903325487597, 0.011097822656901275, 0.009952089924501101, 0.00879554785371754, 0.007665906118647245, 0.006593830305403067, 0.005601743193424534, 0.004703730751376705, 0.003906338689307998, 0.0032099479528672477, 0.0026104016727601263, 0.0021006036914481673, 0.001671892547414174]
#>   age_mean age_median  age_std age_q1 age_q3 n_ages male_proportion
#> 1 66.70909         67 5.691262     62     71    165       0.5939394
#> 2 66.79167         67 5.436464     62     71    168       0.6011905
#> 3 66.68590         67 5.499331     62     71    156       0.5833333
#> 4 78.35256         78 6.034101     73     84    156       0.5192308
#> 5 77.97279         78 6.624928     73     84    147       0.4829932
#>   TARGET_SUBJECT_PREVALENCE CONTROL_SUBJECT_PREVALENCE
#> 1                     0.330                       0.01
#> 2                     0.336                       0.01
#> 3                     0.312                       0.00
#> 4                     0.312                       0.00
#> 5                     0.294                       0.00
#>   PREVALENCE_DIFFERENCE_RATIO
#> 1                        33.0
#> 2                        33.6
#> 3                       100.0
#> 4                       100.0
#> 5                       100.0
```

## `ordinal_summaries.parquet`

Row grain: one ordinalized concept sequence position (for concepts
treated as repeated ordinals).

### Column Dictionary

``` r

ordinalSummaries <- summaryTables[["ordinal_summaries.parquet"]]

ordinalDescriptions <- c(
  CONCEPT_ID = "Derived ordinal concept id, typically `ORIGINAL_CONCEPT_ID` + ordinal suffix.",
  HERITAGE = "OMOP domain/heritage.",
  ORDINAL = "Ordinal occurrence index (1st, 2nd, ...).",
  time_count = "Total number of recorded concept-time observations for this ordinal position.",
  time_min = "Minimum time-to-event for this ordinal concept.",
  time_max = "Maximum time-to-event for this ordinal concept.",
  time_mean = "Mean time-to-event for this ordinal concept.",
  time_median = "Median time-to-event for this ordinal concept.",
  time_std = "Standard deviation of time-to-event.",
  time_q1 = "First quartile of time-to-event.",
  time_q3 = "Third quartile of time-to-event.",
  time_iqr = "Interquartile range for time-to-event.",
  patient_count = "Number of target patients with this ordinal event.",
  age_mean = "Mean age among target patients with this ordinal event.",
  age_median = "Median age among target patients with this ordinal event.",
  age_std = "Age standard deviation among target patients with this ordinal event.",
  age_q1 = "First age quartile for this ordinal event.",
  age_q3 = "Third age quartile for this ordinal event.",
  n_ages = "Number of non-missing ages used for age statistics.",
  male_proportion = "Male proportion among ordinal-event-positive target patients.",
  ordinal_name_suffix = "Human-readable ordinal suffix (for example 1st, 2nd).",
  ORIGINAL_CONCEPT_ID = "Base concept id before ordinal expansion.",
  CONCEPT_NAME = "Ordinalized concept label (for example 'Death 2nd').",
  IS_ORDINAL = "Flag indicating ordinalized concept rows.",
  time_histogram_bins = "JSON-encoded histogram bin edges for ordinal timing.",
  time_histogram_counts = "JSON-encoded histogram counts for ordinal timing.",
  time_kde_x = "JSON-encoded x-grid for ordinal timing KDE.",
  time_kde_y = "JSON-encoded y-values for ordinal timing KDE.",
  TARGET_SUBJECT_PREVALENCE = "Target prevalence for the ordinalized concept row.",
  CONTROL_SUBJECT_PREVALENCE = "Control prevalence for the ordinalized concept row.",
  PREVALENCE_DIFFERENCE_RATIO = "Target/control prevalence ratio for the ordinalized concept row."
)

ordinalExamples <- c(
  CONCEPT_ID = paste("Rows:", nrow(ordinalSummaries)),
  HERITAGE = paste("Values:", paste(sort(unique(ordinalSummaries$HERITAGE)), collapse = ", ")),
  ORDINAL = paste("Range:", paste(range(ordinalSummaries$ORDINAL, na.rm = TRUE), collapse = " to ")),
  time_count = paste("Range:", paste(range(ordinalSummaries$time_count, na.rm = TRUE), collapse = " to ")),
  time_min = paste("Range:", paste(range(ordinalSummaries$time_min, na.rm = TRUE), collapse = " to ")),
  time_max = paste("Range:", paste(range(ordinalSummaries$time_max, na.rm = TRUE), collapse = " to ")),
  time_mean = paste("Range:", paste(round(range(ordinalSummaries$time_mean, na.rm = TRUE), 2), collapse = " to ")),
  time_median = paste("Range:", paste(range(ordinalSummaries$time_median, na.rm = TRUE), collapse = " to ")),
  time_std = paste("Range:", paste(round(range(ordinalSummaries$time_std, na.rm = TRUE), 2), collapse = " to ")),
  time_q1 = paste("Range:", paste(range(ordinalSummaries$time_q1, na.rm = TRUE), collapse = " to ")),
  time_q3 = paste("Range:", paste(range(ordinalSummaries$time_q3, na.rm = TRUE), collapse = " to ")),
  time_iqr = paste("Range:", paste(range(ordinalSummaries$time_iqr, na.rm = TRUE), collapse = " to ")),
  patient_count = paste("Range:", paste(range(ordinalSummaries$patient_count, na.rm = TRUE), collapse = " to ")),
  age_mean = paste("Range:", paste(round(range(ordinalSummaries$age_mean, na.rm = TRUE), 2), collapse = " to ")),
  age_median = paste("Range:", paste(range(ordinalSummaries$age_median, na.rm = TRUE), collapse = " to ")),
  age_std = paste("Range:", paste(round(range(ordinalSummaries$age_std, na.rm = TRUE), 2), collapse = " to ")),
  age_q1 = paste("Range:", paste(range(ordinalSummaries$age_q1, na.rm = TRUE), collapse = " to ")),
  age_q3 = paste("Range:", paste(range(ordinalSummaries$age_q3, na.rm = TRUE), collapse = " to ")),
  n_ages = paste("Range:", paste(range(ordinalSummaries$n_ages, na.rm = TRUE), collapse = " to ")),
  male_proportion = paste("Range:", paste(round(range(ordinalSummaries$male_proportion, na.rm = TRUE), 3), collapse = " to ")),
  ordinal_name_suffix = paste("Values:", paste(unique(ordinalSummaries$ordinal_name_suffix), collapse = ", ")),
  ORIGINAL_CONCEPT_ID = paste("Unique original concepts:", dplyr::n_distinct(ordinalSummaries$ORIGINAL_CONCEPT_ID)),
  CONCEPT_NAME = paste("Example:", ordinalSummaries$CONCEPT_NAME[1]),
  IS_ORDINAL = paste("Values:", paste(unique(ordinalSummaries$IS_ORDINAL), collapse = ", ")),
  time_histogram_bins = paste("JSON string; starts with:", substr(ordinalSummaries$time_histogram_bins[1], 1, 30)),
  time_histogram_counts = paste("JSON string; starts with:", substr(ordinalSummaries$time_histogram_counts[1], 1, 30)),
  time_kde_x = paste("JSON string; starts with:", substr(ordinalSummaries$time_kde_x[1], 1, 30)),
  time_kde_y = paste("JSON string; starts with:", substr(ordinalSummaries$time_kde_y[1], 1, 30)),
  TARGET_SUBJECT_PREVALENCE = paste("Range:", paste(range(ordinalSummaries$TARGET_SUBJECT_PREVALENCE, na.rm = TRUE), collapse = " to ")),
  CONTROL_SUBJECT_PREVALENCE = paste("Range:", paste(range(ordinalSummaries$CONTROL_SUBJECT_PREVALENCE, na.rm = TRUE), collapse = " to ")),
  PREVALENCE_DIFFERENCE_RATIO = paste("Range:", paste(round(range(ordinalSummaries$PREVALENCE_DIFFERENCE_RATIO, na.rm = TRUE), 3), collapse = " to "))
)

buildColumnDictionary(ordinalSummaries, ordinalDescriptions, ordinalExamples)
#>                                                  column      type
#> CONCEPT_ID                                   CONCEPT_ID character
#> HERITAGE                                       HERITAGE character
#> ORDINAL                                         ORDINAL   numeric
#> time_count                                   time_count   numeric
#> time_min                                       time_min   numeric
#> time_max                                       time_max   numeric
#> time_mean                                     time_mean   numeric
#> time_median                                 time_median   numeric
#> time_std                                       time_std   numeric
#> time_q1                                         time_q1   numeric
#> time_q3                                         time_q3   numeric
#> time_iqr                                       time_iqr   numeric
#> patient_count                             patient_count   numeric
#> age_mean                                       age_mean   numeric
#> age_median                                   age_median   numeric
#> age_std                                         age_std   numeric
#> age_q1                                           age_q1   numeric
#> age_q3                                           age_q3   numeric
#> n_ages                                           n_ages   numeric
#> male_proportion                         male_proportion   numeric
#> ordinal_name_suffix                 ordinal_name_suffix character
#> ORIGINAL_CONCEPT_ID                 ORIGINAL_CONCEPT_ID character
#> CONCEPT_NAME                               CONCEPT_NAME character
#> IS_ORDINAL                                   IS_ORDINAL   logical
#> time_histogram_bins                 time_histogram_bins character
#> time_histogram_counts             time_histogram_counts character
#> time_kde_x                                   time_kde_x character
#> time_kde_y                                   time_kde_y character
#> TARGET_SUBJECT_PREVALENCE     TARGET_SUBJECT_PREVALENCE   numeric
#> CONTROL_SUBJECT_PREVALENCE   CONTROL_SUBJECT_PREVALENCE   numeric
#> PREVALENCE_DIFFERENCE_RATIO PREVALENCE_DIFFERENCE_RATIO   numeric
#>                                                                                               description
#> CONCEPT_ID                  Derived ordinal concept id, typically `ORIGINAL_CONCEPT_ID` + ordinal suffix.
#> HERITAGE                                                                            OMOP domain/heritage.
#> ORDINAL                                                         Ordinal occurrence index (1st, 2nd, ...).
#> time_count                  Total number of recorded concept-time observations for this ordinal position.
#> time_min                                                  Minimum time-to-event for this ordinal concept.
#> time_max                                                  Maximum time-to-event for this ordinal concept.
#> time_mean                                                    Mean time-to-event for this ordinal concept.
#> time_median                                                Median time-to-event for this ordinal concept.
#> time_std                                                             Standard deviation of time-to-event.
#> time_q1                                                                  First quartile of time-to-event.
#> time_q3                                                                  Third quartile of time-to-event.
#> time_iqr                                                           Interquartile range for time-to-event.
#> patient_count                                          Number of target patients with this ordinal event.
#> age_mean                                          Mean age among target patients with this ordinal event.
#> age_median                                      Median age among target patients with this ordinal event.
#> age_std                             Age standard deviation among target patients with this ordinal event.
#> age_q1                                                         First age quartile for this ordinal event.
#> age_q3                                                         Third age quartile for this ordinal event.
#> n_ages                                                Number of non-missing ages used for age statistics.
#> male_proportion                             Male proportion among ordinal-event-positive target patients.
#> ordinal_name_suffix                                 Human-readable ordinal suffix (for example 1st, 2nd).
#> ORIGINAL_CONCEPT_ID                                             Base concept id before ordinal expansion.
#> CONCEPT_NAME                                         Ordinalized concept label (for example 'Death 2nd').
#> IS_ORDINAL                                                      Flag indicating ordinalized concept rows.
#> time_histogram_bins                                  JSON-encoded histogram bin edges for ordinal timing.
#> time_histogram_counts                                   JSON-encoded histogram counts for ordinal timing.
#> time_kde_x                                                    JSON-encoded x-grid for ordinal timing KDE.
#> time_kde_y                                                  JSON-encoded y-values for ordinal timing KDE.
#> TARGET_SUBJECT_PREVALENCE                              Target prevalence for the ordinalized concept row.
#> CONTROL_SUBJECT_PREVALENCE                            Control prevalence for the ordinalized concept row.
#> PREVALENCE_DIFFERENCE_RATIO              Target/control prevalence ratio for the ordinalized concept row.
#>                                                                             lc500s_example
#> CONCEPT_ID                                                                        Rows: 17
#> HERITAGE                    Values: death, observation, procedure_occurrence, visit_detail
#> ORDINAL                                                                      Range: 1 to 4
#> time_count                                                                Range: 79 to 455
#> time_min                                                                   Range: 0 to 186
#> time_max                                                                  Range: 61 to 320
#> time_mean                                                           Range: 16.38 to 249.66
#> time_median                                                               Range: 16 to 251
#> time_std                                                               Range: 8.6 to 34.87
#> time_q1                                                                    Range: 5 to 235
#> time_q3                                                                   Range: 24 to 264
#> time_iqr                                                                Range: 11 to 38.75
#> patient_count                                                             Range: 79 to 455
#> age_mean                                                              Range: 56.27 to 78.4
#> age_median                                                                 Range: 55 to 78
#> age_std                                                               Range: 5.36 to 10.58
#> age_q1                                                                     Range: 52 to 73
#> age_q3                                                                     Range: 62 to 84
#> n_ages                                                                    Range: 79 to 455
#> male_proportion                                                      Range: 0.468 to 0.617
#> ordinal_name_suffix                                             Values: 1st, 2nd, 3rd, 4th
#> ORIGINAL_CONCEPT_ID                                            Unique original concepts: 7
#> CONCEPT_NAME                                                            Example: Death 1st
#> IS_ORDINAL                                                                    Values: TRUE
#> time_histogram_bins               JSON string; starts with: [11.0, 16.22, 21.4399999999999
#> time_histogram_counts             JSON string; starts with: [5, 0, 0, 0, 0, 0, 0, 0, 0, 0,
#> time_kde_x                        JSON string; starts with: [11.0, 13.636363636363637, 16.
#> time_kde_y                        JSON string; starts with: [0.00023302549387978672, 0.000
#> TARGET_SUBJECT_PREVALENCE                                             Range: 0.158 to 0.91
#> CONTROL_SUBJECT_PREVALENCE                                               Range: 0 to 0.462
#> PREVALENCE_DIFFERENCE_RATIO                                             Range: 1.97 to 100
```

### Example Rows

``` r

utils::head(ordinalSummaries, 5)
#>   CONCEPT_ID             HERITAGE ORDINAL time_count time_min time_max
#> 1    32280_1                death       1        156       11      272
#> 2    32280_2                death       2        155      156      320
#> 3    32280_3                death       3        110      180      299
#> 4    32280_4                death       4         79      186      299
#> 5  4008211_1 procedure_occurrence       1        171        0       82
#>   time_mean time_median time_std time_q1 time_q3 time_iqr patient_count
#> 1 193.45513       192.5 30.13054     175   212.0     37.0           156
#> 2 224.61290       222.0 28.46355     207   239.5     32.5           155
#> 3 236.76364       234.5 21.69426     224   251.0     27.0           110
#> 4 249.65823       251.0 22.72629     235   264.0     29.0            79
#> 5  20.69006        20.0 12.15405      13    26.0     13.0           171
#>   age_mean age_median  age_std age_q1 age_q3 n_ages male_proportion
#> 1 78.35256       78.0 6.034101     73  84.00    156       0.5192308
#> 2 78.40000       78.0 6.024409     73  84.00    155       0.5161290
#> 3 78.20909       77.5 5.884353     73  83.75    110       0.4727273
#> 4 78.31646       78.0 5.891202     73  83.00     79       0.4683544
#> 5 56.94152       56.0 5.798283     52  62.00    171       0.5789474
#>   ordinal_name_suffix ORIGINAL_CONCEPT_ID     CONCEPT_NAME IS_ORDINAL
#> 1                 1st               32280        Death 1st       TRUE
#> 2                 2nd               32280        Death 2nd       TRUE
#> 3                 3rd               32280        Death 3rd       TRUE
#> 4                 4th               32280        Death 4th       TRUE
#> 5                 1st             4008211 Radiotherapy 1st       TRUE
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               time_histogram_bins
#> 1 [11.0, 16.22, 21.439999999999998, 26.66, 31.88, 37.099999999999994, 42.32, 47.54, 52.76, 57.98, 63.199999999999996, 68.41999999999999, 73.64, 78.86, 84.08, 89.3, 94.52, 99.74, 104.96, 110.17999999999999, 115.39999999999999, 120.61999999999999, 125.83999999999999, 131.06, 136.28, 141.5, 146.72, 151.94, 157.16, 162.38, 167.6, 172.82, 178.04, 183.26, 188.48, 193.7, 198.92, 204.14, 209.35999999999999, 214.57999999999998, 219.79999999999998, 225.01999999999998, 230.23999999999998, 235.45999999999998, 240.67999999999998, 245.89999999999998, 251.11999999999998, 256.34, 261.56, 266.78, 272.0]
#> 2                                                                                                                          [156.0, 159.28, 162.56, 165.84, 169.12, 172.4, 175.68, 178.96, 182.24, 185.52, 188.8, 192.07999999999998, 195.36, 198.64, 201.92, 205.2, 208.48, 211.76, 215.04, 218.32, 221.6, 224.88, 228.16, 231.44, 234.72, 238.0, 241.28, 244.56, 247.83999999999997, 251.12, 254.39999999999998, 257.68, 260.96, 264.24, 267.52, 270.8, 274.08, 277.36, 280.64, 283.91999999999996, 287.2, 290.48, 293.76, 297.03999999999996, 300.32, 303.6, 306.88, 310.15999999999997, 313.44, 316.72, 320.0]
#> 3                                                                                                                                      [180.0, 182.38, 184.76, 187.14, 189.52, 191.9, 194.28, 196.66, 199.04, 201.42, 203.8, 206.18, 208.56, 210.94, 213.32, 215.7, 218.07999999999998, 220.46, 222.84, 225.22, 227.6, 229.98, 232.36, 234.74, 237.12, 239.5, 241.88, 244.26, 246.64, 249.01999999999998, 251.39999999999998, 253.78, 256.15999999999997, 258.53999999999996, 260.92, 263.3, 265.68, 268.06, 270.44, 272.82, 275.2, 277.58, 279.96, 282.34, 284.72, 287.1, 289.48, 291.86, 294.24, 296.62, 299.0]
#> 4                                                                                                                           [186.0, 188.26, 190.52, 192.78, 195.04, 197.3, 199.56, 201.82, 204.07999999999998, 206.34, 208.6, 210.86, 213.12, 215.38, 217.64, 219.9, 222.16, 224.42, 226.68, 228.94, 231.2, 233.45999999999998, 235.72, 237.98, 240.24, 242.5, 244.76, 247.01999999999998, 249.28, 251.54, 253.8, 256.06, 258.32, 260.58, 262.84, 265.1, 267.36, 269.62, 271.88, 274.14, 276.4, 278.65999999999997, 280.91999999999996, 283.18, 285.44, 287.7, 289.96, 292.21999999999997, 294.48, 296.74, 299.0]
#> 5                                                                                                             [0.0, 1.64, 3.28, 4.92, 6.56, 8.2, 9.84, 11.479999999999999, 13.12, 14.76, 16.4, 18.04, 19.68, 21.32, 22.959999999999997, 24.599999999999998, 26.24, 27.88, 29.52, 31.159999999999997, 32.8, 34.44, 36.08, 37.72, 39.36, 41.0, 42.64, 44.279999999999994, 45.919999999999995, 47.559999999999995, 49.199999999999996, 50.839999999999996, 52.48, 54.12, 55.76, 57.4, 59.04, 60.68, 62.31999999999999, 63.959999999999994, 65.6, 67.24, 68.88, 70.52, 72.16, 73.8, 75.44, 77.08, 78.72, 80.36, 82.0]
#>                                                                                                                                           time_histogram_counts
#> 1   [5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 5, 0, 0, 5, 5, 5, 5, 5, 11, 18, 6, 9, 21, 7, 13, 9, 13, 9, 7, 5, 5, 0, 5, 5, 0, 5, 5, 5]
#> 2    [5, 0, 0, 0, 5, 5, 5, 5, 5, 5, 5, 5, 6, 5, 7, 7, 7, 7, 11, 10, 12, 11, 5, 5, 5, 5, 5, 5, 7, 5, 6, 0, 5, 5, 0, 5, 0, 5, 0, 0, 5, 0, 5, 0, 0, 5, 0, 5, 5, 5]
#> 3      [5, 0, 0, 0, 5, 5, 0, 5, 0, 5, 6, 5, 0, 5, 0, 5, 5, 5, 11, 5, 5, 6, 7, 10, 5, 5, 5, 5, 9, 5, 5, 6, 5, 5, 5, 0, 6, 5, 5, 5, 0, 5, 5, 0, 5, 0, 0, 0, 0, 5]
#> 4        [5, 0, 0, 0, 0, 5, 0, 0, 0, 5, 5, 5, 5, 0, 0, 5, 5, 5, 0, 5, 5, 5, 5, 6, 5, 5, 5, 5, 5, 5, 7, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 0, 5, 0, 0, 5, 0, 5, 0, 5]
#> 5 [6, 6, 5, 7, 5, 5, 8, 10, 5, 13, 14, 7, 14, 8, 12, 13, 5, 7, 10, 5, 5, 5, 5, 5, 0, 5, 5, 0, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5]
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         time_kde_x
#> 1                                                                                                                   [11.0, 13.636363636363637, 16.272727272727273, 18.909090909090907, 21.545454545454547, 24.18181818181818, 26.818181818181817, 29.454545454545453, 32.09090909090909, 34.72727272727273, 37.36363636363636, 40.0, 42.63636363636363, 45.27272727272727, 47.90909090909091, 50.54545454545455, 53.18181818181818, 55.81818181818181, 58.45454545454545, 61.090909090909086, 63.72727272727273, 66.36363636363636, 69.0, 71.63636363636363, 74.27272727272727, 76.9090909090909, 79.54545454545455, 82.18181818181817, 84.81818181818181, 87.45454545454545, 90.0909090909091, 92.72727272727272, 95.36363636363636, 98.0, 100.63636363636363, 103.27272727272727, 105.9090909090909, 108.54545454545455, 111.18181818181817, 113.81818181818181, 116.45454545454545, 119.09090909090908, 121.72727272727272, 124.36363636363636, 127.0, 129.63636363636363, 132.27272727272725, 134.9090909090909, 137.54545454545453, 140.1818181818182, 142.8181818181818, 145.45454545454544, 148.0909090909091, 150.72727272727272, 153.36363636363635, 156.0, 158.63636363636363, 161.27272727272725, 163.9090909090909, 166.54545454545453, 169.1818181818182, 171.8181818181818, 174.45454545454544, 177.0909090909091, 179.72727272727272, 182.36363636363635, 185.0, 187.63636363636363, 190.27272727272725, 192.9090909090909, 195.54545454545453, 198.1818181818182, 200.8181818181818, 203.45454545454544, 206.0909090909091, 208.72727272727272, 211.36363636363635, 214.0, 216.63636363636363, 219.27272727272725, 221.9090909090909, 224.54545454545453, 227.18181818181816, 229.8181818181818, 232.45454545454544, 235.0909090909091, 237.72727272727272, 240.36363636363635, 243.0, 245.63636363636363, 248.27272727272725, 250.9090909090909, 253.54545454545453, 256.18181818181813, 258.8181818181818, 261.45454545454544, 264.09090909090907, 266.72727272727275, 269.3636363636364, 272.0]
#> 2 [156.0, 157.65656565656565, 159.31313131313132, 160.96969696969697, 162.62626262626262, 164.2828282828283, 165.93939393939394, 167.59595959595958, 169.25252525252526, 170.9090909090909, 172.56565656565658, 174.22222222222223, 175.87878787878788, 177.53535353535352, 179.1919191919192, 180.84848484848484, 182.50505050505052, 184.16161616161617, 185.8181818181818, 187.47474747474746, 189.13131313131314, 190.78787878787878, 192.44444444444446, 194.1010101010101, 195.75757575757575, 197.4141414141414, 199.07070707070707, 200.72727272727272, 202.3838383838384, 204.04040404040404, 205.6969696969697, 207.35353535353536, 209.010101010101, 210.66666666666666, 212.32323232323233, 213.97979797979798, 215.63636363636363, 217.2929292929293, 218.94949494949495, 220.60606060606062, 222.26262626262627, 223.91919191919192, 225.57575757575756, 227.23232323232324, 228.88888888888889, 230.54545454545456, 232.2020202020202, 233.85858585858585, 235.5151515151515, 237.17171717171718, 238.82828282828282, 240.4848484848485, 242.14141414141415, 243.7979797979798, 245.45454545454544, 247.11111111111111, 248.76767676767676, 250.42424242424244, 252.08080808080808, 253.73737373737373, 255.39393939393938, 257.0505050505051, 258.7070707070707, 260.3636363636364, 262.020202020202, 263.67676767676767, 265.3333333333333, 266.98989898989896, 268.64646464646466, 270.3030303030303, 271.95959595959596, 273.6161616161616, 275.27272727272725, 276.92929292929296, 278.5858585858586, 280.24242424242425, 281.8989898989899, 283.55555555555554, 285.21212121212125, 286.86868686868684, 288.52525252525254, 290.1818181818182, 291.83838383838383, 293.49494949494954, 295.1515151515151, 296.80808080808083, 298.4646464646465, 300.1212121212121, 301.77777777777777, 303.4343434343434, 305.0909090909091, 306.7474747474747, 308.4040404040404, 310.06060606060606, 311.7171717171717, 313.3737373737374, 315.030303030303, 316.6868686868687, 318.34343434343435, 320.0]
#> 3 [180.0, 181.2020202020202, 182.40404040404042, 183.6060606060606, 184.8080808080808, 186.010101010101, 187.21212121212122, 188.41414141414143, 189.6161616161616, 190.8181818181818, 192.02020202020202, 193.22222222222223, 194.42424242424244, 195.62626262626262, 196.82828282828282, 198.03030303030303, 199.23232323232324, 200.43434343434342, 201.63636363636363, 202.83838383838383, 204.04040404040404, 205.24242424242425, 206.44444444444446, 207.64646464646464, 208.84848484848484, 210.05050505050505, 211.25252525252526, 212.45454545454544, 213.65656565656565, 214.85858585858585, 216.06060606060606, 217.26262626262627, 218.46464646464648, 219.66666666666666, 220.86868686868686, 222.07070707070707, 223.27272727272728, 224.47474747474746, 225.67676767676767, 226.87878787878788, 228.08080808080808, 229.2828282828283, 230.4848484848485, 231.68686868686868, 232.88888888888889, 234.0909090909091, 235.2929292929293, 236.49494949494948, 237.6969696969697, 238.8989898989899, 240.1010101010101, 241.3030303030303, 242.50505050505052, 243.7070707070707, 244.9090909090909, 246.11111111111111, 247.3131313131313, 248.5151515151515, 249.7171717171717, 250.91919191919192, 252.12121212121212, 253.32323232323233, 254.52525252525254, 255.72727272727272, 256.92929292929296, 258.13131313131316, 259.3333333333333, 260.5353535353535, 261.73737373737373, 262.93939393939394, 264.14141414141415, 265.34343434343435, 266.54545454545456, 267.7474747474747, 268.9494949494949, 270.1515151515151, 271.35353535353534, 272.55555555555554, 273.75757575757575, 274.95959595959596, 276.16161616161617, 277.3636363636364, 278.5656565656566, 279.7676767676768, 280.969696969697, 282.17171717171715, 283.37373737373736, 284.57575757575756, 285.77777777777777, 286.979797979798, 288.1818181818182, 289.3838383838384, 290.5858585858586, 291.78787878787875, 292.98989898989896, 294.19191919191917, 295.3939393939394, 296.5959595959596, 297.7979797979798, 299.0]
#> 4 [186.0, 187.14141414141415, 188.2828282828283, 189.42424242424244, 190.56565656565655, 191.7070707070707, 192.84848484848484, 193.989898989899, 195.13131313131314, 196.27272727272728, 197.41414141414143, 198.55555555555554, 199.6969696969697, 200.83838383838383, 201.97979797979798, 203.12121212121212, 204.26262626262627, 205.40404040404042, 206.54545454545456, 207.68686868686868, 208.82828282828282, 209.96969696969697, 211.11111111111111, 212.25252525252526, 213.3939393939394, 214.53535353535352, 215.67676767676767, 216.8181818181818, 217.95959595959596, 219.1010101010101, 220.24242424242425, 221.3838383838384, 222.52525252525254, 223.66666666666666, 224.8080808080808, 225.94949494949495, 227.0909090909091, 228.23232323232324, 229.37373737373738, 230.5151515151515, 231.65656565656565, 232.7979797979798, 233.93939393939394, 235.08080808080808, 236.22222222222223, 237.36363636363637, 238.50505050505052, 239.64646464646466, 240.78787878787878, 241.92929292929293, 243.07070707070707, 244.21212121212122, 245.35353535353536, 246.49494949494948, 247.63636363636363, 248.77777777777777, 249.91919191919192, 251.06060606060606, 252.2020202020202, 253.34343434343435, 254.4848484848485, 255.62626262626264, 256.7676767676768, 257.9090909090909, 259.0505050505051, 260.19191919191917, 261.3333333333333, 262.47474747474746, 263.6161616161616, 264.75757575757575, 265.8989898989899, 267.04040404040404, 268.1818181818182, 269.32323232323233, 270.4646464646465, 271.6060606060606, 272.74747474747477, 273.8888888888889, 275.030303030303, 276.1717171717172, 277.3131313131313, 278.45454545454544, 279.5959595959596, 280.73737373737373, 281.8787878787879, 283.020202020202, 284.16161616161617, 285.3030303030303, 286.44444444444446, 287.5858585858586, 288.72727272727275, 289.86868686868684, 291.01010101010104, 292.1515151515151, 293.29292929292933, 294.4343434343434, 295.57575757575756, 296.7171717171717, 297.85858585858585, 299.0]
#> 5                                     [0.0, 0.8282828282828283, 1.6565656565656566, 2.484848484848485, 3.313131313131313, 4.141414141414142, 4.96969696969697, 5.797979797979798, 6.626262626262626, 7.454545454545455, 8.282828282828284, 9.11111111111111, 9.93939393939394, 10.767676767676768, 11.595959595959595, 12.424242424242424, 13.252525252525253, 14.080808080808081, 14.90909090909091, 15.737373737373737, 16.565656565656568, 17.393939393939394, 18.22222222222222, 19.050505050505052, 19.87878787878788, 20.707070707070706, 21.535353535353536, 22.363636363636363, 23.19191919191919, 24.02020202020202, 24.848484848484848, 25.67676767676768, 26.505050505050505, 27.333333333333332, 28.161616161616163, 28.98989898989899, 29.81818181818182, 30.646464646464647, 31.474747474747474, 32.303030303030305, 33.131313131313135, 33.95959595959596, 34.78787878787879, 35.61616161616162, 36.44444444444444, 37.27272727272727, 38.101010101010104, 38.92929292929293, 39.75757575757576, 40.58585858585859, 41.41414141414141, 42.24242424242424, 43.07070707070707, 43.898989898989896, 44.72727272727273, 45.55555555555556, 46.38383838383838, 47.21212121212121, 48.04040404040404, 48.86868686868687, 49.696969696969695, 50.525252525252526, 51.35353535353536, 52.18181818181818, 53.01010101010101, 53.83838383838384, 54.666666666666664, 55.494949494949495, 56.323232323232325, 57.15151515151515, 57.97979797979798, 58.80808080808081, 59.63636363636364, 60.464646464646464, 61.292929292929294, 62.121212121212125, 62.94949494949495, 63.77777777777778, 64.60606060606061, 65.43434343434343, 66.26262626262627, 67.0909090909091, 67.91919191919192, 68.74747474747475, 69.57575757575758, 70.4040404040404, 71.23232323232324, 72.06060606060606, 72.88888888888889, 73.71717171717172, 74.54545454545455, 75.37373737373737, 76.20202020202021, 77.03030303030303, 77.85858585858585, 78.68686868686869, 79.51515151515152, 80.34343434343434, 81.17171717171718, 82.0]
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     time_kde_y
#> 1 [0.00023302549387978672, 0.00022639769450415097, 0.00020762406442582107, 0.00017972997037586958, 0.00014685895806767815, 0.00011327068014520122, 8.246538665725038e-05, 5.667127931075837e-05, 3.676135064082027e-05, 2.2509042839747e-05, 1.3009473530846879e-05, 7.097404240033714e-06, 3.654908316838869e-06, 1.7766036872099498e-06, 8.151579433029913e-07, 3.530451027361106e-07, 1.4433008236409531e-07, 5.569724706348526e-08, 2.029556165466489e-08, 7.009717190129657e-09, 2.395415384402803e-09, 1.1674104267528035e-09, 1.8118062558050071e-09, 5.229253760479723e-09, 1.571127077508466e-08, 4.498512649440527e-08, 1.2176690423457188e-07, 3.1139177215499216e-07, 7.523016960725168e-07, 1.717112205763002e-06, 3.702943190211893e-06, 7.545066237992897e-06, 1.4527213350425187e-05, 2.6433635818198817e-05, 4.5463908561614576e-05, 7.393311497890246e-05, 0.0001137313821770333, 0.00016562716255303002, 0.00022864321950276145, 0.0002998460978836485, 0.00037488015389756414, 0.0004493842537788695, 0.0005210740771707935, 0.0005918832518070349, 0.0006693286438092551, 0.0007663697632806688, 0.0008995098030114647, 0.0010855924134431814, 0.0013383909811206345, 0.001666340871190362, 0.0020724243964825207, 0.0025563213650350767, 0.0031178059801476527, 0.0037595240592908446, 0.0044872343464370645, 0.005306570893556537, 0.006217107487949001, 0.0072062100533910175, 0.008245887662150977, 0.009294969411403911, 0.010306618256141544, 0.011238503455704102, 0.012061361939298038, 0.01276222702606016, 0.013341184958753924, 0.01380380314261408, 0.014153509380492263, 0.014387938124941796, 0.014500752467689367, 0.014487196127721853, 0.014349546229037648, 0.014098885211645878, 0.013751922818927271, 0.013324530169984763, 0.012825508987975935, 0.012253969848286517, 0.011601784751974991, 0.010860065890902491, 0.010026819684026467, 0.009112601097089513, 0.008142074342111862, 0.007151156695372723, 0.0061810105332583565, 0.00527097502125099, 0.004452484929243948, 0.0037453632222835846, 0.0031569696611347398, 0.0026838462280585514, 0.002314910294619458, 0.0020350097672221074, 0.0018278040172690737, 0.0016774063108793084, 0.0015688336171354848, 0.0014878024776836512, 0.0014205781527102477, 0.0013543844107299532, 0.0012784532668670295, 0.0011853736106472953, 0.0010721828914850937, 0.0009407061972187639]
#> 2                                                   [0.0006912647167904268, 0.0007552814126724429, 0.0008234492325198714, 0.0008991438621060977, 0.0009864832796232178, 0.0010900816389340662, 0.0012147703371150236, 0.0013653229083108915, 0.0015462114117150744, 0.0017614078761816994, 0.0020142297548291013, 0.002307217809369225, 0.0026420315217745086, 0.003019351790677928, 0.003438791527115819, 0.003898828040648694, 0.004396782136931127, 0.0049288733899871375, 0.005490376446282875, 0.006075889105559011, 0.0066797013948457565, 0.007296230016997039, 0.007920459677977547, 0.008548317207183588, 0.009176900404843716, 0.009804493659168691, 0.010430326744489278, 0.011054069640066827, 0.011675100391590502, 0.012291629027975728, 0.01289980132274402, 0.013492934298652177, 0.014061043748490477, 0.014590807112782576, 0.015066060172603606, 0.015468855011561011, 0.01578101705376134, 0.015986043979710908, 0.016071106620325206, 0.016028860381098516, 0.0158587709237651, 0.0155677073756836, 0.01516965662018282, 0.014684547630254912, 0.014136320311710738, 0.013550499943796309, 0.012951620102200846, 0.012360857488397605, 0.011794198479676995, 0.011261360494335041, 0.010765563105686955, 0.010304110673230258, 0.009869634716737258, 0.009451767798263752, 0.009038988803171418, 0.008620389962134574, 0.008187159285654935, 0.007733635420776611, 0.007257862573937729, 0.006761640915979807, 0.0062501260307646, 0.00573107566808742, 0.005213871382305475, 0.00470845560663345, 0.004224320993534112, 0.0037696690869496318, 0.003350821872783217, 0.0029719268943004824, 0.002634950798195566, 0.002339914792493648, 0.0020852954375745725, 0.0018685002051433877, 0.001686330751866591, 0.0015353656295762047, 0.0014122229179745892, 0.0013136950761116098, 0.0012367762527785442, 0.0011786210354109267, 0.0011364804040733003, 0.0011076556396525483, 0.0010894967869262394, 0.0010794533330453322, 0.0010751659458348992, 0.0010745738467273958, 0.0010760057439795324, 0.0010782244274061094, 0.0010804053275919247, 0.0010820450183307297, 0.0010828130620538353, 0.0010823756596211754, 0.0010802286681885296, 0.0010755783888805955, 0.0010673006217659434, 0.0010539933351311236, 0.0010341190771289263, 0.0010062141431212196, 0.0009691267282492508, 0.0009222391432742139, 0.0008656312481787391, 0.0008001530730485097]
#> 3                                                                       [0.0008088151867655098, 0.000905581303981699, 0.0010128668968380835, 0.0011330962969916458, 0.001268963174469951, 0.0014232752975575696, 0.00159876169442585, 0.0017978506142518592, 0.002022430212714502, 0.002273608882691747, 0.002551498518658308, 0.0028550507017373397, 0.0031819808316468317, 0.003528816028729022, 0.0038910966855400506, 0.004263747294031145, 0.004641609743372471, 0.005020103953898263, 0.005395950838608773, 0.00576786694491456, 0.006137124802238867, 0.006507872990829394, 0.006887127903859365, 0.00728438451569007, 0.007710842141817388, 0.008178296157796748, 0.008697799104159196, 0.009278235510136281, 0.009924976625849322, 0.010638779619089772, 0.011415070258841826, 0.012243702506521155, 0.013109230204132577, 0.013991664818148956, 0.014867638877924947, 0.01571185549446268, 0.016498684889298846, 0.017203769614434594, 0.01780551734570623, 0.018286387064338674, 0.018633903392350507, 0.018841358318261545, 0.018908176064034315, 0.0188399258078675, 0.018647972295888146, 0.01834876191336802, 0.017962757115427344, 0.01751305825279801, 0.017023787671761992, 0.016518350995081657, 0.016017725844195273, 0.015538948977751035, 0.015093970578005147, 0.014689015032934327, 0.014324532519666583, 0.013995752262281858, 0.013693768248124199, 0.013407015220863343, 0.013122939847716994, 0.012829648467694429, 0.012517322763203411, 0.012179236000395315, 0.011812267622450953, 0.011416891557921617, 0.010996690591875496, 0.01055751325055635, 0.010106431399985856, 0.009650670952956951, 0.009196674409098535, 0.008749416765908187, 0.008312043620725768, 0.007885842070157079, 0.0074705015146574304, 0.007064581251954925, 0.006666080262713468, 0.00627300344400155, 0.005883835444219301, 0.005497862810550643, 0.0051153202414796085, 0.004737370064567755, 0.004365949703466099, 0.0040035363259081505, 0.00365288035422375, 0.003316751906526141, 0.0029977301288795564, 0.0026980490950503535, 0.0024194994581870936, 0.0021633751385350683, 0.0019304502345572597, 0.0017209726762809317, 0.0015346662930260362, 0.0013707396777990907, 0.001227906202417806, 0.0011044229880111903, 0.0009981566491176358, 0.0009066802768622454, 0.0008274003532389721, 0.0007577056203407176, 0.0006951240726344411, 0.0006374706742782475]
#> 4                                                                               [0.0008567297801018124, 0.0009198593045940619, 0.0009849798713337586, 0.0010531493257191942, 0.0011256472549536153, 0.0012039110036746777, 0.0012894552427874378, 0.0013837805736434954, 0.0014882780100760424, 0.001604137198180353, 0.001732266816047154, 0.0018732356310171966, 0.0020272420546990663, 0.0021941186090687504, 0.0023733754288971006, 0.002564283792591321, 0.00276599683522582, 0.0029777003359549165, 0.003198782201750289, 0.0034290054982555403, 0.003668667141947721, 0.003918723148782661, 0.004180861984551405, 0.004457510238864309, 0.004751759478289902, 0.005067209428508663, 0.005407730100052975, 0.005777153477297479, 0.006178913230014798, 0.0066156578604391674, 0.007088868081805046, 0.007598512427655491, 0.00814277561465372, 0.008717891669709679, 0.00931810811961258, 0.009935798709134228, 0.010561730548835604, 0.011185478035865231, 0.011795961441948501, 0.012382074140604946, 0.01293335066689084, 0.013440619795300273, 0.013896583986234692, 0.014296269812766733, 0.014637303567441648, 0.014919981566649877, 0.015147124282970945, 0.015323725201635654, 0.015456426628850885, 0.015552872904274078, 0.015621004225768155, 0.01566835989751901, 0.01570145752478168, 0.01572530482996938, 0.015743084704629658, 0.01575603399754449, 0.01576351500229189, 0.015763258356060496, 0.01575173948833285, 0.015724639621724813, 0.01567733755238327, 0.015605380058888273, 0.015504886035339512, 0.015372850932053808, 0.01520733207652581, 0.015007510113743421, 0.014773635477535445, 0.014506880138970328, 0.014209122951874162, 0.013882701252025464, 0.013530161899904986, 0.013154041968102983, 0.01275670331645716, 0.01234023712506142, 0.011906444962237972, 0.011456893130607469, 0.010993027863275426, 0.010516331390439513, 0.010028493787856935, 0.009531573448974562, 0.00902812027372329, 0.008521240143345366, 0.008014586460355487, 0.007512273624807478, 0.007018717176662438, 0.006538414720276072, 0.006075689466594922, 0.005634423298834656, 0.0052178080548302905, 0.004828142025910852, 0.004466693756438635, 0.004133647759105343, 0.0038281377156494482, 0.0035483632542284327, 0.00329177763575599, 0.003055326654085045, 0.0028357145085346398, 0.0026296707492177132, 0.0024341936604224527, 0.002246749315590833]
#> 5                                                    [0.008984869189537998, 0.010174901945381282, 0.011330788473660277, 0.012439011785706212, 0.013497927953329035, 0.014517723205295378, 0.015518483370562792, 0.016526724298294607, 0.017571018384444033, 0.01867749955530968, 0.019865999225532678, 0.021147366716052104, 0.022522215811303908, 0.023981005100604278, 0.025505098992273656, 0.027068336322217845, 0.02863867130620725, 0.030179611566082544, 0.031651390139085696, 0.033011997617928064, 0.034218312588891235, 0.03522758229104997, 0.03599942959810245, 0.03649842226172149, 0.036697064810407444, 0.03657889040030799, 0.03614116984350323, 0.03539665750496072, 0.03437380617618356, 0.03311504638487649, 0.031673049486389736, 0.030105333181030338, 0.028468017583895004, 0.026809857796141114, 0.025167735918543536, 0.02356453144652998, 0.022009749370131712, 0.020502616707635476, 0.019036760663005164, 0.0176052359184091, 0.016204670560921706, 0.01483763006301556, 0.013512836808967239, 0.012243459595801363, 0.011044145002930396, 0.009927698464144073, 0.008902308694997474, 0.007969981052387495, 0.007126481992354168, 0.00636269485871977, 0.005666942091417273, 0.005027618440454401, 0.004435449809585619, 0.0038848447755328802, 0.003374093951937584, 0.002904510182313267, 0.002478888912570667, 0.002099819451514179, 0.0017683569140689687, 0.0014833926818628514, 0.001241807748956295, 0.0010392479214360226, 0.0008711981700902743, 0.0007339921020613064, 0.0006254622650381927, 0.0005450747143003784, 0.0004935428713874183, 0.00047203820299043017, 0.0004811886197595707, 0.0005200812399563097, 0.0005854765729787892, 0.0006714071821109219, 0.0007692787945048763, 0.0008685138206157256, 0.0009576778797923742, 0.0010259222303462773, 0.0010644849128562531, 0.0010679521780803652, 0.0010350122577428345, 0.000968537440257695, 0.0008749834330233634, 0.0007632533980825752, 0.0006432902955030926, 0.0005247016611951395, 0.0004156780228925275, 0.0003223594055750091, 0.00024867153263784215, 0.00019653561154038136, 0.0001662839829929995, 0.00015710130885110204, 0.00016735168710612627, 0.00019472689698113148, 0.00023623453613927324, 0.0002881125929478678, 0.0003457906237472558, 0.00040400851811495396, 0.000457154814751257, 0.0004998121325000434, 0.0005274203695162199, 0.0005369139689480908]
#>   TARGET_SUBJECT_PREVALENCE CONTROL_SUBJECT_PREVALENCE
#> 1                     0.312                       0.00
#> 2                     0.310                       0.00
#> 3                     0.220                       0.00
#> 4                     0.158                       0.00
#> 5                     0.342                       0.01
#>   PREVALENCE_DIFFERENCE_RATIO
#> 1                       100.0
#> 2                       100.0
#> 3                       100.0
#> 4                       100.0
#> 5                        34.2
```

## `clustering_k*_summary.parquet` (k-specific cluster summaries)

These files have the same schema for each `k` in `cluster_k_values`.

``` r

kSummaryFiles <- grep("^clustering_k[0-9]+_summary[.]parquet$", names(summaryTables), value = TRUE)
kSummarySizes <- data.frame(
  file = kSummaryFiles,
  rows = vapply(summaryTables[kSummaryFiles], nrow, integer(1)),
  cols = vapply(summaryTables[kSummaryFiles], ncol, integer(1)),
  stringsAsFactors = FALSE
)
kSummarySizes
#>                                                        file rows cols
#> clustering_k2_summary.parquet clustering_k2_summary.parquet   58   17
#> clustering_k3_summary.parquet clustering_k3_summary.parquet   83   17
#> clustering_k4_summary.parquet clustering_k4_summary.parquet  100   17
#> clustering_k5_summary.parquet clustering_k5_summary.parquet  123   17
```

### Column Dictionary (schema shared by all `k` summary files)

``` r

clusterSummaryRef <- summaryTables[[kSummaryFiles[1]]]

clusterSummaryDescriptions <- c(
  CONCEPT_ID = "Concept id for the row.",
  cluster = "Cluster label (C1, C2, ... for selected k).",
  patient_count = "Number of cluster members with concept present.",
  time_median = "Median time-to-event within this cluster for this concept.",
  time_q1 = "First time quartile within cluster for this concept.",
  time_q3 = "Third time quartile within cluster for this concept.",
  time_min = "Minimum time-to-event within cluster for this concept.",
  time_max = "Maximum time-to-event within cluster for this concept.",
  total_cluster_patients = "Total number of patients assigned to this cluster.",
  CONCEPT_NAME = "Human-readable concept name.",
  ORIGINAL_CONCEPT_ID = "Original (base) concept id before ordinal transformation.",
  ORDINAL = "Ordinal index for ordinal concepts; 0 for non-ordinal concepts.",
  IS_ORDINAL = "Flag indicating whether this row represents an ordinalized concept.",
  age_mean = "Mean age of patients with this concept in the cluster.",
  age_std = "Age standard deviation of patients with this concept in the cluster.",
  male_proportion = "Male proportion among patients with this concept in the cluster.",
  prevalence = "patient_count / total_cluster_patients for this concept-cluster row."
)

clusterSummaryExamples <- c(
  CONCEPT_ID = paste("Unique concepts:", dplyr::n_distinct(clusterSummaryRef$CONCEPT_ID)),
  cluster = paste("Values:", paste(sort(unique(clusterSummaryRef$cluster)), collapse = ", ")),
  patient_count = paste("Range:", paste(range(clusterSummaryRef$patient_count, na.rm = TRUE), collapse = " to ")),
  time_median = paste("Range:", paste(range(clusterSummaryRef$time_median, na.rm = TRUE), collapse = " to ")),
  time_q1 = paste("Range:", paste(range(clusterSummaryRef$time_q1, na.rm = TRUE), collapse = " to ")),
  time_q3 = paste("Range:", paste(range(clusterSummaryRef$time_q3, na.rm = TRUE), collapse = " to ")),
  time_min = paste("Range:", paste(range(clusterSummaryRef$time_min, na.rm = TRUE), collapse = " to ")),
  time_max = paste("Range:", paste(range(clusterSummaryRef$time_max, na.rm = TRUE), collapse = " to ")),
  total_cluster_patients = paste("Range:", paste(range(clusterSummaryRef$total_cluster_patients, na.rm = TRUE), collapse = " to ")),
  CONCEPT_NAME = paste("Rows:", nrow(clusterSummaryRef)),
  ORIGINAL_CONCEPT_ID = paste("Unique:", dplyr::n_distinct(clusterSummaryRef$ORIGINAL_CONCEPT_ID)),
  ORDINAL = paste("Range:", paste(range(clusterSummaryRef$ORDINAL, na.rm = TRUE), collapse = " to ")),
  IS_ORDINAL = paste("Values:", paste(unique(clusterSummaryRef$IS_ORDINAL), collapse = ", ")),
  age_mean = paste("Range:", paste(round(range(clusterSummaryRef$age_mean, na.rm = TRUE), 2), collapse = " to ")),
  age_std = paste("Range:", paste(round(range(clusterSummaryRef$age_std, na.rm = TRUE), 2), collapse = " to ")),
  male_proportion = paste("Range:", paste(round(range(clusterSummaryRef$male_proportion, na.rm = TRUE), 3), collapse = " to ")),
  prevalence = paste("Range:", paste(round(range(clusterSummaryRef$prevalence, na.rm = TRUE), 3), collapse = " to "))
)

buildColumnDictionary(clusterSummaryRef, clusterSummaryDescriptions, clusterSummaryExamples)
#>                                        column      type
#> CONCEPT_ID                         CONCEPT_ID character
#> cluster                               cluster character
#> patient_count                   patient_count   numeric
#> time_median                       time_median   numeric
#> time_q1                               time_q1   numeric
#> time_q3                               time_q3   numeric
#> time_min                             time_min   numeric
#> time_max                             time_max   numeric
#> total_cluster_patients total_cluster_patients   numeric
#> CONCEPT_NAME                     CONCEPT_NAME character
#> ORIGINAL_CONCEPT_ID       ORIGINAL_CONCEPT_ID character
#> ORDINAL                               ORDINAL   numeric
#> IS_ORDINAL                         IS_ORDINAL   logical
#> age_mean                             age_mean   numeric
#> age_std                               age_std   numeric
#> male_proportion               male_proportion   numeric
#> prevalence                         prevalence   numeric
#>                                                                                 description
#> CONCEPT_ID                                                          Concept id for the row.
#> cluster                                         Cluster label (C1, C2, ... for selected k).
#> patient_count                               Number of cluster members with concept present.
#> time_median                      Median time-to-event within this cluster for this concept.
#> time_q1                                First time quartile within cluster for this concept.
#> time_q3                                Third time quartile within cluster for this concept.
#> time_min                             Minimum time-to-event within cluster for this concept.
#> time_max                             Maximum time-to-event within cluster for this concept.
#> total_cluster_patients                   Total number of patients assigned to this cluster.
#> CONCEPT_NAME                                                   Human-readable concept name.
#> ORIGINAL_CONCEPT_ID               Original (base) concept id before ordinal transformation.
#> ORDINAL                     Ordinal index for ordinal concepts; 0 for non-ordinal concepts.
#> IS_ORDINAL              Flag indicating whether this row represents an ordinalized concept.
#> age_mean                             Mean age of patients with this concept in the cluster.
#> age_std                Age standard deviation of patients with this concept in the cluster.
#> male_proportion            Male proportion among patients with this concept in the cluster.
#> prevalence             patient_count / total_cluster_patients for this concept-cluster row.
#>                               lc500s_example
#> CONCEPT_ID               Unique concepts: 33
#> cluster                       Values: C1, C2
#> patient_count                Range: 5 to 328
#> time_median                  Range: 6 to 251
#> time_q1                      Range: 3 to 235
#> time_q3                      Range: 9 to 264
#> time_min                     Range: 0 to 186
#> time_max                    Range: 17 to 320
#> total_cluster_patients     Range: 164 to 336
#> CONCEPT_NAME                        Rows: 58
#> ORIGINAL_CONCEPT_ID               Unique: 16
#> ORDINAL                        Range: 0 to 4
#> IS_ORDINAL               Values: FALSE, TRUE
#> age_mean                     Range: 56 to 87
#> age_std                     Range: 0 to 8.54
#> male_proportion                Range: 0 to 1
#> prevalence             Range: 0.015 to 0.976
```

### Example Rows (`k = 3`)

``` r

k3Summary <- summaryTables[["clustering_k3_summary.parquet"]]
utils::head(k3Summary, 5)
#>   CONCEPT_ID cluster patient_count time_median time_q1 time_q3 time_min
#> 1    2107967      C1             5       116.5  104.75  130.25       80
#> 2    2107967      C2             5        61.5   33.75  103.25       28
#> 3    2107967      C3           157        22.0   17.00   27.00        0
#> 4    2107968      C1             5       104.0   65.00  121.00       47
#> 5    2107968      C2             5        84.0   49.25  114.25       19
#>   time_max total_cluster_patients      CONCEPT_NAME ORIGINAL_CONCEPT_ID ORDINAL
#> 1      161                    170         Lobectomy             2107967       0
#> 2      145                    160         Lobectomy             2107967       0
#> 3       43                    170         Lobectomy             2107967       0
#> 4      129                    170 Lobectomy of lung             2107968       0
#> 5      147                    160 Lobectomy of lung             2107968       0
#>   IS_ORDINAL age_mean  age_std male_proportion prevalence
#> 1      FALSE 53.66667 7.234178       0.6666667 0.02941176
#> 2      FALSE 76.80000 6.942622       0.6000000 0.03125000
#> 3      FALSE 66.63694 5.060091       0.5923567 0.92352941
#> 4      FALSE 57.75000 3.403430       0.7500000 0.02941176
#> 5      FALSE 75.00000 8.041559       0.5000000 0.03125000
```

## `clustering_k*_pairwise_overlap.parquet` (k-specific overlap matrices)

These files provide concept-concept overlap metrics in long format for:

- `group = overall`
- `group = C1`, `C2`, … for each cluster

``` r

kOverlapFiles <- grep("^clustering_k[0-9]+_pairwise_overlap[.]parquet$", names(summaryTables), value = TRUE)
kOverlapSizes <- data.frame(
  file = kOverlapFiles,
  rows = vapply(summaryTables[kOverlapFiles], nrow, integer(1)),
  cols = vapply(summaryTables[kOverlapFiles], ncol, integer(1)),
  stringsAsFactors = FALSE
)
kOverlapSizes
#>                                                                          file
#> clustering_k2_pairwise_overlap.parquet clustering_k2_pairwise_overlap.parquet
#> clustering_k3_pairwise_overlap.parquet clustering_k3_pairwise_overlap.parquet
#> clustering_k4_pairwise_overlap.parquet clustering_k4_pairwise_overlap.parquet
#> clustering_k5_pairwise_overlap.parquet clustering_k5_pairwise_overlap.parquet
#>                                        rows cols
#> clustering_k2_pairwise_overlap.parquet  408    9
#> clustering_k3_pairwise_overlap.parquet  544    9
#> clustering_k4_pairwise_overlap.parquet  680    9
#> clustering_k5_pairwise_overlap.parquet  816    9
```

### Column Dictionary (schema shared by all overlap files)

``` r

overlapRef <- summaryTables[[kOverlapFiles[1]]]

overlapDescriptions <- c(
  concept_id_1 = "First concept id in pair.",
  concept_id_2 = "Second concept id in pair.",
  jaccard = "Jaccard index for pair co-occurrence in selected group.",
  phi_correlation = "Phi correlation between binary concept presence vectors in selected group.",
  prevalence = "Single-concept prevalence; populated for diagonal rows where concept_id_1 == concept_id_2.",
  patient_count = "Single-concept patient count; populated for diagonal rows.",
  group = "Scope of metric: overall or cluster label (C1, C2, ...).",
  co_occurrence = "Number of patients with both concepts present; populated for off-diagonal rows.",
  union = "Number of patients with either concept present; populated for off-diagonal rows."
)

overlapExamples <- c(
  concept_id_1 = paste("Unique concepts:", dplyr::n_distinct(overlapRef$concept_id_1)),
  concept_id_2 = paste("Unique concepts:", dplyr::n_distinct(overlapRef$concept_id_2)),
  jaccard = paste("Range:", paste(round(range(overlapRef$jaccard, na.rm = TRUE), 3), collapse = " to ")),
  phi_correlation = paste("Range:", paste(round(range(overlapRef$phi_correlation, na.rm = TRUE), 3), collapse = " to ")),
  prevalence = paste("Non-missing rows:", sum(!is.na(overlapRef$prevalence))),
  patient_count = paste("Non-missing rows:", sum(!is.na(overlapRef$patient_count))),
  group = paste("Values:", paste(sort(unique(overlapRef$group)), collapse = ", ")),
  co_occurrence = paste("Non-missing rows:", sum(!is.na(overlapRef$co_occurrence))),
  union = paste("Non-missing rows:", sum(!is.na(overlapRef$union)))
)

buildColumnDictionary(overlapRef, overlapDescriptions, overlapExamples)
#>                          column      type
#> concept_id_1       concept_id_1 character
#> concept_id_2       concept_id_2 character
#> jaccard                 jaccard   numeric
#> phi_correlation phi_correlation   numeric
#> prevalence           prevalence   numeric
#> patient_count     patient_count   numeric
#> group                     group character
#> co_occurrence     co_occurrence   numeric
#> union                     union   numeric
#>                                                                                                description
#> concept_id_1                                                                     First concept id in pair.
#> concept_id_2                                                                    Second concept id in pair.
#> jaccard                                            Jaccard index for pair co-occurrence in selected group.
#> phi_correlation                 Phi correlation between binary concept presence vectors in selected group.
#> prevalence      Single-concept prevalence; populated for diagonal rows where concept_id_1 == concept_id_2.
#> patient_count                                   Single-concept patient count; populated for diagonal rows.
#> group                                             Scope of metric: overall or cluster label (C1, C2, ...).
#> co_occurrence              Number of patients with both concepts present; populated for off-diagonal rows.
#> union                     Number of patients with either concept present; populated for off-diagonal rows.
#>                          lc500s_example
#> concept_id_1        Unique concepts: 16
#> concept_id_2        Unique concepts: 16
#> jaccard                   Range: 0 to 1
#> phi_correlation        Range: -0.9 to 1
#> prevalence         Non-missing rows: 48
#> patient_count      Non-missing rows: 48
#> group           Values: C1, C2, overall
#> co_occurrence     Non-missing rows: 360
#> union             Non-missing rows: 360
```

### Diagonal vs Off-Diagonal Semantics

``` r

k3Overlap <- summaryTables[["clustering_k3_pairwise_overlap.parquet"]]
diagRows <- k3Overlap[k3Overlap$concept_id_1 == k3Overlap$concept_id_2, , drop = FALSE]
pairRows <- k3Overlap[k3Overlap$concept_id_1 != k3Overlap$concept_id_2, , drop = FALSE]

data.frame(
  check = c(
    "Diagonal rows count",
    "Off-diagonal rows count",
    "Diagonal prevalence non-missing",
    "Off-diagonal prevalence all NA",
    "Off-diagonal co_occurrence non-missing",
    "Off-diagonal union non-missing"
  ),
  value = c(
    nrow(diagRows),
    nrow(pairRows),
    all(!is.na(diagRows$prevalence)),
    all(is.na(pairRows$prevalence)),
    all(!is.na(pairRows$co_occurrence)),
    all(!is.na(pairRows$union))
  ),
  stringsAsFactors = FALSE
)
#>                                    check value
#> 1                    Diagonal rows count    64
#> 2                Off-diagonal rows count   480
#> 3        Diagonal prevalence non-missing     1
#> 4         Off-diagonal prevalence all NA     1
#> 5 Off-diagonal co_occurrence non-missing     1
#> 6         Off-diagonal union non-missing     1
```

### Example Rows (`k = 3`)

``` r

utils::head(k3Overlap, 8)
#>   concept_id_1 concept_id_2    jaccard phi_correlation prevalence patient_count
#> 1      2107967      2107967 1.00000000       1.0000000       0.33           165
#> 2      2107967      2107968 0.80000000       0.8334994         NA            NA
#> 3      2107967      2108158 0.79329609       0.8310146         NA            NA
#> 4      2107967        32280 0.01904762      -0.4175270         NA            NA
#> 5      2107967        32815 0.01298701      -0.4155442         NA            NA
#> 6      2107967      4008211 0.01510574      -0.4611329         NA            NA
#> 7      2107967      4008226 0.01230769      -0.4540724         NA            NA
#> 8      2107967      4014023 0.01960784      -0.3968722         NA            NA
#>     group co_occurrence union
#> 1 overall            NA    NA
#> 2 overall           148   185
#> 3 overall           142   179
#> 4 overall             6   315
#> 5 overall             5   308
#> 6 overall             5   331
#> 7 overall             5   325
#> 8 overall             6   306
```

## `complementaryMappingTable.parquet`

This keeps mapping history structure in summary mode. In `lc500s` it is
empty.

``` r

mappingSummary <- summaryTables[["complementaryMappingTable.parquet"]]
mappingSummary
#> [1] CONCEPT_ID       CONCEPT_NAME     NEW_CONCEPT_ID   NEW_CONCEPT_NAME
#> [5] TYPE             HERITAGE        
#> <0 rows> (or 0-length row.names)
```

### Column Dictionary

``` r

mappingDescriptions <- c(
  CONCEPT_ID = "Source/original concept id.",
  CONCEPT_NAME = "Source/original concept name.",
  NEW_CONCEPT_ID = "Mapped/merged concept id.",
  NEW_CONCEPT_NAME = "Mapped/merged concept name.",
  TYPE = "Mapping origin/type (for example custom/hierarchy/correlation).",
  HERITAGE = "Domain/heritage for mapping context."
)

mappingExamples <- c(
  CONCEPT_ID = paste("Rows:", nrow(mappingSummary)),
  CONCEPT_NAME = if (nrow(mappingSummary) == 0) "No mappings in lc500s example." else "",
  NEW_CONCEPT_ID = "",
  NEW_CONCEPT_NAME = "",
  TYPE = "",
  HERITAGE = ""
)

buildColumnDictionary(mappingSummary, mappingDescriptions, mappingExamples)
#>                            column    type
#> CONCEPT_ID             CONCEPT_ID integer
#> CONCEPT_NAME         CONCEPT_NAME integer
#> NEW_CONCEPT_ID     NEW_CONCEPT_ID integer
#> NEW_CONCEPT_NAME NEW_CONCEPT_NAME integer
#> TYPE                         TYPE integer
#> HERITAGE                 HERITAGE integer
#>                                                                      description
#> CONCEPT_ID                                           Source/original concept id.
#> CONCEPT_NAME                                       Source/original concept name.
#> NEW_CONCEPT_ID                                         Mapped/merged concept id.
#> NEW_CONCEPT_NAME                                     Mapped/merged concept name.
#> TYPE             Mapping origin/type (for example custom/hierarchy/correlation).
#> HERITAGE                                    Domain/heritage for mapping context.
#>                                  lc500s_example
#> CONCEPT_ID                              Rows: 0
#> CONCEPT_NAME     No mappings in lc500s example.
#> NEW_CONCEPT_ID                                 
#> NEW_CONCEPT_NAME                               
#> TYPE                                           
#> HERITAGE
```

## Practical Interpretation Order in Summary Mode

1.  Read `metadata.json` first: confirms mode, cohort scale, and
    precomputed `k` values.
2.  Use `concept_summaries.parquet` for overall concept interpretation.
3.  Use `ordinal_summaries.parquet` to inspect repeated-event
    progression.
4.  Use `clustering_k*_summary.parquet` for concept behavior inside each
    cluster.
5.  Use `clustering_k*_pairwise_overlap.parquet` for concept
    co-occurrence and correlation structure by group.
6.  Check `complementaryMappingTable.parquet` for merge provenance.
