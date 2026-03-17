# Remove Temporal Bias from CohortContrast Analysis

This function identifies and optionally removes concepts that may
represent temporal bias in a CohortContrast analysis. It works by
creating age/sex matched controls from the general population for the
same time periods as the target cohort, then using a proportion test to
identify concepts where the matched cohort has greater or equal
prevalence compared to the target. These concepts likely represent
temporal trends (e.g., seasonal effects, healthcare changes) rather than
condition-specific features.

## Usage

``` r
removeTemporalBias(
  data,
  cdm,
  ratio = 1,
  alpha = 0.05,
  domainsIncluded = NULL,
  removeIdentified = FALSE
)
```

## Arguments

- data:

  A CohortContrast result object (returned from CohortContrast function)

- cdm:

  Connection to the database (package CDMConnector)

- ratio:

  Matching ratio for control cohort generation (default: 1)

- alpha:

  Significance level for the proportion test before Bonferroni
  correction (default: 0.05)

- domainsIncluded:

  Domains to analyze for temporal bias (default: same as original
  analysis)

- removeIdentified:

  If TRUE, automatically remove identified temporal bias concepts from
  the data (default: FALSE)

## Value

A list containing:

- temporal_bias_concepts:

  A data frame of concepts identified as potential temporal bias

- data:

  The original or filtered CohortContrast data object (if
  removeIdentified = TRUE)

- matched_control_prevalences:

  Prevalence data from the matched control cohort

## Details

The function applies Bonferroni correction for multiple testing,
adjusting the significance level by dividing alpha by the number of
concepts being tested.

## Examples

``` r
if (FALSE) { # \dontrun{
# Run CohortContrast analysis
data <- CohortContrast(cdm, targetTable, controlTable, pathToResults, ...)

# Identify temporal bias concepts
result <- removeTemporalBias(data, cdm, ratio = 1)

# View identified concepts
print(result$temporal_bias_concepts)

# Remove identified concepts and get filtered data
result_filtered <- removeTemporalBias(data, cdm, removeIdentified = TRUE)
filtered_data <- result_filtered$data
} # }
```
