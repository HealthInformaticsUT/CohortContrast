# Function for automatically combining concepts by hierarchy mapping

Function for automatically combining concepts by hierarchy mapping

## Usage

``` r
automaticCorrelationCombineConcepts(
  data,
  abstractionLevel = -1,
  minCorrelation = 0.7,
  maxDaysInBetween = 1,
  heritageDriftAllowed = FALSE
)
```

## Arguments

- data:

  CohortContrastObject

- abstractionLevel:

  abstraction level to use for mapping

- minCorrelation:

  minimum correlation to use for automatic concept combining

- maxDaysInBetween:

  minimum days between concepts to use for automatic concept combining

- heritageDriftAllowed:

  boolean for allowing heritage drift (combining concepts from differing
  heritages)

## Value

A CohortContrastObject with correlation-based concept merges applied.
The returned object keeps the same overall structure as the input, while
updating the patient-, feature-, and cohort-level tables together with
the complementary mapping table to reflect the executed correlation
mappings.

## Examples

``` r
study <- structure(
  list(
    data_initial = data.frame(
      COHORT_DEFINITION_ID = c(rep("target", 4), rep("control", 4)),
      SUBJECT_ID = 1:8,
      COHORT_START_DATE = as.Date(rep("2020-01-01", 8)),
      COHORT_END_DATE = as.Date(rep("2020-01-10", 8))
    ),
    data_patients = data.frame(
      COHORT_DEFINITION_ID = c(
        "target", "target", "target", "target", "target", "target",
        "control", "control", "control"
      ),
      PERSON_ID = c(1, 1, 2, 2, 3, 4, 5, 6, 7),
      CONCEPT_ID = c(1, 2, 1, 2, 1, 2, 1, 2, 1),
      CONCEPT_NAME = c(
        "Concept A", "Concept B", "Concept A", "Concept B", "Concept A",
        "Concept B", "Concept A", "Concept B", "Concept A"
      ),
      HERITAGE = rep("drug_exposure", 9),
      ABSTRACTION_LEVEL = rep(-1, 9),
      PREVALENCE = rep(1, 9),
      TIME_TO_EVENT = I(list(0, 1, 0, 1, 0, 1, 0, 1, 0))
    ),
    data_features = data.frame(
      CONCEPT_ID = c(1, 2),
      CONCEPT_NAME = c("Concept A", "Concept B"),
      ABSTRACTION_LEVEL = c(-1, -1),
      TARGET_SUBJECT_COUNT = c(3, 3),
      CONTROL_SUBJECT_COUNT = c(2, 1),
      TIME_TO_EVENT = I(list(c(0, 0, 0), c(1, 1, 1))),
      TARGET_SUBJECT_PREVALENCE = c(0.75, 0.75),
      CONTROL_SUBJECT_PREVALENCE = c(0.5, 0.25),
      PREVALENCE_DIFFERENCE_RATIO = c(1.5, 3),
      CHI2Y = c(TRUE, TRUE),
      CHI2Y_P_VALUE = c(0.1, 0.01),
      LOGITTEST = c(FALSE, FALSE),
      LOGITTEST_P_VALUE = c(1, 1),
      HERITAGE = c("drug_exposure", "drug_exposure")
    ),
    data_person = data.frame(
      PERSON_ID = 1:8,
      YEAR_OF_BIRTH = 1980:1987,
      GENDER_CONCEPT_ID = c(8507, 8532, 8507, 8532, 8507, 8532, 8507, 8532)
    ),
    complementaryMappingTable = data.frame(
      CONCEPT_ID = integer(),
      CONCEPT_NAME = character(),
      NEW_CONCEPT_ID = integer(),
      NEW_CONCEPT_NAME = character(),
      ABSTRACTION_LEVEL = integer(),
      TYPE = character()
    ),
    config = list(
      runChi2YTests = TRUE,
      runLogitTests = FALSE,
      presenceFilter = 0,
      prevalenceCutOff = 0
    )
  ),
  class = "CohortContrastObject"
)

combined <- automaticCorrelationCombineConcepts(
  study,
  abstractionLevel = -1,
  minCorrelation = 0.5,
  maxDaysInBetween = 2
)
#> ! Automatic correlation mapping iteration 1
combined$data_features
#>   CONCEPT_ID CONCEPT_NAME ABSTRACTION_LEVEL TARGET_SUBJECT_COUNT
#> 1          1    Concept A                -1                    3
#> 2          2    Concept B                -1                    3
#>   CONTROL_SUBJECT_COUNT TIME_TO_EVENT TARGET_SUBJECT_PREVALENCE
#> 1                     2       0, 0, 0                      0.75
#> 2                     1       1, 1, 1                      0.75
#>   CONTROL_SUBJECT_PREVALENCE PREVALENCE_DIFFERENCE_RATIO CHI2Y CHI2Y_P_VALUE
#> 1                       0.50                         1.5  TRUE          0.10
#> 2                       0.25                         3.0  TRUE          0.01
#>   LOGITTEST LOGITTEST_P_VALUE      HERITAGE
#> 1     FALSE                 1 drug_exposure
#> 2     FALSE                 1 drug_exposure
combined$complementaryMappingTable
#> [1] CONCEPT_ID        CONCEPT_NAME      NEW_CONCEPT_ID    NEW_CONCEPT_NAME 
#> [5] ABSTRACTION_LEVEL TYPE             
#> <0 rows> (or 0-length row.names)
```
