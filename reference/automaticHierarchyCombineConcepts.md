# Function for automatically combining concepts by hierarchy mapping

Function for automatically combining concepts by hierarchy mapping

## Usage

``` r
automaticHierarchyCombineConcepts(
  data,
  abstractionLevel = -1,
  minDepthAllowed = 0,
  allowOnlyMinors = FALSE
)
```

## Arguments

- data:

  CohortContrastObject

- abstractionLevel:

  abstraction level to use for mapping

- minDepthAllowed:

  integer for restricting the mapping, if a concept is part of a
  hierarchy tree then minDepthAllowed value will prune the tree from
  said depth value upwards

- allowOnlyMinors:

  allows only mappping if child has smaller prevalence than parent

## Value

A CohortContrastObject with hierarchy-based concept merges applied. The
returned object keeps the same overall structure as the input, while
updating the patient-, feature-, and cohort-level tables together with
the complementary mapping table to reflect the executed hierarchy
mappings.

## Examples

``` r
study <- structure(
  list(
    data_initial = data.frame(
      COHORT_DEFINITION_ID = c("target", "target", "control", "control"),
      SUBJECT_ID = 1:4,
      COHORT_START_DATE = as.Date(rep("2020-01-01", 4)),
      COHORT_END_DATE = as.Date(rep("2020-01-10", 4))
    ),
    data_patients = data.frame(
      COHORT_DEFINITION_ID = c(
        "target", "target", "target", "target", "target", "target",
        "control", "control", "control", "control", "control", "control"
      ),
      PERSON_ID = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4),
      CONCEPT_ID = c(1, 2, 10, 1, 2, 10, 1, 2, 10, 1, 2, 10),
      CONCEPT_NAME = c(
        "Concept A", "Concept B", "Parent concept",
        "Concept A", "Concept B", "Parent concept",
        "Concept A", "Concept B", "Parent concept",
        "Concept A", "Concept B", "Parent concept"
      ),
      HERITAGE = rep("drug_exposure", 12),
      ABSTRACTION_LEVEL = rep(-1, 12),
      PREVALENCE = rep(1, 12),
      TIME_TO_EVENT = I(rep(list(c(0, 2)), 12))
    ),
    data_features = data.frame(
      CONCEPT_ID = c(1, 2, 10),
      CONCEPT_NAME = c("Concept A", "Concept B", "Parent concept"),
      ABSTRACTION_LEVEL = c(-1, -1, -1),
      TARGET_SUBJECT_COUNT = c(2, 2, 2),
      CONTROL_SUBJECT_COUNT = c(2, 2, 2),
      TIME_TO_EVENT = I(list(c(0, 2), c(0, 2), c(0, 2))),
      TARGET_SUBJECT_PREVALENCE = c(1, 1, 1),
      CONTROL_SUBJECT_PREVALENCE = c(1, 1, 1),
      PREVALENCE_DIFFERENCE_RATIO = c(1, 1, 1),
      CHI2Y = c(TRUE, TRUE, TRUE),
      CHI2Y_P_VALUE = c(1, 1, 1),
      LOGITTEST = c(FALSE, FALSE, FALSE),
      LOGITTEST_P_VALUE = c(1, 1, 1),
      HERITAGE = c("drug_exposure", "drug_exposure", "drug_exposure")
    ),
    data_person = data.frame(),
    conceptsData = list(
      concept = data.frame(
        concept_id = c(1, 2, 10),
        concept_name = c("Concept A", "Concept B", "Parent concept"),
        invalid_reason = c(NA, NA, NA)
      ),
      concept_ancestor = data.frame(
        ancestor_concept_id = c(1, 2, 10, 10, 10),
        descendant_concept_id = c(1, 2, 10, 1, 2),
        min_levels_of_separation = c(0, 0, 0, 1, 1),
        max_levels_of_separation = c(0, 0, 0, 1, 1)
      )
    ),
    complementaryMappingTable = data.frame(
      CONCEPT_ID = integer(),
      CONCEPT_NAME = character(),
      NEW_CONCEPT_ID = integer(),
      NEW_CONCEPT_NAME = character(),
      ABSTRACTION_LEVEL = integer(),
      TYPE = character()
    )
  ),
  class = "CohortContrastObject"
)

combined <- suppressWarnings(
  automaticHierarchyCombineConcepts(study, abstractionLevel = -1)
)
#> ! Automatic hierarchy mapping iteration 1
#> ! Automatic hierarchy mapping iteration 2
combined$data_features
#> Index: <ABSTRACTION_LEVEL__CONCEPT_ID>
#>    CONCEPT_ID   CONCEPT_NAME ABSTRACTION_LEVEL TARGET_SUBJECT_COUNT
#>         <num>         <char>             <num>                <int>
#> 1:         10 Parent concept                -1                    2
#>    CONTROL_SUBJECT_COUNT       TIME_TO_EVENT TARGET_SUBJECT_PREVALENCE
#>                    <int>              <list>                     <num>
#> 1:                     2 0,2,0,2,0,2,...[24]                         1
#>    CONTROL_SUBJECT_PREVALENCE PREVALENCE_DIFFERENCE_RATIO  CHI2Y CHI2Y_P_VALUE
#>                         <num>                       <num> <lgcl>         <num>
#> 1:                          1                           1  FALSE             1
#>    LOGITTEST LOGITTEST_P_VALUE      HERITAGE
#>       <lgcl>             <num>        <char>
#> 1:     FALSE                 1 drug_exposure
combined$complementaryMappingTable
#>    CONCEPT_ID CONCEPT_NAME NEW_CONCEPT_ID NEW_CONCEPT_NAME ABSTRACTION_LEVEL
#>         <num>       <char>          <num>           <char>             <num>
#> 1:          1    Concept A             10   Parent concept                -1
#> 2:          2    Concept B             10   Parent concept                -1
#>         TYPE
#>       <char>
#> 1: hierarchy
#> 2: hierarchy
```
