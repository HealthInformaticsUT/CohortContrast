# Create a mapping table with predefined maximum abstraction level

Create a mapping table with predefined maximum abstraction level

## Usage

``` r
generateMappingTable(
  abstractionLevel = 10,
  data = NULL,
  maxMinDataFrame = NULL
)
```

## Arguments

- abstractionLevel:

  Maximum abstraction level allowed

- data:

  CohortContrastObject returned by CohortContrast function

- maxMinDataFrame:

  Optional data frame describing the maximum available abstraction depth
  per descendant concept. Must contain columns \`descendant_concept_id\`
  and \`maximum_minimal_separation\`. If \`NULL\`, the summary is
  computed automatically from \`data\$conceptsData\$concept_ancestor\`.

## Value

A data frame describing hierarchy-based concept mappings. Each row maps
an original concept to a replacement concept through the columns
CONCEPT_ID, CONCEPT_NAME, NEW_CONCEPT_ID, NEW_CONCEPT_NAME,
ABSTRACTION_LEVEL, and TYPE. An empty data frame is returned if no
concepts need to be mapped at the requested abstraction level.

## Examples

``` r
if (requireNamespace("nanoparquet", quietly = TRUE)) {
  studyDir <- system.file("example", "st", package = "CohortContrast")
  study <- loadCohortContrastStudy("lc500", pathToResults = studyDir)

  maxMinDataFrame <- data.frame(
    descendant_concept_id = c(
      4008211, 4176729, 2107967, 2107968, 2108158, 32280, 32815
    ),
    maximum_minimal_separation = c(0, 1, 0, 1, 1, 0, 1)
  )

  mappingTable <- generateMappingTable(
    abstractionLevel = 0,
    data = study,
    maxMinDataFrame = maxMinDataFrame
  )
  mappingTable
}
#> --------------------------------- 
#> Generating mapping table list ... 
#> --------------------------------- 
#> Timestamp:  2026-04-22 09:12:03 
#> 
#> ---------------------------------------------- 
#> Generating mapping for abstraction level 1 ... 
#> ---------------------------------------------- 
#> Timestamp:  2026-04-22 09:12:03 
#> 
#>   CONCEPT_ID                                           CONCEPT_NAME
#> 1    4176729 Treatment planning for external beam radiation therapy
#> 2    2107968                                      Lobectomy of lung
#> 3    2108158                                  Partial pneumonectomy
#> 4      32815                                      Death Certificate
#>   NEW_CONCEPT_ID NEW_CONCEPT_NAME ABSTRACTION_LEVEL      TYPE
#> 1        4008211     Radiotherapy                 0 hierarchy
#> 2        2107967        Lobectomy                 0 hierarchy
#> 3        2107967        Lobectomy                 0 hierarchy
#> 4          32280            Death                 0 hierarchy
```
