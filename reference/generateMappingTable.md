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

  A dataframe with columns descendant_concept_id and
  max_levels_of_separation
