# Function for automatically combining concepts by hierarchy mapping

Function for automatically combining concepts by hierarchy mapping

## Usage

``` r
automaticCorrelationCombineConcepts(
  data,
  abstraction_level = -1,
  minCorrelation = 0.7,
  maxDaysInBetween = 1,
  heritageDriftAllowed = FALSE
)
```

## Arguments

- data:

  CohortContrastObject

- abstraction_level:

  abstraction level to use for mapping

- minCorrelation:

  minimum correlation to use for automatic concept combining

- maxDaysInBetween:

  minimum days between concepts to use for automatic concept combining

- heritageDriftAllowed:

  boolean for allowing heritage drift (combining concepts from differing
  heritages)
