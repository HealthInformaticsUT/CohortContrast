# Function for automatically combining concepts by hierarchy mapping

Function for automatically combining concepts by hierarchy mapping

## Usage

``` r
automaticHierarchyCombineConcepts(
  data,
  abstraction_level = -1,
  minDepthAllowed = 0,
  allowOnlyMinors = FALSE
)
```

## Arguments

- data:

  CohortContrastObject

- abstraction_level:

  abstraction level to use for mapping

- minDepthAllowed:

  integer for restricting the mapping, if a concept is part of a
  hierarchy tree then minDepthAllowed value will prune the tree from
  said depth value upwards

- allowOnlyMinors:

  allows only mappping if child has smaller prevalence than parent
