# This function uses complementaryMappingTable to map concepts to custom names

This function uses complementaryMappingTable to map concepts to custom
names

## Usage

``` r
handleMapping(data, complementaryMappingTable, abstractionLevel = -1)
```

## Arguments

- data:

  Data list object

- complementaryMappingTable:

  Mappingtable for mapping concept_ids if present, columns CONCEPT_ID,
  CONCEPT_NAME, NEW_CONCEPT_ID, NEW_CONCEPT_NAME, ABSTRACTION_LEVEL,

- abstractionLevel:

  Level of abstraction, by default -1 (imported data level)
