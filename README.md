
# CohortContrast

The goal of CohortContrast is to facilitate the comparison between
cohorts in specified domains across all OMOP CDM datasets. It enables
users to analyze and visualize the contrast between target and control
cohorts effectively.

## Installation

The development version of the package from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("HealthInformaticsUT/CohortContrast")
```

## Usage

For full setup and execution code, use the project documentation:

1.  **Project wiki (setup + workflow)**:
    <https://github.com/HealthInformaticsUT/CohortContrast/wiki>
2.  **Package site (reference + vignettes)**:
    <https://healthinformaticsut.github.io/CohortContrast/>

This README intentionally keeps only a concise overview to avoid
duplicating setup instructions in multiple places.

## Outputs

The CohortContrast package generates the following outputs:

1.  Running `CohortContrast` returns a list of tables (patient level
    summarised data for target and control) and saves a study folder with
    parquet files that can be analysed in the GUI directly.
2.  Using viewer helpers with `runCohortContrastViewer` generates plots
    from parquet-formatted results.
3.  Example studies are available under `inst/example/st`.
4.  Published studies can be explored at
    <http://omop-apps.cloud.ut.ee/CohortContrast/>.
    

### More information

CohortContrast provides much more insight generation possibilities. See
the documentation site for details:
<https://healthinformaticsut.github.io/CohortContrast/>

For feature requests create issues on Github or contact Markus Haug
(<markus.haug@ut.ee>) personally.
