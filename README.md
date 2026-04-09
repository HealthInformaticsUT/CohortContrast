
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

For complete setup and workflow code, use:

1.  Project wiki (setup + workflow):
    <https://github.com/HealthInformaticsUT/CohortContrast/wiki>
2.  Package site (function reference + vignettes):
    <https://healthinformaticsut.github.io/CohortContrast/>

If you are running CohortContrast on an air-gapped server, see the wiki
article here:
<https://github.com/HealthInformaticsUT/CohortContrast/wiki>

## Outputs

The CohortContrast package generates the following outputs:

1.  Running `CohortContrast` returns a list of tables (patient level
    summarised data for target and control) and saves a study folder with
    parquet files that can be analysed in the GUI directly.
2.  Using viewer helpers with `runCohortContrastViewer` generates plots
    from parquet-formatted results.
3.  Example studies are available under `inst/example/st`.
4.  Public study outputs can be explored at
    <http://omop-apps.cloud.ut.ee/CohortContrast/>.
5.  A larger example with 1080 diagnosis-level outputs is available in
    the CohortContrast Atlas at
    <http://omop-apps.cloud.ut.ee/CohortContrastAtlas/>.
    

### More information

CohortContrast provides much more insight generation possibilities. See
the package site for details:
<https://healthinformaticsut.github.io/CohortContrast/>

For feature requests create issues on Github or contact Markus Haug
(<markus.haug@ut.ee>) personally. Bug reports can be opened at
<https://github.com/HealthInformaticsUT/CohortContrast/issues>.
