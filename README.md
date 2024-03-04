# CohortContrast

CohortContrast is a package designed to facilitate the comparison between cohorts in specified domains across all OMOP CDM datasets. It enables users to analyze and visualize the contrast between target and control cohorts effectively.

## Usage

To use CohortContrast, follow these steps to configure your environment and input data:

1. **Credentials**: Populate `./extras/codeToRun.R` with your database credentials to enable access to the OMOP CDM.

2. **Cohort JSON Configuration**:
   - Populate `./inst/JSON/target.json` with the JSON content for your target cohort.
   - Populate `./inst/JSON/control.json` with the JSON content for your control cohort.

   **OR**

3. **Cohort CSV Configuration**:
   - Populate `./inst/CSV/cohorts.csv` with patient IDs, cohort inclusion start dates, and end dates. Ensure that the target cohort has a smaller cohort ID in comparison to the control cohort (e.g., target = 1, control = 2).

4. **Run the Study**: Execute the study by running the script located in `./extras/codeToRun.R`.

## Outputs

The CohortContrast package generates the following outputs:

1. **PCA Analysis Plot**: A principal component analysis plot to visualize the data dimensionality reduction.
2. **Feature Prevalence Heatmap**: A heatmap showing the prevalence of selected features across cohorts.
3. **Selected Features**: A list of features selected for contrasting the cohorts.
4. **Dataframe with Selected Features**: A dataframe containing the selected features, ready for further analysis or usage in the Cohort2Trajectory package.
