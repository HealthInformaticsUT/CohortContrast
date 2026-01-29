# =============================================================================
# CohortContrast Viewer - Example Script
# =============================================================================
#
# This script demonstrates how to launch the CohortContrast Viewer dashboard
# using the bundled example data (Prostate cancer summary).
#
# Requirements:
#   - Python 3.8+ installed
#   - CohortContrast package installed
#
# =============================================================================

library(CohortContrast)

# -----------------------------------------------------------------------------
# Step 1: Configure Python Environment
# -----------------------------------------------------------------------------
# Option A: Create a virtual environment (recommended for first-time setup)
configurePython(virtualenv_name = "r-cohortcontrast-viewer")

# Option B: Use system Python without virtual environment (for servers)
# configurePython(python_path = "/usr/bin/python3", create_venv = FALSE)

# -----------------------------------------------------------------------------
# Step 2: Install Python Dependencies
# -----------------------------------------------------------------------------
# Option A: Install from internet (requires network access)
installPythonDeps()

# Option B: Install offline from bundled packages (air-gapped servers)
# installPythonDepsOffline()

# -----------------------------------------------------------------------------
# Step 3: Verify Installation
# -----------------------------------------------------------------------------
checkPythonDeps()

# -----------------------------------------------------------------------------
# Step 4: Get Path to Example Data
# -----------------------------------------------------------------------------
# The example data is a pre-computed summary of Prostate cancer cohort
example_data_dir <- system.file("example", "parquet", package = "CohortContrast")

# Check what's available
cat("Example data directory:", example_data_dir, "\n")
cat("Available studies:\n")
print(list.dirs(example_data_dir, recursive = FALSE, full.names = FALSE))

# -----------------------------------------------------------------------------
# Step 5: Launch the Viewer
# -----------------------------------------------------------------------------
# This will open the dashboard in your default web browser
runCohortContrastViewer(
  data_dir = example_data_dir,
  port = 8050,
  open_browser = TRUE,
  debug = TRUE,
  background = FALSE  # This shows all output inline
)

# -----------------------------------------------------------------------------
# To Stop the Viewer
# -----------------------------------------------------------------------------
# Run this when you're done:
# stopCohortContrastViewer()

# =============================================================================
# Alternative: Quick Start (All-in-One)
# =============================================================================
# If you just want to quickly launch with defaults:
#
# library(CohortContrast)
# configurePython()
# installPythonDeps()
# runCohortContrastViewer(
#   data_dir = system.file("example", "parquet", package = "CohortContrast")
# )
#
# =============================================================================


# Load the package
library(CohortContrast)

# Generate summary with small cell suppression = 5
result <- precomputeSummary(
  study_path = "inst/example/parquet/Prostate_cancer",
  output_path = "inst/example/parquet/Prostate_cancer_summary",
  cluster_k_values = c(2, 3, 4, 5),
  concept_limit = 60,
  min_cell_count = 5  # Counts 1-4 will be reported as 5
)

# View generated files
print(result$files)
