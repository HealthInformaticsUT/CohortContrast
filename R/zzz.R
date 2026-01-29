#' @keywords internal
.onLoad <- function(libname, pkgname) {
  # Set default reticulate options for the CohortContrast Viewer

  options(
    reticulate.conda_binary = NULL  # Let reticulate find conda automatically
  )
}

#' @keywords internal
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "CohortContrast - Cohort Feature Selection and Visualization\n",
    "For the interactive dashboard:\n",
    "  - Use configurePython() to set up the Python environment.\n",
    "  - Use runCohortContrastViewer() to launch the dashboard."
  )
}
