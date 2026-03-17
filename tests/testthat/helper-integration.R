skip_if_no_integration <- function() {
  enabled <- tolower(Sys.getenv("COHORTCONTRAST_RUN_INTEGRATION", "false"))
  if (!identical(enabled, "true")) {
    testthat::skip("Integration tests disabled. Set COHORTCONTRAST_RUN_INTEGRATION=true to run.")
  }
}
