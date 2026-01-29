#' @title CohortContrast Viewer - Interactive Disease Cohort Visualization
#' @description Functions to configure Python environment and launch the CohortContrast Viewer dashboard
#' @name CohortContrastViewer
NULL

# Package-level variables to track state
.ccv_env <- new.env(parent = emptyenv())
.ccv_env$python_configured <- FALSE
.ccv_env$app_process <- NULL
.ccv_env$app_port <- NULL

#' Get the package's Python directory path
#'
#' @return Character string with the path to the Python code
#' @keywords internal
.get_python_dir <- function() {
  # Check multiple possible locations for the Python files

  # 1. Check inst/python in installed package
  pkg_path <- system.file("python", package = "CohortContrast")
  if (pkg_path != "" && dir.exists(pkg_path) && file.exists(file.path(pkg_path, "app.py"))) {
    return(pkg_path)
  }

  # 2. Check inst/python/ folder (development mode)
  if (file.exists("inst/python/app.py")) {
    return(normalizePath("inst/python"))
  }

  # 3. Check custom environment variable
  custom_path <- Sys.getenv("COHORT_CONTRAST_VIEWER_PATH")
  if (custom_path != "" && dir.exists(custom_path) && file.exists(file.path(custom_path, "app.py"))) {
    return(normalizePath(custom_path))
  }

  stop("Could not locate CohortContrast Viewer Python directory. ",
       "Set COHORT_CONTRAST_VIEWER_PATH environment variable or ensure the package is installed.")
}

#' Get the path to the requirements.txt file
#'
#' @return Character string with the path to requirements.txt
#' @keywords internal
.get_requirements_path <- function() {
  python_dir <- .get_python_dir()
  req_path <- file.path(python_dir, "requirements.txt")

  if (!file.exists(req_path)) {
    stop("requirements.txt not found at: ", req_path)
  }

  return(req_path)
}

#' Configure Python Environment for CohortContrast Viewer
#'
#' Sets up the Python environment for running the CohortContrast Viewer dashboard.
#' This function can create a new virtual environment or use an existing Python installation.
#'
#' @param python_path Optional path to a specific Python executable. If NULL,
#'   reticulate will auto-detect Python.
#' @param virtualenv_name Name for the virtual environment. Default is "r-cohortcontrast-viewer".
#' @param create_venv Logical. If TRUE, creates a new virtual environment.
#'   Default is TRUE. Set to FALSE on systems without python3-venv package.
#' @param force Logical. If TRUE, recreates the virtual environment even if it exists.
#'   Default is FALSE.
#'
#' @return Invisibly returns TRUE if configuration was successful.
#'
#' @examples
#' \dontrun{
#' # Use auto-detected Python with a new virtual environment
#' configurePython()
#'
#' # Use a specific Python installation without virtual environment
#' # (useful on servers without python3-venv package)
#' configurePython(python_path = "/usr/bin/python3", create_venv = FALSE)
#'
#' # Use an existing conda environment
#' configurePython(virtualenv_name = "my-conda-env", create_venv = FALSE)
#' }
#'
#' @export
configurePython <- function(python_path = NULL,
                            virtualenv_name = "r-cohortcontrast-viewer",
                            create_venv = TRUE,
                            force = FALSE) {

  message("Configuring Python environment for CohortContrast Viewer...")

  if (create_venv) {
    # Check if virtual environment exists
    venv_exists <- virtualenv_name %in% reticulate::virtualenv_list()

    if (venv_exists && !force) {
      message("Virtual environment '", virtualenv_name, "' already exists. ",
              "Using existing environment. Set force=TRUE to recreate.")
    } else {
      if (venv_exists && force) {
        message("Removing existing virtual environment...")
        reticulate::virtualenv_remove(virtualenv_name, confirm = FALSE)
      }

      message("Creating virtual environment '", virtualenv_name, "'...")

      # Try to create venv, with helpful error message if it fails
      tryCatch({
        if (!is.null(python_path)) {
          reticulate::virtualenv_create(virtualenv_name, python = python_path)
        } else {
          reticulate::virtualenv_create(virtualenv_name)
        }
        message("Virtual environment created successfully.")
      }, error = function(e) {
        if (grepl("venv|ensurepip", e$message, ignore.case = TRUE)) {
          stop("Failed to create virtual environment. The python3-venv package ",
               "may not be installed.\n\n",
               "Solutions:\n",
               "1. Install python3-venv: sudo apt-get install python3-venv\n",
               "2. Use system Python directly:\n",
               "   configurePython(python_path = '/usr/bin/python3', create_venv = FALSE)\n\n",
               "Original error: ", e$message)
        } else {
          stop(e)
        }
      })
    }

    # Use the virtual environment
    reticulate::use_virtualenv(virtualenv_name, required = TRUE)
    .ccv_env$virtualenv_name <- virtualenv_name

  } else {
    # Use system Python directly (no virtual environment)
    if (!is.null(python_path)) {
      message("Using system Python: ", python_path)
      reticulate::use_python(python_path, required = TRUE)
    } else {
      # Try to find Python automatically
      python_path <- Sys.which("python3")
      if (python_path == "") {
        python_path <- Sys.which("python")
      }
      if (python_path != "") {
        message("Using system Python: ", python_path)
        reticulate::use_python(python_path, required = TRUE)
      } else {
        stop("Could not find Python. Please specify python_path parameter.")
      }
    }
    .ccv_env$virtualenv_name <- NULL
  }

  .ccv_env$python_configured <- TRUE

  message("Python configuration complete.")
  message("Python version: ", reticulate::py_config()$version)
  message("Python path: ", reticulate::py_config()$python)

  invisible(TRUE)
}

#' Get Python Configuration Information
#'
#' Returns information about the current Python configuration for the CohortContrast Viewer.
#'
#' @return A list with Python configuration details
#'
#' @examples
#' \dontrun{
#' getPythonInfo()
#' }
#'
#' @export
getPythonInfo <- function() {
  config <- reticulate::py_config()

  list(
    python_version = config$version,
    python_path = config$python,
    virtualenv = if (exists("virtualenv_name", envir = .ccv_env)) .ccv_env$virtualenv_name else NA,
    configured = .ccv_env$python_configured,
    numpy_available = reticulate::py_module_available("numpy"),
    pandas_available = reticulate::py_module_available("pandas"),
    dash_available = reticulate::py_module_available("dash")
  )
}

#' Install Python Dependencies
#'
#' Installs the required Python packages for the CohortContrast Viewer.
#'
#' @param upgrade Logical. If TRUE, upgrades existing packages. Default is FALSE.
#' @param quiet Logical. If TRUE, suppresses pip output. Default is FALSE.
#' @param user Logical. If TRUE, installs to user site-packages (--user flag).
#'   Useful when not using a virtual environment. Default is NULL (auto-detect).
#'
#' @return Invisibly returns TRUE if installation was successful.
#'
#' @examples
#' \dontrun{
#' # First configure Python
#' configurePython()
#'
#' # Then install dependencies
#' installPythonDeps()
#'
#' # Install to user directory (no admin rights needed)
#' installPythonDeps(user = TRUE)
#' }
#'
#' @export
installPythonDeps <- function(upgrade = FALSE, quiet = FALSE, user = NULL) {

  if (!.ccv_env$python_configured) {
    message("Python not configured. Running configurePython() first...")
    configurePython()
  }

  req_path <- .get_requirements_path()
  message("Installing Python dependencies from: ", req_path)

  # Auto-detect if we should use --user flag
  # Use --user when not in a virtual environment
  if (is.null(user)) {
    user <- is.null(.ccv_env$virtualenv_name)
    if (user) {
      message("No virtual environment detected. Installing to user directory (--user).")
    }
  }

  # Build pip options
  pip_options <- c("-r", req_path)
  if (upgrade) pip_options <- c(pip_options, "--upgrade")
  if (user) pip_options <- c(pip_options, "--user")

  # Use reticulate to run pip
  tryCatch({
    reticulate::py_install(
      packages = NULL,
      pip = TRUE,
      pip_options = pip_options
    )
    message("Python dependencies installed successfully.")
    invisible(TRUE)
  }, error = function(e) {
    # Fallback: use system pip directly
    message("Trying alternative installation method...")
    python_path <- reticulate::py_config()$python
    pip_cmd <- paste(python_path, "-m pip install -r", shQuote(req_path))
    if (upgrade) pip_cmd <- paste(pip_cmd, "--upgrade")
    if (user) pip_cmd <- paste(pip_cmd, "--user")

    result <- system(pip_cmd, intern = FALSE)
    if (result != 0) {
      stop("Failed to install Python dependencies.\n",
           "Try running manually: ", pip_cmd, "\n",
           "Original error: ", e$message)
    }
    message("Python dependencies installed successfully.")
    invisible(TRUE)
  })
}

#' Check Python Dependencies
#'
#' Checks if all required Python packages are installed.
#'
#' @return A data frame with package names and their installation status.
#'
#' @examples
#' \dontrun{
#' checkPythonDeps()
#' }
#'
#' @export
checkPythonDeps <- function() {
  required_packages <- c(
    "dash",
    "dash_bootstrap_components",
    "dash_ag_grid",
    "pandas",
    "numpy",
    "plotly",
    "scipy",
    "sklearn",
    "sklearn_extra",
    "diskcache"
  )

  status <- sapply(required_packages, function(pkg) {
    # Handle underscore vs hyphen in package names
    pkg_check <- gsub("_", "-", pkg)
    reticulate::py_module_available(pkg) || reticulate::py_module_available(pkg_check)
  })

  result <- data.frame(
    package = required_packages,
    installed = status,
    stringsAsFactors = FALSE
  )

  missing <- result[!result$installed, "package"]
  if (length(missing) > 0) {
    message("Missing packages: ", paste(missing, collapse = ", "))
    message("Run installPythonDeps() to install missing packages.")
  } else {
    message("All required Python packages are installed.")
  }

  return(result)
}

#' Install Python Dependencies Offline
#'
#' Installs Python packages from pre-downloaded wheel files (offline installation).
#' This is useful for air-gapped servers without internet access.
#'
#' @param packages_dir Path to directory containing wheel files or a .zip archive.
#'   If NULL, uses the bundled packages in the CohortContrast package.
#' @param platform Platform identifier. Default is "linux_x86_64".
#' @param cleanup Logical. If TRUE, removes extracted files after installation.
#'   Default is TRUE.
#'
#' @return Invisibly returns TRUE if installation was successful.
#'
#' @details
#' The function supports two formats:
#' \itemize{
#'   \item A .zip file (e.g., "linux_x86_64.zip") containing wheel files
#'   \item A directory containing .whl files directly
#' }
#'
#' For offline installation:
#' \enumerate{
#'   \item Download wheel files on a machine with internet access
#'   \item Copy the packages folder to the offline server
#'   \item Run this function to install from local files
#' }
#'
#' @examples
#' \dontrun{
#' # Install from bundled packages
#' configurePython(create_venv = FALSE)
#' installPythonDepsOffline()
#'
#' # Install from custom location
#' installPythonDepsOffline(packages_dir = "/path/to/packages")
#' }
#'
#' @export
installPythonDepsOffline <- function(packages_dir = NULL,
                                     platform = "linux_x86_64",
                                     cleanup = TRUE) {

  if (!.ccv_env$python_configured) {
    message("Python not configured. Running configurePython() first...")
    configurePython(create_venv = FALSE)
  }

  # Find packages directory
  if (is.null(packages_dir)) {
    # Check bundled packages in the installed package
    pkg_packages <- system.file("packages", package = "CohortContrast")
    if (pkg_packages == "") {
      # Development mode - check relative path
      if (dir.exists("packages")) {
        packages_dir <- normalizePath("packages")
      } else if (dir.exists("inst/packages")) {
        packages_dir <- normalizePath("inst/packages")
      } else {
        stop("Could not find packages directory. Please specify packages_dir parameter.")
      }
    } else {
      packages_dir <- pkg_packages
    }
  }

  packages_dir <- normalizePath(packages_dir, mustWork = TRUE)
  message("Looking for packages in: ", packages_dir)

  # Determine the source (zip or folder)
  zip_file <- file.path(packages_dir, paste0(platform, ".zip"))
  folder_path <- file.path(packages_dir, platform)
  temp_extract_dir <- NULL

  if (file.exists(zip_file)) {
    message("Found compressed packages: ", basename(zip_file))
    message("Extracting...")

    # Extract to temp directory
    temp_extract_dir <- file.path(tempdir(), paste0("ccv_packages_", Sys.getpid()))
    dir.create(temp_extract_dir, showWarnings = FALSE, recursive = TRUE)

    utils::unzip(zip_file, exdir = temp_extract_dir)
    wheel_dir <- temp_extract_dir

    wheel_files <- list.files(wheel_dir, pattern = "\\.whl$", full.names = TRUE)
    message("Extracted ", length(wheel_files), " wheel files")

  } else if (dir.exists(folder_path)) {
    message("Found packages folder: ", platform)
    wheel_dir <- folder_path
    wheel_files <- list.files(wheel_dir, pattern = "\\.whl$", full.names = TRUE)
    message("Found ", length(wheel_files), " wheel files")

  } else {
    stop("No packages found!\n",
         "Expected: ", zip_file, "\n",
         "Or: ", folder_path, "\n",
         "Download packages first or specify packages_dir parameter.")
  }

  if (length(wheel_files) == 0) {
    stop("No .whl files found in: ", wheel_dir)
  }

  # Install using pip with --find-links
  message("Installing packages offline...")

  python_path <- reticulate::py_config()$python
  pip_cmd <- paste(
    python_path, "-m pip install --user --no-index",
    "--find-links", shQuote(wheel_dir),
    "dash dash-bootstrap-components dash-ag-grid",
    "pandas pyarrow numpy scipy",
    "plotly diskcache psutil multiprocess",
    "scikit-learn scikit-learn-extra"
  )

  result <- system(pip_cmd, intern = FALSE)

  # Cleanup temp directory
  if (!is.null(temp_extract_dir) && cleanup && dir.exists(temp_extract_dir)) {
    message("Cleaning up temporary files...")
    unlink(temp_extract_dir, recursive = TRUE)
  }

  if (result != 0) {
    stop("Installation failed. Check the error messages above.")
  }

  message("Offline installation complete!")
  invisible(TRUE)
}

#' Run CohortContrast Viewer Dashboard
#'
#' Launches the CohortContrast Viewer interactive dashboard.
#'
#' @param data_dir Path to the directory containing parquet data files.
#'   If NULL, uses the default results_parquet directory in the package.
#' @param port Port number for the Dash server. Default is 8050.
#' @param host Host address. Default is "127.0.0.1" (localhost).
#' @param debug Logical. If TRUE, runs in debug mode. Default is FALSE.
#' @param open_browser Logical. If TRUE, opens the dashboard in the default browser.
#'   Default is TRUE.
#' @param background Logical. If TRUE, runs the server in the background.
#'   Default is TRUE.
#'
#' @return If background is TRUE, invisibly returns the process object.
#'   If FALSE, blocks until the server is stopped.
#'
#' @examples
#' \dontrun{
#' # Configure and launch with defaults
#' configurePython()
#' installPythonDeps()
#' runCohortContrastViewer()
#'
#' # Launch with custom data directory
#' runCohortContrastViewer(data_dir = "/path/to/my/data")
#'
#' # Launch on a different port
#' runCohortContrastViewer(port = 8080)
#' }
#'
#' @export
runCohortContrastViewer <- function(data_dir = NULL,
                                port = 8050,
                                host = "127.0.0.1",
                                debug = FALSE,
                                open_browser = TRUE,
                                background = TRUE) {

  # Check Python configuration
  if (!.ccv_env$python_configured) {
    message("Python not configured. Running configurePython() first...")
    configurePython()
  }

  # Check if app is already running
  if (!is.null(.ccv_env$app_process) && .ccv_env$app_process$is_alive()) {
    message("CohortContrast Viewer is already running on port ", .ccv_env$app_port)
    message("URL: http://", host, ":", .ccv_env$app_port)
    message("Use stopCohortContrastViewer() to stop the current instance.")
    return(invisible(.ccv_env$app_process))
  }

  python_dir <- .get_python_dir()
  app_path <- file.path(python_dir, "app.py")

  if (!file.exists(app_path)) {
    stop("app.py not found at: ", app_path)
  }

  # Set environment variables for the app
  env_vars <- list(
    CONTRAST_VIEWER_PORT = as.character(port),
    CONTRAST_VIEWER_HOST = host,
    CONTRAST_VIEWER_DEBUG = if (debug) "1" else "0"
  )

  if (!is.null(data_dir)) {
    if (!dir.exists(data_dir)) {
      stop("Data directory does not exist: ", data_dir)
    }
    env_vars$CONTRAST_VIEWER_DATA_DIR <- normalizePath(data_dir)
  }

  message("Starting CohortContrast Viewer...")
  message("App directory: ", python_dir)
  message("Port: ", port)

  # Get Python path

  python_path <- reticulate::py_config()$python

  if (background) {
    # Run in background using processx
    if (!requireNamespace("processx", quietly = TRUE)) {
      message("Installing 'processx' package for background execution...")
      utils::install.packages("processx", repos = "https://cloud.r-project.org")
    }

    # Set up environment
    env <- Sys.getenv()
    for (name in names(env_vars)) {
      env[name] <- env_vars[[name]]
    }

    .ccv_env$app_process <- processx::process$new(
      command = python_path,
      args = c(app_path),
      wd = python_dir,
      env = env,
      stdout = "|",
      stderr = "|"
    )

    .ccv_env$app_port <- port

    # Wait a moment for the server to start
    Sys.sleep(2)

    if (.ccv_env$app_process$is_alive()) {
      url <- paste0("http://", host, ":", port)
      message("CohortContrast Viewer is running!")
      message("URL: ", url)

      if (open_browser) {
        utils::browseURL(url)
      }

      message("\nUse stopCohortContrastViewer() to stop the server.")
      return(invisible(.ccv_env$app_process))
    } else {
      # Check for errors
      stderr_output <- .ccv_env$app_process$read_error()
      stop("Failed to start CohortContrast Viewer. Error: ", stderr_output)
    }

  } else {
    # Run in foreground (blocking)
    message("Running in foreground mode. Press Ctrl+C to stop.")

    # Set environment variables
    old_env <- Sys.getenv(names(env_vars), unset = NA)
    do.call(Sys.setenv, env_vars)

    on.exit({
      # Restore old environment
      for (name in names(old_env)) {
        if (is.na(old_env[name])) {
          Sys.unsetenv(name)
        } else {
          do.call(Sys.setenv, as.list(setNames(old_env[name], name)))
        }
      }
    })

    if (open_browser) {
      # Schedule browser opening
      later::later(function() {
        utils::browseURL(paste0("http://", host, ":", port))
      }, delay = 2)
    }

    # Run the app
    system2(python_path, args = app_path, wait = TRUE)
  }
}

#' Stop CohortContrast Viewer Dashboard
#'
#' Stops a running CohortContrast Viewer dashboard instance.
#'
#' @return Invisibly returns TRUE if the server was stopped.
#'
#' @examples
#' \dontrun{
#' stopCohortContrastViewer()
#' }
#'
#' @export
stopCohortContrastViewer <- function() {
  if (is.null(.ccv_env$app_process)) {
    message("No CohortContrast Viewer instance is running.")
    return(invisible(FALSE))
  }

  if (!.ccv_env$app_process$is_alive()) {
    message("CohortContrast Viewer has already stopped.")
    .ccv_env$app_process <- NULL
    .ccv_env$app_port <- NULL
    return(invisible(FALSE))
  }

  message("Stopping CohortContrast Viewer...")
  .ccv_env$app_process$kill()

  # Wait for process to stop
  Sys.sleep(1)

  if (!.ccv_env$app_process$is_alive()) {
    message("CohortContrast Viewer stopped successfully.")
    .ccv_env$app_process <- NULL
    .ccv_env$app_port <- NULL
    return(invisible(TRUE))
  } else {
    warning("Failed to stop CohortContrast Viewer gracefully. Forcing termination...")
    .ccv_env$app_process$kill_tree()
    .ccv_env$app_process <- NULL
    .ccv_env$app_port <- NULL
    return(invisible(TRUE))
  }
}


#' Pre-compute Summary Data for a Study
#'
#' Generates aggregated summary data from patient-level parquet files,
#' removing all individual patient information. This creates a "summary mode"
#' dataset that can be shared without privacy concerns.
#'
#' The function pre-computes:
#' - Concept-level time distribution statistics (for violin/box plots)
#' - Age and gender statistics per concept
#' - Ordinal concept summaries (1st, 2nd, 3rd occurrences)
#' - Clustering results for k=2,3,4,5 clusters
#' - Cluster overlap and differentiation metrics
#'
#' @param study_path Path to directory containing patient-level parquet files
#'   (data_patients.parquet, data_features.parquet, etc.)
#' @param output_path Path for output summary files. Default is study_path + "_summary"
#' @param cluster_k_values Vector of k values to pre-compute clustering for.
#'   Default is c(2, 3, 4, 5).
#' @param concept_limit Maximum number of concepts to use for clustering.
#'   Default is 60.
#' @param min_cell_count Minimum patient count for small cell suppression (privacy).
#'   Counts between 1 and (min_cell_count-1) are rounded up to min_cell_count.
#'   Default is 0 (disabled). Set to e.g. 5 to apply suppression.
#' @param max_parallel_jobs Maximum number of parallel clustering jobs.
#'   Default is 1 (sequential) to avoid out-of-memory errors on servers.
#'   Set to 2-4 on machines with ample RAM for faster execution.
#'
#' @return A list with:
#'   \item{output_path}{Path to the generated summary directory}
#'   \item{files}{Named list of generated file paths}
#'   \item{metadata}{Study metadata including demographics and clustering info}
#'
#' @examples
#' \dontrun{
#' # Generate summary data for a study (no suppression)
#' result <- precomputeSummary(
#'   study_path = "results_parquet/Breast_cancer",
#'   output_path = "summaries/Breast_cancer"
#' )
#'
#' # With small cell suppression (counts 1-4 become 5)
#' result <- precomputeSummary(
#'   study_path = "results_parquet/Breast_cancer",
#'   min_cell_count = 5
#' )
#'
#' # View generated files
#' print(result$files)
#'
#' # Run viewer with summary data (no patient data needed)
#' runCohortContrastViewer(data_dir = "summaries")
#' }
#'
#' @export
precomputeSummary <- function(study_path,
                               output_path = NULL,
                               cluster_k_values = c(2, 3, 4, 5),
                               concept_limit = 60,
                               min_cell_count = 0,
                               max_parallel_jobs = 1) {

  # Check Python configuration
  if (!.ccv_env$python_configured) {
    message("Python not configured. Running configurePython() first...")
    configurePython()
  }

  # Validate study path
  study_path <- normalizePath(study_path, mustWork = TRUE)

  if (!file.exists(file.path(study_path, "data_patients.parquet"))) {
    stop("data_patients.parquet not found in: ", study_path)
  }

  # Get Python directory
  python_dir <- .get_python_dir()

  # Build Python command
  python_path <- reticulate::py_config()$python

  # Create the Python script call
    # Handle concept_limit = Inf (use None in Python to mean "all concepts")
  concept_limit_py <- if (is.infinite(concept_limit)) "None" else as.character(as.integer(concept_limit))

  script_code <- sprintf('
import sys
sys.path.insert(0, "%s")
from precompute.summarize import precompute_study_summary
import json

result = precompute_study_summary(
    study_path="%s",
    output_path=%s,
    cluster_k_values=%s,
    concept_limit=%s,
    min_cell_count=%d,
    max_parallel_jobs=%d
)

# Print result as JSON for R to parse
print("__RESULT_START__")
print(json.dumps(result, default=str))
print("__RESULT_END__")
',
    python_dir,
    study_path,
    if (is.null(output_path)) "None" else sprintf('"%s"', output_path),
    paste0("[", paste(cluster_k_values, collapse = ", "), "]"),
    concept_limit_py,
    as.integer(min_cell_count),
    as.integer(max_parallel_jobs)
  )

  message("Pre-computing summary data for: ", study_path)
  message("This may take a few minutes for large datasets...")

  # Run Python script
  result <- tryCatch({
    output <- system2(
      python_path,
      args = c("-c", shQuote(script_code)),
      stdout = TRUE,
      stderr = TRUE
    )

    # Find and parse JSON result
    start_idx <- which(output == "__RESULT_START__")
    end_idx <- which(output == "__RESULT_END__")

    if (length(start_idx) > 0 && length(end_idx) > 0) {
      json_str <- paste(output[(start_idx + 1):(end_idx - 1)], collapse = "\n")
      result <- jsonlite::fromJSON(json_str)

      # Print progress messages (everything before __RESULT_START__)
      if (start_idx > 1) {
        cat(paste(output[1:(start_idx - 1)], collapse = "\n"), "\n")
      }

      result
    } else {
      # Print all output if parsing fails
      cat(paste(output, collapse = "\n"), "\n")
      stop("Failed to parse Python output")
    }
  }, error = function(e) {
    stop("Pre-computation failed: ", e$message)
  })

  message("\nSummary generation complete!")
  message("Output directory: ", result$output_path)
  message("Files generated: ", length(result$files))

  return(result)
}


#' Check if a Data Directory Contains Summary Mode Data
#'
#' Determines whether a data directory contains pre-computed summary data
#' (summary mode) or patient-level data (patient mode).
#'
#' @param data_dir Path to the data directory
#'
#' @return A list with:
#'   \item{mode}{"summary" or "patient"}
#'   \item{has_clustering}{Logical, whether clustering data is available}
#'   \item{cluster_k_values}{Vector of available k values for clustering}
#'
#' @examples
#' \dontrun{
#' info <- checkDataMode("results_parquet/Breast_cancer")
#' print(info$mode)  # "patient" or "summary"
#' }
#'
#' @export
checkDataMode <- function(data_dir) {
  data_dir <- normalizePath(data_dir, mustWork = TRUE)

  # Check for summary mode indicators
  has_metadata <- file.exists(file.path(data_dir, "metadata.json"))
  has_concept_summaries <- file.exists(file.path(data_dir, "concept_summaries.parquet"))
  has_data_patients <- file.exists(file.path(data_dir, "data_patients.parquet"))

  if (has_metadata && has_concept_summaries) {
    # Read metadata
    metadata <- jsonlite::fromJSON(file.path(data_dir, "metadata.json"))

    # Find available clustering k values
    clustering_files <- list.files(data_dir, pattern = "clustering_k\\d+_summary\\.parquet")
    k_values <- as.integer(gsub("clustering_k(\\d+)_summary\\.parquet", "\\1", clustering_files))

    return(list(
      mode = "summary",
      has_clustering = length(k_values) > 0,
      cluster_k_values = sort(k_values),
      demographics = metadata$demographics,
      metadata = metadata
    ))
  } else if (has_data_patients) {
    return(list(
      mode = "patient",
      has_clustering = TRUE,  # Can compute on-the-fly
      cluster_k_values = 2:10,  # Any k is possible
      demographics = NULL,
      metadata = NULL
    ))
  } else {
    stop("Invalid data directory: neither summary nor patient data found")
  }
}


#' Convert RDS Results to Parquet Format
#'
#' Converts CohortContrastObject RDS files to the parquet folder structure
#' required by the CohortContrast Viewer.
#'
#' @param input_dir Path to directory containing .rds and .csv files (e.g., "results/")
#' @param output_dir Path where parquet folders should be created (e.g., "results_parquet/")
#' @param studies Optional character vector of study names to convert. If NULL,
#'   converts all .rds files found in input_dir.
#' @param overwrite Logical. If TRUE, overwrites existing study folders. Default FALSE.
#'
#' @return Invisibly returns a list of converted study paths.
#'
#' @details
#' For each RDS file (e.g., "Breast_cancer.rds"), this function:
#' \itemize{
#'   \item Creates a subfolder in output_dir (e.g., "Breast_cancer/")
#'   \item Extracts data frames from the CohortContrastObject
#'   \item Writes each as a parquet file (data_patients.parquet, etc.)
#'   \item Copies the corresponding CSV file if present
#' }
#'
#' @examples
#' \dontrun{
#' # Convert all RDS files in results/ to results_parquet/
#' convertRdsToParquet(
#'   input_dir = "results",
#'   output_dir = "results_parquet"
#' )
#'
#' # Convert a specific study
#' convertRdsToParquet(
#'   input_dir = "results",
#'   output_dir = "results_parquet",
#'   studies = "Breast_cancer"
#' )
#' }
#'
#' @export
convertRdsToParquet <- function(input_dir,
                                   output_dir,
                                   studies = NULL,
                                   overwrite = FALSE) {

  # Check that nanoparquet is available
  if (!requireNamespace("nanoparquet", quietly = TRUE)) {
    stop("Package 'nanoparquet' is required for converting RDS to parquet.\n",
         "Install it with: install.packages('nanoparquet')")
  }

  # Normalize paths
  input_dir <- normalizePath(input_dir, mustWork = TRUE)

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }
  output_dir <- normalizePath(output_dir)

  # Find RDS files
  rds_files <- list.files(input_dir, pattern = "\\.rds$", full.names = TRUE)

  if (length(rds_files) == 0) {
    stop("No .rds files found in: ", input_dir)
  }

  # Filter to specific studies if requested
  if (!is.null(studies)) {
    rds_files <- rds_files[tools::file_path_sans_ext(basename(rds_files)) %in% studies]
    if (length(rds_files) == 0) {
      stop("None of the specified studies found in: ", input_dir,
           "\nAvailable: ", paste(tools::file_path_sans_ext(basename(
             list.files(input_dir, pattern = "\\.rds$")
           )), collapse = ", "))
    }
  }

  message("Found ", length(rds_files), " study/studies to convert")

  converted_paths <- list()

  for (rds_file in rds_files) {
    study_name <- tools::file_path_sans_ext(basename(rds_file))
    study_output_dir <- file.path(output_dir, study_name)

    message("\n", strrep("=", 50))
    message("Converting: ", study_name)
    message(strrep("=", 50))

    # Check if already exists
    if (dir.exists(study_output_dir)) {
      if (!overwrite) {
        message("  Skipping (already exists, use overwrite=TRUE to replace)")
        next
      } else {
        message("  Removing existing folder...")
        unlink(study_output_dir, recursive = TRUE)
      }
    }

    # Create study folder
    dir.create(study_output_dir, recursive = TRUE)

    # Load RDS file
    message("  Loading RDS file...")
    data <- readRDS(rds_file)

    # Validate it's a CohortContrastObject or similar list
    if (!is.list(data)) {
      warning("  Skipping - not a list/object: ", rds_file)
      next
    }

    # Expected data frames to extract
    parquet_components <- c(
      "data_patients",
      "data_initial",
      "data_person",
      "data_features",
      "complementaryMappingTable"
    )

    # Helper function to prepare data frame for nanoparquet
    # nanoparquet cannot handle:
    # 1. List columns - we serialize them to JSON
    # 2. integer64 columns - we convert to numeric (double)
    prepare_for_nanoparquet <- function(df) {
      # Check if any column is integer64 and load bit64 if needed
      has_integer64 <- any(vapply(df, inherits, logical(1), "integer64"))
      if (has_integer64) {
        if (requireNamespace("bit64", quietly = TRUE)) {
          # Must actually load the package for as.double.integer64 to work
          requireNamespace("bit64", quietly = TRUE)
          # Force loading of the bit64 namespace methods
          if (!isNamespaceLoaded("bit64")) {
            loadNamespace("bit64")
          }
        } else {
          warning("Package 'bit64' not available, integer64 conversion may fail")
        }
      }
      
      for (col_name in names(df)) {
        col <- df[[col_name]]
        
        # Check if column is a list (contains vectors/arrays)
        if (is.list(col) && !is.data.frame(col)) {
          # Convert each element to JSON string
          df[[col_name]] <- vapply(col, function(x) {
            if (is.null(x) || (length(x) == 1 && is.na(x))) {
              NA_character_
            } else {
              jsonlite::toJSON(x, auto_unbox = FALSE)
            }
          }, character(1))
        }
        
        # Check if column is integer64 (from bit64 package)
        # nanoparquet doesn't handle integer64 properly
        # Use bit64::as.double.integer64() which correctly converts
        if (inherits(col, "integer64")) {
          # Use bit64's method directly if available
          if (isNamespaceLoaded("bit64")) {
            df[[col_name]] <- bit64::as.double.integer64(col)
          } else {
            # Fallback: convert via character to avoid corruption
            df[[col_name]] <- as.numeric(as.character(col))
          }
        }
      }
      df
    }

    # Write each data frame as parquet
    # Uses nanoparquet instead of arrow for environments where arrow is not available
    # List columns are serialized to JSON strings for compatibility
    for (component in parquet_components) {
      if (component %in% names(data)) {
        df <- data[[component]]
        if (is.data.frame(df)) {
          # Skip empty data frames - nanoparquet cannot handle 0-row data frames
          # (causes "wrong sign in 'by' argument" error in seq.default)
          if (nrow(df) == 0) {
            message("  ⚠ ", component, " has 0 rows, skipping")
            next
          }
          parquet_path <- file.path(study_output_dir, paste0(component, ".parquet"))
          # Prepare data frame (serialize list columns to JSON)
          df_prepared <- prepare_for_nanoparquet(df)
          nanoparquet::write_parquet(df_prepared, parquet_path)
          message("  ✓ ", component, ".parquet (", nrow(df), " rows)")
        } else {
          message("  ⚠ ", component, " is not a data frame, skipping")
        }
      } else {
        message("  ⚠ ", component, " not found in RDS")
      }
    }

    # Copy CSV file if it exists
    csv_file <- file.path(input_dir, paste0(study_name, ".csv"))
    if (file.exists(csv_file)) {
      file.copy(csv_file, file.path(study_output_dir, paste0(study_name, ".csv")))
      message("  ✓ ", study_name, ".csv (copied)")
    } else {
      # Create a minimal CSV if not present
      csv_content <- data.frame(
        study_name = study_name,
        created = Sys.time()
      )
      utils::write.csv(csv_content,
                       file.path(study_output_dir, paste0(study_name, ".csv")),
                       row.names = FALSE)
      message("  ✓ ", study_name, ".csv (created)")
    }

    converted_paths[[study_name]] <- study_output_dir
    message("  ✓ Complete: ", study_output_dir)
  }

  message("\n", strrep("=", 50))
  message("Conversion complete!")
  message("Output directory: ", output_dir)
  message(strrep("=", 50))

  invisible(converted_paths)
}

