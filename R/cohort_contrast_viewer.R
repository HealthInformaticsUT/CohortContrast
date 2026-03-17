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
.getPythonDir <- function() {
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
.getRequirementsPath <- function() {
  python_dir <- .getPythonDir()
  req_path <- file.path(python_dir, "requirements.txt")

  if (!file.exists(req_path)) {
    stop("requirements.txt not found at: ", req_path)
  }

  return(req_path)
}

#' @keywords internal
.get_python_dir <- function() {
  .getPythonDir()
}

#' @keywords internal
.get_requirements_path <- function() {
  .getRequirementsPath()
}

#' @keywords internal
.resolveLegacyArgs <- function(dots, mapping, explicitNew, fnName) {
  resolved <- list()
  for (oldName in names(mapping)) {
    newName <- unname(mapping[[oldName]])
    if (!is.null(dots[[oldName]])) {
      if (isTRUE(explicitNew[[newName]])) {
        stop(
          "Both `", newName, "` and deprecated `", oldName, "` were provided in `",
          fnName, "()`.",
          call. = FALSE
        )
      }
      resolved[[newName]] <- dots[[oldName]]
      warning(
        "`", oldName, "` is deprecated in `", fnName, "()`; use `", newName, "` instead.",
        call. = FALSE
      )
      dots[[oldName]] <- NULL
    }
  }
  if (length(dots) > 0) {
    stop(
      "Unknown argument(s) in `", fnName, "()`",
      ": ",
      paste(names(dots), collapse = ", "),
      call. = FALSE
    )
  }
  resolved
}

#' Configure Python Environment for CohortContrast Viewer
#'
#' Sets up the Python environment for running the CohortContrast Viewer dashboard.
#' This function can create a new virtual environment or use an existing Python installation.
#'
#' @param pythonPath Optional path to a specific Python executable. If NULL,
#'   reticulate will auto-detect Python.
#' @param virtualenvName Name for the virtual environment. Default is "r-cohortcontrast-viewer".
#' @param createVenv Logical. If TRUE, creates a new virtual environment.
#'   Default is TRUE. Set to FALSE on systems without python3-venv package.
#' @param force Logical. If TRUE, recreates the virtual environment even if it exists.
#'   Default is FALSE.
#' @param ... Backward-compatible aliases:
#'   `python_path`, `virtualenv_name`, `create_venv`.
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
#' configurePython(pythonPath = "/usr/bin/python3", createVenv = FALSE)
#'
#' # Use an existing conda environment
#' configurePython(virtualenvName = "my-conda-env", createVenv = FALSE)
#' }
#'
#' @export
configurePython <- function(pythonPath = NULL,
                            virtualenvName = "r-cohortcontrast-viewer",
                            createVenv = TRUE,
                            force = FALSE,
                            ...) {

  legacy <- .resolveLegacyArgs(
    dots = list(...),
    mapping = c(
      python_path = "pythonPath",
      virtualenv_name = "virtualenvName",
      create_venv = "createVenv"
    ),
    explicitNew = c(
      pythonPath = !missing(pythonPath),
      virtualenvName = !missing(virtualenvName),
      createVenv = !missing(createVenv)
    ),
    fnName = "configurePython"
  )
  if (!is.null(legacy$pythonPath)) pythonPath <- legacy$pythonPath
  if (!is.null(legacy$virtualenvName)) virtualenvName <- legacy$virtualenvName
  if (!is.null(legacy$createVenv)) createVenv <- legacy$createVenv

  message("Configuring Python environment for CohortContrast Viewer...")

  if (createVenv) {
    # Check if virtual environment exists
    venv_exists <- virtualenvName %in% reticulate::virtualenv_list()

    if (venv_exists && !force) {
      message("Virtual environment '", virtualenvName, "' already exists. ",
              "Using existing environment. Set force=TRUE to recreate.")
    } else {
      if (venv_exists && force) {
        message("Removing existing virtual environment...")
        reticulate::virtualenv_remove(virtualenvName, confirm = FALSE)
      }

      message("Creating virtual environment '", virtualenvName, "'...")

      # Try to create venv, with helpful error message if it fails
      tryCatch({
        if (!is.null(pythonPath)) {
          reticulate::virtualenv_create(virtualenvName, python = pythonPath)
        } else {
          reticulate::virtualenv_create(virtualenvName)
        }
        message("Virtual environment created successfully.")
      }, error = function(e) {
        if (grepl("venv|ensurepip", e$message, ignore.case = TRUE)) {
          stop("Failed to create virtual environment. The python3-venv package ",
               "may not be installed.\n\n",
               "Solutions:\n",
               "1. Install python3-venv: sudo apt-get install python3-venv\n",
               "2. Use system Python directly:\n",
               "   configurePython(pythonPath = '/usr/bin/python3', createVenv = FALSE)\n\n",
               "Original error: ", e$message)
        } else {
          stop(e)
        }
      })
    }

    # Use the virtual environment
    reticulate::use_virtualenv(virtualenvName, required = TRUE)
    .ccv_env$virtualenv_name <- virtualenvName

  } else {
    # Use system Python directly (no virtual environment)
    if (!is.null(pythonPath)) {
      message("Using system Python: ", pythonPath)
      reticulate::use_python(pythonPath, required = TRUE)
    } else {
      # Try to find Python automatically
      pythonPath <- Sys.which("python3")
      if (pythonPath == "") {
        pythonPath <- Sys.which("python")
      }
      if (pythonPath != "") {
        message("Using system Python: ", pythonPath)
        reticulate::use_python(pythonPath, required = TRUE)
      } else {
        stop("Could not find Python. Please specify `pythonPath`.")
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
    pythonPath = config$python,
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

  req_path <- .getRequirementsPath()
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
#' @param packagesDir Path to directory containing wheel files or a .zip archive.
#'   This argument is required in CRAN builds.
#' @param platform Platform identifier. Default is "linux_x86_64".
#' @param cleanup Logical. If TRUE, removes extracted files after installation.
#'   Default is TRUE.
#' @param ... Backward-compatible alias: `packages_dir`.
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
#' # Install from custom location
#' configurePython(createVenv = FALSE)
#' installPythonDepsOffline(packagesDir = "/path/to/packages")
#' }
#'
#' @export
installPythonDepsOffline <- function(packagesDir = NULL,
                                     platform = "linux_x86_64",
                                     cleanup = TRUE,
                                     ...) {

  legacy <- .resolveLegacyArgs(
    dots = list(...),
    mapping = c(packages_dir = "packagesDir"),
    explicitNew = c(packagesDir = !missing(packagesDir)),
    fnName = "installPythonDepsOffline"
  )
  if (!is.null(legacy$packagesDir)) packagesDir <- legacy$packagesDir

  if (!.ccv_env$python_configured) {
    message("Python not configured. Running configurePython() first...")
    configurePython(createVenv = FALSE)
  }

  if (is.null(packagesDir)) {
    stop(
      "`packagesDir` is required for offline installation. ",
      "Provide a directory containing either `", platform, ".zip` or `", platform, "/*.whl`."
    )
  }

  packagesDir <- normalizePath(packagesDir, mustWork = TRUE)
  message("Looking for packages in: ", packagesDir)

  # Determine the source (zip or folder)
  zip_file <- file.path(packagesDir, paste0(platform, ".zip"))
  folder_path <- file.path(packagesDir, platform)
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
         "Download packages first or specify `packagesDir`.")
  }

  if (length(wheel_files) == 0) {
    stop("No .whl files found in: ", wheel_dir)
  }

  # Install using pip with --find-links
  message("Installing packages offline...")

  pythonPath <- reticulate::py_config()$python
  pip_cmd <- paste(
    pythonPath, "-m pip install --user --no-index",
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
#' @param dataDir Path to the directory containing parquet data files.
#'   If NULL, defaults to the current working directory (`getwd()`).
#' @param port Port number for the Dash server. Default is 8050.
#' @param host Host address. Default is "127.0.0.1" (localhost).
#' @param mode Run mode. One of `"simple"`, `"server"`, `"debug"`.
#'   `"simple"` hides debug-style output and is the default.
#'   `"server"` enables file logging suitable for hosted/server runs.
#'   `"debug"` enables maximum debug features.
#' @param debug Logical. Backward-compatible alias for debug behavior.
#'   If TRUE, mode is forced to `"debug"`.
#' @param logFile Optional log file path for `"server"` (or `"debug"`) mode.
#'   If NULL in `"server"` mode, defaults to `file.path(dataDir, "contrast_viewer.log")`.
#' @param allowExports Logical. If FALSE, disables export actions (TSV/PNG) in the UI.
#'   Default is TRUE.
#' @param openBrowser Logical. If TRUE, opens the dashboard in the default browser.
#'   Default is TRUE.
#' @param background Logical. If TRUE, runs the server in the background.
#'   Default is TRUE.
#' @param ... Backward-compatible aliases:
#'   `data_dir`, `log_file`, `allow_exports`, `open_browser`.
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
#' runCohortContrastViewer(dataDir = "/path/to/my/data")
#'
#' # Launch on a different port
#' runCohortContrastViewer(port = 8080)
#' }
#'
#' @export
runCohortContrastViewer <- function(dataDir = NULL,
                                port = 8050,
                                host = "127.0.0.1",
                                mode = c("simple", "server", "debug"),
                                debug = FALSE,
                                logFile = NULL,
                                allowExports = TRUE,
                                openBrowser = TRUE,
                                background = TRUE,
                                ...) {
  legacy <- .resolveLegacyArgs(
    dots = list(...),
    mapping = c(
      data_dir = "dataDir",
      log_file = "logFile",
      allow_exports = "allowExports",
      open_browser = "openBrowser"
    ),
    explicitNew = c(
      dataDir = !missing(dataDir),
      logFile = !missing(logFile),
      allowExports = !missing(allowExports),
      openBrowser = !missing(openBrowser)
    ),
    fnName = "runCohortContrastViewer"
  )
  if (!is.null(legacy$dataDir)) dataDir <- legacy$dataDir
  if (!is.null(legacy$logFile)) logFile <- legacy$logFile
  if (!is.null(legacy$allowExports)) allowExports <- legacy$allowExports
  if (!is.null(legacy$openBrowser)) openBrowser <- legacy$openBrowser

  mode <- match.arg(mode)
  if (isTRUE(debug) && mode != "debug") {
    mode <- "debug"
  }

  if (!is.logical(allowExports) || length(allowExports) != 1 || is.na(allowExports)) {
    stop("allowExports must be TRUE or FALSE.")
  }

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

  python_dir <- .getPythonDir()
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

  if (is.null(dataDir)) {
    dataDir <- getwd()
  }
  if (!dir.exists(dataDir)) {
    stop("Data directory does not exist: ", dataDir)
  }
  data_dir_abs <- normalizePath(dataDir)
  env_vars$CONTRAST_VIEWER_DATA_DIR <- data_dir_abs
  env_vars$CONTRAST_VIEWER_MODE <- mode
  env_vars$CONTRAST_VIEWER_ALLOW_EXPORTS <- if (isTRUE(allowExports)) "1" else "0"
  env_vars$CONTRAST_VIEWER_DEBUG <- if (identical(mode, "debug")) "1" else "0"

  if (identical(mode, "server") || identical(mode, "debug")) {
    if (is.null(logFile) || !nzchar(trimws(logFile))) {
      if (identical(mode, "server")) {
        logFile <- file.path(data_dir_abs, "contrast_viewer.log")
      }
    }
    if (!is.null(logFile) && nzchar(trimws(logFile))) {
      logFile <- normalizePath(path.expand(logFile), mustWork = FALSE)
      env_vars$CONTRAST_VIEWER_LOG_FILE <- logFile
    }
  }

  message("Starting CohortContrast Viewer...")
  message("App directory: ", python_dir)
  message("Data directory: ", data_dir_abs)
  message("Mode: ", mode)
  if (!isTRUE(allowExports)) {
    message("Exports are disabled for this run.")
  }
  if (!is.null(env_vars$CONTRAST_VIEWER_LOG_FILE)) {
    message("Log file: ", env_vars$CONTRAST_VIEWER_LOG_FILE)
  }
  message("Port: ", port)

  # Get Python path

  pythonPath <- reticulate::py_config()$python

  if (background) {
    # Run in background using processx
    if (!requireNamespace("processx", quietly = TRUE)) {
      stop("Package 'processx' is required for background execution.\n",
           "Install it with: install.packages('processx')")
    }

    # Set up environment
    env <- Sys.getenv()
    for (name in names(env_vars)) {
      env[name] <- env_vars[[name]]
    }

    .ccv_env$app_process <- processx::process$new(
      command = pythonPath,
      args = c(app_path),
      wd = python_dir,
      env = env,
      stdout = "",
      stderr = ""
    )

    .ccv_env$app_port <- port

    # Wait a moment for the server to start
    Sys.sleep(2)

    if (.ccv_env$app_process$is_alive()) {
      url <- paste0("http://", host, ":", port)
      message("CohortContrast Viewer is running!")
      message("URL: ", url)

      if (openBrowser) {
        utils::browseURL(url)
      }

      message("\nUse stopCohortContrastViewer() to stop the server.")
      return(invisible(.ccv_env$app_process))
    } else {
      # Check for errors
      err_msg <- tryCatch({
        paste(.ccv_env$app_process$read_all_error_lines(), collapse = "\n")
      }, error = function(e) "")
      exit_status <- tryCatch(.ccv_env$app_process$get_exit_status(), error = function(e) NA_integer_)
      if (nzchar(err_msg)) {
        stop("Failed to start CohortContrast Viewer. Exit status: ", exit_status, ". Error: ", err_msg)
      }
      stop("Failed to start CohortContrast Viewer. Exit status: ", exit_status, ".")
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
          do.call(Sys.setenv, as.list(stats::setNames(old_env[name], name)))
        }
      }
    })

    if (openBrowser) {
      utils::browseURL(paste0("http://", host, ":", port))
    }

    # Run the app
    system2(pythonPath, args = app_path, wait = TRUE)
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
#' @param studyPath Path to directory containing patient-level parquet files
#'   (data_patients.parquet, data_features.parquet, etc.)
#' @param outputPath Path for output summary files. Default is studyPath + "_summary"
#' @param clusterKValues Vector of k values to pre-compute clustering for.
#'   Default is c(2, 3, 4, 5).
#' @param conceptLimit Maximum number of concepts to use for clustering.
#'   Default is 60.
#' @param minCellCount Minimum patient count for small cell suppression (privacy).
#'   Counts between 1 and (minCellCount-1) are rounded up to minCellCount.
#'   Default is 0 (disabled). Set to e.g. 5 to apply suppression.
#' @param maxParallelJobs Maximum number of parallel clustering jobs.
#'   Default is 1 (sequential) to avoid out-of-memory errors on servers.
#'   Set to 2-4 on machines with ample RAM for faster execution.
#' @param clusterFeatureMatrixCellThreshold Maximum allowed size for the
#'   clustering feature matrix, measured as patients x features
#'   (features = 3 x clustering concepts). If exceeded, clustering concepts are
#'   auto-capped for memory safety. Default is 50000000.
#' @param pairwiseOverlapMaxConcepts Maximum number of concepts used when
#'   computing pairwise overlap matrices. Higher values increase memory/time
#'   quadratically. Default is 500.
#' @param ... Backward-compatible aliases: `study_path`, `output_path`,
#'   `cluster_k_values`, `concept_limit`, `min_cell_count`, `max_parallel_jobs`,
#'   `cluster_feature_matrix_cell_threshold`, `pairwise_overlap_max_concepts`.
#'
#' @return A list with:
#'   \item{outputPath}{Path to the generated summary directory
#'     (canonical name; legacy `output_path` may also be present depending on Python output).}
#'   \item{files}{Named list of generated file paths}
#'   \item{metadata}{Study metadata including demographics and clustering info}
#'
#' @examples
#' \dontrun{
#' # Generate summary data for a study (no suppression)
#' result <- precomputeSummary(
#'   studyPath = "results_parquet/Breast_cancer",
#'   outputPath = "summaries/Breast_cancer"
#' )
#'
#' # With small cell suppression (counts 1-4 become 5)
#' result <- precomputeSummary(
#'   studyPath = "results_parquet/Breast_cancer",
#'   minCellCount = 5
#' )
#'
#' # View generated files
#' print(result$files)
#'
#' # Run viewer with summary data (no patient data needed)
#' runCohortContrastViewer(dataDir = "summaries")
#' }
#'
#' @export
precomputeSummary <- function(studyPath,
                              outputPath = NULL,
                              clusterKValues = c(2, 3, 4, 5),
                              conceptLimit = 60,
                              minCellCount = 0,
                              maxParallelJobs = 1,
                              clusterFeatureMatrixCellThreshold = 50000000,
                              pairwiseOverlapMaxConcepts = 500,
                              ...) {
  legacy <- .resolveLegacyArgs(
    dots = list(...),
    mapping = c(
      study_path = "studyPath",
      output_path = "outputPath",
      cluster_k_values = "clusterKValues",
      concept_limit = "conceptLimit",
      min_cell_count = "minCellCount",
      max_parallel_jobs = "maxParallelJobs",
      cluster_feature_matrix_cell_threshold = "clusterFeatureMatrixCellThreshold",
      pairwise_overlap_max_concepts = "pairwiseOverlapMaxConcepts"
    ),
    explicitNew = c(
      studyPath = !missing(studyPath),
      outputPath = !missing(outputPath),
      clusterKValues = !missing(clusterKValues),
      conceptLimit = !missing(conceptLimit),
      minCellCount = !missing(minCellCount),
      maxParallelJobs = !missing(maxParallelJobs),
      clusterFeatureMatrixCellThreshold = !missing(clusterFeatureMatrixCellThreshold),
      pairwiseOverlapMaxConcepts = !missing(pairwiseOverlapMaxConcepts)
    ),
    fnName = "precomputeSummary"
  )
  if (!is.null(legacy$studyPath)) studyPath <- legacy$studyPath
  if (!is.null(legacy$outputPath)) outputPath <- legacy$outputPath
  if (!is.null(legacy$clusterKValues)) clusterKValues <- legacy$clusterKValues
  if (!is.null(legacy$conceptLimit)) conceptLimit <- legacy$conceptLimit
  if (!is.null(legacy$minCellCount)) minCellCount <- legacy$minCellCount
  if (!is.null(legacy$maxParallelJobs)) maxParallelJobs <- legacy$maxParallelJobs
  if (!is.null(legacy$clusterFeatureMatrixCellThreshold)) {
    clusterFeatureMatrixCellThreshold <- legacy$clusterFeatureMatrixCellThreshold
  }
  if (!is.null(legacy$pairwiseOverlapMaxConcepts)) {
    pairwiseOverlapMaxConcepts <- legacy$pairwiseOverlapMaxConcepts
  }

  # Check Python configuration
  if (!.ccv_env$python_configured) {
    message("Python not configured. Running configurePython() first...")
    configurePython()
  }

  # Validate study path
  studyPath <- normalizePath(studyPath, mustWork = TRUE)

  if (!file.exists(file.path(studyPath, "data_patients.parquet"))) {
    stop("data_patients.parquet not found in: ", studyPath)
  }

  if (!is.numeric(clusterFeatureMatrixCellThreshold) || length(clusterFeatureMatrixCellThreshold) != 1 || is.na(clusterFeatureMatrixCellThreshold) || clusterFeatureMatrixCellThreshold < 1000) {
    stop("clusterFeatureMatrixCellThreshold must be a single numeric value >= 1000")
  }
  if (!is.numeric(pairwiseOverlapMaxConcepts) || length(pairwiseOverlapMaxConcepts) != 1 || is.na(pairwiseOverlapMaxConcepts) || pairwiseOverlapMaxConcepts < 2) {
    stop("pairwiseOverlapMaxConcepts must be a single numeric value >= 2")
  }

  # Get Python directory
  python_dir <- .getPythonDir()

  # Build Python command
  pythonPath <- reticulate::py_config()$python

  # Create the Python script call
  # Handle conceptLimit = Inf (use None in Python to mean "all concepts")
  concept_limit_py <- if (is.infinite(conceptLimit)) "None" else as.character(as.integer(conceptLimit))

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
    max_parallel_jobs=%d,
    cluster_feature_matrix_cell_threshold=%d,
    pairwise_overlap_max_concepts=%d
)

# Print result as JSON for R to parse
print("__RESULT_START__")
print(json.dumps(result, default=str))
print("__RESULT_END__")
',
    python_dir,
    studyPath,
    if (is.null(outputPath)) "None" else sprintf('"%s"', outputPath),
    paste0("[", paste(clusterKValues, collapse = ", "), "]"),
    concept_limit_py,
    as.integer(minCellCount),
    as.integer(maxParallelJobs),
    as.integer(clusterFeatureMatrixCellThreshold),
    as.integer(pairwiseOverlapMaxConcepts)
  )

  message("Pre-computing summary data for: ", studyPath)
  message("This may take a few minutes for large datasets...")

  # Run Python script
  result <- tryCatch({
    output <- system2(
      pythonPath,
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

  if (!is.null(result$output_path) && is.null(result$outputPath)) {
    result$outputPath <- result$output_path
  }

  message("\nSummary generation complete!")
  outputDir <- result$outputPath
  if (is.null(outputDir) && !is.null(result$output_path)) {
    outputDir <- result$output_path
  }
  message("Output directory: ", outputDir)
  message("Files generated: ", length(result$files))

  return(result)
}


#' Check if a Data Directory Contains Summary Mode Data
#'
#' Determines whether a data directory contains pre-computed summary data
#' (summary mode) or patient-level data (patient mode).
#'
#' @param dataDir Path to the data directory
#' @param ... Backward-compatible alias: `data_dir`.
#'
#' @return A list with:
#'   \item{mode}{"summary" or "patient"}
#'   \item{has_clustering}{Logical, whether clustering data is available}
#'   \item{clusterKValues}{Vector of available k values for clustering
#'     (canonical name; legacy `cluster_k_values` is also returned).}
#'
#' @examples
#' \dontrun{
#' info <- checkDataMode("results_parquet/Breast_cancer")
#' print(info$mode)  # "patient" or "summary"
#' }
#'
#' @export
checkDataMode <- function(dataDir, ...) {
  legacy <- .resolveLegacyArgs(
    dots = list(...),
    mapping = c(data_dir = "dataDir"),
    explicitNew = c(dataDir = !missing(dataDir)),
    fnName = "checkDataMode"
  )
  if (!is.null(legacy$dataDir)) dataDir <- legacy$dataDir

  dataDir <- normalizePath(dataDir, mustWork = TRUE)

  # Check for summary mode indicators
  has_metadata <- file.exists(file.path(dataDir, "metadata.json"))
  has_concept_summaries <- file.exists(file.path(dataDir, "concept_summaries.parquet"))
  has_data_patients <- file.exists(file.path(dataDir, "data_patients.parquet"))

  if (has_metadata && has_concept_summaries) {
    # Read metadata
    metadata <- jsonlite::fromJSON(file.path(dataDir, "metadata.json"))

    # Find available clustering k values
    clustering_files <- list.files(dataDir, pattern = "clustering_k\\d+_summary\\.parquet")
    k_values <- as.integer(gsub("clustering_k(\\d+)_summary\\.parquet", "\\1", clustering_files))

    return(list(
      mode = "summary",
      has_clustering = length(k_values) > 0,
      clusterKValues = sort(k_values),
      cluster_k_values = sort(k_values),
      demographics = metadata$demographics,
      metadata = metadata
    ))
  } else if (has_data_patients) {
    return(list(
      mode = "patient",
      has_clustering = TRUE,  # Can compute on-the-fly
      clusterKValues = 2:10,  # Any k is possible
      cluster_k_values = 2:10,  # Any k is possible
      demographics = NULL,
      metadata = NULL
    ))
  } else {
    stop("Invalid data directory: neither summary nor patient data found")
  }
}
