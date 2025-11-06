#' Package Management and Dependencies
#' This script handles all package installations and loading for the AI Carbon Footprint Analysis project
#' Last updated: October 29, 2025

#' Function to install and load required packages
#' @param pkg_list List of required packages
#' @return Invisible NULL
setup_packages <- function(pkg_list = c(
    "ggplot2",    # For creating sophisticated visualizations
    "dplyr",      # For data manipulation
    "scales",     # For better axis scaling and formatting
    "ggthemes",   # For additional plotting themes
    "ggrepel",    # For non-overlapping text labels
    "plotly",     # For interactive visualizations
    "htmlwidgets",# For saving interactive html widgets
    "shiny",      # For interactive dashboard
    "shinydashboard", # Dashboard layout components
    "shinydashboardPlus", # Enhanced dashboard components
    "shinyBS",    # Tooltips
    "shinyjs",    # JS helpers for theme toggling
    "shinycssloaders", # Loading spinners for plots
    "DT",         # DataTable for explorer
    "bslib",      # Theming
    "ggcorrplot", # Correlation plots
    "reactable",  # Alternative modern tables
    "here",       # Reliable file paths
    "forecast",   # ARIMA forecasting (optional)
    "ranger",     # Fast random forest for feature importance
    "leaflet",    # Maps
    "tidyr",      # For data tidying
    "knitr",      # For report generation
    "rmarkdown",  # For R Markdown processing
    "readr",      # For fast delimited file IO
    "data.table", # For high-performance data manipulation
    "lubridate",  # For dates and times
    "stringr",    # For string operations
    "readxl",     # For reading Excel files
    "janitor"     # For data cleaning helpers
), install_if_missing = TRUE, repos = getOption("repos")) {
    # Ensure a CRAN repo is set
    if (is.null(repos) || length(repos) == 0 || identical(repos[["CRAN"]], "@CRAN@") || anyNA(repos)) {
        options(repos = c(CRAN = "https://cloud.r-project.org"))
    }
    
    # On Windows, prefer binary packages to avoid needing build tools
    if (tolower(.Platform$OS.type) == "windows") {
        options(pkgType = "binary")
        Sys.setenv(R_COMPILE_AND_INSTALL_PACKAGES = "never")
    }

    # Check for missing packages
    installed <- rownames(installed.packages())
    new_packages <- setdiff(pkg_list, installed)

    # Install missing packages
    if (isTRUE(install_if_missing) && length(new_packages) > 0) {
        message("Installing missing packages: ", paste(new_packages, collapse = ", "))
        tryCatch({
            install.packages(
                new_packages,
                dependencies = TRUE,
                quiet = TRUE,
                Ncpus = max(1L, as.integer(tryCatch(parallel::detectCores(), error = function(e) 1L)) - 1L),
                repos = getOption("repos"),
                type = getOption("pkgType", default = "binary")
            )
            # Verify after bulk install; retry any that still failed one-by-one as binary
            still_missing <- setdiff(new_packages, rownames(installed.packages()))
            if (length(still_missing) > 0) {
                message("Retrying binary install for: ", paste(still_missing, collapse = ", "))
                for (pkg in still_missing) {
                    tryCatch({
                        install.packages(pkg, dependencies = TRUE, quiet = TRUE, repos = getOption("repos"), type = "binary")
                    }, error = function(e) {
                        stop(paste0("Package installation failed for '", pkg, "': ", conditionMessage(e)))
                    })
                }
            }
        }, error = function(e) {
            stop(paste0("Package installation failed: ", conditionMessage(e)))
        })
    }

    # Load all packages and verify
    for (package in pkg_list) {
        ok <- suppressPackageStartupMessages(suppressWarnings(
            require(package, character.only = TRUE, quietly = TRUE)
        ))
        if (!isTRUE(ok)) {
            stop(paste0("Package '", package, "' failed to load. Consider setting install_if_missing = TRUE."))
        }
    }

    message("All required packages are installed and loaded successfully!")
    invisible(TRUE)
}

    # Execute package setup
    if (FALSE) {
        tryCatch({
            setup_packages()
        }, error = function(e) {
            stop(paste0("Error in package setup: ", conditionMessage(e)))
        })
    }