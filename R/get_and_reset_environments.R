#' Initialize Environments for the Pension Package
#'
#' Sets up a list of environments used by the pension package, including the creation
#' of named environments such as `frs` and `trs`. This function is called during package
#' loading to ensure that the necessary environments are available. It can also be called
#' manually if needed to reinitialize the environments.
#'
#' @details
#' The `initialize_environments` function creates a list of environments that are used throughout
#' the pension package. Each environment is initialized as an empty environment with no parent,
#' ensuring isolation. The list of environments is stored in the global environment under the
#' name `environments`, making it accessible throughout the package.
#'
#' This function is called automatically during the package's `.onLoad()` process, but can be
#' manually invoked if reinitialization of the environments is required.
#'
#' @examples
#' # Manually initialize environments (typically not needed)
#' initialize_environments()
#'
#' @export
initialize_environments <- function() {
  # Create a named list to hold all environments
  environments <<- list(
    frs = new.env(parent = emptyenv()),
    trs = new.env(parent = emptyenv())
    # Add more environments as needed
  )

  # Expose this list globally
  assign("environments", environments, envir = .GlobalEnv)
}



#' Get and Populate a Named Environment
#'
#' Retrieves an environment by name, populates it with data and functions if it is empty,
#' and assigns it to the global environment. The environment is assumed to be pre-defined
#' in the package and initially empty. This function is intended for use with environments
#' like `frs` and `trs`, with more to be added in the future.
#'
#' @param env_name A character string naming the environment to retrieve and populate.
#'   The environment must exist in the `environments` list created during package initialization.
#'
#' @details
#' The function first checks if the specified environment exists in the `environments` list.
#' If it does not exist, an error is thrown. If the environment is empty, the function attempts
#' to populate it using data from the package's `extdata` directory and assigns the environment
#' to the global environment, making it accessible by its name.
#'
#' The current implementation assumes the existence of an RDS file in the `extdata` directory
#' with a specific structure related to the environment name (e.g., `beneficiaries.rds` for `frs`).
#'
#' The function is designed to be extended with additional environments and data loading
#' mechanisms as needed.
#'
#' @return Invisibly returns the environment after populating it. The environment is also
#'   assigned to the global environment, so it can be accessed directly by its name.
#'
#' @examples
#' initialize_environments()
#'
#' # Retrieve and populate the 'frs' environment
#' get_data("frs")
#'
#' # Retrieve and populate the 'trs' environment
#' get_data("trs")
#'
#' @export
get_data <- function(env_name) {
  # Check if the environment exists in the environments list
  if (!env_name %in% names(environments)) {
    stop(paste0("Environment ", env_name, " does not exist"))
  }

  env <- environments[[env_name]]

  if (length(ls(envir = env)) == 0) {
    print(paste0("Environment ", env_name, " is empty. Populating now..."))

    # Example data file path
    fpath <- fs::path_package("extdata", env_name, "beneficiaries.rds", package = "penvir")

    if (file.exists(fpath)) {
      env$beneficiaries <- readRDS(fpath)
    } else {
      stop(paste0("File not found: ", fpath))
    }

    # Example functions
    env$calculate_benefits <- function() {
      sum(env$beneficiaries$benefits)
    }
  }

  # Assign the populated environment back to the global environment
  assign(env_name, env, envir = .GlobalEnv)
}



#' Reset a Named Environment to Its Empty State
#'
#' Resets a specified environment by removing all objects within it, effectively returning
#' it to its initial empty state. The environment must exist in the `environments` list created
#' during package initialization.
#'
#' @param env_name A character string naming the environment to reset. The environment must
#'   exist in the `environments` list.
#'
#' @details
#' The function checks if the specified environment exists in the `environments` list.
#' If the environment is found, all objects within it are removed, leaving the environment empty.
#' This function can be useful if you want to clear an environment before repopulating it
#' with new data or functions.
#'
#' After resetting the environment, a message is printed to confirm that the environment
#' has been cleared.
#'
#' @return Invisibly returns `NULL`. The function's primary purpose is to reset the environment.
#'
#' @examples
#' initialize_environments()
#' get_data("frs")
#'
#' # Reset the 'frs' environment to its empty state
#' reset_env("frs")
#'
#' # Reset the 'trs' environment to its empty state
#' reset_env("trs")
#'
#' @export
reset_env <- function(env_name) {
  # Check if the environment exists in the environments list
  if (!env_name %in% names(environments)) {
    stop(paste0("Environment ", env_name, " does not exist"))
  }

  env <- environments[[env_name]]

  # Clear all objects from the environment
  rm(list = ls(envir = env), envir = env)

  message(paste0("Environment '", env_name, "' has been reset to empty."))
}


