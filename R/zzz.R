.onLoad <- function(libname, pkgname) {
  initialize_environments()
}

#' Initialize Environments for the Pension Package
#'
#' Sets up a list of empty environments used by the pension package, including the
#' creation of named environments such as `frs` and `trs`. This function is
#' called during package loading to ensure that the necessary environments are
#' available. It can also be called manually if needed to reinitialize the
#' environments.
#'
#' @details
#' The `initialize_environments` function creates a list of environments that
#' are used throughout the pension package. Each environment is initialized as
#' an empty environment with no parent, ensuring isolation. The list of
#' environments is stored in a package-specific environment, making it
#' accessible throughout the package but not cluttering the global environment.
#'
#' @examples
#' # Manually initialize environments with defaults (typically not needed)
#' initialize_environments()
initialize_environments <- function() {
  # Create a package-specific environment to hold all environments
  assign(".penvir_env", new.env(parent = emptyenv()), envir = parent.env(environment()))

  # Define standard set of environments
  env_list <- list(
    frs = new.env(),
    trs = new.env()
  )

  # Store the environments list in the package environment
  assign("environments", env_list, envir = get(".penvir_env", envir = parent.env(environment())))
}
