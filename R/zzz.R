.onLoad <- function(libname, pkgname) {
  initialize_environments()
}


#' Initialize Environments for the Pension Package
#'
#' Sets up lists of empty environments used by the pension package, including
#' the creation of named environments such as `frs` and `trs` for internal
#' funds, and a separate list for user-loaded funds. This function is called
#' during package loading to ensure that the necessary environments are
#' available. It can also be called manually if needed to reinitialize the
#' environments.
#'
#' @details
#' The `initialize_environments` function creates two internal environments that
#' are used throughout the pension environment package. One internal environment
#' stores "internal" pension funds that are part of the package. The other
#' internal environment stores any additional funds that users may load into the
#' package. The function creates a list of environments for each type of fund,
#' with each environment containing an empty environment and a type identifier.
#' The function then stores these lists in the package environment for easy
#' access. Each environment is initialized as an empty environment with no
#' parent, ensuring isolation. The lists of environments are stored in
#' package-specific environments, making them accessible throughout the package
#' but not cluttering the global environment.
#'
#' @examples
#' # Manually initialize environments with defaults (typically not needed)
#' initialize_environments()
#' @noRd
initialize_environments <- function() {
  # Create a package-specific environment to hold all environments
  assign(".penvir_env", new.env(parent = emptyenv()),
         envir = parent.env(environment())
  )

  # Define standard set of internal environments
  internal_env_list <- list(
    frs = list(env = new.env(), type = "internal"),
    trs = list(env = new.env(), type = "internal")
  )

  # Define a list for user environments
  user_env_list <- list()

  # Store the environments lists in the package environment
  assign("internal_environments",
         internal_env_list,
         envir = get(".penvir_env", envir = parent.env(environment())))
  assign("user_environments",
         user_env_list,
         envir = get(".penvir_env", envir = parent.env(environment())))
}
