
#' Get an Internally Stored Pension Fund, Populating It If Empty
#'
#' Retrieves an internally stored pension fund. Populate with data and functions
#' if it is empty. The environment is assumed to be pre-defined in the package
#' and initially empty.
#'
#' Use `get_fund` to put a copy of an internally stored pension fund into the
#' workspace.
#'
#' @param fund_name A character string naming the pension fund to retrieve and
#'   populate. The environment must exist in the `environments` list created
#'   during package initialization.
#'
#' @details The function first checks if the specified environment exists in the
#' `environments` list. If it does not exist, an error is thrown. If the
#' environment is empty, the function attempts to populate it using data from
#' the package's `extdata` directory.
#'
#' @return Invisibly returns the environment after populating it.
#'
#' @examples
#' # Populate and retrieve the 'frs' environment
#' frs <- get_fund("frs")
#'
#' # Attempt to populate an already populated environment
#' frs <- get_fund("frs") # This should indicate that 'frs' is already populated
#'
#' @export
get_fund <- function(fund_name) {
  .penvir_env <- get(".penvir_env", envir = parent.env(environment())) # Access .penvir_env

  # environment_exists will directly stop execution if the fund doesn't exist
  # and stop_on_fail is TRUE
  if (!environment_exists(fund_name, stop_on_fail = TRUE)) {
    return(invisible(NULL))  # This line is only a fallback; it might never be executed
  }

  populate(fund_name)
  message("Retrieving populated fund: ", fund_name, ".")
  env <- get_env(fund_name)

  return(rlang::env_clone(env, parent = emptyenv()))
}


#' Get the List of Pension Fund Environments
#'
#' Retrieves the list of environments stored in the package-specific
#' environment.
#'
#' @return The list of environments.
#' @noRd
get_environments <- function() {
  get("environments", envir = .penvir_env)
}


#' Check if Environment Exists
#'
#' Checks if an environment exists in the list of environments.
#'
#' @param fund_name The name of the fund to check.
#' @param stop_on_fail If TRUE and environment does not exist, print message
#'   then stop.
#' @return TRUE or FALSE.
#' @noRd
environment_exists <- function(fund_name, stop_on_fail = FALSE) {
  environments <- get_environments()  # Retrieve available environments
  exists <- fund_name %in% names(environments)

  if (!exists && stop_on_fail) {
    stop(
      "Pension fund '", fund_name, "' does not exist. ",
      "Valid funds are: \n  ", paste(names(environments), collapse = ", "), ".",
      call. = FALSE  # This prevents the function call from being printed in the error message
    )
  }

  return(exists)
}

#' Get a Specific Pension Fund by Name
#'
#' Retrieves a specific fund from the list of plans.
#'
#' @param fund_name The name of the fund to retrieve.
#' @return The specified fund
#' @noRd
get_env <- function(fund_name) {
  if (environment_exists(fund_name)) {
    return(.penvir_env$environments[[fund_name]])
  } else {
    return(NULL)
  }
}
