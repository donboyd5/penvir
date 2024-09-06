
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
