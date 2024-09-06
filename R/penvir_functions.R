
#' Check the Status of All Pension Fund Environments Stored Internally in penvir
#'
#' Returns a tibble listing all environments defined in the package and their
#' status (either "Populated" or "Unpopulated"). The environments are sorted
#' alphabetically by name in the resulting tibble.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{fund_name}{The name of the pension fund}
#'   \item{status}{The status of the environment, either "Populated" or "Unpopulated".}
#' }
#'
#' @details
#' This function iterates over all pension fund environments in the internal
#' `environments` list, checks whether each environment is populated (contains
#' any objects), and returns a tibble that shows the status of each environment.
#' The tibble provides a clear overview of the current state of all
#' environments, making it easier to manage and track them.
#'
#' @examples
#' check_fund_status()
#'
#' @export
check_fund_status <- function() {
  environments <- get_environments()
  sorted_names <- names(environments) |> sort()

  environment_status <- tibble::tibble(
    fund_name = sorted_names,
    status = sapply(sorted_names, function(fund_name) {
      env <- environments[[fund_name]]
      if (length(ls(envir = env)) == 0) {
        return("Unpopulated")
      } else {
        return("Populated")
      }
    })
  )

  return(environment_status)
}

#' Clone a Pension Fund That is in the Workspace
#'
#' Creates a clone, or "deep copy," of a pension fund in the workspace, including all of
#' its elements. The clone is a separate copy of the original fund, meaning
#' changes to one will not affect the other. within it.
#'
#' @param fund Any fund in the workspace, as long as it is an environment.
#'
#' @return A new fund that is a deep copy of the specified fund, containing
#'   copies of all objects from the original fund.
#'
#' @details
#'
#' This function is useful when you need to work with a separate copy of a fund
#' without altering the original environment's contents. For example it would be
#' useful if you obtained the frs fund from the package, made some changes, want
#' to keep the saved version, and then create new fund based on the changed
#' fund.
#'
#'@examples
#' frs <- get_fund("frs")
#' names(frs)
#'
#' # assignment does not make a true copy of frs, it simply points to the original
#' frs2 <- frs
#' # add something to frs2
#' frs2$x <- 7
#' names(frs2)
#' # did it affect frs? oops, yes, but that's probably NOT what we want
#' names(frs)
#'
#' # what we probably really want is a clone of frs, independent from the original
#' frs <- get_fund("frs") # get a fresh version of the data
#' names(frs) # good, back to the original
#' # add something to frs2
#' frs2$x <- 7
#' names(frs2)
#' # did it affect frs? no. for many use cases, this is what we'll want
#' names(frs)
#' @export
clone_fund <- function(fund) {
  # check for existence and status as environment
  new_fund <- rlang::env_clone(fund, parent = emptyenv())
  return(new_fund)
}

is_populated <- function(env) {
  return(length(ls(envir = env)) > 0)
}


#' Populate Internal Copy of a Pension Fund With Data
#'
#' Populates an internal pension fund environment with data and functions if it
#' is empty. The environment is assumed to be pre-defined in the package and
#' initially empty.
#'
#' @param fund_name A character string naming the fund environment to populate.
#'   The environment must exist in the `environments` list created during
#'   package initialization.
#'
#' @details
#' The function first checks if the specified environment exists in the
#' `environments` list. If it does not exist, an error is thrown. If the
#' environment is empty, the function attempts to populate it using data from
#' the package's `extdata` directory.
#'
#' @return Nothing.
#'
#' @examples
#' # Populate the 'frs' environment
#' check_fund_status()
#' populate("frs")
#' check_fund_status()
#'
#' # Attempt to populate an already populated environment
#' frs <- get_fund("frs") # Getting a fund populates the internal copy if empty
#' populate("frs") # This should indicate that 'frs' is already populated
#'
#' @export
populate <- function(fund_name) {
  if (!environment_exists(fund_name, stop_on_fail = TRUE)) {
    return(invisible(NULL))
  }

  env <- get_env(fund_name)

  if (is_populated(env)) {
    message(paste0(
      "Internal copy of fund ",
      fund_name,
      " is already populated, nothing to do."
    ))
    return(invisible(NULL))
  }

  message("\n", "Populating internal copy of empty fund ", fund_name, " with stored data...")

  # Path to folder for data files associated with fund_name
  folder_path <- fs::path_package("extdata", fund_name, package = "penvir")

  fpath <- fs::path(folder_path, "beneficiaries.rds")

  if (file.exists(fpath)) {
    env$beneficiaries <- readRDS(fpath)
  } else {
    stop(paste0("File not found: ", fpath))
  }

  env$calculate_benefits <- function() {
    sum(env$beneficiaries$benefits)
  }

  return(invisible(NULL))
}


#' Depopulate an Internally Stored Pension Fund
#'
#' Depopulates the internal copy of a specified fund by removing all objects
#' within it, effectively returning it to its initial empty state. The fund must
#' exist in the `environments` list created during package initialization.
#'
#' Depopulate funds that won't be used, to keep memory of the package clean.
#'
#' @param fund_name A character string naming the fund to depopulate. The
#'   fund must exist in the `environments` list.
#'
#' @details
#' The function checks if the specified environment exists in the `environments`
#' list. If the environment is found, all objects within it are removed, leaving
#' the environment empty. This function can be useful if you want to clear an
#' environment before repopulating it with new data or functions.
#'
#' After depopulating the fund environment, a message is printed to confirm that
#' the environment has been cleared.
#'
#' @return Invisibly returns `NULL`. The function's primary purpose is to
#'   depopulate a pension fund's environment.
#'
#' @examples
#' # Populate and then depopulate the 'frs' environment
#' populate("frs")
#' check_fund_status()
#' depopulate("frs")
#' check_fund_status()
#'
#' @export
depopulate <- function(fund_name) {

  # environment_exists will directly stop execution if the fund doesn't exist
  # and stop_on_fail is TRUE
  if (!environment_exists(fund_name, stop_on_fail = TRUE)) {
    return(invisible(NULL))  # This line is only a fallback; it might never be executed
  }

  env <- get_env(fund_name)
  if(!is_populated(env)) {
    message(paste0("Internal copy of pension fund '", fund_name, "' is already empty."))
    return(invisible(NULL))
  }


  rm(list = ls(envir = env), envir = env)
  message(paste0("Internal copy of pension fund '", fund_name, "' has been reset to empty."))
  return(invisible(NULL))
}



