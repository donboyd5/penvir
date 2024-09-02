#' Check if Environment Exists
#'
#' Checks if an environment exists in the list of environments.
#'
#' @param fund_name The name of the fund to check.
#' @param stop_on_fail If TRUE print message if environment does not exist, and
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

#' Create a Clone of a Pension Fund
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

#' Get and Populate a Named Environment
#'
#' Retrieves an environment by name, populates it with data and functions if it
#' is empty. The environment is assumed to be pre-defined in the package and
#' initially empty.
#'
#' @param fund_name A character string naming the pension fund to retrieve and
#'   populate. The environment must exist in the `environments` list created
#'   during package initialization.
#'
#' @details
#' The function first checks if the specified environment exists in the
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
  env <- get_env(fund_name)

  return(rlang::env_clone(env, parent = emptyenv()))
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
  }
}

#' Get the List of Environments
#'
#' Retrieves the list of environments stored in the package-specific
#' environment.
#'
#' @return The list of environments.
#' @noRd
get_environments <- function() {
  get("environments", envir = .penvir_env)
}


#' Run a Pension Model on a Specified Pension Fund Environment
#'
#' Executes a pension model using the functions and data contained within a
#' specified environment. The function currently calculates the benefits
#' associated with the pension fund and is designed to be extended with
#' additional calculations, such as funding.
#'
#' @param fund_env An environment containing the necessary data and functions
#'   for the pension model. The environment is expected to include a
#'   `calculate_benefits` function, and may also contain other functions and
#'   data relevant to pension modeling.
#'
#' @return A list with the results of the pension model. Currently, the list contains:
#' \describe{
#'   \item{benefits}{The calculated benefits from the `calculate_benefits` function.}
#' }
#'
#' @details
#' The `pension_model` function is designed to operate on a specific environment
#' representing a pension fund. The environment should include a
#' `calculate_benefits` function that returns the total benefits for the fund.
#' The function is structured to allow for easy extension with additional
#' calculations, such as funding, which can be uncommented or added as needed.
#'
#' This function provides a flexible framework for pension modeling, allowing
#' users to input different environments representing various pension plans.
#'
#' @examples
#' # initialize_environments()
#' frs <- get_fund("frs")
#' results <- pension_model(frs)
#' print(results$benefits)
#'
#' @export
pension_model <- function(fund_env) {
  benefits <- fund_env$calculate_benefits()

  list(
    benefits = benefits
  )
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

  message(paste0("Populating empty fund ", fund_name, " with stored data..."))

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


#' Depopulate a Named Fund
#'
#' Depopulates the internal copy of a specified fund by removing all objects
#' within it, effectively returning it to its initial empty state. The fund must
#' exist in the `environments` list created during package initialization.
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
  stopifnot(environment_exists(fund_name))

  env <- get_env(fund_name)
  if(!is_populated(env)) {
    message(paste0("Internal copy of pension fund '", fund_name, "' is already empty."))
    return(invisible(NULL))
  }


  rm(list = ls(envir = env), envir = env)
  message(paste0("Internal copy of pension fund '", fund_name, "' has been reset to empty."))
  return(invisible(NULL))
}


#' Sort Names of Object Elements
#'
#' This function takes an object and returns the names of its elements sorted in
#' alphabetical order. It is useful for quickly getting an ordered list of
#' names, particularly for named vectors, lists, or data frames.
#'
#' @param obj An R object with named elements.
#' @return A character vector containing the names of the elements in the object
#'   sorted alphabetically.
#' @examples
#' ns(mtcars) # returns sorted names of the dataset columns
#' ns(list(apple = 1, banana = 2)) # returns c("apple", "banana")
#' @export
ns <- function (obj) {
  names(obj) |> sort()
}

