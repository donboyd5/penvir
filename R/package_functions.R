#' Check if Environment Exists
#'
#' Checks if an environment exists in the list of environments.
#'
#' @param env_name The name of the environment to check.
#' @param verbose If TRUE print message if environment does not exist.
#' @return TRUE or FALSE.
#' @noRd
environment_exists <- function(env_name, verbose = FALSE) {
  environments <- get_environments()
  environment_exists <- env_name %in% names(environments)

  if (!environment_exists && verbose) {
    message(paste0(
      "Environment '", env_name, "' does not exist. ",
      "Valid environments are: \n  ",
      paste(names(environments), collapse = ", "), "."
    ))
  }

  return(environment_exists)
}

#' Check the Status of All Environments
#'
#' Returns a tibble listing all environments defined in the package and their
#' status (either "Populated" or "Unpopulated"). The environments are sorted
#' alphabetically by name in the resulting tibble.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{env_name}{The name of the environment.}
#'   \item{status}{The status of the environment, either "Populated" or "Unpopulated".}
#' }
#'
#' @details
#' This function iterates over all environments in the `environments` list,
#' checks whether each environment is populated (contains any objects), and
#' returns a tibble that shows the status of each environment. The tibble
#' provides a clear overview of the current state of all environments, making it
#' easier to manage and track them.
#'
#' @examples
#' check_environment_status()
#'
#' @export
check_environment_status <- function() {
  environments <- get_environments()
  sorted_names <- names(environments) |> sort()

  environment_status <- tibble::tibble(
    env_name = sorted_names,
    status = sapply(sorted_names, function(env_name) {
      env <- environments[[env_name]]
      if (length(ls(envir = env)) == 0) {
        return("Unpopulated")
      } else {
        return("Populated")
      }
    })
  )

  return(environment_status)
}

#' Create a Clone of a Plan
#'
#' Creates a clone, or "deep copy," of a plan in the workspace, including all of
#' its elements. The clone is a separate copy of the original plan, meaning
#' changes to one will not affect the other. within it.
#'
#' @param plan Any plan in the workspace, as long as it is an environment.
#'
#' @return A new plan that is a deep copy of the specified plan, containing
#'   copies of all objects from the original plan.
#'
#' @details
#'
#' This function is useful when you need to work with a separate copy of a plan
#' without altering the original environment's contents. For example it would be
#' useful if you obtained the frs plan from the package, made some changes, want
#' to keep the saved version, and then create new plan based on the changed
#' plan.
#'
#'@examples
#' frs <- get_data("frs")
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
#' frs <- get_data("frs") # get a fresh version of the data
#' names(frs) # good, back to the original
#' # add something to frs2
#' frs2$x <- 7
#' names(frs2)
#' # did it affect frs? no. for many use cases, this is what we'll want
#' names(frs)
#' @export
clone_plan <- function(plan) {
  # check for existence and status as environment
  new_plan <- rlang::env_clone(plan, parent = emptyenv())
  return(new_plan)
}

#' Get and Populate a Named Environment
#'
#' Retrieves an environment by name, populates it with data and functions if it
#' is empty. The environment is assumed to be pre-defined in the package and
#' initially empty.
#'
#' @param env_name A character string naming the environment to retrieve and
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
#' frs <- get_data("frs")
#'
#' # Attempt to populate an already populated environment
#' frs <- get_data("frs") # This should indicate that 'frs' is already populated
#'
#' @export
get_data <- function(env_name) {
  .penvir_env <- get(".penvir_env", envir = parent.env(environment())) # Access .penvir_env

  stopifnot(environment_exists(env_name))

  populate(env_name)
  env <- get_env(env_name)

  return(rlang::env_clone(env, parent = emptyenv()))
}


#' Get a Specific Pension Fund Environment by Name
#'
#' Retrieves a specific environment from the list of environments.
#'
#' @param env_name The name of the environment to retrieve.
#' @return The specified environment.
#' @noRd
get_env <- function(env_name) {
  if (environment_exists(env_name)) {
    return(.penvir_env$environments[[env_name]])
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


#' Run a Pension Model on a Specified Environment
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
#' users to input different environments representing various pension funds.
#'
#' @examples
#' # initialize_environments()
#' frs <- get_data("frs")
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


#' Populate a Pension Fund Environment With Data
#'
#' Populates a pension fund environment with data and functions if it
#' is empty. The environment is assumed to be pre-defined in the package and
#' initially empty.
#'
#' @param env_name A character string naming the environment to retrieve and
#'   populate. The environment must exist in the `environments` list created
#'   during package initialization.
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
#' frs <- populate("frs")
#'
#' # Attempt to populate an already populated environment
#' frs <- get_data("frs") # This should indicate that 'frs' is already populated
#'
#' @export
populate <- function(env_name) {
  if (!environment_exists(env_name, verbose = TRUE)) {
    return(invisible(NULL))
  }

  env <- get_env(env_name)

  if (is_populated(env)) {
    # message(paste0("Environment ", env_name, " is already populated."))
    return(invisible(NULL))
  }

  message(paste0("Populating empty environment ", env_name, "..."))

  # Path to folder for data files associated with env_name
  folder_path <- fs::path_package("extdata", env_name, package = "penvir")

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


#' Reset a Named Environment to Its Empty State
#'
#' Resets a specified environment by removing all objects within it, effectively
#' returning it to its initial empty state. The environment must exist in the
#' `environments` list created during package initialization.
#'
#' @param env_name A character string naming the environment to reset. The
#'   environment must exist in the `environments` list.
#'
#' @details
#' The function checks if the specified environment exists in the `environments`
#' list. If the environment is found, all objects within it are removed, leaving
#' the environment empty. This function can be useful if you want to clear an
#' environment before repopulating it with new data or functions.
#'
#' After resetting the environment, a message is printed to confirm that the
#' environment has been cleared.
#'
#' @return Invisibly returns `NULL`. The function's primary purpose is to reset
#'   the environment.
#'
#' @examples
#' # initialize_environments()
#'
#' # Reset and repopulate the 'frs' environment
#' reset_env("frs")
#' get_data("frs")
#'
#' @export
reset_env <- function(env_name) {
  stopifnot(environment_exists(env_name))

  env <- get_env(env_name)

  rm(list = ls(envir = env), envir = env)

  message(paste0("Environment '", env_name, "' has been reset to empty."))
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

