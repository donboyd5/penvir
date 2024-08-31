.onLoad <- function(libname, pkgname) {
  # put this into zzz.R when package is further along
  initialize_environments()
}


#' Check the Status of All Environments
#'
#' Returns a tibble listing all environments defined in the package and their status
#' (either "Populated" or "Unpopulated"). The environments are sorted alphabetically
#' by name in the resulting tibble.
#'
#' @return A tibble with two columns:
#' \describe{
#'   \item{env_name}{The name of the environment.}
#'   \item{status}{The status of the environment, either "Populated" or "Unpopulated".}
#' }
#'
#' @details
#' This function iterates over all environments in the `environments` list, checks whether
#' each environment is populated (contains any objects), and returns a tibble that shows
#' the status of each environment. The tibble provides a clear overview of the current state
#' of all environments, making it easier to manage and track them.
#'
#' @examples
#' initialize_environments()
#' check_env_status()
#'
#' @export
check_env_status <- function() {
  environments <- get_environments()
  env_status <- tibble::tibble(
    env_name = names(environments) |> sort(),
    status = sapply(names(environments), function(env_name) {
      env <- environments[[env_name]]
      if (length(ls(envir = env)) == 0) {
        return("Unpopulated")
      } else {
        return("Populated")
      }
    })
  )

  return(env_status)
}



#' Create a Deep Copy of an Environment
#'
#' Creates a deep copy of the specified environment, including all objects within it.
#' The new environment is independent of the original, meaning changes to one will
#' not affect the other.
#'
#' @param env_name A character string naming the environment to be copied.
#'   The environment must exist in the `environments` list created during package initialization.
#'
#' @return A new environment that is a deep copy of the specified environment, containing
#'   copies of all objects from the original environment.
#'
#' @details
#' The function retrieves the specified environment by name, converts it into a list,
#' and then creates a new environment using this list. The result is a completely independent
#' environment that can be modified without affecting the original.
#'
#' This function is useful when you need to work with a separate copy of an environment
#' without altering the original environment's contents.
#'
#' @examples
#' initialize_environments()
#' get_data("frs")
#' frs_copy <- deep_copy_env("frs")
#' frs_copy$new_data <- 42
#'
#' @export
deep_copy_env <- function(env_name) {
  env <- get_env(env_name)
  env_list <- as.list(env, all.names = TRUE)
  new_env <- list2env(env_list, parent = emptyenv())
  return(new_env)
}


#' Expose an Environment
#'
#' Returns a specified environment, allowing the user to access and modify it.
#' This function does not modify the global environment. Users can choose to
#' assign the returned environment to the global environment if needed.
#'
#' @param env_name The name of the environment to return. The environment must
#'   exist in the list of environments initialized by the package.
#'
#' @return The specified environment. The environment is not assigned to the global
#'   environment automatically; it is returned and can be assigned by the user if desired.
#'
#' @examples
#' initialize_environments()
#'
#' # Retrieve and expose the 'frs' environment
#' frs_env <- expose_environment("frs")
#'
#' # Optionally assign it to the global environment
#' frs <- frs_env
#'
#' # Access and modify the environment
#' print(ls(envir = frs))
#'
#' @export
expose_environment <- function(env_name) {
  get_env(env_name)
}



#' Get and Populate a Named Environment
#'
#' Retrieves an environment by name, populates it with data and functions if it is empty.
#' The environment is assumed to be pre-defined in the package and initially empty.
#'
#' @param env_name A character string naming the environment to retrieve and populate.
#'   The environment must exist in the `environments` list created during package initialization.
#' @param expose A logical value indicating whether the populated environment should
#'   be assigned to the global environment. Default is `FALSE`.
#'
#' @details
#' The function first checks if the specified environment exists in the `environments` list.
#' If it does not exist, an error is thrown. If the environment is empty, the function attempts
#' to populate it using data from the package's `extdata` directory. If `expose = TRUE`, the environment
#' will be returned but not assigned to the global environment.
#'
#' @return Invisibly returns the environment after populating it.
#'
#' @examples
#' initialize_environments()
#' frs <- get_data("frs")
#' expose_environment("frs") # If needed
#'
#' @export
get_data <- function(env_name, expose = FALSE) {
  environments <- get_environments()

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

    env$calculate_benefits <- function() {
      sum(env$beneficiaries$benefits)
    }
  } else {
    print(paste0("Environment ", env_name, " is already populated."))
    cat(paste0('Use reset_env("', env_name, '") to clear the environment before populating.\n'))
    return(invisible(env))
  }

  if (expose) {
    return(invisible(env))
  }

  return(env)
}




#' Get a Specific Environment by Name
#'
#' Retrieves a specific environment from the list of environments.
#'
#' @param env_name The name of the environment to retrieve.
#' @return The specified environment.
#' @export
get_env <- function(env_name) {
  environments <- get_environments()
  environments[[env_name]]
}


#' Get the List of Environments
#'
#' Retrieves the list of environments stored in the package-specific environment.
#'
#' @return The list of environments.
#' @export
get_environments <- function() {
  get("environments", envir = .penvir_env)
}




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
#' ensuring isolation. The list of environments is stored in a package-specific environment,
#' making it accessible throughout the package but not cluttering the global environment.
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
  # Create a package-specific environment to hold all environments
  .penvir_env <<- new.env(parent = emptyenv())

  environments <- list(
    frs = new.env(parent = emptyenv()),
    trs = new.env(parent = emptyenv())
    # Add more environments as needed
  )

  # Store the environments list in the package environment
  assign("environments", environments, envir = .penvir_env)
}



#' Run a Pension Model on a Specified Environment
#'
#' Executes a pension model using the functions and data contained within a specified
#' environment. The function currently calculates the benefits associated with the
#' pension fund and is designed to be extended with additional calculations, such as funding.
#'
#' @param fund_env An environment containing the necessary data and functions for the pension model.
#'   The environment is expected to include a `calculate_benefits` function, and may also contain
#'   other functions and data relevant to pension modeling.
#'
#' @return A list with the results of the pension model. Currently, the list contains:
#' \describe{
#'   \item{benefits}{The calculated benefits from the `calculate_benefits` function.}
#' }
#'
#' @details
#' The `pension_model` function is designed to operate on a specific environment representing
#' a pension fund. The environment should include a `calculate_benefits` function that returns
#' the total benefits for the fund. The function is structured to allow for easy extension
#' with additional calculations, such as funding, which can be uncommented or added as needed.
#'
#' This function provides a flexible framework for pension modeling, allowing users to
#' input different environments representing various pension funds.
#'
#' @examples
#' initialize_environments()
#' frs <- get_data("frs")
#' results <- pension_model(frs)
#' print(results$benefits)
#'
#' @export
pension_model <- function(fund_env) {
  benefits <- fund_env$calculate_benefits()
  # funding <- fund_env$calculate_funding()

  list(
    benefits = benefits
  )
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
#' reset_env("frs")
#'
#' @export
reset_env <- function(env_name) {
  environments <- get_environments()

  if (!env_name %in% names(environments)) {
    stop(paste0("Environment ", env_name, " does not exist"))
  }

  env <- environments[[env_name]]

  # Clear all objects from the environment
  rm(list = ls(envir = env), envir = env)

  message(paste0("Environment '", env_name, "' has been reset to empty."))
}
