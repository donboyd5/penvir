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
#' # Initialize environments first (done in the package in .onLoad())
#' initialize_environments()
#'
#' # Check the status of all environments
#' check_env_status()
#'
#' @export
check_env_status <- function() {
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
#' @param env An environment to be copied. This environment may contain any number of objects,
#'   including hidden ones.
#'
#' @return A new environment that is a deep copy of the input environment, containing
#'   copies of all objects from the original environment.
#'
#' @details
#' The function converts the original environment into a list, ensuring that all objects,
#' including hidden ones, are included. It then creates a new environment using this list,
#' with an empty parent environment. The result is a completely independent environment
#' that can be modified without affecting the original.
#'
#' This function is useful when you need to work with a separate copy of an environment
#' without altering the original environment's contents.
#'
#' @examples
#' # Initialize environments first (done in the package in .onLoad())
#' initialize_environments()
#' get_data("frs")
#'
#' # Create a deep copy of the 'frs' environment
#' frs_copy <- deep_copy_env(frs)
#'
#' # Modify the copy without affecting the original
#' frs_copy$new_data <- 42
#'
#' @export
deep_copy_env <- function(env) {
  # Convert the environment to a list, including all objects (even hidden ones)
  env_list <- as.list(env, all.names = TRUE)

  # Create a new environment from this list
  new_env <- list2env(env_list, parent = emptyenv())

  return(new_env)
}

