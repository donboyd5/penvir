# In another R script

#' Title
#'
#' @return
#' @export
#'
#' @examples
initialize_frs <- function() {
  print("populating frs")
  if (length(ls(envir = frs)) == 0) {
    # Example data
    frs$beneficiaries <- data.frame(
      id = 1:3,
      name = c("Alice", "Bob", "Charlie"),
      benefit = c(1000, 1500, 1200)
    )

    frs$contributions <- data.frame(
      id = 1:3,
      contribution = c(500, 800, 600)
    )

    # Example functions
    frs$calculate_benefits <- function() {
      sum(frs$beneficiaries$benefit)
    }

    frs$calculate_funding <- function() {
      sum(frs$contributions$contribution) - frs$calculate_benefits()
    }
  }
}


#' Title
#'
#' @return
#' @export
#'
#' @examples
# populate <- function(env) {
#   env_name <- deparse(substitute(env))
#   print(paste0("possibly will populate ", env_name))
#   if (length(ls(envir = env)) == 0) {
#
#     print(paste0("env empty so now trying to populate ", env_name))
#
#     # Example data
#
#     # fpath <- system.file("extdata", env_name, "beneficiaries.rds", package = "penvir")
#     fpath <- fs::path_package("extdata", env_name, "beneficiaries.rds", package = "penvir")
#
#     env$beneficiaries <- readRDS(fpath)
#
#     # Example functions
#     env$calculate_benefits <- function() {
#       sum(env$beneficiaries$benefits)
#     }
#
#   }
# }
#
#
#
populate <- function(env_name) {
  # Check if the environment exists in package_envs
  if (!env_name %in% names(package_envs)) {
    stop(paste0("Environment ", env_name, " does not exist in package_envs"))
  }

  env <- package_envs[[env_name]]

  print(paste0("Possibly will populate ", env_name))
  if (length(ls(envir = env)) == 0) {

    print(paste0("Environment empty, so now trying to populate ", env_name))

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
}




#' Title
#'
#' @return
#' @export
#'
#' @examples
reset <- function(env) {
  if (!is.environment(env)) {
    stop("Argument must be an environment")
  }

  # Get all object names in the environment
  obj_names <- ls(envir = env, all.names = TRUE)

  # Remove all objects
  rm(list = obj_names, envir = env)

  # Return the now-empty environment (invisible)
  invisible(env)
}



