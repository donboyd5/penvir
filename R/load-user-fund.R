#' Load a User-Provided Pension Fund
#'
#' Loads data for a user-provided pension fund from a specified folder path.
#' The folder should contain CSV files in a specified format.
#'
#' @param fund_name The name of the fund to load.
#' @param folder_path The path to the folder containing the fund data.
#'
#' @return Invisibly returns the loaded fund environment.
#'
#' @examples
#' load_user_fund("my_fund", "path/to/my_fund/data")
#' @export
load_user_fund <- function(fund_name, folder_path) {
  # Check if the fund already exists
  if (environment_exists(fund_name)) {
    stop(paste0("Fund '", fund_name, "' already exists."))
  }

  # Create a new environment for the fund
  new_env <- new.env(parent = emptyenv())
  assign(fund_name, new_env, envir = get(".penvir_env", envir = parent.env(environment()))$environments)

  # Load data from the folder
  fpath <- fs::path(folder_path, "beneficiaries.rds")
  if (file.exists(fpath)) {
    new_env$beneficiaries <- readRDS(fpath)
  } else {
    stop(paste0("File not found: ", fpath))
  }

  # Add a calculate_benefits function to the environment
  new_env$calculate_benefits <- function() {
    sum(new_env$beneficiaries$benefits)
  }

  return(invisible(NULL))
}
