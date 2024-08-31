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
