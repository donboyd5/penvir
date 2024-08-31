

# A function that uses the frs environment
#' Title
#'
#' @param fund_env
#'
#' @return
#' @export
#'
#' @examples
pension_model <- function(fund_env) {
  benefits <- fund_env$calculate_benefits()
  funding <- fund_env$calculate_funding()

  list(
    benefits = benefits,
    funding = funding
  )
}


# devtools::document()
# devtools::install()
