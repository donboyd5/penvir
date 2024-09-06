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
