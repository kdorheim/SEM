#' Check a vector for required contents
#'
#' This is an internal package function that is helpful to make sure
#' the conditions are met.
#'
#' @param req vector of the required contents
#' @param check vector that must contain the required contents
#' @return TRUE if the all required elements are within the check vector
#'  otherwise an error is thrown.
check_contents <- function(req, check) {
  assert_that(is.vector(req))
  assert_that(is.vector(check))
  missing <- setdiff(req, check)
  if (length(missing) == 0) {
    return(TRUE)
    } else {
      stop("following required contents not found: ",
           paste0(missing, collapse = ", "))
      }
}
