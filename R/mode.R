#' Statistical Mode, not to be confused with base::mode()
#'
#' @description Return the most frequent value (statistical mode) in a vector.
#'
#' @param x A vector (numeric, character, or factor).
#'
#' @return A single value: the modal value of `x`. If there are ties, the first
#' value achieving the maximum frequency is returned.
#'
#' @export
#'
#' @examples
#' # Create sample data
#' x <- c(1, 2, 2, 3, 4, 2, 5)
#' Mode(x)
#'
#' # Works with character/factor too
#' y <- c("a", "b", "a", "c")
#' Mode(y)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
