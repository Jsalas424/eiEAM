#' Calculate Pooled Standard Deviation
#'
#' @description
#' Calculates the pooled standard deviation across multiple groups by combining
#' the within-group variances weighted by their degrees of freedom.
#'
#' @param var Numeric vector of variances for each group
#' @param n Numeric vector of sample sizes for each group
#' @param k Integer. Number of groups (defaults to length of var)
#'
#' @return A single numeric value representing the pooled standard deviation
#'
#' @examples
#' library(dplyr)
#' set.seed(42)
#' 
#' # Create sample data: 3 groups, 3 observations each
#' df <- data.frame(
#'   group = rep(c("A", "B", "C"), each = 3),
#'   value = c(10.2, 11.1, 9.8,    # Group A
#'             12.5, 13.0, 12.1,   # Group B  
#'             8.9, 9.5, 8.7)      # Group C
#' )
#' 
#' # Calculate pooled standard deviation
#' result <- df |>
#'   group_by(group) |>
#'   summarise(n = n(), variance = var(value), .groups = 'drop') |>
#'   summarise(pooled_sd = pooled_sd(variance, n))
#' 
#' print(result)
#'
#' @export
pooled_sd <- function(var, n, k = length(var)) {
  # Input validation
  if (any(var < 0)) {
    stop("All variances must be non-negative")
  }
  
  if (any(n <= 0)) {
    stop("All sample sizes must be positive")
  }
  
  # Calculate numerator and denominator
  numerator <- sum(var * (n - 1))
  denominator <- sum(n) - k
  
  if (denominator <= 0) {
    stop("Insufficient degrees of freedom for calculation")
  }
  
  sqrt(numerator / denominator)
}