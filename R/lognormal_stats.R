#' Log-Normal Distribution Statistics
#'
#' @description
#' Functions to calculate arithmetic mean, variance, and standard deviation
#' for log-normally distributed variables.
#'
#' @param x A numeric vector of positive values
#'
#' @return A single numeric value
#'
#' @examples
#' # Generate 30 samples from a log-normal distribution with arithmetic mean = 1
#' set.seed(123)  # For reproducible results
#' 
#' # For log-normal with arithmetic mean = 1, we need:
#' # exp(mu + sigma^2/2) = 1, so mu = -sigma^2/2
#' # Let's use sigma = 0.5
#' sigma <- 0.5
#' mu <- -sigma^2/2
#' 
#' # Generate log-normal samples
#' lognormal_samples <- rlnorm(30, meanlog = mu, sdlog = sigma)
#' 
#' # Verify the arithmetic mean is approximately 1
#' mean(lognormal_samples)
#' 
#' # Calculate log-normal statistics using our functions
#' lognormal_mean(lognormal_samples)
#' lognormal_var(lognormal_samples)
#' lognormal_sd(lognormal_samples)
#' 
#' # Compare with simple sample statistics
#' cat("Sample mean:", mean(lognormal_samples), "\n")
#' cat("Log-normal mean estimate:", lognormal_mean(lognormal_samples), "\n")
#' cat("Sample variance:", var(lognormal_samples), "\n") 
#' cat("Log-normal variance estimate:", lognormal_var(lognormal_samples), "\n")
#'
#' @name lognormal_stats
NULL

#' @rdname lognormal_stats
#' @export
lognormal_mean <- function(x) {
  if (any(x <= 0)) stop("All values must be positive for log-normal calculations")
  log_x <- log(x)
  exp(mean(log_x) + 0.5 * stats::var(log_x))
}

#' @rdname lognormal_stats
#' @export
lognormal_var <- function(x) {
  if (any(x <= 0)) stop("All values must be positive for log-normal calculations")
  log_x <- log(x)
  mean_log_x <- mean(log_x)
  var_log_x <- stats::var(log_x)
  (exp(var_log_x) - 1) * exp(2 * mean_log_x + var_log_x)
}

#' @rdname lognormal_stats
#' @export
lognormal_sd <- function(x) {
  sqrt(lognormal_var(x))
}