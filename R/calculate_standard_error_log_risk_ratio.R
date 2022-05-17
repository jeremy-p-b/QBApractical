#' function to calculate bias standard error of log risk ratio
#' @details a function to calculate the standard error of the log risk ratio
#' @param num_outcome_exposed Number of exposed group with outcome
#' @param num_exposed Number in the exposed group
#' @param num_outcome_unexposed Number of unexposed group with outcome
#' @param num_unexposed Number in the unexposed group

calculate_standard_error_log_risk_ratio <- function(num_outcome_exposed, num_exposed, num_outcome_unexposed, num_unexposed) {
  variance <- 1/num_outcome_exposed + 1/num_outcome_unexposed - 1/num_exposed - 1/num_unexposed
  standard_error <- sqrt(variance)
  return(standard_error)
}