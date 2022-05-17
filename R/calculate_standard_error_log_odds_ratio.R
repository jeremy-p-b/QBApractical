#' function to calculate bias standard error of log odds ratio
#' @details a function to calculate the standard error of the log risk ratio
#' @param num_cases_exposed Number of exposed cases
#' @param num_cases_unexposed Number of unexposed cases
#' @param num_controls_exposed Number of exposed controls
#' @param num_controls_unexposed Number of unexposed controls 

calculate_standard_error_log_odds_ratio <- function(num_cases_exposed, num_cases_unexposed,
                                                    num_controls_exposed, num_controls_unexposed) {
  variance <- 1/num_cases_exposed + 1/num_cases_unexposed - 1/num_controls_exposed - 1/num_controls_unexposed
  standard_error <- sqrt(variance)
  return(standard_error)
}