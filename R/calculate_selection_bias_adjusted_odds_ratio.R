#' function to calculate bias-adjusted odds ratio
#' @details a function to calculate the bias-adjusted odds ratio
#' @param pselect_unexposed_case probability of selection of unexposed case
#' @param pselect_unexposed_control probability of selection of unexposed control
#' @param pselect_exposed_case probability of selection of exposed case
#' @param pselect_exposed_control probability of selection of exposed control
#' @param observed_odds_ratio observed odds ratio

calculate_selection_bias_adjusted_odds_ratio <- function(pselect_unexposed_case,
                                                         pselect_unexposed_control,
                                                         pselect_exposed_case,
                                                         pselect_exposed_control,
                                                         observed_odds_ratio) {
  bias_adjusted_odds_ratio <- observed_odds_ratio *
    (pselect_unexposed_case*pselect_exposed_control)/
    (pselect_exposed_case*pselect_unexposed_control)
  return(bias_adjusted_odds_ratio)
}