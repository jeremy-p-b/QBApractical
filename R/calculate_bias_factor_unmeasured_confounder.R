#' function to calculate bias factor for unmeasured confounder
#' @details a function to calculate the bias factor for risk ratio with a binary unmeasured confounder
#' @param prev_unexposed Prevalence of unmeasured confounder amongst unexposed
#' @param prev_exposed Prevalence of unmeasured confounder amongst exposed
#' @param risk_ratio Risk ratio between unmeasured confounder and outcome

calculate_bias_factor_unmeasured_confounder <- function(prev_unexposed, prev_exposed, risk_ratio) {
  bias_factor <- ((1-prev_exposed) + prev_exposed*risk_ratio)/
    ((1-prev_unexposed) + prev_unexposed*risk_ratio)
  return(bias_factor)
}