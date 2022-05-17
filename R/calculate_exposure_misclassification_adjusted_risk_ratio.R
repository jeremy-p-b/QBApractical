#' function to calculate exposure misclassification adjusted risk ratio
#' @details a function to calculate exposure misclassification adjusted risk ratio
#' @param num_outcome_exposed Number of exposed group with outcome
#' @param num_exposed Number in the exposed group
#' @param num_outcome_unexposed Number of unexposed group with outcome
#' @param num_unexposed Number in the unexposed group
#' @param sensitivity Sensitivity of exposure reporting
#' @param specificity Specificity of exposure reporting

calculate_exposure_misclassification_adjusted_risk_ratio <- function(num_outcome_exposed, num_exposed, 
                                                                     num_outcome_unexposed, num_unexposed,
                                                                     sensitivity, specificity) {
  
  num_no_outcome_exposed <- num_exposed - num_outcome_exposed
  num_no_outcome_unexposed <- num_unexposed - num_outcome_unexposed
  num_outcome <- num_outcome_exposed + num_outcome_unexposed
  num_no_outcome <- num_no_outcome_exposed + num_no_outcome_unexposed
  
  corrected_num_outcome_exposed <- (num_outcome_exposed - num_outcome*(1-specificity))/
    (sensitivity-(1-specificity))
  corrected_num_outcome_unexposed <- num_outcome - corrected_num_outcome_exposed
  corrected_num_no_outcome_exposed <- (num_no_outcome_exposed - num_no_outcome*(1-specificity))/
    (sensitivity-(1-specificity))
  corrected_num_no_outcome_unexposed <- num_no_outcome - corrected_num_no_outcome_exposed
  
  bias_adj_risk_exposed <- (corrected_num_outcome_exposed/(corrected_num_no_outcome_exposed + corrected_num_outcome_exposed))
  bias_adj_risk_unexposed <- (corrected_num_outcome_unexposed/(corrected_num_no_outcome_unexposed + corrected_num_outcome_unexposed))
  bias_adj_risk_ratio <- bias_adj_risk_exposed / bias_adj_risk_unexposed
  return(bias_adj_risk_ratio)
}
