#' function to generate misclassified data
#' @details a function to simulated misclassified data

generate_data_case_five <- function() {
  require(tidyverse)
  
  # simulate data
  sample_size <- 100000
  sex <- as.integer(runif(sample_size) < 0.5)
  high_fat_diet <- as.integer(runif(sample_size) < 0.1)
  coronary_heart_disease <- as.integer(runif(sample_size) < exp(log(0.01) + log(3)*high_fat_diet))
  age <- (rbeta(sample_size, 3, 2) * 60) + 18
  reported_high_fat_diet <- high_fat_diet*(runif(sample_size) < 0.7) + (1-high_fat_diet)*(runif(sample_size) < 0.1)
  
  # label data
  high_fat_diet <- factor(high_fat_diet, levels=c(0,1), labels=c("No", "Yes"))
  reported_high_fat_diet <- factor(reported_high_fat_diet, levels=c(0,1), labels=c("No", "Yes"))
  sex <- factor(sex, levels=c(0,1), labels=c("Female", "Male"))
  coronary_heart_disease <- factor(coronary_heart_disease, levels=c(0,1), labels=c("No", "Yes"))
  
  # combine data
  sim_data <- tibble(reported_high_fat_diet, sex, coronary_heart_disease, age)
  
  return(sim_data)
}