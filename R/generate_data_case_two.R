#' function to generate misclassified data
#' @details a function to simulated misclassified data

generate_data_case_two <- function() {
  require(tidyverse)
  
  # simulate data
  sample_size <- 100000
  sex <- as.integer(runif(sample_size) < 0.5)
  smoker <- as.integer(runif(sample_size) < 0.1)
  lung_cancer <- as.integer(runif(sample_size) < exp(log(0.01) + log(15)*smoker))
  age <- (rbeta(sample_size, 3, 2) * 60) + 18
  self_reported_smoker <- smoker*(runif(sample_size) < 0.8)
  
  # label data
  smoker <- factor(smoker, levels=c(0,1), labels=c("No", "Yes"))
  self_reported_smoker <- factor(self_reported_smoker, levels=c(0,1), labels=c("No", "Yes"))
  sex <- factor(sex, levels=c(0,1), labels=c("Female", "Male"))
  lung_cancer <- factor(lung_cancer, levels=c(0,1), labels=c("No", "Yes"))
  
  # combine data
  sim_data <- tibble(self_reported_smoker, sex, lung_cancer, age)
  
  return(sim_data)
}