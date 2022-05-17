#' function to generate confounded data
#' @details a function to simulated confounded data

generate_data_case_one <- function() {
  require(tidyverse)

  # simulate data
  sample_size <- 100000
  sex <- as.integer(sample_size < 0.5)
  heavy_drinker <- as.integer(runif(sample_size) < 0.1)
  smoker <- as.integer(runif(sample_size) < exp(log(0.1) + log(5)*heavy_drinker))
  liver_cancer <- as.integer(runif(sample_size) < exp(log(0.01) + log(2)*smoker + log(2)*heavy_drinker))
  age <- (rbeta(sample_size, 3, 2) * 60) + 18
  
  # label data
  smoker <- factor(smoker, levels=c(0,1), labels=c("No", "Yes"))
  heavy_drinker <- factor(heavy_drinker, levels=c(0,1), labels=c("No", "Yes"))
  sex <- factor(sex, levels=c(0,1), labels=c("Female", "Male"))
  liver_cancer <- factor(liver_cancer, levels=c(0,1), labels=c("No", "Yes"))
  
  # combine data
  sim_data <- tibble(heavy_drinker, sex, liver_cancer, age)
  
  return(sim_data)
}