#' function to generate confounded data
#' @details a function to simulated confounded data

generate_data_case_four<- function() {
  require(tidyverse)

  # simulate data
  sample_size <- 100000
  sex <- as.integer(sample_size < 0.5)
  high_salt <- as.integer(runif(sample_size) < 0.1)
  little_exercise <- as.integer(runif(sample_size) < exp(log(0.1) + log(6)*high_salt))
  stroke <- as.integer(runif(sample_size) < exp(log(0.01) + log(2)*little_exercise + log(1.5)*high_salt))
  age <- (rbeta(sample_size, 3, 2) * 60) + 18
  
  # label data
  high_salt <- factor(high_salt, levels=c(0,1), labels=c("No", "Yes"))
  little_exercise <- factor(little_exercise, levels=c(0,1), labels=c("No", "Yes"))
  sex <- factor(sex, levels=c(0,1), labels=c("Female", "Male"))
  stroke <- factor(stroke, levels=c(0,1), labels=c("No", "Yes"))
  
  # combine data
  sim_data <- tibble(high_salt, sex, stroke, age)
  
  return(sim_data)
}