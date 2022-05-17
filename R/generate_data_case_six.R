#' function to generate data with selection bias
#' @details a function to simulate data with selection bias

generate_data_case_six <- function() {
  require(tidyverse)
  
  # simulate data
  sample_size <- 10000
  sex <- as.integer(runif(sample_size) < 0.5)
  smoker <- as.integer(runif(sample_size) < 0.2)
  copd <- as.integer(runif(sample_size) < ((exp(log(0.01) + log(5)*smoker))/
                              (1 + exp(log(0.01) + log(5)*smoker))))
  age <- (rbeta(sample_size, 3, 2) * 60) + 18

  selected <- smoker*copd*(runif(sample_size) < 0.9) + 
    (1-smoker)*copd*(runif(sample_size) <  0.8) +
    (1-copd)*(smoker)*(runif(sample_size) < 0.1) + 
    (1-copd)*(1-smoker)*(runif(sample_size) < 0.05)
  
  # label data
  smoker <- factor(smoker, levels=c(0,1), labels=c("No", "Yes"))
  sex <- factor(sex, levels=c(0,1), labels=c("Female", "Male"))
  copd <- factor(copd, levels=c(0,1), labels=c("No", "Yes"))
  
  # combine data
  sim_data <- tibble(smoker, sex, copd, age, selected)
  
  # keep selected data
  sim_data <- sim_data %>% filter(selected == 1) %>% select(!selected)
  
  return(sim_data)
}