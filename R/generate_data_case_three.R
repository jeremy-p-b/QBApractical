#' function to generate data with selection bias
#' @details a function to simulate data with selection bias

generate_data_case_three <- function() {
  require(tidyverse)
  
  # simulate data
  sample_size <- 10000
  sex <- as.integer(runif(sample_size) < 0.5)
  high_blood_pressure <- as.integer(runif(sample_size) < 0.2)
  stroke <- as.integer(runif(sample_size) < ((exp(log(0.01) + log(3)*high_blood_pressure))/
                              (1 + exp(log(0.01) + log(3)*high_blood_pressure))))
  age <- (rbeta(sample_size, 3, 2) * 60) + 18

  selected <- stroke*1 + (1-stroke)*(high_blood_pressure)*(runif(sample_size) < 0.1) + 
    (1-stroke)*(1-high_blood_pressure)*(runif(sample_size) < 0.05)
  
  # label data
  high_blood_pressure <- factor(high_blood_pressure, levels=c(0,1), labels=c("No", "Yes"))
  sex <- factor(sex, levels=c(0,1), labels=c("Female", "Male"))
  stroke <- factor(stroke, levels=c(0,1), labels=c("No", "Yes"))
  
  # combine data
  sim_data <- tibble(high_blood_pressure, sex, stroke, age, selected)
  
  # keep selected data
  sim_data <- sim_data %>% filter(selected == 1) %>% select(!selected)
  
  return(sim_data)
}