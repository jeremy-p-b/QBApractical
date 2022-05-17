if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  EValue,
  here,
  janitor,
  tidyverse)

source(here::here("R", "generate_data_case_one.R"))
source(here::here("R", "generate_data_case_two.R"))
source(here::here("R", "generate_data_case_three.R"))
source(here::here("R", "generate_data_case_four.R"))
source(here::here("R", "generate_data_case_five.R"))
source(here::here("R", "generate_data_case_six.R"))
source(here::here("R", "calculate_bias_factor_unmeasured_confounder.R"))
source(here::here("R", "calculate_standard_error_log_risk_ratio.R"))
source(here::here("R", "calculate_standard_error_log_odds_ratio.R"))
source(here::here("R", "calculate_exposure_misclassification_adjusted_risk_ratio.R"))
source(here::here("R", "calculate_selection_bias_adjusted_odds_ratio.R"))

# Case Study 1: Unmeasured Confounding ------------------------------------

# We will examine the confounded association between alcohol consumption and liver cancer in simulated data.
# Imagine we conduct a cohort study where we collect information on alcohol consumption, age and sex and baseline 
# as well as liver cancer risk over two years follow-up. We do not have available an unmeasured confounder: smoking status.

# generate data
case_one_data <- generate_data_case_one()

# explore data
summary(case_one_data)

# create a two by two table for heavy drinking and liver cancer
case_one_data %>% tabyl(heavy_drinker, liver_cancer) %>% adorn_totals(where="col")

# calculate a crude risk ratio
case_one_risk_unexposed <- case_one_data %>% 
  filter(heavy_drinker == "No") %>%
  {mean(.[["liver_cancer"]] == "Yes")}

case_one_risk_exposed <- case_one_data %>% 
  filter(heavy_drinker == "Yes") %>%
  {mean(.[["liver_cancer"]] == "Yes")}

case_one_crude_risk_ratio <- case_one_risk_exposed / case_one_risk_unexposed
print(case_one_crude_risk_ratio)

# calculate crude 95% confidence interval

num_outcome_exposed <- case_one_data %>% 
  filter(heavy_drinker == "Yes", liver_cancer == "Yes") %>% nrow()
num_outcome_unexposed <- case_one_data %>%
  filter(heavy_drinker == "No", liver_cancer == "Yes") %>% nrow()
num_exposed <- case_one_data %>%
  filter(heavy_drinker == "Yes") %>% nrow()
num_unexposed <- case_one_data %>%
  filter(heavy_drinker == "No") %>% nrow()

standard_error_log_risk_ratio <- calculate_standard_error_log_risk_ratio(num_outcome_exposed, 
                                                                         num_exposed, 
                                                                         num_outcome_unexposed, 
                                                                         num_unexposed)
case_one_crude_lower_ci <- (log(case_one_crude_risk_ratio) - 1.96*standard_error_log_risk_ratio) %>% exp()
case_one_crude_upper_ci <- (log(case_one_crude_risk_ratio) + 1.96*standard_error_log_risk_ratio) %>% exp()

# Smoking status (smoker/non-smoker) is an unmeasured confounding variable. 
# The association between smoking and liver cancer is risk ratio 2.
# The prevalence of smoking is 0.1 among not heavy drinkers and 0.5 among heavy drinkers.
# We can use this information to calculate a bias-adjusted risk ratio.

case_one_bias_factor <- calculate_bias_factor_unmeasured_confounder(0.1, 0.5, 2)
case_one_bias_adjusted <- case_one_crude_risk_ratio / case_one_bias_factor
print(case_one_crude_risk_ratio) 
print(case_one_bias_adjusted)

# We can apply this bias-adjustment to the limits of the confidence interval as well.
case_one_bias_adjusted_lower_ci <- case_one_crude_lower_ci / case_one_bias_factor
case_one_bias_adjusted_upper_ci <- case_one_crude_upper_ci / case_one_bias_factor
print(case_one_bias_adjusted_lower_ci)
print(case_one_bias_adjusted_upper_ci)

# Next we calculate an E-value - the smallest value that either the risk ratio between 
# unmeasured confounder and outcome or the risk ratio between exposure and unmeasured confounder 
# must have to potentially reduce the observed risk ratio to a true bias-adjusted risk ratio of 2
case_one_evalue <- evalues.RR(est = case_one_crude_risk_ratio, true=2)
print(case_one_evalue)

# We can also calculate the smallest value neccesary to potentially reduce the observed risk ratio to the null (i.e. 1)
case_one_null_evalue <- evalues.RR(est = case_one_crude_risk_ratio, true=1)

# We can plot the minimum values for the association between exposure and unmeasured confounder and 
# unmeasured confounder and outcome 
bias_plot(RR=case_one_crude_risk_ratio, xmax=20)

# For more sophisticated graphs we can use the online E-value calculator: 
# https://www.evalue-calculator.com/evalue/

# Case Study 2: Exposure Misclassification  ------------------------------------

# We will now take a look at the association between smoking and lung cancer. Imagine we conduct a cohort study looking at
# the association between smoking and lung cancer. We ascertain self-reported smoking status at baseline.
# We assume no unmeasured confounding, but that some smokers misreport their smoking status.

# generate data
case_two_data <- generate_data_case_two()

# explore data
summary(case_two_data)

# create a two-by-two table for self-reported smoking and lung cancer
case_two_data %>% tabyl(self_reported_smoker, lung_cancer) %>% adorn_totals(where="col")

# calculate a crude risk ratio
case_two_risk_unexposed <- case_two_data %>% 
  filter(self_reported_smoker == "No") %>%
  {mean(.[["lung_cancer"]] == "Yes")}

case_two_risk_exposed <- case_two_data %>% 
  filter(self_reported_smoker == "Yes") %>%
  {mean(.[["lung_cancer"]] == "Yes")}

case_two_crude_risk_ratio <- case_two_risk_exposed / case_two_risk_unexposed
print(case_two_crude_risk_ratio)

# calculate crude confidence interval

num_outcome_exposed <- case_two_data %>% 
  filter(self_reported_smoker == "Yes", lung_cancer == "Yes") %>% nrow()
num_outcome_unexposed <- case_two_data %>%
  filter(self_reported_smoker == "No", lung_cancer == "Yes") %>% nrow()
num_exposed <- case_two_data %>% 
  filter(self_reported_smoker == "Yes") %>% nrow()
num_unexposed <- case_two_data %>%
  filter(self_reported_smoker == "No") %>% nrow()

standard_error_log_risk_ratio <- calculate_standard_error_log_risk_ratio(num_outcome_exposed, 
                                                                         num_exposed, 
                                                                         num_outcome_unexposed, 
                                                                         num_unexposed)
case_two_crude_lower_ci <- (log(case_two_crude_risk_ratio) - 1.96*standard_error_log_risk_ratio) %>% exp()
case_two_crude_upper_ci <- (log(case_two_crude_risk_ratio) + 1.96*standard_error_log_risk_ratio) %>% exp()
print(case_two_crude_lower_ci)
print(case_two_crude_upper_ci)

# For the bias analysis we will assume that:
# - misclassification depends on exposure but not outcome
# - all those who are non-smokers report to be non-smokers - specificity is 100%
# - not all of those who are smokers report to be smokers - sensitivity is 80%
assumed_specificity <- 1
assumed_sensitivity <- 0.8

case_two_bias_adjusted_risk_ratio <- calculate_exposure_misclassification_adjusted_risk_ratio(num_outcome_exposed, 
                                                         num_exposed,
                                                         num_outcome_unexposed, 
                                                         num_unexposed,
                                                         assumed_sensitivity, 
                                                         assumed_specificity)
print(case_two_bias_adjusted_risk_ratio)
# Case Study 3: Selection bias -------------------------------------------------

# For case study 3 imagine we have data from a hospital-based case-control study of the 
# association between high blood pressure and stroke. In this hospital-based case-control
# study we capture all cases from the population, but controls are not a representative
# sample of non-cases from the population, but instead are less healthy and have higher
# blood pressure.

# generate data
case_three_data <- generate_data_case_three()

# explore data
summary(case_three_data)

# create a two-by-two table for self-reported smoking and lung cancer
case_three_data %>% tabyl(high_blood_pressure, stroke) %>% adorn_totals(where="col")

# calculate a crude odds ratio
num_cases_exposed <- case_three_data %>% 
  filter(stroke=="Yes", high_blood_pressure=="Yes") %>% 
  nrow()
num_cases_unexposed <- case_three_data %>% 
  filter(stroke=="Yes", high_blood_pressure=="No") %>% 
  nrow()
num_controls_exposed <- case_three_data %>% 
  filter(stroke=="No", high_blood_pressure=="Yes") %>% 
  nrow()
num_controls_unexposed <- case_three_data %>% 
  filter(stroke=="No", high_blood_pressure=="No") %>% 
  nrow()

case_three_crude_odds_ratio <- (num_cases_exposed/num_cases_unexposed)/
  (num_controls_exposed/num_controls_unexposed)
print(case_three_crude_odds_ratio)

# calculate 95% CI
standard_error_log_odds_ratio <- calculate_standard_error_log_odds_ratio(num_cases_exposed, 
                                                                         num_cases_unexposed,
                                                                         num_controls_exposed,
                                                                         num_controls_unexposed)
case_three_lower_ci <- (log(case_three_crude_odds_ratio) - 1.96*standard_error_log_odds_ratio) %>% exp()
case_three_upper_ci <- (log(case_three_crude_odds_ratio) + 1.96*standard_error_log_odds_ratio) %>% exp()
print(case_three_lower_ci)
print(case_three_upper_ci)

# calculate bias adjusted odds ratio. 
# We will assume that:
# - all cases have been selected
# - 10% of eligible controls in the population with high blood pressure have been selected (i.e. hospitalised)
# - 5% of eligible controls in the population with low blood pressure have been selected 

case_three_bias_adjusted_odds_ratio <- calculate_selection_bias_adjusted_odds_ratio(1, 
                                                                                    0.05,
                                                                                    1,
                                                                                    0.1,
                                                                                    case_three_crude_odds_ratio)
print(case_three_bias_adjusted_odds_ratio)

# We can apply this to the limits of the confidence interval too
case_three_bias_adjusted_lower_ci <- calculate_selection_bias_adjusted_odds_ratio(1, 
                                             0.05,
                                             1,
                                             0.1,
                                             case_three_lower_ci)
case_three_bias_adjusted_upper_ci <- calculate_selection_bias_adjusted_odds_ratio(1, 
                                             0.05,
                                             1,
                                             0.1,
                                             case_three_upper_ci)
print(case_three_bias_adjusted_lower_ci)
print(case_three_bias_adjusted_upper_ci)

# Case Study 4: More confounding -------------------------------------------------
# For case study 4 imagine we conduct a cohort study to investigate the association
# between a high salt diet and stroke, but don't collect information on the confounder
# of exercise level. 

# Estimate a bias-adjusted risk ratio
# We assume that the association between low exercise and stroke is 
# RR 2 and that the prevalence of low exercise is 0.6 amongst those with a high salt diet
# and 0.1 among those without a high salt diet.

case_four_data <- generate_data_case_four()


# Case Study 5: More misclassification -------------------------------------------
# For case study 5 imagine we conduct a cohort study to investigate the association 
# between a high fat diet and coronary heart disease, but some individuals misreport 
# their diet. 

# Estimate a bias-adjusted risk ratio
# Assume that sensitivity is 0.7 and specificity is 0.9

case_five_data <- generate_data_case_five()

# Case Study 6: More selection bias -------------------------------------------
# For case study 6 imagine we conduct a hospital-based case-control study to investigate 
# the association between smoking and chronic obstructive pulmonary disease

case_six_data <- generate_data_case_six()


# Estimate a bias-adjusted odds ratio
# Assume that the probability of selection is 0.9 for exposed case, 0.8 for unexposed cases,
# 0.1 for exposed controls, and 0.05 for unexposed controls


