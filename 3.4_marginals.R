
library(tidyverse)
library(emmeans)


fit_check <- readRDS(file.path("workspace", "fit_hc_parac_check.RData"))
p_check <- readRDS(file.path("workspace", "p_values_hc_parac_check.RData"))

fit_ambulant <- readRDS(file.path("workspace", "fit_hc_ambulant_parac.RData"))
p_ambulant <- readRDS(file.path("workspace", "p_values_hc_ambulant_parac.RData"))

fit_inpatient <- readRDS(file.path("workspace", "fit_hc_inpatient_parac.RData"))
p_inpatient <- readRDS(file.path("workspace", "p_values_hc_inpatient_parac.RData"))

best_vars <- readRDS(file.path("workspace", "best_vars.RData"))



# Function to obtain predicted marginal means and p values -----------------------------

get_marginals <- function(categories, .fit, .p_vals) {
  
  pmms <- emmeans(.fit, categories, type = "response", weights = "proportional") %>% 
    as_tibble() %>% 
    rename(category = 1) %>% 
    mutate(category = as.character(category)) %>% 
    select(category, prob, lower = asymp.LCL, upper = asymp.UCL) %>% 
    mutate(p_val_llrt = .p_vals[categories]) %>% 
    mutate_at(vars(prob, lower, upper), ~. * 100) %>% 
    mutate(p_val_llrt = round(as.numeric(p_val_llrt), 3))
  
}

# Get predicted marginal means ----------------------------------------------------------


# Check up

names(best_vars$form_pred_check_up) <- best_vars$form_pred_check_up

pmm_check <- best_vars$form_pred_check_up %>% 
  map_dfr(get_marginals, .fit = fit_check, .p_vals = p_check, .id = "variable") %>% 
  print(n = 30)


# Outpatient

names(best_vars$form_pred_outpat) <- best_vars$form_pred_outpat

pmm_check <- best_vars$form_pred_outpat %>% 
  map_dfr(get_marginals, .fit = fit_ambulant, .p_vals = p_ambulant, .id = "variable") %>% 
  print(n = 30)


# Inpatient

names(best_vars$form_pred_inpat) <- best_vars$form_pred_inpat

pmm_check <- best_vars$form_pred_inpat %>% 
  map_dfr(get_marginals, .fit = fit_inpatient, .p_vals = p_inpatient, .id = "variable") %>% 
  print(n = 30)



