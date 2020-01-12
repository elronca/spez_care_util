
library(tidyverse)
library(emmeans)



fit_check <- readRDS(file.path("workspace", "fit_hc_parac_check.RData"))
p_check <- readRDS(file.path("workspace", "p_values_hc_parac_check.RData"))

fit_ambulant <- readRDS(file.path("workspace", "fit_hc_ambulant_parac.RData"))
p_ambulant <- readRDS(file.path("workspace", "p_values_hc_ambulant_parac.RData"))

fit_inpatient <- readRDS(file.path("workspace", "fit_hc_inpatient_parac.RData"))
p_inpatient <- readRDS(file.path("workspace", "p_values_hc_inpatient_parac.RData"))

final_vars <- readRDS(file.path("workspace", "final_vars.RData"))

sci <- readRDS(file.path("workspace", "outcome_vars_prepared.Rdata"))



# Function to obtain predicted marginal means and p values -----------------------------

get_marginals <- function(categories, .fit, .p_vals) {
  
  pmms <- emmeans(.fit, categories, type = "response", weights = "proportional") %>% 
    as_tibble() %>% 
    rename(category = 1) %>% 
    mutate(category = as.character(category)) %>% 
    select(category, prob, lower = asymp.LCL, upper = asymp.UCL) %>% 
    mutate(p_val_llrt = .p_vals[categories]) %>% 
    mutate_at(vars(prob, lower, upper), ~. * 100) %>% 
    mutate(p_val_llrt = formatC(as.numeric(p_val_llrt), format = "f", digits = 3))
  
}


get_final_table <- function(.raw_table, .raw_data, .outcome) {
  
  table_pmm <- .raw_table %>% 
    mutate(variable_temp = variable) %>% 
    group_by(variable_temp) %>% 
    mutate(p_val_llrt = if_else(duplicated(variable), "", as.character(p_val_llrt))) %>% 
    ungroup() %>% 
    select(-variable_temp)
  
  fit <- glm(as.formula(str_c(.outcome, " ~ 1")), family = "binomial", data = .raw_data)
  
  res_reg <- c(coef(fit), confint(fit))
  
  get_prop <- function(x) round(100 * exp(x) / (exp(x) + 1), 1)
  
  res_prob <- get_prop(res_reg)
  
  res <- tibble(variable = "all", category = "", prob = res_prob[[1]], lower = res_prob[[2]], upper = res_prob[[3]], p_val_llrt = "")
  
  rbind(res, table_pmm) %>% 
    mutate(res = str_c(round(prob, 0), " (", round(lower, 0), "\u2013", round(upper, 0), ")")) %>% 
    select(variable, category, prob = res, "p value" = p_val_llrt)
  
}

# Get predicted marginal means ----------------------------------------------------------


# Check up

names(final_vars$form_pred_check_up) <- final_vars$form_pred_check_up

emm_check <- final_vars$form_pred_check_up %>% 
  map_dfr(get_marginals, .fit = fit_check, .p_vals = p_check, .id = "variable") %>% 
  print(n = 30)

check_final <- get_final_table(.raw_table = emm_check, .raw_data = sci, .outcome = "hc_parac_check")


# Outpatient

names(final_vars$form_pred_outpat) <- final_vars$form_pred_outpat

emm_outp <- final_vars$form_pred_outpat %>% 
  map_dfr(get_marginals, .fit = fit_ambulant, .p_vals = p_ambulant, .id = "variable") %>% 
  print(n = 30)


outp_data <- filter(sci, hc_ambulant == 1)
outp_final <- get_final_table(.raw_table = emm_outp, .raw_data = outp_data, .outcome = "hc_ambulant_parac")


# Inpatient

names(final_vars$form_pred_inpat) <- final_vars$form_pred_inpat

emm_inp <- final_vars$form_pred_inpat %>% 
  map_dfr(get_marginals, .fit = fit_inpatient, .p_vals = p_inpatient, .id = "variable") %>% 
  print(n = 30)

inpat_data <- filter(sci, hc_inpatient == 1)

inpat_final <- get_final_table(.raw_table = emm_inp, .raw_data = inpat_data, .outcome = "hc_inpatient_parac")




# Put final tables together to final table --------------------------------

raw_final_table <- full_join(check_final, outp_final, by = c("variable", "category")) %>% 
  full_join(inpat_final, by = c("variable", "category")) %>% 
  rename(check_up = prob.x, p_val_cu = `p value.x`, 
         outpatient = prob.y, p_val_op = `p value.y`, 
         inpatient = prob, p_val_ip = `p value`)

unique(raw_final_table$variable)

variable_order <- c("all", "sex", "age_cat", "severity", "etiology", "language", 
                    "problem_sexual", "problem_spasticity", "problem_injury", 
                    "problem_ossification","problem_cancer",
                    "dist_amb_check_up_cat", 
                    "hc_ambulant_num_cat", "hc_inpatient_num_cat", "hc_inpatient_days_cat")

saveRDS(variable_order, file.path("workspace", "vars_for_table_1.R"))

final_table <- raw_final_table %>% 
  mutate(variable = fct_relevel(variable, variable_order)) %>% 
  arrange(variable) %>%
  mutate_all(as.character) %>% 
  mutate_all(~replace_na(., "")) %>% 
  mutate(variable_temp = variable) %>% 
  group_by(variable_temp) %>% 
  mutate(variable  = if_else(duplicated(variable), "", as.character(variable))) %>% 
  ungroup() %>% 
  select(-variable_temp) %>% 
  print(n = 45)

write.csv2(final_table, file.path("output", "table_est_ma_me.csv"), row.names = FALSE)
