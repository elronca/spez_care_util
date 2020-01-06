
library(tidyverse)
stri_replace_last_coll <- stringi::stri_replace_last_coll


imp <- readRDS(file.path("workspace", "imputed_and_modified_ds.RData"))

estim_check_up <- readRDS(file.path("workspace", "estim_check_up.RData"))
estim_outp <- readRDS(file.path("workspace", "estim_outp.RData"))
estim_inp <- readRDS(file.path("workspace", "estim_inp.RData"))


# Get proportion of all categorical variables -----------------------------

get_prop <- function(df, grouping_var) {
  
  df %>%
    group_by(!!ensym(grouping_var)) %>% 
    tally %>% 
    mutate(prop = n / sum(n)) %>% 
    rename(variable_cat = grouping_var) %>% 
    mutate(
      variable_cat = as.character(variable_cat),
      variable_reg = str_c(grouping_var, variable_cat)) %>% 
    select(variable_cat, variable_reg, prop)
  
}

prop_cat_vars <- mice::complete(imp, "long") %>% 
  select_if(negate(is.numeric)) %>% 
  names() %>% 
  map_dfr(get_prop, df = mice::complete(imp, "long")) %>% 
  mutate(var_only = stri_replace_last_coll(variable_reg, variable_cat, ""))



# Calculate marginal effects ----------------------------------------------


# Identfiy used variables in provided regression results

used_vars <- estim_check_up %>% 
  left_join(prop_cat_vars, by = c("variable" = "variable_reg")) %>% 
  pull(var_only) %>% 
  unique() %>% 
  .[!is.na(.)]


# Keep category proportions for used variables only

prop_cats <- prop_cat_vars %>% 
  filter(var_only %in% used_vars) %>% 
  bind_rows(list(variable_cat = "(Intercept)", variable_reg = "(Intercept)", prop = 1, var_only = "(Intercept)"), .)


# Replace NA with effect = 0. This is the case because NA are where the reference category was

estim_completed <- prop_cats %>% 
  left_join(estim_check_up, by = c("variable_reg" = "variable")) %>% 
  replace_na(list(estimate = 0, LL = 0, UL = 0, p.value = ""))

estim_weighted <- mutate(estim_completed, 
                         
                         estimate_wgt = estimate * prop,
                         LL_wgt = LL * prop,
                         UL_wgt = UL * prop
                         
)

filter(estim_weighted, variable_reg == "sexmale") %>% pull(estimate) %>% exp %>% round(2)


# Calculate marignal effect by variable -----------------------------------

?mice::pool

# get_marginal_effect_for_var <- function(my_var) {
  
  my_var <- "sex"
  
  
  # Marginal effect our variable (all estimates are summed up except for the variable we want to calculate the marginal effects for)
  
  marg_eff_wo_var <- colSums(estim_weighted[estim_weighted$var_only != my_var, c("estimate_wgt", "LL_wgt", "UL_wgt")])

  get_prop_avg_pers <- function(x) { 100 * (exp(x) / (1 + exp(x))) }
  
  prop_var <- get_prop_avg_pers(marg_eff_wo_var)
  
  # Get the estimates for our variable
  
  eff_var_cat <- estim_weighted[estim_weighted$var_only == my_var, c("estimate", "LL", "UL")]
  prop_var[1] * exp(eff_var_cat)[1,]
  

  eff_var_cat <- sweep(eff_var_cat, MARGIN = 2, marg_eff_wo_var, `+`)
  
  
  # Get the variable name, categories and p value of that variable
  
  name_var_cat <- estim_weighted %>% 
    filter(var_only == my_var) %>% 
    select(c("var_only", "variable_cat", "p.value"))
    
    # estim_weighted[estim_weighted$var_only == my_var, c("var_only", "variable_cat", "p.value")] 
  
  
  # Transform the marginal effects estimate to odds ratios and then to proportions
  
  marg_effect <- bind_cols(name_var_cat, eff_var_cat) %>% 
    mutate_at(vars(estimate, LL, UL), function(x) { 100 * (exp(x) / (1 + exp(x))) } ) %>% 
    select(variable = var_only, category = variable_cat, prop = estimate, LL, UL, p.value)

  
# }

map_dfr(used_vars, get_marginal_effect_for_var) %>% 
  print(n = 30)


