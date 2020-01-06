
library(tidyverse)
stri_replace_last_coll <- stringi::stri_replace_last_coll


imp <- readRDS(file.path("workspace", "imputed_and_modified_ds.RData"))

estim_check_up <- readRDS(file.path("workspace", "estim_check_up.RData"))
estim_outp <- readRDS(file.path("workspace", "estim_outp.RData"))
estim_inp <- readRDS(file.path("workspace", "estim_inp.RData"))

vars_numeric <- mice::complete(imp, "long") %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)


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

prop_df <- mice::complete(imp, "long") %>% 
  select_if(negate(is.numeric)) %>% 
  names() %>% 
  map_dfr(get_prop, df = mice::complete(imp, "long")) %>% 
  mutate(var_only = stri_replace_last_coll(variable_reg, variable_cat, ""))

prop_df_all <- left_join(estim_check_up, prop_df, by = c("variable" = "variable_reg"))

used_vars <- prop_df_all %>% pull(var_only) %>% unique() %>% .[!is.na(.)]

all_vars <- filter(prop_df, var_only %in% used_vars) %>% 
  bind_rows(list(variable_cat = "(Intercept)", variable_reg = "(Intercept)", prop = 1, var_only = "(Intercept)"), .)

complete_table <- left_join(all_vars, estim_check_up, by = c("variable_reg" = "variable")) %>% 
  replace_na(list(estimate = 0, LL = 0, UL = 0, p.value = "")) %>% 
  mutate(
    estimate_wgt = estimate * prop,
    LL_wgt = LL * prop,
    UL_wgt = UL * prop)

marg_eff_wo_var <- colSums(complete_table[complete_table$var_only != "sex", c("estimate_wgt", "LL_wgt", "UL_wgt")])

eff_var_cat <- complete_table[complete_table$var_only == "sex", c("estimate_wgt", "LL_wgt", "UL_wgt")]
eff_var_cat <- sweep(eff_var_cat, MARGIN = 2, marg_eff_wo_var, `+`)

nm_var_cat <- complete_table[complete_table$var_only == "sex", c("var_only", "variable_cat", "p.value")]

eff_var <- bind_cols(nm_var_cat, eff_var_cat) %>% 
  mutate_at(vars(estimate_wgt, LL_wgt, UL_wgt), exp) %>% 
  mutate_at(vars(estimate_wgt, LL_wgt, UL_wgt), function(x) { 100 * x / (1 + x) } ) %>% 
  select(variable = var_only, category = variable_cat, prop = estimate_wgt, LL = LL_wgt, UL = UL_wgt, p.value)

  


