
## Create new variables and impute data that is easy to impute

library(tidyverse)
library(naniar)

load( file.path("workspace", "variables_modified.Rdata"))



# Check missings ----------------------------------------------------------

filter(sci, tp == "ts2") %>% 
  miss_var_summary() %>% 
  filter(!str_detect(variable, "hc_")) %>% 
  print(n = 50)


# Impute data by last observation carried backwards and forward within same participant

imp_locbf <- function(df, var_impute) {
  
  imp_var_name <- str_c(var_impute, "_imp")
  
  df %>% mutate(!!imp_var_name := !!sym(var_impute)) %>% 
    arrange(as.numeric(id_swisci), tp) %>%
    group_by(id_swisci) %>%
    fill(!!imp_var_name, .direction = "downup") %>% 
    ungroup()
  
}

# Impute variables

sci <- imp_locbf(sci, "medstat")
sci %>% select(medstat, medstat_imp) %>% miss_var_summary()

sci <- imp_locbf(sci, "lesion_level")
sci %>% select(lesion_level, lesion_level_imp) %>% miss_var_summary()

sci <- imp_locbf(sci, "completeness")
sci %>% select(completeness , completeness_imp) %>% miss_var_summary()

sci <- imp_locbf(sci, "etiology")
sci %>% select(etiology , etiology_imp) %>% miss_var_summary()

imputed_vars <- names(sci) %>% str_subset("_imp$") %>% str_remove("_imp")

sci <- sci %>% select(-imputed_vars) %>%  rename_all(~str_remove(., "_imp$"))

sci %>% select(imputed_vars) %>% miss_var_summary()

rm(imputed_vars, imp_locbf)


# Repair time since SCI variable

sci %>% 
  filter(module_hcu_12 == 1) %>% 
  group_by(tp) %>% 
  summarize(n = n(),
            mean_tsci_months = mean(time_since_sci, na.rm = TRUE),
            mean_tsci_years = mean(time_since_sci/12, na.rm = TRUE))

sci %>% 
  add_count(id_swisci) %>% 
  group_by(id_swisci) %>% 
  mutate(n_miss_tsci = n_miss(time_since_sci)) %>% 
  filter(n == 2 & n_miss_tsci == 1) %>% 
  select(id_swisci, tp, time_since_sci)

sci <- sci %>% mutate(
  time_since_sci = if_else(id_swisci == "143607" & tp == "ts1", 410 - 60, time_since_sci),
  time_since_sci = if_else(id_swisci == "509102" & tp == "ts2", 231 + 60, time_since_sci)
)

sci %>% 
  filter(id_swisci %in% c("143607", "509102")) %>% 
  select(id_swisci, tp, time_since_sci)


# Remove case where we have no data

sci <- sci %>% filter(!is.na(sex))

# Save dataset and clear workspace

save(sci, file = file.path("workspace", "manually_imputed.Rdata"))

rm(sci)

