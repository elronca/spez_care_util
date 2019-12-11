
## Multiple imputation

library(mice)
library(tidyverse)
library(naniar)

load(file.path("workspace", "manually_imputed.Rdata"))
load(file.path("workspace", "spatial_vars.RData"))

sci <- filter(sci, tp == "ts2")

miss_var_summary(sci) %>% filter(str_detect(variable, "hc_")) %>% print(n = 32)
miss_var_summary(sci) %>% filter(str_detect(variable, "problem"))

names(sci) %>% str_subset("hc_", negate = T) %>% str_subset("problem_", negate = T) %>% 
  select(sci, .) %>% 
  miss_var_summary() %>% 
  arrange(pct_miss)

sci <- sci %>% mutate_at(vars(starts_with("hc_")), ~if_else(is.na(.), 0L, as.integer(.)))

test <- sci %>% select(starts_with("problem_"))

## Check whether person inserted 0 in tho shc vairables (not chronic), if not code all NA in these variables of this person as 0.

mutate_at(vars(problem_pressure:problem_circulatory), ~case_when(. %in% c(0:2) ~ 0L, . == 3L ~ 1L, TRUE ~ as.integer(.)))



  mutate_at(vars(starts_with("problem_")), ~if_else(is.na(.), 0L, as.integer(.)))





