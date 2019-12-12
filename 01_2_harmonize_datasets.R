

# Harmonize data of 2012 and 2017

library(magrittr)
library(tidyverse)

Sys.setenv(LANGUAGE = 'en')

load(file.path("workspace", "swisci_12_raw.RData"))
load(file.path("workspace", "swisci_17_raw.RData"))

ids_hcu <- read.csv2(file.path("data", "2013-C-013_2015_02_19.csv"), 
                     colClasses = c("character", rep("NULL", 30))) %>% 
  pull()


# Add module information

swisci_12 <- mutate(swisci_12, module_hcu_12 = if_else(id_swisci %in% ids_hcu, 1L, 0L))
swisci_17 <- mutate(swisci_17, module_hcu_12 = if_else(id_swisci %in% ids_hcu, 1L, 0L))


# Compare datasets --------------------------------------------------------

intersect(names(swisci_12), names(swisci_17))

# Variables that are unique to each dataset

setdiff(names(swisci_12), intersect(names(swisci_12), names(swisci_17)))
swisci_12 <- select(swisci_12, -c(scim_15))

setdiff(names(swisci_17), intersect(names(swisci_12), names(swisci_17)))
swisci_17 <- select(swisci_17, -starts_with("scim_15"), -scim_score_total, -scim_incomplete)


# Rename identical variables with different names -------------------------

# Age

swisci_12 <- rename(swisci_12, age = age_quest_admin)
swisci_12 <- rename(swisci_12, scim_rasch = SCIM_2012_rasch_score_imputed_0_100)

swisci_17 <- rename(swisci_17, age = age_quest)
swisci_17 <- rename(swisci_17, scim_rasch = SCIM_2017_rasch_score_imputed_0_100)

# Healthcare utilization

swisci_12 <- swisci_12 %>%  
  rename_all(~str_replace_all(., "^hcu_", "hc_")) %>% 
  rename_all(~str_replace_all(., "_adm_", "_ambulant_"))



## Add missing variables of 2017 in 2012 to dataset of 2012 so that bind_rows will be possible

extra_col_names <- setdiff(names(swisci_17), intersect(names(swisci_12), names(swisci_17)))

n_cols <- length(extra_col_names)
n_rows <- nrow(swisci_12)

swisci_17 %>% select(one_of(extra_col_names)) %>% map_chr(class) %>% unname() %>% unique()

extra_cols <- matrix(data = NA_character_, nrow = n_rows, ncol = n_cols, 
                     dimnames = list(NULL, extra_col_names)) %>% as_tibble()


swisci_12 <- bind_cols(swisci_12, extra_cols)



# Join datasets -----------------------------------------------------------

sci <- bind_rows(swisci_12, swisci_17) %>% arrange(as.numeric(id_swisci), tp)

rm(swisci_12, swisci_17, extra_cols, extra_col_names, n_cols, n_rows, ids_hcu)



# Repair identical categories with different names ------------------------

names(sci) %>% str_subset("scim")

to_factor <- c("sex", "sci_type", "sci_degree", "sci_cause_type", "ef_finances", "ef_short_transport", "ef_long_transport", "scim_20")

sci <- mutate_at(sci, to_factor, as.factor)

ef_levels <- c(
  "had no influence" = "no influence", 
  "had no influence" = "not applicable",
  "made my life a bit more difficult" = "made my life a little harder",
  "made my life a lot more difficult" = "made my life a lot harder"
  )

recode_scim <- c(
  "I need total assistance" = "1",
  "I need an electric wheelchair or partial assistance to operate a manual wheelchair" = "2", 
  "I am independent in a manual wheelchair" = "3",
  "need supervision while walking (with or without walking aids)" = "4",
  "walk with a walking frame or crutches, swinging forward with both feet at a time" = "5",
  "walk with crutches or two canes, setting one foot before the other" = "6",
  "walk with one cane" = "7",
  "walk with a leg orthosis(es) only (e.g., leg splint)" = "8",
  "walk without walking aids" = "9")


sci <- mutate(sci, 
              sci_degree = str_remove(sci_degree, " lesion"),
              
              sci_cause_type = str_remove(sci_cause_type, " sci"), 
              
              ef_finances = fct_recode(ef_finances, !!!ef_levels),
              
              ef_short_transport = fct_recode(ef_short_transport, !!!ef_levels),
              
              ef_long_transport = fct_recode(ef_long_transport, !!!ef_levels),
              
              scim_20 = fct_recode(scim_20, !!!recode_scim))


# There are still different variables

if(F) {
  n_miss_vars <- sci %>% 
    group_by(tp) %>% 
    select(tp, starts_with("hc_")) %>% 
    summarize_all(~sum(is.na(.))) %>% 
    pivot_longer(cols = starts_with("hc_"), names_to = "hcu_type", values_to = "n_missings") %>% 
    pivot_wider(names_from = tp, values_from = n_missings) %>% 
    mutate(ts1_prop = round(ts1 / 1550 * 100, 0),
           ts2_prop = round(ts2 / 1550 * 100, 0))
  
  n_miss_vars %>% filter(ts1_prop >= 99) %>% pull(hcu_type)
}



# Relevel numeric variables -------------------------------------------------------

# Harmonize the differently coded utilization variables in 2012 and 2017 --------------------------

calc_row_sum <- function(df, sum_var_1, sum_var_2, res_sum_vars, res_sum_vars_dic) {
  
  # sum_var_1: 1st variable that will be summed up
  # sum_var_2: 2nd variable that will be summed up
  # res_sum_vars: sum of sum_var_1, sum_var_2
  # res_sum_vars_dic: dichotomized res_sum_vars -> 0 if 0 or NA, 1 otherwise
  
  ## Attention: NA -> 0 also if there are only NAs. The reason is that if somebody did not tick a box,
  ## We expect, that they didn't visit that health provider even if they were asked to enter a 0 if
  ## they've never visited a health care provider
  
  # 777 = Not applicable
  # 888 = Not interpretable answer
  # 999 = Unknown
  
  all_vars <- c(sum_var_1, sum_var_2, res_sum_vars, res_sum_vars_dic)
  
  df %>% 
    mutate_at(vars(!!!syms(all_vars)), as.integer) %>% 
    mutate_at(vars(!!!syms(all_vars)), ~if_else(. %in% c(777, 888, 999), NA_integer_, .)) %>%
    
    mutate(my_sum = select(., c(sum_var_1, sum_var_2)) %>% rowSums(na.rm = TRUE),
           my_sum = as.integer(my_sum)) %>%
    
    mutate(!!res_sum_vars := if_else(!is.na(!!sym(res_sum_vars)), !!sym(res_sum_vars), my_sum)) %>%
    
    mutate(!!res_sum_vars_dic := if_else(!!sym(res_sum_vars) > 0 | !!sym(res_sum_vars_dic) > 0, 1L, 0L),
           !!res_sum_vars_dic := replace_na(!!sym(res_sum_vars_dic), 0L)) %>% 
    
    select(-c(my_sum, sum_var_1, sum_var_2))
  
}

if(F) sci %>% select(tp, starts_with("hc_paraplegic")) %>% group_by(tp) %>% summarize_all(~sum(is.na(.)))


sci <- calc_row_sum(sci, "hc_practitioner_check", "hc_practitioner_acute", "hc_practitioner_num", "hc_practitioner")

sci <- calc_row_sum(sci, "hc_paraplegic_check", "hc_paraplegic_acute", "hc_paraplegic_num", "hc_paraplegic")


# Save data and clear workspace -------------------------------------------

save(sci, file = file.path("workspace", "data_harmonized.Rdata"))

rm("calc_row_sum", "ef_levels", "recode_scim", "sci", "to_factor")
