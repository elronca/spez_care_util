

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
setdiff(names(swisci_17), intersect(names(swisci_12), names(swisci_17)))


# Rename identical variables with different names -------------------------

# Age

swisci_12 <- rename(swisci_12, age = age_quest_admin)
swisci_17 <- rename(swisci_17, age = age_quest)


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
                     dimnames = list(NULL, extra_col_names)) %>% 
  as_tibble()


swisci_12 <- bind_cols(swisci_12, extra_cols)



# Join datasets -----------------------------------------------------------

sci <- bind_rows(swisci_12, swisci_17) %>% 
  arrange(as.numeric(id_swisci), tp)

rm(swisci_12, swisci_17, extra_cols, extra_col_names, n_cols, n_rows)



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

recode_scim_2 <- c(
  electric_wheelchair = "I need total assistance",
  electric_wheelchair = "I need an electric wheelchair or partial assistance to operate a manual wheelchair", 
  manual_wheekchair = "I am independent in a manual wheelchair",
  manual_wheekchair = "need supervision while walking (with or without walking aids)",
  walking_independently = "walk with a walking frame or crutches, swinging forward with both feet at a time",
  walking_independently = "walk with crutches or two canes, setting one foot before the other",
  walking_independently = "walk with one cane",
  walking_freely = "walk with a leg orthosis(es) only (e.g., leg splint)",
  walking_freely = "walk without walking aids")




sci <- mutate(sci, 
              sci_degree = str_remove(sci_degree, " lesion"),
              
              sci_cause_type = str_remove(sci_cause_type, " sci"), 
              
              ef_finances = fct_recode(ef_finances, !!!ef_levels),
              
              ef_short_transport = fct_recode(ef_short_transport, !!!ef_levels),
              
              ef_long_transport = fct_recode(ef_long_transport, !!!ef_levels),
              
              scim_20 = fct_recode(scim_20, !!!recode_scim),
              
              amb_status = fct_recode(scim_20, !!!recode_scim_2))

count(sci, scim_20)
table(sci$amb_status, useNA = "always")


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


# Recode paracenter utilization -------------------------------------------

table(sci$hc_paracenter, useNA = "always")

sci <- sci %>% 
  mutate_at(vars(starts_with("hc_paracenter")), ~if_else(. %in% c("777", "888", "999", "unknown"), NA_character_, .)) %>% 
  
  mutate_at(vars(starts_with("hc_paracenter")), ~as.integer(.)) %>% 
  
  # If all specific paracenter variables are NA and if the hc_paracenter is not 1 then hc_paracenter becomes 0 
  mutate(hc_paracenter = if_else(pmap_lgl(select(., starts_with("hc_paracenter_")), ~all(is.na(c(...)))) & !hc_paracenter %in% 1L, 0L, hc_paracenter)) %>% 
  
  # If any of the specific paracenter variables are larger than one then hc_paracenter becomes 1
  mutate(hc_paracenter = if_else(pmap_lgl(select(., starts_with("hc_paracenter_")), ~any(c(...) > 0)) & is.na(hc_paracenter), 1L, hc_paracenter)) %>% 
  
  # If any of the specific paracenter variables are either NA or 0 and hc_paracenter is NA it becomes 0
  mutate(hc_paracenter = if_else(pmap_lgl(select(., starts_with("hc_paracenter_")), ~all(is.na(na_if(c(...), 0)))) & is.na(hc_paracenter), 0L, hc_paracenter)) %>% 
  
  # If somebody indicated not to have utilized any paracenter services then all specific paracenter services will be coded as 0
  mutate_at(vars(starts_with("hc_paracenter_")), ~if_else(hc_paracenter %in% 0L, 0L, .))

table(sci$hc_paracenter, useNA = "always")


# Recode inpatient hospitalizations ---------------------------------------


vars_inpatient_paracenter <- names(sci) %>% str_subset("inpat") %>% str_subset("paracenter")

sci[vars_inpatient_paracenter]
table(sci$hc_inpatient, useNA = "always")

sci <- sci %>% 
  mutate_at(vars(starts_with("hc_inpatient")), ~if_else(. %in% c("777", "888", "999", "unknown"), NA_character_, .)) %>% 
  mutate_at(vars(starts_with("hc_inpatient")), ~as.integer(.)) %>% 
  
  # If we have a NA in hc_inpatient but not in hc_inpatient_num/days and also there are no zeros indicated in these variables, 
  # then we recode this variable to be 1
  
  mutate(hc_inpatient = if_else(is.na(hc_inpatient) & all(is.na(na_if(select(., hc_inpatient_num, hc_inpatient_days), 0))), 1L, hc_inpatient)) %>% 
  
  # If all records are NA then we guess that the respondent have never been to the hospital
  mutate_at(vars(starts_with("hc_inpatient")), ~if_else(is.na(hc_inpatient) & is.na(hc_inpatient_num) & is.na(hc_inpatient_days), 0L, .)) %>% 
  
  # We recode the number of inpatient visits and stays as 0 if there hasn't been indicated that there were any visits at all
  mutate_at(vars(hc_inpatient_num, hc_inpatient_days), ~if_else(hc_inpatient %in% 0L, 0L, .)) %>% 
  
  # Change no ambulant visits to ambulant visits if somebody indicated that they had an ambulant visit at an SCI center
  mutate(hc_inpatient = if_else(select(., vars_inpatient_paracenter) %>% rowSums(na.rm = T) > 0L, 1L, hc_inpatient))

sci[c("hc_inpatient", vars_inpatient_paracenter)]
table(sci$hc_inpatient, useNA = "always")




# Recode outpatient hospitalizations --------------------------------------

sci %>% select(starts_with("hc_ambulant"))

vars_ambulant_paracenter <- names(sci) %>% str_subset("ambulant") %>% str_subset("paracenter")

sci[vars_ambulant_paracenter]
table(sci$hc_ambulant, useNA = "always")

sci <- sci %>% 
  mutate_at(vars(starts_with("hc_ambulant")), ~if_else(. %in% c("777", "888", "999", "unknown"), NA_character_, .)) %>% 
  mutate_at(vars(starts_with("hc_ambulant")), ~as.integer(.)) %>% 
  
  mutate(
    hc_ambulant_planned = if_else(hc_ambulant_planned_num > 0L & !is.na(hc_ambulant_planned_num), 1L, hc_ambulant_planned),
    hc_ambulant_unplanned = if_else(hc_ambulant_unplanned_num > 0L & !is.na(hc_ambulant_unplanned_num), 1L, hc_ambulant_unplanned)) %>% 
  
  # If all specific ambulant specific variables variables are NA and if the hc_ambulant is not 1 then hc_ambulant becomes 0 
  mutate(hc_ambulant = if_else(pmap_lgl(select(., starts_with("hc_ambulant_")), ~all(is.na(c(...)))) & !hc_ambulant %in% 1L, 0L, hc_ambulant)) %>% 
  
  # If any of the specific ambulant variables are larger than 1 then hc_ambulant becomes 1
  mutate(hc_ambulant = if_else(pmap_lgl(select(., starts_with("hc_ambulant_")), ~any(c(...) > 0)) & is.na(hc_ambulant), 1L, hc_ambulant)) %>% 
  
  # If any of the specific ambulant variables are either NA or 0 and hc_ambulant is NA it becomes 0
  mutate(hc_ambulant = if_else(pmap_lgl(select(., starts_with("hc_ambulant_")), ~all(is.na(na_if(c(...), 0)))) & is.na(hc_ambulant), 0L, hc_ambulant)) %>% 
  
  # If somebody indicated not to have utilized any ambulant services then all specific ambulant services will be coded as 0
  mutate_at(vars(starts_with("hc_ambulant_")), ~if_else(hc_ambulant %in% 0L, 0L, .)) %>% 
  
  # create new variable -> total number of ambulant visits
  mutate(hc_ambulant_num = select(., c(hc_ambulant_planned_num, hc_ambulant_unplanned_num )) %>% rowSums(na.rm = TRUE),
         hc_ambulant_num = as.integer(hc_ambulant_num)) %>% 
  
  # Change no ambulant visits to ambulant visits if somebody indicated that they had an ambulant visit at an SCI center
  mutate(hc_ambulant = if_else(select(., vars_ambulant_paracenter) %>% rowSums(na.rm = T) > 0L, 1L, hc_ambulant))

sci[c("hc_ambulant", vars_ambulant_paracenter)]
table(sci$hc_ambulant, useNA = "always")



# Rename variables

sci <- sci %>% select(id_swisci, tp, sex, age, time_since_sci,
                      lesion_level = sci_type,
                      completeness = sci_degree, 
                      etiology = sci_cause_type,
                      financial_hardship = ef_finances,
                      short_transp_barr = ef_short_transport,
                      long_transp_barr = ef_long_transport,
                      {str_subset(names(.), "^hc") %>% sort()}, 
                      amb_status,
                      module_hcu_12,
                      medstat) 


# Change character variables to factors

ef_order <- c(
  "had no influence", 
  "made my life a bit more difficult", 
  "made my life a lot more difficult"
)

sci <- sci %>% 
  
  mutate(
    
    tp = fct_relevel(tp, c("ts1", "ts2")),
    
    sex = fct_relevel(sex, c("male", "female")),
    
    lesion_level = fct_relevel(lesion_level, c("paraplegia", "tetraplegia")),
    
    completeness = fct_relevel(completeness, c("incomplete", "complete")),
    
    etiology = fct_relevel(etiology, c("traumatic", "nontraumatic")),
    
    financial_hardship = fct_relevel(financial_hardship, !!!ef_order),
    
    short_transp_barr = fct_relevel(short_transp_barr, !!!ef_order),
    
    long_transp_barr = fct_relevel(long_transp_barr, !!!ef_order),
    
    amb_status = fct_relevel(amb_status, c("walking_freely", "walking_independently", "manual_wheekchair", "electric_wheelchair"))
    
    ) %>% 
  
  mutate_if(is.factor, fct_drop) %>% 
  
  arrange(tp, as.numeric(id_swisci))

levels(sci$amb_status)
  


# Make numeric variables numeric ------------------------------------------

sci <- sci %>% mutate_at(vars(age, time_since_sci), as.numeric)


# Save data and clear workspace -------------------------------------------

save(sci, file = file.path("workspace", "raw_data.Rdata"))

rm("calc_row_sum", "ef_levels", "ef_order", "ids_hcu", "sci", 
  "to_factor", "vars_ambulant_paracenter", "vars_inpatient_paracenter",
  "recode_scim", "recode_scim_2")
