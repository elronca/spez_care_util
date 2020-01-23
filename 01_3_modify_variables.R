
## Modify variables

library(tidyverse)
library(naniar)

sci <- readRDS(file = file.path("workspace", "data_harmonized.Rdata"))

# Make numeric variables numeric ------------------------------------------

sci <- sci %>% mutate_at(vars(age, time_since_sci), as.numeric)


# Change numeric values ---------------------------------------------------

sci$time_since_sci_y <- sci$time_since_sci/12

# Make new categories of existing variables -------------------------------

# Ambulatory status

recode_scim_20 <- c(
  electric_wheelchair = "I need total assistance",
  electric_wheelchair = "I need an electric wheelchair or partial assistance to operate a manual wheelchair", 
  manual_wheekchair = "I am independent in a manual wheelchair",
  manual_wheekchair = "need supervision while walking (with or without walking aids)",
  walking_independently = "walk with a walking frame or crutches, swinging forward with both feet at a time",
  walking_independently = "walk with crutches or two canes, setting one foot before the other",
  walking_independently = "walk with one cane",
  walking_freely = "walk with a leg orthosis(es) only (e.g., leg splint)",
  walking_freely = "walk without walking aids")

sci <- mutate(sci, amb_status = fct_recode(scim_20, !!!recode_scim_20))


# Secondary health conditions and chronic conditions

# Replace NA's with 0 in those participants who never indicated a 0. We expect that they just didn't fill out the 
# conditions of which they did not suffer from

sci <- mutate_at(sci, vars(starts_with("problem_")), as.integer)

sci %>% select(matches("problem_")) %>% miss_var_summary

sci <- sci %>% 
  mutate(n_zeros = pmap_int(select(., starts_with("problem_")), ~sum(c(...) == 0, na.rm = T))) %>% 
  mutate(n_missing = pmap_int(select(., starts_with("problem_")), ~sum(is.na(c(...)), na.rm = T))) %>% 
  mutate_at(vars(starts_with("problem_")), ~if_else(n_zeros %in% 0L & is.na(.), 0L, as.integer(.)))

sci %>% select(matches("problem_")) %>% miss_var_summary

SHC_all_levels <- sci %>% select(id_swisci, matches("problem_"))

saveRDS(SHC_all_levels, file = file.path("workspace", "SHC_all_levels.RData"))


# Keep only severe and chronic forms of secondary health conditions

shc_vars <- str_subset(names(sci), "problem_") %>% str_subset(c("diabetes|heart|cancer|depression"), negate = TRUE)

sci <- sci %>%
  mutate_at(vars(shc_vars), ~case_when(
  . %in% c(0:2) ~ 0L,
  . == 3L ~ 1L,
  TRUE ~ as.integer(.)))

select(sci, matches("problem_")) %>% colSums(na.rm = T)


# Recode paracenter utilization -------------------------------------------

parac_vars <- str_subset(names(sci), "hc_paracenter")

sci <- sci %>% 
  mutate_at(parac_vars, ~if_else(. %in% c("777", "888", "999", "unknown"), NA_character_, .)) %>% 
  mutate_at(parac_vars, ~as.integer(.))

select(sci, parac_vars) %>% 
  pivot_longer(everything(), names_to = "center_var", values_to = "visits") %>% 
  count(center_var, visits) %>% 
  print(n = 38)


# If all specific paracenter variables are NA and if the hc_paracenter is not 1 then hc_paracenter becomes 0 

table(sci$hc_paracenter, useNA = "always")
sci <- sci %>% mutate(hc_paracenter = if_else(pmap_lgl(select(., parac_vars), ~all(is.na(c(...)))) & !hc_paracenter %in% 1L, 0L, hc_paracenter))
table(sci$hc_paracenter, useNA = "always")


# If any of the specific paracenter variables are larger than zero then hc_paracenter becomes 1

table(sci$hc_paracenter, useNA = "always")

if(F) { # Test whether coding is valid
  
  sci %>% 
    mutate(hc_paracenter = if_else(pmap_lgl(select(., parac_vars), ~any(c(...) > 0)) & is.na(hc_paracenter), 999L, hc_paracenter)) %>% 
    filter(hc_paracenter == 999) %>% 
    select(id_swisci, parac_vars, -hc_paracenter) %>% 
    pivot_longer(starts_with("hc_paracenter"), names_to = "vars", values_to = "values") %>% 
    mutate(values = if_else(is.na(values), "NA", "1")) %>% 
    group_by(id_swisci) %>% 
    distinct(values, .keep_all = TRUE) %>% 
    print(n = 100)
  
}

sci <- sci %>% mutate(hc_paracenter = if_else(pmap_lgl(select(., parac_vars), ~any(c(...) > 0)) & is.na(hc_paracenter), 1L, hc_paracenter))

table(sci$hc_paracenter, useNA = "always")


# If somebody indicated not to have utilized any paracenter services then all specific paracenter services will be coded as 0

sci <- sci %>% mutate_at(vars(starts_with("hc_paracenter_")), ~if_else(hc_paracenter %in% 0L, 0L, .))
select(sci, starts_with("hc_paracenter_")) %>% sapply(function(x) sum(is.na(x)))


# Recode inpatient hospitalizations ---------------------------------------

vars_inpatient_paracenter <- names(sci) %>% str_subset("inpat") %>% str_subset("paracenter")

select(sci, vars_inpatient_paracenter) %>% 
  pivot_longer(everything(), names_to = "center_var", values_to = "visits") %>% 
  count(center_var, visits) %>% 
  print(n = 17)

sci <- sci %>% 
  mutate_at(vars(starts_with("hc_inpatient")), ~if_else(. %in% c("777", "888", "999", "unknown"), NA_character_, .)) %>% 
  mutate_at(vars(starts_with("hc_inpatient")), ~as.integer(.))


# If we have a NA in hc_inpatient but not in hc_inpatient_num/days and also there are no zeros indicated in these variables, 
# then we recode this variable to be 1

if(F) { # Just to check what happens -> nothing
  select(sci, hc_inpatient, hc_inpatient_num, hc_inpatient_days) %>% 
  filter(is.na(hc_inpatient)) %>% 
  summarise_all(~sum(is.na(.)))
}

table(sci$hc_inpatient, useNA = "always")
sci <- sci %>% mutate(hc_inpatient = if_else(is.na(hc_inpatient) & all(is.na(na_if(select(., hc_inpatient_num, hc_inpatient_days), 0))), 1L, hc_inpatient))
table(sci$hc_inpatient, useNA = "always")


# If all records are NA then we guess that the respondent have never been to the hospital

select(sci, starts_with("hc_inpatient"))

table(sci$hc_inpatient, useNA = "always")
sci <- sci %>% mutate_at(vars(starts_with("hc_inpatient")), ~if_else(is.na(hc_inpatient) & is.na(hc_inpatient_num) & is.na(hc_inpatient_days), 0L, .))
table(sci$hc_inpatient, useNA = "always")


# We recode the number of inpatient visits and stays as 0 if there hasn't been indicated that there were any visits at all

select(sci, starts_with("hc_inpatient")) %>% 
  filter(hc_inpatient %in% 0 & (!is.na(hc_inpatient_num) | !is.na(hc_inpatient_days)))

sci <- sci %>% mutate(hc_inpatient = if_else(hc_inpatient %in% 0 & (!is.na(hc_inpatient_num) | !is.na(hc_inpatient_days)), 1L, hc_inpatient))

select(sci, starts_with("hc_inpatient")) %>% 
  filter(hc_inpatient %in% 0 & (!is.na(hc_inpatient_num) | !is.na(hc_inpatient_days)))


# Change no inpatient visits to inpatient visits if somebody indicated that they had an inpatient visit at an SCI center

sci %>% filter(hc_inpatient == 0 & rowSums(select(sci,vars_inpatient_paracenter), na.rm = T) > 0) %>% select(hc_inpatient, vars_inpatient_paracenter)
table(sci$hc_inpatient)

sci <- sci %>% mutate(hc_inpatient = if_else(select(., vars_inpatient_paracenter) %>% rowSums(na.rm = T) > 0L, 1L, hc_inpatient))

table(sci$hc_inpatient)
sci %>% filter(hc_inpatient == 0 & rowSums(select(sci,vars_inpatient_paracenter), na.rm = T) > 0) %>% select(hc_inpatient, vars_inpatient_paracenter)

sci %>% 
  filter(tp == "ts2") %>% 
  filter(hc_inpatient %in% 1) %>% 
  select(hc_inpatient_num, hc_inpatient_days) %>% 
  naniar::miss_var_summary()


# Recode outpatient hospitalizations --------------------------------------

vars_ambulant_paracenter <- names(sci) %>% str_subset("ambulant") %>% str_subset("paracenter")


# Recode unknown cells

select(sci, hc_ambulant_planned, hc_ambulant_unplanned) %>% 
  pivot_longer(everything(), names_to = "center_var", values_to = "visits") %>% 
  count(center_var, visits)

sci <- sci %>% mutate_at(vars(starts_with("hc_ambulant")), ~if_else(. %in% c("777", "888", "999", "unknown"), NA_character_, .)) %>% 
  mutate_at(vars(starts_with("hc_ambulant")), ~as.integer(.))

select(sci, hc_ambulant_planned, hc_ambulant_unplanned) %>% 
  pivot_longer(everything(), names_to = "center_var", values_to = "visits") %>% 
  count(center_var, visits)


# If somebody indicated a number of visits then it is obvious that they visited a clinic

select(sci, hc_ambulant_planned, hc_ambulant_unplanned) %>% 
  pivot_longer(everything(), names_to = "center_var", values_to = "visits") %>% 
  count(center_var, visits)

sci <- sci %>% 
  mutate(
    hc_ambulant_planned = if_else(hc_ambulant_planned_num > 0L & !is.na(hc_ambulant_planned_num), 1L, hc_ambulant_planned),
    hc_ambulant_unplanned = if_else(hc_ambulant_unplanned_num > 0L & !is.na(hc_ambulant_unplanned_num), 1L, hc_ambulant_unplanned)
    )

select(sci, hc_ambulant_planned, hc_ambulant_unplanned) %>% 
  pivot_longer(everything(), names_to = "center_var", values_to = "visits") %>% 
  count(center_var, visits)



# If all specific ambulant specific variables variables are NA and if the hc_ambulant is not 1 then hc_ambulant becomes 0 

table(sci$hc_ambulant, useNA = "always")

if(F) {
  select(sci, starts_with("hc_ambulant_"))
  apply(select(sci, starts_with("hc_ambulant_")), 1, function(x) {all(is.na(x))})[1:10]
}

sci <- sci %>% mutate(hc_ambulant = if_else(pmap_lgl(select(., starts_with("hc_ambulant_")), ~all(is.na(c(...)))) & !hc_ambulant %in% 1L, 0L, hc_ambulant))
table(sci$hc_ambulant, useNA = "always")


# If any of the specific ambulant variables are larger than 0 then hc_ambulant becomes 1

select(sci, starts_with("hc_ambulant_"))

table(sci$hc_ambulant, useNA = "always")
sci <- sci %>% mutate(hc_ambulant = if_else(pmap_lgl(select(., starts_with("hc_ambulant_")), ~any(c(...) > 0)) & is.na(hc_ambulant), 1L, hc_ambulant))
table(sci$hc_ambulant, useNA = "always")



# If any of the specific ambulant variables are either NA or 0 and hc_ambulant is NA it becomes 0

table(sci$hc_ambulant, useNA = "always")
sci <- sci %>% mutate(hc_ambulant = if_else(pmap_lgl(select(., starts_with("hc_ambulant_")), ~all(is.na(na_if(c(...), 0)))) & is.na(hc_ambulant), 0L, hc_ambulant)) 
table(sci$hc_ambulant, useNA = "always")


# If somebody indicated not to have utilized any ambulant services then all specific ambulant services will be coded as 0

select(sci, starts_with("hc_ambulant_")) %>% 
  pivot_longer(everything(), names_to = "center_var", values_to = "visits") %>% 
  count(center_var, visits) %>% 
  filter(visits == 0 | is.na(visits))

sci <- sci %>% mutate_at(vars(starts_with("hc_ambulant_")), ~if_else(hc_ambulant %in% 0L, 0L, .))

select(sci, starts_with("hc_ambulant_")) %>% 
  pivot_longer(everything(), names_to = "center_var", values_to = "visits") %>% 
  count(center_var, visits) %>% 
  filter(visits == 0 | is.na(visits))
  

# Change no ambulant visits to ambulant visits if somebody indicated that they had an ambulant visit at an SCI center

sci %>% filter(hc_ambulant == 0 & rowSums(select(sci, vars_ambulant_paracenter), na.rm = T) > 0) %>% select(hc_ambulant, vars_ambulant_paracenter)
table(sci$hc_ambulant, useNA = "always")

sci <- sci %>% mutate(hc_ambulant = if_else(select(., vars_ambulant_paracenter) %>% rowSums(na.rm = T) > 0L, 1L, hc_ambulant))

sci %>% filter(hc_ambulant == 0 & rowSums(select(sci, vars_ambulant_paracenter), na.rm = T) > 0) %>% select(hc_ambulant, vars_ambulant_paracenter)
table(sci$hc_ambulant, useNA = "always")



# create new variable -> total number of ambulant visits

sci <- sci %>% 
  mutate(hc_ambulant_num = select(., c(hc_ambulant_planned_num, hc_ambulant_unplanned_num)) %>% rowSums(na.rm = TRUE) %>% as.integer()) %>% 
  mutate(hc_ambulant_num = if_else(hc_ambulant %in% 1 & is.na(hc_ambulant_planned_num) & is.na(hc_ambulant_unplanned_num), NA_integer_, hc_ambulant_num))

select(sci, hc_ambulant, hc_ambulant_num, hc_ambulant_planned_num, hc_ambulant_unplanned_num)

sci %>% filter(tp == "ts2") %>% pull(hc_ambulant) %>% table() %>% prop.table()

sci %>% 
  filter(tp == "ts2") %>% 
  filter(hc_ambulant == 1) %>% 
  pull(hc_ambulant_num) %>% 
  table()

sci %>% 
  filter(tp == "ts2") %>% 
  filter(hc_ambulant == 1) %>% 
  filter(hc_ambulant_num == 0) %>% 
  select(id_swisci, hc_ambulant, hc_ambulant_planned_num, hc_ambulant_unplanned_num) %>% 
  print(n = 50)

sci %>% 
  filter(tp == "ts2") %>% 
  filter(hc_ambulant == 1) %>% 
  pull(hc_ambulant_num) %>% 
  ifelse( . > 0, 1, .) %>% 
  ifelse(. %in% 0, NA, .) %>% 
  table(useNA = "always") %>% 
  prop.table()


# Rename variables --------------------------------------------------------

names(sci)

sci <- sci %>% select(id_swisci, tp, sex, age, 
                      liv_arrangement,
                      time_since_sci,
                      lesion_level = sci_type,
                      completeness = sci_degree, 
                      etiology = sci_cause_type,
                      financial_hardship = ef_finances,
                      short_transp_barr = ef_short_transport,
                      long_transp_barr = ef_long_transport,
                      {str_subset(names(.), "^hc") %>% sort()}, 
                      amb_status,
                      module_hcu_12,
                      medstat,
                      matches("problem_"), scim_1:scim_rasch)


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
    
    amb_status = fct_relevel(amb_status, c("walking_freely", "walking_independently", "manual_wheekchair", "electric_wheelchair")),
    
    scim_20 = fct_rev(scim_20)) %>% 
  
  mutate_if(is.factor, fct_drop) %>% 
  
  arrange(tp, as.numeric(id_swisci))


# Rename visits to specialized clinics/center variables

sci <- sci %>% 
  rename_at(vars(starts_with("hc_paracenter"), -hc_paracenter), ~str_replace_all(., c("1" = "Balgrist",
                                                                                      "2" = "RehaB",
                                                                                      "3" = "CRR",
                                                                                      "4" = "SPZ",
                                                                                      "5" = "Plein_Soleil",
                                                                                      "6" = "Bellinzona")))



# One patient was incorrectly coded as female

filter(sci, id_swisci == "507514") %>% select(id_swisci:sex)

sci <- sci %>% 
  
  mutate(
  sex = as.character(sex),
  sex = if_else(id_swisci == "507514", "male", sex),
  sex = as.factor(sex)
  )

filter(sci, id_swisci == "507514") %>% select(id_swisci:sex)


# This person mentioned inpatient days but the number is 0, we recode it as being 1

sci %>% filter(hc_inpatient_days > 0 & hc_inpatient_num == 0) %>% 
  select(id_swisci, tp, starts_with("hc_inpatient"))

sci <- mutate(sci, hc_inpatient_num = if_else(id_swisci == "507163" & tp == "ts1", 1L, hc_inpatient_num))


# Save data and clear workspace -------------------------------------------

saveRDS(sci, file = file.path("workspace", "variables_modified.Rdata"))

rm("ef_order", "recode_scim_20", "sci", "vars_ambulant_paracenter", 
   "vars_inpatient_paracenter", "shc_vars", "SHC_all_levels", "parac_vars")

