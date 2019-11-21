

# Harmonize data of 2012 and 2017

library(tidyverse)
Sys.setenv(LANGUAGE='en')

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



# Prepare hcu variables -------------------------------------------------

names(swisci_12) <- str_replace_all(names(swisci_12), "^hcu_", "hc_")

names(swisci_12) <- str_replace_all(names(swisci_12), c(
  "hc_adm_planned" = "hc_ambulant_planned",
  "hc_adm_unplanned" = "hc_ambulant_unplanned",
  "hc_adm_planned_num" = "hc_ambulant_planned_num",
  "hc_adm_unplanned_num" = "hc_ambulant_unplanned_num"))

intersect(names(swisci_12), names(swisci_17))


swisci_12 <- swisci_12 %>% 
  mutate_at(vars("hc_practitioner_check", "hc_practitioner_acute"), ~if_else(. %in% c(777, 888, 99, 999), NA_real_, .)) %>% 
  mutate_at(vars("hc_paraplegic_check", "hc_paraplegic_acute"), ~if_else(. %in% c(777, 888, 99, 999), NA_real_, .)) %>% 
  
  mutate(
    hc_practitioner_num = rowSums(select(., "hc_practitioner_check", "hc_practitioner_acute"), na.rm = TRUE),
    hc_practitioner_num = if_else(is.na(hc_practitioner_check) & is.na(hc_practitioner_acute), NA_real_, hc_practitioner_num),
    hc_practitioner = if_else(hc_practitioner_num > 0, "yes", NA_character_)) %>% 
  
  mutate(
    hc_paraplegic_num = rowSums(select(., "hc_paraplegic_check", "hc_paraplegic_acute"), na.rm = TRUE),
    hc_paraplegic_num = if_else(is.na(hc_paraplegic_check) & is.na(hc_paraplegic_acute), NA_real_, hc_paraplegic_num),
    hc_paraplegic = if_else(hc_paraplegic_num > 0, "yes", NA_character_)) %>% 
  
  select(-hc_practitioner_check, -hc_practitioner_acute, -hc_paraplegic_check, -hc_paraplegic_acute)


setdiff(names(swisci_12), intersect(names(swisci_12), names(swisci_17)))
setdiff(names(swisci_17), intersect(names(swisci_12), names(swisci_17)))


## Add missing variables of 2017 in 2012 to dataset of 2012 so that bind_rows will be possible

extra_col_names <- setdiff(names(swisci_17), intersect(names(swisci_12), names(swisci_17)))

n_cols <- length(extra_col_names)
n_rows <- nrow(swisci_12)

swisci_17 %>% select(one_of(extra_col_names)) %>% map_chr(class) %>% unname() %>% unique()

extra_cols <- matrix(data = NA_character_, nrow = n_rows, ncol = n_cols, 
                     dimnames = list(NULL, extra_col_names)) %>% 
  as_tibble()


swisci_12 <- bind_cols(swisci_12, extra_cols)

sci <- bind_rows(swisci_12, swisci_17) %>% 
  arrange(as.numeric(id_swisci), tp)


# Reorder variables

sci <- bind_rows(swisci_12, swisci_17) %>% arrange(as.numeric(id_swisci), tp)


# Check factor levels

select_if(sci, is.character) %>% 
  select(-c(id_swisci, medstat)) %>% 
  map(unique)


# Repair identical categories with different names

sci <- mutate(sci, 
              sci_degree = str_remove(sci_degree, " lesion"),
              sci_cause_type = str_remove(sci_cause_type, " sci"),
              ef_finances = str_replace_all(ef_finances, c(
                "^no influence" = "had no influence",
                "made my life a little harder" = "made my life a bit more difficult",
                "made my life a lot harder" = "made my life a lot more difficult")))


sci <- sci %>% 
  mutate_if(is.character, ~if_else(. %in% "yes", "1", .)) %>% 
  mutate_if(is.character, ~if_else(. %in% "no", "0", .)) %>% 
  mutate_at(vars(starts_with("hc_paracenter")), ~if_else(is.na(.), 0L, as.integer(.))) %>% 
  mutate_at(vars(starts_with("hc_paraplegic")), ~if_else(is.na(.), 0L, as.integer(.))) %>% 
  mutate_at(vars(starts_with("hc_practitioner")), ~if_else(is.na(.), 0L, as.integer(.)))
  


# Rename variables

sci <- sci %>% select(id_swisci, tp, sex, age, time_since_sci,
                      lesion_level = sci_type,
                      completeness = sci_degree, 
                      etiology = sci_cause_type,
                      financial_hardship = ef_finances,
                      {str_subset(names(.), "^hc") %>% sort()}, 
                      module_hcu_12,
                      medstat)


# Change character variables to factors

to_factor <- c("sex", "lesion_level", "completeness", "etiology", "financial_hardship")
sci <- mutate_at(sci, to_factor, as.factor)

sci <- sci %>% 
  
  mutate(
    tp = fct_relevel(tp, c("ts1", "ts2")),
    sex = fct_relevel(sex, c("male", "female")),
    lesion_level = fct_relevel(lesion_level, c("paraplegia", "tetraplegia")),
    completeness = fct_relevel(completeness, c("incomplete", "complete")),
    etiology = fct_relevel(etiology, c("traumatic", "nontraumatic")),
    financial_hardship = fct_relevel(financial_hardship, c("not applicable", "had no influence", 
                                                           "made my life a bit more difficult", 
                                                           "made my life a lot more difficult"))) %>% 
  arrange(tp, as.numeric(id_swisci))


# Save data and clear workspace -------------------------------------------

save(sci, file = file.path("workspace", "raw_data.Rdata"))

rm("extra_col_names", "extra_cols", "ids_hcu", "n_cols", "n_rows", 
   "sci", "swisci_12", "swisci_17", "to_factor")
