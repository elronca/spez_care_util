

# Harmonize data of 2012 and 2017

library(tidyverse)
Sys.setenv(LANGUAGE='en')

load(file.path("workspace", "swisci_12_raw.RData"))
load(file.path("workspace", "swisci_17_raw.RData"))

ids_hcu <- read.csv2(file.path("data", "2013-C-013_2015_02_19.csv"), 
                      colClasses = c("character", rep("NULL", 30))) %>% 
  pull()


# Add module information

swisci_12 <- mutate(swisci_12, module_hcu = if_else(id_swisci %in% ids_hcu, 1L, 0L))
swisci_17 <- mutate(swisci_17, module_hcu = if_else(id_swisci %in% ids_hcu, 1L, 0L))

# Compare datasets --------------------------------------------------------

intersect(names(swisci_12), names(swisci_17))

# Variables that are unique to each dataset

setdiff(names(swisci_12), intersect(names(swisci_12), names(swisci_17)))
setdiff(names(swisci_17), intersect(names(swisci_12), names(swisci_17)))


# Rename identical variables with different names -------------------------

# Age

swisci_12 <- rename(swisci_12, age = age_quest_admin)
swisci_17 <- rename(swisci_17, age = age_quest)



# # Prepare hcu variables -------------------------------------------------

swisci_12 %>% names() %>% str_subset("hcu_")
swisci_17 %>% names() %>% str_subset("hc_")

names(swisci_12) <- str_replace_all(names(swisci_12), "hcu_", "hc_")

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
  hc_practitioner_num = if_else(is.na(hc_practitioner_check) & is.na(hc_practitioner_acute), NA_real_, hc_practitioner_num)) %>% 
  
  mutate(
    hc_paraplegic_num = rowSums(select(., "hc_paraplegic_check", "hc_paraplegic_acute"), na.rm = TRUE),
    hc_paraplegic_num = if_else(is.na(hc_paraplegic_check) & is.na(hc_paraplegic_acute), NA_real_, hc_paraplegic_num)) %>% 
  
  select(-hc_practitioner_check, -hc_practitioner_acute, -hc_paraplegic_check, -hc_paraplegic_acute)


setdiff(names(swisci_12), intersect(names(swisci_12), names(swisci_17)))
setdiff(names(swisci_17), intersect(names(swisci_12), names(swisci_17)))



my_col_names <- setdiff(names(swisci_17), intersect(names(swisci_12), names(swisci_17)))

my_cols <- length(my_col_names)
my_rows <- nrow(swisci_12)

swisci_17 %>% select(one_of(my_col_names)) %>% map_chr(class) %>% unname() %>% unique()

add_cols <- matrix(data = NA_character_, nrow = my_rows, ncol = my_cols, 
                   dimnames = list(NULL, my_col_names)) %>% 
  as_tibble()


swisci_12 <- bind_cols(swisci_12, add_cols)

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
              sci_cause_type = str_remove(sci_cause_type, " sci"))


# Change character variables to factors
to_factor <- c("sex", "sci_type", "sci_degree", "sci_cause_type")

sci <- mutate_at(sci, to_factor, as.factor)


# Save data and clear workspace -------------------------------------------

save(sci, file = file.path("workspace", "raw_data.Rdata"))

rm("swisci_12", "swisci_17", "to_factor", "sci")