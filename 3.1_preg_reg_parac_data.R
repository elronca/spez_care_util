
## Prepare specialized care center variables

library(tidyverse)

sci <- readRDS(file.path("workspace", "manually_imputed.Rdata"))
spatial_vars <- readRDS(file.path("workspace", "spatial_vars.RData"))


# Reduce dataset to records of the 2017 survey ----------------------------

sci <- filter(sci, tp == "ts2")


# Prepare data for study of outpatient clinic visits ----------------------

# Who went to specialized centers

parac_amb_vars <- str_subset(names(sci), "hc_para") %>% str_subset("_ambulant")
parac_check_vars <- str_subset(names(sci), "hc_para") %>% str_subset("_check")
parac_inpat_vars <- str_subset(names(sci), "hc_para") %>% str_subset("_inpat")

sci <- sci %>% 
  mutate(hc_ambulant_parac = if_else(rowSums(select(., parac_amb_vars), na.rm = T) > 0L, 1L, 0L)) %>% 
  mutate(hc_parac_check = if_else(rowSums(select(., parac_check_vars), na.rm = T) > 0L, 1L, 0L)) %>% 
  mutate(hc_inpatient_parac = if_else(rowSums(select(., parac_check_vars), na.rm = T) > 0L, 1L, 0L))


# Driving time for outpatient visits --------------------------------------

# Bellinzona is not yet as it was very new in 2017

sci <- spatial_vars %>% 
  filter(!place %in%  c("Ente Ospedaliero Cantonale")) %>% 
  group_by(MEDSTAT04) %>% 
  filter(rank(duration_min) == 1) %>% 
  ungroup() %>% 
  select(medstat = MEDSTAT04, dist_ambulant = duration_min) %>% 
  left_join(sci, ., by = "medstat")



# Driving times for check up visits ---------------------------------------

sci <- mutate(sci, dist_checkup = dist_ambulant)


# Driving times for inpatient hospitalizations ---------------------------------------

sci <- spatial_vars %>% 
  filter(!place %in%  c("Ente Ospedaliero Cantonale", "Plein Soleil")) %>% 
  group_by(MEDSTAT04) %>% 
  filter(rank(duration_min) == 1) %>% 
  ungroup() %>% 
  select(medstat = MEDSTAT04, dist_inpat = duration_min) %>% 
  left_join(sci, ., by = "medstat")



# Add langauge and degree of urbanization ---------------------------------------------

sci <- left_join(sci, distinct(select(spatial_vars, medstat = MEDSTAT04, language, degurba)), by = "medstat")

sum(is.na(sci$dist_checkup))


# Save file and clear workspace -------------------------------------------

saveRDS(sci, file.path("workspace", "outcome_vars_prepared.Rdata"))

rm("parac_amb_vars", "parac_check_vars", "parac_inpat_vars", "sci", "spatial_vars")
