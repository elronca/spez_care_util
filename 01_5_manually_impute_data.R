
## Create new variables and impute data that is easy to impute

library(tidyverse)
library(naniar)
library(readxl)

sci <- readRDS(file.path("workspace", "variables_modified.Rdata"))

# Check missings ----------------------------------------------------------

filter(sci, tp == "ts2") %>% 
  miss_var_summary() %>% 
  filter(!str_detect(variable, "hc_")) %>% 
  print(n = 50)


# Add missing medstat regions ---------------------------------------------



missing_medstats_Balgrist <- read_excel("data/Medstat_Balgrist IDs_Completed_20191211.xlsx") %>% 
  select(id_swisci = `SwiSCI ID`, medstat = `Medstat Region`) %>% 
  mutate_if(is.numeric, as.character)

missing_medstats_SPV <- read_excel("data/Medstat_SPV IDs.xlsx") %>% 
  select(id_swisci = `SPFID`, medstat = `MD`) %>% 
  mutate_if(is.numeric, as.character)


tp2 <- sci$tp == "ts2"

miss_var_summary(sci) %>% filter(variable == "medstat")

sci[tp2,][match(missing_medstats_Balgrist$id_swisci, sci$id_swisci[tp2]), ]$medstat <- missing_medstats_Balgrist$medstat
sci[tp2,][match(missing_medstats_SPV$id_swisci, sci$id_swisci[tp2]), ]$medstat <- missing_medstats_SPV$medstat


# Parahelp missing medstats

sci[tp2,][sci[tp2,]$id_swisci == "122429", ]$medstat <- "BL05"
sci[tp2,][sci[tp2,]$id_swisci == "122653", ]$medstat <- "AG52"
sci[tp2,][sci[tp2,]$id_swisci == "123601", ]$medstat <- "JU05"


# RehaB missing medstats

sci[tp2,][sci[tp2,]$id_swisci == "125050", ]$medstat <- "BL06"


miss_var_summary(sci) %>% filter(variable == "medstat")

rm(missing_medstats_Balgrist, missing_medstats_SPV, tp2)


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


# Some wrong tsi coding in 2012

# Checked with the study center, ts2 times since sci were correct

sci[sci$id_swisci == "133810", ]$time_since_sci <- c(78 - 60, 78)
sci[sci$id_swisci == "133824", ]$time_since_sci <- c(138 - 60, 138)
sci[sci$id_swisci == "142415", ]$time_since_sci <- c(146 - 60, 146)
sci[sci$id_swisci == "142525", ]$time_since_sci <- c(230 - 60, 230)
sci[sci$id_swisci == "505519", ]$time_since_sci <- c(554 - 60, 554)
sci[sci$id_swisci == "507078", ]$time_since_sci <- c(620 - 60, 620)
sci[sci$id_swisci == "507089", ]$time_since_sci <- c(332 - 60, 332)
sci[sci$id_swisci == "507375", ]$time_since_sci <- c(367 - 60, 367)


# Remove case where we have no data

sci <- sci %>% filter(!is.na(sex))



# Recode utilization variables (ambulant and inpatient --------------------


# Code hc_ambulant_num as missing if hc_ambulant = 1 and hc_ambulant_num = 0

sci %>% 
  filter(hc_ambulant %in% 1) %>% 
  filter(hc_ambulant_num %in% 0) %>% 
  select(hc_ambulant, hc_ambulant_num)

sci <- sci %>% mutate(hc_ambulant_num = if_else(hc_ambulant %in% 1 & hc_ambulant_num %in% 0, NA_integer_, hc_ambulant_num))

sci %>% filter(tp == "ts2") %>% pull(hc_ambulant) %>% table(useNA = "always") %>% prop.table() * 100

sci %>% 
  filter(tp == "ts2" & hc_ambulant == 1) %>%  
  miss_var_summary() %>% 
  filter(variable == "hc_ambulant_num")


# Code hc_inpatient_num and inpatient_days as missing if hc_inpatient = 1 and hc_ambulant_num/_days = 0

sci %>% 
  filter(hc_inpatient %in% 1) %>%
  filter(hc_inpatient_num %in% 0) %>%
  select(hc_inpatient, hc_inpatient_num, hc_inpatient_days)

sci <- sci %>% mutate(hc_inpatient_num = if_else(hc_inpatient %in% 1 & hc_inpatient_num %in% 0, NA_integer_, hc_inpatient_num))


sci %>% 
  filter(hc_inpatient %in% 1) %>% 
  filter(hc_inpatient_days %in% 0) %>%
  select(hc_inpatient, hc_inpatient_days, hc_inpatient_num)

sci <- sci %>% mutate(hc_inpatient_days = if_else(hc_inpatient %in% 1 & hc_inpatient_days %in% 0, NA_integer_, hc_inpatient_days))

sci %>% filter(tp == "ts2") %>% pull(hc_inpatient) %>% table(useNA = "always") %>% prop.table() * 100

sci %>% 
  filter(tp == "ts2" & hc_inpatient == 1) %>%  
  miss_var_summary() %>% 
  filter(variable %in% c("hc_inpatient_num", "hc_inpatient_days"))


# Medstat regions for Armin

select(sci, id_swisci, survey = tp, medstat) %>% 
  arrange(id_swisci, survey) %>% 
  write.csv2(file.path("output", "medstat_regionen.csv"), row.names = FALSE)


# Save dataset and clear workspace

saveRDS(sci, file.path("workspace", "manually_imputed.Rdata"))

rm(sci)
