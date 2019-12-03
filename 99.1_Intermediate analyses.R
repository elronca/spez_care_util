
library(tidyverse)

# Check number of missings in Medstat -------------------------------------

library(naniar)

load(file.path("workspace", "raw_data.Rdata"))

sci %>% 
  filter(tp == "ts2") %>% 
  replace_with_na(replace = list(medstat = "")) %>% 
  miss_var_summary() %>% 
  filter(variable == "medstat")
  
miss_var_summary(sci) %>% filter(!str_detect(variable, "hc_"))

sci %>% filter(tp == "ts2") %>% nrow()


## Some more calculations

load(file.path("workspace", "manually_imputed.Rdata"))

id_swisci_2012 <- sci %>% filter(tp == "ts1") %>% pull(id_swisci)
length(id_swisci_2012)

sci %>% filter(tp == "ts2") %>% filter(id_swisci %in% id_swisci_2012) %>% nrow()

common_ids <- sci %>% 
  filter(module_hcu_12 == 1) %>% 
  count(id_swisci) %>% 
  filter(n == 2) %>% 
  pull(id_swisci)

sex_comparison <- filter(sci, id_swisci %in% common_ids) %>% 
  select(id_swisci, tp, sex) %>% 
  pivot_wider(id_cols = everything(), names_from = tp, values_from = sex) %>% 
  mutate_if(is.factor, as.character) %>% 
  mutate(same_sex = as.integer(ts1 == ts2))

wrong_sex <- sex_comparison %>% filter(same_sex == 0)
wrong_sex_id <- wrong_sex$id_swisci

strange_person <- sci %>% filter(id_swisci %in% wrong_sex_id)


##

# Identify strange time since sci variables

month_tsci_btwn_surveys <- sci %>% 
  add_count(id_swisci) %>% 
  group_by(id_swisci) %>% 
  mutate(diff_tsci = time_since_sci - lag(time_since_sci)) %>% 
  select(id_swisci, tp, time_since_sci, diff_tsci, n) %>% 
  filter(n == 2) %>% 
  summarize(avg_tsci_diff = sum(diff_tsci, na.rm = TRUE)) 

month_tsci_btwn_surveys %>% pull() %>% summary()

strangies_ids <- month_tsci_btwn_surveys %>% 
  filter(avg_tsci_diff > 80 | avg_tsci_diff < 48) %>% 
  pull(id_swisci)

strangies_df <- filter(sci, id_swisci %in% strangies_ids) %>% 
  select(id_swisci:time_since_sci) %>% 
  group_by(id_swisci) %>% 
  mutate(diff_tsci = time_since_sci - lag(time_since_sci)) %>% 
  mutate(diff_age = age - lag(age),
         diff_age = diff_age *12) %>% 
  filter(sum(is.na(diff_tsci)) < 2) %>% 
  mutate(diff_tsci = replace_na(diff_tsci, ""),
         diff_age = replace_na(diff_tsci, ""))

write.csv2(strangies_df, file.path("output", "strange_times_since_sci.csv"), row.names = FALSE)

rm(month_tsci_btwn_surveys, strangies_ids, strangies_df)
