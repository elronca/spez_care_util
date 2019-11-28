
library(tidyverse)

load(file.path("workspace", "manually_imputed.Rdata"))

sci <- filter(sci, tp == "ts2")


soc_dem_vars <- sci %>% select(id_swisci, sex:financial_hardship, medstat:etiology) %>% names()


# Outpatient clinic visits ------------------------------------------------

## Total

ambulant <- sci %>% 
  select(id_swisci, starts_with("hc_ambulant")) %>% 
  rename(amb = hc_ambulant, amb_num = hc_ambulant_num,
         amb_pl = hc_ambulant_planned, amb_pl_num = hc_ambulant_planned_num, 
         amb_unpl = hc_ambulant_unplanned, amb_unpl_num = hc_ambulant_unplanned_num) %>% 
  
  mutate_at(vars(amb_num:amb_unpl_num), ~if_else(amb %in% 0L, 0L, .))

if(F) {
ambulant %>% filter(amb_pl %in% 0 & is.na(amb_pl_num))
ambulant %>% filter(amb_unpl  %in% 0 & is.na(amb_unpl_num))
ambulant %>% filter(pmap_lgl(select(., amb_pl:amb_unpl_num), ~all(is.na(c(...)))))
}

# Replace missings with 0 if not all specified ambulatory variables are NA

not_all_NA <- pmap_lgl(select(ambulant, amb_pl:amb_unpl_num), ~any(!is.na(c(...))))

ambulant <- mutate_at(ambulant, vars(amb_pl:amb_unpl_num), ~if_else(not_all_NA, replace_na(., 0L), .))


## At specialized institutions

ambulant <- sci %>% 
  select(id_swisci, starts_with("hc_para")) %>% 
  select(id_swisci, ends_with("_ambulant")) %>% 
  mutate(amb_spec = select(., -id_swisci) %>% rowSums(na.rm = TRUE),
         amb_spec = if_else(amb_spec > 0, 1L, 0L)) %>% 
  select(id_swisci, amb_spec) %>% 
  full_join(ambulant, ., by = "id_swisci")


# Get persons who made an outpatient visit --------------------------------

id_s_amb_patients <- ambulant %>% filter(amb %in% 1) %>% pull(id_swisci)

amb_ds <- sci %>% 
  filter(id_swisci %in% id_s_amb_patients) %>% select(soc_dem_vars) %>% 
  full_join(filter(ambulant, id_swisci %in% id_s_amb_patients), by = "id_swisci")

amb_ds$time_since_sci_y <- amb_ds$time_since_sci/12

fit <- glm(amb_spec ~ sex + age + lesion_level + completeness, data = amb_ds)

summary(fit)

merge(round(exp(coef(fit)), 2), round(exp(confint(fit)), 2), by = "row.names", all = TRUE)
  

# Paracenter in total ---------------------------------------------------------

fit <- glm(hc_paracenter ~ sex + age + lesion_level + completeness, data = sci)

summary(fit)

merge(round(exp(coef(fit)), 2), round(exp(confint(fit)), 2), by = "row.names", all = TRUE)



# Check up ----------------------------------------------------------------

check_up_vars <- names(sci) %>% str_subset("_check")

sci <- sci %>% mutate(check_up = if_else(select(., check_up_vars) %>% rowSums(na.rm = T) > 0, 1L, 0L))

fit <- glm(check_up ~ sex + age + lesion_level + completeness, data = sci)

plot(fit)


summary(fit)

merge(round(exp(coef(fit)), 2), round(exp(confint(fit)), 2), by = "row.names", all = TRUE)






