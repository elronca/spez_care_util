
library(tidyverse)
library(effects)
library(splines)

load(file.path("workspace", "manually_imputed.Rdata"))
load(file.path("workspace", "spatial_vars.RData"))

sci <- filter(sci, tp == "ts2")

sci$time_since_sci_y <- sci$time_since_sci/12

soc_dem_vars <- sci %>% select(id_swisci, sex:long_transp_barr, medstat:etiology) %>% names()


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




# Do some preliminary analyses --------------------------------------------

# Get persons who made an outpatient visit --------------------------------

id_s_amb_patients <- ambulant %>% filter(amb %in% 1) %>% pull(id_swisci)

amb_ds <- sci %>% 
  filter(id_swisci %in% id_s_amb_patients) %>% 
  full_join(filter(ambulant, id_swisci %in% id_s_amb_patients), by = "id_swisci")

spat_vars_outp <- spatial_vars %>% 
  filter(!place %in%  c("Ente Ospedaliero Cantonale")) %>% 
  group_by(MEDSTAT04) %>% 
  filter(rank(duration_min) == 1) %>% 
  ungroup()

amb_ds <- left_join(amb_ds, spat_vars_outp, by = c("medstat" = "MEDSTAT04"))

fit <- glm(amb_spec ~ sex + age + lesion_level + duration_min + language + long_transp_barr, 
           data = amb_ds, family = binomial)

summary(fit)

merge(round(exp(coef(fit)), 2), round(exp(confint(fit)), 2), by = "row.names", all = TRUE)


fit <- glm(amb_spec ~ sex + ns(age, 4) + lesion_level + completeness + ns(duration_min, 4) + language + long_transp_barr, 
           data = amb_ds, family = binomial)

summary(fit)


eff.age <- Effect("age", fit)
eff.dur <- Effect("duration_min", fit)

plot(eff.age)
plot(eff.dur)


# Check up ----------------------------------------------------------------

spat_vars_check <- spatial_vars %>% 
  filter(!place %in%  c("Ente Ospedaliero Cantonale")) %>% 
  group_by(MEDSTAT04) %>% 
  filter(rank(duration_min) == 1) %>% 
  ungroup()

sci_check <- left_join(sci, spat_vars_check, by = c("medstat" = "MEDSTAT04"))

check_up_vars <- names(sci_check) %>% str_subset("_check")

sci_check <- sci_check %>% mutate(check_up = if_else(select(., check_up_vars) %>% rowSums(na.rm = T) > 0, 1L, 0L))

fit <- glm(check_up ~ sex + ns(age, 2) + lesion_level + completeness + ns(duration_min, 2) + language + long_transp_barr, 
           data = sci_check, family = binomial)

summary(fit)

merge(round(exp(coef(fit)), 2), round(exp(confint(fit)), 2), by = "row.names", all = TRUE)

eff.age <- Effect("age", fit)
eff.dur <- Effect("duration_min", fit)

plot(eff.age)
plot(eff.dur)




# Inpatient ---------------------------------------------------------------

spat_vars_inpat <- spatial_vars %>% 
  filter(!place %in%  c("Ente Ospedaliero Cantonale", "Plein Soleil")) %>% 
  group_by(MEDSTAT04) %>% 
  filter(rank(duration_min) == 1) %>% 
  ungroup()

sci_inpat %>% select(ends_with("_inpat"))

sci_inpat <- left_join(sci, spat_vars_inpat, by = c("medstat" = "MEDSTAT04")) %>% 
  filter(hc_inpatient == 1) %>% 
  mutate(inpat_paracenter = if_else(rowSums(select(., ends_with("_inpat")), na.rm = T) > 0L, 1L, 0L))



fit <- glm(inpat_paracenter ~ sex + ns(age, 2) + lesion_level + ns(duration_min, 2) + language + long_transp_barr, 
           data = sci_inpat, family = binomial)

summary(fit)

merge(round(exp(coef(fit)), 2), round(exp(confint(fit)), 2), by = "row.names", all = TRUE)

eff.age <- Effect("age", fit)
eff.dur <- Effect("duration_min", fit)

plot(eff.age)
plot(eff.dur)


