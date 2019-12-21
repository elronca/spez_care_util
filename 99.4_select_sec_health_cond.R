
## Select secondary health conditions which we want to have included in the model

library(tidyverse)
library(broom)


sci <- readRDS(file.path("workspace", "outcome_vars_prepared.Rdata"))


## Get a vector of the variable names with the secondary and chronic health conditions

health_cond <- names(sci) %>% str_subset("problem_")

sci <- mutate_at(sci, vars(health_cond), as.factor)


## Function to study the effect of each secondary and chronic health condition on the study outcomes
## in an univariable logistic regression.

eff_SHC <- function(predictor, outcome, df){
  
  fit <- glm(expr(!!ensym(outcome) ~ !!ensym(predictor)), data = df, family = binomial(link = "logit"))
  
  if(F) {print(na.action(fit))}
  if(T) {print(naprint(na.action(fit)))}
  
  res <- tidy(fit, exponentiate = TRUE, conf.int = TRUE) %>% mutate(p.value = round(p.value, 3))
  
  return(res)
  
}

## Check effect of secondary and chronic health conditions on propensity to visit a paracenter instead of
## another hospital for an outpatient visit

amb <- map_dfr(health_cond, eff_SHC, outcome = hc_ambulant_parac, df = filter(sci, hc_ambulant == 1)) %>% 
  filter(term != "(Intercept)") %>% 
  filter(p.value < 0.05) %>% 
  mutate(util_type = "amb")


## Check effect of secondary and chronic health conditions on propensity to get a check-up

check <- map_dfr(health_cond, eff_SHC, outcome = hc_parac_check, df = sci) %>% 
  filter(term != "(Intercept)") %>% 
  filter(p.value < 0.05) %>% 
  mutate(util_type = "check")


## Check effect of secondary and chronic health conditions on propensity to visit a paracenter instead of
## another hospital for an inpatient visit

inpat <- map_dfr(health_cond, eff_SHC, outcome = hc_inpatient_parac, df = filter(sci, hc_inpatient == 1)) %>% 
  filter(term != "(Intercept)") %>% 
  filter(p.value < 0.05) %>% 
  mutate(util_type = "inpat")

SHC_severe_chron <- bind_rows(amb, check, inpat) %>% 
  arrange(term) %>% 
  select(term, estimate, conf.low, conf.high, p.value, util_type)

count(SHC_severe_chron, term, sort = T)



# Do the same things for all categories of the secondary health conditions --------

SHC_all_levels <- readRDS(file.path("workspace", "SHC_all_levels.RData"))

sci_core <- select(sci, id_swisci, hc_ambulant_parac, hc_parac_check, hc_inpatient_parac, hc_ambulant, hc_inpatient) %>% 
  full_join(SHC_all_levels, by = "id_swisci") %>% 
  mutate_at(vars(health_cond), as.factor)


amb <- map_dfr(health_cond, eff_SHC, outcome = hc_ambulant_parac, df = filter(sci_core, hc_ambulant == 1)) %>% 
  filter(term != "(Intercept)") %>% 
  filter(p.value < 0.05) %>% 
  mutate(util_type = "amb")


## Check effect of secondary and chronic health conditions on propensity to get a check-up

check <- map_dfr(health_cond, eff_SHC, outcome = hc_parac_check, df = sci_core) %>% 
  filter(term != "(Intercept)") %>% 
  filter(p.value < 0.05) %>% 
  mutate(util_type = "check")


## Check effect of secondary and chronic health conditions on propensity to visit a paracenter instead of
## another hospital for an inpatient visit

inpat <- map_dfr(health_cond, eff_SHC, outcome = hc_inpatient_parac, df = filter(sci_core, hc_inpatient == 1)) %>% 
  filter(term != "(Intercept)") %>% 
  filter(p.value < 0.05) %>% 
  mutate(util_type = "inpat")

SHC_all_levels <- bind_rows(amb, check, inpat) %>% 
  arrange(term) %>% 
  select(term, estimate, conf.low, conf.high, p.value, util_type)




# Count SHC ---------------------------------------------------------------

count_severe <- count(SHC_severe_chron, term, sort = T) %>% 
  mutate(term = str_remove(term, "1"))

count_all_lev <- count(SHC_all_levels, term, sort = T) %>% 
  mutate(term = str_remove(term, "1|2|3")) %>% 
  distinct()

full_join(count_severe, count_all_lev, by = "term") %>% 
  set_names("term", "severe", "all_lev")











