
library(tidyverse)
library(emmeans)


fit_check <- readRDS(file.path("workspace", "fit_hc_parac_check.RData"))
fit_ambulant <- readRDS(file.path("workspace", "fit_hc_ambulant_parac.RData"))
fit_inpatient <- readRDS(file.path("workspace", "fit_hc_inpatient_parac.RData"))

best_vars <- readRDS(file.path("workspace", "best_vars.RData"))

check_rg <- ref_grid(fit_check)

check_pw_comp <- function(x) {
  
  as.data.frame(pairs(x)) %>% 
  select(contrast, OR = odds.ratio, p_value = p.value) %>% 
  mutate(
    OR = round(OR, 2),
    p_value = round(p_value, 3),
    OR_inv = round(1 / OR, 2)) %>% 
  select(contrast, OR, OR_inv, p_value)
  
}


## Check age



check.emm.age <- emmeans(check_rg, "age_cat", type = "response")
check_pw_comp(check.emm.age) %>% slice(1:4)
confint(check.emm.age)
plot(check.emm.age)


## Check completeness of injury

check.emm.compl <- emmeans(check_rg, "completeness", type = "response")
check_pw_comp(check.emm.compl)
confint(check.emm.compl)
plot(check.emm.compl)


## Sex

check.emm.sex <- emmeans(check_rg, "sex", type = "response")
check_pw_comp(check.emm.sex)
confint(check.emm.sex)
plot(check.emm.sex)
















get_marginals <- function(categories) {
  
  (emmeans(fit_check, categories, type = "response")) %>% 
    as_tibble() %>% 
    rename(variable = 1) %>% 
    mutate(variable = as.character(variable))
  
}

best_vars$best_vars_check_up %>% 
  set_names(.) %>% 
  map_dfr(get_marginals, .id = "variable")


