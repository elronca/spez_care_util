
## Multiple imputation

library(mice)
library(tidyverse)
library(naniar)
library(parallel)

sci <- readRDS(file.path("workspace", "outcome_vars_prepared.Rdata"))

names(sci) %>% str_subset("arrangement")

naniar::miss_summary(sci)["miss_var_summary"] %>% unnest(cols = c(miss_var_summary)) %>% print(n = 100)
naniar::miss_summary(sci)["miss_case_summary"] %>% unnest(cols = c(miss_case_summary)) 

sci %>% slice(1499) %>% glimpse()
sci %>% slice(1499) %>% add_n_miss %>% pull(n_miss_all)
sci %>% slice(1499) %>% add_prop_miss %>% pull(prop_miss_all)

names(sci) %>% str_subset("inpatient")



# Variable selection ------------------------------------------------------


# Used for imputation and will be imputed

predictor_vars <- c("sex", "age", "time_since_sci", "lesion_level", "completeness", "etiology", 
                    "financial_hardship", "short_transp_barr", "long_transp_barr", "liv_arrangement",
                    "problem_pressure", "problem_urinary", "problem_sexual", "problem_spasticity", 
                    "problem_respiratory", "problem_sleep", "problem_injury", "problem_contractures", 
                    "problem_ossification", "problem_bladder", "problem_bowel", "problem_dysreflexia", 
                    "problem_hypotension", "problem_circulatory", "problem_diabetes", "problem_heart", 
                    "problem_cancer", "problem_depression", "problem_pain", "scim_20", 
                    "dist_amb_check_up", "dist_inpat", "language", "degurba", "hc_inpatient_days", 
                    "hc_inpatient_num", "hc_ambulant_num")

sci <- sci %>% mutate_at(vars(starts_with("problem_")), as.factor)
sci$scim_rasch <- as.numeric(sci$scim_rasch)

sci <- sci %>% 
  mutate_at(vars(financial_hardship, short_transp_barr, long_transp_barr, scim_20), ~factor(., ordered = T))

select(sci, predictor_vars) %>% miss_summary %>% pluck("miss_var_summary") %>% .[[1]] %>% print(n = 50)


# Used for imputation but will not be imputed

# Outcome vars

outcome_vars <- c("hc_ambulant", "hc_inpatient", "hc_parac_check", "hc_ambulant_parac", "hc_inpatient_parac")
sci <- sci %>% mutate_at(vars(outcome_vars), as.factor)

select(sci, outcome_vars) %>% miss_summary %>% pluck("miss_var_summary")


# scim vars

scim_vars <- c("scim_1", "scim_2", "scim_3", "scim_4", "scim_5", "scim_6", "scim_7", 
               "scim_8", "scim_9", "scim_10", "scim_11", "scim_12", "scim_13", 
               "scim_14", "scim_16", "scim_17", "scim_18", "scim_19",
               "scim_21", "scim_22", "scim_23")

if(F) {scim_levels <- readRDS(file.path("workspace", "cb_17.RData")) %>% 
  filter(str_detect(var_name, "scim_"))}

sci <- sci %>% mutate_at(vars(scim_vars), ~factor(., ordered = T))

scim_over_20_per_mis <- select(sci, scim_vars) %>% 
  miss_summary %>% 
  pluck("miss_var_summary") %>% 
  .[[1]] %>% 
  filter(pct_miss > 20) %>% 
  pull(variable)

scim_vars <- setdiff(scim_vars, scim_over_20_per_mis)


# Not used for imputation and won't be imputed

other_vars_to_keep <- c("id_swisci", "medstat")
select(sci, other_vars_to_keep)



# Construct predictor matrix ----------------------------------------------

sci <- sci %>% select(predictor_vars, outcome_vars, scim_vars, other_vars_to_keep)

# The rows correspond to incomplete target variables, in the sequence as
# they appear in the data. A value of 1 indicates that the column variable is
# a predictor to impute the target (row) variable, and a 0 means that it is not
# used.

outlist_constant <- c("id_swisci", "medstat")

outlist_scim <- flux(select(sci, -outlist_constant)) %>% 
  select(pobs, influx, outflux) %>% 
  as_tibble(rownames = "variable") %>% 
  arrange(desc(outflux)) %>% 
  filter(str_detect(variable, "scim_")) %>% 
  filter(outflux < 0.9) %>% 
  pull(variable)


outlist_all <- unique(c(outlist_constant, setdiff(outlist_scim, "scim_20")))

my_data <- sci %>% select(-outlist_all)

mice::fluxplot(my_data)

predictor_matrix <- make.predictorMatrix(data = my_data, blocks = make.blocks(my_data))


# These variables won't be used as predictors in the imputation (columns are the variables that impute the rows)
# But they will be imputed

predictor_matrix[, c("hc_ambulant_num", "hc_inpatient_num", "hc_inpatient_days")] <- 0L

sci %>% select(c("hc_ambulant_num", "hc_inpatient_num", "hc_inpatient_days"))

# Now we want to avoid implausible values to be imputed -------------------

# See: https://www.gerkovink.com/miceVignettes/Passive_Post_processing/Passive_imputation_post_processing.html

# If somebody made an outpatient visit this persons cannot have 0 outpatient visits
# Therefore 0 should not be imputed

my_data %>% filter(hc_ambulant == 0) %>% pull(hc_ambulant_num)
my_data %>% filter(hc_ambulant == 1) %>% pull(hc_ambulant_num) %>% table(useNA = "always")

my_data %>% filter(hc_inpatient == 0) %>% pull(hc_inpatient_num)
my_data <- mutate(my_data, hc_inpatient_num = if_else(hc_inpatient %in% 0L, 0L, hc_inpatient_num))
my_data %>% filter(hc_inpatient == 0) %>% pull(hc_inpatient_num)

my_data %>% filter(hc_inpatient == 0) %>% pull(hc_inpatient_days)
my_data <- mutate(my_data, hc_inpatient_days = if_else(hc_inpatient %in% 0L, 0L, hc_inpatient_days))
my_data %>% filter(hc_inpatient == 0) %>% pull(hc_inpatient_days)

my_data %>% filter(hc_ambulant == 1) %>% pull(hc_ambulant_num) %>% table(useNA = "always")
my_data %>% filter(hc_inpatient == 1) %>% pull(hc_inpatient_num) %>% table(useNA = "always")
my_data %>% filter(hc_inpatient == 1) %>% pull(hc_inpatient_days) %>% table(useNA = "always")


define_constrain <- function(.min, .max) {
  
  str_c("imp[[j]][, i] <- mice::squeeze(imp[[j]][, i], c(", .min, ", ",  .max,  "))")
  
}

# Outpatient visits

min_max_outp_num <- my_data %>% filter(hc_ambulant == 1) %>% pull(hc_ambulant_num) %>% summary() %>% .[c("Min.",  "Max.")]
constrain_outp <- define_constrain(.min = min_max_outp_num[["Min."]], .max = min_max_outp_num[["Max."]])

# Inpatient visits

min_max_inp_num <- my_data %>% filter(hc_inpatient == 1) %>% pull(hc_inpatient_num) %>% summary() %>% .[c("Min.",  "Max.")]
constrain_inp_num <- define_constrain(.min = min_max_inp_num[["Min."]], .max = min_max_inp_num[["Max."]])

# Inpatient LOS

min_max_inp_days <- my_data %>% filter(hc_inpatient == 1) %>% pull(hc_inpatient_days) %>% summary() %>% .[c("Min.",  "Max.")]
constrain_inp_days <- define_constrain(.min = min_max_inp_days[["Min."]], .max = min_max_inp_days[["Max."]])

ini <- mice(my_data, maxit = 0, predictorMatrix = predictor_matrix)

meth <- ini$meth

meth["hc_ambulant_num"] <- "pmm"
meth["hc_inpatient_num"] <- "pmm"
meth["hc_inpatient_days"] <- "pmm"

post <- ini$post

post["hc_ambulant_num"] <- constrain_outp
post["hc_inpatient_num"] <- constrain_inp_num
post["hc_inpatient_days"] <- constrain_inp_days


# Start imputation --------------------------------------------------------

n_cores <- detectCores(all.tests = FALSE, logical = TRUE)

n_imp_per_core <- if_else(n_cores <= 4, 2, 1)
if(F) n_imp_per_core <- 10

start_imp <- Sys.time()

imp <- parlmice(data = my_data, 
                predictorMatrix = predictor_matrix,
                meth = meth,
                post = post,
                n.core = n_cores, 
                cluster.seed = 1, 
                n.imp.core = n_imp_per_core)

end_imp <- Sys.time()

end_imp - start_imp

imp$loggedEvents

?mice


# Remove factor order from imputed datasets --------------------------------------

imp <- mice::complete(imp, "long", include = TRUE) %>%
  mutate_if(is.factor, ~factor(., ordered = FALSE)) %>% 
  as.mids()

saveRDS(imp, file.path("workspace", "imputed_sci.RData"))
if(F) imp <- readRDS(file.path("workspace", "imputed_sci.RData"))

imp_long <- mice::complete(imp, "long")

imp_long %>% pull(.imp) %>% unique()

naniar::miss_summary(imp_long)["miss_var_summary"] %>% 
  unnest(cols = c(miss_var_summary)) %>% 
  print(n = 75)

imp_long %>% 
  filter(hc_ambulant == 1) %>% 
  pull(hc_ambulant_num) %>% 
  table(useNA = "always")

imp_long %>% 
  filter(hc_inpatient == 1) %>% 
  pull(hc_inpatient_num) %>% 
  table(useNA = "always")

imp_long %>% 
  filter(hc_inpatient == 1) %>% 
  pull(hc_inpatient_days) %>% 
  table(useNA = "always")


# Diagnostics -------------------------------------------------------------

stripplot(imp, hc_ambulant_num + hc_inpatient_num + hc_inpatient_days ~.imp, jitter=T, layout = c(3, 1))

densityplot(imp)

fit <- with(imp, glm(ici(imp) ~ sex + age + completeness + etiology + language + short_transp_barr + 
                       problem_injury + problem_sexual + problem_ossification, family = binomial))

ps <- rep(rowMeans(sapply(fit$analyses, fitted.values)), imp$m + 1)

xyplot(imp, completeness ~ ps|as.factor(.imp), xlab = "Probability that record is incomplete", ylab = "completeness", cex = 2)


# Clear workspace ---------------------------------------------------------

rm("constrain_inp_days", "constrain_inp_num", "constrain_outp", 
  "define_constrain", "end_imp", "fit", "imp", "imp_long", 
  "ini", "meth", "min_max_inp_days", "min_max_inp_num", "min_max_outp_num", 
  "my_data", "n_cores", "n_imp_per_core", "other_vars_to_keep", "outcome_vars", 
  "outlist_all", "outlist_constant", "outlist_scim", "post", "predictor_matrix", 
  "predictor_vars", "ps", "sci", "scim_over_20_per_mis", "scim_vars", 
  "start_imp")
