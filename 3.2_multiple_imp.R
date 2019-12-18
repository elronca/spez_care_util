
## Multiple imputation

library(mice)
library(tidyverse)
library(naniar)
library(mice)
library(parallel)

sci <- readRDS(file.path("workspace", "outcome_vars_prepared.Rdata"))


naniar::miss_summary(sci)["miss_var_summary"] %>% unnest(cols = c(miss_var_summary)) %>% print(n = 100)
naniar::miss_summary(sci)["miss_case_summary"] %>% unnest(cols = c(miss_case_summary)) 

sci %>% slice(1499) %>% glimpse()
sci %>% slice(1499) %>% add_n_miss %>% pull(n_miss_all)
sci %>% slice(1499) %>% add_prop_miss %>% pull(prop_miss_all)



# Variable selection ------------------------------------------------------


# Used for imputation and will be imputed

predictor_vars <- c("sex", "age", "time_since_sci", "lesion_level", "completeness", "etiology", 
                    "financial_hardship", "short_transp_barr", "long_transp_barr", 
                    "problem_pressure", "problem_urinary", "problem_sexual", "problem_spasticity", 
                    "problem_respiratory", "problem_sleep", "problem_injury", "problem_contractures", 
                    "problem_ossification", "problem_bladder", "problem_bowel", "problem_dysreflexia", 
                    "problem_hypotension", "problem_circulatory", "problem_diabetes", "problem_heart", 
                    "problem_cancer", "problem_depression", "problem_pain", "scim_20", "dist_ambulant", 
                    "dist_checkup", "dist_inpat", "language", "degurba")

sci <- sci %>% mutate_at(vars(starts_with("problem_")), as.factor)
sci$scim_rasch <- as.numeric(sci$scim_rasch)

sci <- sci %>% 
  mutate_at(vars(financial_hardship, short_transp_barr, long_transp_barr, scim_20), ~factor(., ordered = T))

select(sci, predictor_vars) %>% miss_summary %>% pluck("miss_var_summary") %>% .[[1]] %>% print(n = 50)


# Used for imputation but will not be imputed


# Outcome vars

outcome_vars <- c("hc_ambulant", "hc_inpatient", "hc_ambulant_parac", "hc_parac_check", "hc_inpatient_parac", "hc_parac_check")
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

if(F) {
  predictor_matrix <- make.predictorMatrix(sci, blocks = make.blocks(sci))
  predictor_matrix[, other_vars_to_keep] <- 0
}



# Define variables not to be imputed -----------------------------------------

if(F) {
  
  vars_to_impute <- is.na(sci)
  vars_to_impute[, outcome_vars] <- FALSE
  vars_to_impute[, scim_vars] <- FALSE
  vars_to_impute[, other_vars_to_keep] <- FALSE

}


# Start imputation --------------------------------------------------------

n_cores <- detectCores(all.tests = FALSE, logical = TRUE)

start_imp <- Sys.time()

imp <- parlmice(sci, n.core = n_cores, cluster.seed = 1, n.imp.core = 2)

end_imp <- Sys.time()

end_imp - start_imp

imp$predictorMatrix
imp$loggedEvents

saveRDS(imp, file.path("workspace", "imputed_sci.RData"))

imp_long <- mice::complete(imp, "long")

imp_long %>% 
  group_by(.imp) %>% 
  summarize(n_NA = sum(is.na(completeness)))



naniar::miss_summary(imp_long)["miss_var_summary"] %>% unnest(cols = c(miss_var_summary)) %>% print(n = 75)

sapply(sci_sel_short, function(x) sum(is.na(x)))





# Literature --------------------------------------------------------------


# The influx of a variable quantifies how well its missing data connect to the observed data on other variables. 
# The outflux of a variable quantifies how well its observed data connect to the missing data on other variables. 
# In general, higher influx and outflux values are preferred.

# https://stefvanbuuren.name/fimd/missing-data-pattern.html

flux(sci[, -1])[, 1:3] %>% as_tibble(rownames = "my_var") %>% arrange(desc(outflux)) %>% print(n = 30)

mice::fluxplot(sci)

# In general, variables that are located higher up in the display are more complete and thus potentially more 
# useful for imputation. In practice variables closer to the subdiagonal are typically better connected than 
# those farther away. The fluxplot can be used to spot variables that clutter the imputation model. 
# Variables that are located in the lower regions (especially near the lower-left corner) and that are 
# uninteresting for later analysis are better removed from the data prior to imputation.

# A variable with high outflux may turn out to be useless for imputation if it is unrelated to the incomplete variables. 
# On the other hand, the usefulness of a highly predictive variable is severely limited by a low outflux.
# More refined measures of usefulness are conceivable, e.g., multiplying outflux by the average proportion 
# of explained variance. Also, we could specialize to one or a few key variables to impute. 

# https://stefvanbuuren.name/fimd/missing-data-pattern.html


# A useful feature of the mice() function is the ability to specify the set
# of predictors to be used for each incomplete variable. The basic specifcation
# is made through the predictorMatrix argument, which is a square matrix
# of size ncol(data) containing 0/1 data. Each row in predictorMatrix identifes 
# which predictors are to be used for the variable in the row name. If
# diagnostics=T (the default), then mice() returns a mids object containing
# a predictorMatrix entry.

# imp <- mice(nhanes, print = FALSE)
# imp$predictorMatrix

#      age bmi hyp chl
# age   0   1   1   1
# bmi   1   0   1   1
# hyp   1   1   0   1
# chl   1   1   1   0

# The rows correspond to incomplete target variables, in the sequence as
# they appear in the data. A value of 1 indicates that the column variable is
# a predictor to impute the target (row) variable, and a 0 means that it is not
# used.

# Thus, in the above example, bmi is predicted from age, hyp and chl.
# Note that the diagonal is 0 since a variable cannot predict itself. Since age
# contains no missing data, mice() silently sets all values in the row to 0.



# As a general rule, using every bit of available information yields 
# multiple imputations that have minimal bias and maximal efficiency.
# It is often beneficial to choose as large a number of predictors as possible. 
# Including as many predictors as possible tends to make the MAR (missing at random) assumption more plausible, 
# thus reducing the need to make special adjustments for MNAR (missing not at random) mechanisms.

# For datasets containing hundreds or thousands of variables, using all predictors may not be feasible 
# (because of multicollinearity and computational problems) to include all these variables. 
# It is also not necessary. In my experience, the increase in explained variance in linear regression is 
# typically negligible after the best, say, 15 variables have been included. For imputation purposes, 
# it is expedient to select a suitable subset of data that contains no more than 15 to 25 variables.

# Strategy for selecting predictor variables from a large database:

# 1)
# Include all variables that appear in the complete-data model, i.e., the model that will be applied to 
# the data after imputation, including the outcome (Little 1992; Moons et al. 2006). 
# Failure to do so may bias the complete-data model, especially if the complete-data model contains 
# strong predictive relations. Note that this step is somewhat counter-intuitive, 
# as it may seem that imputation would artificially strengthen the relations of the complete-data model, 
# which would be clearly undesirable. If done properly however, this is not the case. 
# On the contrary, not including the complete-data model variables will tend to bias the results toward zero. 
# Note that interactions of scientific interest also need to be included in the imputation model.

# 2)
# In addition, include the variables that are related to the nonresponse. 
# Factors that are known to have influenced the occurrence of missing data 
# (stratification, reasons for nonresponse) are to be included on substantive grounds. 
# Other variables of interest are those for which the distributions differ between 
# the response and nonresponse groups. These can be found by inspecting their 
# correlations with the response indicator of the variable to be imputed. 
# If the magnitude of this correlation exceeds a certain level, then the variable should be included.

# 3)
# In addition, include variables that explain a considerable amount of variance. 
# Such predictors help reduce the uncertainty of the imputations. 
# They are basically identified by their correlation with the target variable.

# 4)
# Remove from the variables selected in steps 2 and 3 those variables that have 
# too many missing values within the subgroup of incomplete cases. 
# A simple indicator is the percentage of observed cases within this subgroup, 
# the percentage of usable cases.

# https://stefvanbuuren.name/fimd/sec-modelform.html

