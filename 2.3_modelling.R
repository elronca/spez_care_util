
library(tidyverse)
library(mice)

imp_raw <- readRDS(file.path("workspace", "imputed_sci.RData"))

imp_raw$m

imp_raw$data %>% names()


# Relevel variables -------------------------------------------------------

imp_raw$data %>% select(starts_with("dist")) %>% as_tibble()


had_no_influence <- c("had no influence", "made my life a bit more difficult")

imp <- imp_raw %>% 
  
  mice::complete("long", include = TRUE) %>% 
  
  mutate(long_transp_barr = fct_collapse(long_transp_barr, "had no influence" = had_no_influence)) %>% 
  
  mutate(financial_hardship = fct_collapse(financial_hardship, "had no influence" = had_no_influence)) %>% 
  
  mutate(short_transp_barr = fct_collapse(short_transp_barr, "had no influence" = had_no_influence)) %>% 
  
  mutate_at(vars(lesion_level, completeness), as.character) %>% 
  
  mutate(severity = case_when(
    lesion_level == "paraplegia" & completeness == "incomplete" ~ "incomplete para",
    lesion_level == "paraplegia" & completeness == "complete" ~ "complete para",
    lesion_level == "tetraplegia" & completeness == "incomplete" ~ "incomplete tetra",
    lesion_level == "tetraplegia" & completeness == "complete" ~ "complete tetra",
    TRUE ~ NA_character_),
    
    completeness = fct_relevel(as.factor(completeness), "incomplete", "complete"),
    
    severity = fct_relevel(as.factor(severity), c("incomplete para",  "incomplete tetra", "complete para", "complete tetra"))) %>% 
  
  mutate(age_cat = cut(age, breaks = c(0, 30, 45, 60, 75, Inf), labels = c("16-30", "31-45", "46-60","61-75", "75+")),
         time_since_sci_years = time_since_sci/12,
         time_since_sci_years_cat = cut(time_since_sci_years, breaks = c(0, 4, 9, 14, Inf), labels = c("<=4", "5-9","10-14", "15+")),
         hc_inpatient_num_cat = cut(hc_inpatient_num, breaks = c(-1, 2, 4, Inf), labels = c("1", "2-4", "5+")),
         hc_ambulant_num_cat = cut(hc_ambulant_num, breaks = c(-1, 2, 4, Inf), labels = c("1", "2-4", "5+")),
         hc_inpatient_days_cat = cut(hc_inpatient_days, breaks = c(-1, 5, 20, Inf), labels = c("1-5", "6-20", "21+")),
         sex = fct_relevel(sex, c("male", "female")),
         dist_amb_check_up_cat = cut(dist_amb_check_up, breaks = c(0, 30, 60, 90, Inf), labels = c("0-30 min", "31-60 min", "60-90 min", "90+ min")),
         dist_inpat_cat = cut(dist_inpat, breaks = c(0, 30, 60, 90, Inf), labels = c("0-30 min", "31-60 min", "60-90 min", "90+ min"))) %>% 
  
  as.mids()


# Formulas ----------------------------------------------------------------

## Identify best variables to predict specific outcomes using a step wise procedure that tests all combinations of the provided variables 
## in all datasets. 
## Predictors that are significantly associated with the outcome on a significance level provided by the user (.p_val_threshold)
## will be further tested using a Wald test and kept if it is associated with the variables that were associated with the 
## outcome in all imputed datasets.

## Procedure is performed according to https://stefvanbuuren.name/fimd/sec-stepwise.html

## In a second step, additional variables can be provided (as vector) to test whether they provide any benefit over the model that was
## defined in the first step

get_best_vars <- function(.imp_data, .predictor_vars, .response_var, .reg_family = "binomial", .p_val_threshold = 0.05, .add_vars = NULL) {
  
  # .imp_data in the regular (not long) format
  # .predictor_vars, as vector of strings
  # .response_var, the outcome variables as string
  # .reg_family, family function of the glm provided as string
  
  if(F) {
    .imp_data <- imp
    .predictor_vars <- my_predictors
    .response_var <- "hc_parac_check"
    .reg_family <- "binomial"
    .p_val_threshold <- 0.05
    .add_vars <- NULL
  }
 
  
  test_predictors <- function(.imp_data, .predictor_vars, .response_var) {
    
    # Construct predictor part of formula (string, variables separated by a + )
    
    predictors <- str_c("~", str_c(.predictor_vars, collapse = " + ")) %>% rlang::parse_expr(.)
    
    scope <- list(upper = predictors, lower = ~1)
    
    regres_formula <- str_c(.response_var, " ~ 1")
    
    fit <- mice::complete(.imp_data, "all") %>% 
      map(~step(
        glm(formula = as.formula(regres_formula), family = .reg_family, data = .), 
        scope = scope, trace = 0))
    
    formulas <- lapply(fit, formula)
    terms <- lapply(formulas, terms)
    votes <- unlist(lapply(terms, labels))
    
    my_table <- as_tibble(table(votes)) %>% arrange(desc(n)) %>% print()
    
    return(my_table)
    
  }
  
  votes_to_include <- test_predictors(.imp_data, .predictor_vars, .response_var)
  
  
  
  
  # Now we evaluate and select predictors that were not important predictors in every single imputed dataset
  # We do that by fitting two models, one with the predictors that were significant in each imputed dataset
  # and with the same predictors but an additional boarderline variable
  # We define borderline predictors as predictors that were significantly associated with the outcome on
  # a 5% significance level in more than half of the imputed datasets
  
  evaluate_borderline_vars <- function(candidate_vars) {
    
    var_sel <- mutate(candidate_vars, 
                      is_included = as.integer(n == max(n)),
                      is_tested = as.integer(n >= max(n) / 2 & n != max(n))) %>% print()
    
    include <- filter(var_sel, is_included == 1) %>% pull(votes)
    maybe_include <- filter(var_sel, is_tested == 1) %>% pull(votes)
    
    if(length(maybe_include) == 0) {
      
      return(include)
      
    } else {
      
      # Regression formula with all the variables to include
      formula_without <- str_c(.response_var, " ~ ", str_c(include, collapse = " + "))
      
      # Regression formulas with all the variables to include plus the additinal candidate variable
      formula_with <- maybe_include %>% str_c(str_c(include, collapse = " + "), ., sep = " + ") %>% str_c(.response_var, " ~ ", .)
      
      # Fit regression with all the variables to include
      fit.without <- with(.imp_data, glm(formula = as.formula(formula_without), family = .reg_family))
      fit.without$analyses
      
      # Fit multiple regression with all the variables to include plut the additinal candiadate variables
      fit.with <- map(formula_with, ~with(.imp_data, glm(formula = as.formula(.), family = .reg_family)))
      
      # Get the p values from the Wald test between the models with and without the candidate variables
      p_vals <- fit.with %>% map(~D1(., fit.without)) %>% map(~pluck(., "result")) %>% map_chr(4)
      
      # Show the candidate variables and the corresponding p values
      maybe_include_tested <- tibble(vars = maybe_include, p_vals = p_vals) %>% print()
      
      # Keep or remove the candidate variables depending on their effect in the regression model measured as p value
      # and depending on the predefined p value threshold 
      maybe_include_kept <- filter(maybe_include_tested, p_vals < .p_val_threshold) %>% pull(vars)
      
      
      # Return p values that were siginificantly associated with the outcome variables in the regression models with
      # all imputed datasets and the variables that were only significant in half or more of the datasets 
      
      return(c(include, maybe_include_kept))
      
    }
    
  }
  
  ## Run the function 
  
  results_2nd_step <- evaluate_borderline_vars(votes_to_include)
  
  
  ## In a second round we check whether some additional variables have an effect on the outcomes that we are
  ## sure we are going to include (actually not 100% sure whether this is a valid approach)
  
  chosen_vars_and_add_vars <- c(results_2nd_step, .add_vars)
  
  
  # Run the function to choose the predictor variables
  
  final_votes_to_include <- test_predictors(.imp_data, chosen_vars_and_add_vars, .response_var)
  
  
  # Evaluate the additional predictors using Wald test
  
  my_vars_for_final_regression <- evaluate_borderline_vars(final_votes_to_include)
  
  
  ## This is the final output, the variables that were significant in all imputed datasets
  ## plus the candidate variabels that were significant in an additinal analysis using a
  ## likelihood ratio test.
  
  return(my_vars_for_final_regression)
  
}



## Modify the predictors for the regression models

modify_predictors <- function(base_vars, remove_vars = NULL, add_vars = NULL, add_spline = NULL, as_formula = FALSE) {
  
  my_vars <- setdiff(c(base_vars, add_vars), remove_vars)
  
  if(length(add_spline) != 0) {
    
    my_vars <- str_c(str_c(my_vars, collapse = " + "), " + ", add_spline)
    
  }
  
  if(as_formula) {
    
    my_vars <- str_c(my_vars, collapse = " + ")
    
  }
  
  return(my_vars)
  
}


## Formula to get the estimates 

get_estimates <- function(.imp_data, .outcome_var, .formula_predictors, 
                          save_fit = FALSE, save_p_values = FALSE, save_OR = FALSE) {
  
  if(length(.formula_predictors) != 1) stop("formula_predictors needs to be a string, not a vector")
  
  
  # Fit the logistic regression
  
  if(F){
    .imp_data <- imp_inp
    .outcome_var <- "hc_inpatient_parac"
    .formula_predictors <- form_pred_inpat
  }
  
  fit <- with(.imp_data, glm(as.formula(str_c(.outcome_var, " ~ ", .formula_predictors)), family = "binomial"))
  
  
  # Calculate p values for the different variables using a likelihood ratio test
  
  # Get all predictors
  all_pred <- .formula_predictors %>% 
    str_split(" \\+ ") %>% 
    unlist()
  
  
  # Get vectors of a combination of all predictors where always one predictor is missing
  
  single_pred_missing <- map(all_pred, function(x) setdiff(all_pred, x))
  single_pred_missing <- map(single_pred_missing, function(x) str_c(x, collapse = " + "))
  names(single_pred_missing) <- all_pred
  
  
  # Get fitted models where always one predictor is missing and compare these models to the originnal models
  # using a log likelihood ratio test
  
  get_alternative_fits <- function(x) {
    
    with(.imp_data, glm(as.formula(str_c(.outcome_var, " ~ ", x)), family = binomial))
    
  }
  
  fit_alt <- map(single_pred_missing, get_alternative_fits)
  
  p_values <- map(fit_alt, function(x) D3(fit, x)) %>% 
    map(~pluck(., "result")) %>% map_chr(4)
  
  if(length(p_values[as.numeric(p_values) > 0.05]) != 0) {
    
    cat("The variable(s)",  names(p_values[as.numeric(p_values) > 0.05]), 
        "has/have a p value greater than 0.05 in the likelihood ratio test\n\n")
    
  }
  
  
  
  
  res <- summary(pool(fit), conf.int = TRUE, exponentiate = TRUE) %>% 
    as_tibble(rownames = "variable")
  
  res %>% 
    mutate_at(vars(estimate, `2.5 %`, `97.5 %`), ~formatC(., format = "f", digits = 2)) %>% 
    mutate(p.value = formatC(p.value, format = "f", digits = 3)) %>% 
    mutate(OR = str_c(estimate, " (", `2.5 %`, " \u2013 ",`97.5 %`, ")")) %>% 
    select(variable, OR, p.value) %>% 
    slice(-1) %>%
    write.csv2(file.path("output", str_c("reg_res_", .outcome_var, ".csv")), row.names = FALSE)
  
  
  
  if(save_fit) {
    
    saveRDS(fit, file.path("workspace", str_c("fit_", .outcome_var, ".RData")))
    
  }
  
  if(save_p_values) {
    
    saveRDS(p_values, file.path("workspace", str_c("p_values_", .outcome_var, ".RData")))
    
  }
  
  if(save_OR) {
    
    saveRDS(res, file.path("workspace", str_c("OR_table_", .outcome_var, ".RData")))
    
  }
  
  
  return(list("regression results" = res, "likelihood ratio test" = p_values))
  
  
}




# Some preconfigurations --------------------------------------------------

# General variables to test

soc_dem_all  <- c("sex", "age_cat", "time_since_sci_years_cat", "severity", "etiology", "liv_arrangement", 
                  "financial_hardship", "short_transp_barr", "long_transp_barr", "language",  "degurba")


# Health conditions to test

shc_vars <- mice::complete(imp, "all")[[1]] %>% names() %>% str_subset("problem")
length(shc_vars)


# Should models be saved to csv files

save_models <- TRUE



# Check-up visits ----------------------------------------------------------------------------------------

# Variables to test as predictors, secondary health conditions will be added automatically

my_predictors <- modify_predictors(soc_dem_all, add_vars = "dist_amb_check_up")


# Test predictors for effect on outcome

best_vars_check_up <- get_best_vars(.imp_data = imp, 
                                    .predictor_vars = my_predictors, 
                                    .response_var = "hc_parac_check", 
                                    .p_val_threshold = 0.05,
                                    .add_vars = shc_vars)


# Choose good predictors and make formula

form_pred_check_up <- modify_predictors(best_vars_check_up, as_formula = TRUE, 
                                        remove_vars = "time_since_sci_years_cat", 
                                        add_vars = NULL)


# Save fit and table with effects of predictors on outcome measured as odds ratios

get_estimates(.imp_data = imp, 
              .outcome_var = "hc_parac_check", 
              .formula_predictors = form_pred_check_up,
              save_fit = F,
              save_p_values = F)


form_pred_check_up <- modify_predictors(best_vars_check_up, 
                                        as_formula = TRUE,
                                        remove_vars = c("problem_diabetes"),
                                        add_vars = NULL)

get_estimates(.imp_data = imp, 
              .outcome_var = "hc_parac_check", 
              .formula_predictors = form_pred_check_up,
              save_fit = save_models,
              save_p_values = save_models,
              save_OR = save_models)


# Outpatient visits -------------------------------------------------------

imp_outp <- mice::complete(imp, "long", include = TRUE) %>% 
  filter(hc_ambulant == 1) %>% 
  as.mids()

imp_outp$loggedEvents

my_vars <- modify_predictors(soc_dem_all, add_vars = c("dist_amb_check_up_cat", "hc_ambulant_num_cat"))

best_vars_outp <- get_best_vars(.imp_data = imp_outp, 
                                .predictor_vars = my_vars, 
                                .response_var = "hc_ambulant_parac", 
                                .p_val_threshold = 0.05,
                                .add_vars = shc_vars)


## These variables are added as we want to have them in every model

form_pred_outpat <- modify_predictors(best_vars_outp, 
                                      remove_vars = NULL, 
                                      add_vars = c("sex", "age_cat", "severity", "dist_amb_check_up_cat"),
                                      as_formula = TRUE)

get_estimates(.imp_data = imp_outp, 
              .outcome_var = "hc_ambulant_parac", 
              .formula_predictors = form_pred_outpat,
              save_fit = F,
              save_p_values = F)


form_pred_outpat <- modify_predictors(best_vars_outp, 
                                      remove_vars = NULL,
                                      add_vars = c("sex", "age_cat", "severity", "dist_amb_check_up_cat"),
                                      as_formula = TRUE)

get_estimates(.imp_data = imp_outp, 
              .outcome_var = "hc_ambulant_parac", 
              .formula_predictors = form_pred_outpat,
              save_fit = save_models,
              save_p_values = save_models,
              save_OR = save_models)


# Inpatient visits -------------------------------------------------------

imp_inp <- mice::complete(imp, "long", include = TRUE) %>% 
  filter(hc_inpatient == 1) %>% 
  as.mids()

imp_inp$loggedEvents

my_vars <- modify_predictors(soc_dem_all, add_vars = c("dist_inpat_cat", "hc_inpatient_days_cat", "hc_inpatient_num_cat"))

best_vars_inpat <- get_best_vars(.imp_data = imp_inp, 
                                 .predictor_vars = my_vars, 
                                 .response_var = "hc_inpatient_parac", 
                                 .p_val_threshold = 0.05,
                                 .add_vars = shc_vars)

form_pred_inpat <- modify_predictors(best_vars_inpat, as_formula = TRUE, 
                                     add_vars = c("age_cat"), 
                                     remove_vars = NULL)

get_estimates(.imp_data = imp_inp, 
              .outcome_var = "hc_inpatient_parac", 
              .formula_predictors = form_pred_inpat,
              save_fit = F,
              save_p_values = F)


form_pred_inpat <- modify_predictors(best_vars_inpat, as_formula = TRUE, 
                                     remove_vars = c("problem_contractures", "problem_diabetes", "problem_heart"),
                                     add_vars = c("age_cat"))

get_estimates(.imp_data = imp_inp, 
              .outcome_var = "hc_inpatient_parac", 
              .formula_predictors = form_pred_inpat,
              save_fit = save_models,
              save_p_values = save_models,
              save_OR = save_models)


# Save best vars ----------------------------------------------------------

final_vars <- list(form_pred_check_up, form_pred_outpat, form_pred_inpat) %>% 
  map(~unlist(str_split(.," \\+ "))) %>% 
  set_names(c("form_pred_check_up", "form_pred_outpat", "form_pred_inpat"))

saveRDS(final_vars, file.path("workspace", "final_vars.RData"))



# Final OR table ----------------------------------------------------------

OR_check <- readRDS(file.path("workspace", "OR_table_hc_parac_check.RData"))
OR_ambulant <- readRDS(file.path("workspace", "OR_table_hc_ambulant_parac.RData"))
OR_inpatient <- readRDS(file.path("workspace", "OR_table_hc_inpatient_parac.RData"))

get_OR_table <- function(fit) {
  
  fit %>% 
    select(variable, estimate, LL = `2.5 %`, UL = `97.5 %`,  p.value) %>% 
    mutate(OR = str_c(
      formatC(estimate, format = "f", 2), " (", 
      formatC(LL, format = "f", 2), "\u2013",
      formatC(UL, format = "f", 2), ")")) %>% 
    mutate(p.value = formatC(p.value, format = "f", 3)) %>% 
    select(variable, OR, p.value) %>% 
    filter(variable != "(Intercept)")
  
}

LR_table_check <- get_OR_table(OR_check) %>% rename(check_OR = OR, check_p_value = p.value)
LR_table_outp <- get_OR_table(OR_ambulant) %>% rename(outp_OR = OR, outp_p_value = p.value)
LR_table_inp <- get_OR_table(OR_inpatient) %>% rename(inp_OR = OR, inp_p_value = p.value)

final_table_LR <- list(LR_table_check, LR_table_outp, LR_table_inp) %>% reduce(full_join, by = "variable")

final_table_LR$variable %>% unique() %>% dput()

cat_order <- c("sexfemale",
  
  "age_cat31-45", "age_cat46-60", "age_cat61-75", "age_cat75+",
  
  "severityincomplete tetra", "severitycomplete para", "severitycomplete tetra",  
  
  "etiologynontraumatic", 
  
  "problem_sexual1",  "problem_spasticity1", "problem_ossification1", "problem_cancer1", 
  
  "languageFrench", "languageItalian", 
  
  "dist_amb_check_up_cat31-60 min", "dist_amb_check_up_cat60-90 min", "dist_amb_check_up_cat90+ min",
  
  "hc_ambulant_num_cat2-4", "hc_ambulant_num_cat5+", 
  
  "hc_inpatient_num_cat2-4", "hc_inpatient_num_cat5+", 
  
  "hc_inpatient_days_cat6-20", "hc_inpatient_days_cat21+"
)


mutate_all(final_table_LR, ~if_else(is.na(.), "", .)) %>% 
  slice(match(cat_order, variable)) %>% 
  write.csv2(file.path("output", "logistic_regression_table.csv"), row.names = FALSE)



# Clear workspace ---------------------------------------------------------

rm("best_vars_check_up", "best_vars_inpat", "best_vars_outp", 
  "cat_order", "final_table_LR", "final_vars", "form_pred_check_up", 
  "form_pred_inpat", "form_pred_outpat", "get_best_vars", "get_estimates", 
  "get_OR_table", "had_no_influence", "imp", "imp_inp", "imp_outp", 
  "imp_raw", "LR_table_check", "LR_table_inp", "LR_table_outp", 
  "modify_predictors", "my_predictors", "my_vars", "OR_ambulant", 
  "OR_check", "OR_inpatient", "save_models", "shc_vars", "soc_dem_all"
)
