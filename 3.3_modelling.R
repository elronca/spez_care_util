
library(tidyverse)
library(splines)
library(mice)
library(emmeans)

imp <- readRDS(file.path("workspace", "imputed_sci.RData"))


# Some descriptives -------------------------------------------------------

imp_long <- imp %>% mice::complete("long", include = TRUE)

names(imp_long)

imp_long %>% 
  filter(.imp == 1) %>% 
  mutate_at(vars(starts_with("hc_")), ~as.integer(as.character(.))) %>% 
  
  summarize(n = n(),
            n_check_up = sum(hc_parac_check),
            n_amb = sum(hc_ambulant),
            n_amb_parac = sum(hc_ambulant_parac),
            n_inp = sum(hc_inpatient),
            n_inp_parac = sum(hc_inpatient_parac)) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "n_visits") %>% 
  mutate(prop_util = 100 * n_visits / 1530) %>% 
  mutate(prop_util_2 = case_when(
    variable == "n_amb_parac" ~ 100 * 235 / 713,
    variable == "n_inp_parac" ~ 100 * 178 / 403,
    TRUE ~ NA_real_))


# Relevel variables -------------------------------------------------------

imp <- imp %>% 
  mice::complete("long", include = TRUE) %>% 
  
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
         time_since_sci_cat = cut(time_since_sci/12, breaks = c(0, 1, 4, 9, 14, Inf), labels = c("<1", "1-4", "5-9","10-14", "15+")),
         hc_inpatient_num_cat = cut(hc_inpatient_num, breaks = c(-1, 2, 4, Inf), labels = c("1", "2-4", "5+")),
         hc_ambulant_num_cat = cut(hc_ambulant_num, breaks = c(-1, 2, 4, Inf), labels = c("1", "2-4", "5+")),
         hc_inpatient_days_cat = cut(hc_inpatient_days, breaks = c(-1, 5, 20, Inf), labels = c("1-5", "6-20", "21+")),
         sex = fct_relevel(sex, c("male", "female")),
         dist_amb_check_up_cat = cut(dist_amb_check_up, breaks = c(0, 30, 60, 90, Inf), labels = c("0-30 min", "31-60 min", "60-90 min", "90+ min")),
         dist_inpat_cat = cut(dist_inpat, breaks = c(0, 30, 60, 90, Inf), labels = c("0-30 min", "31-60 min", "60-90 min", "90+ min"))) %>%
  as.mids()


# Formulas ----------------------------------------------------------------

## Identify best variables to predict specific outcomes 

get_best_vars <- function(.imp_data, .predictor_vars, .response_var, p_val_threshold = 0.05) {
  
  

  # Choose best predictors that are provided via predictor vars argument
  
  # Those variables that are important predictors in every single imputed file
  # will be included in the next step
  
  choose_vars <- function(.imp_data, .predictor_vars, .response_var) {
    
    predictors <-  str_c("~", str_c(.predictor_vars, collapse = " + ")) %>% rlang::parse_expr(.)
    
    scope <- list(upper = predictors, lower = ~1)
    
    regres_formula <- str_c(.response_var, " ~ 1")
    
    fit <- mice::complete(.imp_data, "all") %>% 
      map(~step(
        glm(formula = as.formula(regres_formula), family = binomial(link = 'logit'), data = .), 
        scope = scope, trace = 0))
    
    formulas <- lapply(fit, formula)
    terms <- lapply(formulas, terms)
    votes <- unlist(lapply(terms, labels))
    
    my_table <- as_tibble(table(votes)) %>% arrange(desc(n)) %>% print()
    
    return(my_table)
    
  }
  
  my_table_soc_dem <- choose_vars(.imp_data, .predictor_vars, .response_var)
  
  
  
  
  # Now we evaluate and select predictors that were not important predictors in every single imputed dataset 
  
  evaluate_borderline_vars <- function(candidate_vars) {
    
    if(F) { # For debugging
      
      p_val_threshold <- 0.05
      my_table_soc_dem <- my_table
      candidate_vars <- my_table_soc_dem
      
      candidate_vars <- my_table_all
      
      maybe_include <- c("degurba", "time_since_sci")
      
    }
    
    
    
    
    var_sel <- mutate(candidate_vars, 
                      include = as.integer(n == max(n)),
                      test = as.integer(n >= max(n) / 2 & n != max(n))) %>% print()
    
    all_include <- filter(var_sel, include == 1) %>% pull(votes)
    maybe_include <- filter(var_sel, test == 1) %>% pull(votes)
    
    if(length(maybe_include) == 0) {
      
      return(all_include)
      
    } else {
      
      # Regression formula with all the variables to include
      formula_without <- str_c(.response_var, " ~ ", str_c(all_include, collapse = " + "))
      
      # Regression formulas with all the variables to include plus the additinal candidate variable
      formula_with <- maybe_include %>% str_c(str_c(all_include, collapse = " + "), ., sep = " + ") %>% str_c(.response_var, " ~ ", .)
      
      # Fit regression with all the variables to include
      fit.without <- with(.imp_data, glm(formula = as.formula(formula_without), family = "binomial"))
      fit.without$analyses
      
      # Fit multiple regression with all the variables to include plut the additinal candiadate variables
      fit.with <- map(formula_with, ~with(.imp_data, glm(formula = as.formula(.), family = "binomial")))
      
      # Get the p values from the likelihood ratio test between the models with and without the candidate variables
      p_vals <- fit.with %>% map(~D3(., fit.without)) %>% map(~pluck(., "result")) %>% map_chr(4)
      
      # Show the candidate variables and the corresponding p values
      maybe_include_tested <- tibble(vars = maybe_include, p_vals = p_vals) %>% print()
      
      # Keep or remove the candidate variables depending on their effect in the regression model measured as p value
      # and depending on the predefined p value threshold 
      maybe_include_kept <- filter(maybe_include_tested, p_vals < p_val_threshold) %>% pull(vars)
      
      
      # Return p values that were siginificantly associated with the outcome variables in the regression models with
      # all imputed datasets and the variables that were only significant in half or more of the datasets 
      
      return(c(all_include, maybe_include_kept))
      
    }
    
  }
  
  ## Run the function 
  
  soc_dem_include <- evaluate_borderline_vars(my_table_soc_dem)
  
  
  
  
  
  # Check how well secondary health conditions predict the outcome, using the same algorithm as
  # before for the socio-demographic factors and also including those socio-demographic facotrs
  # that were best at predicting the outcomes
  
  shc_all <- mice::complete(imp, "all")[[1]] %>% names() %>% str_subset("problem")
  
  my_vars <- c(soc_dem_include, shc_all)
  
  
  # Run the function to choose the predictor variables
  my_table_all <- choose_vars(.imp_data, my_vars, .response_var)
  
  # Evaluate the condidate variables
  vars_to_include <- evaluate_borderline_vars(my_table_all)
  
  
  ## This is the final output, the variables that were significant in all imputed datasets
  ## plus the candidate variabels that were significant in an additinal analysis using a
  ## likelihood ratio test.
  
  return(vars_to_include)
  
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

get_estimates <- function(.imp_data, .outcome_var, .formula_predictors, save_fit = FALSE, save_p_values = FALSE) {
  
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
    
    cat("The variable",  names(p_values[as.numeric(p_values) > 0.05]), 
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


return(list("regression results" = res, "likelihood ratio test" = p_values))


}



# Selection of predictor variables tested for all models ----------------------------------------

soc_dem_all  <- c("sex", "age_cat", "time_since_sci_cat", "severity", "etiology", 
                  "financial_hardship", "short_transp_barr", "long_transp_barr", 
                  "language", "degurba")




# Check-up visits ----------------------------------------------------------------------------------------

# Variables to test as predictors, secondary health conditions will be added automatically



my_vars <- modify_predictors(soc_dem_all, add_vars = "dist_amb_check_up")


# Test predictors for effect on outcome

best_vars_check_up <- get_best_vars(.imp_data = imp, 
                                    .predictor_vars = my_vars, 
                                    .response_var = "hc_parac_check", 
                                    p_val_threshold = 0.05)


# Chhose good predictors and make formula

form_pred_check_up <- modify_predictors(best_vars_check_up, as_formula = TRUE, remove_vars = NULL)


# Save fit and table with effects of predictors on outcome measured as odds ratios

get_estimates(.imp_data = imp, 
              .outcome_var = "hc_parac_check", 
              .formula_predictors = form_pred_check_up,
              save_fit = TRUE,
              save_p_values = TRUE)



# Outpatient visits -------------------------------------------------------

imp_outp <- mice::complete(imp, "long", include = TRUE) %>% 
  filter(hc_ambulant == 1) %>% 
  as.mids()

imp_outp$loggedEvents

my_vars <- modify_predictors(soc_dem_all, add_vars = c("dist_amb_check_up_cat", "hc_ambulant_num_cat"))


best_vars_outp <- get_best_vars(.imp_data = imp_outp, 
                                .predictor_vars = my_vars, 
                                .response_var = "hc_ambulant_parac", 
                                p_val_threshold = 0.05)


form_pred_outpat <- modify_predictors(best_vars_outp, remove_vars = NULL, add_vars = NULL, as_formula = TRUE)

get_estimates(.imp_data = imp_outp, 
              .outcome_var = "hc_ambulant_parac", 
              .formula_predictors = form_pred_outpat,
              save_fit = TRUE,
              save_p_values = TRUE)

# Inpatient visits -------------------------------------------------------

imp_inp <- mice::complete(imp, "long", include = TRUE) %>% 
  filter(hc_inpatient == 1) %>% 
  as.mids()

imp_inp$loggedEvents

complete(imp_inp, "long") %>% select(severity) %>% as_tibble
complete(imp_inp, "long") %>% select(hc_inpatient_days, hc_inpatient_days_cat)

complete(imp_inp, "long") %>% select(time_since_sci, time_since_sci_cat)


my_vars <- modify_predictors(soc_dem_all, add_vars = c("dist_inpat_cat", "hc_inpatient_days_cat", "hc_inpatient_num_cat"))

best_vars_inpat <- get_best_vars(.imp_data = imp_inp, 
                                 .predictor_vars = my_vars, 
                                 .response_var = "hc_inpatient_parac", 
                                 p_val_threshold = 0.05)

form_pred_inpat <- modify_predictors(best_vars_inpat, as_formula = TRUE, remove_vars = NULL)

get_estimates(.imp_data = imp_inp, 
              .outcome_var = "hc_inpatient_parac", 
              .formula_predictors = form_pred_inpat,
              save_fit = TRUE,
              save_p_values = TRUE)


# Save best vars ----------------------------------------------------------

best_vars <- list(form_pred_check_up, form_pred_outpat, form_pred_inpat) %>% 
  map(~unlist(str_split(.," \\+ "))) %>% 
  set_names(c("form_pred_check_up", "form_pred_outpat", "form_pred_inpat"))

saveRDS(best_vars, file.path("workspace", "best_vars.RData"))


# Clear workspace ---------------------------------------------------------

rm("best_vars", "best_vars_check_up", "best_vars_inpat", "best_vars_outp", 
  "form_pred_check_up", "form_pred_inpat", "form_pred_outpat", 
  "get_best_vars", "get_estimates", "imp", "imp_inp", "imp_long", 
  "imp_outp", "modify_predictors", "my_vars", "soc_dem_all")
