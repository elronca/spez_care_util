
library(tidyverse)
library(splines)
library(mice)

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
         dist_amb_check_up_cat = cut(dist_amb_check_up, breaks = c(0, 30, 60, 90, Inf), labels = c("0-30 min", "31-60 min", "60-90 min", "90+ min")),
         dist_inpat_cat = cut(dist_inpat, breaks = c(0, 30, 60, 90, Inf), labels = c("0-30 min", "31-60 min", "60-90 min", "90+ min"))) %>% 
  as.mids()


# Formulas ----------------------------------------------------------------

## Identify best variables to predict specific outcomes 

get_best_vars <- function(imp_data, predictor_vars, response_var, p_val_threshold = 0.05) {
  
  
  if(F) { # For debugging
    
    soc_dem_all  <- c("sex", "age", "time_since_sci", "completeness", "etiology", 
                      "financial_hardship", "short_transp_barr", "long_transp_barr", "language", "degurba")
    
    imp_data <-  imp
    predictor_vars <- soc_dem_all
    response_var <- "hc_parac_check"
    
  }
  
  
  
  
  # Choose best predictors that are provided via predictor vars argument
  
  # Those variables that are important predictors in every single imputed file
  # will be included in the next step
  
  choose_vars <- function(imp_data, predictor_vars, response_var) {
    
    predictors <-  str_c("~", str_c(predictor_vars, collapse = " + ")) %>% rlang::parse_expr(.)
    
    scope <- list(upper = predictors, lower = ~1)
    
    regres_formula <- str_c(response_var, " ~ 1")
    
    fit <- mice::complete(imp_data, "all") %>% 
      map(~step(
        glm(formula = as.formula(regres_formula), family = binomial(link = 'logit'), data = .), 
        scope = scope, trace = 0))
    
    formulas <- lapply(fit, formula)
    terms <- lapply(formulas, terms)
    votes <- unlist(lapply(terms, labels))
    
    my_table <- as_tibble(table(votes)) %>% arrange(desc(n)) %>% print()
    
    return(my_table)
    
  }
  
  my_table_soc_dem <- choose_vars(imp_data, predictor_vars, response_var)
  
  
  
  
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
      formula_without <- str_c(response_var, " ~ ", str_c(all_include, collapse = " + "))
      
      # Regression formulas with all the variables to include plus the additinal candidate variable
      formula_with <- maybe_include %>% str_c(str_c(all_include, collapse = " + "), ., sep = " + ") %>% str_c(response_var, " ~ ", .)
      
      # Fit regression with all the variables to include
      fit.without <- with(imp_data, glm(formula = as.formula(formula_without), family = binomial(link = 'logit')))
      fit.without$analyses
      
      # Fit multiple regression with all the variables to include plut the additinal candiadate variables
      fit.with <- map(formula_with, ~with(imp_data, glm(formula = as.formula(.), family = binomial(link = 'logit'))))
      
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
  my_table_all <- choose_vars(imp_data, my_vars, response_var)
  
  # Evaluate the condidate variables
  vars_to_include <- evaluate_borderline_vars(my_table_all)
  
  
  ## This is the final output, the variables that were significant in all imputed datasets
  ## plus the candidate variabels that were significant in an additinal analysis using a
  ## likelihood ratio test.
  
  return(vars_to_include)
  
} 





## Formula to get the estimates 

get_estimates <- function(imp_data_long, formula, .exp = TRUE) {
  
  res <- imp_data_long %>% 
    group_by(.imp) %>%
    do(model = glm(formula = as.formula(formula), family = binomial(link = 'logit'), data = .)) %>%
    as.list() %>%
    .[[-1]] %>% 
    pool() %>% 
    summary(conf.int = T, exponentiate = .exp) %>% 
    as_tibble(rownames = "variable") %>% 
    mutate(p.value = round(p.value, 3)) %>% 
    select(variable, estimate, LL = `2.5 %`, UL = `97.5 %`, p.value)
  
  if(.exp) {
    return(rename(res, OR = estimate))
  }else{
    return(res)
  }
  
}

get_res <- function(data, OR, LL, UL) {
  
  fC <- function(x) {formatC(x, format = "f", digits = 2)}
  with(data, str_c(fC(OR), " (", fC(LL), " - ", fC(UL), ")"))
  
}


# Check-up visits ----------------------------------------------------------------------------------------


soc_dem_all  <- c("sex", "age_cat", "time_since_sci", "completeness", "etiology", 
                  "financial_hardship", "long_transp_barr", "language", "degurba", 
                  "dist_amb_check_up")

# "short_transp_barr", 

best_vars_check_up <- get_best_vars(imp_data = imp, 
                                    predictor_vars = soc_dem_all, 
                                    response_var = "hc_parac_check", 
                                    p_val_threshold = 0.05)


imp_data_long <- mice::complete(imp, "long")

imp_data_long <- imp_data_long %>% mutate(sex = fct_relevel(sex, c("male", "female")))

formula_reg <- "hc_parac_check ~ sex + age_cat + completeness + etiology + language + short_transp_barr + 
problem_injury + problem_sexual + problem_ossification"

formula_reg <- str_remove(formula_reg, "\n")

check_up_OR <- get_estimates(imp_data_long = imp_data_long, formula = formula_reg, .exp = T)

mutate(check_up_OR, res = get_res(check_up_OR, OR, LL, UL)) %>% 
  select(variable, res, p.value) %>% 
  slice(-1) %>% 
  write.csv2(file.path("output", "reg_res_check_up.csv"), row.names = FALSE)

saveRDS(check_up_OR, file.path("workspace", "check_up_OR.RData"))

# Outpatient visits -------------------------------------------------------

outp <- mice::complete(imp, "long", include = TRUE) %>% 
  filter(hc_ambulant == 1) %>% 
  as.mids()

# "short_transp_barr", 

soc_dem_all  <- c("sex", "age_cat", "time_since_sci", "completeness", "etiology", 
                  "financial_hardship", "long_transp_barr", "language", "degurba",
                  "dist_amb_check_up_cat")


best_vars_outp <- get_best_vars(imp_data = outp, 
                                predictor_vars = soc_dem_all, 
                                response_var = "hc_ambulant_parac", 
                                p_val_threshold = 0.05)


outp_data_long <- mice::complete(outp, "long")

formula_reg <- "hc_ambulant_parac ~ completeness + dist_amb_check_up_cat + language + problem_bladder + problem_cancer + problem_spasticity"

formula_reg <- str_remove(formula_reg, "\n")

outp_OR <- get_estimates(imp_data_long = outp_data_long, formula = formula_reg, .exp = T)
saveRDS(outp_OR, file.path("workspace", "outp_OR.RData"))

mutate(outp_OR, res = get_res(outp_OR, OR, LL, UL)) %>% 
  select(variable, res, p.value) %>% 
  slice(-1) %>% 
  write.csv2(file.path("output", "reg_res_outpatient.csv"), row.names = FALSE)



# Inpatient visits -------------------------------------------------------

inpat <- mice::complete(imp, "long", include = TRUE) %>% 
  filter(hc_inpatient == 1) %>% 
  as.mids()

# "short_transp_barr", 

soc_dem_all  <- c("sex", "age_cat", "time_since_sci", "completeness", "etiology", 
                  "financial_hardship", "long_transp_barr", "language", 
                  "dist_inpat_cat")

best_vars_inpat <- get_best_vars(imp_data = inpat, 
                                 predictor_vars = soc_dem_all, 
                                 response_var = "hc_inpatient_parac", 
                                 p_val_threshold = 0.05)


inpat_data_long <- mice::complete(inpat, "long")

formula_reg <- "hc_inpatient_parac ~ sex + dist_inpat_cat + etiology + completeness + problem_diabetes"

formula_reg <- str_remove(formula_reg, "\n")

inp_OR <- get_estimates(imp_data_long = inpat_data_long, formula = formula_reg, .exp = T)

mutate(inp_OR, res = get_res(inp_OR, OR, LL, UL)) %>% 
  select(variable, res, p.value) %>% 
  slice(-1) %>% 
  write.csv2(file.path("output", "reg_res_inpatient.csv"), row.names = FALSE)

saveRDS(inp_OR, file.path("workspace", "estim_inp.RData"))



# Save estimates and imputed dataset --------------------------------------

saveRDS(imp, file.path("workspace", "imputed_and_modified_ds.RData"))

rm("best_vars_check_up", "best_vars_inpat", "best_vars_outp", 
  "check_up_OR", "formula_reg", "get_best_vars", "get_estimates", 
  "get_res", "imp", "imp_data_long", "imp_long", "inp_OR", "inpat", 
  "inpat_data_long", "outp", "outp_data_long", "outp_OR", "soc_dem_all")
