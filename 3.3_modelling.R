
library(tidyverse)
library(splines)
library(mice)

imp <- readRDS(file.path("workspace", "imputed_sci.RData"))




# Formulas ----------------------------------------------------------------


## Identify best variables to predict specific outcomes 

get_best_vars <- function(imp_data, predictor_vars, response_var, p_val_threshold = 0.05) {
  
  
  
  
  if(F) { # For debugging
  
  soc_dem_all  <- c("sex", "age", "time_since_sci", "lesion_level", "completeness", "etiology", 
                    "financial_hardship", "short_transp_barr", "long_transp_barr", "language", "degurba",
                    "dist_checkup")
  
  imp_data <-  imp
  predictor_vars <- soc_dem_all
  response_var <- "hc_parac_check"
  
  }
  
  
  # Choose best predictors that are provided via predictor vars argument
  
  choose_vars <- function(imp_data, predictor_vars, response_var) {
    
    predictors <- predictor_vars %>%
      str_c(collapse = " + ") %>%
      str_c("~", .) %>%
      rlang::parse_expr(.)
    
    scope <- list(upper = predictors, lower = ~1)
    
    form <- str_c(response_var, " ~ 1")
    
    fit <- imp_data %>%
      
      mice::complete("all") %>%
      
      lapply(function(x) {
        
        step(
          glm(formula = as.formula(form), family = binomial(link = 'logit'), data = x), 
          scope = scope, trace = 0
          )
        
      })
    
    formulas <- lapply(fit, formula)
    terms <- lapply(formulas, terms)
    votes <- unlist(lapply(terms, labels))
    
    my_table <- as_tibble(table(votes)) %>% arrange(desc(n)) %>% print()
    
    return(my_table)
    
  }
  
  # The best socio-demographic variables are selected
  
  my_table_soc_dem <- choose_vars(imp_data, predictor_vars, response_var)
  
  
  if(F) { # For debugging
    
    p_val_threshold <- 0.05
    my_table_soc_dem <- my_table
    candidate_vars <- my_table_soc_dem
    candidate_vars <- my_table_all
    
  }
  
  # Those variables that are important predictors in every single imputed file
  # will be included in the next step
  
  # Those who were not important predictors in every imputed dataset will be
  # further evaluated
  
  evaluate_borderline_vars <- function(candidate_vars) {
    
    var_sel <- candidate_vars %>%
      mutate(
        include = as.integer(n == max(n)),
        test = as.integer(n >= max(n) / 2 & n != max(n))
      ) %>% print()
    
    all_include <- filter(var_sel, include == 1) %>% pull(votes)
    maybe_include <- filter(var_sel, test == 1) %>% pull(votes)
    
    if(length(maybe_include) == 0) {
      
      return(all_include)
      
    } else {
    
    form_without <- str_c(all_include, collapse = " + ") %>% 
      str_c(response_var, " ~ ", .)
    
    form_with <- maybe_include %>% 
      str_c(str_c(all_include, collapse = " + "), ., sep = " + ") %>% 
      str_c(response_var, " ~ ", .)
    
    fit.without <- with(imp_data, glm(formula = as.formula(form_without), family = binomial(link = 'logit')))
    fit.with <- lapply(form_with, function(x) {with(imp_data, glm(formula = as.formula(x), family = binomial(link = 'logit'))) } )
    
    lapply(fit.with, function(x) D3(x, fit.without))[[1]] %>% pluck("result") %>% .[4]
    
    p_vals <- fit.with %>% 
      map(~D1(., fit.without)) %>% 
      map(~pluck(., "result")) %>%
      map_chr(4)
    
    maybe_include_tested <- tibble(vars = maybe_include, p_vals = p_vals) %>% print()
    maybe_include_kept <- filter(maybe_include_tested, p_vals < p_val_threshold) %>% pull(vars)
    
    return(c(all_include, maybe_include_kept))
    
    }
    
  }
  
  soc_dem_include <- evaluate_borderline_vars(my_table_soc_dem)
  
  
  # Add best secondary health conditions to add
  
  shc_all <- mice::complete(imp, "all")[[1]] %>% names() %>% str_subset("problem")
  
  my_vars <- c(soc_dem_include, shc_all)
  
  my_table_all <- choose_vars(imp_data, my_vars, response_var)
  
  vars_to_include <- evaluate_borderline_vars(my_table_all)
  
  
  ## Those are the variables that we decided to keep
  
  return(vars_to_include)
  
}



## Formula to get the estimates 

get_estimates <- function(imp_data_long, formula) {
  
  res <- imp_data_long %>% 
    group_by(.imp) %>%
    do(model = glm(formula = as.formula(form), family = binomial(link = 'logit'), data = .)) %>%
    as.list() %>%
    .[[-1]] %>% 
    pool() %>% 
    summary(conf.int = T, exponentiate = T) %>% 
    as_tibble(rownames = "variable") %>% 
    mutate(p.value = round(p.value, 3)) %>% 
    select(variable, OR = estimate, LL = `2.5 %`, UL = `97.5 %`, p.value) %>% 
    print()
  
}


# Check-up visits ----------------------------------------------------------------------------------------

soc_dem_all  <- c("sex", "age", "time_since_sci", "lesion_level", "completeness", "etiology", 
              "financial_hardship", "short_transp_barr", "long_transp_barr", "language", "degurba",
              "dist_checkup")

best_vars_check_up <- get_best_vars(imp_data = imp, 
                                    predictor_vars = soc_dem_all, 
                                    response_var = "hc_parac_check", 
                                    p_val_threshold = 0.05)

imp_data_long <- mice::complete(imp, "long")

form <- "hc_parac_check ~ sex + ns(age, 3) + completeness + etiology + language + short_transp_barr +
problem_injury + problem_sexual"

form <- str_remove(form, "\n")

get_estimates(imp_data_long = imp_data_long, formula = form)



# Outpatient visits -------------------------------------------------------

inpat <- imp_long %>% 
  filter(hc_inpatient == 1) %>% 
  as.mids()
imp_long$hc_inpatient
inpat$loggedEvents
  

shc <- inpat %>% filter(.imp == 1) %>% names() %>% str_subset("problem")
shc_sel <- c("problem_hypotension", "problem_ossification", "problem_sexual", "problem_spasticity", "problem_urinary")

soc_dem  <- c("sex", "age", "time_since_sci", "lesion_level", "completeness", "etiology", 
              "financial_hardship", "short_transp_barr", "long_transp_barr", "language", "degurba", "dist_inpat")

soc_dem_sel <- c("age", "sex", "completeness", "degurba", "long_transp_barr")

my_vars <- c(soc_dem_sel, shc)

scope <- my_vars %>% 
  str_c(collapse = " + ") %>% 
  str_c("~", .) %>% 
  list(upper = eval(parse(text = .)), lower = ~1)

my_expr <- expression(
  f1 <- glm(hc_inpatient_parac ~ 1, family = binomial(link = 'logit')), 
  f2 <- step(f1, scope = scope))

fit <- with(inpat, my_expr)

formulas <- lapply(fit$analyses, formula)
terms <- lapply(formulas, terms)
votes <- unlist(lapply(terms, labels))
table(votes)

soc_dem_sel
shc_sel

res_inpat <- inpat %>%
  mice::complete("long") %>% 
  group_by(.imp) %>%
  do(model = glm(hc_inpatient_parac ~  ns(age, 3) + completeness + degurba + long_transp_barr +
                   problem_hypotension + problem_ossification + problem_sexual + problem_spasticity + problem_urinary, 
                 family = binomial(link = 'logit'), data = .)) %>%
  as.list() %>%
  .[[-1]] %>% 
  pool() %>% 
  summary(conf.int = T, exponentiate = T) %>% 
  as_tibble(rownames = "variable") %>% 
  mutate(p.value = round(p.value, 3)) %>% 
  select(variable, OR = estimate, LL = `2.5 %`, UL = `97.5 %`, p.value) %>% 
  print()


