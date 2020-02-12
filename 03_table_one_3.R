

library(tidyverse)

used_vars <- readRDS(file.path("workspace", "vars_for_table_1.RData"))

sci <- readRDS(file.path("workspace", "outcome_vars_prepared.Rdata"))

summary(sci$age)

names(sci) %>% str_subset("dist_")


# Make categories used in regressions -------------------------------------

sci <- sci %>% 
  
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
         hc_inpatient_num_cat = cut(hc_inpatient_num, breaks = c(-1, 0, 1, 4, Inf), labels = c("0", "1", "2-4", "5+"), ),
         hc_ambulant_num_cat = cut(hc_ambulant_num, breaks =  c(-1, 0, 1, 4, Inf), labels = c("0", "1", "2-4", "5+")),
         hc_inpatient_days_cat = cut(hc_inpatient_days, breaks = c(-1, 0, 5, 20, Inf), labels = c("0", "1-5", "6-20", "21+")),
         sex = fct_relevel(sex, c("male", "female")),
         dist_amb_check_up_cat = cut(dist_amb_check_up, breaks = c(0, 30, 60, 90, Inf), labels = c("0-30 min", "31-60 min", "60-90 min", "90+ min")),
         dist_inpat_cat = cut(dist_inpat, breaks = c(0, 30, 60, 90, Inf), labels = c("0-30 min", "31-60 min", "60-90 min", "90+ min")),
         time_since_sci = round(time_since_sci / 12, 0))



# Inspect outcome variables (health care utilization) ---------------------

outcome_vars <- c("hc_parac_check", "hc_ambulant", 
                  "hc_ambulant_parac", "hc_inpatient", 
                  "hc_inpatient", "hc_inpatient_parac")

outcome_vars_data <- select(sci, outcome_vars) %>% mutate_all(~as.integer(as.character(.)))


# Missings

naniar::miss_summary(outcome_vars_data)[["miss_var_summary"]][[1]]


# Frequencies and proportion

N <- nrow(outcome_vars_data)

outcome_vars_data %>% 
  summarize(n = n(),
          n_check_up = sum(hc_parac_check),
          n_amb = sum(hc_ambulant),
          n_amb_parac = sum(hc_ambulant_parac),
          n_inp = sum(hc_inpatient),
          n_inp_parac = sum(hc_inpatient_parac)) %>% 
  pivot_longer(everything(), names_to = "variable", values_to = "n_visits") %>% 
  mutate(prop_util = 100 * n_visits / N) %>% 
  mutate(prop_util_2 = case_when(
    variable == "n_amb_parac" ~ 100 * 235 / 713,
    variable == "n_inp_parac" ~ 100 * 178 / 403,
    TRUE ~ NA_real_)) %>% 
  mutate_at(vars(n_visits:prop_util_2), ~formatC(., format = "f", digits = 0)) %>% 
  print(n = 10) %>% 
  write.csv2(file.path("output", "desc_table_outcome_vars.csv"), row.names = FALSE)



# Other utilization variables ---------------------------------------------

util_vars_ds <- select(sci, 
                       "hc_ambulant_num", "hc_ambulant_num_cat", 
                       "hc_inpatient_num", "hc_inpatient_num_cat", 
                       "hc_inpatient_days", "hc_inpatient_days_cat")

sci %>% filter(hc_inpatient == 0) %>% pull(hc_inpatient_num)

naniar::miss_summary(util_vars_ds)[["miss_var_summary"]][[1]] %>% 
  filter(!variable %in% c("hc_inpatient_num_cat", "hc_inpatient_days_cat", "hc_ambulant_num_cat"))

cat("These results are not really valid, since the denominator contains all records")


## Look at outpatient cases

amb_res_data <- sci %>% filter(hc_ambulant == 1) %>% pull(hc_ambulant_num_cat)
amb_res_data %>% table(useNA = "always")
amb_res_data %>% table(useNA = "always") %>% prop.table() * 100
amb_res_data %>% table() %>% prop.table() * 100


## Look at inpatient cases

# Number of hospitalizations

inp_num_data <- sci %>% filter(hc_inpatient == 1) %>% pull(hc_inpatient_num_cat)
inp_num_data %>% table(useNA = "always")
inp_num_data %>% table(useNA = "always") %>% prop.table() * 100
inp_num_data %>% table() %>% prop.table() * 100


# Length of stay

sci %>% filter(hc_inpatient == 1) %>% select(hc_inpatient_days_cat, hc_inpatient_days)
inp_los_data <- sci %>% filter(hc_inpatient == 1) %>% pull(hc_inpatient_days_cat)
inp_los_data %>% table(useNA = "always")
inp_los_data %>% table(useNA = "always") %>% prop.table() * 100
inp_los_data %>% table() %>% prop.table() * 100


## Get the median and interquartile range of all utilization variables

sci %>% filter(hc_ambulant == 1) %>% pull(hc_ambulant_num) %>% summary()
sci %>% filter(hc_inpatient == 1) %>% pull(hc_inpatient_num) %>% summary()
sci %>% filter(hc_inpatient == 1) %>% pull(hc_inpatient_days) %>% summary()



# Missings in the predictor variables -------------------------------------

used_cat_vars <- c("sex", "age_cat", "lesion_level", "completeness", "severity", "etiology", "language", 
                   "problem_sexual", "problem_spasticity", "problem_injury", "problem_injury", "problem_ossification", 
                   "problem_cancer", "dist_amb_check_up_cat")

analysis_ds <- select(sci, used_cat_vars, "time_since_sci")


## Missings by variable

naniar::miss_summary(analysis_ds)[["miss_var_summary"]][[1]]
table(sci$time_since_sci, useNA = "always")



## Missings by participant

naniar::miss_summary(sci)[["miss_case_summary"]][[1]] %>% 
  mutate(pct_miss_zero = if_else(pct_miss == 0, 1L, 0L)) %>% 
  pull(pct_miss_zero) %>% 
  table()

naniar::miss_summary(select(sci, used_cat_vars))[["miss_case_summary"]][[1]] %>% 
  mutate(pct_miss_zero = if_else(pct_miss == 0, 1L, 0L)) %>% 
  pull(pct_miss_zero) %>% 
  table() %>% 
  prop.table() * 100

# Have a look at sexual dysfunction by sex

sci %>% 
  group_by(sex) %>% 
  count(problem_sexual) %>% 
  mutate(n_cases = sum(n),
         perc = 100 * n / n_cases)



# Calculate relative frequencies of categories across discrete variables ---------------------------------------

get_bt_discrete_vars <- function(df, .tp = "ts2", vars_to_remove = "tp", vars_to_remove_missings = "",
                                 .module_hcu_12 = FALSE, hsr_only = FALSE) {
  
  
  # Reduce dataset to those participants who responded to hsr questions in both surveys
  
  if (hsr_only) {
    
    ids_hsr_both_surveys <- filter(df, module_hcu_12 %in% 1) %>% count(id_swisci) %>% filter(n == 2) %>% pull(id_swisci)
    
    df <- filter(df, id_swisci %in% ids_hsr_both_surveys)
    
  }
  
  
  # Select data from one specific survey
  
  df <- filter(df, tp == .tp)
  year <- if_else(.tp == "ts1", 2012, 2017)
  
  # Reduce dataset to those participants who participated in the hsr module in 2012
  
  if (.module_hcu_12) {df <- df %>% filter(module_hcu_12 %in% 1)}
  
  
  # Calculate total numbers of categories by variable
  
  raw_table <- df %>% 
    
    select_if(is.factor) %>% 
    
    map(function(category) table(category, useNA = "ifany")) %>% 
    
    map_dfr(~as_tibble(.), .id = "variable") %>% 
    
    mutate(variable = if_else(category %in% .tp, "n_tot", variable),
           category = if_else(category %in% .tp, "", category))
  
  
  # Remove defined missings using argument: vars_to_remove_missings
  
  missings_removed <- filter(raw_table, !(is.na(category) & variable %in% vars_to_remove_missings))
  
  
  # Calculate relative frequencies
  
  rel_freq_num <- missings_removed %>% 
    
    group_by(variable) %>%
    
    mutate(prop = n / sum(n) * 100)
  
  
  # Change from number to string, use year in column title
  
  rel_freq_string <- rel_freq_num %>% 
    
    mutate(prop = formatC(prop, digits = 0, format = "f")) %>% 
    
    mutate(!!str_c("n_perc_", year) := str_c(n, " ", "(", prop, ")")) %>% 
    
    ungroup()
  
  
  # Format table
  
  rel_freq_string %>% 
    
    replace_na(list(category = "missing")) %>% 
    
    filter(!variable %in% c(vars_to_remove)) %>% 
    
    filter(n != 0) %>% 
    
    select(-n, -prop)
  
}


## Get table one for all participants


categ_data <- sci %>% 
  select(tp, used_cat_vars) %>% 
  mutate_if(is.integer, ~case_when(. == 1 ~ "yes", 
                                   . == 0 ~ "no",
                                   TRUE ~ NA_character_)) %>% 
  mutate_if(is.character, as.factor)

bt <- get_bt_discrete_vars(categ_data, vars_to_remove_missings = used_cat_vars)

print(bt, n = 35)

write.csv2(bt, file.path("output", "_table_1_final_categ_vars.csv"), row.names = FALSE)



## Get table one for outpatient clinic visitors


categ_data_outp <- sci %>% 
  filter(hc_ambulant == 1) %>% 
  select(tp, used_cat_vars) %>% 
  mutate_if(is.integer, ~case_when(. == 1 ~ "yes", 
                                   . == 0 ~ "no",
                                   TRUE ~ NA_character_)) %>% 
  mutate_if(is.character, as.factor)

bt_outp <- get_bt_discrete_vars(categ_data_outp, vars_to_remove_missings = used_cat_vars)

print(bt_outp, n = 35)

write.csv2(bt_outp, file.path("output", "_table_1_final_categ_vars_outp.csv"), row.names = FALSE)



## Get table one for inpatient care users

used_cat_vars_inp <- c(str_subset(used_cat_vars, "dist_amb_check_up_cat", negate = TRUE), "dist_inpat_cat")


categ_data_inp <- sci %>% 
  filter(hc_inpatient == 1) %>% 
  select(tp, used_cat_vars, dist_inpat_cat, -dist_amb_check_up_cat) %>% 
  mutate_if(is.integer, ~case_when(. == 1 ~ "yes", 
                                   . == 0 ~ "no",
                                   TRUE ~ NA_character_)) %>% 
  mutate_if(is.character, as.factor)

bt_inp <- get_bt_discrete_vars(categ_data_inp, vars_to_remove_missings = used_cat_vars_inp)

print(bt_inp, n = 35)

write.csv2(bt_inp, file.path("output", "_table_1_final_categ_vars_inp.csv"), row.names = FALSE)


# Summarize continuous variables ------------------------------------------------------------------------------------

get_bt_continous_vars <- function(df, n_digits = 0, .tp = "ts1", my_vars, get_iqr = TRUE, get_prop = FALSE, 
                                  .module_hcu_12 = FALSE, hsr_only = FALSE) {
  
  if(sum(get_iqr, get_prop) != 1) stop("Either get_iqr or get_prop has to be true; both cannot")
  
  if(hsr_only) {
    
    ids_hsr_both_surveys <- filter(df, module_hcu_12 %in% 1) %>% 
      count(id_swisci) %>% filter(n == 2) %>% pull(id_swisci)
    
    df <- df %>% filter(id_swisci %in% ids_hsr_both_surveys)
    
  }
  
  df <- filter(df, tp == .tp)
  year <- if_else(.tp == "ts1", 2012, 2017)
  
  
  
  if(.module_hcu_12) {df <- df %>% filter(module_hcu_12 %in% 1)}
  
  
  if(get_iqr) {
    
    res <- df %>% 
      
      select(tp, !!!syms(my_vars)) %>% 
      
      select_if(is.numeric) %>% 
      
      map(~quantile(., c(0.25, 0.5, 0.75), na.rm = TRUE)) %>% 
      
      map(~formatC(., digits = n_digits, format = "f")) %>% 
      
      map(~str_c(.["50%"], " (", .["25%"], "\u2013", .["75%"], ")")) %>% 
      
      map_dfr(~enframe(., name = NULL), .id = "variable") %>%
      
      select(variable, value)
    
    names(res)[names(res) == "value"] <- str_c("num_vars", year, sep = "_")
    
    return(res)
    
  }
  

  if(get_prop) {
    
    res <- df %>% 
      
      select(tp, !!!syms(my_vars)) %>% 
      
      select_if(is.numeric) %>% 
      
      summarize_all(list(~sum(., na.rm = TRUE), ~mean(., na.rm = TRUE))) %>% 
      
      mutate_at(vars(ends_with("_mean")), "*", 100) %>% 
      
      pivot_longer(everything(), 
                   names_to = c("variable", ".value"), 
                   names_pattern = "(.+)_(.+$)") %>% 
      
      mutate(n_rel_freq = str_c(sum, " (", formatC(mean, format = "f", digits = n_digits), ")")) %>% 
      
      select(-sum, -mean) %>% 
      
      add_row(variable = "n_tot", n_rel_freq = str_c(nrow(df), " (100)"), .before = 1)
    
    names(res)[names(res) == "n_rel_freq"] <- str_c("n_rel_freq", year, sep = "_")
    
    return(res)
    
  }
  
}

names(sci)



# Continuous socio-demographic variables of all participants ------------------------------------

bt_cont_vars <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci", "dist_amb_check_up"), .tp = "ts2")

write.csv2(bt_cont_vars, file.path("output", "_table_1_final_cont_vars.csv"), row.names = FALSE)


## Outpatient clinic care users

bt_cont_vars_outp <- get_bt_continous_vars(filter(sci, hc_ambulant == 1), my_vars = c("age", "time_since_sci", "dist_amb_check_up"), .tp = "ts2")

write.csv2(bt_cont_vars_outp, file.path("output", "_table_1_final_cont_vars_outp.csv"), row.names = FALSE)


## Inpatient care users

bt_cont_vars_inp <- get_bt_continous_vars(filter(sci, hc_inpatient == 1), my_vars = c("age", "time_since_sci", "dist_inpat"), .tp = "ts2")

write.csv2(bt_cont_vars_inp, file.path("output", "_table_1_final_cont_vars_inp.csv"), row.names = FALSE)

rm("amb_res_data", "analysis_ds", "bt", "bt_cont_vars", "categ_data", "get_bt_continous_vars", 
   "get_bt_discrete_vars", "inp_los_data", "inp_num_data", "outcome_vars", "outcome_vars_data", 
   "sci", "used_cat_vars", "used_vars", "util_vars_ds")
