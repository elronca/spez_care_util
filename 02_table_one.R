

library(tidyverse)

Sys.setenv(LANGUAGE = 'en')

sci <- readRDS(file.path("workspace", "manually_imputed.Rdata"))


# Prepare some data

sci <- mutate(sci, time_since_sci = time_since_sci / 12)


# Get missings in specific variables

sum(is.na(sci$medstat))

names(sci)


# Calculate relative frequencies of categories across discrete variables ---------------------------------------

get_bt_discrete_vars <- function(df, .tp = "ts1", vars_to_remove = "tp", vars_to_remove_missings = "",
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

names(sci)

table_2012 <- get_bt_discrete_vars(sci, vars_to_remove_missings = c("lesion_level", "etiology"))
table_2017 <- get_bt_discrete_vars(sci, .tp = "ts2", vars_to_remove_missings = c("lesion_level", "etiology"))


# Merge tables of different years to one final table depicting the relative frequencies of discrete variables

table_discrete_vars <- full_join(table_2012, table_2017, by = c("variable", "category")) %>% 
  mutate(variable = replace(variable, duplicated(variable), ""))

write.csv2(table_discrete_vars, file.path("output", "tab_cat_vars_all.csv"), row.names = FALSE)


## Get table one for those only who participated in both SwiSCI surveys

table_2012 <- get_bt_discrete_vars(sci, vars_to_remove_missings = c("lesion_level", "etiology"),  .module_hcu_12 = T)
table_2017 <- get_bt_discrete_vars(sci, .tp = "ts2", vars_to_remove_missings = c("lesion_level", "etiology"),  .module_hcu_12 = T)

table_discrete_vars_hsr <- full_join(table_2012, table_2017, by = c("variable", "category")) %>% 
  mutate(variable = replace(variable, duplicated(variable), ""))

rm(table_2012, table_2017)

write.csv2(table_discrete_vars_hsr, file.path("output", "tab_cat_vars_hsr.csv"), row.names = FALSE)


## Get table one for those only who participated in both HSR surveys

table_2012 <- get_bt_discrete_vars(sci, vars_to_remove_missings = c("lesion_level", "etiology"),  hsr_only = T)
table_2017 <- get_bt_discrete_vars(sci, .tp = "ts2", vars_to_remove_missings = c("lesion_level", "etiology"), hsr_only = T)

table_discrete_vars_hsr_only <- full_join(table_2012, table_2017, by = c("variable", "category")) %>% 
  mutate(variable = replace(variable, duplicated(variable), ""))

write.csv2(table_discrete_vars_hsr_only, file.path("output", "tab_cat_vars_hsr_only.csv"), row.names = FALSE)



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



# Continuous socio-demographic variables of all participants ------------------------------------

cont_vars_2012 <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci"))
cont_vars_2017 <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci"), .tp = "ts2")

table_continous_vars <- full_join(cont_vars_2012, cont_vars_2017, by = c("variable"))

write.csv2(table_continous_vars, file.path("output", "tab_num_vars_all.csv"), row.names = FALSE)



# Continous socio-demographic variables of participants who answered the HSR questionnaire in 2012 --------------------------

cont_vars_2012 <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci"),  .module_hcu_12 = TRUE)
cont_vars_2017 <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci"), .tp = "ts2", .module_hcu_12 = TRUE)

table_continous_vars_hsr <- full_join(cont_vars_2012, cont_vars_2017, by = c("variable"))

write.csv2(table_continous_vars_hsr, file.path("output", "tab_num_vars_hsr.csv"), row.names = FALSE)

rm(cont_vars_2012, cont_vars_2017)


# Continous socio-demographic variables of only those participants who answered HSR questions in both years -------------------

cont_vars_2012 <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci"),  hsr_only = TRUE)
cont_vars_2017 <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci"), .tp = "ts2", hsr_only = TRUE)

table_continous_vars_hsr_only <- full_join(cont_vars_2012, cont_vars_2017, by = c("variable"))

write.csv2(table_continous_vars_hsr_only, file.path("output", "tab_num_vars_hsr_only.csv"), row.names = FALSE)


# Clear workspace ----------------------------------------------------------------------------------------------------------

rm("cont_vars_2012", "cont_vars_2017", "table_2012", "table_2017", "table_continous_vars", "table_continous_vars_hsr", 
   "table_continous_vars_hsr_only", "table_discrete_vars", "table_discrete_vars_hsr", "table_discrete_vars_hsr_only")


# Reorder healthcare utilization variables

hc_vars <- select(sci, starts_with("hc_")) %>% names() %>% {
  
  c(str_subset(., "hc_practitioner"),
    str_subset(., "hc_ambulant"), 
    str_subset(., "hc_inpatient"), 
    str_subset(., "hc_paraplegic"),
    str_subset(., "hc_paracenter"))
  
}


# Categorical healthcare variables of all participants who responded to the HSR questions ------------------------------------

hc_vars_cat <- str_subset(hc_vars, "_num|_days", negate = TRUE)

sci <- sci %>% mutate_at(vars(hc_vars_cat), ~if_else(is.na(.), 0L, .))

hc_2012 <- get_bt_continous_vars(sci, my_vars = hc_vars_cat, get_iqr = FALSE, get_prop = TRUE, .module_hcu_12 = TRUE)
hc_2017 <- get_bt_continous_vars(sci, my_vars = hc_vars_cat, get_iqr = FALSE, get_prop = TRUE, .tp = "ts2")

table_hc_vars <- full_join(hc_2012, hc_2017, by = c("variable")) %>% 
  mutate(n_rel_freq_2012 = if_else(str_detect(variable, "paracenter"), "", n_rel_freq_2012))

write.csv2(table_hc_vars, file.path("output", "tab_hc_vars_cat.csv"), row.names = FALSE)

cat("Some people may have indicated that they made an ambulant visit but they do not state, whether it was planned or unplanned
    furthermore, some have indicated that they did not make ambulant visits but they later on responded that they had an
    ambulant visit to a specialized clinic, therefore the hc_ambulant was recoded")



# Categorical healthcare variables of those participants who responded to the HSR questions in both surveys -------------------

hc_vars_cat <- str_subset(hc_vars, "_num|_days", negate = TRUE)

sci <- sci %>% mutate_at(vars(hc_vars_cat), ~if_else(is.na(.), 0L, .))

hc_2012 <- get_bt_continous_vars(sci, my_vars = hc_vars_cat, get_iqr = FALSE, get_prop = TRUE, 
                                 hsr_only = TRUE)
hc_2017 <- get_bt_continous_vars(sci, my_vars = hc_vars_cat, get_iqr = FALSE, get_prop = TRUE, 
                                 .tp = "ts2", hsr_only = TRUE)

table_hc_vars_hc_hsr_only <- full_join(hc_2012, hc_2017, by = c("variable")) %>% 
  mutate(n_rel_freq_2012 = if_else(str_detect(variable, "paracenter"), "", n_rel_freq_2012))

write.csv2(table_hc_vars_hc_hsr_only, file.path("output", "table_hc_vars_hc_hsr_only.csv"), row.names = FALSE)



# Calculate number of visits to health care provider for those who at least once visited a provider at least once ----------------------


## Function

get_provider_visits_iqr <- function(.provider_dic, .provider_num, HSR_both_only, .tp = "ts1") {
  
  if(HSR_both_only) {
    
  ids_hsr_both_surveys <- filter(sci, module_hcu_12 %in% 1) %>% 
    count(id_swisci) %>% 
    filter(n == 2) %>% 
    pull(id_swisci)
  
  sci <- filter(sci, id_swisci %in% ids_hsr_both_surveys)
  
  }
  
  quants <- sci %>% 
    
    filter(tp == .tp & module_hcu_12 == 1, !!sym(.provider_dic) == 1) %>% 
    
    summarize(quants = list(enframe(quantile(!!sym(.provider_num), probs = c(0.25, 0.5, 0.75), na.rm = T)))) %>% 
    
    unnest(cols = c(quants)) %>% 
    
    mutate(value = round(value, 0))
  
  quants_str <- str_c(quants[2, "value"], " (", quants[1, "value"], "\u2013", quants[3, "value"], ")")
  names(quants_str) <- .provider_num
  
  return(quants_str)
  
}



## Providers

provider_dic <- c("hc_practitioner", "hc_paraplegic", 
                  "hc_ambulant", "hc_ambulant_planned", "hc_ambulant_unplanned", 
                  "hc_inpatient", "hc_inpatient")

provider_num <- c("hc_practitioner_num", "hc_paraplegic_num", 
                  "hc_ambulant_num", "hc_ambulant_planned_num", "hc_ambulant_unplanned_num",
                  "hc_inpatient_num", "hc_inpatient_days")


## Get data

hcu_12_all <- map2(.x = provider_dic, .y = provider_num, get_provider_visits_iqr, .tp = "ts1", HSR_both_only = FALSE) %>% 
  unlist() %>% enframe(name = "provider", "n_visits_2012_hsr_all")

hcu_12_same <- map2(.x = provider_dic, .y = provider_num, get_provider_visits_iqr, .tp = "ts1", HSR_both_only = TRUE) %>% 
  unlist() %>% enframe(name = "provider", "n_visits_2012_same_persons")


hcu_17_all <- map2(.x = provider_dic, .y = provider_num, get_provider_visits_iqr, .tp = "ts2", HSR_both_only = TRUE) %>% 
  unlist() %>% enframe(name = "provider", "n_visits_2017_hsr_all")

hcu_17_same <- map2(.x = provider_dic, .y = provider_num, get_provider_visits_iqr, .tp = "ts2", HSR_both_only = FALSE) %>% 
  unlist() %>% enframe(name = "provider", "n_visits_2017_same_persons")


## Join into table

n_visits_table <- list(hcu_12_all, hcu_17_all, hcu_12_same, hcu_17_same) %>% reduce(left_join, by = "provider")

write.csv2(n_visits_table, file.path("output", "n_visits_table_IQR.csv"), row.names = FALSE)


# Get proportion of visits to SCI centers by insitution and utilization type --------------

vars_paracenter <- str_subset(hc_vars_cat, "hc_paracenter")

paracenter_rf <- sci %>% 
  filter(hc_paracenter == 1) %>% 
  get_bt_continous_vars(my_vars = vars_paracenter, get_iqr = FALSE, get_prop = TRUE, .tp = "ts2") %>% 
  filter(variable != "hc_paracenter")
  
write.csv2(paracenter_rf, file.path("output", "paracenter_rf.csv"), row.names = FALSE)


# Clear workspace ---------------------------------------------------------

rm("get_bt_continous_vars", "get_bt_discrete_vars", "get_provider_visits_iqr", 
  "hc_2012", "hc_2017", "hc_vars", "hc_vars_cat", "hcu_12_all", 
  "hcu_12_same", "hcu_17_all", "hcu_17_same", "n_visits_table", 
  "paracenter_rf", "provider_dic", "provider_num", "sci", "table_hc_vars", 
  "table_hc_vars_hc_hsr_only", "vars_paracenter")
