

library(tidyverse)
Sys.setenv(LANGUAGE = 'en')

load(file.path("workspace", "manually_imputed.Rdata"))


# Prepare some data

sci <- mutate(sci, time_since_sci = time_since_sci / 12)


# Get missings in specific variables

sum(is.na(sci$medstat))


# Calculate relative frequencies of categories across discrete variables ---------------------------------------

get_bt_discrete_vars <- function(df, .tp = "ts1", vars_to_remove =  "tp", vars_to_remove_missings = "",
                                 .module_hcu_12 = FALSE, hsr_only = FALSE) {
  
  
  # Some preconfigurations
  
  
  # HSR participants of 2012 who particiapted in both surveys
  
   ids_hsr_both_surveys <- filter(df, module_hcu_12 %in% 1) %>% 
     count(id_swisci) %>% 
     filter(n == 2) %>% 
     pull(id_swisci)
  

  # Get survey year
  
  year <- if_else(.tp == "ts1", 2012, 2017)
  
  # Select data from one specific survey
  
  df <- filter(df, tp == .tp)
  
  # Reduce dataset to those participants who participated in the hsr module in 2012
  
  if (.module_hcu_12) {df <- df %>% filter(module_hcu_12 %in% 1)}
  
  # Reduce dataset to those participants who responded to hsr questions in both surveys
  
  if (hsr_only) {df <- df %>% filter(id_swisci %in% ids_hsr_both_surveys)}
  
  
  # Calculate total numbers of categories by variable
  
  raw_table <- df %>% 
    
    select_if(is.factor) %>% 
    
    map(function(category) table(category, useNA = "ifany")) %>% 
    
    map_dfr(~as_tibble(.), .id = "variable") %>% 
    
    mutate(variable = if_else(category %in% .tp, "n_tot", variable),
           category = if_else(category %in% .tp, "", category))
  
  
  
  # Remove defined missings using argument: vars_to_remove_missings
  
  missings_removed <- raw_table %>% filter(!(is.na(category) & variable %in% vars_to_remove_missings))
  
  
  # Calculate relative frequencies
  
  rel_freq_num <- missings_removed %>% 
    
    group_by(variable) %>%
    
    mutate(prop = n / sum(n) * 100) %>% 
    
    ungroup()
  
  
  # Change from number to string, use year in column title
  
  rel_freq_string <- rel_freq_num %>% 
    
    mutate(prop = formatC(prop, digits = 0, format = "f"),
           !!str_c("n_perc_", year) := str_c(n, " ", "(", prop, ")")) %>% 
    
    ungroup()
  
  
  # Format table
  
  rel_freq_string %>% 
    
    replace_na(list(category = "missing")) %>% 
    
    filter(!variable %in% c(vars_to_remove)) %>% 
    
    filter(n != 0) %>% 
    
    select(-n, -prop)
  
}


## Get table one for all participants

table_2012 <- get_bt_discrete_vars(sci, vars_to_remove_missings = c("lesion_level", "etiology"))
table_2017 <- get_bt_discrete_vars(sci, .tp = "ts2", vars_to_remove_missings = c("lesion_level", "etiology"))


# Merge tables of different years to one final table depicting the relative frequencies of discrete variables

table_discrete_vars <- full_join(table_2012, table_2017, by = c("variable", "category")) %>% 
  mutate(variable = replace(variable, duplicated(variable), ""))

write.csv2(table_discrete_vars, file.path("output", "tab_cat_vars_all.csv"), row.names = FALSE)


## Get table one for those only who participated in both surveys

table_2012 <- get_bt_discrete_vars(sci, vars_to_remove_missings = c("lesion_level", "etiology"),  .module_hcu_12 = T)
table_2017 <- get_bt_discrete_vars(sci, .tp = "ts2", vars_to_remove_missings = c("lesion_level", "etiology"),  .module_hcu_12 = T)

table_discrete_vars_hsr <- full_join(table_2012, table_2017, by = c("variable", "category")) %>% 
  mutate(variable = replace(variable, duplicated(variable), ""))

rm(table_2012, table_2017)

write.csv2(table_discrete_vars_hsr, file.path("output", "tab_cat_vars_hsr.csv"), row.names = FALSE)



## Get table one for those only who participated in both surveys

table_2012 <- get_bt_discrete_vars(sci, vars_to_remove_missings = c("lesion_level", "etiology"),  hsr_only = T)
table_2017 <- get_bt_discrete_vars(sci, .tp = "ts2", vars_to_remove_missings = c("lesion_level", "etiology"), hsr_only = T)

table_discrete_vars_hsr_only <- full_join(table_2012, table_2017, by = c("variable", "category")) %>% 
  mutate(variable = replace(variable, duplicated(variable), ""))

write.csv2(table_discrete_vars_hsr_only, file.path("output", "tab_cat_vars_hsr_only.csv"), row.names = FALSE)



# Summarize continuous variables ------------------------------------------------------------------------------------

get_bt_continous_vars <- function(df, n_digits = 0, .tp = "ts1", my_vars = "age", 
                                  get_iqr = TRUE, get_prop = FALSE, .module_hcu_12 = FALSE,
                                  hsr_only = FALSE) {
  
  if(sum(get_iqr, get_prop) != 1) stop("Either get_iqr or get_prop has to be true; both cannot")
  
  if(FALSE) {
    df <- sci
    .tp = "ts1"
    n_digits = 0
    # my_vars = c("hc_inpatient", "hc_ambulant")
    my_vars = c("age", "time_since_sci")
    my_vars = c("hc_ambulant_planned", "hc_ambulant_unplanned")
    get_prop = TRUE
    .module_hcu_12 = FALSE
    hsr_only = FALSE
  }
  
  
  # HSR participants of 2012 who particiapted in both surveys
  
  ids_hsr_both_surveys <- filter(df, module_hcu_12 %in% 1) %>% 
    count(id_swisci) %>% 
    filter(n == 2) %>% 
    pull(id_swisci)
  
  year <- if_else(.tp == "ts1", 2012, 2017)
  df <- filter(df, tp == .tp)
  
  
  if (.module_hcu_12) {df <- df %>% filter(module_hcu_12 %in% 1)}
  
  if (hsr_only) {df <- df %>% filter(id_swisci %in% ids_hsr_both_surveys)}
  
  
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
  
  if(FALSE) {
    df <- sci
    .tp = "ts1"
    n_digits = 0
    my_vars = c("hc_inpatient", "hc_ambulant")
    .module_hcu_12 = TRUE
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


## All participants

cont_vars_2012 <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci"))
cont_vars_2017 <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci"), .tp = "ts2")

table_continous_vars <- full_join(cont_vars_2012, cont_vars_2017, by = c("variable"))

write.csv2(table_continous_vars, file.path("output", "tab_num_vars_all.csv"), row.names = FALSE)


## Participants of hsr survey in 2012

cont_vars_2012 <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci"),  .module_hcu_12 = TRUE)
cont_vars_2017 <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci"), .tp = "ts2", .module_hcu_12 = TRUE)

table_continous_vars_hsr <- full_join(cont_vars_2012, cont_vars_2017, by = c("variable"))

write.csv2(table_continous_vars_hsr, file.path("output", "tab_num_vars_hsr.csv"), row.names = FALSE)

rm(cont_vars_2012, cont_vars_2017)


## Participants of hsr questions in both surveys

cont_vars_2012 <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci"),  hsr_only = TRUE)
cont_vars_2017 <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci"), .tp = "ts2", hsr_only = TRUE)

table_continous_vars_hsr_only <- full_join(cont_vars_2012, cont_vars_2017, by = c("variable"))

write.csv2(table_continous_vars_hsr_only, file.path("output", "tab_num_vars_hsr_only.csv"), row.names = FALSE)

rm("cont_vars_2012", "cont_vars_2017", "table_2012", "table_2017", 
   "table_continous_vars", "table_continous_vars_hsr", "table_continous_vars_hsr_only", 
   "table_discrete_vars", "table_discrete_vars_hsr", "table_discrete_vars_hsr_only")


# Reorder healthcare utilization variables

hc_vars <- select(sci, starts_with("hc_")) %>% names() %>% {
  
  c(str_subset(., "hc_practitioner"),
    str_subset(., "hc_ambulant"), 
    str_subset(., "hc_inpatient"), 
    str_subset(., "hc_paraplegic"),
    str_subset(., "hc_paracenter"))
  
}


# Select categorical healthcare utilizaiton variables

hc_vars_cat <- str_subset(hc_vars, "_num|_days", negate = TRUE)

sci <- sci %>% mutate_at(vars(hc_vars_cat), ~if_else(is.na(.), 0L, .))

hc_2012 <- get_bt_continous_vars(sci, my_vars = hc_vars_cat, get_iqr = FALSE, get_prop = TRUE, .module_hcu_12 = TRUE)
hc_2017 <- get_bt_continous_vars(sci, my_vars = hc_vars_cat, get_iqr = FALSE, get_prop = TRUE, .tp = "ts2")

table_hc_vars <- full_join(hc_2012, hc_2017, by = c("variable")) %>% 
  mutate(n_rel_freq_2012 = if_else(str_detect(variable, "paracenter"), "", n_rel_freq_2012))

write.csv2(table_hc_vars, file.path("output", "tab_hc_vars_cat.csv"), row.names = FALSE)


# Only persons who took part in both surveys

hc_vars_cat <- str_subset(hc_vars, "_num|_days", negate = TRUE)

sci <- sci %>% mutate_at(vars(hc_vars_cat), ~if_else(is.na(.), 0L, .))

hc_2012 <- get_bt_continous_vars(sci, my_vars = hc_vars_cat, get_iqr = FALSE, get_prop = TRUE, 
                                 hsr_only = TRUE)
hc_2017 <- get_bt_continous_vars(sci, my_vars = hc_vars_cat, get_iqr = FALSE, get_prop = TRUE, 
                                 .tp = "ts2", hsr_only = TRUE)

table_hc_vars_hc_hsr_only <- full_join(hc_2012, hc_2017, by = c("variable")) %>% 
  mutate(n_rel_freq_2012 = if_else(str_detect(variable, "paracenter"), "", n_rel_freq_2012))

write.csv2(table_hc_vars_hc_hsr_only, file.path("output", "table_hc_vars_hc_hsr_only.csv"), row.names = FALSE)



# Select numeric healthcare utilizaiton variables

hc_vars_num <- str_subset(hc_vars, "_num")

sci <- sci %>% mutate_at(vars(hc_vars_num), ~if_else(is.na(.), 0L, .))

sci %>% 
  filter(tp == "ts1" & module_hcu_12 == 1) %>% 
  select(hc_vars_num)




## Special calculations, all participants

# Hausarzt

GP <- sci %>% 
  filter(tp == "ts1", hc_practitioner == 1) %>% 
  filter(module_hcu_12 == 1) %>% 
  select(starts_with("hc_practitioner"))

quantile(GP$hc_practitioner_num, c(0.25, 0.5, 0.75), na.rm = TRUE)

GP_2 <- sci %>% 
  filter(tp == "ts2", hc_practitioner == 1) %>% 
  select(starts_with("hc_practitioner"))

quantile(GP_2$hc_practitioner_num, c(0.25, 0.5, 0.75), na.rm = TRUE)


# Ambulant

ambulant <- sci %>% 
  filter(tp == "ts1", hc_ambulant == 1) %>% 
  filter(module_hcu_12 == 1) %>% 
  select(starts_with("hc_ambulant"))

quantile(ambulant$hc_ambulant_planned_num + ambulant$hc_ambulant_unplanned_num , c(0.25, 0.5, 0.75), na.rm = TRUE)

ambulant_2 <- sci %>% 
  filter(tp == "ts2", hc_ambulant == 1) %>% 
  select(starts_with("hc_ambulant"))

quantile(ambulant_2$hc_ambulant_planned_num + ambulant_2$hc_ambulant_unplanned_num , c(0.25, 0.5, 0.75), na.rm = TRUE)


# Stationär

stationaer <- sci %>% 
  filter(tp == "ts1", hc_inpatient == 1) %>% 
  filter(module_hcu_12 == 1) %>% 
  select(starts_with("hc_inpatient"))

quantile(stationaer$hc_inpatient_num, c(0.25, 0.5, 0.75), na.rm = TRUE)
quantile(stationaer$hc_inpatient_days, c(0.25, 0.5, 0.75), na.rm = TRUE)

stationaer_2 <- sci %>% 
  filter(tp == "ts2", hc_inpatient == 1) %>% 
  select(starts_with("hc_inpatient"))

quantile(stationaer_2$hc_inpatient_num, c(0.25, 0.5, 0.75), na.rm = TRUE)
quantile(stationaer_2$hc_inpatient_days, c(0.25, 0.5, 0.75), na.rm = TRUE)


stationaer_d <- sci %>% 
  filter(tp == "ts1", hc_inpatient == 1) %>% 
  filter(module_hcu_12 == 1) %>% 
  select(starts_with("hc_inpatient"))

quantile(stationaer_2$hc_inpatient_num , c(0.25, 0.5, 0.75), na.rm = TRUE)





## Special calculations, participants who participated in both surveys

ids_hsr_both_surveys <- filter(sci, module_hcu_12 %in% 1) %>% 
  count(id_swisci) %>% 
  filter(n == 2) %>% 
  pull(id_swisci)

sci_both <- filter(sci, id_swisci %in% ids_hsr_both_surveys)

# Hausarzt

GP <- sci_both %>% 
  filter(tp == "ts1", hc_practitioner == 1) %>% 
  select(starts_with("hc_practitioner"))

quantile(GP$hc_practitioner_num, c(0.25, 0.5, 0.75), na.rm = TRUE)

GP_2 <- sci_both %>% 
  filter(tp == "ts2", hc_practitioner == 1) %>% 
  select(starts_with("hc_practitioner"))

quantile(GP_2$hc_practitioner_num, c(0.25, 0.5, 0.75), na.rm = TRUE)


# Ambulant

ambulant <- sci_both %>% 
  filter(tp == "ts1", hc_ambulant == 1) %>% 
  select(starts_with("hc_ambulant"))

quantile(ambulant$hc_ambulant_planned_num + ambulant$hc_ambulant_unplanned_num , c(0.25, 0.5, 0.75), na.rm = TRUE)

ambulant_2 <- sci_both %>% 
  filter(tp == "ts2", hc_ambulant == 1) %>% 
  select(starts_with("hc_ambulant"))

quantile(ambulant_2$hc_ambulant_planned_num + ambulant_2$hc_ambulant_unplanned_num , c(0.25, 0.5, 0.75), na.rm = TRUE)


# Stationär

stationaer <- sci_both %>% 
  filter(tp == "ts1", hc_inpatient == 1) %>% 
  select(starts_with("hc_inpatient"))

quantile(stationaer$hc_inpatient_num, c(0.25, 0.5, 0.75), na.rm = TRUE)
quantile(stationaer$hc_inpatient_days, c(0.25, 0.5, 0.75), na.rm = TRUE)

stationaer_2 <- sci_both %>% 
  filter(tp == "ts2", hc_inpatient == 1) %>% 
  select(starts_with("hc_inpatient"))

quantile(stationaer_2$hc_inpatient_num, c(0.25, 0.5, 0.75), na.rm = TRUE)
quantile(stationaer_2$hc_inpatient_days, c(0.25, 0.5, 0.75), na.rm = TRUE)


stationaer_d <- sci %>% 
  filter(tp == "ts1", hc_inpatient == 1) %>% 
  filter(module_hcu_12 == 1) %>% 
  select(starts_with("hc_inpatient"))

quantile(stationaer_2$hc_inpatient_num , c(0.25, 0.5, 0.75), na.rm = TRUE)





tp2_sci_center <- sci %>% 
  filter(hc_paracenter == 1)

vars_paracenter <- str_subset(hc_vars_cat, "hc_paracenter")

paracenter_rf <- get_bt_continous_vars(tp2_sci_center, my_vars = vars_paracenter, get_iqr = FALSE, get_prop = TRUE, 
                                 .tp = "ts2")

write.csv2(paracenter_rf, file.path("output", "paracenter_rf.csv"), row.names = FALSE)




## Persons who had an inpatient stay

inpatient_17 <- sci %>% 
  filter(tp == "ts2") %>% 
  filter(hc_inpatient == 1)

inp_vars <- inpatient_17 %>% names() %>% str_subset("hc_para") %>% str_subset("_inp")
amb_vars <- inpatient_17 %>% names() %>% str_subset("hc_para") %>% str_subset("_ambulant")

inpatient_17 <- inpatient_17 %>% 
  mutate(spec_care_inp = hc_paracenter_Balgrist_inpat +  hc_paracenter_RehaB_inpat +
                         hc_paracenter_CRR_inpat + hc_paracenter_SPZ_inpat +
                         hc_paracenter_Plein_Soleil_inpat + hc_paracenter_Bellinzona_inpat) %>% 
  
  mutate(spec_care_inp = if_else(spec_care_inp >= 1L, 1, 0))
  
                       # inpatient_17 = if_else(inpatient_17 >= 1L, 1, 0))
                       
sum(inpatient_17$spec_care_inp)



ambulant_17 <- sci %>% 
  filter(tp == "ts2") %>% 
  filter(hc_ambulant == 1)

amb_vars <- ambulant_17 %>% names() %>% str_subset("hc_para") %>% str_subset("_ambulant")

ambulant_17 <- ambulant_17 %>% 
  mutate(spec_care_amb = hc_paracenter_Balgrist_ambulant +  hc_paracenter_RehaB_ambulant +
           hc_paracenter_CRR_ambulant + hc_paracenter_SPZ_ambulant +
           hc_paracenter_Plein_Soleil_ambulant + hc_paracenter_Bellinzona_ambulant) %>% 
  
  mutate(spec_care_amb = if_else(spec_care_amb >= 1L, 1, 0))

# inpatient_17 = if_else(inpatient_17 >= 1L, 1, 0))

sum(ambulant_17$spec_care_amb)


