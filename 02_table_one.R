

library(tidyverse)
Sys.setenv(LANGUAGE = 'en')

load(file.path("workspace", "manually_imputed.Rdata"))

sci %>% 
  filter(tp == "ts1") %>% 
  filter(module_hcu_12 == 1) %>% 
  nrow()


# Calculate relative frequencies of categories across discrete variables ---------------------------------------

get_bt_discrete_vars <- function(df, my_tp = "ts1", vars_to_remove =  "tp", vars_to_remove_missings = "") {
  
  
  # Define survey year
  
  year <- if_else(my_tp == "ts1", 2012, 2017)
  
  
  # Calculate total numbers of categories by variable
  
  raw_table <- df %>% 
    
    filter(tp == my_tp) %>% 
    
    select_if(is.factor) %>% 
    
    map(function(category) table(category, useNA = "ifany")) %>% 
    
    map_dfr(~as_tibble(.), .id = "variable") %>% 
    
    mutate(variable = if_else(category %in% my_tp, "n_tot", variable),
           category = if_else(category %in% my_tp, "", category))
  
  
  
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

table_2012 <- get_bt_discrete_vars(sci, vars_to_remove_missings = c("lesion_level", "etiology"))
table_2017 <- get_bt_discrete_vars(sci, my_tp = "ts2", vars_to_remove_missings = c("lesion_level", "etiology"))


# Merge tables of different years to one final table depicting the relative frequencies of discrete variables

table_discrete_vars <- full_join(table_2012, table_2017, by = c("variable", "category")) %>% 
  mutate(variable = replace(variable, duplicated(variable), ""))

rm(table_2012, table_2017)



# Summarize continuous variables ------------------------------------------------------------------------------------

get_bt_continous_vars <- function(df, n_digits = 0, my_tp = "ts1", my_vars = "age", 
                                  get_iqr = TRUE, get_prop = FALSE, .module_hcu_12 = FALSE) {
  
  if(sum(get_iqr, get_prop) != 1) stop("Either get_iqr or get_prop has to be true; both cannot")
  
    if(FALSE) {
    df <- sci
    my_tp = "ts1"
    n_digits = 0
    my_vars = c("hc_inpatient", "hc_ambulant")
    .module_hcu_12 = TRUE
    }

  year <- if_else(my_tp == "ts1", 2012, 2017)
  df <- filter(df, tp == my_tp)
  
  if (.module_hcu_12) {df <- df %>% filter(module_hcu_12 %in% 1)}
  
  
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
    my_tp = "ts1"
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

cont_vars_2012 <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci"))
cont_vars_2017 <- get_bt_continous_vars(sci, my_vars = c("age", "time_since_sci"), my_tp = "ts2")

table_continous_vars <- full_join(cont_vars_2012, cont_vars_2017, by = c("variable"))

rm(cont_vars_2012, cont_vars_2017)

table_discrete_vars
table_continous_vars


# Reorder healthcare utilization variables

hc_vars <- select(sci, starts_with("hc_")) %>% names() %>% {
  
  c(str_subset(., "hc_practitioner"),
    str_subset(., "hc_ambulant"), 
    str_subset(., "hc_inpatient"), 
    str_subset(., "hc_paraplegic"),
    str_subset(., "hc_paracenter"))
  
}


# Select categorical healthcare utilizaiton variables

hc_vars_cat <- str_subset(hc_vars, "_num", negate = TRUE)

hc_2012 <- get_bt_continous_vars(sci, my_vars = hc_vars_cat, get_iqr = FALSE, get_prop = TRUE, .module_hcu_12 = TRUE)
hc_2017 <- get_bt_continous_vars(sci, my_vars = hc_vars_cat, get_iqr = FALSE, get_prop = TRUE, my_tp = "ts2")

table_hc_vars <- full_join(hc_2012, hc_2017, by = c("variable")) %>% 
  mutate(n_rel_freq_2012 = if_else(str_detect(variable, "paracenter"), "", n_rel_freq_2012))

