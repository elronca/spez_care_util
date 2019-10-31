
library(tidyverse)
library(readxl)


# Make codebooks useable to label variables -------------------------------


# Codebooks for SwiSCI pathway 2, 2012 data

format_codebook <- function(data) {
  
  data %>% 
    
    mutate(codes_split =  str_split(codes, "\r\n")) %>% 
    
    unnest(codes_split) %>% 
    
    mutate(levels_labels_split = str_split(codes_split, "=")) %>% 
    
    mutate(var_level = map(levels_labels_split, 1),
           var_label = map(levels_labels_split, 2)) %>%
    
    mutate_at(vars(var_level, var_label), as.character) %>% 
    
    mutate_at(vars(var_level, var_label), ~ if_else(. == "NULL", NA_character_, .)) %>% 
    
    select(-c(codes, codes_split, levels_labels_split)) %>% 
    
    mutate(prefix = if_else(is.na(prefix), "", prefix),
           var_name = str_c(prefix, variable_name))
  
}

# starter & basic

cb_st_ba_12_path <- "data/Pathway2_Codebook_Starter_Basic_2019_04_02.xlsx"

cb_st_ba_12_raw <- read_xlsx(cb_st_ba_12_path, 
                               sheet = "Starter-Basic", col_types = c("skip", rep("text", 8), "skip"), 
                               skip = 1, .name_repair = ~str_to_lower(str_replace_all(.x, " ", "_")))


cb_st_ba_12_full <- format_codebook(cb_st_ba_12_raw)

cb_st_ba_12 <- select(cb_st_ba_12_full, var_name, var_level, var_label)


# Health services research

cb_hsr_12_path <- "data/Pathway2_Codebook_HSR_2013_09_10.xlsx"

cb_hsr_12_raw <- read_xlsx(cb_hsr_12_path, 
                               sheet = "HSR", col_types = c("skip", rep("text", 8), "skip"), 
                               skip = 1, .name_repair = ~str_to_lower(str_replace_all(.x, " ", "_")))


cb_hsr_12_full <- format_codebook(cb_hsr_12_raw)
cb_hsr_12 <- select(cb_hsr_12_full, var_name, var_level, var_label)

cb_hsr_12 <- filter(cb_hsr_12, var_name == "id_swisci" )


cb_12 <- bind_rows(cb_st_ba_12, cb_hsr_12)

rm("cb_hsr_12", "cb_hsr_12_full", "cb_hsr_12_path", "cb_hsr_12_raw", 
   "cb_st_ba_12", "cb_st_ba_12_full", "cb_st_ba_12_path", "cb_st_ba_12_raw", 
   "format_codebook")




# Codebook for SwiSCI pathway 2, 2017 data

cb_17_path <- "data/Pathway2_2017_Codebook_2019_10_10.xlsx"

cb_17_path_raw <- read_xlsx(cb_17_path, 
                              sheet = "Questionnaires", col_types = c("skip", rep("text", 11)), 
                              skip = 1, .name_repair = ~str_to_lower(str_replace_all(.x, " ", "_")))


cb_17_full <- cb_17_path_raw %>% 
  
  mutate(codes_split =  str_split(codes, "\r\n")) %>% 
  
  unnest(codes_split) %>% 
  
  mutate(levels_labels_split = str_split(codes_split, "=")) %>% 
  
  mutate(var_level = map(levels_labels_split, 1),
         var_label = map(levels_labels_split, 2)) %>%
  
  mutate_at(vars(var_level, var_label), as.character) %>% 
  mutate_at(vars(var_level, var_label), ~ if_else(. == "NULL", NA_character_, .)) %>% 
  
  select(-c(codes, codes_split, levels_labels_split)) %>% 
  
  mutate(prefix = if_else(is.na(prefix), "", prefix),
         suffix = if_else(is.na(suffix), "", suffix),
         var_name = str_c(prefix, "_", variable_name, suffix))

cb_17 <- select(cb_17_full, var_name, var_level, var_label)

rm("cb_17_full", "cb_17_path", "cb_17_path_raw")


# Read and format 2012 data ---------------------------------------------------------------


# Preparations

path_ds <- "data/2019-C-006_2012__2019_10_22.csv"

my_vars <- c("id_swisci", "ts1_sex", "ts1_age_quest_admin", "ts1_sci_type", "ts1_sci_degree", 
             "ts1_time_since_sci", "ts1_sci_cause_type", "medstat")

numeric_vars <- c("ts1_age_quest_admin", "ts1_time_since_sci")

cat_vars <- c("ts1_sex", "ts1_sci_type", "ts1_sci_degree", "ts1_sci_cause_type")

n_cols_ds <- 108


data_12_raw <- read_csv2(path_ds, col_types = str_c(rep("c", n_cols_ds), collapse = ""))

data_12 <- select(data_12_raw, my_vars) %>% mutate_at(vars(numeric_vars), as.numeric)


# Recode cateogrical variables

recode_variables <- function(my_var_name, data, codebook) {
  
  my_cb <- filter(codebook, var_name == my_var_name) %>% 
    pivot_wider(names_from = var_name, values_from = var_level)
  
  matched <- match(data[[my_var_name]], my_cb[[my_var_name]])
  
  map_chr(matched, function(x) my_cb$var_label[x])
  
  
}

df_cat_vars <- map_dfc(cat_vars, recode_variables, data = data_12, codebook = cb_12) %>% 
  set_names(cat_vars) %>% 
  add_column(id_swisci = data_12$id_swisci, .before = 1)

df_cat_vars <- mutate_all(df_cat_vars, str_to_lower)


# Get numeric variables

df_num_vars <- data_12 %>% 
  select(id_swisci, numeric_vars)

df_12 <- full_join(df_cat_vars, df_num_vars, by = "id_swisci")

rm("cat_vars", "cb_12", "data_12", "data_12_raw", "df_cat_vars", "df_num_vars", 
   "my_vars", "n_cols_ds", "numeric_vars", "path_ds")




# Read and format 2012 data ---------------------------------------------------------------


# Preparations

path_ds <- "data/2019-C-006_2017__2019_10_23.csv"
n_cols_ds <- 213

data_17_raw <- read_csv2(path_ds, col_types = str_c(rep("c", n_cols_ds), collapse = ""))

select(data_17_raw, ts2_hc_practitioner, ts2_hc_practitioner_1) %>% 
  mutate(test_it = if_else(ts2_hc_practitioner == ts2_hc_practitioner_1, 0, 1)) %>% 
  filter(test_it == 1)

names(data_17_raw)


my_vars <- c("id_swisci", "ts2_sex", "ts2_age_quest", "ts2_sci_type", "ts2_sci_degree", 
             "ts2_time_since_sci", "ts2_sci_cause_type", "medstat")

numeric_vars <- c("ts2_age_quest", "ts2_time_since_sci")

cat_vars <- c("ts2_sex", "ts2_sci_type", "ts2_sci_degree", "ts2_sci_cause_type")


data_17 <- select(data_17_raw, my_vars) %>% 
  mutate_at(vars(numeric_vars), as.numeric)


# Recode cateogrical variables

filter(cb_17, str_detect(var_name, "sci_type"))




df_cat_vars <- map_dfc(cat_vars, recode_variables, data = data_17, codebook = cb_17) %>% 
  set_names(cat_vars) %>% 
  add_column(id_swisci = data_17$id_swisci, .before = 1)

df_cat_vars <- mutate_all(df_cat_vars, str_to_lower)


# Get numeric variables

df_num_vars <- data_17 %>% 
  select(id_swisci, numeric_vars)

df_17 <- full_join(df_cat_vars, df_num_vars, by = "id_swisci")
