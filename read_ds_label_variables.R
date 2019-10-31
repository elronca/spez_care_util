
library(tidyverse)
library(readxl)


# Make codebooks useable to label variables -------------------------------


# Codebooks for SwiSCI pathway 2, 2012 data

format_codebook <- function(data) {
  
  cb <- data %>% 
    
    mutate(codes_split =  str_split(codes, "\r\n")) %>% 
    
    unnest(codes_split) %>% 
    
    mutate(levels_labels_split = str_split(codes_split, "=")) %>% 
    
    mutate(var_level = map(levels_labels_split, 1),
           var_label = map(levels_labels_split, 2)) %>%
    
    mutate_at(vars(var_level, var_label), as.character) %>% 
    
    mutate_at(vars(var_level, var_label), ~ if_else(. == "NULL", NA_character_, .)) %>% 
    
    select(-c(codes, codes_split, levels_labels_split))
  
  if( any(names(data) %in% "suffix") ) {
    
    mutate(cb, suffix = if_else(is.na(suffix), "", suffix),
           var_name = str_c(variable_name, suffix))
    
  } else {cb}
  
}
  

# starter & basic

cb_st_ba_12_path <- "data/Pathway2_Codebook_Starter_Basic_2019_04_02.xlsx"

cb_st_ba_12_raw <- read_xlsx(cb_st_ba_12_path, 
                               sheet = "Starter-Basic", col_types = c("skip", rep("text", 8), "skip"), 
                               skip = 1, .name_repair = ~str_to_lower(str_replace_all(.x, " ", "_")))


cb_st_ba_12_full <- format_codebook(cb_st_ba_12_raw)

cb_st_ba_12 <- select(cb_st_ba_12_full, var_name = variable_name, var_level, var_label)


# Health services research

cb_hsr_12_path <- "data/Pathway2_Codebook_HSR_2013_09_10.xlsx"

cb_hsr_12_raw <- read_xlsx(cb_hsr_12_path, 
                               sheet = "HSR", col_types = c("skip", rep("text", 8), "skip"), 
                               skip = 1, .name_repair = ~str_to_lower(str_replace_all(.x, " ", "_")))


cb_hsr_12_full <- format_codebook(cb_hsr_12_raw)
cb_hsr_12 <- select(cb_hsr_12_full, var_name = variable_name, var_level, var_label)

cb_hsr_12 <- filter(cb_hsr_12, var_name == "id_swisci")


cb_12 <- bind_rows(cb_st_ba_12, cb_hsr_12)




# Codebook for SwiSCI pathway 2, 2017 data

cb_17_path <- "data/Pathway2_2017_Codebook_2019_10_10.xlsx"

cb_17_path_raw <- read_xlsx(cb_17_path, 
                              sheet = "Questionnaires", col_types = c("skip", rep("text", 11)), 
                              skip = 1, .name_repair = ~str_to_lower(str_replace_all(.x, " ", "_")))

cb_17_full <- format_codebook(cb_17_path_raw)


cb_17 <- select(cb_17_full, var_name, var_level, var_label)


# Clean workspace

rm("cb_17_full", "cb_17_path", "cb_17_path_raw", 
  "cb_hsr_12", "cb_hsr_12_full", "cb_hsr_12_path", "cb_hsr_12_raw", 
  "cb_st_ba_12", "cb_st_ba_12_full", "cb_st_ba_12_path", "cb_st_ba_12_raw", 
  "format_codebook")




# Read and format data ---------------------------------------------------------------


# Function takes variable and codebook and labels the levels of categorical variables

recode_variables <- function(my_var_name, data, codebook) {
  
  my_cb <- filter(codebook, var_name == my_var_name) %>% 
    pivot_wider(names_from = var_name, values_from = var_level)
  
  matched <- match(data[[my_var_name]], my_cb[[my_var_name]])
  
  map_chr(matched, function(x) my_cb$var_label[x])
  
  
}

# Format data of SwiSCI pathway 2, survey 2012


# Preparations

path_ds <- "data/2019-C-006_2012__2019_10_22.csv"

my_vars_all <- c("id_swisci", "sex", "age_quest_admin", "sci_type", "sci_degree", 
             "time_since_sci", "sci_cause_type", "medstat")

my_num_vars <- c("age_quest_admin", "time_since_sci")

my_cat_vars <- c("sex", "sci_type", "sci_degree", "sci_cause_type")

n_cols_ds <- 108


# Load whole dataset

data_12_raw <- read_csv2(path_ds, col_types = str_c(rep("c", n_cols_ds), collapse = ""))

data_12 <- data_12_raw %>% 
  rename_all(~str_remove(., "^ts1_")) %>% 
  select(my_vars_all) %>% 
  mutate_at(vars(my_num_vars), as.numeric)


# Recode and get cateogrical variables

df_cat_vars <- map_dfc(my_cat_vars, recode_variables, data = data_12, codebook = cb_12) %>% 
  set_names(my_cat_vars) %>% 
  add_column(id_swisci = data_12$id_swisci, .before = 1)

df_cat_vars <- mutate_all(df_cat_vars, str_to_lower)


# Get numeric variables

df_num_vars <- select(data_12, id_swisci, my_num_vars)

df_12 <- full_join(df_cat_vars, df_num_vars, by = "id_swisci") %>% 
  full_join(data_12[, c("id_swisci", "medstat")], by = "id_swisci") %>% 
  mutate(tp = "ts1")

rm("cb_12", "data_12", "data_12_raw", "df_cat_vars", "df_num_vars", "my_cat_vars", 
   "my_num_vars", "my_vars_all", "n_cols_ds", "path_ds")


# Read and format 2012 data ---------------------------------------------------------------


# Preparations

path_ds <- "data/2019-C-006_2017__2019_10_23.csv"

my_vars_all <- c("id_swisci", "sex", "age_quest", "sci_type", "sci_degree", 
                 "time_since_sci", "sci_cause_type", "medstat")

my_num_vars <- c("age_quest", "time_since_sci")

my_cat_vars <- c("sex", "sci_type", "sci_degree", "sci_cause_type")

n_cols_ds <- 213

data_17_raw <- read_csv2(path_ds, col_types = str_c(rep("c", n_cols_ds), collapse = "")) %>% 
  rename_all(~str_remove(., "^ts2_")) %>% 
  select(my_vars_all) %>% 
  mutate_at(vars(my_num_vars), as.numeric)

select(data_17_raw, ts2_hc_practitioner, ts2_hc_practitioner_1) %>% 
  mutate(test_it = if_else(ts2_hc_practitioner == ts2_hc_practitioner_1, 0, 1)) %>% 
  filter(test_it == 1)

data_17 <- select(data_17_raw, my_vars_all) %>% 
  mutate_at(vars(my_num_vars), as.numeric)


# Recode cateogrical variables

df_cat_vars <- map_dfc(my_cat_vars, recode_variables, data = data_17, codebook = cb_17) %>% 
  set_names(my_cat_vars) %>% 
  add_column(id_swisci = data_17$id_swisci, .before = 1)

df_cat_vars <- mutate_all(df_cat_vars, str_to_lower)


# Get numeric variables

df_num_vars <- data_17 %>% select(id_swisci, my_num_vars)


# Join data and add additional variables

df_17 <- full_join(df_cat_vars, df_num_vars, by = "id_swisci") %>% 
  full_join(data_17[, c("id_swisci", "medstat")], by = "id_swisci") %>% 
  mutate(tp = "ts2")

# Clear workspace

rm("cb_17", "data_17", "data_17_raw", "df_cat_vars", "df_num_vars", "my_cat_vars", 
   "my_num_vars", "my_vars_all", "n_cols_ds", "path_ds", "recode_variables")



# Combine data of 2012 and 2017 ----------------------------------------------------------

intersect(names(df_12), names(df_17))
setdiff(names(df_12), names(df_17))

names(df_12)
names(df_17)

df_12 <- rename(df_12, age = age_quest_admin)
df_17 <- rename(df_17, age = age_quest)

sci <- bind_rows(df_12, df_17) %>% 
  
  arrange(as.numeric(id_swisci), tp) %>% 
  
  mutate(sci_degree = str_remove(sci_degree, " lesion"),
         sci_cause_type = str_remove(sci_cause_type, " sci"))


# Save data and clear workspace -------------------------------------------

save(sci, file = file.path("workspace", "raw_data.Rdata"))

rm("df_12", "df_17", "sci")
