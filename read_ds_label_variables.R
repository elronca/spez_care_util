
library(tidyverse)
library(readxl)



# Make codebooks useable to label variables -------------------------------

get_cb_labels <- function(path_cb, my_sheet, my_col_types) {

  
cb <- read_xlsx(path_cb, sheet = my_sheet, col_types = my_col_types, skip = 1, 
                .name_repair = ~str_to_lower(str_replace_all(.x, " ", "_"))) %>% 
  
  mutate(codes_split =  str_split(codes, "\r\n")) %>% 
  
  unnest(codes_split) %>% 
  
  mutate(levels_labels_split = str_split(codes_split, "=")) %>% 
  
  mutate(var_level = map(levels_labels_split, 1),
         var_label = map(levels_labels_split, 2)) %>%
  
  mutate_at(vars(var_level, var_label), as.character) %>% 
  
  mutate_at(vars(var_level, var_label), ~ if_else(. == "NULL", NA_character_, .)) %>% 
  
  select(-c(codes, codes_split, levels_labels_split))


if( any(names(cb) %in% "suffix") ) {
  
  cb <- mutate(cb, suffix = if_else(is.na(suffix), "", suffix),
         var_name = str_c(variable_name, suffix))
  
} else {cb <- cb}
  
  cb <- select(cb, var_name = variable_name, var_level, var_label)

}


# Get labels for codes from starter and basic module of 2012

cb_st_ba_12 <- get_cb_labels(
  path_cb = "data/Pathway2_Codebook_Starter_Basic_2019_04_02.xlsx", 
  my_sheet = "Starter-Basic", 
  my_col_types = c("skip", rep("text", 8), "skip"))


# Get labels for codes from health services research module of 2012

cb_hsr_12 <- get_cb_labels(
  path_cb = "data/Pathway2_Codebook_HSR_2013_09_10.xlsx", 
  my_sheet = "HSR", 
  my_col_types = c("skip", rep("text", 8), "skip"))


cb_12 <- bind_rows(cb_st_ba_12, cb_hsr_12)


# Get labels for codes from 2017 codebook

cb_17 <- cb_hsr_12 <- get_cb_labels(
  path_cb = "data/Pathway2_2017_Codebook_2019_10_10.xlsx", 
  my_sheet = "Questionnaires", 
  my_col_types = c("skip", rep("text", 11)))


# Clean workspace

rm("cb_hsr_12", "cb_st_ba_12", "get_cb_labels")




# Read and format data ---------------------------------------------------------------

# Function to load and label swisci data

load_swisci_data <- function(path_ds, tp, all_vars, num_vars, cat_vars, codebook) {
  

    # Read dataset, remove prefix (ts_), and change numeric variables to numeric
  
  my_file <- read_csv2(path_ds, col_types = cols(.default = "c")) %>% 
    rename_all(~str_remove(., str_c("^", tp, "_"))) %>% 
    select(all_vars) %>% 
    mutate_at(num_vars, as.numeric)
  
  
  recode_variables <- function(my_var_name, my_file, codebook) {
    
    my_cb <- filter(codebook, var_name %in% my_var_name) %>% 
      pivot_wider(names_from = var_name, values_from = var_level)
    
    var_coding_in_ds <- my_file[[my_var_name]]
    var_coding_in_cb <- my_cb[[my_var_name]]
    
    matched <- match(var_coding_in_ds, var_coding_in_cb)
    
    map_chr(matched, function(x) my_cb$var_label[x])
    
  }
  
  
  ds_cat_vars <- map_dfc(cat_vars, recode_variables, my_file = my_file, codebook = codebook) %>% 
    set_names(cat_vars) %>% 
    add_column(id_swisci = my_file$id_swisci, .before = 1) %>% 
    mutate_all(str_to_lower)
  
  ds_num_vars <- select(my_file, id_swisci, one_of(num_vars))
  
  full_join(ds_cat_vars, ds_num_vars, by = "id_swisci") %>% 
    full_join(my_file[, c("id_swisci", "medstat")], by = "id_swisci") %>% 
    mutate(tp = tp)
  
}

# Load and relabel swisci data of 2012 ------------------------------------------------

all_vars <- read_csv2("data/2019-C-006_2012__2019_10_22.csv", col_types = cols(.default = "c"))

all_vars_names <- all_vars %>% names()

all_vars %>% select("ts1_income_household")

my_vars_all <- c("id_swisci", "sex", "age_quest_admin", "sci_type", "sci_degree", 
                 "time_since_sci", "sci_cause_type", "medstat", "ef_finances",
                 "income_household")

my_num_vars <- c("age_quest_admin", "time_since_sci")

my_cat_vars <- c("sex", "sci_type", "sci_degree", "sci_cause_type", "ef_finances", "income_household")

swisci_12 <- load_swisci_data(path_ds = "data/2019-C-006_2012__2019_10_22.csv",
                            tp = "ts1",
                            all_vars = my_vars_all,
                            num_vars = my_num_vars,
                            cat_vars = my_cat_vars,
                            codebook = cb_12)

rm(cb_12, my_vars_all, my_num_vars, my_cat_vars)


# Load and relabel swisci data of 2017 ------------------------------------------------


# There seem to be problems with the data

if(FALSE) { 
  
  data_17_prob <- read_csv2("data/2019-C-006_2017__2019_10_23.csv", col_types = cols(.default = "c"))
  
  problems(data_17_prob)
  
  warnings()
  
  data_17_prob %>% slice(473) %>% select(ts2_problem_other_1)
  
  select(data_17_prob, id_swisci, ts2_hc_practitioner, ts2_hc_practitioner_1, ts2_hc_practitioner_num) %>% 
    mutate(test_it = if_else(ts2_hc_practitioner == ts2_hc_practitioner_1, 0, 1)) %>% 
    filter(test_it == 1)
  
  select(data_17_prob, id_swisci, id_swisci_1) %>% 
    mutate(test_it = if_else(id_swisci == id_swisci_1, 0, 1)) %>% 
    filter(test_it == 1)
  
  data_17_prob %>% select(starts_with("ts2_hc_practitioner"))
  
}


# Repair and save repaired dataset as csv file

data_17_no_prob <- "data/2019-C-006_2017__2019_10_23.csv" %>% 
  
  read_csv2(col_types = cols(.default = "c")) %>% 
  
  mutate(ts2_hc_practitioner_num = if_else(ts2_hc_practitioner == ts2_hc_practitioner_1, 
                                           ts2_hc_practitioner_num, 
                                           ts2_hc_practitioner_1)) %>% 
  
  select(-ts2_hc_practitioner_1, -id_swisci_1) %>% 
 
  write_excel_csv2("data/2019-C-006_2017__2019_10_23_repaired.csv")


# Load and format repaired dataset

all_vars <- read_csv2("data/2019-C-006_2017__2019_10_23_repaired.csv", col_types = cols(.default = "c")) %>% 
  # names()
  select(ts2_sex)

my_vars_all <- c("id_swisci", "sex", "age_quest", "sci_type", "sci_degree", 
                 "time_since_sci", "sci_cause_type", "medstat", "ef_finances", "income_household")

my_num_vars <- c("age_quest", "time_since_sci")

my_cat_vars <- c("sex", "sci_type", "sci_degree", "sci_cause_type", "ef_finances", "income_household")

swisci_17 <- load_swisci_data(path_ds = "data/2019-C-006_2017__2019_10_23_repaired.csv",
                              tp = "ts2",
                              all_vars = my_vars_all,
                              num_vars = my_num_vars,
                              cat_vars = my_cat_vars,
                              codebook = cb_17)

rm(cb_17, my_vars_all, my_num_vars, my_cat_vars, load_swisci_data, data_17_no_prob)


# Combine data of 2012 and 2017 ----------------------------------------------------------

# Variables that have the same name in both datasets
intersect(names(swisci_12), names(swisci_17))

# Variables that are unique to each dataset
setdiff(names(swisci_12), intersect(names(swisci_12), names(swisci_17)))
setdiff(names(swisci_17), intersect(names(swisci_12), names(swisci_17)))

# Rename identical variables with different names
swisci_12 <- rename(swisci_12, age = age_quest_admin)
swisci_17 <- rename(swisci_17, age = age_quest)

# Reorder variables
sci <- bind_rows(swisci_12, swisci_17) %>% arrange(as.numeric(id_swisci), tp)


# Check factor levels
select_if(sci, is.character) %>% 
  select(-c(id_swisci, medstat)) %>% 
  map(unique)


# Repair identical categories with different names
sci <- mutate(sci, 
              sci_degree = str_remove(sci_degree, " lesion"),
              sci_cause_type = str_remove(sci_cause_type, " sci"))


# Change character variables to factors
to_factor <- c("sex", "sci_type", "sci_degree", "sci_cause_type")

sci <- mutate_at(sci, to_factor, as.factor)


# Save data and clear workspace -------------------------------------------

save(sci, file = file.path("workspace", "raw_data.Rdata"))

rm("swisci_12", "swisci_17", "to_factor", "sci")
