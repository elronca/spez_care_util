
library(tidyverse)
library(readxl)

fread <- data.table::fread



# Read all data to get the variables --------------------------------------

dir("data") %>% str_subset("2019") %>% str_subset("C-006")

swisci_12_vars <- names(fread("data/2019-C-006_2012__2019_10_22.csv", nrows = 1))
swisci_17_vars <- names(fread("data/2019-C-006_2017__2019_10_23.csv", nrows = 1))


# Make codebooks useable to label variables -------------------------------

get_labels_from_codebook <- function(path_cb, my_sheet, my_col_types) {
  
  
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
    
    mutate(cb, suffix = if_else(is.na(suffix), "", suffix),
           var_name = str_c(variable_name, suffix)) %>% 
      
      select(var_name = variable_name, var_level, var_label)
    
    
  } else {
    
    select(cb, var_name = variable_name, var_level, var_label)
    
  }
  
}


# Get labels for codes from starter and basic module of 2012

cb_st_ba_12 <- get_labels_from_codebook(
  path_cb = "data/Pathway2_Codebook_Starter_Basic_2019_04_02.xlsx", 
  my_sheet = "Starter-Basic", 
  my_col_types = c("skip", rep("text", 8), "skip"))


# Get labels for codes from health services research module of 2012

cb_hsr_12 <- get_labels_from_codebook(
  path_cb = "data/Pathway2_Codebook_HSR_2013_09_10.xlsx", 
  my_sheet = "HSR", 
  my_col_types = c("skip", rep("text", 8), "skip"))


cb_12 <- bind_rows(cb_st_ba_12, cb_hsr_12)


# Get labels for codes from 2017 codebook

cb_17 <- get_labels_from_codebook(
  path_cb = "data/Pathway2_2017_Codebook_2019_10_10.xlsx", 
  my_sheet = "Questionnaires", 
  my_col_types = c("skip", rep("text", 11)))


# Clean workspace

rm("cb_hsr_12", "cb_st_ba_12", "get_labels_from_codebook")

saveRDS(cb_12, file.path("workspace", "cb_12.RData"))
saveRDS(cb_17, file.path("workspace", "cb_17.RData"))


# Read and format data ---------------------------------------------------------------

# Function to load and label swisci data

load_swisci_data <- function(path_ds, tp, cat_vars = NULL, num_vars = NULL, other_vars = NULL, codebook) {
  
  
  # Check whether there are variables to label
  
  if(is.null(cat_vars) && is.null(num_vars) && is.null(other_vars)) {
    stop("there are no variables to compute a baseline table with", call. = FALSE)
  }
  
  
  # Read dataset, remove prefix (ts_), and change numeric variables to numeric
  
  my_file <- fread(path_ds, colClasses = 'character', data.table = FALSE, na.strings = c("", "NA")) %>% 
    rename_all(~str_remove(., str_c("^", tp, "_"))) 
  
  
  recode_variables <- function(my_var_name, my_file, codebook) {
    
    
    # Search all_vars variable in codebook 
    
    my_cb <- filter(codebook, var_name %in% my_var_name) %>% 
      drop_na(var_label) %>% 
      pivot_wider(names_from = var_name, values_from = var_level)
    
    # Match coding, to find corresponing label in codebook
    
    var_coding_in_ds <- my_file[[my_var_name]]
    var_coding_in_cb <- my_cb[[my_var_name]]
    
    matched <- match(var_coding_in_ds, var_coding_in_cb)
    
    
    # Obtain labels
    
    map_chr(matched, function(x) my_cb$var_label[x])
    
  }
  
  
  # Loop function over my categorical variables to get those variables labelled
  
  ds_cat_vars <- NULL
  
  if( !is.null(cat_vars) ) {
    
    ds_cat_vars <- map_dfc(cat_vars, recode_variables, my_file = my_file, codebook = codebook) %>% 
      set_names(cat_vars) %>% 
      add_column(id_swisci = my_file$id_swisci, .before = 1) %>% 
      mutate_all(str_to_lower)
    
  }
  
  ds_num_vars <- NULL
  
  if( !is.null(num_vars) ) {
    
    ds_num_vars <- select(my_file, id_swisci, num_vars) %>% 
      mutate_at(num_vars, as.numeric)
    
  }
  
  
  # Merge labeled, modified and other variables
  
  to_merge <- list(cat_vars = ds_cat_vars, num_vars = ds_num_vars, other_vars = select(my_file, id_swisci, other_vars))
  to_merge <- to_merge[map_lgl(to_merge, function(x) ncol(x) > 1 && !is.null(x))]
  
  reduce(to_merge, full_join, by = "id_swisci") %>% 
    mutate(tp = tp)
  
}

# Load and relabel swisci data of 2012 ------------------------------------------------

my_cat_vars <- c("sex", "sci_type", "sci_degree", "sci_cause_type", "ef_finances", 
                 "hcu_inpatient","hcu_ambulant", "hcu_adm_unplanned", "hcu_adm_planned")

my_num_vars <- c("age_quest_admin", "time_since_sci", "hcu_practitioner_check", "hcu_practitioner_acute", 
                 "hcu_inpatient_num", "hcu_inpatient_days", "hcu_adm_unplanned_num", "hcu_adm_planned_num",
                 "hcu_paraplegic_check", "hcu_paraplegic_acute")

my_other_vars <- c("medstat")


# Label selected variables

swisci_12 <- load_swisci_data(path_ds = file.path("data", "2019-C-006_2012__2019_10_22.csv"),
                              tp = "ts1",
                              cat_vars = my_cat_vars,
                              num_vars = my_num_vars,
                              other_vars = my_other_vars,
                              codebook = cb_12)



# Load additional variables

swisci_12_add <- load_swisci_data(path_ds = file.path("data", "2018-C-006_2019_02_27.csv"),
                                  tp = "ts1",
                                  cat_vars = c("ef_short_transport", "ef_long_transport"),
                                  codebook = cb_12)


scim_vars <- read.csv2("data/scim_2012.csv") %>% names() %>% .[-1] %>% str_remove("ts1_")

swisci_12_add2 <- load_swisci_data(path_ds = file.path("data", "scim_2012.csv"),
                                  tp = "ts1",
                                  other_vars = scim_vars,
                                  codebook = cb_12)

scim_rasch <- read.csv2("data/scim_2012_rasch.csv") %>% names() %>% .[-1]

swisci_12_add3 <- load_swisci_data(path_ds = file.path("data", "scim_2012_rasch.csv"),
                                   tp = "ts1",
                                   num_vars = scim_rasch,
                                   codebook = cb_12)


swisci_12 <- swisci_12 %>% 
  left_join(swisci_12_add, by = c("id_swisci", "tp")) %>% 
  left_join(swisci_12_add2, by = c("id_swisci", "tp")) %>% 
  left_join(swisci_12_add3, by = c("id_swisci", "tp"))

rm(cb_12, my_cat_vars, my_num_vars, my_other_vars, swisci_12_add, swisci_12_add2, scim_vars, scim_rasch)




# Load and relabel swisci data of 2017 ------------------------------------------------


my_cat_vars <- c("sex", "sci_type", "sci_degree", "sci_cause_type", "ef_finances", "survey1",
                 "hc_practitioner", "hc_paraplegic", "hc_inpatient","hc_ambulant", 
                 "hc_ambulant_unplanned", "hc_ambulant_planned",
                 "hc_paracenter",
                 "hc_paracenter_1_check", "hc_paracenter_2_check", "hc_paracenter_3_check",
                 "hc_paracenter_4_check", "hc_paracenter_5_check", "hc_paracenter_6_check",
                 "hc_paracenter_1_ambulant", "hc_paracenter_2_ambulant", "hc_paracenter_3_ambulant",
                 "hc_paracenter_4_ambulant", "hc_paracenter_5_ambulant","hc_paracenter_6_ambulant",
                 "hc_paracenter_1_inpat", "hc_paracenter_2_inpat", "hc_paracenter_3_inpat",
                 "hc_paracenter_4_inpat", "hc_paracenter_5_inpat", "hc_paracenter_6_inpat")

my_num_vars <- c("age_quest", "time_since_sci", 
                 "hc_practitioner_num", "hc_paraplegic_num", 
                 "hc_inpatient_num",
                 "hc_inpatient_days", "hc_ambulant_unplanned_num",
                 "hc_ambulant_planned_num")

sec_health_cond <- swisci_17_vars %>% 
  str_subset("_problem_") %>% 
  str_subset("_treat", negate = TRUE) %>% 
  str_subset("_other", negate = TRUE) %>% 
  str_remove("ts2_")


my_other_vars <- c("medstat", sec_health_cond)


swisci_17 <- load_swisci_data(path_ds = file.path("data", "2019-C-006_2017__2019_10_23.csv"),
                              tp = "ts2",
                              cat_vars = my_cat_vars,
                              num_vars = my_num_vars,
                              other_vars = my_other_vars,
                              codebook = cb_17)


swisci_17_add <- load_swisci_data(path_ds = file.path("data", "2018-C-006_Fragebogen2_2019_02_27.csv"),
                                  tp = "ts2",
                                  cat_vars = c("ef_short_transport", "ef_long_transport"),
                                  codebook = cb_17)

scim_vars <- read.csv2("data/scim_2017.csv") %>% names() %>% .[-1] %>% str_remove("ts2_")


swisci_17_add2 <- load_swisci_data(path_ds = file.path("data", "scim_2017.csv"),
                                   tp = "ts2",
                                   other_vars = scim_vars,
                                   codebook = cb_17)

scim_rasch <- read.csv2("data/scim_2017_rasch.csv") %>% names() %>% .[-1]

swisci_17_add3 <- load_swisci_data(path_ds = file.path("data", "scim_2017_rasch.csv"),
                                   tp = "ts2",
                                   num_vars = scim_rasch,
                                   codebook = cb_17)



swisci_17 <- swisci_17 %>% 
  left_join(swisci_17_add, by = c("id_swisci", "tp")) %>% 
  left_join(swisci_17_add2, by = c("id_swisci", "tp")) %>% 
  left_join(swisci_17_add3, by = c("id_swisci", "tp"))



## Recode binary variables

swisci_12 <- swisci_12 %>% 
  mutate_all(as.character) %>% 
  mutate_all(~if_else(. %in% "yes", "1", .)) %>% 
  mutate_all(~if_else(. %in% "no", "0", .))

swisci_17 <- swisci_17 %>% 
  mutate_all(as.character) %>% 
  mutate_all(~if_else(. %in% "yes", "1", .)) %>% 
  mutate_all(~if_else(. %in% "no", "0", .))



# Save datasets and clear workspace

saveRDS(swisci_12, file = file.path("workspace", "swisci_12_raw.RData"))
saveRDS(swisci_17, file = file.path("workspace", "swisci_17_raw.RData"))

rm("cb_17", "fread", "load_swisci_data", "my_cat_vars", "my_num_vars", 
  "my_other_vars", "scim_rasch", "scim_vars", "sec_health_cond", 
  "swisci_12", "swisci_12_add3", "swisci_12_vars", "swisci_17", 
  "swisci_17_add", "swisci_17_add2", "swisci_17_add3", "swisci_17_vars")
