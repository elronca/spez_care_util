
library(tidyverse)

emms <- readRDS(file.path("workspace", "emm_to_plot.RData"))
var_order <- readRDS(file.path("workspace", "vars_for_table_1.R"))


# Bind lists with differnt utilization types

to_plot_raw <- bind_rows(readRDS(file.path("workspace", "emm_to_plot.RData")), .id = "ut")
print(to_plot_raw, n = 70)


# Reformat p-values

to_plot <- mutate(to_plot_raw,
                  p_val_llrt = str_sub(p_val_llrt, 2),
                  p_val_llrt = str_c("P=", p_val_llrt),
                  p_val_llrt = if_else(p_val_llrt == "P=", "", p_val_llrt),
                  p_val_llrt = if_else(p_val_llrt == "P=.000", "P<.001", p_val_llrt)
)

print(to_plot, n = 5)


# Change category labels

to_plot <- mutate(to_plot,
                  category = case_when(
                    str_detect(variable, "problem_cancer") & category == "0" ~ "Absent",
                    str_detect(variable, "problem_cancer") & category == "1" ~ "Present",
                    str_detect(variable, "problem_") & category == "0" ~ "Absent",
                    str_detect(variable, "problem_") & category == "1" ~ "Significant",
                    TRUE ~ category))


# Change order of utilization type

new_ut_labels <- c(
  
  "Utilization of specialized\noutpatient clinic care" = 'outp', 
  "Utilization of specialized\ninpatient care" = 'inp',
  "Attendance in\nannual checkup" = 'checkup')

to_plot <- mutate(to_plot, 
                  ut = factor(ut, levels = c('outp','inp','checkup')),
                  ut = fct_recode(ut, !!!new_ut_labels))


# Change order of variables and rename variables

unique(to_plot$variable)

variable_order <- c("all", 
                    "sex", 
                    "age_cat", 
                    "severity", 
                    "etiology",  
                    # "time_since_sci_years_cat", 
                    
                    "problem_cancer",
                    "problem_spasticity", 
                    "problem_injury",
                    "problem_sexual", 
                    "problem_ossification", 

                    
                    # "problem_heart",
                    "language", 
                    "dist_amb_check_up_cat", 
                    "hc_ambulant_num_cat", 
                    "hc_inpatient_num_cat",
                    "hc_inpatient_days_cat")

new_var_labels <- c("All" = "all",
                    "Sex" = "sex",
                    "Age" = "age_cat",
                    "Lesion\nseverity" = "severity",
                    "Etiology" = "etiology",
                    # "Time since\nonset of SCI" = "time_since_sci_years_cat",
                    "Spasticity" = "problem_spasticity", 
                    "Injury caused by\nloss of sensation" = "problem_injury",
                    "Cancer" = "problem_cancer",
                    "Sexual\ndysfunction" = "problem_sexual", 
                   
                    # "Severe injury\n due to loss of senssation " = "problem_ossification", 
                    "Ossification" = "problem_ossification", 

                    
                    # "Coronary heart disease" = "problem_heart",
                    "Language\nregion" = "language", 
                    "Driving time\nto specialized\ntreatment facility" = "dist_amb_check_up_cat", 
                    "Number of\noutpatient clinic\nvisits" = "hc_ambulant_num_cat", 
                    "Number of\ninpatient\nvisits" = "hc_inpatient_num_cat",
                    "Length of\nhospital stay" = "hc_inpatient_days_cat")

to_plot <- mutate(to_plot, 
                  variable = factor(variable, levels = variable_order),
                  variable = fct_recode(variable, !!!new_var_labels))


# Change order of categories and rename categories

category_order <- c(
  "", 
  "75+", "61-75", "46-60", "31-45", "16-30", 
  "nontraumatic", "traumatic", 
  "Italian", "French", "German", 
  "5+", "2-4", "1", 
  "Present",
  "Significant", "Absent", 
  "complete tetra", "complete para", "incomplete tetra", "incomplete para", 
  "female", "male", 
  "15+", "10-14", "5-9", "<=4",
  "90+ min", "60-90 min", "31-60 min", "0-30 min", 
  "21+", "6-20", "1-5"
  
)

new_cat_labels <- c(
  ">75 years" = "75+",
  "61\u201375 years" = "61-75",
  "46\u201360 years" = "46-60",
  "31\u201345 years" = "31-45",
  "16\u201330 years" = "16-30",
  
  "Nontraumatic" = "nontraumatic",
  "Traumatic" = "traumatic",
  
  "5+ visits" = "5+", 
  "2\u20134 visits" = "2-4",
  "1   visit" = "1",
  
  "Complete tetraplegia" = "complete tetra", 
  "Complete paraplegia" = "complete para", 
  "Incomplete tetraplegia" = "incomplete tetra", 
  "Incomplete paraplegia" = "incomplete para", 
  
  "Female" = "female", "Male" = "male",
  
  # "15+ years" = "15+", 
  # "10\u201314 years" = "10-14", 
  # "5\u20139 years" = "5-9", 
  # '0\u20134 years' = "<=4",
  
  ">90 min" = "90+ min", 
  "61\u201390 min" = "60-90 min", 
  "31\u201360 min" = "31-60 min", 
  "0\u201330 min" = "0-30 min", 
  
  "21+ days" = "21+", 
  "6\u201320 days" = "6-20", 
  "1\u20135 days" = "1-5")

to_plot <- mutate(to_plot,
                  category = factor(category, levels = category_order),
                  category = fct_recode(category, !!!new_cat_labels))


# Create a group of significant different categories

to_plot <- mutate(to_plot,
                  lr_sig_05 = if_else(p_value_lr < 0.05, "yes", "no"),
                  lr_sig_05 = as.factor(lr_sig_05))


# Prepare dataframe with information to plot text (p values)

dat_text <- to_plot %>% 
  select(ut, variable, p_val_llrt) %>% 
  group_by(ut, variable) %>% 
  distinct(.keep_all = TRUE) %>% 
  rename(label = p_val_llrt) %>% 
  mutate(x = Inf, hjust = 1.2, y = Inf, vjust = 2)


# Plot

p <- ggplot(to_plot, aes(y = category, x = prob, 
                         xmin = lower, xmax = upper,
                         color = lr_sig_05)) +
  
  geom_point() + geom_errorbarh(height = 0)

p <- p + facet_grid(variable ~ ut, scales = "free", space = "free")

p <- p + geom_text(data = dat_text,  mapping = aes(
  x = x, hjust = hjust, 
  y = y, vjust = vjust, 
  label = label), 
  size = 2.5,
  inherit.aes = FALSE)

p <- p + scale_color_manual(values = c("grey50", "black")) +
  scale_x_continuous(breaks = c(0, 40, 80)) +
  expand_limits(x = c(0, 80))

p <- p + 
  xlab("Estimated relative frequencies of specialized care utilization in the last twelve month (%)") + 
  ylab(NULL)+
  theme_bw() +
  theme(strip.text.y = element_text(angle = 0), legend.position = "none",
        axis.text.y = element_text(size = 8),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank())

p

A4 <- c(width = 210, height = 297)
A4["width"] <- A4["width"] * 0.9
A4["height"] <- A4["height"] * 0.7

ggsave(filename = file.path("output", "regression_plot.pdf"),
       device = "pdf", width = A4["width"], height = A4["height"], units = "mm")

