
library(tidyverse)
library(ggfittext)

emms <- readRDS(file.path("workspace", "emm_to_plot.RData"))

to_plot <- emms %>% 
  map_dfr(~filter(., variable %in% c("sex", "age_cat", "severity")), .id = "ut") %>% 
  mutate(variable = as.factor(variable)) %>% 
  filter(ut == "checkup")

dput(to_plot$category)


to_plot <- mutate(to_plot, 
                  category = fct_relevel(category, 
                                         c("75+", "61-75", "46-60", "31-45", "16-30", 
                                           "female", "male", 
                                           "complete tetra", "complete para", "incomplete tetra", "incomplete para")))

ggplot(data = to_plot, aes(x = variable, y = prob, ymin = lower, ymax = upper, group = category)) +
  
  geom_pointrange(position = position_dodge(width = .8)) +
  
  geom_text(aes(y = 0, label = category), position = position_dodge(width= .8)) +
  
  # geom_hline(yintercept = 43, linetype="dotted") +
  
  ylim(0, NA) +
  
  coord_flip()
