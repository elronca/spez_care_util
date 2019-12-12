
## Multiple imputation

library(mice)
library(tidyverse)
library(naniar)

load(file.path("workspace", "manually_imputed.Rdata"))
load(file.path("workspace", "spatial_vars.RData"))

miss_var_summary(sci) %>% print(n = 100)


