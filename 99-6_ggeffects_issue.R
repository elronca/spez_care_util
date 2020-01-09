
library(emmeans)
library(mice)
library(carData)
library(tidyverse)
library(ggeffects)


fit <- glm(volunteer ~ sex + extraversion, data = Cowles, family = binomial)

pmm_cow <- emmeans(fit, "sex")

ggemmeans(fit, "sex")


Cowles_NA <- Cowles

set.seed(42)

Cowles_NA[,-4] <- do.call(cbind.data.frame, 
                          lapply(Cowles_NA[,-4], function(x) {
                            n <- nrow(Cowles_NA)
                            x[sample(c(1:n),floor(n/10))]<-NA
                            x
                          })
)

fit_me <- Cowles_NA %>%
  mice(seed = 123, m = 2, print = FALSE) %>%
  with(glm(volunteer ~ sex + extraversion, family = binomial))


emmeans(fit_me, "sex")

ggemmeans(fit_me, "sex")
