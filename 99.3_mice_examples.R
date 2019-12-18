fit <- lm(Ozone ~ Wind, data = airquality, na.action = na.omit)
summary(fit)

# fit <- lm(Ozone ~ Wind, data = airquality, na.action =  na.fail)

airquality2 <- cbind(airquality, predict(fit))

head(na.action(fit))

airquality2 <- cbind(na.omit(airquality[, c("Ozone", "Wind")]),
                     predicted = predict(fit))

data <- airquality[, c("Ozone", "Solar.R", "Wind")]
mu <- colMeans(data, na.rm = TRUE)
cv <- cov(data, use = "pairwise")

library(lavaan)
fit <- lavaan("Ozone ~ 1 + Wind + Solar.R
              Ozone ~~ Ozone",
              sample.mean = mu, sample.cov = cv,
              sample.nobs = sum(complete.cases(data)))

summary(fit)

library(mice)
imp <- mice(airquality, method = "mean", m = 1, maxit = 1)

fit <- lm(Ozone ~ Solar.R, data = airquality)
pred <- predict(fit, newdata = ic(airquality))


data <- airquality[, c("Ozone", "Solar.R")]
imp <- mice(data, method = "norm.predict", seed = 1,
            m = 1, print = FALSE)
xyplot(imp, Ozone ~ Solar.R)


data <- airquality[, c("Ozone", "Solar.R")]
imp <- mice(data, method = "norm.nob", m = 1, maxit = 1,
            seed = 1, print = FALSE)

xyplot(imp, Ozone ~ Solar.R)



imp <- mice(airquality, method = "mean", m = 1,
            maxit = 1, print = FALSE)
airquality2 <- cbind(complete(imp),
                     r.Ozone = is.na(airquality[, "Ozone"]))
fit <- lm(Wind ~ Ozone + r.Ozone, data = airquality2)

summary(fit)
naprint(na.action(fit))


imp <- mice(airquality, seed = 1, m = 20, print = FALSE)
fit <- with(imp, lm(Ozone ~ Wind + Temp + Solar.R))
summary(pool(fit))
naprint(na.action(fit))

fit <- lm(Ozone ~ Wind + Temp + Solar.R, data = airquality)
coef(summary(fit))

mvrnorm <- MASS::mvrnorm

set.seed(1)
n <- 10000
sigma <- matrix(c(1, 0.6, 0.6, 1), nrow = 2)
cmp <- MASS::mvrnorm(n = n, mu = c(5, 5), Sigma = sigma)
p2.marright <- 1 - plogis(-5 + cmp[, 1])
r2.marright <- rbinom(n, 1, p2.marright)
yobs <- cmp
yobs[r2.marright == 0, 2] <- NA


amp <- ampute(cmp, type = "RIGHT")
apply(amp$amp, 2, mean, na.rm = TRUE)

colMeans(cmp)

round(colMeans(amp$amp, na.rm = T), 2)




library("ImputeRobust")
library("gamlss")
data(db)
data <- subset(db, age > 1 & age < 2, c("age", "head"))
names(data) <- c("age", "hc")
synthetic <- rep(c(FALSE, TRUE), each = nrow(data))
data2 <- rbind(data, data)
row.names(data2) <- 1:nrow(data2)
data2[synthetic, "hc"] <- NA
imp <- mice(data2, m = 1, meth = "gamlssTF", seed = 88009,
            print = FALSE)
syn <- subset(mice::complete(imp), synthetic)




library(mice)

as_tibble(nhanes)

imp <- mice(nhanes, seed = 123, print = FALSE)
fit <- with(imp, lm(chl ~ age + bmi + hyp))
est1 <- pool(fit)

summary(est1)

est2 <- nhanes %>%
  mice(seed = 123, print = FALSE) %>%
  with(lm(chl ~ age + bmi + hyp)) %>%
  pool()

summary(est2)

est3 <- nhanes %>%
  mice(seed = 123, print = FALSE) %>%
  mice::complete("all") %>%
  lapply(lm, formula = chl ~ age + bmi + hyp) %>%
  pool()

summary(est3)


est4 <- nhanes %>%
  mice(seed = 123, print = FALSE) %>%
  mice::complete("all") %>%
  Map(f = lm, MoreArgs = list(f = chl ~ age + bmi + hyp)) %>%
  pool()


# mild workflow using purrr::map
library(purrr)
est5 <- nhanes %>%
  mice(seed = 123, print = FALSE) %>%
  mice::complete("all") %>%
  map(lm, formula = chl ~ age + bmi + hyp) %>%
  pool()


est7 <- nhanes %>%
  mice(seed = 123, print = FALSE) %>%
  mice::complete("long") %>%
  group_by(.imp) %>%
  do(model = lm(formula = chl ~ age + bmi + hyp, data = .)) %>%
  as.list() %>%
  .[[-1]] %>%
  pool()

as_tibble(nhanes2)
imp <- mice(nhanes2, m = 10, print = FALSE, seed = 71242)
m2 <- with(imp, lm(chl ~ age + bmi))
pool(m2)

m1 <- with(imp, lm(chl ~ bmi))

summary(D1(m2, m1))
summary(D2(m2, m1))
summary(D3(m2, m1))

as_tibble(boys)

data <- boys[boys$age >= 8, -4]
imp <- mice(data, seed = 28382, m = 10, print = FALSE)
scope <- list(upper = ~ age + hgt + wgt + hc + gen + phb + reg,
              lower = ~1)
expr <- expression(f1 <- lm(tv ~ 1),
                   f2 <- step(f1, scope = scope))
fit <- with(imp, expr)

names(fit)

fit["nmis"]

formulas <- lapply(fit$analyses, formula)
terms <- lapply(formulas, terms)
votes <- unlist(lapply(terms, labels))
table(votes)

fit.without <- with(imp, lm(tv ~ age + gen + reg + phb))
fit.with <- with(imp, lm(tv ~ age + gen + reg + phb + hgt))
D1(fit.with, fit.without)