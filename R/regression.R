# supervised problem
# the task is to predict a target numerical value (AHI)

rm(list = ls())   # clear working space

library(readxl)   # read Excel files
library(dplyr)    # a grammar of data manipulation

file <- 'DB.xlsx'
directory <- '../data'

df <- read_excel(paste(directory, file, sep = "/"))
df <- as.data.frame(df)
df <- 
  df %>% mutate(
    gender = as.factor(gender),
    smoker = as.factor(smoker),
    snorer = as.factor(snorer)
  ) %>% select(- patient)

glimpse(df)

# linear model
## simple
lm.gender <- lm(AHI ~ gender, df)
lm.weight <- lm(AHI ~ weight, df)
lm.height <- lm(AHI ~ height, df)
lm.age <- lm(AHI ~ age, df)
lm.neck <- lm(AHI ~ neck, df)
lm.smoker <- lm(AHI ~ smoker, df)
lm.snorer <- lm(AHI ~ snorer, df)
lm.BMI <- lm(AHI ~ BMI, df)

attach(df)
op <- par(mfrow =c(2,4))
plot(gender, AHI, col = "lightblue")
points(c(
  coef(lm.gender)[1],
  coef(lm.gender)[1] + coef(lm.gender)[2:length(coef(lm.gender))]), 
  pch = 4, col = "blue"
)
lines(c(
  coef(lm.gender)[1],
  coef(lm.gender)[1] + coef(lm.gender)[2:length(coef(lm.gender))]),
  lty = 2, col = "darkblue"
)
plot(weight, AHI, col = "lightblue")
abline(lm.weight, lty = 2, col = "darkblue")
plot(height, AHI, col = "lightblue")
abline(lm.height, lty = 2, col = "darkblue")
plot(age, AHI, col = "lightblue")
abline(lm.age, lty = 2, col = "darkblue")
plot(neck, AHI, col = "lightblue")
abline(lm.neck, lty = 2, col= "darkblue")
plot(smoker, AHI, col = "lightblue")
points(c(
  coef(lm.smoker)[1],
  coef(lm.smoker)[1] + coef(lm.smoker)[2:length(coef(lm.smoker))]), 
  pch = 4, col= "blue"
)
lines(c(
  coef(lm.smoker)[1],
  coef(lm.smoker)[1] + coef(lm.smoker)[2:length(coef(lm.smoker))]),
  lty = 2, col = "darkblue"
)
plot(snorer, AHI, col = "lightblue")
points(c(
  coef(lm.snorer)[1],
  coef(lm.snorer)[1] + coef(lm.snorer)[2:length(coef(lm.snorer))]), 
  pch = 4, col= "blue"
)
lines(c(
  coef(lm.snorer)[1],
  coef(lm.snorer)[1] + coef(lm.snorer)[2:length(coef(lm.snorer))]),
  lty = 2, col= "darkblue"
)
plot(BMI, AHI, col = "lightblue")
abline(lm.BMI, lty = 2, col= "darkblue")
par(op)
detach(df)

op <- par(mfrow = c(1, 3))
plot( # residual standard error (deviation)
  c(
    1 / sigma(lm.gender),
    1 / sigma(lm.weight),
    1 / sigma(lm.height),
    1 / sigma(lm.age),
    1 / sigma(lm.neck),
    1 / sigma(lm.smoker),
    1 / sigma(lm.snorer),
    1 / sigma(lm.BMI)
  ), 
  xaxt = "n", ylab = '1 / RSE', xlab = '', pch = 7, col = 1:8
)
axis(1, at = 1:8, labels = names(df)[-2])
plot( # R squared (R^2)
  c(
    summary(lm.gender)$r.sq,
    summary(lm.weight)$r.sq,
    summary(lm.height)$r.sq,
    summary(lm.age)$r.sq,
    summary(lm.neck)$r.sq,
    summary(lm.smoker)$r.sq,
    summary(lm.snorer)$r.sq,
    summary(lm.BMI)$r.sq
    ),
  xaxt = "n", ylab = 'R-squared', xlab = '', pch = 7, col = 1:8
)
axis(1, at = 1:8, labels = names(df)[-2])
plot( # F statistic
  c(
    summary(lm.gender)$fstatistic[1],
    summary(lm.weight)$fstatistic[1],
    summary(lm.height)$fstatistic[1],
    summary(lm.age)$fstatistic[1],
    summary(lm.neck)$fstatistic[1],
    summary(lm.smoker)$fstatistic[1],
    summary(lm.snorer)$fstatistic[1],
    summary(lm.BMI)$fstatistic[1]
  ), 
  xaxt = "n", ylab = 'F-statistic', xlab = '', pch = 7, col = 1:8
)
axis(1, at = 1:8, labels = names(df)[-2])
par(op)

summary(lm.neck)

op <- par(mfrow =c(2,2))
plot(lm.neck)
par(op)

## multiple
lm.fit <- lm(formula = AHI ~ ., data = df)
summary(lm.fit)
### backward selection
lm.bckwd <- update(object = lm.fit, formula. = ~ . - smoker)
summary(lm.bckwd)
lm.bckwd <- update(object = lm.bckwd, formula. = ~ . - gender)
summary(lm.bckwd)
lm.bckwd <- update(object = lm.bckwd, formula. = ~ . - snorer)
summary(lm.bckwd)
lm.bckwd <- update(object = lm.bckwd, formula. = ~ . - weight - height)
summary(lm.bckwd)

lm.bang <- lm(formula = AHI ~ BMI + age + neck + gender, data = df) # BANG predictors
summary(lm.bang)

op <- par(mfrow =c(2,2))
plot(lm.fit)
plot(lm.bckwd)
plot(lm.bang)
par(op)

## interaction terms
lm.it <- lm(formula = AHI ~ age * neck * BMI, data = df)
summary(lm.it)
lm.it <- update(object = lm.it, formula. = ~ . - age:neck - age:BMI)
summary(lm.it)

## non-linear transformations
# function I() is needed since the ^ has a special meaning in a formula;
# wrapping as we do allows the standard usage in R
lm.nlt <- lm(formula = AHI ~ neck + I(neck^2), data = df)
lm.log <- update(object = lm.it, formula. = log1p(AHI) ~ .)
summary(lm.nlt)
summary(lm.log)

anova(lm.neck, lm.fit, lm.bckwd, lm.bang, lm.it, lm.nlt) # can't compare lm.log
anova(lm.fit, lm.it, lm.nlt)
