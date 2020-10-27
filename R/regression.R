# supervised problem
# prediction
# the task is to predict a target numerical value (AHI)

rm(list = ls()) # clear working space

library(readxl) # read Excel files
library(dplyr)  # a grammar of data manipulation

file <- 'DB.xlsx'
directory <- '../data'

df <- read_excel(paste(directory, file, sep = "/"))
df <- as.data.frame(df)
df.pred <- df %>% mutate(
  gender = as.factor(gender),
  smoker = as.factor(smoker),
  snorer = as.factor(snorer)
) %>% select(- c(patient, diagnosis)) %>% select(AHI, everything())
df.pred.m <- subset(x = df.pred, subset = gender == "male")
df.pred.f <- subset(x = df.pred, subset = gender == "female")

glimpse(df.pred)

# linear model
## simple
lm.gender <- lm(AHI ~ gender, df.pred)
lm.weight <- lm(AHI ~ weight, df.pred)
lm.height <- lm(AHI ~ height, df.pred)
lm.age <- lm(AHI ~ age, df.pred)
lm.neck <- lm(AHI ~ neck, df.pred)
lm.smoker <- lm(AHI ~ smoker, df.pred)
lm.snorer <- lm(AHI ~ snorer, df.pred)
lm.bmi <- lm(AHI ~ BMI, df.pred)

attach(df.pred)
op <- par(mfrow =c(2, 4))
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
abline(lm.bmi, lty = 2, col= "darkblue")
par(op)
detach(df.pred)

sigma <- sort(
  c(
    1 / sigma(lm.gender),
    1 / sigma(lm.weight),
    1 / sigma(lm.height),
    1 / sigma(lm.age),
    1 / sigma(lm.neck),
    1 / sigma(lm.smoker),
    1 / sigma(lm.snorer),
    1 / sigma(lm.bmi)
  ), index.return = TRUE
)
r.sq <- sort(
  c(
    summary(lm.gender)$r.sq,
    summary(lm.weight)$r.sq,
    summary(lm.height)$r.sq,
    summary(lm.age)$r.sq,
    summary(lm.neck)$r.sq,
    summary(lm.smoker)$r.sq,
    summary(lm.snorer)$r.sq,
    summary(lm.bmi)$r.sq
  ), index.return = TRUE
)
f <- sort(
  c(
    summary(lm.gender)$fstatistic[1],
    summary(lm.weight)$fstatistic[1],
    summary(lm.height)$fstatistic[1],
    summary(lm.age)$fstatistic[1],
    summary(lm.neck)$fstatistic[1],
    summary(lm.smoker)$fstatistic[1],
    summary(lm.snorer)$fstatistic[1],
    summary(lm.bmi)$fstatistic[1]
  ), index.return = TRUE
)

op <- par(mfrow = c(1, 3))
plot( # residual standard error (deviation)
  x = sigma$x, 
  type = "b", xaxt = "n", ylab = '1 / RSE', xlab = '', 
  pch = 7, col = 1:8, lty = 2
)
axis(1, at = 1:8, labels = names(df.pred)[1 + sigma$ix])
plot( # R squared (R^2)
  x = r.sq$x, 
  type = "b", xaxt = "n", ylab = 'R-squared', xlab = '', 
  pch = 7, col = 1:8, lty = 2
)
axis(1, at = 1:8, labels = names(df.pred)[1 + r.sq$ix])
plot( # F statistic
  x = f$x, 
  type = "b", xaxt = "n", ylab = 'F-statistic', xlab = '', 
  pch = 7, col = 1:8, lty = 2
)
axis(1, at = 1:8, labels = names(df.pred)[1 + f$ix])
par(op)

summary(lm.neck)

op <- par(mfrow =c(2, 2))
plot(lm.neck)
par(op)

## multiple
lm.fit <- lm(formula = AHI ~ ., data = df.pred)
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

lm.bang <- lm(formula = AHI ~ BMI + age + neck + gender, data = df.pred) # BANG predictors
summary(lm.bang)

op <- par(mfrow =c(2, 2))
plot(lm.fit)
plot(lm.bckwd)
plot(lm.bang)
par(op)

### interaction terms
lm.it <- lm(formula = AHI ~ age * neck * BMI, data = df.pred)
summary(lm.it)
lm.it <- update(object = lm.it, formula. = ~ . - age:neck - age:BMI)
summary(lm.it)

### non-linear transformations
# function I() is needed since the ^ has a special meaning in a formula;
# wrapping allows the standard usage in R
lm.nlt <- lm(formula = AHI ~ neck + I(neck^2), data = df.pred)
lm.log <- update(object = lm.it, formula. = log1p(AHI) ~ .)
summary(lm.nlt)
summary(lm.log)

anova(lm.neck, lm.fit, lm.bckwd, lm.bang, lm.it, lm.nlt) # can't compare lm.log
anova(lm.fit, lm.it, lm.nlt)
coef(lm.fit)
coef(lm.it)
coef(lm.nlt)

lm.it.male <- lm(formula = AHI ~ age * neck * BMI, data = df.pred.m)
lm.it.female <- lm(formula = AHI ~ age * neck * BMI, data = df.pred.f)
lm.nlt.male <- lm(formula = AHI ~ neck + I(neck^2), data = df.pred.m)
lm.nlt.female <- lm(formula = AHI ~ neck + I(neck^2), data = df.pred.f)

summary(lm.it.male)
summary(lm.nlt.male)
anova(lm.it.male, lm.nlt.male)

summary(lm.it.female)
summary(lm.nlt.female)
anova(lm.it.female, lm.nlt.female)

# lattice::rfs(model = )
