# supervised problem

rm(list = ls())  # clear working space

library(readxl)  # read Excel files
library(dplyr)   # a grammar of data manipulation
library(MASS)    # support functions and datasets for Venables and Ripley's MASS
library(class)   # functions for classification
library(leaps)   # regression subset selection
library(glmnet)  # lasso and elastic-net regularized generalized linear models
library(pls)     # partial least squares and principal component regression
library(boot)    # bootstrap functions (originally by Angelo Canty for S)
library(ggplot2) # create data visualisations using the Grammar of Graphics 
library(splines) # regression spline functions and classes

source(file = 'fun.R')

file <- 'DB.xlsx'
directory <- '../data'

df <- read_excel(paste(directory, file, sep = "/"))
df <- as.data.frame(df)

# regression
# the task is to predict a target numerical value (AHI)

df.reg <- df %>% mutate(
  gender = as.factor(gender),
  smoker = as.factor(smoker),
  snorer = as.factor(snorer)
) %>% 
  dplyr::select(- c(patient, diagnosis)) %>%
  dplyr::select(AHI, everything())
df.reg.m <- subset(x = df.reg, subset = gender == "male")
df.reg.f <- subset(x = df.reg, subset = gender == "female")

glimpse(df.reg)

# simple linear model
lm.gender <- lm(AHI ~ gender, df.reg)
lm.weight <- lm(AHI ~ weight, df.reg)
lm.height <- lm(AHI ~ height, df.reg)
lm.age <- lm(AHI ~ age, df.reg)
lm.neck <- lm(AHI ~ neck, df.reg)
lm.smoker <- lm(AHI ~ smoker, df.reg)
lm.snorer <- lm(AHI ~ snorer, df.reg)
lm.bmi <- lm(AHI ~ BMI, df.reg)

attach(df.reg)
op <- par(mfrow =c(2, 4))
plot(gender, AHI, col = "lightblue", xlab = 'gender', ylab = 'AHI')
points(c(
  coef(lm.gender)[1], # intercept is different
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
plot(smoker, AHI, col = "lightblue", xlab = 'smoker', ylab = 'AHI')
points(c(
  coef(lm.smoker)[1], # intercept is different
  coef(lm.smoker)[1] + coef(lm.smoker)[2:length(coef(lm.smoker))]), 
  pch = 4, col= "blue"
)
lines(c(
  coef(lm.smoker)[1],
  coef(lm.smoker)[1] + coef(lm.smoker)[2:length(coef(lm.smoker))]),
  lty = 2, col = "darkblue"
)
plot(snorer, AHI, col = "lightblue", xlab = 'snorer', ylab = 'AHI')
points(c(
  coef(lm.snorer)[1], # intercept is different
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
detach(df.reg)

# split data into training / validation
train <- sample(
  x = c(TRUE, FALSE), size = nrow(df.reg), replace = TRUE, prob = c(.7, .3)
)
test <- !train
# fit models only with training data
lm.gender.t <- lm(AHI ~ gender, df.reg[train,])
lm.weight.t <- lm(AHI ~ weight, df.reg[train,])
lm.height.t <- lm(AHI ~ height, df.reg[train,])
lm.age.t <- lm(AHI ~ age, df.reg[train,])
lm.neck.t <- lm(AHI ~ neck, df.reg[train,])
lm.smoker.t <- lm(AHI ~ smoker, df.reg[train,])
lm.snorer.t <- lm(AHI ~ snorer, df.reg[train,])
lm.bmi.t <- lm(AHI ~ BMI, df.reg[train,])

sigma <- sort(
  c(
    sigma(lm.gender),
    sigma(lm.weight),
    sigma(lm.height),
    sigma(lm.age),
    sigma(lm.neck),
    sigma(lm.smoker),
    sigma(lm.snorer),
    sigma(lm.bmi)
  ), decreasing = TRUE, index.return = TRUE
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
# use testing data for calculating error
sigma.t <- c(
  rse.lm(lm.gender.t, df.reg[test,], df.reg$AHI[test]),
  rse.lm(lm.weight.t, df.reg[test,], df.reg$AHI[test]),
  rse.lm(lm.height.t, df.reg[test,], df.reg$AHI[test]),
  rse.lm(lm.age.t, df.reg[test,], df.reg$AHI[test]),
  rse.lm(lm.neck.t, df.reg[test,], df.reg$AHI[test]),
  rse.lm(lm.smoker.t, df.reg[test,], df.reg$AHI[test]),
  rse.lm(lm.snorer.t, df.reg[test,], df.reg$AHI[test]),
  rse.lm(lm.bmi.t, df.reg[test,], df.reg$AHI[test])
)
r.sq.t <- c(
  rsq.lm(lm.gender.t, df.reg[test,], df.reg$AHI[test]),
  rsq.lm(lm.weight.t, df.reg[test,], df.reg$AHI[test]),
  rsq.lm(lm.height.t, df.reg[test,], df.reg$AHI[test]),
  rsq.lm(lm.age.t, df.reg[test,], df.reg$AHI[test]),
  rsq.lm(lm.neck.t, df.reg[test,], df.reg$AHI[test]),
  rsq.lm(lm.smoker.t, df.reg[test,], df.reg$AHI[test]),
  rsq.lm(lm.snorer.t, df.reg[test,], df.reg$AHI[test]),
  rsq.lm(lm.bmi.t, df.reg[test,], df.reg$AHI[test])
)

op <- par(mfrow = c(1, 3))
plot( # residual standard error (deviation)
  x = sigma$x, ylim = c(15, 25),
  type = "b", xaxt = "n", ylab = 'RSE', xlab = '', 
  pch = 7, col = 1:8, lty = 2
)
points(x = sigma.t[sigma$ix], pch = 7, col = 1:8)
lines(x = sigma.t[sigma$ix], lty = 2, col = "red")
legend(
  x = "topleft", legend = c('training set', 'validation set'),
  col = c("black", "red"), lty = 2, bty = "n"
)
axis(1, at = 1:8, labels = names(df.reg)[1 + sigma$ix])
plot( # R squared (R^2)
  x = r.sq$x, 
  type = "b", xaxt = "n", ylab = 'R-squared', xlab = '', 
  pch = 7, col = 1:8, lty = 2
)
points(x = r.sq.t[sigma$ix], pch = 7, col = 1:8)
lines(x = r.sq.t[sigma$ix], lty = 2, col = "red")
legend(
  x = "topleft", legend = c('training set', 'validation set'),
  col = c("black", "red"), lty = 2, bty = "n"
)
axis(1, at = 1:8, labels = names(df.reg)[1 + r.sq$ix])
plot( # F statistic
  x = f$x, 
  type = "b", xaxt = "n", ylab = 'F-statistic', xlab = '', 
  pch = 7, col = 1:8, lty = 2
)
axis(1, at = 1:8, labels = names(df.reg)[1 + f$ix])
par(op)

summary(lm.neck)

op <- par(mfrow =c(2, 2))
plot(lm.neck)
par(op)

# multiple linear model
lm.fit <- lm(formula = AHI ~ ., data = df.reg)
summary(lm.fit)
# backward selection
lm.bckwd <- update(object = lm.fit, formula. = ~ . - smoker)
summary(lm.bckwd)
lm.bckwd <- update(object = lm.bckwd, formula. = ~ . - gender)
summary(lm.bckwd)
lm.bckwd <- update(object = lm.bckwd, formula. = ~ . - snorer)
summary(lm.bckwd)
lm.bckwd <- update(object = lm.bckwd, formula. = ~ . - weight - height)
summary(lm.bckwd)
# BANG predictors
lm.bang <- lm(formula = AHI ~ BMI + age + neck + gender, data = df.reg) 
summary(lm.bang)

op <- par(mfrow =c(2, 2))
plot(lm.fit)
plot(lm.bckwd)
plot(lm.bang)
par(op)

# interaction terms
lm.it <- lm(formula = AHI ~ age * neck * BMI, data = df.reg)
summary(lm.it)
lm.it <- update(object = lm.it, formula. = ~ . - age:neck - age:BMI)
summary(lm.it)

# logarithmic response
lm.log <- update(object = lm.it, formula. = log1p(AHI) ~ .)
summary(lm.log)

# anova() compares the models sequentially
anova(lm.neck, lm.fit, lm.bckwd, lm.bang, lm.it) # only nested models
anova(lm.fit, lm.bckwd, lm.it)

rse <- rep(NA, 3)
names(rse) <- c('full', 'backward selection', 'interaction')
rse[1] <- rse.lm(
  object = lm(AHI ~ ., data = df.reg[train,]), 
  x = df.reg[test,], 
  y = df.reg$AHI[test]
)
rse[2] <- rse.lm(
  object = lm(AHI ~ age + neck + BMI, data = df.reg[train,]),
  x = df.reg[test,],
  y = df.reg$AHI[test]
)
rse[3] <- rse.lm(
  object = lm(
    AHI ~ age + neck + BMI + neck:BMI + age:neck:BMI, data = df.reg[train,]
  ),
  x = df.reg[test,],
  y = df.reg$AHI[test]
)
cat('The lowest error model is the', names(rse)[which.min(rse)], 'model\n')

coef(lm.it)[2:length(coef(lm.it))]
confint(object = lm.it, parm = 2:length(coef(lm.it)))

lm.it.male <- lm(formula = AHI ~ age * neck * BMI, data = df.reg.m)
lm.it.female <- lm(formula = AHI ~ age * neck * BMI, data = df.reg.f)
lm.nlt.male <- lm(formula = AHI ~ neck + I(neck^2), data = df.reg.m)
lm.nlt.female <- lm(formula = AHI ~ neck + I(neck^2), data = df.reg.f)

summary(lm.it.male)
summary(lm.nlt.male)
anova(lm.it.male, lm.nlt.male)

summary(lm.it.female)
summary(lm.nlt.female)
anova(lm.it.female, lm.nlt.female)

# resampling methods
glmfit <- glm(AHI ~ ., data = df.reg)
# validation set
train <- sample(x = nrow(df), size = nrow(df) * 2/3)
mean((df.reg$AHI - predict(glmfit, df))[- train]^2) # MSE
# leave-one-out cross-validation (LOOCV)
cv.glm(data = df.reg, glmfit = glmfit)$delta[1]
# k-fold CV
cv.glm(data = df.reg, glmfit = glmfit, K = 10)$delta[1]
# bootstrap
# boot(data, statistic, R)

error.v <- rep(x = 0, times = 10)
error.cv <- rep(x = 0, times = 10)
error.loocv <- rep(x = cv.glm(data = df.reg, glmfit = glmfit)$delta[1], times = 10)
for (i in 1:10) {
  train <- sample(x = nrow(df), size = nrow(df) * 2/3)
  error.v[i] <- mean((df.reg$AHI - predict(glmfit, df))[- train]^2)
  error.cv[i] <- cv.glm(data = df.reg, glmfit = glmfit, K = 10)$delta[1]
}
plot(
  x = error.loocv, ylab = 'MSE', ylim = c(200, 350), 
  type = "o", lty = 2, pch = 20, col = "darkblue"
)
lines(x = error.v, lty = 2, col = "darkred")
points(x = error.v, pch = 20, col = "darkred")
lines(x = error.cv, lty = 2, col = "darkgreen")
points(x = error.cv, pch = 20, col = "darkgreen")
legend(
  x = "topleft", legend = c('LOOCV', 'Validation', 'CV'), bty = "n",
  col = c("darkblue", "darkred", "darkgreen"), lty = 2
)

# subset selection
# best subset selection
reg.sum <- summary(regsubsets(x = AHI ~ ., data = df.reg))
op <- par(mfrow = c(2, 2))
plot(x = reg.sum$rss, xlab = 'no. of variables', ylab = 'RSS', type = "b")
plot(
  x = reg.sum$adjr2, 
  xlab = 'no. of variables', ylab = expression('adjusted R'^2), type = "b"
)
points(
  x = which.max(reg.sum$adjr2), y = reg.sum$adjr2[which.max(reg.sum$adjr2)],
  col = "red", pch = 20
)
plot(
  x = reg.sum$cp, 
  xlab = 'no. of variables', ylab = 'Cp', type = "b"
)
points(
  x = which.min(reg.sum$cp), y = reg.sum$cp[which.min(reg.sum$cp)],
  col = "red", pch = 20
)
plot(
  x = reg.sum$bic, 
  xlab = 'no. of variables', ylab = 'BIC', type = "b"
)
points(
  x = which.min(reg.sum$bic), y = reg.sum$bic[which.min(reg.sum$bic)],
  col = "red", pch = 20
)
par(op)

coef(regsubsets(x = AHI ~ ., data = df.reg), which.min(reg.sum$bic))[-1]
# bacward & forward stepwise selection
summary(regsubsets(x = AHI ~ ., data = df.reg, method = "backward"))
summary(regsubsets(x = AHI ~ ., data = df.reg, method = "forward"))

# subset selection using validation approach
regfit <- regsubsets(
  x = AHI ~ ., data = df.reg[train,], nvmax = length(df.reg) - 1
)
matrix <- model.matrix(object = AHI ~ .,  data = df.reg[test,])
error <- rep(x = NA, times = length(df.reg) - 1)
for (i in 1:(length(df.reg) - 1)) {
  ci <- coef(object = regfit, id = i)
  pred <- matrix[,names(ci)] %*% ci # algebraic multiplication
  error[i] <- mean((df.reg$AHI[test] - pred)^2) # MSE
}

coef(regsubsets(x = AHI ~ ., data = df.reg), id = which.min(error)) # all data

# regularization (shrinkage methods)
# glmnet(x, y, alpha = 1, nlambda = 100, lambda = NULL, standardize)
x <- model.matrix(object = AHI ~ ., data = df.reg)[,-1] # exclude intercept
y <- df.reg$AHI
# ridge regression (alpha = 0)
ridge <- glmnet(x = x[train,], y = y[train], alpha = 0, standardize = FALSE)
mean((predict(object = ridge, s = 50, newx = x[test,]) - y[test])^2)
# the lasso (alpha = 1)
lasso <- glmnet(x = x[train,], y = y[train], alpha = 1, standardize = FALSE)
mean((predict(object = lasso, s = 50, newx = x[test,]) - y[test])^2)

op <- par(mfrow = c(1, 2))
plot(ridge)
plot(lasso)
par(op)

coef(object = lasso, s = 0) # linear regression
coef(object = lasso, s = .1)
coef(object = lasso, s = 1)
coef(object = lasso, s = 10)

# partial least squares (dimension reduction)
# plsr(formula, subset, scale = FALSE, validation = c("none", "CV", "LOO"))
pls.none <- plsr(
  formula = AHI ~ ., data = df.reg, subset = train, 
  scale = TRUE, validation = "none"
)
pls.loo <- plsr(
  formula = AHI ~ ., data = df.reg, subset = train, 
  scale = TRUE, validation = "LOO"
)
summary(pls.loo)
validationplot(object = pls.none, val.type = "RMSEP", legendpos = "topright")
validationplot(object = pls.loo, val.type = "RMSEP", legendpos = "topright")
mean((predict(object = pls.loo, newdata = df.reg[test,], ncomp = 3) - y[test])^2)
mean((mean(y[train]) - y[test])^2) # null linear model

# non-linear transformations
# polynomial regression
# function I() is needed since the ^ has a special meaning in a formula;
# wrapping allows the standard usage in R
lm.sq <- lm(formula = AHI ~ neck + I(neck^2), data = df.reg)
# use raw and not orthogonal polynomials
lm.pol <- lm(formula = AHI ~ poly(x = neck, degree = 5, raw = T), data = df.reg)
summary(lm.sq)
summary(lm.pol)
# number of degrees
v.error <- rep.int(x = 0, times = 5)
for (i in 1:5) {
  pol.fit <- lm(
    AHI ~ poly(neck, degree = i, raw = TRUE), data = df.reg, subset = train
  )
  v.error[i] <- mean((df.reg$AHI - predict(pol.fit, df.reg))[test]^2)
}
plot(x = v.error, type = "b", pch = 19, xlab = 'Polynomial degree', ylab = 'MSE')
points(x = which.min(v.error), y = min(v.error), pch = 19, col = "grey")

lims <- range(df.reg$neck)
grid <- seq(from = lims[1], to = lims[2])
sq.pred <- predict(object = lm.sq, newdata = list(neck = grid), se.fit = TRUE)
pol.pred <- predict(object = lm.pol, newdata = list(neck = grid), se.fit = TRUE)
op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
plot(
  x = df.reg$neck, y = df.reg$AHI, xlim = lims, cex = .5, col = "darkgrey", 
  main = 'degree-2 polynomial', xlab = 'neck', ylab = 'AHI'
)
lines(x = grid, y = sq.pred$fit, lwd = 2, col = "darkblue")
matlines(
  x = grid, 
  y = cbind(sq.pred$fit + 2*sq.pred$se.fit, sq.pred$fit - 2*sq.pred$se.fit), 
  lty = 3, col = "darkblue"
)
plot(
  x = df.reg$neck, y = df.reg$AHI, xlim = lims, cex = .5, col = "darkgrey", 
  main = 'degree-5 polynomial', xlab = 'neck', ylab = 'AHI'
)
lines(x = grid, y = pol.pred$fit, lwd = 2, col = "darkblue")
matlines(
  x = grid, 
  y = cbind(pol.pred$fit + 2*pol.pred$se.fit, pol.pred$fit - 2*pol.pred$se.fit), 
  lty = 3, col = "darkblue"
)
title(main = 'polynomial regression', outer = TRUE)
par(op)

pol.full <- lm( # only quantitative features
  formula = AHI ~ 
    poly(x = weight, degree = 5, raw = T) +
    poly(x = height, degree = 5, raw = T) +
    poly(x = age, degree = 5, raw = T) +
    poly(x = neck, degree = 5, raw = T) +
    poly(x = BMI, degree = 5, raw = T),
  data = df.reg
)
summary(pol.full)

# step function
lm.step <- lm(formula = AHI ~ cut(x = neck, breaks = 4), data = df.reg)
summary(lm.step)

v.error <- rep.int(x = 0, times = 5)
v.error[1] <- mean((df.reg$AHI - predict(lm.neck.t, df.reg))[test]^2)
for (i in 2:7) {
  step.fit <- lm(AHI ~ cut(neck, breaks = i), data = df.reg, subset = train)
  v.error[i] <- mean((df.reg$AHI - predict(step.fit, df.reg))[test]^2)
}
plot(x = v.error, type = "b", pch = 19, xlab = 'Number of cuts', ylab = 'MSE')
points(x = which.min(v.error), y = min(v.error), pch = 19, col = "grey")

step.pred <- predict(object = lm.step, newdata = list(neck = grid), se.fit = TRUE)
plot(
  x = df.reg$neck, y = df.reg$AHI, xlim = lims, cex = .5, col = "darkgrey", 
  main = '4-step function', xlab = 'neck', ylab = 'AHI'
)
lines(x = grid, y = step.pred$fit, lwd = 2, col = "darkblue")
matlines(
  x = grid, 
  y = cbind(step.pred$fit + 2*step.pred$se.fit, step.pred$fit - 2*step.pred$se.fit), 
  lty = 3, col = "darkblue"
)

# regression splines
# polynomial splines
# bs(x, df = NULL, knots = NULL, degree = 3, intercept = FALSE)
lm.bs <- lm(AHI ~ bs(x = neck, df = 6), data = df.reg)
summary(lm.bs)
attr(x = bs(x = df.reg$neck, df = 6), "knots")
# natural splines
# ns(x, df = NULL, knots = NULL, intercept = FALSE)
lm.ns <- lm(AHI ~ ns(x = neck, df = 4), data = df.reg)
summary(lm.ns)
attr(x = ns(x = df.reg$neck, df = 4), "knots")

bs.pred <- predict(object = lm.bs, newdata = list(neck = grid), se.fit = TRUE)
ns.pred <- predict(object = lm.ns, newdata = list(neck = grid), se.fit = TRUE)
op <- par(mfrow = c(1, 2), mar = c(4, 4, 3, 1), oma = c(0, 0, 3, 0))
plot(
  x = df.reg$neck, y = df.reg$AHI, xlim = lims, cex = .5, col = "darkgrey", 
  main = 'polynomial', xlab = 'neck', ylab = 'AHI'
)
lines(x = grid, y = bs.pred$fit, lwd = 2, col = "darkblue")
matlines(
  x = grid, 
  y = cbind(bs.pred$fit + 2*bs.pred$se.fit, bs.pred$fit - 2*bs.pred$se.fit), 
  lty = 3, col = "darkblue"
)
plot(
  x = df.reg$neck, y = df.reg$AHI, xlim = lims, cex = .5, col = "darkgrey", 
  main = 'natural', xlab = 'neck', ylab = 'AHI'
)
lines(x = grid, y = ns.pred$fit, lwd = 2, col = "darkblue")
matlines(
  x = grid, 
  y = cbind(ns.pred$fit + 2*ns.pred$se.fit, ns.pred$fit - 2*ns.pred$se.fit), 
  lty = 3, col = "darkblue"
)
title(main = 'regression splines', outer = TRUE)
par(op)

rs.full <- lm( # only quantitative features
  formula = AHI ~
    bs(x = weight, df = 6) +
    bs(x = height, df = 6) +
    bs(x = age, df = 6) +
    bs(x = neck, df = 6) + 
    bs(x = BMI, df = 6), 
  data = df.reg
)
summary(rs.full)
rs.bckwd <- lm( # only quantitative features
  formula = AHI ~ bs(x = age, df = 6) + bs(x = neck, df = 6) + bs(x = BMI, df = 3), 
  data = df.reg
)
summary(rs.bckwd)

# smooth splines
ss.fit <- smooth.spline(x = df.reg$neck, y = df.reg$AHI, cv = TRUE) # LOOCV
ss.fit$df

plot(
  x = df.reg$neck, y = df.reg$AHI, xlim = lims, cex = .5, col = "darkgrey", 
  xlab = 'neck', ylab = 'AHI', main = 'smoothing splines'
)
lines(x = ss.fit, lwd = 2, col = "darkblue")
lines(
  x = smooth.spline(x = df.reg$neck, y = df.reg$AHI, df = 13), 
  lwd = 2, col = "darkred"
)
legend(
  x = "topleft", legend = c('6.55 DF', '13 DF'), 
  col = c("darkblue", "darkred"), lwd = 2, cex = .8
)

# local regression
lr.fit <- loess(formula = AHI ~ neck, data = df.reg)

plot(
  x = df.reg$neck, y = df.reg$AHI, xlim = lims, cex = .5, col = "darkgrey", 
  xlab = 'neck', ylab = 'AHI', main = 'local regression'
)
lines(
  x = grid, y = predict(lr.fit, newdata = data.frame(neck = grid)), 
  lwd = 2, col = "darkblue"
)
lines(
  x = grid, 
  y = predict(
    loess(AHI ~ neck, df.reg, span = .5), newdata = data.frame(neck = grid)
  ), 
  lwd = 2, col = "darkred"
)
lines(
  x = grid, 
  y = predict(
    loess(AHI ~ neck, df.reg, span = .25), newdata = data.frame(neck = grid)
  ), 
  lwd = 2, col = "darkgreen"
)
legend(
  x = "topleft", legend = c('75%', '50%', '25%'), 
  col = c("darkblue", "darkred", "darkgreen"), lwd = 2, cex = .8
)

# lattice::rfs(model = )

# classification
# the task is to predict a target categorical value (diagnosis)

rm(f, r.sq, r.sq.t, sigma, sigma.t, list = ls(pattern = 'lm')) # clear some space

df.class <- df %>% 
  mutate(
    gender = as.factor(gender),
    smoker = as.factor(smoker),
    snorer = as.factor(snorer)
  ) %>% 
  dplyr::select(- c(patient, AHI)) %>% 
  dplyr::select(diagnosis, everything()) %>%
  filter(diagnosis == "normal" | diagnosis == "severe") %>% 
  mutate(diagnosis = as.factor(diagnosis))
df.class.m <- subset(x = df.class, subset = gender == "male")
# not enough women data

glimpse(df.class)

contrasts(df.class$diagnosis)

# simple logistic regression
# sigmoid function
plot.function(
  x = sigmoid, from = -10, to = 10, type = "l", lwd = 3, col = "darkblue"
)

glm.gender <- glm(diagnosis ~ gender, binomial, df.class)
glm.weight <- glm(diagnosis ~ weight, binomial, df.class)
glm.height <- glm(diagnosis ~ height, binomial, df.class)
glm.age <- glm(diagnosis ~ age, binomial, df.class)
glm.neck <- glm(diagnosis ~ neck, binomial, df.class)
glm.smoker <- glm(diagnosis ~ smoker, binomial, df.class)
glm.snorer <- glm(diagnosis ~ snorer, binomial, df.class)
glm.bmi <- glm(diagnosis ~ BMI, binomial, df.class)

aic <- sort(
  x = c(
    glm.gender$aic,
    glm.weight$aic,
    glm.height$aic,
    glm.age$aic,
    glm.neck$aic,
    glm.smoker$aic,
    glm.snorer$aic,
    glm.bmi$aic
  ),
  decreasing = TRUE,
  index.return = TRUE
)
plot( # Akaike's an information criterion (aic)
  x = aic$x, 
  type = "b", xaxt = "n", ylab = 'An Information Criterion (AIC)', xlab = '', 
  pch = 7, col = "darkblue", lty = 2
)
axis(1, at = 1:8, labels = names(df.class)[1 + aic$ix])

# the default prediction is on the scale of the linear predictors
cl.pred <- predict(object = glm.neck, newdata = list(neck = grid), se.fit = TRUE)
se.bands <- cbind(cl.pred$fit + 2*cl.pred$se.fit, cl.pred$fit - 2*cl.pred$se.fit)
plot(
  x = df.class$neck, y = as.numeric(df.class$diagnosis) - 1, 
  xlim = lims, type = "n"
)
points( # rug plot
  x = jitter(df.class$neck), y = as.numeric(df.class$diagnosis) - 1, 
  pch = "|", col = "darkgrey"
)
lines(x = grid, y = sigmoid(cl.pred$fit), lwd = 2, col = "darkblue")
matlines(x = grid, y = sigmoid(se.bands), lty = 3, col = "darkblue")
abline(h = .5 , lty = 2)
# predict probabilities
cl.pred <- predict(glm.neck, newdata = list(neck = grid), type = "response", se.fit = T)
se.bands <- cbind(cl.pred$fit + 2*cl.pred$se.fit, cl.pred$fit - 2*cl.pred$se.fit)
plot(
  x = df.class$neck, y = as.numeric(df.class$diagnosis) - 1, xlim = lims, 
  type = "n", xlab = 'neck', ylab = 'Pr(diagnosis = severe | neck)'
)
points( # rug plot
  x = jitter(df.class$neck), y = as.numeric(df.class$diagnosis) - 1, 
  pch = "|", col = "darkgrey"
)
lines(x = grid, y = cl.pred$fit, lwd = 2, col = "darkblue")
matlines(x = grid, y = se.bands, lty = 3, col = "darkblue")
abline(h = .5 , lty = 2, col = "darkgrey")

# multiple logistic regression
glm.fit <- glm(formula = diagnosis ~ ., family = binomial, data = df.class)
summary(glm.fit)
summary(glm.fit)$coef
coef(glm.fit)
# backward selection
glm.bckwd <- update(object = glm.fit, formula. = ~ . - smoker)
summary(glm.bckwd)
glm.bckwd <- update(object = glm.bckwd, formula. = ~ . - snorer)
summary(glm.bckwd)
glm.bckwd <- update(object = glm.bckwd, formula. = ~ . - neck)
summary(glm.bckwd)
glm.bckwd <- update(object = glm.bckwd, formula. =  ~ . - height - weight)
summary(glm.bckwd)
coef(glm.bckwd)[-1]
exp(coef(glm.bckwd))[-1] # transform the coeficients from log-odds (logits) to odds.

# linear discriminant analysis
lda.fit <- lda(formula = diagnosis ~ ., data = df.class)
lda.fit$svd   # ratio of the between- and within-group standard deviations
lda.fit$svd^2 # their squares are the canonical F-statistics

lda.bang <- lda(
  formula = diagnosis ~ BMI + age + neck + gender,
  data = df.class
)
lda.bang$svd^2

train <- sample(x = nrow(df.class), size = nrow(df.class) * 4/5)

glm.fit <- glm(
  formula = diagnosis ~ gender + age + BMI, 
  family = binomial, data = df.class, subset = train
)
lda.fit <- lda(
  formula = diagnosis ~ gender + age + BMI, data = df.class, subset = train
)

glm.prob <- predict(object = glm.fit, type = "response", newdata = df.class[-train,])
glm.pred <- rep_len(x = levels(df.class$diagnosis)[1], length.out = nrow(df.class[-train,]))
glm.pred[glm.prob > .5] <- levels(df.class$diagnosis)[2] # decision threshold
lda.pred <- predict(object = lda.fit, newdata = df.class[-train,])
# sum(lda.pred$posterior[,2] > .9) # decision threshold
# confusion matrix
table(predicted = glm.pred, true = df.class$diagnosis[-train])
table(predicted = lda.pred$class, true = df.class$diagnosis[-train])
# accuracy
mean(glm.pred == df.class$diagnosis[-train])
mean(lda.pred$class == df.class$diagnosis[-train])
# plotting
ggplot(data = df.class[-train,], mapping = aes(glm.prob)) +
  geom_histogram(
    mapping = aes(color = diagnosis, fill = diagnosis), 
    position = "identity", bins = 20, alpha = .1
  ) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))
ggplot(data = df.class[-train,], mapping = aes(lda.pred$posterior[,2])) +
  geom_histogram(
    mapping = aes(color = diagnosis, fill = diagnosis), 
    position = "identity", bins = 20, alpha = .1
  ) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

sum(df.class$diagnosis[-train] == "normal")
sum(df.class$diagnosis[-train] == "severe")

# quadratic discriminant analysis
# Uses a QR decomposition which will give an error message 
# if the within-group variance is singular for any group
qda.fit <- qda(formula = diagnosis ~ weight + height + age + neck + BMI ,data = df.class.m)

# k-nearest neighbors
# can't be used for inference
# has no parameters