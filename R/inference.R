# supervised problem

rm(list = ls()) # clear working space

library(readxl) # read Excel files
library(dplyr)  # a grammar of data manipulation
library(MASS)   # support functions and datasets for Venables and Ripley's MASS
library(class)  # functions for classification
library(leaps)  # regression subset selection
library(glmnet) # lasso and elastic-net regularized generalized linear models
library(pls)    # partial least squares and principal component regression
library(boot)   # bootstrap functions (originally by Angelo Canty for S)

source(file = 'fun.R')

file <- 'DB.xlsx'
directory <- '../data'

df <- read_excel(paste(directory, file, sep = "/"))
df <- as.data.frame(df)

# regression
# the task is to predict a target numerical value (AHI)

df.pred <- df %>% mutate(
  gender = as.factor(gender),
  smoker = as.factor(smoker),
  snorer = as.factor(snorer)
) %>% 
  dplyr::select(- c(patient, diagnosis)) %>%
  dplyr::select(AHI, everything())
df.pred.m <- subset(x = df.pred, subset = gender == "male")
df.pred.f <- subset(x = df.pred, subset = gender == "female")

glimpse(df.pred)

# simple linear model
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
detach(df.pred)

# split data into training / validation
train <- sample(
  x = c(TRUE, FALSE), size = nrow(df.pred), replace = TRUE, prob = c(.7, .3)
)
test <- !train
# fit models only with training data
lm.gender.t <- lm(AHI ~ gender, df.pred[train,])
lm.weight.t <- lm(AHI ~ weight, df.pred[train,])
lm.height.t <- lm(AHI ~ height, df.pred[train,])
lm.age.t <- lm(AHI ~ age, df.pred[train,])
lm.neck.t <- lm(AHI ~ neck, df.pred[train,])
lm.smoker.t <- lm(AHI ~ smoker, df.pred[train,])
lm.snorer.t <- lm(AHI ~ snorer, df.pred[train,])
lm.bmi.t <- lm(AHI ~ BMI, df.pred[train,])

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
  rse.lm(lm.gender.t, df.pred[test,], df.pred$AHI[test]),
  rse.lm(lm.weight.t, df.pred[test,], df.pred$AHI[test]),
  rse.lm(lm.height.t, df.pred[test,], df.pred$AHI[test]),
  rse.lm(lm.age.t, df.pred[test,], df.pred$AHI[test]),
  rse.lm(lm.neck.t, df.pred[test,], df.pred$AHI[test]),
  rse.lm(lm.smoker.t, df.pred[test,], df.pred$AHI[test]),
  rse.lm(lm.snorer.t, df.pred[test,], df.pred$AHI[test]),
  rse.lm(lm.bmi.t, df.pred[test,], df.pred$AHI[test])
)
r.sq.t <- c(
  rsq.lm(lm.gender.t, df.pred[test,], df.pred$AHI[test]),
  rsq.lm(lm.weight.t, df.pred[test,], df.pred$AHI[test]),
  rsq.lm(lm.height.t, df.pred[test,], df.pred$AHI[test]),
  rsq.lm(lm.age.t, df.pred[test,], df.pred$AHI[test]),
  rsq.lm(lm.neck.t, df.pred[test,], df.pred$AHI[test]),
  rsq.lm(lm.smoker.t, df.pred[test,], df.pred$AHI[test]),
  rsq.lm(lm.snorer.t, df.pred[test,], df.pred$AHI[test]),
  rsq.lm(lm.bmi.t, df.pred[test,], df.pred$AHI[test])
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
axis(1, at = 1:8, labels = names(df.pred)[1 + sigma$ix])
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

# multiple linear model
lm.fit <- lm(formula = AHI ~ ., data = df.pred)
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
lm.bang <- lm(formula = AHI ~ BMI + age + neck + gender, data = df.pred) 
summary(lm.bang)

op <- par(mfrow =c(2, 2))
plot(lm.fit)
plot(lm.bckwd)
plot(lm.bang)
par(op)

# interaction terms
lm.it <- lm(formula = AHI ~ age * neck * BMI, data = df.pred)
summary(lm.it)
lm.it <- update(object = lm.it, formula. = ~ . - age:neck - age:BMI)
summary(lm.it)

# non-linear transformations
# function I() is needed since the ^ has a special meaning in a formula;
# wrapping allows the standard usage in R
lm.nlt <- lm(formula = AHI ~ neck + I(neck^2), data = df.pred)
lm.log <- update(object = lm.it, formula. = log1p(AHI) ~ .)
summary(lm.nlt)
summary(lm.log)

anova(lm.neck, lm.fit, lm.bckwd, lm.bang, lm.it, lm.nlt) # can't compare lm.log
anova(lm.fit, lm.it, lm.nlt)

rse <- rep(NA, 3)
names(rse) <- c('full', 'interaction', 'non-linear')
rse[1] <- rse.lm(
  object = lm(AHI ~ ., data = df.pred[train,]), 
  x = df.pred[test,], 
  y = df.pred$AHI[test]
)
rse[2] <- rse.lm(
  object = lm(AHI ~ age * neck * BMI - age:neck - age:BMI, data = df.pred[train,]),
  x = df.pred[test,],
  y = df.pred$AHI[test]
)
rse[3] <- rse.lm(
  object = lm(AHI ~ neck + I(neck^2), data = df.pred[train,]),
  x = df.pred[test,],
  y = df.pred$AHI[test]
)
cat('The lowest error model is the', names(rse)[which.min(rse)], 'model\n')

coef(lm.it)[2:length(coef(lm.it))]
confint(object = lm.it, parm = 2:length(coef(lm.it)))

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

# resampling methods
glmfit <- glm(AHI ~ ., data = df.pred)
# validation set
train <- sample(x = nrow(df), size = nrow(df) * 2/3)
mean((df.pred$AHI - predict(glmfit, df))[- train]^2) # MSE
# leave-one-out cross-validation (LOOCV)
cv.glm(data = df.pred, glmfit = glmfit)$delta[1]
# k-fold CV
cv.glm(data = df.pred, glmfit = glmfit, K = 10)$delta[1]
# bootstrap
# boot(data, statistic, R)

error.v <- rep(x = 0, times = 10)
error.cv <- rep(x = 0, times = 10)
error.loocv <- rep(x = cv.glm(data = df.pred, glmfit = glmfit)$delta[1], times = 10)
for (i in 1:10) {
  train <- sample(x = nrow(df), size = nrow(df) * 2/3)
  error.v[i] <- mean((df.pred$AHI - predict(glmfit, df))[- train]^2)
  error.cv[i] <- cv.glm(data = df.pred, glmfit = glmfit, K = 10)$delta[1]
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
reg.sum <- summary(regsubsets(x = AHI ~ ., data = df.pred))
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

coef(regsubsets(x = AHI ~ ., data = df.pred), which.min(reg.sum$bic))[-1]
# bacward & forward stepwise selection
summary(regsubsets(x = AHI ~ ., data = df.pred, method = "backward"))
summary(regsubsets(x = AHI ~ ., data = df.pred, method = "forward"))

# subset selection using validation approach
regfit <- regsubsets(x = AHI ~ ., data = df.pred[train,], nvmax = length(df.pred) - 1)
matrix <- model.matrix(object = AHI ~ .,  data = df.pred[test,])
error <- rep(x = NA, times = length(df.pred) - 1)
for (i in 1:(length(df.pred) - 1)) {
  ci <- coef(object = regfit, id = i)
  pred <- matrix[,names(ci)] %*% ci # algebraic multiplication
  error[i] <- mean((df.pred$AHI[test] - pred)^2) # MSE
}

coef(regsubsets(x = AHI ~ ., data = df.pred), id = which.min(error)) # full data set

# regularization (shrinkage methods)
# glmnet(x, y, alpha = 1, nlambda = 100, lambda.min.ratio, lambda = NULL, standardize)
x <- model.matrix(object = AHI ~ ., data = df.pred)[,-1] # exclude intercept
y <- df.pred$AHI
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
  formula = AHI ~ ., data = df.pred, subset = train, scale = TRUE, validation = "none"
)
pls.loo <- plsr(
  formula = AHI ~ ., data = df.pred, subset = train, scale = TRUE, validation = "LOO"
)
summary(pls.loo)
validationplot(object = pls.none, val.type = "RMSEP", legendpos = "topright")
validationplot(object = pls.loo, val.type = "RMSEP", legendpos = "topright")
mean((predict(object = pls.loo, newdata = df.pred[test,], ncomp = 3) - y[test])^2)
mean((mean(y[train]) - y[test])^2) # null linear model

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
x <- seq(from = -10, to = 10, length.out = 100)
plot(x = x, y = sigmoid(x), type = "l", lwd = 3, col = "darkblue")

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
lda.fit <- lda(formula = diagnosis ~ gender + age + BMI, data = df.class, subset = train)

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

sum(df.class$diagnosis[-train] == "normal")
sum(df.class$diagnosis[-train] == "severe")

# quadratic discriminant analysis
# Uses a QR decomposition which will give an error message 
# if the within-group variance is singular for any group
qda.fit <- qda(formula = diagnosis ~ weight + height + age + neck + BMI ,data = df.class.m)

# k-nearest neighbors
# can't be used for inference
# has no parameters
