# supervised problem

rm(list = ls())  # clear working space

library(readxl)  # read Excel files
library(dplyr)   # a grammar of data manipulation
library(caTools) # tools: moving window statistics, GIF, base64, ROC AUC, etc
library(leaps)   # regression subset selection
library(MASS)    # support functions and datasets for Venables and Ripley's MASS
library(glmnet)  # lasso and elastic-net regularized generalized linear models
library(class)   # functions for classification
library(boot)    # bootstrap functions (originally by Angelo Canty for S)
library(pls)     # partial least squares and principal component regression
library(caret)   # classification and regression training
library(klaR)    # classification and visualization
library(gam)     # generalized additive models
library(tree)    # classification and regression trees
library(randomForest) # Breiman and Cutler's random forests for classification and regression
library(gbm)     # generalized boosted regression models

source(file = 'fun.R')

file <- 'DB.xlsx'
directory <- '../data'

df <- read_excel(paste(directory, file, sep = "/"))
df <- as.data.frame(df)
df <- df %>% mutate(
  gender = as.factor(gender),
  smoker = as.factor(smoker),
  snorer = as.factor(snorer),
  diagnosis = factor(
    x = diagnosis,
    levels = c("normal", "mild", "moderate", "severe"),
    ordered = TRUE
  )
) %>% dplyr::select(patient, AHI, diagnosis, everything())

set.seed(1)

# standardise
df[,c(5:8, 11)] <- scale(x = df[,c(5:8, 11)])
# split data (train / test) preserving relative ratios of different labels
split <- split(x = df, f = sample.split(Y = df$gender, SplitRatio = 3/4))
test <- split[[1]]
train <- split[[2]]

# regression
train.reg <- subset(x = train, select = c(AHI, gender, age, neck, BMI))
# train.reg.m <- subset(x = train.reg, subset = gender == "male")
# train.reg.f <- subset(x = train.reg, subset = gender == "female")

# subset selection + cross validation
k <- 10
folds <- sample(x = 1:k, size = nrow(train.reg), rep = T) # divide data set in k folds
cv.error <- matrix( # MSE
  nrow = k, ncol = (length(train.reg) - 1)^2,
  dimnames = list(NULL, paste(1:(length(train.reg) - 1)^2))
)
for (i in 1:k) {
  fit <- regsubsets(
    x = AHI ~ age * neck * BMI * gender, data = train.reg[folds != i,], 
    nvmax = (length(train.reg) - 1)^2
  )
  for (j in 1:((length(train.reg) - 1)^2 - 1)) {
    pred <- predict(object = fit, newdata = train.reg[folds == i,], id = j)
    cv.error[i, j] <- mean((train.reg$AHI[folds == i] - pred)^2)
  }
}
cv.error <- apply(X = cv.error, MARGIN = 2, FUN = mean) # to columns
plot(x = cv.error, type = "b", xlab = 'no. variables', ylab = 'MSE')
points(
  x = which.min(cv.error), y = cv.error[which.min(cv.error)],
  col = "red", pch = 20
)
coef(regsubsets(
  x = AHI ~ age * neck * BMI * gender, data = train.reg), id = which.min(cv.error)
)
lm.fit <- lm(AHI ~ neck + BMI + age:neck + neck:BMI + age:gender, data = train.reg)
lm.fit$coefficients

# shrinkage methods
x <- model.matrix(object = AHI ~ age * neck * BMI * gender, data = train.reg)[,-1]
y <- train.reg$AHI
# cv.glmnet(x, y, alpha = 1, nfolds = 10)
# ridge regression
ridge <- glmnet(x = x, y = y, alpha = 0, standardize = FALSE)
cv.ridge <- cv.glmnet(x = x, y = y, alpha = 0)
plot(cv.ridge)
lambda.ridge <- cv.ridge$lambda.min
# the lasso
lasso <- glmnet(x = x, y = y, alpha = 1, standardize = FALSE)
cv.lasso <- cv.glmnet(x = x, y = y, alpha = 1)
plot(cv.lasso)
lambda.lasso <- cv.lasso$lambda.min
# accuracy
mean((predict(object = ridge, s = lambda.ridge, newx = x) - y)^2)
mean((predict(object = lasso, s = lambda.lasso, newx = x) - y)^2)
# interpretability
predict(object = ridge, s = lambda.ridge, type = "coefficients")
predict(object = lasso, s = lambda.lasso, type = "coefficients")

# partial least squares (dimension reduction)
pls.fit <- plsr(AHI ~ age * neck * BMI * gender, data = train.reg, validation = "CV")
summary(pls.fit)
validationplot(object = pls.fit, val.type = "RMSEP", legendpos = "topright")
mean((predict(object = pls.fit, newdata = train.reg, ncomp = 2) - y)^2)
MSEP(object = pls.fit, estimate = c("train", "CV"), ncomp = 2)

# polynomial regression
set.seed(2) # cv.glm() updates the value of .Random.seed
n <- 6
cv.error <- rep(x = 0, times = n - 1)
for (i in 2:n) {
  pol.fit <- glm(
    AHI ~ 
      poly(age, degree = i) + poly(neck, degree = i) + poly(BMI, degree = i) +
      neck:BMI:gender,
    data = train.reg
  )
  cv.error[i-1] = cv.glm(data = train.reg, glmfit = pol.fit, K = k)$delta[1]
}
plot(
  x = 2:n, y = cv.error, type = "b", xlab = 'order of the polynomial', ylab = 'MSE'
)
points(x = which.min(cv.error) + 1, y = min(cv.error), col = "red", pch = 20)
pol.fit <- glm(
  AHI ~ 
    poly(age, degree = 2) + poly(neck, degree = 2) + poly(BMI, degree = 2) +
    neck:BMI:gender,
  data = train.reg
)

# regression splines
set.seed(3)
n <- 10
cv.error <- rep(x = 0, times = n)
for (i in 1:n) {
  ns.fit <- glm(
    AHI ~ ns(age, df = i) + ns(neck, df = i) + ns(BMI, df = i) + neck:BMI:gender,
    data = train.reg
  )
  cv.error[i] = cv.glm(data = train.reg, glmfit = ns.fit, K = k)$delta[1]
}
plot(x = cv.error, type = "b", xlab = 'degrees of freedom', ylab = 'MSE')
points(x = which.min(cv.error), y = min(cv.error), col = "red", pch = 20)
ns.fit <- glm(
  AHI ~ ns(age, df = 1) + ns(neck, df = 1) + ns(BMI, df = 1) + neck:BMI:gender,
  data = train.reg
)

# smoothing splines
set.seed(4)
n <- 10
cv.error <- rep(x = 0, times = n)
for (i in 1:n) {
  ss.fit <- gam(
    AHI ~ s(age, df = i) + s(neck, df = i) + s(BMI, df = i) + neck:BMI:gender,
    data = train.reg
  )
  cv.error[i] = cv.glm(data = train.reg, glmfit = ss.fit, K = k)$delta[1]
}
plot(x = cv.error, type = "b", xlab = 'degrees of freedom', ylab = 'MSE')
points(x = which.min(cv.error), y = min(cv.error), col = "red", pch = 20)
ss.fit <- gam(
  AHI ~ s(age, df = 2) + s(neck, df = 2) + s(BMI, df = 2) + neck:BMI:gender,
  data = train.reg
)

# local regression
set.seed(5)
n <- 10
cv.error <- rep(x = 0, times = n - 1)
for (i in 2:n) {
  lo.fit <- gam(
    AHI ~ lo(age, neck, BMI, span = i/10) + neck:BMI:gender, data = train.reg
  )
  cv.error[i-1] = cv.glm(data = train.reg, glmfit = lo.fit, K = k)$delta[1]
}
plot(x = 2:n/10, y = cv.error, type = "b", xlab = 'span', ylab = 'MSE')
points(
  x = (which.min(cv.error) + 1)/10, y = min(cv.error),
  col = "red", pch = 20
)
lo.fit <- gam(
  AHI ~ lo(age, neck, BMI, span = .7) + neck:BMI:gender, data = train.reg
)

# GAM
gam.fit <- gam(AHI ~ lo(age, BMI) + s(neck) + neck:BMI:gender, data = train.reg)
# library(akima) to plot two-dimensional surface
# plot(gam.fit)

# trees
tree <- tree( # trees cannot handle interaction terms
  formula = AHI ~ ., data = train.reg 
)
cv.tree <- cv.tree(object = tree) # pruning based on deviance
plot(
  x = cv.tree$size, y = cv.tree$dev, type = "b", 
  xlab = 'no. terminal nodes', ylab = 'RSE'
)
points(
  x = cv.tree$size[which.min(cv.tree$dev)], y = min(cv.tree$dev),
  col = "red", pch = 20
)
tree <- prune.tree(tree = tree, best = cv.tree$size[which.min(cv.tree$dev)])

# bagging
bag.fit <- randomForest( # ntree = 500
  formula = AHI ~ ., data = train.reg,
  mtry = ncol(train.reg) - 1, importance = TRUE
)
importance(bag.fit)
varImpPlot(bag.fit)

# random forest
rf.fit <- randomForest( # mtry = p/3 (for regression)
  formula = AHI ~ ., data = train.reg, importance = TRUE
)
importance(rf.fit)
varImpPlot(rf.fit)

# boosting
set.seed(6)
k <- 10
cv.error <- rep(x = 0, times = k)
for (i in 1:k) {
  fit <- gbm(
    formula = AHI ~ ., distribution = "gaussian", data = train.reg[folds != i,],
    n.trees = i * 100
  )
  pred <- predict(object = fit, newdata = train.reg[folds == i,])
  cv.error[i] <- mean((train.reg$AHI[folds == i] - pred)^2)
}
plot(x = 1:10 * 100, y = cv.error, type = "b", xlab = 'no. trees', ylab = 'MSE')
points(
  x = which.min(cv.error) * 100, y = min(cv.error),
  col = "red", pch = 20
)
# n.trees = 100, interaction.depth = 1, shrinkage = 0.1
boo.fit <- gbm(formula = AHI ~ ., distribution = "gaussian", data = train.reg)
summary(boo.fit)

# test set
xt <- model.matrix(object = AHI ~ age * neck * BMI * gender, data = test)[,-1] # glmnet

rmse <- c(
  sqrt(mean((mean(train$AHI) - test$AHI)^2)),
  sqrt(mean((predict(lm.fit, test) - test$AHI)^2)),
  sqrt(mean((predict(ridge, lambda.ridge, newx = xt) - test$AHI)^2)),
  sqrt(mean((predict(lasso, lambda.lasso, newx = xt) - test$AHI)^2)),
  RMSEP(pls.fit, "test", test, 2)$val[2],
  sqrt(mean((predict(pol.fit, test) - test$AHI)^2)),
  sqrt(mean((predict(ns.fit, test) - test$AHI)^2)),
  sqrt(mean((predict(ss.fit, test) - test$AHI)^2)),
  sqrt(mean((predict.glm(lo.fit, test) - test$AHI)^2)),
  sqrt(mean((predict.glm(gam.fit, test) - test$AHI)^2)),
  sqrt(mean((predict(tree, test) - test$AHI)^2)),
  sqrt(mean((predict(bag.fit, test) - test$AHI)^2)),
  sqrt(mean((predict(rf.fit, test) - test$AHI)^2)),
  sqrt(mean((predict(boo.fit, test) - test$AHI)^2))
)
mae <- c(
  mean(abs(mean(train$AHI) - test$AHI)),
  mean(abs(predict(lm.fit, test) - test$AHI)),
  mean(abs(predict(ridge, lambda.ridge, newx = xt) - test$AHI)),
  mean(abs(predict(lasso, lambda.lasso, newx = xt) - test$AHI)),
  mean(abs(predict(pls.fit, test, 2) - test$AHI)),
  mean(abs(predict(pol.fit, test) - test$AHI)),
  mean(abs(predict(ns.fit, test) - test$AHI)),
  mean(abs(predict(ss.fit, test) - test$AHI)),
  mean(abs(predict.glm(lo.fit, test) - test$AHI)),
  mean(abs(predict.glm(gam.fit, test) - test$AHI)),
  mean(abs(predict(tree, test) - test$AHI)),
  mean(abs(predict(bag.fit, test) - test$AHI)),
  mean(abs(predict(rf.fit, test) - test$AHI)),
  mean(abs(predict(boo.fit, test) - test$AHI))
)

names <- c(
  'null', 'linear', 'ridge', 'lasso', 'PLS', 
  'polynomial', 'natural splines', 'smooth splines', 'local', 'GAM',
  'tree', 'bagging', 'forest', 'boosting'
)
# data.frame(tag = value, ..., row.names = NULL)
acc <- data.frame(rmse = rmse, mae = mae, row.names = names)
xyplot(
  x = rmse ~ mae, data = acc, groups = rownames(x = acc), 
  auto.key = list(columns = 5)
)
xyplot(
  x = rmse ~ mae, data = acc, subset = -1, groups = rownames(x = acc), 
  auto.key = list(columns = 5)
)
ggplot2::ggplot(data = acc[-1,], mapping = aes(x = mae, y = rmse)) +
   geom_point(aes(color = names[-1]))
 

# classification
train.class <- subset(x = train, select = c(diagnosis, gender, age, neck, BMI))
# binary classifier
train.twoclass <- df %>% 
  mutate(diagnosis = as.character(diagnosis)) %>% # reset no. levels
  dplyr::select(diagnosis, gender, age, neck, BMI) %>%
  filter(diagnosis == "normal" | diagnosis == "severe") %>% 
  mutate(diagnosis = as.factor(diagnosis))

# CARET setup
# index <- createDataPartition(y = df.twoclass$diagnosis, p = .75, list = FALSE)
# train.twoclass <- df.twoclass[index,]
# test.twoclass  <- df.twoclass[-index,]

ctrl <- trainControl(
  method = "cv",
  classProbs = TRUE, # needed to computed ROC curve
  summaryFunction = twoClassSummary
)
# model fitting
set.seed(1)
# logical regression
glm.fit <- train(
  form = diagnosis ~ gender * age * neck * BMI, data = train.twoclass,
  method = 'glm', family = "binomial",
  metric = "ROC", trControl = ctrl
)
set.seed(2)
# LDA
lda.fit <- train(
  form = diagnosis ~ gender * age * neck * BMI, data = train.twoclass,
  method = 'lda',
  metric = "ROC", trControl = ctrl
)
set.seed(3)
# QDA
qda.fit <- train(
  form = diagnosis ~ gender * age * neck * BMI, data = train.twoclass,
  method = 'qda',
  metric = "ROC", trControl = ctrl
)
# set.seed(4)
# # QDA + stepwise feature selection
# sqda.fit <- train(
#   form = diagnosis ~ gender * age * neck * BMI, data = train.twoclass,
#   method = 'stepQDA',
#   # tuning parameters: maxvar (maximum #variables), direction (search direction)
#   metric = "ROC", tuneGrid = expand.grid(maxvar = 3:ncol(train), direction = 'both'),
#   trControl = ctrl
# )
set.seed(5)
# KNN
knn.fit <- train(
  form = diagnosis ~ gender * age * neck * BMI, data = train.twoclass,
  method = 'knn', # tuning parameter: k (#neighbors)
  # preProcess = c("center", "scale"),
  metric = "ROC", tuneLength = 20, trControl = ctrl
)
trellis.par.set(name = caretTheme())
plot(knn.fit, col = "darkblue")
densityplot(knn.fit, pch = "|")
# ggplot(knn.fit)
cat(
  'The largest area under the ROC curve is', max(knn.fit[["results"]]$ROC),
  'for the value of k =', 
  knn.fit[["results"]]$k[which.max(knn.fit[["results"]]$ROC)], '\n'
)
set.seed(6)
# Logistic regression
lo.fit <- train(
  form = diagnosis ~ gender + age + neck + BMI, data = train.twoclass,
  method = 'gamLoess', # tuning parameter: span, degree
  # preProcess = c("center", "scale"),
  metric = "ROC", tuneGrid = expand.grid(span = seq(.1, 1, .05), degree = 1),
  trControl = ctrl
)
trellis.par.set(name = caretTheme())
plot(lo.fit, col = "darkblue")
densityplot(lo.fit, pch = "|")
cat(
  'The largest area under the ROC curve is', max(lo.fit[["results"]]$ROC),
  'for the value of span =', 
  lo.fit[["results"]]$span[which.max(lo.fit[["results"]]$ROC)], '\n'
)
set.seed(7)
# Splines
spl.fit <- train(
  form = diagnosis ~ gender + age + neck + BMI, data = train.twoclass,
  method = 'gamSpline', # tuning parameter: df (Degrees of Freedom)
  # preProcess = c("center", "scale"),
  metric = "ROC", tuneLength = 20, trControl = ctrl
)
trellis.par.set(name = caretTheme())
plot(spl.fit, col = "darkblue")
densityplot(lo.fit, pch = "|")
cat(
  'The largest area under the ROC curve is', max(spl.fit[["results"]]$ROC),
  'for the value of df =', 
  spl.fit[["results"]]$df[which.max(spl.fit[["results"]]$ROC)], '\n'
)

# comparison
rs <- resamples(x = list(
  logreg = glm.fit,
  lda = lda.fit,
  qda = qda.fit,
  # stepqda = sqda.fit,
  knn = knn.fit,
  loess = lo.fit,
  spline = spl.fit
))
summary(rs)

# test set
# predict(object, newdata, type = "prob")
# glm.pred <- predict(glm.fit, newdata = test.twoclass)
# lda.pred <- predict(lda.fit, newdata = test.twoclass)
# qda.pred <- predict(qda.fit, newdata = test.twoclass)
# sqda.pred <- predict(sqda.fit, newdata = test.twoclass)
# knn.pred <- predict(knn.fit, newdata = test.twoclass)
# confusion matrices
# confusionMatrix(data, reference)$byClass[n] # accuracy metrics
# glm.cf <- confusionMatrix(data = glm.pred, reference = test.twoclass$diagnosis)
# lda.cf <- confusionMatrix(data = lda.pred, reference = test.twoclass$diagnosis)
# qda.cf <- confusionMatrix(data = qda.pred, reference = test.twoclass$diagnosis)
# sqda.cf <- confusionMatrix(data = sqda.pred, reference = test.twoclass$diagnosis)
# knn.cf <- confusionMatrix(data = knn.pred, reference = test.twoclass$diagnosis)

# sensitivity is the percentage of true defaulters that are identified
# sens <- c(
#   glm.cf$byClass[1], lda.cf$byClass[1], qda.cf$byClass[1], 
#   sqda.cf$byClass[1], knn.cf$byClass[1]
# )
sens <- c(
  glm.fit[["results"]]$Sens,
  lda.fit[["results"]]$Sens,
  qda.fit[["results"]]$Sens,
  # sqda.fit[["results"]]$Sens,
  knn.fit[["results"]]$Sens[which.max(knn.fit[["results"]]$ROC)],
  lo.fit[["results"]]$Sens[which.max(lo.fit[["results"]]$ROC)],
  spl.fit[["results"]]$Sens[which.max(spl.fit[["results"]]$ROC)]
)
# specificity is the percentage of non-defaulters that are correctly identified
# spec <- c(
#   glm.cf$byClass[2], lda.cf$byClass[2], qda.cf$byClass[2], 
#   sqda.cf$byClass[2], knn.cf$byClass[2]
# )
spec <- c(
  glm.fit[["results"]]$Spec,
  lda.fit[["results"]]$Spec,
  qda.fit[["results"]]$Spec,
  # sqda.fit[["results"]]$Spec,
  knn.fit[["results"]]$Spec[which.max(knn.fit[["results"]]$ROC)],
  lo.fit[["results"]]$Spec[which.max(lo.fit[["results"]]$ROC)],
  spl.fit[["results"]]$Spec[which.max(spl.fit[["results"]]$ROC)]
)

names <- c('logistic', 'lda', 'qda', 'knn', 'local', 'splines')
ratio <- data.frame(sens = sens, spec = spec, row.names = names)
xyplot(
  x = sens ~ spec, data = ratio, groups = rownames(x = ratio), 
  auto.key = list(columns = 6)
)
