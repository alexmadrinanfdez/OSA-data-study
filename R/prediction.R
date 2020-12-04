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
train.pred <- subset(x = train, select = c(AHI, gender, age, neck, BMI))
# train.pred.m <- subset(x = train.pred, subset = gender == "male")
# train.pred.f <- subset(x = train.pred, subset = gender == "female")

# subset selection + cross validation
k <- 10
folds <- sample(x = 1:k, size = nrow(train.pred), rep = T) # divide data set in k folds
cv.error <- matrix( # MSE
  nrow = k, ncol = (length(train.pred) - 1)^2,
  dimnames = list(NULL, paste(1:(length(train.pred) - 1)^2))
)
for (i in 1:k) {
  fit <- regsubsets(
    x = AHI ~ age * neck * BMI * gender, data = train.pred[folds != i,], 
    nvmax = (length(train.pred) - 1)^2
  )
  for (j in 1:((length(train.pred) - 1)^2 - 1)) {
    pred <- predict(object = fit, newdata = train.pred[folds == i,], id = j)
    cv.error[i, j] <- mean((train.pred$AHI[folds == i] - pred)^2)
  }
}
cv.error <- apply(X = cv.error, MARGIN = 2, FUN = mean) # to columns
plot(x = cv.error, type = "b", xlab = 'no. of variables', ylab = 'MSE')
points(
  x = which.min(cv.error), y = cv.error[which.min(cv.error)],
  col = "red", pch = 20
)
coef(regsubsets(
  x = AHI ~ age * neck * BMI * gender, data = train.pred), id = which.min(cv.error)
)
lm.fit <- lm(AHI ~ neck + BMI + age:neck + neck:BMI + age:gender, data = train.pred)
lm.fit$coefficients

# shrinkage methods
x <- model.matrix(object = AHI ~ age * neck * BMI * gender, data = train.pred)[,-1]
y <- train.pred$AHI
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
pls.fit <- plsr(AHI ~ age * neck * BMI * gender, data = train.pred, validation = "CV")
summary(pls.fit)
validationplot(object = pls.fit, val.type = "RMSEP", legendpos = "topright")
mean((predict(object = pls.fit, newdata = train.pred, ncomp = 3) - y)^2)
MSEP(object = pls.fit, estimate = c("train", "CV"), ncomp = 3)

# test set
xt <- model.matrix(object = AHI ~ age * neck * BMI * gender, data = test)[,-1] # glmnet

rmse <- c(
  sqrt(mean((mean(train$AHI) - test$AHI)^2)),
  sqrt(mean((predict(lm.fit, test) - test$AHI)^2)),
  sqrt(mean((predict(ridge, lambda.ridge, newx = xt) - test$AHI)^2)),
  sqrt(mean((predict(lasso, lambda.lasso, newx = xt) - test$AHI)^2)),
  RMSEP(pls.fit, "test", test, 3)$val[2]
)

mae <- c(
  mean(abs(mean(train$AHI) - test$AHI)),
  mean(abs(predict(lm.fit, test) - test$AHI)),
  mean(abs(predict(ridge, lambda.ridge, newx = xt) - test$AHI)),
  mean(abs(predict(lasso, lambda.lasso, newx = xt) - test$AHI)),
  mean(abs(predict(pls.fit, test, 3) - test$AHI))
)

plot(rmse, mae)

# data.frame(tag = value, ..., row.names = NULL)
# e <- data.frame(RMSE = rmse, MAE = mae)

# ggplot2::ggplot(data = e, mapping = aes(x = rmse, y = mae)) +
#   geom_point(aes(color = as.factor(1:length(rmse))))
 
# classification
train.class <- subset(x = train, select = c(diagnosis, gender, age, neck, BMI))
# binary classifier
df.twoclass <- df %>% 
  mutate(
    diagnosis = as.character(diagnosis) # reset no. levels
  ) %>% 
  dplyr::select(diagnosis, gender, age, neck, BMI) %>%
  filter(diagnosis == "normal" | diagnosis == "severe") %>% 
  mutate(diagnosis = as.factor(diagnosis))

# CARET setup
index <- createDataPartition(
  y = df.twoclass$diagnosis, # the outcome data are needed
  p = .75, # percentage of data in the training set
  list = FALSE # should the results be in a list (TRUE) or a matrix (FALSE)
)
train.twoclass <- df.twoclass[index,]
test.twoclass  <- df.twoclass[-index,]

ctrl <- trainControl(
  method = "cv",
  classProbs = TRUE, # needed to computed ROC curve
  summaryFunction = twoClassSummary
)
# model fitting
set.seed(1)
# logical regression
glm.fit <- train(
  form = diagnosis ~ .,
  data = train.twoclass,
  method = 'glm',
  family = "binomial",
  metric = "ROC",
  trControl = ctrl
)
set.seed(2)
# LDA
lda.fit <- train(
  form = diagnosis ~ .,
  data = train.twoclass,
  method = 'lda',
  metric = "ROC",
  trControl = ctrl
)
set.seed(3)
# QDA
qda.fit <- train(
  form = diagnosis ~ .,
  data = train.twoclass,
  method = 'qda',
  metric = "ROC",
  trControl = ctrl
)
set.seed(4)
# QDA + stepwise feature selection
qdastep.fit <- train(
  form = diagnosis ~ .,
  data = train.twoclass,
  method = 'stepQDA',
  # Tuning parameters: maxvar (Maximum #Variables), direction (Search Direction)
  metric = "ROC",
  trControl = ctrl
)
set.seed(5)
# KNN
knn.fit <- train(
  form = diagnosis ~ .,
  data = train.twoclass,
  method = 'knn', # tuning parameters: k (#Neighbors)
  # preProcess = c("center", "scale"),
  metric = "ROC",
  tuneLength = 20,
  trControl = ctrl
)
trellis.par.set(name = caretTheme())
plot(knn.fit, col = "darkblue")
# ggplot(knn.fit)
cat(
  'The largest area under the ROC curve is', max(knn.fit[["results"]]$ROC),
  'for the value of k =', knn.fit[["results"]]$k[which.max(knn.fit[["results"]]$ROC)]
)

# comparison
rs <- resamples(x = list(
  logreg = glm.fit,
  lda = lda.fit,
  qda = qda.fit,
  stepqda = qdastep.fit,
  knn = knn.fit
))
summary(rs)

# test set
# predict(object, newdata, type = "prob")
glm.pred <- predict(glm.fit, newdata = test.twoclass)
lda.pred <- predict(lda.fit, newdata = test.twoclass)
qda.pred <- predict(qda.fit, newdata = test.twoclass)
sqda.pred <- predict(qdastep.fit, newdata = test.twoclass)
knn.pred <- predict(knn.fit, newdata = test.twoclass)
# confusion matrices
# confusionMatrix(data, reference)$byClass[n] # accuracy metrics
glm.cf <- confusionMatrix(data = glm.pred, reference = test.twoclass$diagnosis)
lda.cf <- confusionMatrix(data = lda.pred, reference = test.twoclass$diagnosis)
qda.cf <- confusionMatrix(data = qda.pred, reference = test.twoclass$diagnosis)
sqda.cf <- confusionMatrix(data = sqda.pred, reference = test.twoclass$diagnosis)
knn.cf <- confusionMatrix(data = knn.pred, reference = test.twoclass$diagnosis)

# sensitivity is the percentage of true defaulters that are identified
sens <- c(
  glm.cf$byClass[1], lda.cf$byClass[1], qda.cf$byClass[1], 
  sqda.cf$byClass[1], knn.cf$byClass[1]
)
# specificity is the percentage of non-defaulters that are correctly identified
spec <- c(
  glm.cf$byClass[2], lda.cf$byClass[2], qda.cf$byClass[2], 
  sqda.cf$byClass[2], knn.cf$byClass[2]
)

plot(x = sens,y = spec)

# ## KNN
# the KNN classifier predicts the class of a given test observation by identifying
# the observations that are nearest to it, the scale of the variables matters
# standardized.df <- scale(df.class[-1]) # zero mean and unit variance
# knn(train, test, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
# knn.cv(train, cl, k = 1, l = 0, prob = FALSE, use.all = TRUE)
# knn1(train, test, cl)
# knn1(
#   train = df.class[c(3:6, 9)],
#   test = df.class[c(3:6, 9)],
#   cl = df.class$diagnosis
# )
#
# # example with naive bayes
# # load the library
# library(caret)
# # load the iris dataset
# data(iris)
# # define training control
# train_control <- trainControl(method="cv", number=10)
# # fix the parameters of the algorithm
# grid <- expand.grid(.fL=c(0), .usekernel=c(FALSE))
# # train the model
# model <- train(Species~., data=iris, trControl=train_control, method="nb", tuneGrid=grid)
# # summarize results
# print(model)
# 