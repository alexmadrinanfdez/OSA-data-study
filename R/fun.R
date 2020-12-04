# logistic function
sigmoid <- function(x) {
  exp(x) / (1 + exp(x))
}

# compute RSE from the output of predict.lm()
rse.lm <- function(object, x, y) {
  fit <- predict.lm(object = object, newdata = x)
  sqrt(sum((y - fit)^2)/(length(y) - 2))
}

# compute R-squared from the output of predict.lm()
rsq.lm <- function(object, x, y) {
  fit <- predict.lm(object = object, newdata = x)
  rss <- sum((y - fit)^2)
  tss <- sum((y - mean(y))^2)
  1 - rss/tss
}

# predict() for objects of class "regsubsets"
predict.regsubsets <- function(object, newdata, id) {
  # extract the formula
  formula <- as.formula(object = object$call[[2]])
  # apply the different coefficients of each subset to a matrix of values
  matrix <- model.matrix(object = formula, data = newdata)
  ci <- coef(object = object, id = id)
  xvars <- names(ci)
  matrix[,xvars] %*% ci
}