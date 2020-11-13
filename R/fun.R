# compute RSE from the output of predict.lm()
rse.lm <- function(object, x, y) {
  fit <- predict.lm(object = object, newdata = x)
  sqrt(sum((y - fit)^2)/(length(y) - 2))
}

# # predict() for objects of class "regsubsets"
# predict.regsubsets <- function(object, newdata, id) {
#   # extract the formula
#   formula <- as.formula(object = object$call[[2]])
#   # apply the different coefficients of each subset to a matrix of values
#   matrix <- model.matrix(object = formula, data = newdata)
#   ci <- coef(object = object, id = id)
#   xvars <- names(ci)
#   matrix[,xvars] %*% ci
# }