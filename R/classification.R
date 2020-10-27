# supervised problem
# the task is to predict a target categorical value (diagnosis)

rm(list = ls()) # clear working space

library(readxl) # read Excel files
library(dplyr)  # a grammar of data manipulation
library(MASS)   # support functions and datasets for Venables and Ripley's MASS
library(class)  # functions for classification

file <- 'DB.xlsx'
directory <- '../data'

df <- read_excel(paste(directory, file, sep = "/"))
df <- as.data.frame(df)
df.class <- df %>% 
  mutate(
    gender = as.factor(gender),
    smoker = as.factor(smoker),
    snorer = as.factor(snorer),
  ) %>% 
  dplyr::select(- c(patient, AHI)) %>% 
  dplyr::select(diagnosis, everything()) %>%
  filter(diagnosis == "normal" | diagnosis == "severe") %>% 
  mutate(diagnosis = as.factor(diagnosis))
df.class.m <- subset(x = df.class, subset = gender == "male")
# not enough women data

glimpse(df.class)

contrasts(df.class$diagnosis)

# logistic regression (generalized linear model)
## simple
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

## multiple
glm.fit <- glm(formula = diagnosis ~ ., family = binomial, data = df.class)
summary(glm.fit)
summary(glm.fit)$coef
coef(glm.fit)
### backward selection
glm.bckwd <- update(object = glm.fit, formula. = ~ . - smoker)
summary(glm.bckwd)
glm.bckwd <- update(object = glm.bckwd, formula. = ~ . - snorer)
summary(glm.bckwd)
glm.bckwd <- update(object = glm.bckwd, formula. = ~ . - neck)
summary(glm.bckwd)
glm.bckwd <- update(object = glm.bckwd, formula. =  ~ . - height - weight)
summary(glm.bckwd)

op <- par(mfrow =c(2, 2))
plot(glm.bckwd)
par(op)

# linear discriminant analysis
lda.fit <- lda(formula = diagnosis ~ ., data = df.class)
# lda.fit$svd

# quadratic discriminant analysis
# Uses a QR decomposition which will give an error message 
# if the within-group variance is singular for any group
qda.fit <- qda(
  formula = diagnosis ~ weight + height + age + neck + BMI ,
  data = df.class.m
)

# k-nearest neighbors
# can't be used for inference
# already gives the result of the prediction

