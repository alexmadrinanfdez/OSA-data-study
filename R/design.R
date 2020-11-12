# model design / setup

rm(list = ls())   # clear working space

library(readxl)   # read Excel files
library(dplyr)    # a grammar of data manipulation
library(lattice)  # Trellis graphics for R
library(caTools)  # tools: moving window statistics, GIF, base64, ROC AUC, etc

file <- 'DB.xlsx'
directory <- '../data'

df.tot <- read_excel(paste(directory, file, sep = "/"))
df.tot <- as.data.frame(df.tot)
df.tot <- df.tot %>% mutate(
  gender = as.factor(gender),
  smoker = as.factor(smoker),
  snorer = as.factor(snorer),
  diagnosis = as.factor(diagnosis)
) %>% dplyr::select(- patient)
# comparison between gender populations
df.male <- subset(x = df.tot, subset = gender == "male")
df.female <- subset(x = df.tot, subset = gender == "female")

## mean value
ahi.tot <- mean(df.tot$AHI)
ahi.male <- mean(df.male$AHI)
ahi.female <- mean(df.female$AHI)

cat(
  " mean total:", ahi.tot, "\n",
  "mean male:", ahi.male, "\n",
  "mean female:", ahi.female, "\n"
)

### MAE
mae.tot <- sum(abs(df.tot$AHI - ahi.tot)) / nrow(df.tot)
mae.male <- sum(abs(df.male$AHI - ahi.male)) / nrow(df.male)
mae.female <- sum(abs(df.female$AHI - ahi.female)) / nrow(df.female)

cat(
  " MAE total:", mae.tot, "\n",
  "MAE male:", mae.male, "\n",
  "MAE female:", mae.female, "\n"
)

## models
lm.tot <- lm(formula = AHI ~ age + neck + BMI, data = df.tot)
lm.male <- lm(formula = AHI ~ age + neck + BMI, data = df.male)
lm.female <- lm(formula = AHI ~ age + neck + BMI, data = df.female)

### MAE
mae.lm.tot <- sum(abs(lm.tot$residuals)) / length(lm.tot$residuals)
mae.lm.male <- sum(abs(lm.male$residuals)) / length(lm.male$residuals)
mae.lm.female <- sum(abs(lm.female$residuals)) / length(lm.female$residuals)

cat(
  " total model >", "R2:", summary(lm.tot)$r.sq, "MAE:", mae.lm.tot, "\n",
  "male model >", "R2:", summary(lm.male)$r.sq, "MAE:", mae.lm.male, "\n",
  "female model >", "R2:", summary(lm.female)$r.sq, "MAE:", mae.lm.female, "\n"
)

## regression curves
pred.tot <- lm(formula = lm.tot$fitted.values ~ df.tot$AHI)
pred.male <- lm(formula = lm.male$fitted.values ~ df.male$AHI)
pred.female <- lm(formula = lm.female$fitted.values ~ df.female$AHI)

## graphics
xyplot(
  x = lm.tot$fitted.values ~ AHI,
  data = df.tot, groups = gender,
  ylim = c(-5, 50), auto.key = TRUE,
  ylab = 'fitted values'
)

plot(x = df.tot$AHI, y = lm.tot$fitted.values, ylab = 'fitted values', xlab = 'AHI')
abline(pred.tot, lty = 2, lwd = 2, col = "red")

plot(
  x = df.male$AHI, y = lm.male$fitted.values,
  xlab = 'AHI', ylab = 'fitted values',
  col = "salmon"
)
points(x = df.female$AHI, y = lm.female$fitted.values, col = "lightblue")
abline(pred.male, lty = 2, lwd = 2, col = "red")
abline(pred.female, lty = 2, lwd = 2, col = "blue")
legend(x = "bottomright", legend = c('male', 'female'),
       col = c("salmon", "lightblue"),
       pch = 1, cex = 0.7)

## prevalence problem
op <- par(mfrow = c(1, 3))
pie(table(df.tot$diagnosis), main = 'population sample')
pie(table(df.male$diagnosis), main = 'male population')
pie(table(df.female$diagnosis), main = 'female population')
par(op)

# beta values in predictors although it only ranges between 0 and 1
contrasts(df.tot$gender)

# training / testing data
set.seed(123) # for consistency in results

split_series = sample.split(Y = df.tot, SplitRatio = .8) # split train / test data
df.tot.train = subset(df.tot, split_series == TRUE)
df.tot.test = subset(df.tot, split_series == FALSE)

lm.tot.train <- lm(formula = AHI ~ age + neck + BMI, data = df.tot.train)

predAHI.test <- predict(lm.tot.train, df.tot.test)

mae.lm.tot.train <- sum(abs(lm.tot.train$residuals)) / length(lm.tot.train$residuals)
mae.lm.tot.test <- mean(abs(df.tot.test$AHI - predAHI.test))
mae.naive.test <- mean(abs(df.tot.test$AHI - mean(df.tot.train$AHI)))

cat(
  " MAE train: ", mae.lm.tot.train, "\n",
  "MAE test: ", mae.lm.tot.test, "\n",
  "MAE naive test: ", mae.naive.test
)
