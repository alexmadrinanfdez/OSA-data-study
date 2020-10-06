# Exploratory Data Analysis

# clear working space
rm(list = ls())

library(readxl)   # read Excel files
library(dplyr)    # a grammar of data manipulation
library(corrplot) # visualization of a correlation matrix

file <- 'DB.xlsx'
directory <- '../data'

df <- read_excel(paste(directory, file, sep = "/"))
df <- as.data.frame(df)
df <- 
  df %>% mutate(
    gender = as.factor(gender),
    smoker = as.factor(smoker)
  ) %>% select(patient, IAH, everything())

dim(df)
glimpse(df)

summary(df)

attach(df) # database is attached to the R search path

# aproximated distributions
op <- par(mfrow = c(2, 3))
hist(IAH)
hist(weight)
hist(height)
hist(age)
hist(cervical)
hist(BMI, breaks = 20)
par(op)

# non-normal distributions (Poisson)
op <- par(mfcol = c(2,2))
qqplot(IAH, rpois(length(IAH), lambda = 1), ylab = 'Poisson (lambda = 1)')
hist(log(IAH), breaks = 30)
qqplot(BMI, rpois(length(BMI), lambda = mean(BMI)), ylab = 'Poisson')
hist(log(BMI), breaks = 30)
par(op)

# normal distributions
op <- par(mfrow = c(2, 3), lty = 2)
qqnorm(log(IAH)[is.finite(log(IAH))], main = 'IAH') # log(0) = -Inf
qqline(log(IAH), col = "darkgoldenrod")
qqnorm(weight, main = 'Weight')
qqline(weight, col = "darkgoldenrod")
qqnorm(height, main = 'Height')
qqline(height, col = "darkgoldenrod")
qqnorm(age, main = 'Age')
qqline(age, col = "darkgoldenrod")
qqnorm(cervical, main = 'Cervical')
qqline(cervical, col = "darkgoldenrod")
qqnorm(log(BMI), main = 'BMI')
qqline(log(BMI), col = "darkgoldenrod")
par(op)

# analyze factors differently
gender.tbl <- table(gender)
smoker.tbl <- table(smoker)
op <- par(mfrow = c(1,2))
pie(gender.tbl, main = 'gender')
pie(smoker.tbl, main = 'smoker')
par(op)

coplot(IAH ~ patient | gender)
coplot(IAH ~ patient | smoker, rows = 1)
coplot(IAH ~ patient | smoker + gender)

barplot(table(smoker[gender == "hombre"]))
barplot(table(smoker[gender == "mujer"]))

detach(df)

# correlations (linear relationship)
pairs(~ IAH + weight + height + age + cervical + BMI, data = df)
pairs(~ IAH + age + cervical + BMI, data = df)
op <- par(mfrow = c(2, 3))
plot(IAH ~ age + cervical + BMI, data = df)
plot(log(IAH) ~ age + cervical + BMI, data = df)
par(op)
op <- par(mfrow = c(1,2))
plot(IAH ~ gender + smoker)
par(op)

df_tmp <- 
  df %>% mutate(
    gender = as.numeric(gender),
    smoker = as.numeric(smoker),
    log_IAH = log(IAH)
  )

df_tmp$log_IAH[is.infinite(df_tmp$log_IAH)] <- min(is.finite(df_tmp$log_IAH))

M <- cor(subset(df_tmp, select = - patient))
corrplot(M, method = "pie", type = "lower", tl.srt = 45, tl.col = "darkblue")
corrplot.mixed(M, tl.cex = 0.7, tl.col = "black")

rm(df_tmp)
