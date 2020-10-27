# Exploratory Data Analysis

rm(list = ls())   # clear working space

library(readxl)   # read Excel files
library(dplyr)    # a grammar of data manipulation
library(corrplot) # visualization of a correlation matrix
library(lattice)  # Trellis graphics for R
library(ggplot2)  # create elegant data visualisations using the grammar of graphics

file <- 'DB.xlsx'
directory <- '../data'

df <- read_excel(paste(directory, file, sep = "/"))
df <- as.data.frame(df)
df <- df %>% mutate(
  gender = as.factor(gender),
  smoker = as.factor(smoker),
  snorer = as.factor(snorer),
  diagnosis = as.factor(diagnosis)
) %>% select(patient, AHI, diagnosis, everything())

dim(df)
glimpse(df)
summary(df)

attach(df) # database is attached to the R search path

# aproximated distributions
op <- par(mfrow = c(2, 3))
hist(AHI)
hist(weight)
hist(height)
hist(age)
hist(neck)
hist(BMI, breaks = 20)
par(op)

# non-normal distributions
op <- par(mfcol = c(2, 2))
qqplot(
  AHI, rexp(n = length(AHI), rate = 1 / mean(AHI)),
  ylab = 'exponential distribution'
)
hist(log1p(AHI), breaks = 30) # log1p(x) computes log(1+x)
qqplot(
  BMI, rf(n = length(BMI), df1 = length(BMI), df2 = length(BMI), ncp = 2),
  ylab = 'F distribution (ncp = 2)'
)
hist(log(BMI), breaks = 30)
par(op)

# normal distributions
op <- par(mfrow = c(2, 3), lty = 2)
qqnorm(log(AHI)[is.finite(log(AHI))], main = 'AHI') # log(0) = -Inf
qqline(log(AHI), col = "darkgoldenrod")
qqnorm(weight, main = 'Weight')
qqline(weight, col = "darkgoldenrod")
qqnorm(height, main = 'Height')
qqline(height, col = "darkgoldenrod")
qqnorm(age, main = 'Age')
qqline(age, col = "darkgoldenrod")
qqnorm(neck, main = 'neck')
qqline(neck, col = "darkgoldenrod")
qqnorm(log(BMI), main = 'BMI')
qqline(log(BMI), col = "darkgoldenrod")
par(op)

# analyze factors differently
op <- par(mfrow = c(1, 4))
pie(table(gender), main = 'gender')
pie(table(smoker), main = 'smoker')
pie(table(snorer), main = 'snorer')
pie(table(diagnosis), main = 'diagnosis')
par(op)

op <- par(mfcol = c(2, 3))
barplot(table(smoker[gender == "male"]), main = 'smoker M')
barplot(table(smoker[gender == "female"]), main = 'smoker F')
barplot(table(snorer[gender == "male"]), main = 'snorer M')
barplot(table(snorer[gender == "female"]), main = 'snorer F')
barplot(table(diagnosis[gender == "male"]), main = 'diagnosis M')
barplot(table(diagnosis[gender == "female"]), main = 'diagnosis F')
par(op)

detach(df) # database is detached from the R search path

ggplot(data = df, mapping = aes(x = gender, y = diagnosis)) + 
  geom_count()

coplot(AHI ~ patient | smoker + gender, data = df, pch = 20, col = "#009E73")
coplot(AHI ~ patient | snorer + gender, data = df, pch = 20, col = "#009E73")

xyplot(AHI ~ weight | gender, data = df)
xyplot(AHI ~ height | gender, data = df)
xyplot(AHI ~ age | gender, data = df)
xyplot(AHI ~ neck | gender, data = df)
xyplot(AHI ~ BMI | gender, data = df)
xyplot(
  x = weight ~ height, data = df, groups = gender,
  pch = 19, alpha = 0.2,
  auto.key = list(corner = c(1, 0), cex = 0.7, points = FALSE, rectangles = TRUE)
)

xyplot(BMI ~ age | diagnosis, data = df)
xyplot(neck ~ BMI | diagnosis, data = df)
xyplot(
  x = neck ~ BMI, groups =  diagnosis, data = df,
  pch = c(0, 4, 4, 0), alpha = c(0.75, 0.25, 0.25, 0.75),
  auto.key = list(columns = 4, points = FALSE, lines = TRUE)
)
xyplot(
  x = neck ~ BMI | gender, groups =  diagnosis, data = df,
  pch = c(0, 4, 4, 0), alpha = c(0.75, 0.25, 0.25, 0.75),
  auto.key = list(columns = 4, points = FALSE, lines = TRUE)
)

# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(
#     mapping = aes(<MAPPINGS>),
#     stat = <STAT>, 
#     position = <POSITION>
#   ) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>

ggplot(mapping = aes(x = age), data = df) +
  geom_histogram(
    aes(color = diagnosis, fill = diagnosis),
    position = "identity",
    alpha = 0.1
  )

df %>% 
  filter(diagnosis == "normal" | diagnosis == "severe") %>%
  ggplot(mapping = aes(x = age), data = df) +
  geom_histogram(
    aes(color = diagnosis, fill = diagnosis),
    position = "identity",
    alpha = 0.1
  ) +
  scale_color_manual(values = c("#00AF00", "#E7B800")) +
  scale_fill_manual(values = c("#00AF00", "#E7B800"))
df %>% 
  filter(diagnosis == "normal" | diagnosis == "severe") %>%
  ggplot(mapping = aes(x = neck), data = df) +
    geom_histogram(
      aes(color = diagnosis, fill = diagnosis),
      position = "identity",
      alpha = 0.1
    ) +
  scale_color_manual(values = c("#00AF00", "#E7B800")) +
  scale_fill_manual(values = c("#00AF00", "#E7B800"))
df %>% 
  filter(diagnosis == "normal" | diagnosis == "severe") %>%
  ggplot(mapping = aes(x = BMI), data = df) +
    geom_histogram(
      aes(color = diagnosis, fill = diagnosis),
      position = "identity",
      alpha = 0.1
    ) +
    scale_color_manual(values = c("#00AF00", "#E7B800")) +
    scale_fill_manual(values = c("#00AF00", "#E7B800"))
df %>% 
  filter(diagnosis == "normal" | diagnosis == "severe") %>%
  ggplot(mapping = aes(x = gender), df) +
    geom_histogram(
      aes(color = diagnosis, fill = diagnosis),
      position = "identity",
      stat = "count",
      alpha = 0.1
    ) +
    scale_color_manual(values = c("#00AF00", "#E7B800")) +
    scale_fill_manual(values = c("#00AF00", "#E7B800"))

# correlations (linear relationship)
pairs(
  ~ AHI + weight + height + age + neck + BMI, data = df,
  pch = 20, col = "darkblue"
)
pairs(
  ~ AHI + age + neck + BMI, data = df,
  pch = 20, col = "darkblue"
)
op <- par(mfrow = c(2, 3))
plot(
  AHI ~ age + neck + BMI, data = df,
  pch = 19, col = palette.colors(1, alpha = 0.2)
)
plot(
  log1p(AHI) ~ age + neck + BMI, data = df,
  pch = 19, col = palette.colors(1, alpha = 0.2)
)
par(op)
op <- par(mfrow = c(1, 3))
plot(AHI ~ gender + smoker + snorer, data = df)
par(op)

df.num <- df %>% mutate(
  gender = as.numeric(gender),
  smoker = as.numeric(smoker),
  snorer = as.numeric(snorer),
  log_AHI = log1p(AHI)
) %>% select(- c(patient, diagnosis))

M <- cor(df.num)

corrplot(
  corr = M, method = "pie", type = "upper",
  tl.pos = "d", tl.col = "black", tl.cex = 0.8
)
corrplot(
  corr = M, type = "lower", method = "color",
  number.cex = .7, add = TRUE,
  addCoef.col = "white",      # add coefficient of correlation
  tl.pos = "n", cl.pos = "n", # don't show text and color labels again
  # hide correlation coefficient on the principal diagonal
  diag = FALSE           
)
