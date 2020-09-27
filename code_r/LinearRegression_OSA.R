########################################
# OSA data analysis using:
#         - linear regression models
#

Input_file <- "OSA_DB_UPM.xlsx"

Data_Directory <- "D:\\OSA_CaseStudy\\DATA\\"

# Using readxl package to read an Excel file
# Install the readxl package is nor already installed

library(readxl)

df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))

df_OSA = as.data.frame(df_OSA)
names(df_OSA)
dim(df_OSA)

## Describe the Database

# First define Gender as a factor!

df_OSA$Gender = factor(df_OSA$Gender)
summary(df_OSA)

# See relations between variables
attach(df_OSA)
pairs(~ IAH + Gender + Weight + Height + Cervical + Age)

## PLOT Correlation Matrix

# FIRST
# install corrplot and then load it
library(corrplot)
# back to as.numeric for including it..

df_OSA_C=df_OSA

df_OSA_C$Gender = as.numeric(df_OSA_C$Gender)
M <- cor(subset(df_OSA_C, select = - Patient))
corrplot(M, method="number")
corrplot(M, method="circle")

# Study the use of Simple and Multiple LR models

lm.fit=lm(IAH~Weight+Height+Cervical+Age+Gender)

summary(lm.fit)

# Study independently male and female populations

### Male population
df_OSA_male=subset(df_OSA_C, Gender==1)

# Another way
# df_OSA_male = df_OSA_C[df_OSA_C$Gender == 1, ]

names(df_OSA_male)
attach(df_OSA_male)

lm_male.fit=lm(IAH~Height+Cervical+Age+Weight)

summary(lm_male.fit)

############ Female population ################

df_OSA_female=subset(df_OSA_C, Gender==2)

# Another way
# df_OSA_female = df_OSA_C[df_OSA_C$Gender == 2, ]

names(df_OSA_female)
attach(df_OSA_female)

lm_female.fit=lm(IAH~Height+Cervical+Age+Weight)

summary(lm_female.fit)

# BMI add a column
df_OSA_C$BMI <- with(df_OSA_C, Weight / (Height/100.0)^2)

attach(df_OSA_C)

lm.fit=lm(IAH~BMI+Cervical+Age)

summary(lm.fit)


########################################
########################################
####   NEXT STEPS:
####
####  You can try:
####        - Regularization
####        - Feature selection
####        - other Regression models
####
####  BUT you can wait to see Classification Script
####           and use regression with some libraries as CARET
####


####

