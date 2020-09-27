########################################
# OSA data analysis using:
#         - Classification models
#
# We will try to classify extreme OSA cases
#   IAH <= 10 vs IAH >=30
#

rm(list=ls())

Input_file <- "OSA_DB_UPM.xlsx"

Output_file <- "OSA_extreme_male.xlsx"

Data_Directory <- "D:\\OSA_CaseStudy\\DATA\\"


# Using readxl package to read an Excel file
# Install the readxl package is nor already installed

library(readxl)

df_OSA <- read_excel(paste(Data_Directory, Input_file, sep = ""))

df_OSA = as.data.frame(df_OSA)
names(df_OSA)
dim(df_OSA)

## We will begin considering male subjets
### Male population
df_OSA_male=subset(df_OSA, Gender=="hombre")


########################################
# We add column to tag three clases:
#     Healthy (IAH <= 10)
#     Mild (10<IAH<30)
#     Severe (IAH >=30)

# We will use dplyr library (mutate operator)
library(dplyr)

df_OSA_male <- df_OSA_male %>%
          mutate(OSA = ifelse(IAH <= 10, "Healthy",
          ifelse(IAH>=30, "Severe", "Mild")))

df_OSA_male <- df_OSA_male %>% filter(OSA != "Mild")


# Define OSA as a factor!
  
df_OSA_male$OSA = factor(df_OSA_male$OSA)

# Add BMI column
df_OSA_male$BMI <-
  with(df_OSA_male, Weight / (Height/100.0)^2)

summary(df_OSA_male)

###### SAVE the data frame into an EXCEL file
#
# Output_file <- "OSA_extreme_male.xlsx"
#
#

library(writexl)

write_xlsx(df_OSA_male,
           paste(Data_Directory, Output_file, sep = ""))

##########################################################
####### BEFORE TESTING DIFFERENT Classification Models
#######
#######    try some EDA (Exploratory Data Analysis)

########################################################
### For example: the correlation among predictors and IAH

cor(df_OSA_male[,3:7])

# for examle a visualization
library(corrplot)

correlations = cor(df_OSA_male[,3:7])
corrplot(correlations, method="number")

############################################
##
## Some Plots and scatter plots per class
## using lattice (see ?lattice):
#         lattice add-on package is a powerful
#         and elegant high-level data
#         visualization system with an
#         emphasis on multivariate da

library(lattice)

# Each group in a separate mini plot
xyplot(BMI ~ Age | OSA, data = df_OSA_male)

xyplot(BMI ~ Age , 
       groups =  OSA, data = df_OSA_male,
       auto.key = list(corner = c(1, 1), cex = 0.7))


############################################
#### you can use ggplot2 for plotting
#### histograms of a dataframe by group

library(ggplot2)

ggplot(df_OSA_male, aes(x = BMI)) +
  geom_histogram(aes(color = OSA), fill = "white",
                 position = "identity", bins = 30, alpha = 0.1) +
  scale_color_manual(values = c("#00AF00", "#E7B800")) +
  scale_fill_manual(values = c("#00AF00", "#E7B800"))

#### USE These or other tools like this and
#### add your comments in your Half Term report

