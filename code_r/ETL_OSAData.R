########################################
# OSA Use Case
#
# Simple ETL process on a single Excel file
#

# Clear the working space
rm(list=ls())

Input_file <- "Info_BDApnea_QuironMalaga.xlsx"
Output_file <- "OSA_DB_UPM.xlsx"

Data_Directory <- "D:\\OSA_CaseStudy\\DATA\\"

# Using readxl package to read an Excel file
# Install the readxl package is nor already installed

library(readxl)

df_tmp <- read_excel(paste(Data_Directory, Input_file, sep = ""))

typeof(df_tmp)
is.data.frame(df_tmp)

### NOTE: ##############################################
# df_tmp is NOT only a data frame!
# use as.data.frame to avoid later problems
class(df_tmp)
df_tmp = as.data.frame(df_tmp)
class(df_tmp)

######## PREPARING THE DATA FRAME 
# Select only those columns (variables) we are going
# to work with:
# Patient, Gender, IAH, Peso, Talla, Edad, PerCervical

# You can work with dplyr
# for using select(df_tmp, Patient, Gender,...)
# https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html
# dplyr's basic set of tools to apply on data frames

library(dplyr)

df_tmp1 <- select(df_tmp, Patient, Gender, IAH, Peso, Talla, Edad, PerCervical)


######## HOW TO MANAGE Non Available values !!!
# To change -1 values for NA in all columns (i.e. variables)
# you can install package naniar
# load it
#    note: you can also see
#          https://cran.r-project.org/web/packages/naniar/vignettes/getting-started-w-naniar.html


# BUT first you can visualize the issue with chr, num and NA
# using visdat

library(visdat)
vis_dat(df_tmp1)

# BEFORE CONTINUING EXECUTING NEXT lines of code
#        BE SURE TO FIX the Excel removing comments in Peso

##### QUESTIONS: #############################################

### HOW MANY PATIENTS?

### HOW MANY MALE /FEMALE



library(naniar)

# Now change -1 values for NA in all columns (i.e. variables)
df_tmp2 <- replace_with_na_all(df_tmp1,condition = ~.x == -1)

# Finally remove (drop out) all rows containing a NA
# at least one column

# You can use tidyr
# https://blog.rstudio.com/2014/07/22/introducing-tidyr/
# tidyr is a package that makes it easy to "tidy" 
# your data.
#
# Tidy data is data that's easy to work with:
# it's easy to munge (with dplyr), visualise (with ggplot2 or ggvis) and model (with R's hundreds of modelling packages). The two most important properties of tidy data are:
#
# Each column is a variable.
#
# Each row is an observation.

library(tidyr)

df_final <- df_tmp2 %>% drop_na()

##### QUESTIONS: #############################################
#####   AFTER CLEANING:
#####
#####       HOW MANY PATIENTS?
#####       HOW MANY MALE /FEMALE?


######### SAVING CLEAN DATA ##################################
# Write the clean data into Output_file
# you can install writexl package

library(writexl)

# You can change the names of the columns

df_final <- df_final %>% rename(Weight = Peso,
                                Height = Talla,
                                Age = Edad,
                                Cervical = PerCervical) 

vis_dat(df_final)

write_xlsx(df_final,
           paste(Data_Directory, Output_file, sep = ""))

