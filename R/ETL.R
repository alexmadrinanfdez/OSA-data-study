# simple ETL process on a single Excel file

# clear working space
rm(list = ls())

library(readxl)  # read Excel files
library(dplyr)   # a grammar of data manipulation
library(visdat)  # preliminary visualisation of data
library(naniar)  # data structures, summaries, and visualisations for missing data
library(tidyr)   # tidy messy data
library(writexl) # export data frames to Excel '.xlsx' format
library(stringr) # simple, consistent wrappers for common string operations

i_file <- 'Info_BDApnea_QuironMalaga.xlsx'
o_file <- 'DB.xlsx'
directory <- '../data'

## extract ##

df_origin <- read_excel(paste(directory, i_file, sep = "/"))

class(df_origin) # not ONLY a data.frame
df_origin <- as.data.frame(df_origin)

## transform (cleansing) ##

glimpse(df_origin)

# based on the BANG risk indicators:
#   - BMI > 35
#   - Age > 50 years
#   - Neck circumference > 40 cm
#   - Gender male

# keep only meaningful predictors for diagnosis
df_tmp <- 
  df_origin %>% 
    select(
      patient = Patient,
      gender = Gender,
      IAH,
      weight = Peso,
      height = Talla,
      age = Edad,
      cervical = PerCervical,
      smoker = Fumador, # additional predictor
      snorer = Roncador # additional predictor
      )

vis_dat(df_tmp) # at-a-glance ggplot object of what is inside a dataframe
# change all -1 values for NA
df_tmp <- df_tmp %>% replace_with_na_all(condition = ~.x == -1)
# remove anything that is not a number in variable weight
df_tmp <- df_tmp %>% mutate(weight = str_remove_all(weight, "[^[:digit:]]"))
df_tmp <- df_tmp %>% mutate(weight = as.numeric(weight))
# unify responses in factors
# str_replace(string, pattern, replacement)
# df_tmp <- df_tmp %>% mutate(smoker = str_replace(smoker, 'poco', 'si'))
df_tmp <- df_tmp %>% mutate(snorer = str_replace(snorer, 'no con CPAD','CPAP'))
df_tmp <- df_tmp %>% mutate(snorer = str_replace(snorer, 'si sin CPAP', 'CPAP'))
df_tmp <- df_tmp %>% mutate(snorer = str_replace(snorer, 'poco', 'si'))

# df_tmp <- df_tmp %>% replace_with_na(replace = list(smoker = "ns"))

df <- df_tmp %>% drop_na()                                      # drop rows containing missing values

df <- 
  df %>% 
    mutate(
      patient = str_trunc(patient, width = 3, side = "left", ellipsis = ""),
      BMI = weight / (height / 100)^2, # new column BMI = [kg]/[m]^2
      )

## load ##

write_xlsx(df,
           paste(directory, o_file, sep = "/" ))