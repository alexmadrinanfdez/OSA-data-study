# simple ETL process on a single Excel file

rm(list = ls())  # clear working space

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

data <- read_excel(paste(directory, i_file, sep = "/"))

class(data) # not ONLY a data.frame
data <- as.data.frame(data)

## transform (cleansing) ##

glimpse(data)

# based on the BANG risk indicators:
#   - BMI > 35
#   - Age > 50 years
#   - Neck circumference > 40 cm
#   - Gender male

# keep only meaningful predictors for diagnosis
df <- data %>% select(
  patient = Patient,
  gender = Gender,
  AHI = IAH,
  weight = Peso,
  height = Talla,
  age = Edad,
  neck = PerCervical,
  smoker = Fumador, # additional predictor
  snorer = Roncador # additional predictor
)

vis_dat(df) # at-a-glance ggplot object of what is inside a dataframe

# change all -1 values for NA
df <- df %>% replace_with_na_all(condition = ~.x == -1)
# remove anything that is not a number in variable weight
df <- df %>% mutate(
  weight = str_remove_all(weight, "[^[:digit:]]"),
  weight = as.numeric(weight)
)
# str_replace(string, pattern, replacement)
# unify responses in factors
# df <- df %>% mutate(smoker = str_replace(smoker, 'poco', 'si'))
df <- df %>% mutate(
  snorer = str_replace(snorer, 'no con CPAD','CPAP'),
  snorer = str_replace(snorer, 'si sin CPAP', 'CPAP'),
  snorer = str_replace(snorer, 'poco', 'si')
)
# translate data in factors
df <- df %>% mutate(
  gender = str_replace(gender, 'hombre', 'male'),
  gender = str_replace(gender, 'mujer', 'female'),
  smoker = str_replace(smoker, 'antiguo', 'old'),
  smoker = str_replace(smoker, 'ns', 'na'),
  smoker = str_replace(smoker, 'poco', 'little'),
  smoker = str_replace(smoker, 'si', 'yes'),
  snorer = str_replace(snorer, 'ns', 'na'),
  snorer = str_replace(snorer, 'si', 'yes')
)

# df <- df %>% replace_with_na(replace = list(smoker = "ns"))

df <- df %>% drop_na() # drop rows containing missing values

df <- df %>% mutate(
  patient = str_trunc(patient, width = 3, side = "left", ellipsis = ""),
  BMI = weight / (height / 100)^2, # new column BMI [kg / m^2]
)

## load ##

write_xlsx(df,
           paste(directory, o_file, sep = "/" ))
