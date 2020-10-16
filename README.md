# OSA-diagnosis
Basic study of the clinical data from various patients in order to ease 
the diagnosis of the Obstructive Sleep Apnea (OSA) disease.

OSA consists on sporadic obstruction of the breathing system at the 
neck while sleeping. In the data, the diagnosis is based on the number 
of obstructions per hour (on averge).

### ETL
The first step is to perform an **ETL**:

1. *Extract* the data from its sources
2. *Transform* and tidy the data (cleansing)
3. *Load* the data for storage

It's still necessary to manually study the data in order to gain 
insights about the relations between predictors and the data itself. This is called 
Exploratory Data Analysis (**EDA**) and paves the way for further 
analysis.

## Regression

It is used to make predictions based on data. It uses a function that takes the variables and gives a single value.
$$y = f(x_1, x_2, ..., x_n)$$

### Linear regression

The regression function takes a linear form.
In this project is mainly used for understanding the effect of each variable on the response, given the great simplicity of the linear model.
