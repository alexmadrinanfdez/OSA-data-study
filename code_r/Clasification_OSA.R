########################################
# Study the use of several Classification models
#         to classify extreme OSA cases
#               IAH <= 10 vs IAH >=30
#

rm(list=ls())

########################################
#
#         - load the data from

Input_file <- "OSA_extreme_male.xlsx"

Data_Directory <- "D:\\OSA_CaseStudy\\DATA\\"

# Using readxl package to read an Excel file
# Install the readxl package is nor already installed

library(readxl)

df_OSA_male <- read_excel(paste(Data_Directory, Input_file, sep = ""))

summary(df_OSA_male)

# Define OSA column as a factor for being used be
# classification models
df_OSA_male$OSA = factor(df_OSA_male$OSA)

# Using contrasts you can see how the levels of
# the factors will be coded when fitting the model
contrasts(df_OSA_male$OSA)

##################################################
#### Let's start trying LOGISTIC REGRESSION #######

glm.fit=glm(OSA~BMI+Age+Cervical,data=df_OSA_male,
            family=binomial)

# ... you can explore results following the ideas
#     in the text book

summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef


#####################################################
#
#    CLASSIFICATION ACCURACY
#
# If no data set is supplied to the predict() function, 
# then the probabilities are computed for the training
# data that was used to fit the logistic regression model.
# Here we have printed only the first ten probabilities. 

glm.probs=predict(glm.fit,type="response")
glm.probs[1:10]
contrasts(df_OSA_male$OSA)

df_OSA_male$IAH[1:10]

#######################################################
#
#   - Use the Probabilities to perform classification
#   - Obtaing a CONFUSSION MATRIX

# The first command creates a vector of 278 Healthy elements.
# The second line transforms to Up all of the elements 
# for which the predicted probability of Healthy exceeds 0.5.

### !!! NOTE that this depends on the result of the
###     assignment : see contrasts
glm.pred=rep("Healthy",278)
glm.pred[glm.probs>.5]="Severe"


# table() can be used to obtain a CONFUSSION MATRIX
table(glm.pred,df_OSA_male$OSA)

### UNDERSTAND THE DIFFERENT WAYS TO EXPLORE
### THE CONFUSION MATRIX

sum(df_OSA_male$OSA=='Healthy')
sum(df_OSA_male$OSA=='Severe')

# Correct Prediction
(97+103)/278
mean(glm.pred==df_OSA_male$OSA)

##### SOME IMPORTANT QUESTIONS:
#   - Can you understand the different types of errors?
#
#   - Could you "change" the results?
#     (think on the decission threshold, ROC, AUC curves, etc)


##### AS AN APPROACH TO ROC, DET COURVES ########
#####
#####  you can plot the histograms of model probabilities grouped
#####  by class

##  A simple way to do this is adding a new column to the
##  df_OSA_male with these probabilities

df_OSA_male$Probs <- glm.probs

############################################
#### As we already used in PrepareOSA R script
#### you can use ggplot2 for plotting
#### histograms of a dataframe by group

library(ggplot2)

ggplot(df_OSA_male, aes(x = Probs)) +
  geom_histogram(aes(color = OSA), fill = "white",
                 position = "identity", bins = 30, alpha = 0.1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"))

### Latter you can see how to plot ROC or DET curves
### using a library as CARET

###################################################
### THIS IS EVAL ON TRAINING DATA!!!
############################################

### ... you can predict NEW data

predict(glm.fit,newdata=data.frame(BMI=c(22,32),
                                   Age=c(30,60),
                                   Cervical=c(40,45)),type="response")

######################################################
##### BUT YOU SHOULD DESING an Experimental Setup
##### for Testing the validity of your model !
#####
#####    Let's consider two main ways for doinbg that:
#####
#####       A.- Using training (development) and testing data
#####
#####       B.- Using crossvalidation

####  As our data set is small we will use crossvalidation
####  (think and discuss about this)

# for example, take 200 randomly selected samples for training
train = sample(278,200)

df_OSA_male_train=df_OSA_male[train,]
df_OSA_male_test=df_OSA_male[-train,]


glm.fit=glm(OSA~BMI+Age+Cervical,data=df_OSA_male_train,
            family=binomial)

# Predic the TEST data
glm.probs_test=predict(glm.fit,df_OSA_male_test,type="response")

glm.pred_test=rep("Healthy",dim(df_OSA_male_test)[1])
glm.pred_test[glm.probs_test>.5]="Severe"

# table() can be used to obtain a CONFUSSION MATRIX
table(glm.pred_test,df_OSA_male_test$OSA)
mean(glm.pred_test==df_OSA_male_test$OSA)

# Predic the TRAIN data
glm.probs_train=predict(glm.fit,df_OSA_male_train,type="response")

glm.pred_train=rep("Healthy",dim(df_OSA_male_train)[1])
glm.pred_train[glm.probs_train>.5]="Severe"

# table() can be used to obtain a CONFUSSION MATRIX
table(glm.pred_train,df_OSA_male_train$OSA)
mean(glm.pred_train==df_OSA_male_train$OSA)

#######################################################
#
# TO Develop cross validation you can:
#
#    A.- develop your own "for loops" (more control)
#
#    B.- use existing ML packages:
#                   for example CARET , MLR in R
#                   or Scikit-Learn in Python


## IF NOT INSTALLED, INSTALL THE CARET Package
library(caret)


# Caret streamlines the process for creating predictive models.

# The package contains tools for:

#  data splitting
#  pre-processing
#  feature selection
#  model tuning using resampling
#  variable importance estimation
#  as well as other functionality.

######################################################
### We have followed:
###
### https://machinelearningmastery.com/how-to-estimate-model-accuracy-in-r-using-the-caret-package/
######################################################


# define an 80%/20% train/test split of the dataset
split=0.80
trainIndex <- createDataPartition(df_OSA_male$OSA, p=split, list=FALSE)

data_train <- df_OSA_male[ trainIndex,]
data_test <- df_OSA_male[-trainIndex,]

# train a naive bayes model from klaR package
# INSTALL KlaR if not available

library(klaR)
# klaR: Classification and Visualization
# Miscellaneous functions for classification and visualization, 
# e.g. regularized discriminant analysis, sknn() kernel-density
# naive Bayes, an interface to 'svmlight' and stepclass() wrapper
# variable... 

model <- NaiveBayes(OSA~BMI+Age+Cervical, data=data_train)

# make predictions on TEST data
predictions <- predict(model, data_test)
# summarize results
confusionMatrix(predictions$class, data_test$OSA)

# make predictions on TRAIN data
predictions_train <- predict(model, data_train)
# summarize results
confusionMatrix(predictions_train$class, data_train$OSA)



###########################################################
#####   CROSS VALIDATION
###########################################################

# define training control
train_control <- trainControl(method="cv", number=10,
                     summaryFunction=twoClassSummary, 
                     classProbs=T,
                     savePredictions = T)

# train the model
model <- train(OSA~BMI+Age+Cervical, data=df_OSA_male,
               trControl=train_control, method="glm",
               metric = 'ROC'
               )
# summarize results
print(model)

# See final model
model$finalModel

#### PLOT the ROC cource

model$pred$obs

#### PLOTTING a ROC curve #################################
# Create a data frame of true outcomes and probabilities
# (NOTE this will be for the last cv fold)

for_lift <- data.frame(Class = model$pred$obs,  glm = model$pred$Severe)

# Use a lift object using caret function lift
lift_obj <- lift(Class ~ glm, data = for_lift, class = "Severe")


# Use ggplot to Plot ROC
ggplot(lift_obj$data) +
  geom_line(aes(1 - Sp, Sn , color = liftModelVar)) +
  scale_color_discrete(guide = guide_legend(title = "method"))

### Remember:
# Sensitivity = TP/(TP + FN) 
# Specicity = TN/(TN + FP)
# 1 - Sp (False Positive), Sn (True Positive)


######### Let's try KNN with hyperparameter selection (value of k)
#         using grid search (consider also rando, bayesian... optimization)
# 
#   For example:  k= 3,5,7,...15  (expand.grid creates a data frame)
k_values <- expand.grid(k = seq(3, 15, 2)) 

# Train control CV k´fold=10
train_control <- trainControl(method='cv',
                        number = 10)

# Training the model for hyperparameter selection using cross validation
knn.model <- train(OSA~BMI+Age+Cervical,
                   data=df_OSA_male,
                   method = 'knn',
                   tuneGrid = k_values,
                   trControl = train_control)
knn.model

####################################################
##### Try with more powerfull models as:
##### Random Forest, Xgboost

# define training control
RF_train_control <- trainControl(method="cv", number=10,
                              savePredictions = TRUE, 
                              classProbs = TRUE,
                              verboseIter = TRUE)

##################################################
#### learn the meaning of preProcess center & scale

randomForest.model = train(OSA~BMI+Age+Cervical,
                        data=df_OSA_male,
                        method = "rf", 
                        trControl = RF_train_control, 
                        preProcess = c("center","scale"), 
                        ntree = 400)
randomForest.model

randomForest.model$finalModel

###################################################################
#### EACH model has its specificities (learn, use and report them)
####
####    for example, RF can give information on the variables importance


randomForest.model$finalModel$importance


############ XGBoost ###########################################
####
####    XGBoots is a powerfull model
####    However it has a lot of tunning, try following a
####    proper methodology.
####         ...you can see an example:
####         https://www.kaggle.com/pelkoja/visual-xgboost-tuning-with-caret)
