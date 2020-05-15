#Preamble ----
# Author:       Mike Alonzo
#
# Date:        5/14/2020
# Origin Date:  3/4/2020
# Purpose:      RF classification of points
# R version:    3.6.2
# Input file:   cal/val data as some sort of table with true class and input variables as columns
# Output files: None, per se. Classification statistics including OA, kapp, confusion matrix.
# Notes:        

#
#############################################################################################
library(randomForest)
#you may not need this if you are adding a csv or something
library(readxl)

#read in data (change to suit your format) (USER INPUT)
data_full_sheet<-read_excel("F:/ENVS_RS Dropbox/Projects/sesync/zinda/zinda_2020/pts_201904_with_metric_image_values.xlsx")
#break up your input sheet into data frame that is just the input variables
data_cols <- c(18:27,31:32, 34:38) #USER INPUT
data1<-data_full_sheet[,data_cols]
#grab the reference code column (i.e., the thing you're trying to classify)
ref_code<-as.factor(data_full_sheet$Name) #USER INPUT

# Split into Train and Validation sets
# Training Set : Validation Set = 70 : 30 (random)
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
ref_code_train <- ref_code[train]
ref_code_valid <- ref_code[-train]
summary(TrainSet)
summary(ValidSet)


# Create a Random Forest model with default parameters
#here ref_code is the species (for example) code variable that is to be classified
model1 <- randomForest(ref_code_train ~ ., data = TrainSet, importance = TRUE)
model1

# Predicting on train set (this should be ultra high accruacy and not necessarily worth anything)
# predTrain <- predict(model1, TrainSet, type = "class")
# # Checking classification accuracy
# table(predTrain, ref_code_train)  

# Predicting on Validation set (this is much more useful as output)
predValid <- predict(model1, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ref_code_valid)                    

#check variable importance
importance(model1)        
varImpPlot(model1)  

# Using For loop to identify the right mtry for model where mtry is the number of variables to include in the decision trees
a=c()
#set the highest number of variables you'd like to consider
num_vars <- 17 #USER INPUT
for (i in 1:num_vars) {
  model3 <- randomForest(ref_code_train ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = "class")
  a[i] = mean(predValid == ref_code_valid)
}

a

plot(1:num_vars,a)


#Now you may want to create a new model based on what you learned about best mtry 
#to maximize usage of data, I'm including the full dataset here because, so far, I've found that the OOB estimates are adquately
#similar to the predictions on the validation subset
#here is where you choose your best mtry
mtry_best <- 5 #USER INPUT
model_updated <- randomForest(ref_code ~ ., data = data1, importance = TRUE, mtry=mtry_best)
model_updated



