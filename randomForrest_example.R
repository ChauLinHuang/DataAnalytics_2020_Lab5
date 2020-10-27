# Random Forest Example
# This example is from R-Bloggers: https://www.r-bloggers.com/2018/01/how-to-implement-random-forests-in-r/

# Car Acceptability (Car conditions) dataset from UCI ML Repository
# Data Source: https://archive.ics.uci.edu/ml/machine-learning-databases/car/
# Download the car.data from here: https://archive.ics.uci.edu/ml/machine-learning-databases/car/


rm(list = ls())

if (!require('randomForest')) install.packages('randomForest')
library(randomForest)

# Loding the dataset
# dataFile_dir <- file.choose()
dataFile_dir <- '/home/charly_huang/Documents/RPI_ITWS_coursework/Fall_2020/Data_Analytics/Group 3/Lab_5/datasets/car.data'
data1 <- read.csv(dataFile_dir, header = TRUE)

head(data1)
str(data1)

# Adding the column names
colnames(data1) <- c('BuyingPrice', 'Maintenance', 'NumDoors', 'NumPersons', 'BootSpace', 'Safety', 'Condition')
head(data1)
str(data1)

# Let's take a look at the levels of the condition column
# Conditions has 4 levels: "acc" "good" "uacc" "vgood"
levels(data1$Condition)
summary(data1)

# Creating the "training dataset" and "validation dataset"
# we will randomly choose 70% (0.7) of the data points for training and 30% (0.3) for validation
# First we need to set the seed
set.seed(100)
train <- sample( nrow(data1), 0.7 * nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
summary(TrainSet)
summary(ValidSet)

help(randomForest) # the Random Forest documentation

# Random Forest model with default parameters
model1 <- randomForest(Condition ~ ., data = TrainSet, importance = TRUE)
model1
# By default, the number of trees is 500 and number of variables tried at each split is 2 in this case

# Fine tuning the parameters of the RandomForest model
# we have increased the mtry from 2 to 6
# mtry = number of variables randomly sampled as candidates at each split
# Note that the default values are different for
# classification (sqrt(p) where p is the number of variables in x) and regression (p/3)
model2 <- randomForest(Condition ~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE )
model2

# First, we will conduct prediction using training set after that we will do predictiion using validation set
# Reason we are doing this is to see the difference in results
# Predicting on the training datasets
predTrain <- predict(model2, TrainSet, type = 'class')
# We can use table() to check the classification accuracy
table(predTrain, TrainSet$Condition)
# Predicting on validation dataset
predValid <- predict(model2, ValidSet, type = 'class')
table(predValid, ValidSet$Condition)

# We can also use importance() function to check important variables
# The below functions show the drop in mean accuracy for each of the variables
# To check the important variables
importance(model2)
varImpPlot(model2)

# Now we will use 'for' loop and check for different values of mtry
# using a for-loop to identify the right 'mtry' for the model
a = c()
i = 5
for (i in 3:8) {
  model3 <- randomForest(Condition ~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(model3, ValidSet, type = 'class')
  a[i - 2] = mean(predValid == ValidSet$Condition)
}
a
plot(3:8, a)
# Warning: In randomForest.default(m, y, ...) :
# invalid mtry: reset to within valid range

# Now we have seen the implementation of random forest and understood the importance of the model
# let's compare this model with decision tree and see how decision trees does in comparison
# to random forest.
# Compare with Decision Tree

if (!require('caret')) install.packages('caret', dependencies = TRUE)

library(rpart)
library(caret)
library(e1071)

# We will compare random forest against decision tree
model1_dt <- train(Condition ~ ., data = TrainSet, method = 'rpart')
model1_dt_1 = predict(model_dt, data = TrainSet)
table(model_dt_1, TrainSet$Condition)
mean(model_dt_1 == TrainSet$Condition)
table(model_dt_1, TrainSet$Condition)
mean(model_dt_1 == TrainSet$Condition)

# On the training dataset, the accuracy is around 79.4% and there is a lot of misclassification
# Now, look at the validation dataset

# Run on validation set
model_dt_vs = predict(model_dt, newdata = ValidSet)
table(model_dt_vs, ValidSet$Condition)
mean(model_dt_vs == ValidSet$Condition)