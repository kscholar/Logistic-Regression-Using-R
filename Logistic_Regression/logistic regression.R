############# Source code for practicing the Logistic Regression #############


####Removing all the previous object####
rm(list=ls())

###### Setting the working directory and importing the dataset ######
setwd("C:/Users/acer/Desktop")
dataset=read.csv('Social_Network_Ads2.csv')
##initially the dataset contains  variables
## but we need to operate only "age", "Estmated Salary" and "purchased"
## So we will skip Variables with With Index 0,1,2
dataset = dataset[3:5]
## The present dataset only contains "age", "Estmated Salary" and "purchased"

###### Understanding The dataset ######
##Check the column names##
colnames(dataset)
##Checking the Datatypes##
str(dataset)

## Based on the column names of dataset it is found that there are 3 variables named as
## "Age", "Estimated salary" "Purchased"
## all the variables are similar and are INT type
## the purpose of this ML model will be to find whether the object is purchased or not

##Counting the No of unique values in Purchased variable##
unique((dataset$Purchased))
##There are two unique value in Purchased variable##
##   0 ----> Not Purchased
##   1 ----> Purchased

##########Data Preprocessing##########
#### Splitting the dataset into training set and test set ####
##calling Library CAtools for splitting Dataset
library(caTools)
set.seed(123)
##Creating the variable split 
split=sample.split(dataset$Purchased, SplitRatio = 0.75)
train_set = subset(dataset, split==TRUE)
test_set  = subset(dataset, split==FALSE)

##Feature Scaling
train_set[, 1:2]=scale(train_set[, 1:2])
test_set[,1:2] = scale(test_set[, 1:2])


########## Building the Logistic Regression Model ##########
##Bulding a classifier for training set##
classifier = glm(formula=Purchased ~ .,
                 family = binomial,
                 data=train_set)

##glm is Generalized Linear Model 
## formula = dependent variable
## family = binomial

##predicting the classifier on test Set##

prob_pred=predict(classifier, type='response',newdata=test_set[-3])
prob_pred
## Initially the classifier will predict in the form of probabilities
## and it needs to be converted into form of 0,1
## ifelse statement will be used to do so
##probability greater than 0.5 will give output 1 and less than 0.5 will give 0
##   0 ----> Not Purchased
##   1 ----> Purchased
y_pred= ifelse(prob_pred>0.5 ,1,0)
y_pred


## making the Confusion matrix ##
cm=table(test_set[,3],y_pred)
cm

#### Visualising the training set ####
library(ElemStatLearn)
set = train_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Training set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


# Visualising the Test set results
library(ElemStatLearn)
set = test_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Logistic Regression (Test set)',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
