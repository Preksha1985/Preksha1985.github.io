## Module - Predictive Analysis II - Support Vector Machines-Assignment
## Name - Preksha Tiwari
## Instructions - Set the working directory path , Unzip datafiles and place in the same path.

############################ HandWritten Digit Recognition problem ######################################
# 1. Objective
# 2. Data Understanding
# 3. Data Cleaning & Preparation
# 4. Linear Model Building and Tunning 
# 5. Radial Model Building and Tunning 
# 6. Conclusion

######################################## 1. Objective ###################################################

# Our objective is to develop a model using Support Vector Machine which should correctly classify 
# the handwritten digits based on the pixel values given as features.
# The below code takes almost ~35 minutes to complete using Intel CORE i5 processor.

########################################################################################################

# Loading Libraries 

#install.packages("caret")
#install.packages("kernlab")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("caTools")

library(ggplot2)
library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(gridExtra)
library(caTools)

remove(list=ls())
suppressWarnings(library(RODBC))
set.seed(100)

###################################### 2.Data Understanding ###############################################

# we use the MNIST data which is a large database of handwritten digits 
# We have provided two datasets - mnist_train and mnist_test
# mnist_train dataset has 60,000 observations and 784 features and one "Digit" label column 
# mnist_test dataset has 10,000 observations with 784 features and one "Digit" label column
# All the dependent variables are having pixel values from 0 to 255

####################################################################################################

## read.csv() function is used to read given csv file into a data frames that it creates.
## Argu-stringAsFactor is set as False so strings in a data frame is treated as just string not as a factor. 

SVM_train_rec <- read.csv("mnist_train.csv",stringsAsFactors = F,header = F)
SVM_test_rec <- read.csv("mnist_test.csv",stringsAsFactors = F,header = F)

#2.1) Check the dimension of the dataframe
dim(SVM_train_rec) #60000 785
dim(SVM_test_rec) #10000 785

#2.2) Check the structure of features and all are interger as expected by SVM model
str(SVM_train_rec) 
str(SVM_test_rec)

###################################### 3.Data Cleaning & Prepration ###############################################

#3.1) Removed NA values from dataset using colSums() function

SVM_train_rec <- SVM_train_rec[colSums(!is.na(SVM_train_rec))!=0] # no mising values
SVM_test_rec <- SVM_test_rec[colSums(!is.na(SVM_test_rec))!=0] # no missing values

#3.2)  Checked for duplicate rows and removed if any using duplicated() function

sum(duplicated(SVM_train_rec)) # no duplicated rows
sum(duplicated(SVM_test_rec)) # no duplicated rows

#3.3) Rename the independent col name (datafiles have no column names) 

colnames(SVM_train_rec)[1] <- "Digit"
colnames(SVM_test_rec)[1] <- "Digit"

#3.4) Convert independent variable into factors

SVM_train_rec$Digit <- as.factor(SVM_train_rec$Digit)
SVM_test_rec$Digit <- as.factor(SVM_test_rec$Digit)

head(sapply(SVM_train_rec[1,], class))
head(sapply(SVM_test_rec[1,], class))

#3.5) Sampling training dataset as computation time would be very high for such a large database
# Training_rec -> I have taken 15% of entire train dataset (60,000) for training which is ~9005  
# Validation_recs -> I have prepared two validation data sets for the purpose of hypertunning and for this 
# taken 2% of remaining dataset (50995) which is ~1000 each  so that computation time should be less 

trainIndex <- createDataPartition(SVM_train_rec$Digit, p = 0.15, list = FALSE, times = 1)
training_rec <- SVM_train_rec[trainIndex,] #9005
validating <- SVM_train_rec[-trainIndex,] #50995
validIndex1 <- createDataPartition(validating$Digit, p = 0.02, list = FALSE, times = 1) #1009
validate1_rec <- validating[validIndex1,]
validatingRem <- validating[-validIndex1,]
validIndex2 <- createDataPartition(validatingRem$Digit, p = 0.02, list = FALSE, times = 1) #1006
validate2_rec <- validatingRem[validIndex2,]

#3.6) Scaling data 
# I have used max pixel value for generalization of data as some of digit's pixel values goes up to 255 and for other it was  100 
max(training_rec[ ,2:ncol(training_rec)]) # max pixel value is 255
training_rec[ , 2:ncol(training_rec)] <- training_rec[ , 2:ncol(training_rec)]/255
validate1_rec[ , 2:ncol(validate1_rec)] <- validate1_rec[ , 2:ncol(validate1_rec)]/255
validate2_rec[ , 2:ncol(validate2_rec)] <- validate2_rec[ , 2:ncol(validate2_rec)]/255
SVM_test_rec[ , 2:ncol(SVM_test_rec)] <- SVM_test_rec[ , 2:ncol(SVM_test_rec)]/255

#3.7) Exploratory Data Analysis
## Average Intensity Plot for each digits

training_rec$intensity <- apply(training_rec[,-1], 1, mean) #Gives the mean of each row in train
intbylabel <- aggregate (training_rec$intensity, by = list(training_rec$Digit), FUN = mean)
SVM_test_rec$intensity <- apply(SVM_test_rec[,-1], 1, mean) #gives the mean of each row in train
intbylabeltest <- aggregate (SVM_test_rec$intensity, by = list(SVM_test_rec$Digit), FUN = mean)

ggplot(data=intbylabel, aes(x=Group.1, y = x)) + geom_bar(stat="identity") + theme_classic() + 
  labs(y= "Avg Intensity",x="Digit label",title = "Digit Intensity plot in Training set") 

ggplot(data=intbylabeltest, aes(x=Group.1, y = x)) + geom_bar(stat="identity") + theme_classic() + 
  labs(y= "Avg Intensity",x="Digit label",title = "Digit Intensity plot in Testing set") 
# we observed that average intensity is given same in both the data set that shows that intensity preserved while sampling

## Distribution of digits in given datasets (train and test) and sample dataset.  
ggplot(SVM_train_rec, aes(x = Digit, y = (..count..)/sum(..count..))) + geom_bar() + theme_light() +
  labs(y = "Relative frequency", title = "mnist train dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))
ggplot(training_rec, aes(x = Digit, y = (..count..)/sum(..count..))) + geom_bar() + theme_light() +
  labs(y = "Relative frequency", title = "Sample train dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))
ggplot(SVM_test_rec, aes(x = Digit, y = (..count..)/sum(..count..))) + geom_bar() + theme_light() +
  labs(y = "Relative frequency", title = "mnist Test dataset") + 
  scale_y_continuous(labels=scales::percent, limits = c(0 , 0.15)) +
  geom_text(stat = "count", 
            aes(label = scales:: percent((..count..)/sum(..count..)), vjust = -1))
# Similar frequencies of digits has been observed for given datasets (training and test) along with sample dataset.  

############################## 4.Linear Model Building and Tunning #################################

Model1_linear <- ksvm(Digit~ ., data = training_rec, scale = FALSE, kernel = "vanilladot")
print(Model1_linear)
Eval1_linear<- predict(Model1_linear, SVM_test_rec)
confusionMatrix(Eval1_linear,SVM_test_rec$Digit)
# Observations:
# Cost=1, Overall accuracy of 91.56%
# Sensitivity  > 84.50%
# Accuracy of Digits 5,8 and 9 are lower than the other digits.We need to tune these further

###########################################################################################################
# Hyperparameter tuning and Cross Validation  - Linear - SVM 

# Here are the steps to tune the hyperparameter(i.e C) in Linear SVM model 
#i) We will use the train function from caret package to perform crossvalidation 
#ii) and then making a grid of c(cost) values to perform 5-fold cross validation on validatation dataset
#iii) Print and plot the output to get the highest accuracy and value of best tuned C
#iv) We will repeat these stes in both the validation set to double check and fine tune the cost value
# So here I am taking a 2 subsample of ~1000 records each and apply a wide range of C so in this way the computational time is less
###########################################################################################################

trainControl <- trainControl(method="cv", number=5)
metric <- "Accuracy"
grid1 <- expand.grid(C=seq(1, 5, by=1))
#Validation dataset 1
fit11.svm <- train(Digit~., data=validate1_rec, method="svmLinear", metric=metric, 
                  tuneGrid=grid1, trControl=trainControl)
print(fit11.svm)
plot(fit11.svm)
#Validation dataset 2
fit12.svm <- train(Digit~., data=validate2_rec, method="svmLinear", metric=metric, 
                  tuneGrid=grid1, trControl=trainControl)
print(fit12.svm)
plot(fit12.svm)

# Best tune at C=1, Accuracy - 0.87 for both the validation datasets
# Here we got the accuracy same for all the values of C above 1 so now,
# trying with different set of values for C which are less than 1

grid2 <- expand.grid(C=seq(0.5,1, by=0.1))
#Validation dataset 1
fit21.svm <- train(Digit~., data=validate1_rec, method="svmLinear", metric=metric, 
                  tuneGrid=grid2, trControl=trainControl)
print(fit21.svm)
plot(fit21.svm)
# Validation dataset 2
fit22.svm <- train(Digit~., data=validate2_rec, method="svmLinear", metric=metric, 
                   tuneGrid=grid2, trControl=trainControl)
print(fit22.svm)
plot(fit22.svm)

# Best tune at C=0.5, Accuracy - 0.88 for both the validation datasets
# Here we got the accuracy same for all the values of C above 0.5 so now,
# trying with different set of values for C which are less than 0.5

grid3 <- expand.grid(C=seq(0.1,0.5, by=0.1))
#Validation dataset 1
fit31.svm <- train(Digit~., data=validate1_rec, method="svmLinear", metric=metric, tuneGrid=grid3, trControl=trainControl)
print(fit31.svm)
plot(fit31.svm)
#Validation dataset 2
fit32.svm <- train(Digit~., data=validate2_rec, method="svmLinear", metric=metric, tuneGrid=grid3, trControl=trainControl)
print(fit32.svm)
plot(fit32.svm)

# Best tune at C=0.1, Accuracy - 0.87 &0.88 for both the validation sets respectively.
# Here as plot shows highest accuracy is at C=0.1 and accuracy got constant from 0.2 onwords.
# so trying with the lower values C which are less than 0.1


grid4 <- expand.grid(C=c(0.001,0.01,0.05,0.1))
# Validation set 1
fit41.svm <- train(Digit~., data=validate1_rec, method="svmLinear", metric=metric, 
                  tuneGrid=grid4, trControl=trainControl)
print(fit41.svm)
plot(fit41.svm)
# Validation set 2
fit42.svm <- train(Digit~., data=validate2_rec, method="svmLinear", metric=metric, 
                   tuneGrid=grid4, trControl=trainControl)
print(fit42.svm)
plot(fit42.svm)
# Best tune at C=0.1, Accuracy - 0.88 for validation data set 1
# Best tune at C=0.01,Accuracy -0.89 for validation data set 2

## Summary of Linear hypertune parameter in validation dataset
##-----------------------------------------------------------------------------------------------
## Grid Values      |      Validation Dataset 1          |     Validation Dataset 2             |
##   ranges         |------------------------------------|--------------------------------------|
##                  | Best tune at C    |    Accuracy    | Best Tune at C    |  Accuracy        | 
##------------------|-------------------|----------------|-------------------|------------------|                                                                            
##  C=1 to 5        |      C=1          |     0.87       |        C=1        |  0.87            |
##------------------|-------------------|----------------|-------------------|------------------|
##  C=0.5 to 1      |      C=0.5        |     0.88       |        C=0.5      |     0.88         |
##------------------|-------------------|----------------|-------------------|------------------|
##  C=0.1 to 0.5    |      C=0.1        |     0.87       |        C=0.1      |      0.88        |
##------------------|-------------------|----------------|-------------------|------------------|
##  C=0.001,0.01,   |      C=0.1        |     0.88       |        C=0.01     |    0.89          |
##    0.05,0.1      |                   |                |                   |                  |
##----------------------------------------------------------------------------------------------

# Now, Applied the tunned hyperparameters to bigger training dataset and try to predict with highest accuracy
# We got the optimal C values as 0.5,0.1 and 0.01
Model2_linear <- ksvm(Digit~ ., data = training_rec, scale = FALSE, kernel = "vanilladot",C=0.5)
Eval2_linear<- predict(Model2_linear, SVM_test_rec)
confusionMatrix(Eval2_linear,SVM_test_rec$Digit)

# Observations:
# Cost=0.5, Overall accuracy of 92.06%
# Sensitivity  > 85.6%
# Accuracy of Digits 5,8 and 9 are lower than the other digits, but better than Model number one which has C=1

Model3_linear <- ksvm(Digit~ ., data = training_rec, scale = FALSE, kernel = "vanilladot",C=0.1)
Eval3_linear<- predict(Model3_linear, SVM_test_rec)
confusionMatrix(Eval3_linear,SVM_test_rec$Digit)

# Observations:
# Cost=0.1, Overall accuracy of 93.1%
# Sensitivity  > 87.7%
# Accuracy of Digits 5,8 and 9 are lower than the other digits.but better as compare to C=0.5 and 1

Model4_linear <- ksvm(Digit~ ., data = training_rec, scale = FALSE, kernel = "vanilladot",C=0.01)
Eval4_linear<- predict(Model4_linear, SVM_test_rec)
confusionMatrix(Eval4_linear,SVM_test_rec$Digit)

# Observations:
# Cost=0.01 , Overall accuracy of 93.1%
# Sensitivity  > 88.57%
# Accuracy of Digits 3,5,8 and 9 are lower than the other digits.best accuracy of all digit at C=0.01

## Summary of Linear Model in test dataset
##-------------------------------------------------------------------------------------------
##  Best Tune C     |   Accuracy        |    Sensitivity |   Accuracy of predicting digits (0-9)       | 
##------------------|-------------------|----------------|-------------------------- -------|                                                                            
##  C=0.5           |      92.06%       |     >85.6%      |        Ok                        |
##------------------|-------------------|----------------|----------------------------------|
##  C=0.1           |      93.1%        |     >87.7%      |        Better                    |
##------------------|-------------------|----------------|----------------------------------|
##  C=0.01          |     93.1%         |     >88.57%     |       Best                       |                                     |
##-------------------------------------------------------------------------------------------

############################## 5. Radial Model Building and Tunning  #################################

Model1_RBF <- ksvm(Digit~ ., data = training_rec, scale = FALSE, kernel = "rbfdot")
print(Model1_RBF)
Eval1_RBF<- predict(Model1_RBF, SVM_test_rec)
confusionMatrix(Eval1_RBF,SVM_test_rec$Digit)

# Observations:
# Overall accuracy of 95.84% Using C=1 and sigma ~ 0.010
#Increase in overall accuracy and sensitivty from linear kernel using C = 1, sigma ~ 0.010

###########################################################################################################
# Hyperparameter tuning and Cross Validation  - RBF - SVM 

# Here are the steps to tune the hyperparameter(i.e C and sigma) in RBF SVM model 
#i) We will use the train function from caret package to perform crossvalidation 
#ii) and then making a grid of c & sigma values to perform 5-fold cross validation on validatation dataset
#iii) Print and plot the output to get the highest accuracy and value of best tuned C & Sigma
#iv) We will repeat these stes in both the validation sets to double check and fine tune the hyperparameters
# So here I am taking a 2 subsample of ~1000 records each and apply a wide range of C & Sigma,
# so in this way the computational time is less (approx~3-6 minutes)
###########################################################################################################

#install.packages("parallel")
#library(doParallel)
#library(parallelSVM)
#no_cores <- detectCores() - 1
#cl <- makePSOCKcluster(3)
#registerDoParallel(cl)

# We will use the train function from caret package to perform crossvalidation
grid_r1 <- expand.grid(sigma=c(0.001,0.005,0.01), C=c(0.01,0.1,1))
#Validation set - 1
fit.svm11_radial <- train(Digit~., data=validate1_rec, method="svmRadial", metric=metric, 
                         tuneGrid=grid_r1, trControl=trainControl(method = "cv", number = 5),preProcess = NULL)
print(fit.svm11_radial)
plot(fit.svm11_radial)

# Validation set 2
fit.svm12_radial <- train(Digit~., data=validate2_rec, method="svmRadial", metric=metric, 
                         tuneGrid=grid_r1, trControl=trainControl(method = "cv", number = 5),preProcess = NULL)
print(fit.svm12_radial)
plot(fit.svm12_radial)

# Observations:
# Best sigma value is ~ 0.01 & C=1 Accuracy = 0.901

grid_r2 <- expand.grid(sigma=c(0.01,0.05,0.1), C=c(1,2,3))
#Validation set - 1
fit.svm21_radial <- train(Digit~., data=validate1_rec, method="svmRadial", metric=metric, 
                         tuneGrid=grid_r2, trControl=trainControl(method = "cv", number = 5),preProcess = NULL)
print(fit.svm21_radial)
plot(fit.svm21_radial)
#Validation set - 2
fit.svm22_radial <- train(Digit~., data=validate2_rec, method="svmRadial", metric=metric, 
                          tuneGrid=grid_r2, trControl=trainControl(method = "cv", number = 5),preProcess = NULL)
print(fit.svm22_radial)
plot(fit.svm22_radial)

# Observations:
# Best sigma value is ~ 0.01 & C=3 and C=1 Accuracy = 0.920 and 91.2%
# since sigma value is constant in above grids we take as final value 0.01 and tune value of Cost

# Making grid of for C values. 
grid_r3 <- expand.grid(sigma=0.01, C=c(4, 5, 6))
#Validation set - 1
fit.svm31_radial <- train(Digit~., data=validate1_rec, method="svmRadial", metric=metric, 
                         tuneGrid=grid_r3, trControl=trainControl(method = "cv", number = 5),preProcess = NULL)
print(fit.svm31_radial)
plot(fit.svm31_radial)
#Validation set 2
fit.svm32_radial <- train(Digit~., data=validate2_rec, method="svmRadial", metric=metric, 
                          tuneGrid=grid_r3, trControl=trainControl(method = "cv", number = 5),preProcess = NULL)
print(fit.svm32_radial)
plot(fit.svm32_radial)

# Observations:
# Best sigma value is ~ 0.01 & C=4 Accuracy ~ 0.92

# Making grid of for C values. 
grid_r4 <- expand.grid(sigma=0.01, C=c(7,8,9,10))
#Validation set - 1
fit.svm41_radial <- train(Digit~., data=validate1_rec, method="svmRadial", metric=metric, 
                          tuneGrid=grid_r4, trControl=trainControl(method = "cv", number = 5),preProcess = NULL)
print(fit.svm41_radial)
plot(fit.svm41_radial)
#Validation set 2
fit.svm42_radial <- train(Digit~., data=validate2_rec, method="svmRadial", metric=metric, 
                          tuneGrid=grid_r4, trControl=trainControl(method = "cv", number = 5),preProcess = NULL)
print(fit.svm42_radial)
plot(fit.svm42_radial)

# Observations:
# Best sigma value is ~ 0.01 & C=7 Accuracy ~ 0.92 and we observed that accuracy get constant after C=4.

## Summary of RBF hypertunning parameter in validation datasets  
##-----------------------------------------------------------------------------------------------------
## Grid Values            |      Validation Dataset 1          |     Validation Dataset 2             |
##   ranges               |------------------------------------|--------------------------------------|
##                        | Best C  |  Sigma  | Accuracy       | Best C  | Sigma    | Accuracy        |
##----------------------- |---------|---------|----------------|------------------- |-----------------|                                                                            
## sigma= 0.001,0.005,0.01|   1     | 0.01    |   90.1%        |  1      | 0.01     | 90.1%           |
##     C= 0.01,0.1,1      |         |         |                |         |          |                 |
##------------------------|---------|---------|----------------|---------|--------- |-----------------|
## Sigma= 0.01,0.05,0.1   |   3     | 0.01    |   92.0%        |  1      | 0.01     | 91.2%           |
##     C= 1,2,3           |         |         |                |         |          |                 |
##------------------------|---------|---------|----------------|---------|----------|-----------------|
## Sigma= 0.01            |   4     | 0.01    |   91.9%        |  4      | 0.01     | 91.4%           |
##     C= 4,5,6           |         |         |                |         |          |                 |
##------------------------|---------|---------|----------------|---------|----------|-----------------|
## Sigma= 0.01            |   7     | 0.01    |   92.7%        |  7      | 0.01     | 91%             |
##     C= 4,5,6,7         |         |         |                |         |          |                 |
##------------------------|---------|---------|----------------|---------|----------|-----------------|


# Now, Applied the tunned hyperparameters to bigger training dataset and try to predict with highest accuracy
# We got the optimal C values as 1,3,4 and 7 along with sigma =0.01
Model2_SVM_RBF <- ksvm(Digit~ ., data = training_rec, scale = FALSE, kernel = "rbfdot",C=3, 
                       kpar = list(sigma = 0.01))
evaluate_non_linear_2 <- predict(Model2_SVM_RBF, SVM_test_rec)
confusionMatrix(evaluate_non_linear_2, SVM_test_rec$Digit)

# Observations:
# Cost=3 , sigma = 0.01 and Overall accuracy of 96.46%
# Sensitivity  > 94.45%


Model3_SVM_RBF <- ksvm(Digit~ ., data = training_rec, scale = FALSE, kernel = "rbfdot",C=4, kpar = list(sigma = 0.01))
evaluate_non_linear_3 <- predict(Model3_SVM_RBF, SVM_test_rec)
confusionMatrix(evaluate_non_linear_3, SVM_test_rec$Digit)

# Observations:
# Cost=4 , sigma = 0.01 and Overall accuracy of 96.58%
# Sensitivity  > 94.65%

Model4_SVM_RBF <- ksvm(Digit~ ., data = training_rec, scale = FALSE, kernel = "rbfdot",C=7, kpar = list(sigma = 0.01))
evaluate_non_linear_4 <- predict(Model4_SVM_RBF, SVM_test_rec)
confusionMatrix(evaluate_non_linear_4, SVM_test_rec$Digit)

# Observations:
# Cost=7 , sigma = 0.01 and Overall accuracy of 96.6%
# Sensitivity  > 94.55%

## Summary of RBF model in test dataset
##----------------------------------------------------------------------------------------------|
##  Best Tune C         |   Accuracy        |    Sensitivity |   Accuracy of digits (0-9)       | 
##------------------    |-------------------|----------------|-------------------------- -------|                                                                            
## C=1 and Sigma = 0.01 |  95.84%           |     >93.5%     |     Best                         |
##----------------------|-------------------|----------------|----------------------------------|
## C=3 and Sigma = 0.01 |  96.46%           |     >94.45%    |     Good                         |
##----------------------|-------------------|----------------|----------------------------------|
## C=4 and Sigma = 0.01 |  96.5%            |     >94.65     |     Good                         |                                     
##----------------------|-------------------|----------------|----------------------------------|
## C=7 and Sigma = 0.01 |  96.5%            |     >94.55     |     Good                         |                                     
##----------------------|-------------------|----------------|----------------------------------|

###################################### 6.Conclusion ###################################################
# Our best fit model is Model3_SVM_RBF with rbf kernal (C=4 and sigma= 0.01) achieved highest accuracy 
# in predicting digits as ~96.5%
# Here are the statistics of sensitivity and specificity 
#             Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
#Sensitivity    0.9908   0.9885   0.9622   0.9644   0.9684   0.9540   0.9749   0.9504   0.9466   0.9465
#Specificity    0.9960   0.9975   0.9951   0.9952   0.9956   0.9968   0.9969   0.9959   0.9963   0.9958

# Here are the steps to be followed for achieving best fit model 
# 1) Taken a random sample of given training data set to 15% instances this is our training dataset
# 2) Extracted 2 subsample data sets as Validation 1000 records each from the remaining dataset, 
#    which not include in step1 training dataset  
# 3) Made sure and performed EDA to check that distribution of the dependent variables (digtits) has been preserved while sampling
# 4) Performed scaling on data with the use of max pixel values.
# 5) Performed Linear Model Building with command KSVM with kernal as "vanilladot" which gives 
#    default value of C as 1 with Overall accuracy of 91.56%
# 6) Performed the hypertuning and cross validation on two validation datasets to get the optimal 
#    value of C which comes as C= 0.01 , Accuracy = 93.1% , Sensitivity >88.57 , Specificity >99.1% 
# 7) Now move to RBF model building with command KSVM with kernal as "rbfdot" which gives 
#    default value of C as 1 and sigma as 0.01 with overall accuracy as 95.84%
# 8) Performed the hypertuning and cross validation on two validation datasets to get the optimal 
#    value of C& Sigma which comes as C= 4 Sigma = 0.01 , Accuracy = 96.5% , Sensitivity > 94.65  , Specificity > 99.5
