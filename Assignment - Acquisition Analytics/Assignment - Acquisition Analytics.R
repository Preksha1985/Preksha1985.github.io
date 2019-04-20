## Name - Preksha Tiwari
## Module - Assignment - Acquisition Analytics 
## Instructions - Set the working directory path and place the data file(bank_marketing.csv) in the same path.
#-------------------------------------------------------------------------------------------------------------------------------
# Business Objective
#i) To reduce the customer acquisition cost by targeting the ones who are likely to buy
#ii) To improve the response rate, i.e. the fraction of prospects who respond to the campaign
# we have used bank_marketing.csv data file where we have 4 types of data
#i) Customer data: Demographic data, data about other financial products like home loan, personal loan etc.
#ii) Campaign data: Data about previous campaigns (number of previous calls, no. of days since the last call was made, etc.)
#iii) Macroeconomic data : cons.price.idx, emp.variable.rate etc
#iv) Target variable: Response (Yes/No)
# The standard process followed in Acquisition analytics assignments are as follows :
# 1. Business Understanding
# 2. Data Understanding  
# 3. Data Preparation
# 4. Logistic Modelling
# 5. Model Evaluation
# 6. Model Deployment and Recommendations
#-------------------------------------------------------------------------------------------------------------------------------
# Loading bank marketing data in the working directory. 
## read.csv() function is used to read given csv file into a data frame that it creates called bank_data.

bank_data<- read.csv("bank_marketing.csv")

## Libraries
#install.packages("ggplot2")
#install.package("dummies")
#install.packages("MASS")
#install.packages("car")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("caTools")
#install.packages("tidyr")
#install.packages("dplyr")

library(ggplot2)
library(caret)
library(caTools)
library(dummies)
library(MASS)
library(car)
library(dplyr)
#-------------------------------------------------------------------------------------------------------------------------------
## Task 1) Data Understanding and Prep 
#*******************************************************************************************************************************
str(bank_data)
summary(bank_data)

#Checking missing values
sum(is.na(bank_data))
## No missing values found

#Checking response rate of prospect customer
response <- 4640/(36548+4640)
response
# In dataset we have total number of NOs :36548 & total number of Yess - 4640 
# so for this we got the 11.2% of response rate without using model
#-------------------------------------------------------------------------------------------------------------------------------

# Checking relationship with target variable "response"  
# Attribute Name          |      Relationship With Target Variable
#-------------------------|-------------------------------------------------------    
# 1) Age                  | Age between 16-20 and 60-80 have higer response rate  
# 2) Job                  | Student and Retired job grp have higher response rate 
# 3) Marital Status       | No clear significant pattern
# 4) Education            | Tertiary_education grp has higher response rate
# 5) Default              | No clear significant pattern
# 6) Housing              | No clear significant pattern
# 7) Loan                 | No clear significant patter between target variable 
# 8) Contact              | Cellular contact menthod has higher response rate
# 9) Month                | Month of Mar and Dec has very high response rate
# 10) Day_of_week         | No clear significant patter between target variable
# 11) Duration            | Avg duration for Yes Call - 553 and for No - 220 
# 12) Campaign            | one or two times call have higher response rate
# 13) Pdays               | Contacted_in_first_10days has higher response rate 
# 14) Previous            | More_tha_3_times had higher response rate
# 15) poutcome            | If previous output is Success we have better response
# 
#1) AGE
# ****************************************************
# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()
# Let's check the outlier in the variables 
quantile(bank_data$age,seq(0,1,0.01))
# Box plot 
boxplot(bank_data$age)
# Capping the upper values of age with 71.
bank_data[(which(bank_data$age>71)),]$age <- 71
# Binning the age variable and store it into "binning.age".
bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))
# Change the response value to numbers i.e"yes-no" to "1-0"
bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)
# Check the numeric value of response rate in each bucket
agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 
# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)
# changing column name of each variables in agg_age dataframe
colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")
# Round Off the values
agg_age$response_rate <- format(round(agg_age$response_rate, 2))
agg_age
# age           response_rate  count_prospects  No.of_prospect
# (16,20]          0.41             57            140
# (20,30]          0.15            1067           7243
# (30,40]          0.10            1597          16385
# (40,50]          0.08             837          10240
# (50,60]          0.11             668           6270
# (60,70]          0.43             212            488
# (70,80]          0.48             202            422
# Let's see the response rate of each age bucket in the plot
ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)
# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)
#View(Bank_data_age20)
summary(Bank_data_age20)

#2) job
#**********************************************
# Checking the levels of the job
levels(bank_data$job)
# [1] "admin."        "blue-collar"   "entrepreneur"  "housemaid"     "management"   
# [6] "retired"       "self-employed" "services"      "student"       "technician"   
# [11] "unemployed"    "unknown"
# Plotting bar graph for job variable.
# Writing a function "plot_response" to do the same task for each variable
plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(size = 3, vjust = -0.5) + xlab(var_name)
}

plot_response(bank_data$job, "job")

#3) Marital status
#**********************************************
summary(bank_data$marital)
# divorced  married   single  unknown 
# 4612    24928    11568       80 
# Let's replace Unknown level to married
levels(bank_data$marital)[4] <- "married"
# Plotting marital status
plot_response(bank_data$marital,"marital")

#4) Education Variables
#*************************************************
plot_response(bank_data$education,"Education")
# Reducing the levels of education variable
levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"
# Let's again check the education plot
plot_response(bank_data$education,"Education_levels")
summary(bank_data$education)
#Primary_Education Secondary_Education professional.course Tertiary_Education Unknown 
#12531                9515                5243               12168            1731

#5) Default variable
#*************************************************
table(bank_data$default)
#  no     unknown     yes 
# 32588    8597       3 
plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

#6) Housing Variables 
#**************************************************
summary(bank_data$housing)
#no       unknown     yes 
#18622     990        21576 
plot_response(bank_data$housing, "Housing")

#7) loan
#*************************************
summary(bank_data$loan)
#no       unknown     yes 
#33950     990       6248 
plot_response(bank_data$loan, "Loan Status")

#8) Campaign Information
#*******************************************
#  Next variable is Contact, Let's see the response rate of each mode 
summary(bank_data$contact)
#cellular telephone 
#26144     15044
plot_response(bank_data$contact,"Contact_mode")

#9) Month
#**********************************************
# Next variable is "Month" i.e contact month. 
plot_response(bank_data$month,"Contact_month")

#10) Day of Week
#**************************************************
plot_response(bank_data$day_of_week,"day_of_week")

#11) Duration
#*****************************************************
# Now, Let's see the "duration" variable: Which is Quantitative variable
# Let's check the histogram 
ggplot(bank_data,aes(duration))+geom_histogram()
# Let's see the summary of this variable once 
summary(bank_data$duration)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0   102.0   180.0   258.3   319.0  4918.0 
# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)
#response_1   duration
#1      0     220.8448
#2      1     553.1912
bank_data <- bank_data[,-22]
## Definitely the outlier is present in the dataset
# So let's check the percentile distribution of duration 
quantile(bank_data$duration,seq(0,1,0.01))
# So, capping the duration seconds at 99% which is 1271.3sec 
bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13
# Now, again plot the histogram 
ggplot(bank_data,aes(duration))+geom_histogram()

#12) Campaign
#**********************************************************
#So let's check the summay of this variable
summary(bank_data$campaign)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   1.000   2.000   2.568   3.000  56.000 
# Let's see the percentile distribution of this variable
boxplot(bank_data$campaign)
quantile(bank_data$campaign,seq(0,1,0.01))
# Capping this at 99% which the value is 14
bank_data[which(bank_data$campaign>14),]$campaign <- 14
# Visualizing it with plot
ggplot(bank_data,aes(campaign))+geom_histogram()

#13) Pdays
#********************************************************
bank_data$pdays<- as.factor(bank_data$pdays)
# Checking summary
summary(bank_data$pdays)
levels(bank_data$pdays)
# Reducing the levels of this variable to 3.
levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"
# Also,lets see the respose rate of each levels. 
plot_response(bank_data$pday,"Pday")
# Number of prospects under each category
table(bank_data$pdays)
#Contacted_in_first_10days    Contacted_after_10days      First_time_contacted 
# 1259                         256                        39673 

#14) Previous
#******************************************************
# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)
summary(bank_data$previous)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   0.000   0.000   0.173   0.000   7.000 
# Max=7, best is to convert this variable to factor
bank_data$previous <- as.factor(bank_data$previous)
levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"
summary(bank_data$previous)
#Never contacted Less_than_3_times More than_3_times 
#35563              5531                94 
plot_response(bank_data$previous,"Previous_contacts")

#15) Poutcome
#********************************************************
# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')
summary(bank_data$poutcome)
#failure nonexistent     success 
#4252       35563        1373 
plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

#16) Economic Variable
#*********************************************************
#-- social and economic context attributes
# emp.var.rate- :employment variation rate - quarterly indicator (numeric)
summary(bank_data$emp.var.rate)
# Histogram of employment variation rate variable
ggplot(bank_data,aes(emp.var.rate))+geom_histogram()
# cons.price.idx:consumer price index - monthly indicator (numeric) 
summary(bank_data$cons.price.idx)
# Histogram of consumer price index variable
ggplot(bank_data,aes(cons.price.idx))+geom_histogram()
# cons.conf.idx: consumer confidence index - monthly indicator (numeric) 
summary(bank_data$cons.conf.idx)
# euribor3m: euribor 3 month rate - daily indicator (numeric)
summary(bank_data$euribor3m)
# nr.employed: number of employees - quarterly indicator (numeric)
summary(bank_data$nr.employed)
#-------------------------------------------------------------------------------------------------------------------------------
# Task 2) Build a logistic regression model without using the variable 'duration'
#*********************************************************************************************
# Removing binning variables 
bank_data <- bank_data[, -21]
#creating dummy variables
bank_data$response <- as.integer(bank_data$response)
k1 <- bank_data # backup 
# In order to include categorical variables in regression model the variable needs to converted into 
# numeric variables by the means of "Dummy Variables".
bank_data <- dummy.data.frame(bank_data)
# Converted target variable "response" into factor with two level yes and no
bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))
str(bank_data)
# all variables are either int/number or factor after creating dummy variables.
## splitting the data between train and test
# set.seed () function is used for reproducible random number results
set.seed(100)
split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)
train <- bank_data[split_indices,-45] 
test <- bank_data[!split_indices, ]  
# 28832 obs of 60 variables removed "duration" variable from train dataset
# 12356 obs of 61 variables  
nrow(train)/nrow(bank_data)
# [1] 0.70
nrow(test)/nrow(bank_data)
# [1] 0.30

## Logistic Regression: Model Building
#-------------------------------------------------------------------------------------------------------------------------------
#Initial model containing all variables
# Model_1 - AIC 15907 nullDev 20299 resDev 15813

logistic_1 <- glm(response ~ ., family = "binomial", data = train)
summary(logistic_1)

# Task 2.1) Perform variable selection using the usual methods
#**********************************************************************************************************************************
# Stepwise selection
## In stepAIC function, we pass our first model i.e logistic_1 and direction is set as both, because in stepwise,  
## both the forward selection of variables and backward elimination of variables happen simultaneously 
# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. 

# Commented out the StepAIC function as it takes half an hrs to complete 
# and store the output of stepwise method into an object called logistic_2

# logistic_2 <- stepAIC(logistic_1, direction = "both") #commented

#Model_2 -  AIC 15876 nullDev 20299 resDev 15820
logistic_2 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                    monthaug + monthdec + monthjun + monthmar + monthmay + monthnov + 
                    day_of_weekfri + day_of_weekmon + day_of_weekthu + day_of_weektue + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    previousLess_than_3_times + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
                    educationTertiary_Education + `jobblue-collar` + jobservices, 
                  family = "binomial", data = train)

summary(logistic_2)
# Removing multicollinearity through VIF check
sort(vif(logistic_2))

# Steps to be followed while building models are 
## I) sort() use to get the data in order firstly Check for high value of VIFs 
## II) and check the p values of corresponding to highest VIF variable, if the value is high (>0.05) then remove the variable and 
## III) if the high vif variable has very low p value then pick the second highest vif and perform the same steps
## Rule 1 just to keep in mind that after removing variable check the AIC value that should not dropped drastically. 
## Rule 2 As we can see the highest VIFs have their p values very significants so these variables are statiscally significant and we can't remove.
## follow the above steps for each highest VIFs variables until you get the variable with high VIF and Comparatively low P value 

#Model_3 - Removed variable -  euribor3m VIF-93.149416 and p value- 0.024679 and 
#AIC 15879 nullDev 20299 resDev 15825

logistic_3 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                    monthaug + monthdec + monthjun + monthmar + monthmay + monthnov + 
                    day_of_weekfri + day_of_weekmon + day_of_weekthu + day_of_weektue + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    previousLess_than_3_times + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx +  nr.employed + 
                    educationTertiary_Education + `jobblue-collar` + jobservices, 
                  family = "binomial", data = train)
summary(logistic_3)
sort(vif(logistic_3))

#Model_4 - Removed variable -  previousLess_than_3_times VIF-10.566093 and p value- 0.058675 and 
#AIC 15881 nullDev 20299 resDev 15829

logistic_4 <-  glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                     monthaug + monthdec + monthjun + monthmar + monthmay + monthnov + 
                     day_of_weekfri + day_of_weekmon + day_of_weekthu + day_of_weektue + 
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + 
                     cons.price.idx + cons.conf.idx +  nr.employed + 
                     educationTertiary_Education + `jobblue-collar` + jobservices, 
                   family = "binomial", data = train)
summary(logistic_4)
sort(vif(logistic_4))

#Model_5 - Removed variable -  day_of_weekthu VIF-1.630251 and p value- 0.110666 and 
#AIC 15881 nullDev 20299 resDev 15831

logistic_5 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                    monthaug + monthdec + monthjun + monthmar + monthmay + monthnov + 
                    day_of_weekfri + day_of_weekmon + day_of_weektue + 
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx +  nr.employed + 
                    educationTertiary_Education + `jobblue-collar` + jobservices, 
                  family = "binomial", data = train)
summary(logistic_5)
sort(vif(logistic_5))

## cannot exclude any more variable based on vif as most of them have low vif; those with higher vif are very significant and not correlated
## Now removing  based on p values 

#Model_6 - Removed variable -  day_of_weektue  p value- 0.071632 and 
#AIC 15881 nullDev 20299 resDev 15833

logistic_6 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                    monthaug + monthdec + monthjun + monthmar + monthmay + monthnov + 
                    day_of_weekfri + day_of_weekmon +  
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx +  nr.employed + 
                    educationTertiary_Education + `jobblue-collar` + jobservices, 
                  family = "binomial", data = train)
summary(logistic_6)

#Model_7 - Removed variable -  educationTertiary_Education  p value- 0.139962 and 
#AIC 15881 nullDev 20299 resDev 15835

logistic_7 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + 
                    monthaug + monthdec + monthjun + monthmar + monthmay + monthnov + 
                    day_of_weekfri + day_of_weekmon +  
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx +  nr.employed + 
                    `jobblue-collar` + jobservices, 
                  family = "binomial", data = train)
summary(logistic_7)

#Model_8 - Removed variable -  loanno  p value- 0.050595 and 
#AIC 15883 nullDev 20299 resDev 15839

logistic_8 <- glm(formula = response ~ age + jobretired + contactcellular + 
                    monthaug + monthdec + monthjun + monthmar + monthmay + monthnov + 
                    day_of_weekfri + day_of_weekmon +  
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx +  nr.employed + 
                    `jobblue-collar` + jobservices, 
                  family = "binomial", data = train)
summary(logistic_8)

#Model_9 - Removed variable -  day_of_weekfri  p value- 0.033460 and 
#AIC 15885 nullDev 20299 resDev 15843

logistic_9 <- glm(formula = response ~ age + jobretired + contactcellular + 
                    monthaug + monthdec + monthjun + monthmar + monthmay + monthnov + 
                    day_of_weekmon +  
                    campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx +  nr.employed + 
                    `jobblue-collar` + jobservices, 
                  family = "binomial", data = train)
summary(logistic_9)

#Model_10 - Removed variable -  jobretired  p value- 0.02791 and 
#AIC 15888 nullDev 20299 resDev 15848

logistic_10 <- glm(formula = response ~ age  + contactcellular + 
                     monthaug + monthdec + monthjun + monthmar + monthmay + monthnov + 
                     day_of_weekmon +  
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + 
                     cons.price.idx + cons.conf.idx +  nr.employed + 
                     `jobblue-collar` + jobservices, 
                   family = "binomial", data = train)
summary(logistic_10)

#Model_11 - Removed variable -  age  p value- 0.113779 and 
#AIC 15889 nullDev 20299 resDev 15851

logistic_11 <- glm(formula = response ~  contactcellular + 
                     monthaug + monthdec + monthjun + monthmar + monthmay + monthnov + 
                     day_of_weekmon +  
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + 
                     cons.price.idx + cons.conf.idx +  nr.employed + 
                     `jobblue-collar` + jobservices, 
                   family = "binomial", data = train)
summary(logistic_11)

#Model_12 - Removed variable -  monthdec  p value- 0.009461 and 
#AIC 15893 nullDev 20299 resDev 15857

logistic_12 <- glm(formula = response ~  contactcellular + 
                     monthaug +  monthjun + monthmar + monthmay + monthnov + 
                     day_of_weekmon +  
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + 
                     cons.price.idx + cons.conf.idx +  nr.employed + 
                     `jobblue-collar` + jobservices, 
                   family = "binomial", data = train)
summary(logistic_12)

#Model_13 - Removed variable -  jobservices  p value- 0.006162 and 
#AIC 15899 nullDev 20299 resDev 15865

logistic_13 <- glm(formula = response ~  contactcellular + 
                     monthaug +  monthjun + monthmar + monthmay + monthnov + 
                     day_of_weekmon +  
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + 
                     cons.price.idx + cons.conf.idx +  nr.employed + 
                     `jobblue-collar` , 
                   family = "binomial", data = train)
summary(logistic_13)

#Model_14 - Removed variable -  monthaug  p value- 0.000600 and 
#AIC 15909 nullDev 20299 resDev 15877

logistic_14 <- glm(formula = response ~  contactcellular + 
                     monthjun + monthmar + monthmay + monthnov + 
                     day_of_weekmon +  
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + 
                     cons.price.idx + cons.conf.idx +  nr.employed + 
                     `jobblue-collar` , 
                   family = "binomial", data = train)
summary(logistic_14)

#Model_15 - Removed variable -  nr.employed  p value- 0.00846 and 
#AIC 15914 nullDev 20299 resDev 15884

logistic_15 <- glm(formula = response ~  contactcellular + 
                     monthjun + monthmar + monthmay + monthnov + 
                     day_of_weekmon +  
                     campaign + pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + 
                     cons.price.idx + cons.conf.idx +   
                     `jobblue-collar` , 
                   family = "binomial", data = train)
summary(logistic_15)

## Now all the p values are very low. Hence, our final model Model number 15 With 14 significant variables which are
# contactcellular | monthjun | monthmar | monthmay | monthnov  
# day_of_weekmon  | campaign |  pdaysContacted_in_first_10days | pdaysContacted_after_10days  
# poutcomefailure | emp.var.rate | cons.price.idx | cons.conf.idx | `jobblue-collar`  
## Null deviance: 20299  on 28831  degrees of freedom
## Residual deviance: 15884  on 28817  degrees of freedom
## AIC: 15914

logistic_final <- logistic_15

## Logistic Regression: Model Evaluation
#-------------------------------------------------------------------------------------------------------------------------------
# In a model evaluation process, we use our model that has been prepared with the help of training data, 
# to make predictions for the testing data. We used below methods to evaluate the model
# i) Accuracy-Sensitivity-Specificity ii) Gain-Lift  

# A) Acuracy-Sensitivity-Specificity
# predict() function is used to predict probabilities of attrition for test data
# Input to predict function - our final_model(model_number 15) and type as response to get the output in terms of probability
# We need to exclude "Response" column as it is the dependent variable

predictions_logit <- predict(logistic_final, newdata = test[, -61], type = "response")

# Let's see the Min and Max values using summary function which give us the prediction range on testdata from .06% TO 80% 

summary(predictions_logit)
#Min.    1st Qu.  Median Mean    3rd Qu.    Max. 
#0.01072 0.03846 0.05729 0.11074 0.09653 0.89263 

# To calculate the cutoff value we use table/ConfusionMatrix which measure the performance of a  model
# Let's use the probability cutoff of 50%.

predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))
# Creating confusion matrix for identifying the model evaluation.

conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
conf
# Accuracy : 0.898 
# Sensitivity : 0.21480         
# Specificity : 0.98477
# Since the sensitivity is very poor in case of probability cutoff of 50%.
# Let's find out the optimal probalility cutoff 

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff values from prediction range (from min.01% to max .99%) for plotting and initiallizing a matrix of 100 X 3.
s = seq(.01,.99,length=100)
OUT = matrix(0,100,3)
for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

## Plotting the Sensitivity,Specificity and Accuracy of the model to identify the final cut off value. 
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

## Getting the cutoff value. 
# So based on the above plot we choose a cutoff value of 0.07 for final model
# since we are focusing on prospects who will say Yes to product and that is capture in sensitivity so 
# when the cutoff is increased,its sensitivity decrease and specificity increase so we choose 0.07 as cutoff 

cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.04)]
cutoff

# Let's choose a cutoff value of 7% for final model
# Here the confusion matrix gives us 3 measures which shows how well our model dscriminates between two classes
# Sensitivity is a ratio which gives true positive rate which came 0.725 for final model
# Specificity is a ratio which gives true negative rate which came 0.702 for final model
# Accuracy - is a ratio which gives sum of true positive and true negative divided by all observation i.e. 0.705
predicted_response <- factor(ifelse(predictions_logit >= 0.07, "yes", "no"))
conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]
acc # 0.705
sens #0.725
spec #0.702

# Task 2.2) Sort the data points in decreasing order of probability of response
#************************************************************************************************************************************
# Let's append prediction probability and predicated response to testdata set and  
# Sort the data points in decreasing order of probability of response

test$pred_prob <- predictions_logit
test$pred_response <- predicted_response
test <- test[order(test$pred_prob,decreasing = T),]

# Task 2.3) Find the optimal probability cut-off and report the relevant evaluation metrics
#***************************************************************************************************************************************
#-----------------------------|-------|
# Optimal Probability Cut-off | 0.07  |
# Accuracy                    | 0.705 |
# Sensitivity                 | 0.725 |
# Specificity                 | 0.702 |
#-----------------------------|-------|

# Task 3) Create a data frame with the variables prospect ID, actual response, predicted response, 
# predicted probability of response, duration of call in seconds, and cost of call 
#***********************************************************************************************

test_predictions_df <- subset(test,select = c(response,pred_response,pred_prob,duration))

# created a unique ID for each prospect of newly created dataframe 
test_predictions_df$prospect_id <- 1:nrow(test_predictions_df)

# Reorder and Renamed the columns name of newly created dataframe

test_predictions_df <- test_predictions_df[,c(5,1,2,3,4)]
colnames(test_predictions_df) <- c("prospect_ID","actual_response","predicted_response","predicted_probability","duration_of_call")

# Cost of call varies with duration as follows
test_predictions_df$cost_of_call <- 0.033*test_predictions_df$duration_of_call+0.8
str (test_predictions_df)
# New data frame have all 6 below mentioned columns 
# data.frame':	12356 obs. of  6 variables:
# prospect_ID          : int  1 2 3 4 5 6 7 8 9 10 ...
# actual_response      : Factor w/ 2 levels "no","yes": 2 2 1 2 2 2 2 1 2 2 ...
# predicted_response   : Factor w/ 2 levels "no","yes": 2 2 2 2 2 2 2 2 2 2 ...
# predicted_probability: num  0.893 0.893 0.893 0.893 0.889 ...
# duration_of_call     : num  180 263 184 101 313 363 215 370 272 263 ...
# cost_of_call         : num  6.74 9.48 6.87 4.13 11.13 ...

# Task 4) Find the number of top X% prospects you should target to meet the business objective 
# and report the Avg call duration for targeting the TOP X% prospects
#*************************************************************************************************************************************

lift <- function(labels , predicted_prob, groups=10) {
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}

# Create a Table of cumulative gain and lift 
# # as observed that table shows when using our best fit model vs logistic model 
# at the 5th decile we'll be able to identified ~80% of the prospects who will respond.

test_predictions_df$actual_response <- as.factor(ifelse(test_predictions_df$actual_response=="yes",1,0))
LG = lift(test_predictions_df$actual_response, test_predictions_df$predicted_probability, groups = 10)

#  bucket total totalresp Cumresp Gain   Cumlift
#  ------ ----- --------- ------- ----   -------
#      1  1236       591     591  42.5    4.25
#      2  1236       266     857  61.6    3.08
#      3  1235       116     973  69.9    2.33
#      4  1236        86    1059  76.1    1.90
#      5  1235        66    1125  80.8    1.62
#      6  1236        72    1197  86.0    1.43
#      7  1236        59    1256  90.2    1.29
#      8  1235        44    1300  93.4    1.17
#      9  1236        44    1344  96.6    1.07
#     10  1235        48    1392 100      1   

# Average call duration for targeting the top 80% prospects
Top_5 <- sum(LG$total[1:5])
avg_duration <- mean(test_predictions_df$duration[1:Top_5])
avg_cost <- mean(test_predictions_df$cost_of_call[1:Top_5])
#sum(test_predictions_df$cost_of_call[1:6178])
#sum(test_predictions_df$cost_of_call[1:9985])
# Outcome - 
# 1) as per the above table by just targeting the prospects of first 5 decile i.e 
# (1236+1236+1235+1236+1235) 6178 prospects we can capture 80% of responders
# 2) average call duration for targeting the top 80% Prospects ~ 267.7884 seconds and 
# 3) average cost for targeting the top 80% Prospects using model ~ 9.64Rs  

# Task 5) create a lift chart The x-axis contains the number of prospects contacted; 
#the y-axis contains the ratio: response rate using the model/ response rate without using the model
#*************************************************************************************************************************************************
# Lift is a measure of the effectiveness of a predictive model calculated as the ratio between 
# the results obtained with and without the predictive model.
# Compares the 'lift in response rate' you will get with the model viz-à-viz when you target the entire population (without using the model)
# Contains lift (on y-axis) and the number of prospects targeted (on x-axis)

plot(LG$bucket,LG$Cumlift,col="red",type="l",main="Lift Chart",xlab="% of total targeted",ylab = "Lift")

# Outcome - 
# our current response rate to target the entire population is without model  
# ~11.2% and using with model is ~ 1125/6178 ~ 18.2, So Significant lift ~ 18.2/11.2 ~ 1.625
# As per Lift chart at the 2nd decile on x axis we get a lift of approx 3.2 this means 
# we get 3.2*11.2% response rate  by targeting 20% prospects using the model. Similarly 
# At the 4th decile on the x-axis, we get a lift of approx 2, it means 
# that we get 2 x 11.2% response rate by targeting 40% prospects using the model

# ******************************************* Conclusion ******************************************************************************************
# 1) The financial benifit of this logistic model is if we market to only top 5 decile
# (50% of the customers),we will able to capture more than 80% of the prospects
#----------------------------------------|--------------------------|-----------------------|
# 2) Parameters                          |   Without Model          |   With Model          |
#----------------------------------------|--------------------------|---------------------- |
#1) No. of responders for 80% prospects  |   ~ 9,985                |   ~ 6178              |
#2) Response Rate                        |  1125/9985 ~ 11.2%       |   1125/6178 ~ 18.2%   |
#3) cost per response for 80% prospects  |  92454/1125 ~ 82.18 Rs   | 59537.5/1125 ~ 52.92Rs|
#----------------------------------------|--------------------------|-----------------------|
# 3) Number of responders to get the 1125(i.e.80% prospects) 
#   Without model we need to contact ~ 9,985 prospects and With model only 6178 responders
# 4) cost of acquisition per customer without model 9985/1125 ~ 8.875 and
#   using this model 6178/1125 ~ 5.491 
# 5) cost per response to get 1125(i.e.80% prospects) responders is Without model i.e 92454/1125 ~  82.18RS
#  and using with model cost per response to get 1125 is ~ 59537.5 i.e. 59533/1125 ~ 52.92Rs
# 6) significant lift ~ 82.18/52.92 ~ 1.62    
# 7) response rate : our current response rate to target the entire population is without model  
# ~11.2% and using with model is ~ 1125/6178 ~ 18.2
