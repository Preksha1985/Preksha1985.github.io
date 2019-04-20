## Module - Linear Regression - Assignment
## Name - Preksha Tiwari
## Instructions - Set the working directory path and place the datafile(car_price_Assignment.csv) in the same path.

# Remove existing memory space if any
remove(list=ls())

## read.csv() function is used to read given csv file into a data frame that it creates called uber.
 
carprice <- read.csv("Carprice_Assignment.csv")

## Libraries
install.packages("tidyverse")
install.packages("MASS")
library(tidyverse)
library(MASS)
library(car)

##1) Data Cleaning
#1.1) Remove NA values from dataset using colSums() function
# Observation - There are no NA values in the dataframe 
carprice <- carprice[colSums(!is.na(carprice))!=0]
 
#1.2) Checked for duplicate rows and removed if any usinf duplicated() function
# Observation-  No duplicate data in dataset
carprice <- carprice[!duplicated(carprice),]

##2) Data manipulation
#2.1) Derived metrics 
## Extract CompanyName using Separate() function
 carprice <- separate(carprice, CarName , into=c("Company"),extra='drop')
## tolower() - used to change the case of data in lowercase. 
 carprice$Company <- tolower(carprice$Company)
## Spell check  
 carprice$Company[which(carprice$Company=="vokswagen")] <- "volkswagen"
 carprice$Company[which(carprice$Company=="volkswagen")] <- "volkswagon" 
 carprice$Company[which(carprice$Company=="porcshce")] <- "porsche"
 carprice$Company[which(carprice$Company=="maxda")] <- "mazda"
 carprice$Company[which(carprice$Company=="vw")] <- "volkswagon"
 carprice$Company[which(carprice$Company=="toyouta")] <- "toyota"
 
#2.2) Dummy variable creation 
 ## For Categorical variables with 2 levels; the categories were converted into levels 0 and 1 using level() 
 ## For Categorical variables with 3 and more leves; dummy variables were created using model.matrix()

#a) Fuel Type (structur Gas - 0 and Diesel - 1)
    levels(carprice$fueltype) <- c(1,0)
    carprice$fueltype <- as.numeric(levels(carprice$fueltype))[carprice$fueltype]
#b) Aspiration (std - 1 and turbo - 0) 
    levels(carprice$aspiration) <-  c(1,0)
    carprice$aspiration <- as.numeric(levels(carprice$aspiration))[carprice$aspiration]
#c) doornumber (two :0 ; four :1) 
    levels(carprice$doornumber) <- c(1,0)
    carprice$doornumber <- as.numeric(levels(carprice$doornumber))[carprice$doornumber]
#d) enginelocation(front :1 , rear : 0 )
    levels(carprice$enginelocation) <-  c(1,0)
     carprice$enginelocation <- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]

#2.3) Create the dummy variable for variable having more than 2 levels usinf model.matrix
#a) Drive wheel
dummy_1 <- data.frame(model.matrix( ~drivewheel, data = carprice))
dummy_1 <- dummy_1[,-1]
## Combine the dummy variables and the numeric columns of dataset, in a new dataset called car_price
car_price <- cbind(carprice[,-8], dummy_1)

##b) Car Company Name 
car_price$Company<- as.factor(car_price$Company)
dummy_2 <- data.frame(model.matrix( ~Company, data = car_price))
dummy_2 <- dummy_2[,-1]
car_price <- cbind(car_price[,-3], dummy_2)

##c) Carbody 
dummy_3 <- data.frame(model.matrix( ~carbody, data = car_price))
dummy_3 <- dummy_3[,-1]
car_price <- cbind(car_price[,-6], dummy_3)

##d)enginetype
dummy_4 <- data.frame(model.matrix( ~enginetype, data = car_price))
dummy_4 <- dummy_4[,-1]
car_price <- cbind(car_price[,-12], dummy_4)

##e) cylindernumber
dummy_5 <- data.frame(model.matrix( ~cylindernumber, data = car_price))
dummy_5 <- dummy_5[,-1]
car_price <- cbind(car_price[,-12], dummy_5)

##f) fuelsystem
dummy_6 <- data.frame(model.matrix( ~fuelsystem, data = car_price))
dummy_6 <- dummy_6[,-1]
car_price <- cbind(car_price[,-13], dummy_6)

##g) symboling 
car_price$symboling <- as.factor(car_price$symboling)
dummy_7 <- data.frame(model.matrix( ~symboling, data = car_price))
dummy_7 <- dummy_7[,-1]
car_price <- cbind(car_price[,-2], dummy_7)

# separate traindataing and testing data
set.seed(100)
trainindices= sample(1:nrow(car_price), 0.7*nrow(car_price))
traindata <- car_price[trainindices,]
testdata <- car_price[-trainindices,]

## Build model 1 containing all variables
model_1 <-lm(price~.,data=traindata)
summary(model_1)

## In stepAIC function, we pass our first model i.e model_1 and direction is ser as both, because in stepwise,  
## both the forward selection of variables and backward elimination of variables happen simultaneously 
step <- stepAIC(model_1, direction="both")

# stepAIC makes multiple calls while checking which variables to keep
# The last call that step makes, contains only the variables it considers to be important in the model. 
# some insignifican variables have been removed. store the last model equation of stepwise method into an object called model_2
step
## model 2 

model_2 <-  lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
                 curbweight + enginesize + stroke + peakrpm + citympg + drivewheelrwd + 
                 Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth + Companyporsche + Companyrenault + 
                 Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
                 carbodywagon + enginetyperotor + cylindernumberfour + cylindernumbersix + 
                 fuelsystem2bbl + symboling.1 + symboling0 + symboling3 + 
                 symboling2, data = traindata)
# Let us look at the summary of the model (Multiple R-squared:  0.9815,	Adjusted R-squared:  0.9745)
summary(model_2)
## Let us check for multicollinearity using VIF () 
sort(vif(model_2))

## Steps to be followed while building models are 
## I) Check for high value of VIFs sort() use to get the data in order
## II) check the p values of variable, if the value is high (>0.05) then remove the variable and 
## III) if the high vif variable has very low p value then pick the second highest vif and perform the same steps
## IV) after removing variable check the adjusted R^2 values that should not dropped drastically. 

## As we can see the highest VIFs (more than 100) are for var "ComapnyToyota" and "Car_ID" 
## but their p values are very significants so these variables are statiscally significant and we can't remove.
## follow the above steps for each highest VIFs variables until you get the variable with high VIF and Comparatively low P value 

## Model_3 - Removed variable - curbweight with VIF-26.069347 and p value- 0.001885 and 
## Multiple R-squared:  0.9797,	Adjusted R-squared:  0.9722

model_3 <-    lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
                   enginesize + stroke + peakrpm + citympg + drivewheelrwd + 
                   Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                   Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                   Companypeugeot + Companyplymouth + Companyporsche + Companyrenault + 
                   Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                   Companyvolvo + carbodyhardtop + carbodyhatchback + carbodysedan + 
                   carbodywagon + enginetyperotor + cylindernumberfour + cylindernumbersix + 
                   fuelsystem2bbl + symboling.1 + symboling0 + symboling3 + 
                   symboling2, data = traindata)
summary(model_3)
sort(vif(model_3))

## Model_4 - Removed variable carbodysedan with VIF-19.033455 and p value- 0.014342 and 
## Multiple R-squared:  0.9785,	Adjusted R-squared:  0.9709 

model_4 <-  lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + citympg + drivewheelrwd + 
                 Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth + Companyporsche + Companyrenault + 
                 Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo + carbodyhardtop + carbodyhatchback +  
                 carbodywagon + enginetyperotor + cylindernumberfour + cylindernumbersix + 
                 fuelsystem2bbl + symboling.1 + symboling0 + symboling3 + 
                 symboling2, data = traindata)
summary(model_4)
sort(vif(model_4))

## Model_5 - Removed variable - cylindernumberfour with VIF - 9.391486 and p Value- 0.002394 and
## Multiple R-squared:  0.9765,	Adjusted R-squared:  0.9685 

model_5 <-  lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm + citympg + drivewheelrwd + 
                 Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth + Companyporsche + Companyrenault + 
                 Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo + carbodyhardtop + carbodyhatchback +  
                 carbodywagon + enginetyperotor +  cylindernumbersix + 
                 fuelsystem2bbl + symboling.1 + symboling0 + symboling3 + 
                 symboling2, data = traindata)

summary(model_5)
sort(vif(model_5))

## Model_6 - Removed variable - Companyporsche with VIF - 10.857822 and p Value- 0.002772 and
## Multiple R-squared:  0.9765,	Adjusted R-squared:  0.9685 

model_6 <- lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + citympg + drivewheelrwd + 
                Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                Companypeugeot + Companyplymouth +  Companyrenault + 
                Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                Companyvolvo + carbodyhardtop + carbodyhatchback +  
                carbodywagon + enginetyperotor +  cylindernumbersix + 
                fuelsystem2bbl + symboling.1 + symboling0 + symboling3 + 
                symboling2, data = traindata)

summary(model_6)
sort(vif(model_6))

## Model_7 - Removed variable - car_id with VIF - 56.052048 and p Value- 0.006910 and
## Multiple R-squared:  0.9726,	Adjusted R-squared:  0.9639 

model_7 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + citympg + drivewheelrwd + 
                Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                Companypeugeot + Companyplymouth +  Companyrenault + 
                Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                Companyvolvo + carbodyhardtop + carbodyhatchback +  
                carbodywagon + enginetyperotor +  cylindernumbersix + 
                fuelsystem2bbl + symboling.1 + symboling0 + symboling3 + 
                symboling2, data = traindata)

summary(model_7)
sort(vif(model_7))

## Model_8 - Removed variable - drivewheelrwd with VIF - 5.802236 and p Value- 0.354822 and
## Multiple R-squared:  0.9724,	Adjusted R-squared:  0.96 

model_8 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm + citympg +  
                Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                Companypeugeot + Companyplymouth +  Companyrenault + 
                Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                Companyvolvo + carbodyhardtop + carbodyhatchback +  
                carbodywagon + enginetyperotor +  cylindernumbersix + 
                fuelsystem2bbl + symboling.1 + symboling0 + symboling3 + 
                symboling2, data = traindata)

summary(model_8)
sort(vif(model_8))

## Model_9 - Removed variable - citympg with VIF - 4.198351 and p Value- 0.113078 and 
## Multiple R-squared:  0.9717,	Adjusted R-squared:  0.9635

model_9 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm +   
                Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                Companypeugeot + Companyplymouth +  Companyrenault + 
                Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                Companyvolvo + carbodyhardtop + carbodyhatchback +  
                carbodywagon + enginetyperotor +  cylindernumbersix + 
                fuelsystem2bbl + symboling.1 + symboling0 + symboling3 + 
                symboling2, data = traindata)

summary(model_9)
sort(vif(model_9))

## Model_10 - Removed variable - fuelsystem2bbl with VIF - 3.155303 and p Value- 0.031918 and 
## Multiple R-squared:  0.9705,	Adjusted R-squared:  0.9622

model_10 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm +   
                 Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo + carbodyhardtop + carbodyhatchback +  
                 carbodywagon + enginetyperotor +  cylindernumbersix + 
                 symboling.1 + symboling0 + symboling3 + 
                 symboling2, data = traindata)

summary(model_10)
sort(vif(model_10))

## Model_11 - Removed variable - symboling.1 with VIF - 2.651046 and p Value- 0.878117 and 
## Multiple R-squared:  0.9705,	Adjusted R-squared:  0.9626 

model_11 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm +   
                 Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo + carbodyhardtop + carbodyhatchback +  
                 carbodywagon + enginetyperotor +  cylindernumbersix + 
                  symboling0 + symboling3 + 
                 symboling2, data = traindata)

summary(model_11)
sort(vif(model_11))

## Model_12 - Removed variable - cylindernumbersix with VIF - 2.384422 and p Value- 0.189094 and 
## Multiple R-squared:   0.97,	Adjusted R-squared:  0.9623  

model_12 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                enginesize + stroke + peakrpm +   
                Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                Companypeugeot + Companyplymouth +  Companyrenault + 
                Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                Companyvolvo + carbodyhardtop + carbodyhatchback +  
                carbodywagon + enginetyperotor +   
                symboling0 + symboling3 + 
                symboling2, data = traindata)

summary(model_12)
sort(vif(model_12))

## Model_13 - Removed variable - symboling2 with VIF - 2.291580 and p Value- 0.039629 and 
## Multiple R-squared:  0.9689,	Adjusted R-squared:  0.9612 

model_13 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke + peakrpm +   
                 Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo + carbodyhardtop + carbodyhatchback +  
                 carbodywagon + enginetyperotor +   
                 symboling0 + symboling3 , data = traindata)

summary(model_13)
sort(vif(model_13))

## Model_14 - Removed variable - peakrpm with VIF - 2.143721 and p Value- 0.175428 and 
## Multiple R-squared:  0.9684,	Adjusted R-squared:  0.9609 

model_14 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo + carbodyhardtop + carbodyhatchback +  
                 carbodywagon + enginetyperotor +   
                 symboling0 + symboling3 , data = traindata)

summary(model_14)
sort(vif(model_14))

## Model_15 - Removed variable - symboling3 with VIF - 2.083846 and p Value- 0.201579 and 
## Multiple R-squared:  0.9679,	Adjusted R-squared:  0.9607
 
model_15 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo + carbodyhardtop + carbodyhatchback +  
                 carbodywagon + enginetyperotor +   
                 symboling0  , data = traindata)

summary(model_15)
sort(vif(model_15))

## We reached at the steps where all high VIFs (>2) variables have very low p values 
## So now started removing higher p values now onwards no need to look at VIFs values. 

## Model_16 - Removed variable - carbodywagon with p Value- 0.917125 and maintained 
## Multiple R-squared:  0.9679,	Adjusted R-squared:  0.9611

model_16 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo + carbodyhardtop + carbodyhatchback +  
                 enginetyperotor +   
                 symboling0  , data = traindata)
summary(model_16)

## Model_17 - Removed variable - symboling0 with p Value- 0.37725 and maintained 
## Multiple R-squared:  0.9677,	Adjusted R-squared:  0.9611 

model_17 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo + carbodyhardtop + carbodyhatchback +  
                 enginetyperotor  , data = traindata)
summary(model_17)

## Model_18 - Removed variable - carbodyhardtop with p Value- 0.127022 and maintained 
## Multiple R-squared:  0.967,	Adjusted R-squared:  0.9607 

model_18 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo +  carbodyhatchback +  
                 enginetyperotor  , data = traindata)
summary(model_18)

## Model_19 - Removed variable - carbodyhatchback with p Value- 0.029920 and maintained 
## Multiple R-squared:  0.9657,	Adjusted R-squared:  0.9594

model_19 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw + Companydodge + Companyhonda + Companyisuzu + 
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo +    
                 enginetyperotor  , data = traindata)
summary(model_19)

## Model_20 - Removed variable - Companyisuzu with p Value- 0.006557 and maintained 
## Multiple R-squared:  0.9635,	Adjusted R-squared:  0.9572

model_20 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw + Companydodge + Companyhonda +  
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysaab + Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo +    
                 enginetyperotor  , data = traindata)
summary(model_20)

## Model_21 - Removed variable - Companysaab with p Value- 0.007604 and maintained 
## Multiple R-squared:  0.9613,	Adjusted R-squared:  0.955 

model_21 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw + Companydodge + Companyhonda +  
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo +    
                 enginetyperotor  , data = traindata)
summary(model_21)

## Model_22 - Removed variable - Companyhonda with p Value- 0.005379 and maintained 
## Multiple R-squared:  0.9588,	Adjusted R-squared:  0.9524 

model_22 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw + Companydodge +   
                 Companymazda + Companymercury + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo +    
                 enginetyperotor  , data = traindata)
summary(model_22)

## Model_23 - Removed variable - Companymercury with p Value- 0.009505 and maintained 
## Multiple R-squared:  0.9564,	Adjusted R-squared:  0.9501 

model_23 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw + Companydodge +   
                 Companymazda  + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysubaru + Companytoyota + Companyvolkswagon + 
                 Companyvolvo +    
                 enginetyperotor  , data = traindata)
summary(model_23)

## Model_24 - Removed variable - Companyvolvo with p Value- 0.025767 and maintained 
## Multiple R-squared:  0.9546,	Adjusted R-squared:  0.9485 

model_24 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw + Companydodge +   
                 Companymazda  + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysubaru + Companytoyota + Companyvolkswagon + 
                 enginetyperotor  , data = traindata)
summary(model_24)

## Model_25 - Removed variable - Companyvolkswagon with p Value- 0.017364 and maintained 
## Multiple R-squared:  0.9525,	Adjusted R-squared:  0.9465 

model_25 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw + Companydodge +   
                 Companymazda  + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +  Companyrenault + 
                 Companysubaru + Companytoyota + 
                 enginetyperotor  , data = traindata)
summary(model_25)

## Model_26 - Removed variable - Companyrenault with p Value- 0.016737 and maintained 
## Multiple R-squared:  0.9503,	Adjusted R-squared:  0.9444  

model_26 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw + Companydodge +   
                 Companymazda  + Companymitsubishi + Companynissan + 
                 Companypeugeot + Companyplymouth +   
                 Companysubaru + Companytoyota + 
                 enginetyperotor  , data = traindata)
summary(model_26)

## Model_27 - Removed variable - Companyplymouth with p Value- 0.004235 and maintained 
## Multiple R-squared:  0.947,	Adjusted R-squared:  0.9412  

model_27 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw + Companydodge +   
                 Companymazda  + Companymitsubishi + Companynissan + 
                 Companypeugeot +    
                 Companysubaru + Companytoyota + 
                 enginetyperotor  , data = traindata)
summary(model_27)

## Model_28 - Removed variable - Companydodge with p Value- 0.015091 and maintained 
## Multiple R-squared:  0.9445,	Adjusted R-squared:  0.9389  

model_28 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw +    
                 Companymazda  + Companymitsubishi + Companynissan + 
                 Companypeugeot +    
                 Companysubaru + Companytoyota + 
                 enginetyperotor  , data = traindata)
summary(model_28)

## Model_29 - Removed variable - Companynissan with p Value- 0.007334 and maintained 
## Multiple R-squared:  0.9413,	Adjusted R-squared:  0.9359 

model_29 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw +    
                 Companymazda  + Companymitsubishi +  
                 Companypeugeot +    
                 Companysubaru + Companytoyota + 
                 enginetyperotor  , data = traindata)
summary(model_29)

## Model_30 - Removed variable - Companymazda with p Value- 0.013226 and maintained 
## Multiple R-squared:  0.9384,	Adjusted R-squared:  0.9333  

model_30 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw +    
                  Companymitsubishi +  
                 Companypeugeot +    
                 Companysubaru + Companytoyota + 
                 enginetyperotor  , data = traindata)
summary(model_30)

## Model_31 - Removed variable - Companymitsubishi with p Value- 0.004406 and maintained 
## Multiple R-squared:  0.9345,	Adjusted R-squared:  0.9295 

model_31 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw +    
                 Companypeugeot +    
                 Companysubaru + Companytoyota + 
                 enginetyperotor  , data = traindata)
summary(model_31)

## Model_32 - Removed variable - Companytoyota with p Value- 0.00415 and maintained 
## Multiple R-squared:  0.9303,	Adjusted R-squared:  0.9255 

model_32 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw +    
                 Companypeugeot +    
                 Companysubaru + 
                 enginetyperotor  , data = traindata)
summary(model_32)

## Model_33 - Removed variable - Companypeugeot with p Value- 0.002961 and maintained 
## Multiple R-squared:  0.9255,	Adjusted R-squared:  0.921

model_33 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw +    
                 Companysubaru + 
                 enginetyperotor  , data = traindata)
summary(model_33)

## Model_34 - Removed variable - Companysubaru with p Value- 0.001326 and maintained 
##Multiple R-squared:  0.9195,	Adjusted R-squared:  0.9153 

model_34 <- lm(formula = price ~  aspiration + enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw +    
                 enginetyperotor  , data = traindata)
summary(model_34)

## After model 34 all P's become statistically significant
# We will consider this model for predict analysis
## now trying to remove the highest P value variable from model_34 and get the variations. 
## Let's try to first remove the variable with highest p value among others

## Model_35 - Removed variable - aspiration with p Value- 0.000614 and maintained 
## Multiple R-squared:  0.9121,	Adjusted R-squared:  0.9083  

model_35 <- lm(formula = price ~  enginelocation + carwidth + 
                 enginesize + stroke +    
                 Companybmw +    
                 enginetyperotor  , data = traindata)
summary(model_35)

## Model_36 - Removed variable - stroke with p Value- 0.00255 and maintained 
## Multiple R-squared:  0.906,	Adjusted R-squared:  0.9026   

model_36 <- lm(formula = price ~  enginelocation + carwidth + 
                 enginesize +     
                 Companybmw +    
                 enginetyperotor  , data = traindata)
summary(model_36)

## Model_37 - Removed variable - enginetyperotor with p Value- 0.000169 and maintained 
## Multiple R-squared:  0.8958,	Adjusted R-squared:  0.8927  

model_37 <- lm(formula = price ~  enginelocation + carwidth + 
                 enginesize +     
                 Companybmw    
                , data = traindata)
summary(model_37)

# Here model number 37 has all P's statistically significant which can be consider for prediction
# We will consider this model for predict analysis
# now for further analysis let's remove the highest P value from Model 37 i.e. companybmw and carwidth one by one

## Model_38 - Removed variable - companybmw with p Value- 4.56e-06 and noticed there is a drastic change in adjusted R square value 
## Multiple R-squared:  0.8786,	Adjusted R-squared:  0.8759 
## so we are not consider this model for further analysis

model_38 <- lm(formula = price ~  enginelocation + carwidth + 
                 enginesize 
               , data = traindata)
summary(model_38)

## Model_39 - Removed variable - carwidth with p Value- 2.94e-14 and noticed there is a drastic change in adjusted R square value 
## Multiple R-squared:  0.8413,	Adjusted R-squared:  0.8379 
## so we are not consider this model for further analysis

model_39 <- lm(formula = price ~  enginelocation + 
                 enginesize + Companybmw
               , data = traindata)
summary(model_39)

# predicting the results in test dataset & calculating error 


# Predictive Model 1 -  model_number 34
testdata$test_price34 <- predict(model_34,testdata[,-19])
testdata$error_price34 <- testdata$price - testdata$test_price34
# Now, we need to test the r square between actual and predicted price 
r <- cor(testdata$price,testdata$test_price34)
rsquared <- cor(testdata$price,testdata$test_price34)^2
## Here for Model number34 we have correlation (r) as 92% and rsquare as 0.84 in test data
## whereas R square in training data comes out as 0.91 which mean difference ~ 7%

## Actual vs. Predicted Price PLot  for the final model i.e. Model 34 
ggplot(testdata, aes(car_ID, price)) + geom_line(aes(colour = "black" )) + geom_line(aes(x=car_ID, y=test_price34, colour="red"))
## Also plotting Error i.e. difference between actual price and predicted price 
ggplot(testdata, aes(car_ID, error_price34)) + geom_point() + geom_hline(yintercept = 0)
# here error are random as no specific pattern followed in error graph SO we can say model is quite good and stable 

# Predictive model 2 - Model number 37
testdata$test_price37 <- predict(model_37,testdata[,-19])
testdata$error_price37 <- testdata$price - testdata$test_price37
# Now, we need to test the r square between actual and predicted price. 
r <- cor(testdata$price,testdata$test_price37)
rsquared <- cor(testdata$price,testdata$test_price37)^2
## Here for Model number37 we have correlation (r) as 92% and rsquare as 0.84 in test data
## whereas R square in training data comes out as 0.89 that is difference between training and test as ~ 5%

## Actual vs. Predicted Price PLot  for the final model i.e. Model 37 
ggplot(testdata, aes(car_ID, price)) + geom_line(aes(colour = "black" )) + geom_line(aes(x=car_ID, y=test_price37, colour="red"))
## Also plotting Error i.e. difference between actual price and predicted price 
ggplot(testdata, aes(car_ID, error_price37)) + geom_point() + geom_hline(yintercept = 0)
# here error are random as no specific pattern followed in error graph SO we can say model is quite good and stable 

# Conclusion
#------------
# Here is summary of all the Predictive models with the value of adjusted R square of Train and Test data 
# and correlation between Actual Car Price and Predicted Car price

## Predictive Model |  Independent Variables which are significant   |Adjusted R square of |Correlation between actual|
##                  | in predicting the price of car                 |training and testdata|price and predicted price | 
##------------------|------------------------------------------------|---------------------|--------------------------|                                                                             
##        1         | aspiration,enginlocation,stroke,Companybmw,    |0.91 & 0.84          |     92%                  |      
##                  | enginetyperotor,carwidth,enginsize             |                     |                          |
##------------------|------------------------------------------------|---------------------|--------------------------|        
##        2         |  enginelocation,carwidth,enginesize,Companybmw |0.89 & 0.84          |     92%                  |            
##                  |                                                |                     |                          |

## As we can seen from graphs and R values , Predictive models 1 and 2 both are fairly good,
##  but the best fit model id Predictive model 2 i.e. model number 37 because
##  difference between Adjusted R square of the Train Dataset and Test dataset is minimum 
##  correlation between actual and predicted price is very fair. 
##  Also when Actual Price and Predicted price were plotted it was quite overlapping and the error plot also showed very less randomness. 
## So,Price depends mainly on these below independent variables :
##      1. Enginelocation
##      2. carwidth
##      3. enginesize
##      4. Companybmw
## Additionaly we can also consider below variables to predict the price 
##      5. Stroke
##      6. Aspiration
##      7. engin type of rotor
## Below is the statistics of best fit model
## Model number 37
##
##Call:
##  lm(formula = price ~ enginelocation + carwidth + enginesize + 
##       Companybmw, data = traindata)
##
##Residuals:
##  Min      1Q  Median      3Q     Max 
##-5757.3 -1759.7     3.1  1421.9  7151.4 
##
##Coefficients:
##  Estimate Std. Error t value Pr(>|t|)    
##(Intercept)    -79942.860   9943.118  -8.040 3.63e-13 ***
##  enginelocation -16431.882   1744.223  -9.421  < 2e-16 ***
##  carwidth         1464.582    172.539   8.488 2.94e-14 ***
##  enginesize         99.346      9.338  10.638  < 2e-16 ***
##  Companybmw       7533.448   1578.182   4.773 4.56e-06 ***
##  ---
##  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 2676 on 138 degrees of freedom
## Multiple R-squared:  0.8958,	Adjusted R-squared:  0.8927 
##F-statistic: 296.5 on 4 and 138 DF,  p-value: < 2.2e-16

