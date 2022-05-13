## Document Overview
This file showcases examples of data cleaning experiences. This examples are from a project that I worked on for Kaggle competition. More specifically, the Housing Price Prediction competition.

This document, will closely examine the steps I took to data clean predictors (both nominal and continuous).


## Variable 1: MSSubClass
`MSSubClass` was one of 72 predictors that appeared in this project. This predictor is a nominal predictor and here are the steps I took to clean this predictor.
### Step 1: Explore variable: 
In order to decide how to treat this variable, I chose to explore the predictor with a simple frequency table and a bargraph. 
From this procedure, I found that this variable is extremely imbalanced, with some of the levels being the majority and some having frequency of less than five.
The imbalance of categorical predictors produce a problem when splitting a dataset between training set and a testing set, thus, it needed to be recoded to be more balanced.
```r
#####MSSubClass##### No missing value
#Show barplot and table to count classes.
ggplot(data,aes(MSSubClass)) + geom_bar() 
table(data$MSSubClass)
```
### Step 2: Recoding: 
I recoded this variable from having 13 levels of smaller frequency to five levels of bigger frequency by combining three to four original levels together.
Each of the numbers that are being recoded represents a certain type of house.
After recoding the variable, then its datatype was changed to factor.
```r
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#                                                          collapse some of the classes with some other classes.
data[(data$MSSubClass == 20 | 
        data$MSSubClass == 30 | 
        data$MSSubClass == 40 |
        data$MSSubClass == 120), 'MSSubClass'] <- '1Story'
data[(data$MSSubClass == 45 | 
        data$MSSubClass == 50 | 
        data$MSSubClass == 150), 'MSSubClass'] <- '1-1/2Story'
data[(data$MSSubClass == 60 | 
        data$MSSubClass == 70 | 
        data$MSSubClass == 75 |
        data$MSSubClass == 160), 'MSSubClass'] <- '2Story'
data[(data$MSSubClass == 80 | 
        data$MSSubClass == 85 | 
        data$MSSubClass == 180), 'MSSubClass'] <- 'Split'
data[(data$MSSubClass == 90 | 
        data$MSSubClass == 190), 'MSSubClass'] <- '2Family'
#Check with barplot again.
ggplot(data,aes(MSSubClass)) + geom_bar() #Some features are still bigger than the others, 
#                                             but the occurence of each classes have balanced out better than before.

data$MSSubClass <- factor(data$MSSubClass)
#Check the value counts to ensure there are no categories that are extremely imbalanced
table(data$MSSubClass)
```
## Variable 1: LotFrontage
`LotFrontage` was another predictor that appeared in this project.
This predictor is a continuous predictor and here are the steps I took to clean this variable.
### Step 1: Explore variable: 
Before applying applying any treatment to this variable, I first explored the sumamary statistics and density distribution to identify any skewness.
Presence of skewness can influence how imputations of missing values should be performed. 
If the data is skewed, then we should consider imputation utilizing the median of the vector rather than the mean.
```r
#####LotFrontage##### Missing value: 16%
#Run summary statistics
summary(data$LotFrontage)
#Distribution plot
ggplot(data, aes(x=LotFrontage)) + geom_density()#Extremely skewed to the right.
```
### Step 2: Missing Value Imputation
Since it is found that the data was skewed, I decided to impute the missing value with the median of the vector. 
```r
#Since data is very skewed to the right, it is better to impute median than to impute mean.
data[(is.na(data$LotFrontage)), 'LotFrontage'] <- median(data[(!is.na(data$LotFrontage)), 'LotFrontage'])
```
### Step 3: Outlier Imputation
Next I decided to check for any outliers. The presence of outlier can negatively affect the final predictive model, and can lower the predictive power.
Thus, outliers should be seriously considered to be removed a lot of the times.
In the following code, I used a user predefined function to fit a best fit linear regression line on the variable, to find out any outliers that exsits. 
Additionally, this function provides `R^2`. I am looking for `R^2` value before and after the outlier imputation to see if it improves the predictive power of the variable.
```r
#Fit regression line
ggplotRegression(lm(SalePrice~LotFrontage, data)) #there is an outlier over 300. See if taking out the outlier will affect the slope.
ggplotRegression(lm(SalePrice~LotFrontage, data[data$LotFrontage < 300, ])) #The slope only changed slightly.
#Impute the outlier with the median. 
data[data$LotFrontage > 300, 'LotFrontage'] <- median(data$LotFrontage)
ggplotRegression(lm(SalePrice~LotFrontage, data)) #Imputed succesfully.
```
