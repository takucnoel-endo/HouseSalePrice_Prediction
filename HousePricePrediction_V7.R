###########
#Libraries#
###########
library(dplyr)
library(ggplot2)
library(tidyr)
library(DMwR2)
library(car)
library(geoR)
library(onehot)
library(caret)
library(glmnet)
library(tree)



###########
#Functions#
###########
ggplotRegression <- function (fit) {
  #Description:*External Code Source* Creates a best fitline between two variables, 
  #gives slope, AdjR2, Intercept, and P value for each line.
  #Reference: Original code from https://sejohnston.com/2012/08/09/a-quick-and-easy-function-to-plot-lm-results-in-r/
  require(ggplot2)
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}


GetAllNA <- function (df) {
  #Description: Function to print out percentage of missing values in a data frame.
  #Use this function on console when I need reference throughout data cleaning.
  for (col in colnames(df)) {
    print(col)
    print((sum(is.na(df[col]))/nrow(data))*100) 
  }
}

#################
###Data Import###
#################
#Set working directory
#setwd("C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Data")
#Read data.
data_original <- read.csv("HousePricesCompleteData.csv")
#Copy the original data into working data so we have memory of both.
data <- data_original



#######################################
###Data Cleaning/Feature Engineering###
#######################################
#####SalePrice#####
#Data type into numeric.
data$SalePrice <- as.numeric(data$SalePrice)
#Summary statistics
summary(data$SalePrice)
#Distribution of response variable.
ggplot(data,aes(SalePrice)) + geom_density()
#Density plot to compare log and sqrt transformation.
ggplot(data,aes(sqrt(SalePrice))) + geom_density()
ggplot(data,aes(SalePrice^(1/3))) + geom_density()
ggplot(data,aes(log(SalePrice))) + geom_density() #log transformation works the best.
#Transform
data$SalePrice <- log(data$SalePrice)


#####MSSubClass##### No missing value
#Show barplot and table to count classes.
ggplot(data,aes(MSSubClass)) + geom_bar() 
table(data$MSSubClass)
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



#####MSZoning#####Missing value: 0.13%
#Check the each classes in the feature.
unique(data$MSZoning) #There are C(All) but should be renamed as C
#Modify "C (All)" to "C"
data[(!is.na(data$MSZoning) & data$MSZoning == 'C (all)'), 'MSZoning'] <- "C"
#Inpute the missing value with the most frequent class.
table(data$MSZoning) #RL has the most frequency
data[is.na(data$MSZoning), 'MSZoning'] <- 'RL'
#Class distribution.
ggplot(data,aes(MSZoning)) + geom_bar() 
#Factorize
data$MSZoning <- factor(data$MSZoning)




#####LotFrontage##### Missing value: 16%
#Run summary statistics
summary(data$LotFrontage)
#Distribution plot
ggplot(data, aes(x=LotFrontage)) + geom_density()#Extremely skewed to the right.
#Since data is very skewed to the right, it is better to impute median than to impute mean.
data[(is.na(data$LotFrontage)), 'LotFrontage'] <- median(data[(!is.na(data$LotFrontage)), 'LotFrontage'])
#Fit regression line
ggplotRegression(lm(SalePrice~LotFrontage, data)) #there is an outlier over 300. See if taking out the outlier will affect the slope.
ggplotRegression(lm(SalePrice~LotFrontage, data[data$LotFrontage < 300, ])) #The slope only changed slightly.
#Impute the outlier with the median. 
data[data$LotFrontage > 300, 'LotFrontage'] <- median(data$LotFrontage)
ggplotRegression(lm(SalePrice~LotFrontage, data)) #Imputed succesfully.
#Since the feature is skewed to the right, visualize if applying the log transformation will normalize the feature values.
ggplot(data, aes(x=log(LotFrontage))) + geom_density()
ggplot(data,aes(sqrt(LotFrontage))) + geom_density() 
ggplot(data,aes(LotFrontage^(1/3))) + geom_density() #Log transformation works the best.
#Apply Log transformation to this feature.
data$LotFrontage <- log(data$LotFrontage)
ggplotRegression(lm(SalePrice~LotFrontage, data))


#####LotArea##### No missing value
#Change datatype to numeric
data$LotArea <- as.numeric(data$LotArea)
#Run summary statistics
summary(data$LotArea)
#Distribution plot
ggplot(data, aes(x=LotArea)) + geom_density()#Extremely skewed to the right.
#Fit regression line
ggplotRegression(lm(SalePrice~LotArea, data)) #There are outliers above 100000 that creates a significant assiciation between
#LotArea and SalePrice, even if there are clearly seem to have no relationship.
#Try fitting the regression line withought the outliers.
ggplotRegression(lm(SalePrice~LotArea, data[data$LotArea < 30000, ])) #The fit seems better.
#Impute the outlier with the median. 
data[data$LotArea > 30000, 'LotArea'] <- median(data$LotArea)
#Visualize to see if applying the log transformation will normalize the feature values.
ggplot(data, aes(x=log(LotArea))) + geom_density()
ggplot(data, aes(x=sqrt(LotArea))) + geom_density()
ggplot(data, aes(x=LotArea^(1/3))) + geom_density()
#Apply cube root transformation to this feature.
data$LotArea <- data$LotArea^(1/3)
#Check the regression line again. 
ggplotRegression(fit <- lm(SalePrice~LotArea, data))


#####Street##### No missing value
#Distribution plot with bar plot.
ggplot(data,aes(Street)) + geom_bar()
#Check values with tables
table(data$Street) #The Grvl class only have 12 counts. There are no way to collapse classes into
                  #larger categories since there only two classes available. It is good to drop this column to
                    #avoid error when training ols. 
#Drop feature. 
data <- subset(data, select=-Street)



#####Alley##### Missing value: 46%
#The missing value in this feature means that there are no alley access.
#Put None values into the NA values to indicate no access
data[is.na(data$Alley), 'Alley'] <- 'None'
#Distribution plot with bar plot.
ggplot(data,aes(Alley)) + geom_bar() 
table(data$Alley) #All class counts are above 20.
#Factorize
data$Alley <- factor(data$Alley)


#####LotShape##### No missing value
ggplot(data,aes(LotShape)) + geom_bar() 
table(data$LotShape) #Some class counts are below 20.
#collapse some of the classes with some other classes. Combine all irregular classes into "Irreg"
data[(data$LotShape == 'IR1' | 
        data$LotShape == 'IR2' | 
        data$LotShape == 'IR3'), 'LotShape'] <- 'Irreg'

ggplot(data,aes(LotShape)) + geom_bar() 
#Factorize
data$LotShape <- factor(data$LotShape)
table(data$LotShape) #now all class counts are above 20, and are moe balanced as well. 


#####LandContour##### No missing value
ggplot(data,aes(LandContour)) + geom_bar() 
table(data$LandContour) #All class counts are above 20
#Factorize
data$LandContour <- factor(data$LandContour)


#####Utilities##### Missing value: 0.068%
ggplot(data,aes(Utilities)) + geom_bar() 
data[is.na(data$Utilities), 'Utilities'] <- 'AllPub'
table(data$Utilities)  #There are only one count of NoSeWa class that is just one of the two calsses. 
                      #There is no way to make this feature usable in the model, and can also create biases.
                      #So it is better to drop the feature.
#Drop feature
data <- subset(data, select=-Utilities)


#####LotConfig##### No missing value
ggplot(data,aes(LotConfig)) + geom_bar() 
table(data$LotConfig)  #There are classes below counts of 20. But it is possible to assign them with broader class level.
#FR2 and FR3 are in a similar category as Corner Lots.
data[(data$LotConfig == 'FR2' | 
        data$LotConfig == 'FR3'), 'LotConfig'] <- 'Corner'
#CulDSac is in a similar category as in Inside lot.
data[data$LotConfig == 'CulDSac', 'LotConfig'] <- 'Inside'
#Check the categories again.
ggplot(data,aes(LotConfig)) + geom_bar() 
table(data$LotConfig)  #Now there are no more class counts below 20
#Factorize
data$LotConfig <- factor(data$LotConfig)


#####Landslope##### No missing value
ggplot(data,aes(LandSlope)) + geom_bar() 
table(data$LandSlope) #Sev has a class count below 20. This can cause error when predicting on the OLS model.
                      #By general knowledge having a moderate land slope vs. severe land slope can affect the price of a house.
                      #Therefore, it is not ideal to collapse Sev class with Mod class. In this case it would be better to
                      #drop the feature.
data <- subset(data, select=-LandSlope)


#####Neighborhood##### No missing value
ggplot(data,aes(Neighborhood)) + geom_bar() 
table(data$Neighborhood) #Some class counts are below 20. It cannot be collapsed, but an external data can be added.

#Add external data (Household Average Income: Low/Average/High) depending on the neighborhood
#Data manually collected from https://bestneighborhood.org/household-income-ames-ia/
data$AreaAvgIncome <- NA
data[(data$Neighborhood == 'Blmngtn' | 
        data$Neighborhood == 'ClearCr' |
        data$Neighborhood == 'NAmes' |
        data$Neighborhood == 'NoRidge' | 
        data$Neighborhood == 'NridgHt' |
        data$Neighborhood == 'NWAmes' |
        data$Neighborhood == 'Somerst' |
        data$Neighborhood == 'StoneBr' |
        data$Neighborhood == 'Veenker'), 'AreaAvgIncome'] <- 'High'
data[(data$Neighborhood == 'Timber' | 
        data$Neighborhood == 'Sawyer' |
        data$Neighborhood == 'SawyerW' |
        data$Neighborhood == 'OldTown' |
        data$Neighborhood == 'NPkVill' | 
        data$Neighborhood == 'Mitchel' |
        data$Neighborhood == 'MeadowV' |
        data$Neighborhood == 'Gilbert' |
        data$Neighborhood == 'CollgCr' |
        data$Neighborhood == 'BrDale' |
        data$Neighborhood == 'SWISU'), 'AreaAvgIncome'] <- 'Average'
data[(data$Neighborhood == 'Blueste' | 
        data$Neighborhood == 'BrkSide' |
        data$Neighborhood == 'Crawfor' |
        data$Neighborhood == 'Edwards' | 
        data$Neighborhood == 'IDOTRR'), 'AreaAvgIncome'] <- 'Low'

#Drop Neighborhood feature.
data <- subset(data, select=-Neighborhood)
data$AreaAvgIncome <- factor(data$AreaAvgIncome, 
                             ordered=TRUE, 
                             levels=c('Low','Average','High'))

ggplot(data,aes(AreaAvgIncome)) + geom_bar() 
table(data$AreaAvgIncome) #All classes counts are above 20. 
ggplotRegression(lm(SalePrice~AreaAvgIncome, data)) #Have slight positive correlation to response variable.



#####Condition1##### No missing value
ggplot(data,aes(Condition1)) + geom_bar() 
table(data$Condition1) #Some classes have counts of below 20.
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#collapse some of the classes with some other classes. 
data[(data$Condition1 == 'Artery' | 
        data$Condition1 == 'Feedr'), 'Condition1'] <- 'NearStreet'
data[(data$Condition1 == 'RRNn' | 
        data$Condition1 == 'RRAn' |
        data$Condition1 == 'RRNe' |
        data$Condition1 == 'RRAe'), 'Condition1'] <- 'NearRailRoad'
data[(data$Condition1 == 'PosN' | 
        data$Condition1 == 'PosA'), 'Condition1'] <- 'NearOff-site'

ggplot(data,aes(Condition1)) + geom_bar() 
#Check frequency table again.
table(data$Condition1) #no more class counts below 20
#Factorize
data$Condition1 <- factor(data$Condition1)

#####Condition2##### No missing value
ggplot(data,aes(Condition2)) + geom_bar() 
table(data$Condition2) #Some class counts below 20. 
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#collapse some of the classes with some other classes. 
data[(data$Condition2 == 'Artery' | 
        data$Condition2 == 'Feedr'), 'Condition2'] <- 'NearStreet'
data[(data$Condition2 == 'RRNn' | 
        data$Condition2 == 'RRAn' |
        data$Condition2 == 'RRNe' |
        data$Condition2 == 'RRAe'), 'Condition2'] <- 'NearRailRoad'
data[(data$Condition2 == 'PosN' | 
        data$Condition2 == 'PosA'), 'Condition2'] <- 'NearOff-site'

table(data$Condition2) #There are still some classes below 20. At this point they are not able to further collapse into
                      #bigger categories. It is better to drop the feature. 

data <- subset(data, select=-Condition2)


#####BldgType##### No missing value
ggplot(data,aes(BldgType)) + geom_bar() 
table(data$BldgType) #No classes below count of 20.
#Factorize
data$BldgType <- factor(data$BldgType)

#####HouseStyle##### No missing value
ggplot(data,aes(HouseStyle)) + geom_bar() 
table(data$HouseStyle) #Some classes below counts of 20.
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#collapse some of the classes with some other classes.
data[(data$HouseStyle == '1.5Fin' | 
        data$HouseStyle == '1.5Unf'), 'HouseStyle'] <- '1.5Story'
data[(data$HouseStyle == '2.5Fin' | 
        data$HouseStyle == '2.5Unf'), 'HouseStyle'] <- '2.5Story'
data[(data$HouseStyle == 'SFoyer' | 
        data$HouseStyle == 'SLvl'), 'HouseStyle'] <- 'Split'
#Factorize
data$HouseStyle <- factor(data$HouseStyle)
ggplot(data,aes(HouseStyle)) + geom_bar() 
table(data$HouseStyle) #no classes below counts of 20 anymore. 


#####OverallQual##### No missing value
ggplot(data,aes(OverallQual)) + geom_bar()
ggplotRegression(lm(SalePrice~OverallQual, data))
table(data$OverallQual) #Some classes below counts of 20. 
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#collapse some of the classes with some other classes.
data[(data$OverallQual == 1 | 
        data$OverallQual == 2 |
        data$OverallQual == 3), 'OverallQual'] <- 1
#Down grade all of the other models to match the order. 4 to 2, 5 to 3, 6 to 4 and so on. 
data[data$OverallQual == 4, 'OverallQual'] <- 2
data[data$OverallQual == 5, 'OverallQual'] <- 3
data[data$OverallQual == 6, 'OverallQual'] <- 4
data[data$OverallQual == 7, 'OverallQual'] <- 5
data[data$OverallQual == 8, 'OverallQual'] <- 6
data[data$OverallQual == 9, 'OverallQual'] <- 7
data[data$OverallQual == 10, 'OverallQual'] <- 8
table(data$OverallQual) #no more class counts less than 20. 
data$OverallQual <- factor(data$OverallQual, 
                           ordered=TRUE, 
                           levels=c(1,2,3,4,5,6,7,8))
#Fit regression line
ggplotRegression(lm(SalePrice~OverallQual, data)) #this feature is strongly positively correlated with the response.



#####OverallCond##### No missing value
ggplot(data,aes(OverallCond)) + geom_bar() 
table(data$OverallCond)#Some classes below counts of 20. 
data[(data$OverallCond == 1 | 
        data$OverallCond == 2 |
        data$OverallCond == 3), 'OverallCond'] <- 1
#Down grade all of the other models to match the order. 4 to 2, 5 to 3, 6 to 4 and so on. 
data[data$OverallCond == 4, 'OverallCond'] <- 2
data[data$OverallCond == 5, 'OverallCond'] <- 3
data[data$OverallCond == 6, 'OverallCond'] <- 4
data[data$OverallCond == 7, 'OverallCond'] <- 5
data[data$OverallCond == 8, 'OverallCond'] <- 6
data[data$OverallCond == 9, 'OverallCond'] <- 7
table(data$OverallCond) #no more class counts less than 20. 

data$OverallCond <- factor(data$OverallCond, 
                           ordered=TRUE, 
                           levels=c(1,2,3,4,5,6,7,8,9))
#Fit regression line
ggplotRegression(lm(SalePrice~OverallCond, data)) #Slightly positively correlate to response.



#####YearBuilt##### No missing value
#Chage data type to numeric. 
data$YearBuilt <- as.numeric(data$YearBuilt)
#Fit regression line
ggplotRegression(lm(SalePrice~YearBuilt, data)) #Positively correlated with response. 
#Distribution plot.
ggplot(data, aes(x=YearBuilt)) + geom_density() #Skewed to the left



#####YearRemodAdd##### No missing value
#Change data tyoe to numeric
data$YearRemodAdd <- as.numeric(data$YearRemodAdd)
#Fit regression line
ggplotRegression(lm(SalePrice~YearRemodAdd, data)) #Positively correlated with response. 
#Distribution plot.
ggplot(data, aes(x=YearRemodAdd)) + geom_density() #Skewed to the left



#####Roofstyle##### No missing value
ggplot(data,aes(RoofStyle)) + geom_bar()
table(data$RoofStyle) #Some classes counts are less than 20.
                      #It is not possible to collapse many of those classes into one. Just drop feature. 
#Drop feauture.
data <- subset(data, select=-RoofStyle)



#####Roofmatl##### No missing value
ggplot(data,aes(RoofMatl)) + geom_bar()
table(data$RoofMatl)#Some classes counts are less than 20.
#It is not possible to collapse many of those classes into one. Just drop feature. 
#Drop feauture.
data <- subset(data, select=-RoofMatl)


#####Exterior1st##### Missing value: 0.034%
#Use previously defined FreqImpute() function
ggplot(data,aes(Exterior1st)) + geom_bar()
table(data$Exterior1st) #Vinylsd has the most frequency.
data[is.na(data$Exterior1st), 'Exterior1st'] <- 'VinylSd'
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#collapse some of the classes with some other classes. 
data[(data$Exterior1st == 'AsbShng' | 
        data$Exterior1st == 'CBlock' |
        data$Exterior1st == 'Stone'), 'Exterior1st'] <- 'Concrete/Asphalt/Stone'
data[(data$Exterior1st == 'BrkComm' | 
        data$Exterior1st == 'BrkFace'), 'Exterior1st'] <- 'Brick'
data[(data$Exterior1st == 'CemntBd' | 
        data$Exterior1st == 'ImStucc' |
        data$Exterior1st == 'Stucco'), 'Exterior1st'] <- 'Cement'
data[(data$Exterior1st == 'AsphShn' | 
        data$Exterior1st == 'Wd Sdng' |
        data$Exterior1st == 'WdShing'), 'Exterior1st'] <- 'Wood'
table(data$Exterior1st) #All the counts are above 20
data$Exterior1st <- factor(data$Exterior1st)


#####Exteriror2nd##### Missing value: 0.034%
ggplot(data,aes(Exterior2nd)) + geom_bar()
table(data$Exterior2nd) #Vinylsd has the most frequency.
data[is.na(data$Exterior2nd), 'Exterior2nd'] <- 'VinylSd'
unique(data$Exterior2nd)
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#collapse some of the classes with some other classes. 
data[(data$Exterior2nd == 'AsbShng' | 
        data$Exterior2nd == 'CBlock' |
        data$Exterior2nd == 'Stone'), 'Exterior2nd'] <- 'Concrete/Asphalt/Stone'
data[(data$Exterior2nd == 'BrkComm' | 
        data$Exterior2nd == 'BrkFace'), 'Exterior2nd'] <- 'Brick'
data[(data$Exterior2nd == 'CemntBd' | 
        data$Exterior2nd == 'ImStucc' |
        data$Exterior2nd == 'Stucco'), 'Exterior2nd'] <- 'Cement'
data[(data$Exterior2nd == 'AsphShn' | 
        data$Exterior2nd == 'Wd Sdng' |
        data$Exterior2nd == 'WdShing'), 'Exterior2nd'] <- 'Wood'
#Assing other to a most frequent calss
data[data$Exterior2nd == 'Other', 'Exterior2nd'] <- 'VinylSd'
table(data$Exterior2nd) #All the counts are above 20
data$Exterior2nd <- factor(data$Exterior2nd)

#####MasVnrType##### Missing value: 0.82%
ggplot(data,aes(MasVnrType)) + geom_bar()
table(data$MasVnrType) #All classes have count of above 20
data[is.na(data$MasVnrType), 'MasVnrType'] <- 'None'
unique(data$MasVnrType)
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#collapse some of the classes with some other classes. 
data[(data$MasVnrType == 'BrkCmn' | 
        data$MasVnrType == 'BrkFace'), 'MasVnrType'] <- 'Brick'
ggplot(data,aes(MasVnrType)) + geom_bar()
table(data$MasVnrType)
#Factorize
data$MasVnrType <- factor(data$MasVnrType)

#####MasVnrArea##### Missing value: 0.78%
summary(data$MasVnrArea)
#Change dtype to numeric
data$MasVnrArea <- as.numeric(data$MasVnrArea)
ggplot(data,aes(MasVnrArea)) + geom_density() #Value extremely skewed to the right.
#Fit the best fit line.
ggplotRegression(lm(SalePrice~MasVnrArea, data)) #There seems to be no obvious outliers.
ggplot(data,aes(MasVnrArea)) + geom_density()
#For imputation, perform a median imputation instead of mean.
data[is.na(data$MasVnrArea), 'MasVnrArea'] <- median(data[(!is.na(data$MasVnrArea)), 'MasVnrArea'])
#value of 0 turns into -inf when transformed. In order to avoid this issue on ransformation. 
#Rather than applying log transformation, apply root transformation to normalize.
#visualize the distribution of transformed data with dist plot
#visualize the distribution of transformed data with dist plot
ggplot(data,aes(sqrt(MasVnrArea))) + geom_density() #Still skewed.
ggplot(data,aes(MasVnrArea^(1/3))) + geom_density() #Cube root made it more normalized
#Transform with cube root.
data$MasVnrArea <- data$MasVnrArea^(1/3)
ggplotRegression(lm(SalePrice~MasVnrArea, data)) 

#Check if some rows containing MasVnrType do not contain MasVnrArea.
#If there are, impute with the 3rd Qu of MasVnrArea.
data[(data$MasVnrType != 'None' & data$MasVnrArea == 0), c('MasVnrType', 'MasVnrArea')] #There are 3 rows.
data[(data$MasVnrType != 'None' & data$MasVnrArea == 0), 'MasVnrArea'] <- 5.4


#####ExterQual##### No missing value
table(data$ExterQual)
ggplot(data,aes(ExterQual)) + geom_bar()
data$ExterQual <- factor(data$ExterQual, 
                         ordered=TRUE, 
                         levels=c('Fa','TA','Gd','Ex'))
#Fit regression line
ggplotRegression(lm(SalePrice~ExterQual, data)) #This feature seems to have positive correlattion with response.


#####ExterCond##### No missing value
table(data$ExterCond)
ggplot(data,aes(ExterCond)) + geom_bar()
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#collapse some of the classes with some other classes. 
data[(data$ExterCond == 'Po' | 
        data$ExterCond == 'Fa'), 'ExterCond'] <- 'Soso'
data[(data$ExterCond == 'Gd' | 
        data$ExterCond == 'Ex'), 'ExterCond'] <- 'Yay'

data$ExterCond <- factor(data$ExterCond, 
                         ordered=TRUE, 
                         levels=c('Soso','TA','Yay'))
table(data$ExterCond) #No class counts are under 20.
#Fit regression line
ggplotRegression(lm(SalePrice~ExterCond, data))  #Does not seemt to correlate alot.



#####Foundation##### No missing value
table(data$Foundation) #Some classes with count of under 20. 
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#collapse some of the classes with some other classes. 
data[data$Foundation == 'Slab', 'Foundation'] <- 'Stone'
  #Although wood is does not fir in any of the calsses, it is just 5 datapoints, and it needs somewhere to be recategorized.
data[data$Foundation == 'Wood', 'Foundation'] <- 'BrkTil'
table(data$Foundation)
ggplot(data,aes(Foundation)) + geom_bar()
data$Foundation <- factor(data$Foundation)

#####BsmtQual##### Missing value: 2.77%
table(data$BsmtQual) #All classes count over 20.
data[is.na(data$BsmtQual), 'BsmtQual'] <- 'TA'
ggplot(data,aes(BsmtQual)) + geom_bar()
data$BsmtQual <- factor(data$BsmtQual, 
                        ordered=TRUE, 
                        levels=c('Fa','TA','Gd','Ex'))

#Fit regression line
ggplotRegression(lm(SalePrice~BsmtQual, data)) #This feature seems to have positive correlattion with response.


#####BsmtCond##### Missing value: 2.8%
table(data$BsmtCond) #TA has the most frequency.
data[is.na(data$BsmtCond), 'BsmtCond'] <- 'TA'
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#collapse some of the classes with some other classes. 
data[(data$BsmtCond == 'Po' | 
        data$BsmtCond == 'Fa'), 'BsmtCond'] <- 'Fa'
ggplot(data,aes(BsmtCond)) + geom_bar()
table(data$BsmtCond) #All class counts over 20.
data$BsmtCond <- factor(data$BsmtCond, 
                        ordered=TRUE, 
                        levels=c('Fa','TA','Gd'))

#Fit regression line
ggplotRegression(lm(SalePrice~BsmtCond, data)) #Does not seemt to correlate alot.



#####BsmtExposure##### Missing value: 2.8%
table(data$BsmtExposure) 
data[is.na(data$BsmtExposure), 'BsmtExposure'] <- 'No'
ggplot(data,aes(BsmtExposure)) + geom_bar()
data$BsmtExposure <- factor(data$BsmtExposure, 
                            ordered=TRUE, 
                            levels=c('No','Mn','Av','Gd'))

#Fit regression line
ggplotRegression(lm(SalePrice~BsmtExposure, data)) #Slightly positively correlate with the response


#####BsmtFinType1##### Missing value: 2.7%
table(data$BsmtFinType1) #No class counts under 20. 
data[is.na(data$BsmtFinType1), 'BsmtFinType1'] <- 'None'
ggplot(data,aes(BsmtFinType1)) + geom_bar()
data$BsmtFinType1 <- factor(data$BsmtFinType1, 
                            ordered=TRUE, 
                            levels=c('None','Unf','LwQ','Rec','BLQ','ALQ','GLQ'))
#Fit regression line
ggplotRegression(lm(SalePrice~BsmtFinType1, data)) #Slightly positively correlate with the response


#####BsmtFinSF1##### Missing value: 0.034%
summary(data$BsmtFinSF1) #the value 0 indicating that there are no basements, the maximum value is 5644sqft.
ggplot(data,aes(BsmtFinSF1)) + geom_density() #Feature is extremely skewed to the right
data[(is.na(data$BsmtFinSF1)), 'BsmtFinSF1'] <- median(data[(!is.na(data$BsmtFinSF1)), 'BsmtFinSF1'])
#Fit the best fit line.
ggplotRegression(lm(SalePrice~BsmtFinSF1, data)) #Outlier found above 4000.
#Impute the outlier with the median. 
data[data$BsmtFinSF1 > 4000, 'BsmtFinSF1'] <- median(data$BsmtFinSF1)
#value of 0 turns into -inf when transformed. In order to avoid this issue on ransformation. 
#Rather than applying log transformation, apply root transformation to normalize.
#visualize the distribution of transformed data with dist plot
ggplot(data,aes(sqrt(BsmtFinSF1))) + geom_density() #Still skewed.
ggplot(data,aes(BsmtFinSF1^(1/3))) + geom_density() #Sqrt made it more normalized
#Transform with cube root.
data$BsmtFinSF1 <- sqrt(data$BsmtFinSF1)
ggplotRegression(lm(SalePrice~BsmtFinSF1, data))

#####BsmtFinType2##### Missing value: 2.74%
table(data$BsmtFinType2) #No class counts under 20. 
data[is.na(data$BsmtFinType2), 'BsmtFinType2'] <- 'None'
ggplot(data,aes(BsmtFinType2)) + geom_bar()

data$BsmtFinType2 <- factor(data$BsmtFinType2, 
                            ordered=TRUE, 
                            levels=c('None','Unf','LwQ','Rec','BLQ','ALQ','GLQ'))

#Fit regression line
ggplotRegression(lm(SalePrice~BsmtFinType2, data)) #Does not seemt to correlate alot.


#####BsmtFinSF2##### Missing value: 0.034%
summary(data$BsmtFinSF2) #the value 0 indicating that there are no basements, the maximum value is 1526sqft.
ggplot(data,aes(BsmtFinSF2)) + geom_density() #Feature is extremely skewed to the right
data[(is.na(data$BsmtFinSF2)), 'BsmtFinSF2'] <- median(data[(!is.na(data$BsmtFinSF2)), 'BsmtFinSF2'])
#Fit the best fit line.
ggplotRegression(lm(SalePrice~BsmtFinSF2, data)) #There seems to be no outliers, but the best fit line is horizontal.
#Indicates that this feature should not add much influence in the response variable.
#value of 0 turns into -inf when transformed. In order to avoid this issue on ransformation. 
#Rather than applying log transformation, apply root transformation to normalize.
#visualize the distribution of transformed data with dist plot
#visualize the distribution of transformed data with dist plot
ggplot(data,aes(sqrt(BsmtFinSF2))) + geom_density() #Still skewed.
ggplot(data,aes(BsmtFinSF2^(1/3))) + geom_density() #Cube root made it better, but far from being normalized.
#Still, it is better than no treatment.
#Transform with cube root.
data$BsmtFinSF2 <- data$BsmtFinSF2^(1/3)
ggplotRegression(lm(SalePrice~BsmtFinSF2, data)) #Even after transformation, it is horizontal.
#Drop this column since it has no affect in sale price.
data <- subset(data, seleect=-BsmtFinSF2)

#####BsmtUnfSF#####
#Convert dtype to numeric.
data$BsmtUnfSF <- as.numeric(data$BsmtUnfSF)
#Summary statistics
summary(data$BsmtUnfSF)
#Data normality.
ggplot(data,aes(BsmtUnfSF)) + geom_density() #Data is right skewed but it looks sqrt or log transformable.
#First impute.
data[(is.na(data$BsmtUnfSF)), 'BsmtUnfSF'] <- median(data[(!is.na(data$BsmtUnfSF)), 'BsmtUnfSF'])
#Fit the best fit line.
ggplotRegression(lm(SalePrice~BsmtUnfSF, data)) #There are no obvious outliers. 
#Density plot to compare log and sqrt transformation.
ggplot(data,aes(sqrt(BsmtUnfSF))) + geom_density()
ggplot(data,aes(BsmtUnfSF^(1/3))) + geom_density()
ggplot(data,aes(log(BsmtUnfSF))) + geom_density()#Sqrt transformation works better in this case
#log transformation over shifts the data to the left, so did cuberoot.
#Transformation.
data$BsmtUnfSF <- sqrt(data$BsmtUnfSF)
#Check the fit again.
ggplotRegression(lm(SalePrice~BsmtUnfSF, data)) #Some very slight positive effect in response

#####TotalBsmtSF#####
#Convert dtype to numeric.
data$TotalBsmtSF <- as.numeric(data$TotalBsmtSF) 
#Summary statistics
summary(data$TotalBsmtSF)
#Data normality.
ggplot(data,aes(TotalBsmtSF)) + geom_density() #Data is right skewed but it looks sqrt or log transformable.
#First impute.
data[(is.na(data$TotalBsmtSF)), 'TotalBsmtSF'] <- median(data[(!is.na(data$TotalBsmtSF)), 'TotalBsmtSF'])
#Fit the best fit line.
ggplotRegression(lm(SalePrice~TotalBsmtSF, data)) #There is definitely an outlier above the 3000 mark.
#Impute the outlier with the median. 
data[data$TotalBsmtSF > 3000, 'TotalBsmtSF'] <- median(data$TotalBsmtSF)
ggplot(data,aes(TotalBsmtSF)) + geom_density() 
ggplotRegression(lm(SalePrice~TotalBsmtSF, data)) 
#Density plot to compare log and sqrt transformation.
ggplot(data,aes(sqrt(TotalBsmtSF))) + geom_density()
#Density plot to compare log and sqrt transformation.
ggplot(data,aes(sqrt(TotalBsmtSF))) + geom_density()
ggplot(data,aes(TotalBsmtSF^(1/3))) + geom_density()
#Add a small number to avoid making 0s to undefined.
ggplot(data,aes(log(TotalBsmtSF+0.01))) + geom_density()  #Sqrt transformation works the best in this case.
Sqrt_TotalBsmtSF <- sqrt(data$TotalBsmtSF)
#Check the fit again.
ggplotRegression(lm(SalePrice~Sqrt_TotalBsmtSF, data)) #Applying sqrt transformation actually makes the feature
#less correlated to respose variable.
#Keep the original distribution, since the distribution is not too skewed to start with.


#####Heating##### No missing value
table(data$Heating) #Some classes counts below 20. The only way is the collapse into Gas and Other.
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#collapse some of the classes with some other classes. 
data[data$Heating == 'GasA', 'Heating'] <- 'Gas'
data[(data$Heating == 'Floor' | 
        data$Heating == 'Grav' |
        data$Heating == 'OthW' |
        data$Heating == 'Wall' |
        data$Heating == 'GasW'), 'Heating'] <- 'Other'
table(data$Heating) #No more classes under 20. 
ggplot(data,aes(Heating)) + geom_bar()
data$Heating <- factor(data$Heating)


#####Heating##### No missing value
table(data$HeatingQC) #Some classes count under 20. 
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#collapse some of the classes with some other classes. 
data[(data$HeatingQC == 'Po' | 
        data$HeatingQC == 'Fa'), 'HeatingQC'] <- 'Fa'
table(data$HeatingQC) #No more classes under count 20. 
ggplot(data,aes(HeatingQC)) + geom_bar()
data$HeatingQC <- factor(data$HeatingQC, 
                            ordered=TRUE, 
                            levels=c('Fa','Gd','TA','Ex'))
#Look at the best fit line.
ggplotRegression(lm(SalePrice~HeatingQC, data)) #Positively correlate to response.


#####CentralAir##### No missing value
table(data$CentralAir) #no class counts under 20. 
ggplot(data,aes(CentralAir)) + geom_bar()
#Factorize
data$CentralAir <- factor(data$CentralAir)


#####Electrical##### Missing value: 0.034%
table(data$Electrical) #Some class counts under 20
data[is.na(data$Electrical), 'Electrical'] <- 'SBrkr'
ggplot(data,aes(Electrical)) + geom_bar()

data[(data$Electrical == 'FuseA' | 
        data$Electrical == 'FuseP' |
        data$Electrical == 'FuseF' |
        data$Electrical == 'Mix'), 'Electrical'] <- 'Fuse'
table(data$Electrical) #no more class counts under 20.
ggplot(data,aes(Electrical)) + geom_bar()
#Factorize
data$Electrical <- factor(data$Electrical)


#####X1stFlrSF##### No missing value
#Convert dtype to numeric.
data$X1stFlrSF <- as.numeric(data$X1stFlrSF)
#Summary statistics
summary(data$X1stFlrSF)
#Fit the best fit line.
ggplotRegression(lm(SalePrice~X1stFlrSF, data)) #There is definitely an outlier above the 4000 mark.
#Impute the outlier with the median. 
data[data$X1stFlrSF > 4000, 'X1stFlrSF'] <- median(data$X1stFlrSF)
#Data normality.
ggplot(data,aes(X1stFlrSF)) + geom_density() #Data is right skewed but it looks sqrt or log transformable.
#Density plot to compare log and sqrt transformation.
ggplot(data,aes(sqrt(X1stFlrSF))) + geom_density()
ggplot(data,aes(X1stFlrSF^(1/3))) + geom_density()
ggplot(data,aes(log(X1stFlrSF))) + geom_density() #Log transformation works the best in this case.
#Transform.
data$X1stFlrSF <- log(data$X1stFlrSF)
ggplotRegression(lm(SalePrice~X1stFlrSF, data))



#####X2ndFlrSF##### No missing value
#Convert dtype to numeric.
data$X2ndFlrSF <- as.numeric(data$X2ndFlrSF)
#Summary statistics
summary(data$X2ndFlrSF)
#Fit the best fit line.
ggplotRegression(lm(SalePrice~X2ndFlrSF, data)) #No obvious outliers.
#Data normality.
ggplot(data,aes(X2ndFlrSF)) + geom_density() 
#Density plot to compare log and sqrt transformation.
ggplot(data,aes(sqrt(X2ndFlrSF))) + geom_density()
ggplot(data,aes(X2ndFlrSF^(1/3))) + geom_density()
#Add a small number to avoid making 0s to undefined.
ggplot(data,aes(log(X2ndFlrSF+0.01))) + geom_density() #Although it is far from being normalized, sqaurerooot
#transformation works the best in this case. Better than no transformation.
#Transform.
data$X2ndFlrSF <- sqrt(data$X2ndFlrSF)
ggplotRegression(lm(SalePrice~X2ndFlrSF, data))
#After transformation, this feature correlate to response variable less.
#There is no way to keep the initial distribution, because it is too skewed from the begginging.
#Drop the feature, so that i won't affect the model.
data <- subset(data, select=-X2ndFlrSF)


#####LowQualFinSF##### No missing value
#Convert dtype to numeric.
data$LowQualFinSF <- as.numeric(data$LowQualFinSF)
#Summary statistics
summary(data$LowQualFinSF)
#Fit the best fit line.
ggplotRegression(lm(SalePrice~LowQualFinSF, data)) #Seems to be outliers in above 550.
#Impute the outlier with the median. 
data[data$LowQualFinSF > 550, 'LowQualFinSF'] <- median(data$LowQualFinSF)
ggplotRegression(lm(SalePrice~LowQualFinSF, data))  #best fit line is horizontal.
#Indicates that this feature should not add much influence in the response variable.
data <- subset(data, select=-LowQualFinSF)


#####GrLivArea##### No missing value
#Convert dtype to numeric.
data$GrLivArea <- as.numeric(data$GrLivArea)
#Summary statistics
summary(data$GrLivArea)
#Fit the best fit line.
ggplotRegression(lm(SalePrice~GrLivArea, data)) #Seems to be outliers in above 4500.
#Impute the outlier with the median. 
data[data$GrLivArea > 4500, 'GrLivArea'] <- median(data$GrLivArea)
#Data normality.
ggplot(data,aes(GrLivArea)) + geom_density() #Data is right skewed also not transformable
#Because it has a lot og 0s.
#Density plot to compare log and sqrt transformation.
ggplot(data,aes(sqrt(GrLivArea))) + geom_density()
ggplot(data,aes(GrLivArea^(1/3))) + geom_density()
ggplot(data,aes(log(GrLivArea))) + geom_density() #cube root transformation works the best in this case.
#Transform.
data$GrLivArea <- data$GrLivArea^(1/3)
#Check the best fit line again .
ggplotRegression(lm(SalePrice~GrLivArea, data))


#####BsmtFullBath##### Missing value: 0.06%
#Count of full bathrooms in the basement. Ordinal feature so keep datatypes as integer.
#Frequency table.
table(data$BsmtFullBath)
#Impute missing value with the most frequent value: 0.
data[is.na(data$BsmtFullBath), 'BsmtFullBath'] <- 0
ggplot(data,aes(BsmtFullBath)) + geom_bar()

data$BsmtFullBath <- factor(data$BsmtFullBath, 
                            ordered=TRUE, 
                            levels=c(0,1,2,3))
ggplotRegression(lm(SalePrice~BsmtFullBath, data)) #This should correlate by general knowledge, but the distribution of the data is making the best fit slope to almost horizontal.
#Indicates that this feature should not add much influence in the response variable.
data <- subset(data, select=-BsmtFullBath)

#####BsmtHalfBath##### Missing value: 0.06%
#Count of half bathrooms in the basement.
#Frequency table.
table(data$BsmtHalfBath)
#Impute missing value with the most frequent value: 0.
data[is.na(data$BsmtHalfBath), 'BsmtHalfBath'] <- 0
ggplot(data,aes(BsmtHalfBath)) + geom_bar()

data$BsmtHalfBath <- factor(data$BsmtHalfBath, 
                            ordered=TRUE, 
                            levels=c(0,1,2))
ggplotRegression(lm(SalePrice~BsmtHalfBath, data)) #This should correlate by general knowledge, but the distribution of the data is making the best fit slope to almost horizontal.
#Indicates that this feature should not add much influence in the response variable.
data <- subset(data, select=-BsmtHalfBath)


#####FullBath##### No missing value
#Count of full bathrooms in the whole house. Ordinal feature so keep datatypes as integer.
#Frequency table.
table(data$FullBath)
#Impute missing value with the most frequent value: 2.
data[is.na(data$FullBath), 'FullBath'] <- 2
ggplot(data,aes(FullBath)) + geom_bar()
#Impute the rating of 4 with 3  and 0 with 1since the count is too small for that class.
data[data$FullBath == 4, 'FullBath'] <- 3
data[data$FullBath == 0, 'FullBath'] <- 1
data$FullBath <- factor(data$FullBath, 
                        ordered=TRUE, 
                        levels=c(1,2,3))
ggplotRegression(lm(SalePrice~FullBath, data)) #This featuer positively Correlate to the response.


#####HalfBath##### No missing value
#Count of half bathrooms in the whole house. Ordinal feature so keep datatypes as integer.
#Frequency table.
table(data$HalfBath)
ggplot(data,aes(HalfBath)) + geom_bar()
#Impute 2 with 1 since 2 has a little small count. It might improve the correlation. 
data[data$HalfBath == 2, 'HalfBath'] <- 1
data$HalfBath <- factor(data$HalfBath, 
                        ordered=TRUE, 
                        levels=c(0,1))
ggplotRegression(lm(SalePrice~HalfBath, data)) #This feature slightly positively Correlate to the response.


#####BedroomAbvGr##### No missing value
table(data$BedroomAbvGr) #Some classes count under 20. 
ggplot(data,aes(BedroomAbvGr)) + geom_bar()
#Missing a level:7
#Downgrade all 8th level into 7
data[data$BedroomAbvGr == 8, 'BedroomAbvGr'] <- 7
ggplot(data,aes(BedroomAbvGr)) + geom_bar()
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#collapse some of the classes with some other classes. 
data[data$BedroomAbvGr == 0, 'BedroomAbvGr'] <- 1
data[data$BedroomAbvGr == 6, 'BedroomAbvGr'] <- 5
data[data$BedroomAbvGr == 7, 'BedroomAbvGr'] <- 5
table(data$BedroomAbvGr) #No more class count under 20. 
#Factorize Ordinal
data$BedroomAbvGr <- factor(data$BedroomAbvGr, 
                            ordered=TRUE, 
                            levels=c(0,1,2,3,4,5,6,7))
ggplotRegression(lm(SalePrice~BedroomAbvGr, data))


#####KitchenAbvGr##### No missing value
table(data$KitchenAbvGr)
ggplot(data,aes(KitchenAbvGr)) + geom_bar()
data[data$KitchenAbvGr == 0, 'KitchenAbvGr'] <- 1
data[data$KitchenAbvGr == 3, 'KitchenAbvGr'] <- 2
data$KitchenAbvGr <- factor(data$KitchenAbvGr, 
                            ordered=TRUE, 
                            levels=c(1,2))
ggplotRegression(lm(SalePrice~KitchenAbvGr, data)) #This feature slightly positively Correlate to the response.

#####KitchenQual##### Missing value: 0.034%
table(data$KitchenQual)
ggplot(data,aes(KitchenQual)) + geom_bar()
data$KitchenQual <- factor(data$KitchenQual, 
                           ordered=TRUE, 
                           levels=c('TA','Gd','Ex'))
#For missing data just assign TA which is the most frequent. 
data[is.na(data$KitchenQual), 'KitchenQual'] <- 'TA'
ggplotRegression(lm(SalePrice~KitchenQual, data)) #This feature positively Correlate to the response.

#####TotRmsAbvGrd##### No missing value
table(data$TotRmsAbvGrd) #Some classes count under 20
ggplot(data,aes(TotRmsAbvGrd)) + geom_bar()
data[(data$TotRmsAbvGrd == 2 | 
        data$TotRmsAbvGrd == 3), 'TotRmsAbvGrd'] <- 4
data[(data$TotRmsAbvGrd == 11 | 
        data$TotRmsAbvGrd == 12 |
        data$TotRmsAbvGrd == 13 |
        data$TotRmsAbvGrd == 14 |
        data$TotRmsAbvGrd == 15), 'TotRmsAbvGrd'] <- 10
#Factorize ordinal
data$TotRmsAbvGrd <- factor(data$TotRmsAbvGrd, 
                            ordered=TRUE, 
                            levels=c(4,5,6,7,8,9,10))
ggplotRegression(lm(SalePrice~TotRmsAbvGrd, data)) #This feature positively Correlate to the response.

#####Functional##### Missing value: 0.06%
table(data$Functional) #Some classes under count 20.
ggplot(data,aes(Functional)) + geom_bar()
#First impute missing data with the most frequent class
data[is.na(data$Functional), 'Functional'] <- 'Typ'
#Since the class categories are extremely imbalanced for some, it is a better approach to 
#collapse some of the classes with some other classes. 
data[(data$Functional == 'Maj1' |
        data$Functional == 'Maj2' |
        data$Functional == 'Sev'), 'Functional'] <- 'Maj'
data[(data$Functional == 'Min1' |
        data$Functional == 'Min2'), 'Functional'] <- 'Min'
data[(data$Functional == 'Maj1' |
        data$Functional == 'Maj2'), 'Functional'] <- 'Maj'

data$Functional <- factor(data$Functional, 
                          ordered=TRUE, 
                          levels=c('Maj','Mod','Min','Typ'))
ggplotRegression(lm(SalePrice~Functional, data)) #Does not seemt to correlate alot.


#####Fireplaces##### No missing value
table(data$Fireplaces) #Some classes count under 20. 
ggplot(data,aes(Fireplaces)) + geom_bar()
#Collapse some rank to have fewer rank class but with more datapoints.
data[(data$Fireplaces == 3 | 
        data$Fireplaces == 4), 'Fireplaces'] <- 2
table(data$Fireplaces) #no more class under 20
#Factorize ordinal
data$Fireplaces <- factor(data$Fireplaces, 
                          ordered=TRUE, 
                          levels=c(0,1,2))
#Look at the best fit line.
ggplotRegression(lm(SalePrice~Fireplaces, data))  #This feature positively Correlate to the response.


#####FireplaceQu##### Missing value: 48%
table(data$FireplaceQu)
ggplot(data,aes(FireplaceQu)) + geom_bar()
data[is.na(data$FireplaceQu), 'FireplaceQu'] <- 'None'
#Factorize ordinal
data$FireplaceQu <- factor(data$FireplaceQu, 
                           ordered=TRUE, 
                           levels=c('None','Po','Fa','TA','Gd','Ex'))
#Look at the best fit line.
ggplotRegression(lm(SalePrice~FireplaceQu, data))  #This feature positively Correlate to the response.

#####GarageType##### Missing value: 5.3%
table(data$GarageType) #Some class count of under 20.
ggplot(data,aes(GarageType)) + geom_bar()
data[is.na(data$GarageType), 'GarageType'] <- 'None'
#Collapse some classes into broader classes
data[(data$GarageType == 'Attchd' | 
        data$GarageType == 'BuiltIn' |
        data$GarageType == 'Basment'), 'GarageType'] <- 'HomeAttach'
data[(data$GarageType == 'Detchd' | 
        data$GarageType == 'CarPort' |
        data$GarageType == '2Types'), 'GarageType'] <- 'HomeDettach'

table(data$GarageType) #no more class count of under 20
ggplot(data,aes(GarageType)) + geom_bar()
#Factorize
data$GarageType <- factor(data$GarageType)


#####GarageYrBlt##### Missing value: 5.4%
ggplot(data,aes(GarageYrBlt)) + geom_bar() 
table(data$GarageYrBlt)
#For missing values, impute with the median of the feature (floored).
data[(is.na(data$GarageYrBlt)), 'GarageYrBlt'] <- floor(median(data[(!is.na(data$GarageYrBlt)), 'GarageYrBlt']))
#1 value with 2207. Impute the number to 2007.
data[data$GarageYrBlt == 2207, 'GarageYrBlt'] <- 2007
data$GarageYrBlt <- as.numeric(data$GarageYrBlt)
#Fit regression line
ggplotRegression(lm(SalePrice~GarageYrBlt, data)) 
#Distribution plot.
ggplot(data, aes(x=GarageYrBlt)) + geom_density() #Skewed to the left

#####GarageFinish##### Missing value: 5.4%
table(data$GarageFinish) #no class count of under 20. 
ggplot(data,aes(GarageFinish)) + geom_bar()
data[is.na(data$GarageFinish), 'GarageFinish'] <- 'None'
#Factorize
data$GarageFinish <- factor(data$GarageFinish)

#####GarageCars##### Missing value: 0.034%
table(data$GarageCars)
ggplot(data,aes(GarageCars)) + geom_bar()
data[is.na(data$GarageCars), 'GarageCars'] <- 2
#Collapse some rank to have fewer rank class but with more datapoints.
data[(data$GarageCars == 4 | 
        data$GarageCars == 5), 'GarageCars'] <- 3
data$GarageCars <- factor(data$GarageCars, 
                          ordered=TRUE, 
                          levels=c(0,1,2,3))
ggplotRegression(lm(SalePrice~GarageCars, data))  #This feature positively Correlate to the response.


#####GarageArea##### Missing value: 0.034%
#Convert dtype to numeric.
data$GarageArea <- as.numeric(data$GarageArea)
#Summary statistics
summary(data$GarageArea)
#First impute.
data[(is.na(data$GarageArea)), 'GarageArea'] <- median(data[(!is.na(data$GarageArea)), 'GarageArea'])
#Fit the best fit line.
ggplotRegression(lm(SalePrice~GarageArea, data)) #Seems to be outliers in above 1250.
#Impute the outlier with the median. 
data[data$GarageArea > 1250, 'GarageArea'] <- median(data$GarageArea)
#Data normality.
ggplot(data,aes(GarageArea)) + geom_density() #Data is right skewed but it looks sqrt or log transformable.
#Density plot to compare log and sqrt transformation.
ggplot(data,aes(sqrt(GarageArea))) + geom_density()
ggplot(data,aes(GarageArea^(1/3))) + geom_density()
#Add a small number to avoid making 0s to undefined.
ggplot(data,aes(log(GarageArea+0.01))) + geom_density()  #The original distribution seems to be normal than that of
#all the transformed distribution. Do not transform.
#Check the best fit line again .
ggplotRegression(lm(SalePrice~GarageArea, data))

#####GarageQual##### Missing value: 5.4%
table(data$GarageQual) #Some class counts under 20
ggplot(data,aes(GarageQual)) + geom_bar()
data[is.na(data$GarageQual), 'GarageQual'] <- 'None'
#Collapse some rank to have fewer rank class but with more datapoints.
data[data$GarageQual == 'Po', 'GarageQual'] <- 'Fa'
data[data$GarageQual == 'Ex', 'GarageQual'] <- 'Gd'
#Factorize ordinal
data$GarageQual <- factor(data$GarageQual, 
                          ordered=TRUE, 
                          levels=c('None','Fa','TA','Gd'))
table(data$GarageQual) #No more class counts under 20.
ggplotRegression(lm(SalePrice~GarageQual, data))  #This feature slightly positively Correlate to the response.


#####GarageCond##### Missing value: 5.4%
table(data$GarageCond)
ggplot(data,aes(GarageCond)) + geom_bar()
data[is.na(data$GarageCond), 'GarageCond'] <- 'None'
#Collapse some rank to have fewer rank class but with more datapoints.
data[data$GarageCond == 'Po', 'GarageCond'] <- 'Fa'
data[(data$GarageCond == 'Ex' |
        data$GarageCond == 'Gd'), 'GarageCond'] <- 'TA'
#Factorize Ordinal
data$GarageCond <- factor(data$GarageCond, 
                          ordered=TRUE, 
                          levels=c('None','Fa','TA'))

ggplotRegression(lm(SalePrice~GarageCond, data))  #This feature slightly positively Correlate to the response.


#####PavedDrive##### No missing value
table(data$PavedDrive) #no class counts under 20
ggplot(data,aes(PavedDrive)) + geom_bar()
data[is.na(data$PavedDrive), 'PavedDrive'] <- 'Y'
#Factorize
data$PavedDrive <- factor(data$PavedDrive)

#####WoodDeckSF##### No missing value
#Convert dtype to numeric.
data$WoodDeckSF <- as.numeric(data$WoodDeckSF)
#Summary statistics
summary(data$WoodDeckSF)
#Fit the best fit line.
ggplotRegression(lm(SalePrice~WoodDeckSF, data)) #Does not seem to have obvious outliers.
#Data normality.
ggplot(data,aes(WoodDeckSF)) + geom_density() #Data is right skewed but it looks sqrt or log transformable.
#Density plot to compare log and sqrt transformation.
ggplot(data,aes(sqrt(WoodDeckSF))) + geom_density()
ggplot(data,aes(WoodDeckSF^(1/3))) + geom_density()
#Add a small number to avoid making 0s to undefined.
ggplot(data,aes(log(WoodDeckSF+0.01))) + geom_density()  #Cuberoot transformation works the best in this case.
#Transform
data$WoodDeckSF <- data$WoodDeckSF^(1/3)
#Check the best fit line.
ggplotRegression(lm(SalePrice~WoodDeckSF, data)) 


#####OpenPorchSF##### No missing value
#Convert dtype to numeric.
data$OpenPorchSF <- as.numeric(data$OpenPorchSF)
#Summary statistics
summary(data$OpenPorchSF)
#Fit the best fit line.
ggplotRegression(lm(SalePrice~OpenPorchSF, data)) #Seems to be outliers in above 400.
#Impute the outlier with the median. 
data[data$OpenPorchSF > 400, 'OpenPorchSF'] <- median(data$OpenPorchSF)
#Data normality.
ggplot(data,aes(OpenPorchSF)) + geom_density() #Data is right skewed but it looks sqrt or log transformable.
#Density plot to compare log and sqrt transformation.
ggplot(data,aes(sqrt(OpenPorchSF))) + geom_density()
ggplot(data,aes(OpenPorchSF^(1/3))) + geom_density()
#Add a small number to avoid making 0s to undefined.
ggplot(data,aes(log(WoodDeckSF+0.01))) + geom_density() #Cube root transformation works the best.
#Transform
data$OpenPorchSF <- data$OpenPorchSF^(1/3)
#Check the best fit line again
ggplotRegression(lm(SalePrice~OpenPorchSF, data)) 



#####EnclosedPorch##### No missing value
#Convert dtype to numeric.
data$EnclosedPorch <- as.numeric(data$EnclosedPorch)
#Summary statistics
summary(data$EnclosedPorch)
#Fit the best fit line.
ggplotRegression(lm(SalePrice~EnclosedPorch, data)) #Seems to be outliers in above 400.
#Impute the outlier with the median. 
data[data$EnclosedPorch > 400, 'EnclosedPorch'] <- median(data$EnclosedPorch)
#Data normality.
ggplot(data,aes(EnclosedPorch)) + geom_density() #Data is right skewed but it looks sqrt or log transformable.
#Density plot to compare log and sqrt transformation.
ggplot(data,aes(sqrt(OpenPorchSF))) + geom_density()
ggplot(data,aes(OpenPorchSF^(1/3))) + geom_density()
#Add a small number to avoid making 0s to undefined.
ggplot(data,aes(log(OpenPorchSF+0.01))) + geom_density() #Cube transofrmaiton works the best.
#Check the best fit line
ggplotRegression(lm(SalePrice~EnclosedPorch, data))
data <- subset(data, select=-EnclosedPorch)


#####X3SsnPorch##### No missing value
#Convert dtype to numeric.
data$X3SsnPorch <- as.numeric(data$X3SsnPorch)
#Summary statistics
summary(data$X3SsnPorch)
#Fit the best fit line.
ggplotRegression(lm(SalePrice~X3SsnPorch, data)) #Seems to have no obvious outliers. 
#Data normality.
ggplot(data,aes(X3SsnPorch)) + geom_density()
#Density plot to compare log and sqrt transformation.
ggplot(data,aes(sqrt(X3SsnPorch))) + geom_density()
ggplot(data,aes(X3SsnPorch^(1/3))) + geom_density()
#Add a small number to avoid making 0s to undefined.
ggplot(data,aes(log(X3SsnPorch+0.01))) + geom_density() #No way to tranform this feature.
#This feature is too skewed because of the exccessive 0s
#, should not be included as one 
#of predictors in some models.
data <- subset(data, select=-X3SsnPorch)

#####ScreenPorch##### no missing value
#Convert dtype to numeric.
data$ScreenPorch <- as.numeric(data$ScreenPorch)
#Summary statistics
summary(data$ScreenPorch)
#Fit the best fit line.
ggplotRegression(lm(SalePrice~ScreenPorch, data)) #There seems to be no obvious outliers.
#Data normality.
ggplot(data,aes(ScreenPorch)) + geom_density()
#Density plot to compare log and sqrt transformation.
ggplot(data,aes(sqrt(ScreenPorch))) + geom_density()
ggplot(data,aes(ScreenPorch^(1/3))) + geom_density()
#Add a small number to avoid making 0s to undefined.
ggplot(data,aes(log(ScreenPorch+0.01))) + geom_density() #No way to tranform this feature.
#This feature is too skewed because of the excessive 0s, should not be included as one 
#of predictors in some models.

#Although this feature is very skewed and cannot be part of the continuous variable,
#it is still a very inforrmation rich feature.
#If the there is a screen porch, then price tend to be higher.
#Create another feture into a nominal feature indicating whether there is a porch or not.
#0: screen porch present, 1: no screen porch
data <- data %>% 
  mutate(ScreenPorchYN = if_else(ScreenPorch == 0, "N", "Y"))
table(data$ScreenPorchYN)
data$ScreenPorchYN <- factor(data$ScreenPorchYN)

#####PoolArea##### No missing value
#Convert dtype to numeric.
data$PoolArea <- as.numeric(data$PoolArea)
#Summary statistics
summary(data$PoolArea)
#Fit the best fit line.
ggplotRegression(lm(SalePrice~PoolArea, data)) #There seems to be no obvious outliers.
#Data normality.
ggplot(data,aes(PoolArea)) + geom_density()
#Although this feature is very skewed and cannot be part of the continuous variable,
#it is still a very inforrmation rich feature.
#If the there is a pool, then price tend to be higher.
#Create another feture into a nominal feature indicating whether there is a pool or not.
#0: pool present, 1: no pool
data <- data %>% 
  mutate(PoolYN = if_else(PoolArea == 0, "N", "Y"))
table(data$PoolAreaYN)
#Classes are extremely imbalanced, but each classes are not collapsable. 
#This is going to create error when testing a model.
#Drop the column to avoid the error.
data <- subset(data, select=-PoolYN)

#####PoolQC##### Missing value: 99%
table(data$PoolQC)
ggplot(data,aes(PoolQC)) + geom_bar()
data[is.na(data$PoolQC), 'PoolQC'] <- 'None'
data$PoolQC <- factor(data$PoolQC, 
                      ordered=TRUE, 
                      levels=c('None','Fa','Gd','Ex'))
ggplotRegression(lm(SalePrice~PoolQC, data))  #This feature should correlate to response from the general knowlwedge but does not because of the extreme amount of Nones
#Drop the column to avoid the error.
data <- subset(data, select=-PoolQC)


#####Fence##### Mising value: 80%
table(data$Fence) #MnWw is has count under 20
ggplot(data,aes(Fence)) + geom_bar()
data[is.na(data$Fence), 'Fence'] <- 'None'
#Collape MnWw with MnPrv
data[data$Fence == 'MnWw', 'Fence'] <- 'MnPrv'
table(data$Fence) #No more class count under 20
data$Fence <- factor(data$Fence, 
                     ordered=TRUE, 
                     levels=c('None','GdWo','MnPrv','GdPrv'))

ggplotRegression(lm(SalePrice~Fence, data)) #This feature should correlate to response from the general knowlwedge but does not because of the extreme amount of Nones


#####MiscFeature##### Missing value: 96%
table(data$MiscFeature) #Class counts under 20
ggplot(data,aes(MiscFeature)) + geom_bar()
data[is.na(data$MiscFeature), 'MiscFeature'] <- 'None'
#Reassign classses to if there is a Miscelenious feature or not.
data[(data$MiscFeature == 'Gar2' | 
        data$MiscFeature == 'Othr' |
        data$MiscFeature == 'Shed' |
        data$MiscFeature == 'TenC'), 'MiscFeature'] <- 'Yes'

table(data$MiscFeature) #No more class counts under 20
ggplot(data,aes(MiscFeature)) + geom_bar()
#Factorize
data$MiscFeature <- factor(data$MiscFeature)

#####MiscVal##### No missing value
#Convert dtype to numeric.
data$MiscVal <- as.numeric(data$MiscVal)
#Summary statistics
summary(data$MiscVal)
#Look for outliers.
ggplot(data, aes(MiscVal, SalePrice)) + geom_point() #Value of 0 indicating there are no miscelanious feature.
#Since whether miscelanous feature is present is represented in the MiscFeature column, just drop this column.
data <- subset(data, select = -c(MiscVal))


#####MoSold##### No missing value
table(data$MoSold)
ggplot(data,aes(MoSold)) + geom_bar()
#factorize the months
data$MoSold <- factor(data$MoSold)


#####YrSold##### No missing value
data$YrSold <- as.numeric(data$YrSold)
#Summary statistics
summary(data$YrSold)
#Fit regression line
ggplotRegression(lm(SalePrice~YrSold, data)) #The best fit line is almost tottally horizontal.
#Will not have effect on the SalePrice.
#Distribution plot.
ggplot(data, aes(x=YrSold)) + geom_density() 
#Drop feture since it doe not do anything to the response
data <- subset(data, select=-YrSold)

#####SaleType##### No missing value
table(data$SaleType) #Some class counts under 20
ggplot(data,aes(SaleType)) + geom_bar()
#Impute missing value.
data[is.na(data$SaleType), 'SaleType'] <- 'WD'
#Change categories
data[(data$SaleType == 'WD' | 
        data$SaleType == 'CWD'), 'SaleType'] <- 'WarrantyDeed'
data[(data$SaleType == 'COD' | 
        data$SaleType == 'Oth'), 'SaleType'] <- 'Other'
data[(data$SaleType == 'Con' | 
        data$SaleType == 'ConLw' |
        data$SaleType == 'ConLI' |
        data$SaleType == 'ConLD'), 'SaleType'] <- 'Contract'
#factorize the months
data$SaleType <- factor(data$SaleType)

table(data$SaleType) #No more class counts under 20 
ggplot(data,aes(SaleType)) + geom_bar()


#####SaleCondition#####
table(data$SaleCondition) #some class counts under 20
ggplot(data,aes(SaleCondition)) + geom_bar()

data[(data$SaleCondition == 'AdjLand' | 
        data$SaleCondition == 'Alloca' |
        data$SaleCondition == 'Family' |
        data$SaleCondition == 'Partial'), 'SaleCondition'] <- 'Other'
table(data$SaleCondition) #no more class counts under 20
ggplot(data,aes(SaleCondition)) + geom_bar()
#factorize the months
data$SaleCondition <- factor(data$SaleCondition)








##########################
#####Data Preparation#####
##########################
#Set seed so the results are always consistent. 
set.seed(777) #Lucky Number please.


#Train-Test-Split
train <- data[(!is.na(data$SalePrice)),]
test <- data[is.na(data$SalePrice),]


#Take out ID column
ID_Train <- train[,'Id']
ID_Test <- test[,'Id']
train <- subset(train, select=-Id)
test <- subset(test, select=-Id)

#Split X and Y for train and test.
x_train<-train[,!names(train)=='SalePrice']
y_train<-train[,names(train)=='SalePrice']
x_test<-test[,!names(test)=='SalePrice']
y_test<-test[,names(test)=='SalePrice']


###One Hot Encoded Data###
#Encoding: https://machinelearningmastery.com/one-hot-encoding-for-categorical-data/
library(onehot)
#Encode Train and test

#Get the encoded data train
data_onehot_train<-(predict(onehot(x_train,stringsAsFactors = TRUE, max_levels = 1000),x_train))
#Get the encoded data test
data_onehot_test<-(predict(onehot(x_test,stringsAsFactors = TRUE, max_levels = 1000),x_test))


#PreTrain-Validation-Split
#Get index of all pre-training data with 70:30 proportion. 
pre_train_index <- sample(seq_len(nrow(train)), size=floor(0.8*nrow(train)))

#Next split the dataset using the index created. This is for elastic net.
x_pre_train_onehot <- data_onehot_train[pre_train_index,]
y_pre_train <- y_train[pre_train_index]
x_valid_onehot <- data_onehot_train[-pre_train_index,]
y_valid <- y_train[-pre_train_index]





###Data for linear Regression/Tree...ect###
#For linear models, the imbalanced feautres can cause a problem when in a prediction stage, since 
#training set can contain classes that are not contained in a testing set vise versa.
#In order to get around this, only select numerical features from the dataset, and create a dedicated dataset for OLS model.
#lm_data <- data[, unlist(lapply(data, is.numeric))]
#Splir to train and test and also exclude the Id column
##lm_train <- subset(lm_data[(!is.na(lm_data$SalePrice)),], select=-Id)
#lm_test <- subset(lm_data[is.na(lm_data$SalePrice),], select=-Id)
#Split the dataset to pre-train, validation.
#lm_pre_train <- lm_train[pre_train_index,]
#lm_validation <- lm_train[-pre_train_index,]
#For other models.
pre_train <- train[pre_train_index,]
validation <- train[-pre_train_index,]

###Data For SVM###
#SVM model only runs on a numeric and ordinal data.
#Create a data to inlcude only the numeric and ordinal variable.
svm_columns <- c('LotFrontage','LotArea','OverallQual','OverallCond',
                 'YearBuilt','YearRemod','MasVnrArea','ExterQual',
                 'ExterCond','BsmtQual','BsmtCond','BsmtExposure',
                 'BsmtFinType1','BsmtFinSF1','BsmtFinType2','BsmtFinSF2',
                 'BsmtUnfSF','TotalBsmtSF','HeatingQC','X1stFlrSF',
                 'GrLivArea','FullBath','HalfBath',
                 'BedroomAbvGr','KitchenAbvGr','KitchenQual','TotRmsAbvGrd',
                 'Functional','Fireplaces','FireplaceQu','GarageYrBlt','GarageCars',
                 'GarageArea','GarageQual','GarageCond','WoodDeckSF','OpenPorchSF',
                 'ScreenPorch','Fence','SalePrice')
data_svm<- data[ ,which((names(data) %in% svm_columns)==TRUE)]
#Separate train and test with data_svm.
#Train-Test-Split
train_svm <- data_svm[(!is.na(data_svm$SalePrice)),]
test_svm <- data_svm[is.na(data_svm$SalePrice),]
#NA values needs to be imputed with 0 in order for the prediction to work.
test_svm[is.na(test_svm$SalePrice),'SalePrice'] <- 0
#Split Pretrain and validation.
pre_train_svm <- train_svm[pre_train_index,]
validation_svm <- train_svm[-pre_train_index,]





##############
###Modeling###
##############
library(Metrics)
#####OLS(Plain Vanilla)/Feature Selection######
#Fit the model
lm_model <- lm(SalePrice~., pre_train)
summary(lm_model)
#Predict with model
lm_result <- predict(lm_model, newdata=validation)
#calculate error #Does not work, so I will comment this out so that the script runs without giving error. 
lm_error_validation <- rmsle(y_valid,lm_result)


# Perform the feature selection
library(MASS)
#Forward
lmfwd_model <- stepAIC(lm_model, direction="forward", na.action=na.remove)
summary(lmfwd_model)
#Predict with model
lmfwd_result <- predict(lmfwd_model, newdata=validation)
#calculate error #Does not work, so I will comment this out so that the script runs without giving error. 
lmfwd_error_validation <- rmsle(y_valid,lmfwd_result)

#Backward
lmbwd_model <- stepAIC(lm_model, direction="backward", na.action=na.remove)
summary(lmbwd_model)
#Predict with model
lmbwd_result <- predict(lmbwd_model, newdata=validation)
#calculate error #Does not work, so I will comment this out so that the script runs without giving error. 
lmbwd_error_validation <- rmsle(y_valid,lmbwd_result)

#Hybrid
lmhbd_model <- stepAIC(lm_model, direction="both", na.action=na.remove)
summary(lmhbd_model)
#Predict with model
lmhbd_result <- predict(lmhbd_model, newdata=validation)
#calculate error #Does not work, so I will comment this out so that the script runs without giving error. 
lmhbd_error_validation <- rmsle(y_valid,lmhbd_result)


#####Elastic-Net/Ridge/Lasso######
#Perform k-folds (10-fold) cross validation on the training set 
#using the pre-training and validation set to find the best lambda.
output_cv <- cv.glmnet(as.matrix(x_pre_train_onehot), as.matrix(y_pre_train), nlambda=10,lambda.min.ratio=0.0001)
plot(output_cv)
#Assign the minimum of lambda into best_lambda variable.
best_lambda <- output_cv$lambda.min

#Built model, predict on validation, and calculate RMSE.
#Ridge
ridge_model <- glmnet(x_pre_train_onehot, y_pre_train, alpha=0, lambda=best_lambda)
ridge_result <- predict(ridge_model, newx = x_valid_onehot)
ridge_error_validation <- rmsle(y_valid,ridge_result)
#Lasso
lasso_model <- glmnet(x_pre_train_onehot, y_pre_train, alpha=1, lambda=best_lambda)
lasso_result <- predict(lasso_model, newx = x_valid_onehot)
lasso_error_validation <- rmsle(y_valid,lasso_result)
#Elastic-net
#Initialize error vector.
error_vec <- c()
#Initialize alpha vector.
alpha_vec <- c()
for (i in seq(0.01, 0.99, by = 0.01)) {
  enet_model <- glmnet(x_pre_train_onehot, y_pre_train, alpha=i, lambda=best_lambda)
  enet_result <- predict(enet_model, newx = x_valid_onehot)
  enet_error_validation <- rmsle(y_valid,enet_result)
  error_vec <- c(error_vec, enet_error_validation)
  alpha_vec <- c(alpha_vec, i)
}
df_errors <- data.frame(error_vec,alpha_vec)
#Store best alpha value.
best_alpha <- df_errors[data.frame(error_vec, alpha_vec)$error_vec == 
                          min(data.frame(error_vec, alpha_vec)$error_vec), 'alpha_vec']
#create elastic-net model with the best alpha value.
enet_model <- glmnet(x_pre_train_onehot, y_pre_train, alpha=best_alpha, lambda=best_lambda)
enet_result <- predict(enet_model, newx = x_valid_onehot)
enet_error_validation <- rmsle(y_valid,enet_result)

#####Tree-based######
#Fit the model
tree_model<-tree(SalePrice~.,pre_train)
#Plot the model
plot(tree_model)
#Input text info to the tree plot
text(tree_model, pretty=1)
#Predict with model
tree_result <- predict(tree_model,newdata=validation)
#calculate error
tree_error_validation <- rmsle(y_valid, tree_result)



#####Normal Random forest######
library(randomForest)
#Fit model
rf_model <- randomForest(SalePrice~., pre_train, na.action = na.exclude)
#Predict with model
rf_result <- predict(rf_model, newdata=validation)
#Calculate error
rf_error_validation <- rmsle(y_valid, rf_result)



#####Bagging######
#Fit model
bag_model <- randomForest(SalePrice~., mtry=50, pre_train, na.action = na.exclude)
#Predict with model
bag_result <- predict(bag_model, newdata=validation)
#Calculate error
bag_error_validation <- rmsle(y_valid, bag_result)




#####Boosting#####
library(gbm)
#Fit model
boost_model <- gbm(SalePrice~., pre_train, distribution="gaussian")
#Predict with model
boost_result <- predict.gbm(boost_model, newdata=validation ,n.trees = 100)
#Calculate error
boost_error_validation<-rmsle(y_valid, boost_result)


#####SVM#####
library(e1071)
#Fit model (radial)
svmradial_model <- svm(SalePrice~., pre_train_svm, type="eps-regression",kernel="radial")
svmradial_result <- predict(svmradial_model, newdata=validation_svm)
svmradial_error_validation <- rmsle(y_valid, svmradial_result)
#Fit model (polynomial)
svmpoly_model <- svm(SalePrice~., pre_train_svm, type="eps-regression",kernel="polynomial")
svmpoly_result <- predict(svmpoly_model, newdata=validation_svm)
svmpoly_error_validation <- rmsle(y_valid, svmradial_result)
#Fit model (linear)
svmlinear_model <- svm(SalePrice~., pre_train_svm, type="eps-regression",kernel="linear")
svmlinear_result <- predict(svmlinear_model, newdata=validation)
svmlinear_error_validation <- rmsle(y_valid, svmradial_result)


#Model Comparison
#lm_error_validation
models <- c('Linear','Foward','Backward','Hybrid',
            'Ridge','Lasso','Elastic-Net','Tree','RandomForest', 
            'Bag',"Boost",'SVMRadial','SVMPolynomial','SVMLinear')
RMSE <- c( lm_error_validation,
           lmfwd_error_validation,
           lmbwd_error_validation,
           lmhbd_error_validation,
           ridge_error_validation,
           lasso_error_validation, 
           enet_error_validation,
           tree_error_validation,
           rf_error_validation,
           bag_error_validation,
           boost_error_validation,
           svmradial_error_validation,
           svmpoly_error_validation,
           svmlinear_error_validation)  #Does not include OLS error because could not figure out why the rmsle() for OLS did not work.

print(compare <- data.frame(models,RMSE))



##################
###Test predict###
##################

#OLS TEST RMSLE: 0.12582
#Fit the model
lm_model_test <- lm(SalePrice~., train)
summary(lm_model_test)
#Predict with model
lm_result_test <- predict(lm_model_test, newdata=test)
#Create a dataframe with the Id and back transformed prediction of house price
result<- data.frame(ID_Test,exp(lm_result_test))
result <- rename(result, 'Id' = 'ID_Test')
result <- rename(result, 'SalePrice' = 'exp.lm_result_test.')
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 1\\Final Results\\OLS_Result_V7.csv") 

#Forward TEST RMSLE: 0.12582
lmfwd_model_test <- stepAIC(lm_model_test, direction="forward", na.action=na.remove)
summary(lmfwd_model_test)
#Predict with model
lmfwd_result_test <- predict(lmfwd_model_test, newdata=test)
#Create a dataframe with the Id and back transformed prediction of house price
result<- data.frame(ID_Test,exp(lmfwd_result_test))
result <- rename(result, 'Id' = 'ID_Test')
result <- rename(result, 'SalePrice' = 'exp.lmfwd_result_test.')
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 1\\Final Results\\Forward_Result_V7.csv") 

#Backward TEST RMSLE: 0.12556
lmbwd_model_test <- stepAIC(lm_model_test, direction="backward", na.action=na.remove)
summary(lmbwd_model_test)
#Predict with model
lmbwd_result_test <- predict(lmbwd_model_test, newdata=test)
#Create a dataframe with the Id and back transformed prediction of house price
result<- data.frame(ID_Test,exp(lmbwd_result_test))
result <- rename(result, 'Id' = 'ID_Test')
result <- rename(result, 'SalePrice' = 'exp.lmbwd_result_test.')
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 1\\Final Results\\Backward_Result_V7.csv") 

#Hybrid TEST RMSLE: 0.12556
lmhbd_model_test <- stepAIC(lm_model_test, direction="both", na.action=na.remove)
summary(lmhbd_model_test)
#Predict with model
lmhbd_result_test <- predict(lmhbd_model_test, newdata=test)
#Create a dataframe with the Id and back transformed prediction of house price
result<- data.frame(ID_Test,exp(lmhbd_result_test))
result <- rename(result, 'Id' = 'ID_Test')
result <- rename(result, 'SalePrice' = 'exp.lmhbd_result_test.')
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 1\\Final Results\\Hybrid_Result_V7.csv") 


#Ridge TEST RMSLE: 0.15599
ridge_model_test <- glmnet(data_onehot_train, y_train, alpha=0, lambda=best_lambda)
ridge_result_test <- predict(ridge_model_test, newx = data_onehot_test)
#Create a dataframe with the Id and back transformed prediction of house price
result<- data.frame(ID_Test,exp(ridge_result_test))
result <- rename(result, 'Id' = 'ID_Test')
result <- rename(result, 'SalePrice' = 's0')
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 1\\Final Results\\Ridge_Result_V7.csv") 


#Lasso TEST RMSLE: 0.15603
lasso_model_test <- glmnet(data_onehot_train, y_train, alpha=1, lambda=best_lambda)
lasso_result_test <- predict(lasso_model_test, newx = data_onehot_test)
#Create a dataframe with the Id and back transformed prediction of house price
result<- data.frame(ID_Test,exp(lasso_result_test))
result <- rename(result, 'Id' = 'ID_Test')
result <- rename(result, 'SalePrice' = 's0')
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 1\\Final Results\\Lasso_Result_V7.csv") 


#Elastic-Net TEST RMSLE: 0.15509
enet_model_test <- glmnet(data_onehot_train, y_train, alpha=best_alpha, lambda=best_lambda)
enet_result_test <- predict(enet_model_test, newx = data_onehot_test)
#Create a dataframe with the Id and back transformed prediction of house price
result<- data.frame(ID_Test,exp(enet_result_test))
result <- rename(result, 'Id' = 'ID_Test')
result <- rename(result, 'SalePrice' = 's0')
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 1\\Final Results\\ElasticNet_Result_V7.csv") 


#Tree TEST RMSLE: 0.22498
tree_model_test <- tree(SalePrice~.,train)
#Plot the model
plot(tree_model_test)
#Input text info to the tree plot
text(tree_model_test, pretty=1)
#Predict with model
tree_result_test <- predict(tree_model_test,newdata=test)
#Create a dataframe with the Id and back transformed prediction of house price
result<- data.frame(ID_Test,exp(tree_result_test))
result <- rename(result, 'Id' = 'ID_Test')
result <- rename(result, 'SalePrice' = 'exp.tree_result_test.')
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 1\\Final Results\\Tree_Result_V7.csv") 


#Bagging TEST RMSLE: 0.14517
bag_model_test <- randomForest(SalePrice ~., mtry=50, train, na.action = na.exclude)
#Predict with model
bag_result_test <- predict(bag_model_test, newdata=test)
#Create a dataframe with the Id and back transformed prediction of house price
result<- data.frame(ID_Test,exp(bag_result_test))
result <- rename(result, 'Id' = 'ID_Test')
result <- rename(result, 'SalePrice' = 'exp.bag_result_test.')
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 1\\Final Results\\Bagging_Result_V7.csv") 


#Boosting TEST RMSLE: 0.14668
boost_model_test <- gbm(SalePrice~., train, distribution="gaussian")
#Predict with model
boost_result_test <- predict.gbm(boost_model_test, newdata=test ,n.trees = 100)
#Create a dataframe with the Id and back transformed prediction of house price
result<- data.frame(ID_Test,exp(boost_result_test))
result <- rename(result, 'Id' = 'ID_Test')
result <- rename(result, 'SalePrice' = 'exp.boost_result_test.')
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 1\\Final Results\\Boosting_Result_V7.csv") 


#Random Forest TEST RMSLE: 0.14619
rf_model_test <- randomForest(SalePrice~., train, na.action = na.exclude)
#Predict with model
rf_result_test <- predict(rf_model, newdata=test)
#Create a dataframe with the Id and back transformed prediction of house price
result<- data.frame(ID_Test,exp(rf_result_test))
result <- rename(result, 'Id' = 'ID_Test')
result <- rename(result, 'SalePrice' = 'exp.rf_result_test.')
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 1\\Final Results\\RandomForest_Result_V7.csv") 


#SVM(radial) TEST RMSLE: 0.13688
svmradial_model_test <- svm(SalePrice~., train_svm, type="eps-regression",kernel="radial")
svmradial_result_test <- predict(svmradial_model_test, newdata=test_svm)
#Create a dataframe with the Id and back transformed prediction of house price
result<- data.frame(ID_Test,exp(svmradial_result_test))
result <- rename(result, 'Id' = 'ID_Test')
result <- rename(result, 'SalePrice' = 'exp.svmradial_result_test.')
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 1\\Final Results\\SVMRadial_Result_V7.csv") 


#SVM(polynomial) TEST RMSLE: 0.19350
svmpolynomial_model_test <- svm(SalePrice~., train_svm, type="eps-regression",kernel="polynomial")
svmpolynomial_result_test <- predict(svmpolynomial_model_test, newdata=test_svm)
#Create a dataframe with the Id and back transformed prediction of house price
result<- data.frame(ID_Test,exp(svmpolynomial_result_test))
result <- rename(result, 'Id' = 'ID_Test')
result <- rename(result, 'SalePrice' = 'exp.svmpolynomial_result_test.')
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 1\\Final Results\\SVMPolynomial_Result_V7.csv") 


#SVM(linear) TEST RMSLE: 0.12863
svmlinear_model_test <- svm(SalePrice~., train_svm, type="eps-regression",kernel="linear")
svmlinear_result_test <- predict(svmlinear_model_test, newdata=test_svm)
#Create a dataframe with the Id and back transformed prediction of house price
result<- data.frame(ID_Test,exp(svmlinear_result_test))
result <- rename(result, 'Id' = 'ID_Test')
result <- rename(result, 'SalePrice' = 'exp.svmlinear_result_test.')
#write.csv(result, "C:\\Users\\taku0\\OneDrive\\??????????????????\\FL21\\BAT 3305 - Machine Learning\\Assignment 1\\Final Results\\SVMLinear_Result_V7.csv") 




#Reference:
#https://www.theanalysisfactor.com/outliers-to-drop-or-not-to-drop/
#https://towardsdatascience.com/outlier-why-is-it-important-af58adbefecc
