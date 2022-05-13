## Train and Test Split
```r
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

```
## One Hot Encoding
```r
###One Hot Encoded Data###
#Encoding: https://machinelearningmastery.com/one-hot-encoding-for-categorical-data/
library(onehot)
#Encode Train and test

#Get the encoded data train
data_onehot_train<-(predict(onehot(x_train,stringsAsFactors = TRUE, max_levels = 1000),x_train))
#Get the encoded data test
data_onehot_test<-(predict(onehot(x_test,stringsAsFactors = TRUE, max_levels = 1000),x_test))
```
## Pretrain and Validation Split
```r
#PreTrain-Validation-Split
#Get index of all pre-training data with 70:30 proportion. 
pre_train_index <- sample(seq_len(nrow(train)), size=floor(0.8*nrow(train)))

#Next split the dataset using the index created. This is for elastic net.
x_pre_train_onehot <- data_onehot_train[pre_train_index,]
y_pre_train <- y_train[pre_train_index]
x_valid_onehot <- data_onehot_train[-pre_train_index,]
y_valid <- y_train[-pre_train_index]
```
