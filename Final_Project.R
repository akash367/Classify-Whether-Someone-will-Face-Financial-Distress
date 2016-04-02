# Course		:  CS 513 Knowledge Discovery and Data Mining
# Developers:  Sweta Chowdary (10406940)
#              Ana Parra Vera (10402328)
#              Akash Chawla (10406187)
#              Soham Mukherjee (10409945)
# Purpose   :  Final Project

#DESCRIPTION
#Classify whether or not somebody will experience 
#financial distress in the next two years
#Data Source https://www.kaggle.com/c/GiveMeSomeCredit/data

###########################################################


## remove all objects
rm(list=ls())

#Use class library
library(class)
#Read Data from CSV file
train_org <- read.csv("cs-training.csv")

#Copy the data to another variable
train <-train_org

#to remove exponent formatting of values and display in decimal format
options(scipen=999) 

#Exploratory Data Analysis
summary(train$NumberOfDependents)
boxplot(train$NumberOfDependents)


summary(train$NumberRealEstateLoansOrLines)
boxplot(train$NumberRealEstateLoansOrLines)


summary(train$NumberOfTime30.59DaysPastDueNotWorse)
bp_30_59 <- boxplot(train$NumberOfTime30.59DaysPastDueNotWorse)
bp_30_59$out # a value 98 is depicting something. need to figure out. #269 values with 98
#269 values with 98

summary(train$DebtRatio)


summary(train$MonthlyIncome)

summary(train$NumberOfTimes90DaysLate)
bp_90 <- boxplot(train$NumberOfTimes90DaysLate)


summary(train$NumberOfOpenCreditLinesAndLoans)
bp_OCL <- boxplot(train$NumberOfOpenCreditLinesAndLoans)
bp_OCL$out


summary(train$RevolvingUtilizationOfUnsecuredLines)
bp_RUOUL <- boxplot(train$RevolvingUtilizationOfUnsecuredLines)
bp_RUOUL$out


summary(train$age)
boxplot(train$age) # there are a few age>100 and one age=0





# To predict missing values of monthly income
#create validation dataset from original dataset
validation_ds_income <- subset(train, is.na(train$MonthlyIncome))

summary(validation_ds_income$MonthlyIncome)


training_dataset <- train[-validation_ds_income$X,]
summary(training_dataset$MonthlyIncome)


set.seed(550)
test_ds_rows <- runif(0.3*nrow(clean_ds), min = 1, max = nrow(clean_ds))

test_ds <- clean_ds[test_ds_rows,]
summary(test_ds$MonthlyIncome)

#training_dataset <- train[-validation_ds_income$X,]
training_ds <- clean_ds[-test_ds_rows,]
summary(training_ds$MonthlyIncome)


##Define max-min normalization function
mmnorm <-function (x,minx,maxx) {
  z<-((x-minx)/(maxx-minx))
  return(z)
}

training_normalized <- training_ds
test_normalized <- test_ds



training_normalized <- mmnorm(training_ds[,cbind(3,4,5,8,9,10,11,12)], min(training_ds[,cbind(3,4,5,8,9,10,11,12)]), max(training_ds[,cbind(3,4,5,8,9,10,11,12)]))
test_normalized <- mmnorm(test_ds[,cbind(3,4,5,8,9,10,11,12)], min(test_ds[,cbind(3,4,5,8,9,10,11,12)]), max(test_ds[,cbind(3,4,5,8,9,10,11,12)]))



#use knn to predict income
predict_k20 <- knn(training_normalized,test_normalized,training_ds[,7], k=20)
predict_k10 <- knn(training_normalized,test_normalized,training_ds[,7], k=10)

df_income <- as.data.frame(predict_k20)


df_income$actual <- test_ds$MonthlyIncome

predict_k10 <- knn(training_normalized,test_normalized,training_ds[,7], k=10)
df_income$predict_k10 <- predict_k10
df_income$predict_k10 <- as.numeric(predict_k10)

predict_k05 <- knn(training_normalized,test_normalized,training_ds[,7], k=5)
df_income$predict_k05 <- predict_k05
df_income$predict_k05 <- as.numeric(predict_k05)

df_income$predict_k20 <- as.numeric(predict_k20)


df_income$errorPercent_k05 <- abs(df_income$actual - df_income$predict_k05)/(df_income$actual)
df_income$errorPercent_k10 <- abs(df_income$actual - df_income$predict_k10)/(df_income$actual)
df_income$errorPercent_k20 <- abs(df_income$actual - df_income$predict_k20)/(df_income$actual)


mean(df_income$errorPercent_k05) #73.09
mean(df_income$errorPercent_k10) #71.43
mean(df_income$errorPercent_k20) #69.59




predict_k30 <- knn(training_normalized,test_normalized,training_ds[,7], k=30)
df_income$predict_k30 <- predict_k30
df_income$predict_k30 <- as.numeric(predict_k30)
df_income$errorPercent_k30 <- abs(df_income$actual - df_income$predict_k30)/(df_income$actual)
mean(df_income$errorPercent_k30) #67.63


predict_k50 <- knn(training_normalized,test_normalized,training_ds[,7], k=50)
df_income$predict_k50 <- predict_k50
df_income$predict_k50 <- as.numeric(predict_k50)
df_income$errorPercent_k50 <- abs(df_income$actual - df_income$predict_k50)/(df_income$actual)
mean(df_income$errorPercent_k50) # 65.27


predict_k100 <- knn(training_normalized,test_normalized,training_ds[,7], k=100)
df_income$predict_k100 <- predict_k100
df_income$predict_k100 <- as.numeric(predict_k100)
df_income$errorPercent_k100 <- abs(df_income$actual - df_income$predict_k100)/(df_income$actual)
mean(df_income$errorPercent_k100) # 64.30



predict_k200 <- knn(training_normalized,test_normalized,training_ds[,7], k=200)
df_income$predict_k200 <- predict_k200
df_income$predict_k200 <- as.numeric(predict_k200)
df_income$errorPercent_k200 <- abs(df_income$actual - df_income$predict_k200)/(df_income$actual)
mean(df_income$errorPercent_k200) # 63.23

predict_k2 <- knn(training_normalized,test_normalized,training_ds[,7], k=2)
df_income$predict_k2 <- predict_k2
df_income$predict_k2 <- as.numeric(predict_k2)
df_income$errorPercent_k2 <- abs(df_income$actual - df_income$predict_k2)/(df_income$actual)
mean(df_income$errorPercent_k2) # 73.17

predict_k300 <- knn(training_normalized,test_normalized,training_ds[,7], k=300)
df_income$predict_k300 <- predict_k300
df_income$predict_k300 <- as.numeric(predict_k300)
df_income$errorPercent_k300 <- abs(df_income$actual - df_income$predict_k300)/(df_income$actual)
mean(df_income$errorPercent_k300) # 63.02


df_income$mean_actual_income<- mean(df_income$actual)
df_income$errorPercent_mean <- abs(df_income$actual - df_income$mean_actual_income)/(df_income$actual)
mean(df_income$errorPercent_mean) # 94.70


df_income$median_actual_income<- median(df_income$actual)
df_income$errorPercent_median <- abs(df_income$actual - df_income$median_actual_income)/(df_income$actual)
mean(df_income$errorPercent_median) # 72.95


#We decided not to implement KNN to predict missing values as error rate is very high



rm(list=ls())

library(class)

#Read CSV
train_org <- read.csv("cs-training.csv")

train_new <-train_org
options(scipen=999)
#Subset outliers
new_MI0_DR0 <- subset(train_new$X,train_new$RevolvingUtilizationOfUnsecuredLines > 3|(train_new$MonthlyIncome==0 & train_new$DebtRatio==0) | train_new$DebtRatio==0)

train <- train_new[- new_MI0_DR0,]

summary(train)
age_median <- median(train$age,na.rm=TRUE)

train$age <- ifelse(train$age==0,age_median,train$age)

number_of_real_estate <- median(train$NumberRealEstateLoansOrLines, na.rm=TRUE)

train$NumberRealEstateLoansOrLines <- ifelse(train$NumberRealEstateLoansOrLines==54,number_of_real_estate,train$NumberRealEstateLoansOrLines)

train$MonthlyIncome <- ifelse(is.na(train$MonthlyIncome),1,train$MonthlyIncome)
train$MonthlyIncome <- ifelse(train$MonthlyIncome==0,1,train$MonthlyIncome)
train$NumberOfDependents <- ifelse(is.na(train$NumberOfDependents),0,train$NumberOfDependents)

train$Expenditure <- train$MonthlyIncome*train$DebtRatio


mmnorm <- function(x) {return ((x-min(x, na.rm = TRUE))/(max(x, na.rm = TRUE) - 
                                                           min (x, na.rm = TRUE)))}


training_norm <-data.frame(apply(train,2,mmnorm))
training_norm <- subset(training_norm[,-c(1,6,7)])


set.seed(950)
idx <- sample (nrow(training_norm), as.integer(.70*nrow(training_norm)))


test_ds <- training_norm[-idx,]
training_ds <- training_norm[idx,]

summary(training_norm)

predict_knn_k50 <- knn(training_ds[,-1],test_ds[,-1],training_ds[,1], k=50)

table(predict_knn_k50,test_ds$SeriousDlqin2yrs )


predict_knn_k100 <- knn(training_ds[,-1],test_ds[,-1],training_ds[,1], k=100)
table(predict_knn_k100,test_ds$SeriousDlqin2yrs )

predict_knn_k20 <- knn(training_ds[,-1],test_ds[,-1],training_ds[,1], k=20)
table(predict_knn_k20,test_ds$SeriousDlqin2yrs )



training_cor <-  subset(training_ds[,-c(4,8)])
test_cor <-  subset(test_ds[,-c(4,8)])


predict_knn_cor_k50 <- knn(training_cor[,-1],test_cor[,-1],training_cor[,1], k=50)
table(predict_knn_cor_k50,test_ds$SeriousDlqin2yrs )


debtratio <- subset(train, train$DebtRatio==0)

table(debtratio$SeriousDlqin2yrs)

################# Decision TREE CART

set.seed(950)
idx_DT <- sample (nrow(train), as.integer(.70*nrow(train)))


test_dt <- train[-idx_DT,]
training_dt <- train[idx_DT,]


training_dt <-  subset(training_dt[,-c(1,6,7)])
test_dt <-  subset(test_dt[,-c(1,6,7)])

training_dt$SeriousDlqin2yrs <-ifelse(training_dt$SeriousDlqin2yrs==0,'CR_No','CR_Yes')
test_dt$SeriousDlqin2yrs <-ifelse(test_dt$SeriousDlqin2yrs==0,'CR_No','CR_Yes')

#install.packages(rpart.plot)
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

mytree <- rpart(training_dt$SeriousDlqin2yrs~., data=training_dt)

fancyRpartPlot(mytree)
test_dt_sub <- subset(test_dt[,-1])
Prediction <- data.frame(predict(mytree,test_dt_sub , type ='class'))

Prediction <-edit(Prediction)

table(Prediction$predict.mytree..test_dt_sub..type....class.., test_dt$SeriousDlqin2yrs)


################# Decision TREE C5.0

set.seed(950)
idx <- sample (nrow(train), as.integer(.70*nrow(train)))


test_ds <- train[-idx,]
training_ds <- train[idx,]

test_ds<- subset(test_ds[,-c(1,6,7,8,10,13)])
training_ds<- subset(training_ds[,-c(1,6,7,8,10,13)])



training_ds$SeriousDlqin2yrs <- factor(training_ds$SeriousDlqin2yrs)
C50_treeModel <- C5.0(x = training_ds[, -1], y = training_ds$SeriousDlqin2yrs)

C5imp(C50_treeModel)
C5imp(C50_treeModel, metric = "splits")

C50_treeModel_predict <- predict(C50_treeModel,test_ds[,-1])

table(x=C50_treeModel_predict,y=test_ds[,1])


#Implementation of ANN
library("neuralnet")
training_norm <-data.frame(apply(train,2,mmnorm))
training_norm <- subset(training_norm[,-1])

set.seed(950)
idx <- sample (nrow(training_norm), as.integer(.70*nrow(training_norm)))


test_ds <- training_norm[-idx,]
training_ds <- training_norm[idx,]

test_ds<- subset(test_ds[,-c(5,6)])
training_ds<- subset(training_ds[,-c(5,6)])




#Train the neural network
#Going to have 10 hidden layers
#Threshold is a numeric value specifying the threshold for the partial
#derivatives of the error function as stopping criteria.
weights_list <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)

ANN_model <- neuralnet(training_ds$SeriousDlqin2yrs~training_ds$RevolvingUtilizationOfUnsecuredLines+training_ds$age+training_ds$NumberOfTime30.59DaysPastDueNotWorse+training_ds$NumberOfOpenCreditLinesAndLoans+training_ds$NumberOfTimes90DaysLate+training_ds$NumberRealEstateLoansOrLines+training_ds$NumberOfTime60.89DaysPastDueNotWorse+training_ds$NumberOfDependents+training_ds$Debt,training_ds, hidden=20, threshold=0.01,
                       startweights = weights_list, stepmax = 1e+05)
print(ANN_model)


#Plot the neural network
plot(ANN_model)

test <- test_ds[,-1]
net.results <- compute(ANN_model, test) #Run them through the neural network

#Lets see what properties net.sqrt has
ls(net.results)

#Lets see the results
print(net.results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind(testdata,sqrt(testdata),
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)