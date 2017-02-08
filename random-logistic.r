#Importing test and train datasets 
library(readr)
library(ggplot2)

train <- read.csv("train_u6lujuX_CVtuZ9i (2).csv")
test <- read.csv("test_Y3wMUE5_7gLdaTN (1).csv")
str(test)
str(train)
head(test)
#Combining test and train datasets 
test$Loan_Status <- 0
data<-rbind(train,test)
head(data)
data$Loan_Status
#Loading Package Amelia for missing value imputation 
#Visualizing where the missing values are concentrated 
library(Amelia)
missmap(train)
#Loading plyr for Data Manipulation
#Recoding categorical variables 
library(plyr)
data[data$Gender =="",]$Gender="Male"
missmap(data$Gender)
data$Gender <- revalue(data$Gender, c("Male"="1", "Female"="2"))
head(data$Gender)
--------------------------------------------------------------------------------------
  table(data$Married)
data[data$Married =="",]$Married="Yes"
data$Married
data$Married <- revalue(data$Married,c("Yes"="1", "No"="2"))
head(data$Married)
-----------------------------------------------------------------------------------------
  table(data$Education)  
train$Education <- revalue(train$Education, c("Graduate"="1", "Not Graduate"="2"))
head(train$Education)
-------------------------------------------------------------------------------------------
  table(data$Self_Employed)
data[data$Self_Employed =="",]$Self_Employed="No"
data$Self_Employed
data$Self_Employed<- revalue(data$Self_Employed, c("Yes"="1", "No"="2"))
head(data$Self_Employed)
--------------------------------------------------------------------------------------------
  table(data$Property_Area)
data$Property_Area<- revalue(data$Property_Area, c("Rural"="1", "Semiurban"="2","Urban"="3"))
data$Property_Area
---------------------------------------------------------------------------------------------
  table(data$Dependents)
data[data$Dependents =="",]$Dependents="0"
data$Dependents
-----------------------------------------------------------------------------------------------
  summary(data$Loan_Amount_Term)
data$Loan_Amount_Term[is.na(data$Loan_Amount_Term)] <- "360"
data$Loan_Amount_Term
str(data)
----------------------------------------------------------------------------------------------
  boxplot(data$LoanAmount)
summary(data$LoanAmount)
amount1=data$LoanAmount[data$LoanAmount < 230]
boxplot(amount1,horizontal=T)
data$LoanAmount[data$LoanAmount > 230 ] = 230
head(data$LoanAmount)
data$LoanAmount
data$LoanAmount[is.na(data$LoanAmount)] <- "133"
-----------------------------------------------------------------------------------------------
  summary(data$Credit_History)
data$Credit_History[is.na(data$Credit_History)]="1"
data$Credit_History
------------------------------------------------------------------------------------------------
  missmap(data)
nrow(train)
------------------------------------------------------------------------------------------------
  View(data)
str(data)

#feature engg.

data$LoanAmount <- as.numeric(data$LoanAmount)
data$Loan_Amount_Term <- as.numeric(data$Loan_Amount_Term)
data$Credit_History <- as.factor(data$Credit_History)
data$totalincome <- as.numeric(data$ApplicantIncome + data$CoapplicantIncome)

data$ApplicantIncome <- log(data$ApplicantIncome)
hist(data$ApplicantIncome)

data$CoapplicantIncome <- ifelse(data$CoapplicantIncome > 9000 ,
                                 median(data$CoapplicantIncome),
                                 data$CoapplicantIncome)
hist((data$CoapplicantIncome))
hist((data$totalincome))
data$ratio <- (data$LoanAmount/data$totalincome)

hist((data$ratio))
data$totalincome <- log(data$totalincome)
boxplot(data$totalincome)
summary(data$totalincome)

summary(data$ratio)
boxplot(data$ratio)

train1 <- data[1:614,]
test1 <- data[615:981,]
head(test1)
-------------------------------------------------------------------------------------------------------
  str(train1)
  #Loading Random_Forest Package 
  library(randomForest)
#Building a rfmodel 
#forestmodel <- randomForest(as.factor(Loan_Status) ~ 
#                              Gender + Dependents+Education+totalincome+
#                              Self_Employed+LoanAmount+
#                              Credit_History+Property_Area, 
#                            data = train1, nodesize = 5, ntree = 1000, mtry =2)

forestmodel <- randomForest(as.factor(Loan_Status) ~ 
                              totalincome+ApplicantIncome + CoapplicantIncome
                              +LoanAmount+ Loan_Amount_Term + ratio +
                              Credit_History, 
                            data = train1, nodesize = 1, ntree = 2000, mtry =2)
varImpPlot(forestmodel)
forestmodel
#Applying the model for prediciting in test dataset 
prediction <- predict(forestmodel, newdata = test1, type = 'class')
submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = prediction)
submit
forestmodel
#Writing into csv for competetion submission 
write.csv(submit, file = 'sub21.csv', row.names = FALSE)
