# loading libraries
library(plyr)
library(dplyr)

## function for importing Rscript from github
source_https <- function(url)
{
  library(RCurl)
  eval(parse(text=getURL(url,followlocation=T,cainfo=system.file("CurlSSL","cacert.pem",package="RCurl"))),envir=.GlobalEnv)
}


## loading data
train <- read.csv("./train_u6lujuX_CVtuZ9i.csv", stringsAsFactors=F)
test <- read.csv("./test_1.csv", stringsAsFactors=F)


## cleaning data

X_train <- train
X_test <- test

# Converting dummy to married
X_train$Married[X_train$Married == 'Yes'] <- "1"
X_train$Married[X_train$Married == 'No'] <- "0"
X_train$Married <- as.integer(X_train$Married)

X_test$Married[X_test$Married == 'Yes'] <- "1"
X_test$Married[X_test$Married == 'No'] <- "0"
X_test$Married <- as.integer(X_test$Married)

# Converting dummy for Sel_Employed
X_train$Self_Employed[X_train$Self_Employed == 'No'] <- "1"
X_train$Self_Employed[X_train$Self_Employed == 'Yes'] <- "0"
X_train$Self_Employed <- as.integer(X_train$Self_Employed)

X_test$Self_Employed[X_test$Self_Employed == 'No'] <- "1"
X_test$Self_Employed[X_test$Self_Employed == 'Yes'] <- "0"
X_test$Self_Employed <- as.integer(X_test$Self_Employed)

# creating total income
X_train$totalincome <- X_train$ApplicantIncome + X_train$CoapplicantIncome
X_train$totalincome <- as.integer(X_train$totalincome)

X_test$totalincome <- X_test$ApplicantIncome + X_test$CoapplicantIncome
X_test$totalincome <- as.integer(X_test$totalincome)

# Feature Engg. Creating ration loan_amount/total income
X_train$ratio <- X_train$LoanAmount/X_train$totalincome
X_train$ratio <- as.integer(X_train$ratio)
summary(X_train)
glimpse(X_train)

X_test$ratio <- X_test$LoanAmount/X_test$totalincome
X_test$ratio <- as.integer(X_test$ratio)

# Feature Engg. Creating ration loan_amount/Loan_term

X_train$Loan_Amount_Term <- as.numeric(X_train$Loan_Amount_Term)
X_train$loan_ratio <- X_train$LoanAmount/X_train$Loan_Amount_Term
X_train$loan_ratio <- as.numeric(X_train$loan_ratio)

X_test$Loan_Amount_Term <- as.numeric(X_test$Loan_Amount_Term)
X_test$loan_ratio <- X_test$LoanAmount/X_test$Loan_Amount_Term
X_test$loan_ratio <- as.numeric(X_test$loan_ratio)

X_train_b <- X_train
X_test_b <- X_test

X_train <- X_train_b
X_test <- X_test_b


##Logistic Regression

glimpse(X_train)
glimpse(X_test)
X_train[is.na(X_train)] <- -1
X_test[is.na(X_test)] <- -1

y <- X_train$Loan_y

X_train <- X_train[,-c(1,2,3,4,5,6,7,8,9,10,12,14,15,16,17,18,20)]
X_test <-   X_test[,-c(1,2,3,4,5,6,7,8,9,10,12,14,15,16,18)]


# XGBoost

source_https("https://raw.githubusercontent.com/anishpurohit/R_Models/master/XGBoost.R")

mdl_xgb <- XGBoost(X_train,y,X_test,cv=5,objective="binary:logistic",nrounds=500,max.depth=5,eta=0.01,colsample_bytree=0.5,seed=123,metric="auc",importance=1)

mdl_train <- mdl_xgb$train
mdl_train$y <- y
mdl_test <- mdl_xgb$test

check <- mdl_train$pred_xgb
check <- ifelse(check < 0.45 ,0,1)
table(check,y)
accuracy = (483)/614
Prediction1 <- mdl_test$pred_xgb
Prediction1 <- ifelse(Prediction1 < 0.27 ,"N","Y")
submit = data.frame(Loan_ID = X_test_b$Loan_ID, Loan_Status = Prediction1)
write.csv(submit, "xgb2_loan.csv")

# RandomForest

source_https("https://raw.githubusercontent.com/anishpurohit/R_Models/master/RandomForest.R")

mdl_rf <- RandomForestClassification(X_train,y,X_test,cv=5,ntree=500,nodesize=5,seed=123,metric="auc",plot=1,importance=1)
mdl_rf_train <- data.frame(mdl_rf[[1]])
mdl_rf_train$y <- y
mdl_rf_test <- data.frame(mdl_rf[[2]])
check <- mdl_rf_train$pred_rf
check <- ifelse(check < 0.35 ,0,1)
table(check,y)
accuracy = (500)/614
Prediction1 <- mdl_rf_test$pred_rf
Prediction1 <- ifelse(Prediction1 < 0.3 ,"N","Y")
submit = data.frame(Loan_ID = X_test_b$Loan_ID, Loan_Status = Prediction1)
write.csv(submit, "rf1_loan.csv")

# call logistic from source code 

source_https("https://raw.githubusercontent.com/anishpurohit/R_Models/master/LogisticRegression.R")

mdl_logit <- LogisticRegression(X_train,y,X_test,cv=10,seed=123,metric="auc",importance=1)

log_train <-mdl_logit$train
log_test <- mdl_logit$test

library(ggplot2)
ggplot(log_train, aes(y=y, x=log_train$pred_lr, color=factor(y))) + geom_point() + geom_jitter()

check <- log_train$pred_lr
check <- ifelse(check < 0.7 ,0,1)
table(check,y)
accuracy = (500)/614
Prediction1 <- mdl_rf_test$pred_rf
Prediction1 <- ifelse(Prediction1 < 0.3 ,"N","Y")
submit = data.frame(Loan_ID = X_test_b$Loan_ID, Loan_Status = Prediction1)
write.csv(submit, "rf1_loan.csv")


## Random forest
fit <- randomForest(as.factor(y) ~. ,
                    data=X_train, importance=TRUE, ntree=2000,mtry=1)
fit
Prediction <- predict(fit, X_test, OOB=TRUE, type = "response")
# Look at variable importance
varImpPlot(fit)

#Cforest
library(party)
set.seed(415)
fit1 <- cforest(as.factor(y) ~.,
               data = X_train, controls=cforest_unbiased(ntree=2000, mtry=3)) 
fit1
Prediction <- predict(fit1, test_final,OOB = TRUE, type = "response")
#table(test_final$Loan_y,Prediction)
#acc = (35+142)/214
Prediction1 <- ifelse(Prediction == 1,"Y","N")
submit = data.frame(Loan_ID = X_test$Loan_ID, Loan_Status = Prediction1)
write.csv(submit, "ciforest_loan.csv")
