
library(stringr)
library(plyr)
library(lubridate)
library(randomForest)
library(reshape2)
library(ggplot2)

df <- read.csv("LoanStats 07-11.csv", h=T, stringsAsFactors=F)

#take a peak...
head(df)

#annoying column; just get rid of it
df[,'desc'] <- NULL

summary(df)
#almost all NA, so just get rid of it
df[,'mths_since_last_record'] <- NULL

#get rid of fields that are mainly NA
poor_coverage <- sapply(df, function(x) {
  coverage <- 1 - sum(is.na(x)) / length(x)
  coverage < 0.8
})
df <- df[,poor_coverage==FALSE]

bad_indicators <- c("Late (16-30 days)", "Late (31-120 days)", "Default", "Charged Off")

df$is_bad <- ifelse(df$loan_status %in% bad_indicators, 1,
                    ifelse(df$loan_status=="", NA,
                           0))
table(df$loan_status)
table(df$is_bad)

head(df)
df$issue_d <- paste0("01-",df$issue_d)
df$issue_d <- as.Date(df$issue_d, "%d-%b-%y")
df$year_issued <- year(df$issue_d)
df$month_issued <- month(df$issue_d)

df$earliest_cr_line <- paste0("01-", df$earliest_cr_line)
df$earliest_cr_line <- as.Date(df$earliest_cr_line,"%d-%b-%y")

df$revol_util <- str_replace_all(df$revol_util, "[%]", "")
df$revol_util <- as.numeric(df$revol_util)

outcomes <- ddply(df, .(year_issued, month_issued), function(x) {
  c("percent_bad"=sum(x$is_bad) / nrow(x),
    "n_loans"=nrow(x))
})

plot(outcomes$percent_bad, main="Bad Rate")
outcomes

#figure out which columns are numeirc (and hence we can look at the distribution)
numeric_cols <- sapply(df, is.numeric)
#turn the data into long format (key->value esque)
df.lng <- melt(df[,numeric_cols], id="is_bad")
head(df.lng)

#plot the distribution for bads and goods for each variable
p <- ggplot(aes(x=value, group=is_bad, colour=factor(is_bad)), data=df.lng)
#quick and dirty way to figure out if you have any good variables
p + geom_density() +
  facet_wrap(~variable, scales="free")

#NOTES:
# - be careful of using variables that get created AFTER a loan is issued (prinicpal/interest related)
# - any ID variables that are numeric will be plotted as well. be sure to ignore those as well.

# only evaluate w/ vintages that have come to term
df.term <- subset(df, year_issued < 2012)
df.term$home_ownership <- factor(df.term$home_ownership)
df.term$is_rent <- df.term$home_ownership=="RENT"

idx <- runif(nrow(df.term)) > 0.75
train <- df.term[idx==FALSE,]
test <- df.term[idx==TRUE,]

library(dplyr)
glimpse(train)

rf <- randomForest(factor(is_bad) ~ pub_rec_bankruptcies + revol_util 
                   +inq_last_6mths + is_rent + open_acc ,
                   type="classification", data=train, importance=TRUE, na.action=na.omit)

varImpPlot(rf)
rf
table(train$is_bad)
test1 <- test
test1$is_bad <- NA
#Applying the model for prediciting in test dataset 
prediction <- predict(rf, newdata = test1, type = 'class')
table(prediction,test$is_bad)

submit <- data.frame(Loan_ID = test$Loan_ID, Loan_Status = prediction)
submit
forestmodel


library(yhat)
library(yhatr)

model.require <- function() {
  library(randomForest)
}

model.transform <- function(df) {
  df$is_rent <- df$home_ownership=="RENT"
  df
}

model.predict <- function(df) {
  df$prob_default <- predict(rf, newdata=df, type="prob")[,2]
  df
}

features <- c("revol_util","inq_last_6mths", "home_ownership", "annual_inc", "loan_amnt")
sample.data <- df.term[,features]
sample.data$annual_inc[sample.data$annual_inc > 200000] <- 200000
head(sample.data)
yhat.document(model="LendingClubRandomForest", version=2, df=sample.data)
