setwd("C:/Users/dell/Desktop/Data Science/R Code/Telecom-LR Bill Case (1)/Telecom-LR Bill Case")
data <- read.csv("billdata18052014.csv",stringsAsFactors = FALSE)
library(dplyr)
glimpse(data)
table(data$salary)

data <- data %>%
  mutate(Payer_Company = as.numeric(Payer =="Company"),
         Payer_Self = as.numeric(Payer =="Self")) %>%
select(-Payer)

data <- data %>%
  mutate(Travel_high = as.numeric(Travel =="High"),
         Travel_medium = as.numeric(Travel =="Medium")) %>%
  select(-Travel)

data <- data %>%
  mutate(Relatives_Abroad_Yes = as.numeric(RelativesAbroad =="Yes")) %>%
  select(-RelativesAbroad)

data <- data %>%
  mutate(job_business = as.numeric(jobtype =="Business"),
         job_pvt = as.numeric(jobtype =="Private"),
         job_govt = as.numeric(jobtype =="Government")) %>%
  select(-jobtype)

table(data$AvgBill)

data.bk <- data
colnames(data)


#Setting the seed to review
set.seed(3)
#Splitting the data in train and test
s=sample(1:nrow(data),0.7*nrow(data))
train.temp.set=data[s,]
test.set=data[-s,]

#Splitting the data in train and validation
s1=sample(1:nrow(train.temp.set),0.7*nrow(train.temp.set))
train.set=train.temp.set[s1,]
val.set=train.temp.set[-s1,]

library(car)
fit = (lm(AvgBill~., data=train.set))
vif(fit)
dep.vars <-attributes(alias(fit)$Complete)$dimnames[[1]]

formula.new <- as.formula(
  paste(
    paste(deparse(AvgBill~.), collapse=""),
    paste(dep.vars, collapse="-"),
    sep="-"
  )
)

fit_new = (lm(AvgBill ~ . - dv_jt_pvt - dv_payer_company - ClsRelativesCnt - 
                dv_travel_low - dv_travel_medium - dv_travel_high - dv_Rel_yes - 
                dv_Rel_No - Payer_Company - Payer_Self - Travel_high - Travel_medium - 
                Relatives_Abroad_Yes - job_business - job_pvt - job_govt, data=train.set))

fit_new = (lm(AvgBill ~ . - dv_jt_pvt - dv_payer_company - ClsRelativesCnt - 
                dv_travel_low - dv_travel_medium - dv_travel_high - dv_Rel_yes - 
                dv_Rel_No - Payer_Company - Payer_Self - Travel_high - Travel_medium - 
                Relatives_Abroad_Yes - job_business - job_pvt - job_govt -StdAvgBill, data=train.set))

sort(vif(fit_new), decreasing = T)

library(sandwich)
vcovHC(fit_new,omega=NULL, type="HC4")

library("lmtest")
coeftest(fit_new,df=Inf,vcov=vcovHC(fit_new,type="HC4"))

### for val data
fit_val = (lm(AvgBill ~ . - dv_jt_pvt - dv_payer_company - ClsRelativesCnt - 
                dv_travel_low - dv_travel_medium - dv_travel_high - dv_Rel_yes - 
                dv_Rel_No - Payer_Company - Payer_Self - Travel_high - Travel_medium - 
                Relatives_Abroad_Yes - job_business - job_pvt - job_govt -StdAvgBill, data=val.set))

sort(vif(fit_val), decreasing = T)
summary(fit_val)

fitted(fit_val)
plot(fit_val)

rmse=sqrt(mean((predict(fit_val,test.set)-test.set$AvgBill)**2))

rmse

# visual agreement
plot(test.set$AvgBill,predict(fit_val,test.set))

# Fit diagnostics
# Error randomness
plot(fit_val,which=1)

# There doesnt seem to be any pattern , we need not worry about our model definition
# Error Normality
plot(fit_val,which=2)
hist(fit_val$residuals,breaks = 20)

# Error variance
plot(fit_val,which=3)

# Outliers detection : None found , cook's distance < 1 for all obs
plot(fit_val,which=4)

