
# Set Working Directory
setwd("C:/Users/dell/Desktop/Data Science/Self R Project/Lending's Club")

loan_07_11 <- read.csv('LoanStats 07-11.csv', stringsAsFactors = FALSE, header = TRUE, skip = 1) 
#loan_07_11 <- loan_07_11[-c(39787,42537,42538),]

loan_12_13 <- read.csv('LoanStats 12-13.csv', stringsAsFactors = FALSE, header = TRUE, skip = 1)

loan_14 <- read.csv('LoanStats 14.csv', stringsAsFactors = FALSE, header = TRUE, skip = 1)

loan_15 <- read.csv('LoanStats 15.csv', stringsAsFactors = FALSE, header = TRUE, skip = 1)

Loan_data <- rbind(loan_07_11, loan_12_13, loan_14, loan_15)

Loan_data <- Loan_data[-c(39787,42537,42538,230720,230721,466351, 466352, 887448, 887449),]

print(Loan_data[1,])

LD = Loan_data

LD <- LD[ , apply(LD, 2, function(x) !any(is.na(x)))]

## To browse Data Dictionary for description of only 41 columns with healthy data.

library(readxl)
dataDictionary <- read_excel("LCDataDictionary.xlsx")
# fields available in the data dictionary
dd_names <- as.character(na.omit(dataDictionary$LoanStatNew))
# fields available in the loan book
loanbook_names <- names(LD)
# show the fields described in data dictionary but not in the loan book
cols <- intersect(dd_names, loanbook_names)
dataDictionary <- dataDictionary[dataDictionary$LoanStatNew %in% cols,]
#################

write.csv(LD,"Loan_Data_41.csv")

## Some insights from ggpolt2

library(ggplot2)

# loan_amnt vs grade
ggplot(LD, aes(loan_amnt, col = grade)) + geom_histogram(bins = 50)
ggplot(LD, aes(loan_amnt, col = grade)) + geom_histogram(bins = 50) + facet_grid(grade ~ .)

# Interest rate VS grade
ggplot(LD, aes(int_rate, col = grade)) + geom_bar() +facet_grid(grade ~ .)

# 
library(lubridate)
LD$issue_d <- dmy(paste0("01-",LD$issue_d))
loan_amnt_by_month <- aggregate(loan_amnt ~ issue_d, data = LD, sum)
ggplot(loan_amnt_by_month, aes(issue_d, loan_amnt)) + geom_bar(stat = "identity")

amnt_df_grade <- LD %>% 
  select(issue_d, loan_amnt, grade) %>% 
  group_by(issue_d, grade) %>% 
  summarise(Amount = sum(loan_amnt))

ts_amnt_grade <- ggplot(amnt_df_grade, 
                        aes(x = issue_d, y = Amount))
ts_amnt_grade + geom_area(aes(fill=grade)) + xlab("Date issued")


#
loan_amnt_by_status <- aggregate(loan_amnt ~ loan_status, data = LD, sum)
ggplot(loan_amnt_by_status, aes(loan_status, loan_amnt, fill = loan_status)) + geom_bar(stat = "identity") + scale_x_discrete(breaks=NULL)

#
ggplot(LD, aes(loan_status, loan_amnt, fill = loan_status)) + geom_boxplot() + scale_x_discrete(breaks=NULL)


# 

library(tm)
library(RColorBrewer)
library(wordcloud)

loan_descriptions.corpus <- Corpus(DataframeSource(data.frame(head(LD[,19], n=10000))))
loan_descriptions.corpus <- tm_map(loan_descriptions.corpus, removePunctuation)
loan_descriptions.corpus <- tm_map(loan_descriptions.corpus, content_transformer(tolower))

wordcloud(loan_descriptions.corpus,
          max.words = 100,
          random.order=FALSE, 
          rot.per=0.30, 
          use.r.layout=FALSE, 
          colors=brewer.pal(8, "Paired"))
