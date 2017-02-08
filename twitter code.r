library(twitteR)
library(ROAuth)
library(RJSONIO)
library(tm)
library(wordcloud)

api_key <- "WuRdD8Xht6u7P50VshWinSBSS"
api_secret <- "Ajnudtm7E9zfPhd2ZrqXW92uqwECnXi6jLnRkWR8qNC6wvvfff"
token <- "744491987454197761-vP8dWu3hwCVcpzIlYPuIiw33Ys9xLFE"
token_secret <- "jBcXICnr3Z97Df1H7IbhlUSnHNO18eirym256FUH0HwBc"

# Create Twitter Connection
setup_twitter_oauth(api_key, api_secret, token, token_secret)

fd = c("Janamashtmi")

# Run Twitter Search. Format is searchTwitter("Search Terms", n=100, lang="en", geocode="lat,lng", also accepts since and until).
tweets <- searchTwitter(fd, n=5000, lang="en", since="2016-08-20")

# Transform tweets list into a data frame
tweets.df <- twListToDF(tweets)
library(dplyr)
glimpse(tweets.df)

# Get the text
some_txt = sapply(tweets, function(x) x$getText())

# remove retweet entities
some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)
#some_txt = gsub("fixwhatsbrokeninwords", "", some_txt)

# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}

# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

# Create corpus
corpus=Corpus(VectorSource(some_txt))

# Convert to lower-case
corpus=tm_map(corpus,tolower)

# Remove stopwords
corpus=tm_map(corpus,function(x) removeWords(x,stopwords()))

# convert corpus to a Plain Text Document
corpus=tm_map(corpus,PlainTextDocument)

col=brewer.pal(6,"Dark2")
wordcloud(corpus, min.freq=25, scale=c(5,2),rot.per = 0.25,
          random.color=T, max.word=45, random.order=F,colors=col)

#install.packages("syuzhet")
library(syuzhet)
mySentiment <- get_nrc_sentiment(some_txt)
sentiment.sum <- data.frame(apply(mySentiment, 2, sum))
colnames(sentiment.sum) <- "sentiment.count"
sentiment.sum$emotion <- rownames(sentiment.sum)

View(sentiment.sum)
ggplot(sentiment.sum, aes(x=emotion, y=sentiment.count, fill=factor(emotion))) + geom_bar(stat="identity")
