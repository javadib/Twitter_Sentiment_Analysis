#include required libraries
library(plyr)
library(twitteR)
library(stringr)
library(RColorBrewer)
library(wordcloud)
library(NLP)
library(tm)

setwd("path_to_working_directoy")

source("Sentiment.R")

#setup twitter account
consumer_key <- 'twitter_consumer_key'
consumer_secret <- 'twitter_consumer_secret'
access_token <- 'twitter_access_token'
access_secret <- 'twitter_access_secret'
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#get the tweets
tweets = searchTwitter("#ChennaiExpress", n=500, lang="en")
tweets_txt = sapply(tweets[1:50],function(x) x$getText())

#function to clean data
cleanTweets = function(tweets)
{
  tweets_cl = gsub("(RT|via)((?:\\b\\W*@\\w+)+)","",tweets)
  tweets_cl = gsub("http[^[:blank:]]+", "", tweets_cl)
  tweets_cl = gsub("@\\w+", "", tweets_cl)
  tweets_cl = gsub("[ \t]{2,}", "", tweets_cl)
  tweets_cl = gsub("^\\s+|\\s+$", "", tweets_cl)
  tweets_cl = gsub("[[:punct:]]", " ", tweets_cl)
  tweets_cl = gsub("[^[:alnum:]]", " ", tweets_cl)
  tweets_cl <- gsub('\\d+', '', tweets_cl)
  return(tweets_cl)
}


#load pos,neg statements
afinn_list <- read.delim(file='~/AFINN-111.txt', header=FALSE, stringsAsFactors=FALSE)
names(afinn_list) <- c('word', 'score')
afinn_list$word <- tolower(afinn_list$word)

#categorize words as very negative to very positive and add some movie-specific words
vNegTerms <- afinn_list$word[afinn_list$score==-5 | afinn_list$score==-4]
negTerms <- c(afinn_list$word[afinn_list$score==-3 | afinn_list$score==-2 | afinn_list$score==-1], "second-rate", "moronic", "third-rate", "flawed", "juvenile", "boring", "distasteful", "ordinary", "disgusting", "senseless", "static", "brutal", "confused", "disappointing", "bloody", "silly", "tired", "predictable", "stupid", "uninteresting", "trite", "uneven", "outdated", "dreadful", "bland")
posTerms <- c(afinn_list$word[afinn_list$score==3 | afinn_list$score==2 | afinn_list$score==1], "first-rate", "insightful", "clever", "charming", "comical", "charismatic", "enjoyable", "absorbing", "sensitive", "intriguing", "powerful", "pleasant", "surprising", "thought-provoking", "imaginative", "unpretentious")
vPosTerms <- c(afinn_list$word[afinn_list$score==5 | afinn_list$score==4], "uproarious", "riveting", "fascinating", "dazzling", "legendary")   

#Calculate score on each tweet
tweetResult <- as.data.frame(sentimentScore(tweets_txt, vNegTerms, negTerms, posTerms, vPosTerms))
tweetResult$'2' = as.numeric(tweetResult$'2')
tweetResult$'3' = as.numeric(tweetResult$'3')
tweetResult$'4' = as.numeric(tweetResult$'4')
tweetResult$'5' = as.numeric(tweetResult$'5')

counts = c(sum(tweetResult$'2'),sum(tweetResult$'3'),sum(tweetResult$'4'),sum(tweetResult$'5'))
names = c("Worst","BAD","GOOD","VERY GOOD")
mr = list(counts,names)
colors = c("red", "yellow", "green", "violet")

barplot(mr[[1]], main="Movie Review", xlab="Number of votes",legend=mr[[2]],col=colors)

wordcloud(tweets_txt)

