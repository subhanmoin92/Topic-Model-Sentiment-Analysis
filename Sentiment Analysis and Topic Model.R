# SYED ABDUL SUBHAN MOIN
# MID TERM FALL 2017
# DEADLINE 10/28/17
#--------------------------
setwd("D:/Non P/Omis 670/Midterm")
getwd()


#------------------------------------------------------------------------------------------------------
#Task 1
#Getting Data from the twitter 
#If you have the data proceed from task  2
#---------------------------------------------------------------------------------------------------------
  
install.packages("twitteR")
install.packages("ROAuth")
install.packages("streamR")

install.packages("topicmodels")
install.packages("ldatuning")
install.packages("tidytext")
install.packages("dplyr")
install.packages("stringr")
install.packages("tidyr")
install.packages("tm")
install.packages("plyr")

library(plyr)
library(ggplot2)
library(rvest)
library(tm)
library(tidytext)
library(dplyr)
library(ldatuning)
library(stringr)
library(topicmodels)
library(tidyr)

library(twitteR)
library(ROAuth)
library(streamR)

consumer_key <- ""
consumer_secret <- ""
access_token <- ""
access_secret <- ""

TwitterAuth<-setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)  

search.twitter1<-searchTwitter("Netflix", n=3000,  lang='en', since = "2017-10-23", until = "2017-10-24") 
search.twitter2<-searchTwitter("Netflix", n=3000,  lang='en', since = "2017-10-24", until = "2017-10-25")
search.twitter3<-searchTwitter("Netflix", n=3000,  lang='en', since = "2017-10-25", until = "2017-10-26")
search.twitter4<-searchTwitter("Netflix", n=3000,  lang='en', since = "2017-10-26", until = "2017-10-27" )

no_retweets1<-strip_retweets(search.twitter1, strip_manual = TRUE, strip_mt = TRUE) # removing retweets
no_retweets2<-strip_retweets(search.twitter2, strip_manual = TRUE, strip_mt = TRUE) # removing retweets
no_retweets3<-strip_retweets(search.twitter3, strip_manual = TRUE, strip_mt = TRUE) # removing retweets
no_retweets4<-strip_retweets(search.twitter4, strip_manual = TRUE, strip_mt = TRUE) # removing retweets

search.twitter1.df<-twListToDF(no_retweets1)
search.twitter2.df<-twListToDF(no_retweets2)
search.twitter3.df<-twListToDF(no_retweets3)
search.twitter4.df<-twListToDF(no_retweets4)

#---------------------------------------------------------------------
tweets1.df<-as.data.frame(search.twitter1.df$text)
tweets1.df<-rbind(search.twitter1.df$text,search.twitter2.df$text)
tweets2.df<-rbind(search.twitter3.df$text,search.twitter4.df$text)
tweets.df<-rbind(tweets1.df,tweets2.df)

colnames(tweets.df)[1]<-"text"

tweets.df$text<-lapply(tweets.df$text, function(x) gsub("@\\w+", "", x))
tweets.df$text<-lapply(tweets.df$text, function(x)gsub("#\\w+", '', x ))
tweets.df$text<-lapply(tweets.df$text, function(x)gsub("RT\\w+", "", x ))
tweets.df$text<-lapply(tweets.df$text, function(x)gsub("http.*", "", x ))
tweets.df$text<-lapply(tweets.df$text, function(x)gsub("RT", "", x))
tweets.df$text<-lapply(tweets.df$text, function(x)gsub(":", "", x))
#tweets.df<-lapply(tweets.df, function(x)gsub("BREAKING NEWS", '', x ))

View(tweets.df)
tweetsN.df<-as.data.frame(tweets.df)

write.csv(tweets.df, file="NetflixTweets.csv")


save(tweets.df, file="NetflixTweets.Rdata")

#-----------------------------------------------------------------------------------------------------
#Task 2 
# Data Cleaning : removing unstructured data 
#-----------------------------------------------------------------------------------------------------


tweet.df<-read.csv("NetflixTweets.csv")

tweets.df<-as.data.frame(tweet.df$text)

View(tweets.df)
#tweets.df<-as.data.frame(search.twitter.df$text)
colnames(tweets.df)[1]<-"text"

tweets.df<-lapply(tweets.df, function(x) gsub("@\\w+", "", x))
tweets.df<-lapply(tweets.df, function(x)gsub("#\\w+", '', x ))
tweets.df<-lapply(tweets.df, function(x)gsub("RT\\w+", "", x ))
tweets.df<-lapply(tweets.df, function(x)gsub("http.*", "", x ))
tweets.df<-lapply(tweets.df, function(x)gsub("RT", "", x))
tweets.df<-lapply(tweets.df, function(x)gsub(":", "", x))
#tweets.df<-lapply(tweets.df, function(x)gsub("BREAKING NEWS", '', x ))

View(tweets.df)



#---------------------------------------------------------------------------------------------------
#Task 3
#Performing Lexicon Sentiment Analysis
#---------------------------------------------------------------------------------------------------

#tweets.df<-as.data.frame(tweets.df)
pos<-readLines("happy.txt")
neg<-readLines("sad.txt")


score.sentiment<-function(sentences, pos.words, neg.words, .progress='none')
{
  # Parameters
  # sentences: vector of text to score
  # pos.words: vector of words of postive sentiment
  # neg.words: vector of words of negative sentiment
  # .progress: passed to laply() to control of progress bar
  
  # create simple array of scores with laply
  scores<-laply(sentences,
                function(sentence, pos.words, neg.words)
                {
                  # remove punctuation
                  sentence<-gsub("[[:punct:]]", "", sentence)
                  # remove control characters
                  sentence<-gsub("[[:cntrl:]]", "", sentence)
                  # remove digits?
                  sentence<-gsub('\\d+', '', sentence)
                  
                  #convert to lower
                 # sentence<-tolower(sentence)
                  #remove numbers
                  sentence<-removeNumbers(sentence)
                  
                  
                  # split sentence into words with str_split (stringr package)
                  word.list<- str_split(sentence, "\\s+")
                  words<- unlist(word.list)
                  
                  # compare words to the dictionaries of positive & negative terms
                  pos.matches<-match(words, pos)
                  neg.matches<- match(words, neg)
                  
                  # get the position of the matched term or NA
                  # we just want a TRUE/FALSE
                  pos.matches<- !is.na(pos.matches)
                  neg.matches<- !is.na(neg.matches)
                  
                  # final score
                  score<- sum(pos.matches) - sum(neg.matches)
                  return(score)
                }, pos.words, neg.words, .progress=.progress )
  # data frame with scores for each sentence
  scores.df<- data.frame(text=sentences, score=scores)
  return(scores.df)
}

scores_twitter<-score.sentiment(tweets.df$text, pos, neg, .progress='text')
  
View(scores_twitter)

#Convert sentiment scores from numeric to character to enable the gsub function 
scores_twitter$score_chr<-as.character(scores_twitter$score)

#After looking at the summary(scores_facebook$score) decide on a threshold for the sentiment labels
scores_twitter$score_chr<-gsub("^0$", "Neutral", scores_twitter$score_chr)
scores_twitter$score_chr<-gsub("^1$|^2$", "Positive", scores_twitter$score_chr)
scores_twitter$score_chr<-gsub("^3$|^4$|^5$|^6$", "Very Positive", scores_twitter$score_chr)
scores_twitter$score_chr<-gsub("^-1$|^-2$", "Negative", scores_twitter$score_chr)
scores_twitter$score_chr<-gsub("^-3$|^-4$|^-5$", "Very Negative", scores_twitter$score_chr)

View(scores_twitter)
scores_twitter<-cbind(scores_twitter, tweetsLatest.df$created)
write.csv(scores_twitter, file = "Netflix.csv")

#----------------------------------------------------------------------------------------------------------------------------------------
# Topic Models
#----------------------------------------------------------------------------------------------------------------------------------------

model.raw<-read.csv("Netflix.csv", stringsAsFactors = FALSE)

# Adding a new feature as a new coloumn
model.raw$TextLength<-nchar(model.raw$text)


model.raw$text<-gsub("just", "", model.raw$text)
model.raw$text<-gsub("like", "", model.raw$text)


model.raw$text<-gsub("watch", "", model.raw$text)
model.raw$text<-gsub("amp", "", model.raw$text)


model.raw$text<-gsub("netflix", "", model.raw$text)
model.raw$text<-gsub("get", "", model.raw$text)

model.raw$text<-gsub("eduaubdedubu", "", model.raw$text)
model.raw$text<-gsub("ucueucueucueucueucueucu", "", model.raw$text)





my_source<-VectorSource(model.raw$text)
corpus<-Corpus(my_source)
corpus<- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))

corpus <- tm_map(corpus, removeWords,("just"))

corpus <- tm_map(corpus, removeWords,("like"))

corpus <- tm_map(corpus, removeWords,("watch"))

corpus <- tm_map(corpus, removeWords,("amp"))

#corpus <- tm_map(corpus, removeWords,("netflix"))

corpus <- tm_map(corpus, removeWords,("get"))
corpus <- tm_map(corpus, removeWords,("eduaubdedubu"))

corpus <- tm_map(corpus, removeWords,("ucueucueucueucueucueucu"))
corpus <- tm_map(corpus, stripWhitespace)
corpus<- tm_map(corpus, removeNumbers)
corpus<- tm_map(corpus, stemDocument)

dtm <- DocumentTermMatrix(corpus, control=list(minDocFreq=2, minWordLength=2))

#Remove empty cells and create a new corpus that aligns with the processed dtm
rowTotals <- apply(dtm , 1, sum)
empty.rows<-dtm[rowTotals == 0,]$dimnames[1][[1]]
empty.rows<-as.numeric(empty.rows)
corpus <- corpus[-empty.rows]

#Create a dataframe of the new corpus
corpus.df<-as.data.frame(corpus$content)

#Create the dtm again with the new corpus
dtm <- DocumentTermMatrix(corpus, control=list(minDocFreq=2, minWordLength=2))

#Making sure that the original data set i.e., model.raw aligns with the new corpus. To do that we have to remove the same row numbers in empty.rows in model.raw
x<-length(as.numeric(empty.rows))# calculate the number of empty.rows


#Write a loop that goes through the row numbers of model.raw and delete those rows that match with delete.rows
empty.rows[1]
for (i in 1:x){
  model.raw<-model.raw[-empty.rows[i],]
  i<-i+1
}

#Random check to see consistency between the new and old datasets
corpus.df[743,]
model.raw[743,]$text


#Finding out how many topics to create
result<-FindTopicsNumber(dtm,
                         topics=seq(from = 2, to =15, by=1),
                         metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
                         method = "Gibbs",
                         control = list(seed = 77),
                         mc.cores = 3L,
                         verbose = TRUE)

FindTopicsNumber_plot(result)


#Run the LDA topic model
lda<-LDA(dtm, k=7, control = list(seed=2343))

#Using tidytext manifest topics
topics<-tidy(lda, matrix="beta")
topics

#Showing the top terms and grouping them by topics created - Using dplyr's top_n limiting to 10
top_terms<-topics %>%
  group_by(topic) %>% 
  top_n(5, beta) %>% 
  ungroup() %>% 
  arrange(topic, -beta)

#Convert top_terms$topic to a factor variable - for visualization 
top_terms$topic<-as.factor(top_terms$topic)

#Visualization of the top terms.
top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = topic)) +
  theme_bw()+
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



#Creating a per-document-per-topic probability list
documents<-tidy(lda, matrix="gamma")
documents

documents<-tidy(lda, matrix="gamma")
documents


#Finding the highest probablity gamma score to assign topic number for each document
i<-1
max_pos<-vector()
for(i in documents$document){
  
  x<-filter(documents, document==i)
  max<-max(x$gamma) 
  max_pos[length(max_pos)+1]<-max
  #mutate(documents, max)
  
}
documents$max<-max_pos
documents$diff<-documents$gamma-documents$max
documents$corpus.df<-corpus.df$Content
documents<-filter(documents, diff==0)
documents$document<-as.numeric(documents$document)
documents<-arrange(documents,document)

model.raw<-cbind(model.raw,documents)


##-----------------------------------------------------------------------------------------------------------
#Create a master file 
#------------------------------------------------------------------------------------------------------------
model.raw$X<-NULL
model.raw$TextLength<-NULL
 model.raw$document<-NULL
 model.raw$gamma<-NULL
 model.raw$max<-NULL
 model.raw$diff<-NULL
 View(model.raw)

colnames(model.raw)[5]<-"topicNumber"
colnames(model.raw)[3]<-"label"
colnames(model.raw)[4]<-"date&time"

View(model.raw)

write.csv(model.raw, file = "MasterFile.csv")

#
# Timestamp split
#

model.raw$date <- as.character(as.Date(model.raw$`date&time`))
model.raw$time <- format(as.POSIXct(model.raw$`date&time`) ,format = "%H:%M:%S") 
View(model.raw)

model.raw$`date&time`<-NULL
model.raw$date<-NULL

View(model.raw)

hr<- as.integer(substr(model.raw$time, 1, 2))

d1<-data.frame(datetime=model.raw$time, heurer=hr, period=cut(hr, c(-Inf,7,12,16,18,22,Inf), labels = c("EarlyMorning","Morning","Afternoon","Evening","Night","LateNight")))
View(d1)
model.raw<-cbind(model.raw,d1$period)
colnames(model.raw)[6]<-"period"
View(model.raw)

write.csv(model.raw, file = "MasterFile1.csv")
