library("twitteR")
library("tm")
library("SnowballC")
library("wordcloud")
library("RCurl")
library("stringr")
library("ggplot2")
library("readr")

dataset <- read_csv("~\\Datasets\\demonetization-tweets.csv")

tweet = dataset$text

tweet = gsub("[[:cntrl:]]", " ", tweet)
tweet <- gsub("(RT|via)((?:\\b\\W*@\\W+)+)", " ", tweet, ignore.case = T)
tweet <- gsub('@\\w+', '', tweet)
tweet <- gsub("[[:punct:]]"," ", tweet)
tweet <- gsub("[[:digit:]]"," ", tweet)
tweet <- gsub("http[s]?\\w+", " ", tweet)
tweet <- gsub("[ \t]{2,}", " ", tweet)
tweet <- gsub("^\\s+|\\s+$", " ", tweet)
tweet <- tweet[!is.na(tweet)]
tweet = gsub("^ ", "", tweet)
tweet = gsub(" $", "", tweet)
tweet = gsub("[^[:alnum:] ]", " ", tweet)

tweet = tolower(tweet)

tweet_corpus = Corpus(VectorSource(tweet))
tweet_corpus <- tm_map(tweet_corpus, tolower)
tweet_corpus <- tm_map(tweet_corpus, PlainTextDocument)
tweet_corpus <- tm_map(tweet_corpus, removePunctuation)
tweet_corpus = tm_map(tweet_corpus, removeWords, c("demonetization","demonetisation","rt","amp", stopwords("english")))
tweet_corpus <- tm_map(tweet_corpus, stripWhitespace)
tweet_corpus = tm_map(tweet_corpus, stemDocument)

wordcloud(tweet_corpus, min.freq = 150, max.words = 300, random.order = 'F', rot.per = 0.2, colors = brewer.pal(8, "Dark2"), random.color = TRUE, scale = c(3,.3))

score.sentiment <- function(sentences, positive_words, negative_words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores = laply(sentences, function(sentence, positive_words, negative_words) {
    wordlist = str_split(sentence, '\\s+')
    word = unlist(wordlist)
    negativematches = match(word, negative_words)
    positivematches = match(word, positive_words)
    positivematches <- !is.na(positivematches)
    negativematches <- !is.na(negativematches)
    score = sum(positivematches)-sum(negativematches)
    return(score)
  }, positive_words, negative_words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

positive <- scan("positive-words.txt", what= "character", comment.char= ";")
negative <- scan("negative-words.txt", what= "character", comment.char= ";")

tweetanalysis <- score.sentiment(tweet, positive, negative, .progress="none")
tweetanalysis$sentiment[tweetanalysis$score == 0] = "Nuetral"
tweetanalysis$sentiment[tweetanalysis$score > 0] = "Positive"
tweetanalysis$sentiment[tweetanalysis$score < 0] = "Negative"
tweetanalysis$sentiment <- factor(tweetanalysis$sentiment)
scoretable = table(tweetanalysis$score)
score = tweetanalysis$score
mean = mean(score)
median = median(score)
summary(tweetanalysis$sentiment)

ggplot(data = tweetanalysis, aes(x = score, fill = sentiment)) + 
  geom_bar() + 
  labs(title = "Sentiment Score Bar Plot", x = "Sentiment Score", y = "Tweet Count") +
  scale_x_continuous(breaks = seq(-6,6,1)) + 
  scale_y_continuous(breaks = seq(0,4000,500)) + 
  scale_fill_manual(guide = guide_legend("Sentiment"), values = c("#DD0426","#246EB9","#04B430"))
