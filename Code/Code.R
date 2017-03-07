#Loading required packages
library("twitteR")
library("tm")
library("SnowballC")
library("wordcloud")
library("readr")
library("NLP")
library("RColorBrewer")
library("qdap")

#Read the dataset into a variable
dataset <- read_csv("demonetization-tweets.csv")

#Store the 'text' column in a separate variable
tweet = dataset$text

#Clean the tweets using gsub:
#Remove control characters
tweet = gsub("[[:cntrl:]]", " ", tweet)
#Remove retweets
tweet <- gsub("(RT|via)((?:\\b\\W*@\\W+)+)", " ", tweet, ignore.case = T)
# Remove "@ person'
tweet <- gsub('@\\w+', '', tweet)
#Remove punctuations
tweet <- gsub("[[:punct:]]"," ", tweet)
#Remove digits
tweet <- gsub("[[:digit:]]"," ", tweet)
#Remove links
tweet <- gsub("http[s]?\\w+", " ", tweet)
#Remove unwanted spaces
tweet <- gsub("[ \t]{2,}", " ", tweet)
tweet <- gsub("^\\s+|\\s+$", " ", tweet)
#Remove NAs
tweet <- tweet[!is.na(tweet)]
#Remove all otheer insignificant symbols
tweet = gsub("^ ", "", tweet)
tweet = gsub(" $", "", tweet)
tweet = gsub("[^[:alnum:] ]", " ", tweet)
#Convert the text to lowercase
tweet = tolower(tweet)

#Store the text in a Corpus
tweet_corpus = VCorpus(VectorSource(tweet))
#Remove stopwords
tweet_corpus = tm_map(tweet_corpus, removeWords, c(stopwords("en"), "amp", "demonetization","obqrhlnsl", "uodwxdpmmg"))
#Remove whitespace
tweet_corpus = tm_map(tweet_corpus, stripWhitespace)

#Creating a Term Document Matrix
tweet_tdm = TermDocumentMatrix(tweet_corpus)
#Converting into matrix
tweet_m = as.matrix(tweet_tdm)
#To get word frequency
term_frequency = rowSums(tweet_m)
term_frequency = sort(term_frequency, decreasing = TRUE)
#Plotting frequency of words
barplot(term_frequency[1:10], col = "brown3", las = 2, main = "Frequency Plot of words")
word_frequency = data.frame(term = names(term_frequency), num = term_frequency)
#Creating a wordcloud
wordcloud(word_frequency$term, word_frequency$num, min.freq = 150, max.words = 100, random.order = 'F', rot.per = 0.1, colors = brewer.pal(8, "Dark2"), scale = c(3,0.3), random.color = T)

#Word clustering with dendrogram
#To limit the number of words in TDM
tweet_tdm_s = removeSparseTerms(tweet_tdm, sparse = 0.95)
tweet_m_s = as.matrix(tweet_tdm_s)
tweet_df = as.data.frame(tweet_m_s)
#To compute the diff b/w each row of the matrix
distance = dist(tweet_df)
#To perform cluster analysis
hc = hclust(distance)
#plotting the dendrogram
plot(hc)

#Word Associations
#To find the correlation of the word modi with other words
assocs = findAssocs(tweet_tdm, "modi", 0.2)
#Putting the words and their correlations in a table
assocs_df = list_vect2df(assocs)[,2:3]
#Plotting the correlation
ggplot(assocs_df, aes(y = assocs_df[,1])) + geom_point( aes(x = assocs_df[,2]), color = "firebrick3", data = assocs_df, size = 1.5) + ggtitle("Correlation of the word 'modi' with other words") + theme(plot.title = element_text(hjust = 0.5)) + xlab("Correlation value") + ylab("words")

#Calculating Sentiment scores
score.sentiment <- function(sentences, positive_words, negative_words, .progress='none')
{
    require(plyr)   #forlaply
    require(stringr)  #for str_split
    #list+array+apply=laply
    scores = laply(sentences, function(sentence, positive_words, negative_words) {
    #Split into words
    wordlist = str_split(sentence, '\\s+')
    #Unlist and have as separate words to work on
    word = unlist(wordlist)
    #Compare the words in our file with the list of positive and negative words
    negativematches = match(word, negative_words)
    positivematches = match(word, positive_words)
    #The above match() function returns only the position of the matched terms
    #So, store the words which are not NA, that is, store only the matched words
    positivematches <- !is.na(positivematches)
    negativematches <- !is.na(negativematches)
    #Net score is the differrence between positive and negative matches
    score = sum(positivematches)-sum(negativematches)
    return(score)
  }, positive_words, negative_words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

#Importing postive and negative words list
positive <- scan('positive-words.txt', what='character', comment.char=';')
negative <- scan('negative-words.txt', what='character', comment.char=';')

#Retrieve scores
tweetanalysis <- score.sentiment(tweet, positive, negative, .progress="none")
#Add a component called 'sentiment' to 'tweetanalysis' which stores corresponding sentiment based on score
tweetanalysis$sentiment[tweetanalysis$score == 0] = "Neutral"
tweetanalysis$sentiment[tweetanalysis$score > 0] = "Positive"
tweetanalysis$sentiment[tweetanalysis$score < 0] = "Negative"
tweetanalysis$sentiment <- factor(tweetanalysis$sentiment)

#Make a table of the score and count
scoretable = table(tweetanalysis$score)
score = tweetanalysis$score

#Calculate basic statistical measures of central tendency
mean = mean(score)
median = median(score)
summary(tweetanalysis$sentiment)

#Plot the bar graph with suitable titles
ggplot(data = tweetanalysis, aes(x = score, fill = sentiment)) + 
  geom_bar() + 
  labs(title = "Sentiment Score Bar Plot", x = "Sentiment Score", y = "Tweet Count") +
  scale_x_continuous(breaks = seq(-6,6,1)) + 
  scale_y_continuous(breaks = seq(0,4000,500)) + 
  scale_fill_manual(guide = guide_legend("Sentiment"), values = c("firebrick3","dodgerblue4","seagreen4")) + theme(plot.title = element_text(hjust = 0.5))

#Variation of tweets with time
#Remove duplicate records
tweets <- unique(dataset)
#Add date column
tweetanalysis$date = tweets$created
#Convert date from factors form to date format
tweetanalysis$date <- as.POSIXct(tweetanalysis$date,  tz = "GMT", format = "%d-%m-%Y %H:%M")
#Build datebreaks
#Find the first tweet
minDate <- min(tweetanalysis$date)
#Find the last tweet
maxDate <- max(tweetanalysis$date) + 60 * 60
#Set the breaks for time series analysis
dateBreaks <- seq(minDate, maxDate, by=60 * 60)
#Use hist to count the number of tweets per bin but don't plot
tweetCount <- hist(tweetanalysis$date, breaks=dateBreaks, plot=FALSE)
#Strip out the left endpoint of each bin
binBreaks <- tweetCount$breaks[1:length(tweetCount$breaks)-1]
#creating data to be plotted
plotData <- data.frame(dates=dateBreaks[1:length(dateBreaks)-1], tweets=as.numeric(tweetCount$count))
#customizing the plot
ggplot(plotData) + 
  geom_line(aes(x=dates, y=tweets), color = "khaki4") +
  scale_x_datetime("Time") +
  scale_y_continuous("Number of tweets") +
  labs(title = "Number of tweets over Time") + theme(plot.title = element_text(hjust = 0.5))

#Repeat individually for postive, negative and neutral

#Storing the elements with Neutral Sentiment in a variable
neutraldata = subset(tweetanalysis, subset = sentiment == "Neutral")
#Formatting the date
neutraldata$date <- as.POSIXct(neutraldata$date,  tz = "GMT", format = "%d-%m-%Y %H:%M")
#Assign the minimum and maximum date to variables
minneutraldate = min(neutraldata$date)
maxneutraldate <- max(neutraldata$date) + 60 * 60
#seq(a,b,c) generates a sequence from a to b in steps of c
neutraldatebreaks <- seq(minneutraldate, maxneutraldate, by=60 * 60)
#Generate histogram and store. Give plot=FALSE since we don't want to print it
neutraltweetcount <- hist(neutraldata$date, breaks=neutraldatebreaks, plot=FALSE)
neutralbinbreaks <- neutraltweetcount$breaks[1:length(neutraltweetcount$breaks)-1]
#Converting our data into a data.frame
neutralplotData <- data.frame(dates=neutraldatebreaks[1:length(neutraldatebreaks)-1], tweets=as.numeric(neutraltweetcount$count), sentiment = "Neutral")

#Storing the elements with Positive Sentiment in a variable
Positivedata = subset(tweetanalysis, subset = sentiment == "Positive")
#Formatting the date
Positivedata$date <- as.POSIXct(Positivedata$date,  tz = "GMT", format = "%d-%m-%Y %H:%M")
#Assign the minimum and maximum date to variables
minPositivedate = min(Positivedata$date)
maxPositivedate <- max(Positivedata$date) + 60 * 50
#seq(a,b,c) generates aa sequence from a to b in steps of c
Positivedatebreaks <- seq(minPositivedate, maxPositivedate, by=60 * 50)
#Generate histogram and store. Give plot=FALSE since we don't want to print it
Positivetweetcount <- hist(Positivedata$date, breaks=Positivedatebreaks, plot=FALSE)
Positivebinbreaks <- Positivetweetcount$breaks[1:length(Positivetweetcount$breaks)-1]
#Converting our data into a data.frame
PositiveplotData <- data.frame(dates=Positivedatebreaks[1:length(Positivedatebreaks)-1], tweets=as.numeric(Positivetweetcount$count), sentiment = "Positive")

#Storing the elements with negative Sentiment in a variable
Negativedata = subset(tweetanalysis, subset = sentiment == "Negative")
#Formatting the date
Negativedata$date <- as.POSIXct(Negativedata$date,  tz = "GMT", format = "%d-%m-%Y %H:%M")
#Assign the minimum and maximum date to variables
minNegativedate = min(Negativedata$date)
maxNegativedate <- max(Negativedata$date) + 60 * 50
#seq(a,b,c) generates aa sequence from a to b in steps of c
Negativedatebreaks <- seq(minNegativedate, maxNegativedate, by=60 * 50)
#Generate histogram and store. Give plot=FALSE since we don't want to print it
Negativetweetcount <- hist(Negativedata$date, breaks=Negativedatebreaks, plot=FALSE)
Negativebinbreaks <- Negativetweetcount$breaks[1:length(Negativetweetcount$breaks)-1]
#Converting our data into a data.frame
NegativeplotData <- data.frame(dates=Negativedatebreaks[1:length(Negativedatebreaks)-1], tweets=as.numeric(Negativetweetcount$count), sentiment = "Negative")

#Combining all data into a single data frame 
overallplotdata = rbind(neutralplotData, PositiveplotData, NegativeplotData)
#Plotting everything
ggplot(overallplotdata) + 
  geom_line(aes(x = dates, y = tweets, group = sentiment, colour = sentiment)) + labs(title = "Number of tweets over Time", x = "Time", y = "No. of tweets") + theme(plot.title = element_text(hjust = 0.5)) + scale_color_manual(values = c("dodgerblue4", "forestgreen", "firebrick3"))

