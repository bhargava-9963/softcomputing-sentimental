
apple <- read.csv(file.choose(), header = T)
str(apple)


library(tm)
corpus <- iconv(apple$text, to='UTF-8', sub = "byte")
corpus <- Corpus(VectorSource(corpus))
inspect(corpus[1:10])


corpus <- tm_map(corpus, tolower)
inspect(corpus[1:5])


corpus <- tm_map(corpus, removePunctuation)
inspect(corpus[1:5])


corpus <- tm_map(corpus, removeNumbers)
inspect(corpus[1:5])

cleanset <- tm_map(corpus, removeWords, stopwords('english'))
inspect(cleanset[1:5])


removeURL <- function(x) gsub('http[[:alnum:]]*', '', x)
cleanset <- tm_map(cleanset, content_transformer(removeURL))
inspect(cleanset[1:5])





cleanset <- tm_map(cleanset, removeWords, c('aapl', 'apple'))
cleanset <- tm_map(cleanset, gsub, 
                   pattern = 'stocks', 
                   replacement = 'stock')

cleanset <- tm_map(cleanset, stripWhitespace)
inspect(cleanset[1:5])


tdm <- TermDocumentMatrix(cleanset)
tdm
tdm <- as.matrix(tdm)
tdm[1:10, 1:20]


w <- rowSums(tdm)
w <- subset(w, w>=25)
barplot(w,las = 2,
        col = rainbow(50))


library(wordcloud)
w <- sort(rowSums(tdm), decreasing = TRUE)
set.seed(222)
wordcloud(words = names(w),
          freq = w,
          max.words = 150,
          random.order = F)


library(wordcloud2)
w <- data.frame(names(w), w)
colnames(w) <- c('word', 'freq')
wordcloud2(w,
           size = 0.7,
           shape = 'triangle')





library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(reshape2)
library(dplyr)

apple <- read.csv(file.choose(), header = T)
tweets <- iconv(apple$text, to ='UTF-8',sub="byte")


s <- get_nrc_sentiment(tweets)
head(s)
tweets[4]

get_nrc_sentiment('ugly')

barplot(colSums(s),las = 2,col = rainbow(10),ylab = 'Count',main = 'Sentiment Scores for Apple Tweets')

