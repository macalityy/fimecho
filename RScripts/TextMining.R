

library(stringr)
library(igraph)
library(streamR)
library(plyr)
library(dplyr)
library(tm)
library (readr)
library(wordcloud)


# Load file
file <- "C:/Users/Dominik/Documents/TürkeiReferendumData/english_tweets.csv"
# Read file into R
tweets.df <- read.csv2(file, header = TRUE, stringsAsFactors = FALSE)
# Select text-column
tweets.text <- tweets.df["text"]
#View(tweets.text)

#Build Corpus for Text Mining
mycorpus <- Corpus(VectorSource(tweets.text))

#Start Text Mining

mycorpus <-tm_map(mycorpus, content_transformer(tolower))## Set only lower case letters
mycorpus <-tm_map(mycorpus, stripWhitespace)#Several blanks become one single blank
mycorpus <-tm_map(mycorpus, removeNumbers)#Remove Numbers
mycorpus <-tm_map(mycorpus, removePunctuation)#Remove Punctation

# Removing Stopwords

stopwordselse <- c("but","the", "and","of","rt","to","yesterday","in","with","http", "https","@")
mycorpus <-tm_map(mycorpus, removeWords, stopwords("en"))
mycorpus <-tm_map(mycorpus, removeWords, stopwordselse)


#Build Term Document Matrx
tdm <- TermDocumentMatrix(mycorpus,control=list(wordLengths=c(0,Inf)))
matrix <- as.matrix(tdm)

wordresult <- sort(rowSums(matrix),decreasing=TRUE) #Sort words in decreasing frequency order 

# converting matrix to data frame
matrix.df <- data.frame(word=names(wordresult[3:30]),freq=wordresult[3:30])

#Wordcloud

wordcloud(matrix.df$word, matrix.df$freq)

