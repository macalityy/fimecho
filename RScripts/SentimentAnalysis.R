############################################################
############################################################
# This script is used for Sentiment Analysis
############################################################
############################################################

library(tm)
library(sentimentr)

# get relevant fields of tweets after first election results
# came in until the end of the next day
# extract only necessary fields
sentiment <- tw.dayafter[,c("X","text","id_str","lang","user_id_str")]

sentiment$text <- as.character(sentiment$text)

# now merge with translations
sentiment <- merge(sentiment, translations, by = "id_str", all.x = TRUE, all.y = FALSE)

# get IDs of those tweets which aren't already english
not.en <- which(sentiment$lang != "en")

# number of tweets which need translation (are not English)
length(not.en)

# now replace the original text with our translation
sentiment[not.en, "text"] <- sentiment[not.en, "translation"]
# and get rid of the translation column
sentiment <- sentiment[,c(1:5)]

# save it, just in case
save(sentiment, file = "Data/Seminar/SentimentData.RData")

# number of tweets to be analyzed
nrow(sentiment)

# create column for analysis and transformation
sentiment$analysis <- sentiment$text

# remove RT and Original Poster
sentiment$analysis <- gsub("(RT)(?:\\b\\W@\\w+)", "", sentiment$analysis)
# remove links
sentiment$analysis <- gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", sentiment$analysis)
# remove hashtags
sentiment$analysis <- gsub("[#]{1}(\\w+)", "", sentiment$analysis)
# remove punctuation
sentiment$analysis <- gsub("[[:punct:]]", "", sentiment$analysis)
# remove numerics
sentiment$analysis <- gsub("[[:digit:]]", "", sentiment$analysis)
# remove beginning whitespaces
sentiment$analysis <- gsub("^\\s+|\\s+$", "", sentiment$analysis)
# remove duplicate whitespaces
sentiment$analysis <- gsub("\\s+", " ", sentiment$analysis)

# define function to convert text to lower case
convert.toLower <- function(x) {
  y <- NA
  
  try_error <- tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, "error")) {
    y = tolower(x)
  }
  return(y)
}

# apply function to each row
sentiment$analysis <- sapply(sentiment$analysis, convert.toLower)

# remove those lines which could not be converted to lower case
sentiment <- sentiment[!is.na(sentiment$analysis),]
# remove those lines which only contained hashtags and links
# and therefore are now empty
sentiment <- sentiment[sentiment$analysis != "",]

# number of records to analyzed by sentiment:
nrow(sentiment)


#encode correctly
sentiment$encode <- iconv(sentiment$analysis, "UTF-8", "ASCII", "byte")

analysis <- sentiment_by(sentiment[,"encode"])


######## TESTING
### text mining stuff
# create corpus from texts
corpus <- VCorpus(VectorSource(texts))

# strip whitespace
corpus <- tm_map(corpus, stripWhitespace)
# to lower
corpus <- tm_map(corpus, content_transformer(tolower))
# remove stopwords
corpus <- tm_map(corpus, removeWords, stopwords("en"))


temp <- sentiment$analysis

Encoding(temp)
# encode texts
Encoding(temp) <- "UTF-8"
temp <- iconv(temp, "latin1", "ASCII", "byte")

analysis <- sentiment_by(temp)


texts <- iconv(texts, "ASCII", "UTF-8", sub = "")

sum(nchar(texts))

texts <- as.data.frame(texts)

test <- cbind(tweets.df, texts_temp)
test$texts_temp <- as.character(test$texts_temp)
test <- test[test$lang != "en",]


test$texts_temp <- iconv(test$texts_temp, to="UTF-8")
texts2 <- iconv(texts, to = "UTF-8")

# convert to lower case
texts <- tolower(texts)


