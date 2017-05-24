############################################################
############################################################
# This script is used for Sentiment Analysis
############################################################
############################################################

library(tm)
library(sentimentr)
library(lexicon)
library(ggplot2)

# get relevant fields of tweets after first election results
# came in until the end of the next day
# extract only necessary fields
sentiment <-
  tw.dayafter[, c("X", "text", "id_str", "lang", "user_id_str")]

sentiment$text <- as.character(sentiment$text)

# now merge with translations
sentiment <-
  merge(
    sentiment,
    translations,
    by = "id_str",
    all.x = TRUE,
    all.y = FALSE
  )

# get IDs of those tweets which aren't already english
not.en <- which(sentiment$lang != "en")

# number of tweets which need translation (are not English)
length(not.en)

# now replace the original text with our translation
sentiment[not.en, "text"] <- sentiment[not.en, "translation"]
# and get rid of the translation column
sentiment <- sentiment[, c(1:5)]

# save it, just in case
save(sentiment, file = "Data/Seminar/SentimentData.RData")
load("Data/Seminar/SentimentData.RData")

# backup variable
backup.sent <- sentiment
sentiment <- backup.sent


# number of tweets to be analyzed
nrow(sentiment)

# create column for analysis and transformation
sentiment$analysis <- sentiment$text

# remove RT and Original Poster
sentiment$analysis <-
  gsub("(RT)(?:\\b\\W@\\w+)", "", sentiment$analysis)
# remove links
sentiment$analysis <-
  gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", sentiment$analysis)
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
  
  try_error <- tryCatch(
    tolower(x),
    error = function(e)
      e
  )
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


# define a vector to have an ID row (for manual analysis)
sentiment$id <- as.character(c(1:(nrow(sentiment))))

#encode correctly (since turkish has some not readible s)
sentiment$encode <-
  iconv(sentiment$analysis, "UTF-8", "ASCII", "byte")

# rearrange colums
sentiment <- sentiment[, c(8, 1, 2, 3, 4, 5, 6, 7)]
# analyze the sentiment
analysis <-
  sentiment_by(
    sentiment[, "encode"],
    #polarity_dt = lexicon::hash_sentiment_huliu,
    valence_shifters_dt = lexicon::hash_valence_shifters,
    amplifier.weight = 0.8,
    n.before = 5,
    n.after = 2,
    question.weight = 1,
    adversative.weight = 0.85,
    missing_value = 0
  )

summary(analysis)

tweet.sent <-
  as.data.frame(cbind(sentiment[, "id_str"], analysis[, "ave_sentiment"]))
colnames(tweet.sent) <- c("id_str", "sent_value")
tweet.sent <-
  transform(tweet.sent,
            id_str = as.character(id_str),
            sent_value = as.numeric(sent_value))

save(tweet.sent, file = "Data/Seminar/TweetID_Sentiments.RData")

########################################################################
## PLOTTING
########################################################################

# calculate density
sentiment.density <- density(analysis$ave_sentiment)
require(pastecs)
tp <- turnpoints(ts(sentiment.density$y))

# plot it
plot(sentiment.density, main = "Sentiment Distribution")
# plot the specific local minimum points
#points(sentiment.density$x[209], sentiment.density$y[209], col = "red")
#points(sentiment.density$x[228], sentiment.density$y[228], col = "red")
# all turning points
#points(sentiment.density$x[tp$tppos],sentiment.density$y[tp$tppos],col="red")

# we will use the smaller absolute minimum to have a symmetric classification
# therefore we wont use sentiment.density$y[209],
# but negative value of sentiment.density$y[228]

# define intervals
x0 <- min(which(sentiment.density$x >= sentiment.density$x[1]))
x1 <- min(which(sentiment.density$x >= -sentiment.density$x[228]))
x2 <- max(which(sentiment.density$x <  sentiment.density$x[228]))
x3 <- max(which(sentiment.density$x <  sentiment.density$x[512]))

# colour segments
with(sentiment.density, polygon(
  x = c(x[c(x0, x0:x1, x1)]),
  y = c(0, y[x0:x1], 0),
  col = "red"
))
with(sentiment.density, polygon(
  x = c(x[c(x1, x1:x2, x2)]),
  y = c(0, y[x1:x2], 0),
  col = "grey"
))
with(sentiment.density, polygon(
  x = c(x[c(x2, x2:x3, x3)]),
  y = c(0, y[x2:x3], 0),
  col = "green"
))

# interval for neutral classification is:
min <- -sentiment.density$x[228]
max <- sentiment.density$x[228]

# define factor

freq <- c(nrow(analysis[analysis$ave_sentiment < min]),
          nrow(analysis[analysis$ave_sentiment <= max]),
          nrow(analysis[analysis$ave_sentiment > max]))

barplot(
  freq,
  names.arg = c("Negative", "Neutral", "Positive"),
  col = c("red", "grey", "green")
)

