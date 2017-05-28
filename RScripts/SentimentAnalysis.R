############################################################
############################################################
# This script is used for Sentiment Analysis
############################################################
############################################################

library(tm)
library(sentimentr)
library(lexicon)
library(ggplot2)

load("Data/Seminar/TweetsAfter.RData")
load("Data/Seminar/Translations.RData")

# get relevant fields of tweets after first election results
# came in until the end of the next day
# extract only necessary fields
sentiment <-
  tweets_after.df[, c("X", "text", "id_str", "lang", "user_id_str")]

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
sentiment <- sentiment[!is.na(sentiment$analysis), ]
# remove those lines which only contained hashtags and links
# and therefore are now empty
sentiment <- sentiment[sentiment$analysis != "", ]

# number of records to analyzed by sentiment:
nrow(sentiment)


# define a vector to have an ID row (for manual analysis)
sentiment$id <- as.character(c(1:(nrow(sentiment))))

#encode correctly (since turkish has some not readible s)
sentiment$encode <-
  iconv(sentiment$analysis, "UTF-8", "ASCII", "byte")

# rearrange colums
sentiment <- sentiment[, c(7, 1, 2, 3, 4, 5, 6, 8)]
# analyze the sentiment
analysis <-
  sentiment_by(
    sentiment[, "encode"]
    # polarity_dt = lexicon::hash_sentiment_huliu,
    #  valence_shifters_dt = lexicon::hash_valence_shifters,
    #  amplifier.weight = 0.8,
    #  n.before = 5,
    #  n.after = 2,
    #  question.weight = 1,
    #  adversative.weight = 0.85,
    #  missing_value = 0
  )

summary(analysis)

save(analysis, file = "Data/Seminar/ResultofSentimentR.RData")
load ("Data/Seminar/ResultofSentimentR.RData")
save(sentiment, file = "Data/Seminar/SentimentDF.RData")
load ("Data/Seminar/SentimentDF.RData")

########################################################################
## PLOTTING
########################################################################

# calculate density
sentiment.density <- density(analysis$ave_sentiment)
require(pastecs)
tp <- turnpoints(ts(sentiment.density$y))

summary(tp)

# plot it
plot(sentiment.density, main = "Sentiment Distribution", lwd = 1.5)

# all turning points
points(sentiment.density$x[tp$tppos],sentiment.density$y[tp$tppos],col="tomato", pch = 16)

plot(sentiment.density, xlim = c(-1,1), ylim = c(0,1.5), lwd = 1.5, main = "Sentiment Distribution (Zoom)")
points(sentiment.density$x[tp$tppos],sentiment.density$y[tp$tppos],col="tomato", pch = 16)

plot(sentiment.density, xlim = c(-0.5,0.5), ylim = c(0,1.5), lwd = 1.5, main = "Sentiment Distribution (Zoom 2x)")
points(sentiment.density$x[tp$tppos],sentiment.density$y[tp$tppos],col="tomato", pch = 16)

# plot the specific local minimum points
plot(sentiment.density, xlim = c(-0.5,0.5), ylim = c(0,1.5), lwd = 1.5, main = "Sentiment Distribution (Zoom 2x)")
points(sentiment.density$x[310], sentiment.density$y[310], col = "tomato", pch = 16)
points(sentiment.density$x[319], sentiment.density$y[319], col = "tomato", pch = 16)

text(sentiment.density$x[310], (sentiment.density$y[310]-0.1),
     paste(c("x1=",round(sentiment.density$x[310], digits = 4)), collapse = ""))
text(sentiment.density$x[319], (sentiment.density$y[319]-0.1),
     paste(c("x2=",round(sentiment.density$x[319], digits = 4)), collapse = ""))

plot(sentiment.density, xlim = c(-0.5,0.5), ylim = c(0,1.5), lwd = 1.5, main = "Sentiment Distribution (Zoom 2x)")
abline(v=c(class$min), col = "tomato", lty = "dotted", lwd = 3)
abline(v=c(class$max), col = "tomato", lty = "dotted", lwd = 3)

# we will use the smaller absolute minimum to have a symmetric classification
# therefore we wont use sentiment.density$y[209],
# but negative value of sentiment.density$y[228]

# define intervals
x0 <- min(which(sentiment.density$x >= sentiment.density$x[1]))
x1 <- min(which(sentiment.density$x >= (-sentiment.density$x[319])))
x2 <- max(which(sentiment.density$x <  sentiment.density$x[319]))
x3 <- max(which(sentiment.density$x <  sentiment.density$x[512]))


# plot it
plot(sentiment.density, xlim = c(-1,1), main = "Classified Sentiment Distribution", lwd = 1.5)
# colour segments
with(sentiment.density, polygon(
  x = c(x[c(x0, x0:x1, x1)]),
  y = c(0, y[x0:x1], 0),
  col = "tomato"
))
with(sentiment.density, polygon(
  x = c(x[c(x1, x1:x2, x2)]),
  y = c(0, y[x1:x2], 0),
  col = "wheat"
))
with(sentiment.density, polygon(
  x = c(x[c(x2, x2:x3, x3)]),
  y = c(0, y[x2:x3], 0),
  col = "springgreen4"
))

# interval for neutral classification is:
class <- list()
class$min <- -sentiment.density$x[319]
class$max <- sentiment.density$x[319]

save(class, file = "Data/Seminar/ClassificationInterval.RData")
load("Data/Seminar/ClassificationInterval.RData")

# define frequency
freq <- c(nrow(analysis[analysis$ave_sentiment < class$min]),
          nrow(analysis[analysis$ave_sentiment <= class$max]),
          nrow(analysis[analysis$ave_sentiment > class$max]))

freq <- (freq/sum(freq))*100
# barplot
barplot(
  freq,
  names.arg = c("Negative", "Neutral", "Positive"),
  col = c("red", "grey", "green"),
  ylim = c(0,50)
)


#################################################################################
# Transform Tweet Data to User Data
#################################################################################
# Bind Tweet ID and Average Sentiment
tweet.sent <-
  as.data.frame(cbind(sentiment[, c("id_str", "user_id_str")], analysis[, "ave_sentiment"]))

colnames(tweet.sent) <- c("id_str", "user_id_str", "ave_sentiment")
# Transform and Cast
tweet.sent <-
  transform(
    tweet.sent,
    id_str = as.character(id_str),
    user_id_str = as.character(user_id_str),
    ave_sentiment = as.numeric(ave_sentiment)
  )

# aggregate tweet sentiment to user sentiment
user.sent <-
  aggregate(tweet.sent$ave_sentiment, list(tweet.sent$user_id_str), mean)

colnames(user.sent) <- c("user_id_str", "ave_sentiment")

summary(tweet.sent)
table(tweet.sent$class)
sum(table(tweet.sent$class))

sd(tweet.sent$ave_sentiment)
summary(user.sent)
sd(user.sent$ave_sentiment)

# Save to File
save(tweet.sent, file = "Data/Seminar/TweetID_Sentiments.RData")
load("Data/Seminar/TweetID_Sentiments.RData")
save(user.sent, file = "Data/Seminar/UserID_AveSentiments.RData")
load("Data/Seminar/UserID_AveSentiments.RData")


# merge sentiment with vertices.df
vertices.df <-
  merge(
    vertices.df,
    user.sent,
    by.x = "Id",
    by.y = "user_id_str",
    all.x = TRUE,
    all.y = FALSE
  )

### Tweet Classification
class <- vector(length = nrow(vertices.df))


classify <- function(x) {
  if ( is.na(x) == TRUE) {
    y <- 0
  } else if ( x < class$min ) {
    y <- 1  
  } else if ( x > class$max ) {
    y <- 3
  } else {
    y <- 2
  }
}

tweet.sent$class <- sapply(tweet.sent$ave_sentiment, classify)
vertices.df$class <- sapply(vertices.df$ave_sentiment, classify)

# label levels of classification factor
vertices.df$class <-
  factor(vertices.df$class,
         labels = c("Not Assessed", "Negative", "Neutral", "Positive"))


vertices.df <-
  transform(
    vertices.df,
    ml_comm = as.numeric(ml_comm),
    wt_comm = as.numeric(wt_comm),
    lp_comm = as.numeric(lp_comm),
    im_comm = as.numeric(im_comm)
  )

save(vertices.df, file = "Data/Seminar/VerticesAssessed.RData")

summary(vertices.df$ave_sentiment)
table(vertices.df$class)
sum(table(vertices.df$class))
sd(vertices.df[!is.na(vertices.df$ave_sentiment), "ave_sentiment"])


###### PLOTTING
freq <- summary(vertices.df$class)
freq <- (freq / sum(freq)) * 100

barplot(
  freq,
  names.arg = c("Not Assessed", "Negative", "Neutral", "Positive"),
  col = c("gray", "tomato", "wheat", "springgreen4"),
  main = "User Classification",
  ylab = "Percentage",
  ylim = c(0, 30)
)
abline(
  h = seq(0, 50, 5),
  col = "snow4",
  lty = "dotted"
)
barplot(
  freq,
  names.arg = c("Not Assessed", "Negative", "Neutral", "Positive"),
  col = c("gray", "tomato", "wheat", "springgreen4"),
  main = "User Classification",
  ylab = "Percentage",
  ylim = c(0, 30),
  add = TRUE
)

# define frequency
freq <- c(nrow(user.sent[user.sent$ave_sentiment < class$min, ]),
          nrow(user.sent[user.sent$ave_sentiment <= class$max & user.sent$ave_sentiment >= class$min, ]),
          nrow(user.sent[user.sent$ave_sentiment > class$max, ]))

freq <- (freq / sum(freq) * 100)

# plot(density(user.sent$ave_sentiment))

# barplot
barplot(
  freq,
  names.arg = c("Negative", "Neutral", "Positive"),
  col = c("tomato", "wheat", "springgreen4"),
  main = "User Classification",
  ylab = "Percentage",
  ylim = c(0, 50)
)

abline(
  h = seq(0, 50, 10),
  col = "snow4",
  lty = "dotted"
)

barplot(
  freq,
  names.arg = c("Negative", "Neutral", "Positive"),
  col = c("tomato", "wheat", "springgreen4"),
  main = "User Classification",
  ylab = "Percentage",
  ylim = c(0, 50),
  add = TRUE
)