############################################################
############################################################
# This script is used for Sentiment Analysis
############################################################
############################################################
save(tweets.after, file = "translations.RData")

# first of all we need to prepare the tweet texts
texts <- as.character(tweets.df[tweets.df$lang != "en","text"])

# only original posts
retweets  <- grepl("(RT)(?:\\b\\W@\\w+)", tweets.df$text, ignore.case = TRUE )
texts <- texts[!retweets]

# remove retweets and Original Poster
texts <- gsub("(RT)(?:\\b\\W@\\w+)", "", texts)
# remove links
texts <- gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", texts)
# remove hashtags
texts <- gsub("[#]{1}(\\w+)", "", texts)
# remove punctuation
texts <- gsub("[[:punct:]]", "", texts)
# remove numerics
texts <- gsub("[[:digit:]]", "", texts)
# remove beginning whitespaces
texts <- gsub("^\\s+|\\s+$", "", texts)
# remove duplicate whitespaces
texts <- gsub("\\s+", " ", texts)



######## TESTING

load("Data/microsoft_api.RData")

tw <- tweets.df[3,]
tw$text <- as.character(tw$text)

texts <- texts_temp

# encode texts
Encoding(texts) <- "UTF-8"
texts <- iconv(texts, "ASCII", "UTF-8", sub = "")

texts <- tolower(texts)

sum(nchar(texts))

texts <- as.data.frame(texts)

test <- cbind(tweets.df, texts_temp)
test$texts_temp <- as.character(test$texts_temp)
test <- test[test$lang != "en",]


test$texts_temp <- iconv(test$texts_temp, to="UTF-8")


texts2 <- iconv(texts, to = "UTF-8")

# convert to lower case
texts <- tolower(texts)


