################################################################################
################################################################################
# Calculate Cohens Kappa for manual classification
################################################################################
################################################################################

library(irr)
library(dplyr)

### Cohens Kappa

load("Data/Seminar/SentimentDF.RData")
load("Data/Seminar/TweetsAfter.RData")

nrow(tweets_after.df)
nrow(tweets_after.df[tweets_after.df$lang == "en", ])
nrow(tweets_after.df[tweets_after.df$lang == "tr", ])
nrow(tweets_after.df[tweets_after.df$lang == "de", ])


tr_sample <-
  sample_frac(tweets_after.df[tweets_after.df$lang == "tr", c("X", "id_str", "text")], 0.0025)
de_sample <-
  sample_frac(tweets_after.df[tweets_after.df$lang == "de", c("X", "id_str", "text")], 0.005)
en_sample <-
  sample_frac(tweets_after.df[tweets_after.df$lang == "en", c("X", "id_str", "text")], 0.001)

tr_sample$sentiment <- 0
de_sample$sentiment <- 0
en_sample$sentiment <- 0

write.csv2(tr_sample, file = "TR_Sample.csv")
write.csv2(de_sample, file = "DE_Sample.csv")

de_sample <- read.csv2("Data/Seminar/DE_Sample.csv")
de_sample <- de_sample[, c(1, 2, 4)]
de_sample <- merge(de_sample, temp, by = "X", all.x = TRUE)


tweet.sent <-
  merge(tweet.sent,
        de_sample,
        by = "id_str",
        all.x = TRUE,
        all.y = FALSE)

colnames(tweet.sent) <-
  c("id_str",
    "user_id_str",
    "ave_sentiment",
    "class",
    "manual_sentiment")


en_sample$sentiment <-
  c(
    2,
    1,
    1,
    3,
    1,
    3,
    1,
    2,
    1,
    2,
    2,
    1,
    3,
    2,
    2,
    2,
    1,
    2,
    1,
    2,
    2,
    1,
    2,
    2,
    3,
    2,
    2,
    2,
    1,
    2,
    1,
    1,
    2,
    2,
    2,
    1,
    3,
    2,
    1,
    2,
    1,
    1,
    1,
    1,
    1,
    1,
    2,
    3,
    2,
    2,
    2,
    1,
    2,
    2,
    2,
    1,
    2,
    2,
    2,
    1,
    2,
    1,
    1,
    2,
    2,
    1,
    2,
    1,
    2,
    1
  )

en_sample <- en_sample[,-1]
en_sample <- en_sample[, c(1, 3)]

save(en_sample, file = "Data/Seminar/EN_Sample.RData")

load("Data/Seminar/TweetID_Sentiments.RData")

tweet.sent <-
  merge(tweet.sent,
        en_sample,
        by = "id_str",
        all.x = TRUE,
        all.y = FALSE)

table(is.na(tweet.sent$manual_sentiment))
table(is.na(tweet.sent$sentiment))

tweet.sent$manual_sentiment <-
  ifelse(
    is.na(tweet.sent$manual_sentiment),
    tweet.sent$sentiment,
    tweet.sent$manual_sentiment
  )

table(is.na(tweet.sent$manual_sentiment))
tweet.sent <- tweet.sent[,c(1:5)]

save(tweet.sent, file = "Data/Seminar/TweetID_Sentiments.RData")

temp <-
  subset(tweet.sent, is.na(tweet.sent$manual_sentiment) == FALSE)

table(tweet.sent$manual_sentiment)
sum(table(tweet.sent$manual_sentiment))

irr::kappa2(temp[, c("class", "manual_sentiment")], "unweighted")

cor(temp$class, temp$manual_sentiment)
