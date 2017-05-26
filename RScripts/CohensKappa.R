################################################################################
################################################################################
# Calculate Cohens Kappa for manual classification
################################################################################
################################################################################

library(irr)
library(dplyr)

### Cohens Kappa

load("Data/Seminar/SentimentDF.RData")

nrow(tweets_after.df)
nrow(tweets_after.df[tweets_after.df$lang == "en", ])
nrow(tweets_after.df[tweets_after.df$lang == "tr", ])
nrow(tweets_after.df[tweets_after.df$lang == "de", ])


tr_sample <-
  sample_frac(tweets_after.df[tweets_after.df$lang == "tr", c("id_str", "text")], 0.0025)
de_sample <-
  sample_frac(tweets_after.df[tweets_after.df$lang == "de", c(2, 3)], 0.005)
en_sample <-
  sample_frac(tweets_after.df[tweets_after.df$lang == "en", c(2, 3)], 0.0025)

tr_sample$sentiment <- 0
de_sample$sentiment <- 0
en_sample$sentiment <- 0

write.csv2(tr_sample, file = "TR_Sample.csv")
write.csv2(de_sample, file = "DE_Sample.csv")
write.csv2(en_sample, file = "EN_Sample.csv")



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

save(tweet.sent, file = "Data/Seminar/TweetID_Sentiments.RData")


temp <- subset(tweet.sent, is.na(tweet.sent$manual_sentiment) == FALSE)

irr::kappa2(temp[,c("class","manual_sentiment")], "unweighted")

  