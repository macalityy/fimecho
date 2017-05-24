######################################################################
######################################################################
# This script is being used in order to convert all tweets out of a
# data frame to an edgelist and list of vertices respectively
# At the end one can draw a sample out of the data for faster processing
######################################################################
######################################################################

# Load Libraries
library(plyr)
library(dplyr)
library(stringr)
workingDT <- getwd()

#load("fimecho/Data/Turkey/TweetsDF.RData")


#tweets.df<-tweetsBefore.df


#shrink dataframe to columns we might use later
tweets.df <-
  subset(tweets.df[, c(
    "X",
    "text",
    "truncated",
    "id_str",
    "source",
    "created_at",
    "lang",
    "listed_count",
    "location",
    "user_id_str",
    "geo_enabled",
    "user_created_at",
    "statuses_count",
    "followers_count",
    "favourites_count",
    "time_zone",
    "user_lang",
    "friends_count",
    "screen_name",
    "expanded_url",
    "url"
  )])

tweets.df <- transform(
  tweets.df,
  id_str = as.character(id_str),
  user_id_str = as.character(user_id_str),
  text = as.character(text)
)

#get all vertices data
users.df <-
  subset(tweets.df[, c(
    "user_id_str",
    "screen_name",
    "geo_enabled",
    "user_created_at",
    "time_zone",
    "user_lang"
  )])

#remove duplicates by twitters user_id
users.df <- users.df %>% distinct(user_id_str, .keep_all = TRUE)

# get all vertices and their count variables
v.count.df <- subset(tweets.df[, c(
  "user_id_str",
  "statuses_count",
  "followers_count",
  "favourites_count",
  "friends_count"
)])

#summarize vertices based on max function
v.count.df <- v.count.df %>% group_by(user_id_str) %>% summarise_at(
  c(
    "statuses_count",
    "followers_count",
    "favourites_count",
    "friends_count"
  ),
  funs(max, max, max, max)
)

#merge both sets together
users.df <-
  merge(x = users.df,
        y = v.count.df,
        by = "user_id_str",
        all = TRUE)
rm(v.count.df)

# now we need all retweet ids (those lines in tweets.df which are actual retweets)
retweets.ids  <-
  grep("(RT)(?:\\b\\W@\\w+)", tweets.df$text, ignore.case = TRUE)

# create list to store user names of retweets
user.originaltw <- as.list(1:length(retweets.ids))
user.retweet <- as.list(1:length(retweets.ids))

# from the data frame, we only need the column text to find out which rows are retweets
tweets.text <- tweets.df[, "text"]

## User Identification in Retweets ##
for (i in 1:length(retweets.ids))
{
  #get tweet text
  tweet.text <- tweets.text[[retweets.ids[i]]]
  #find user who posted original tweet
  tweet.original.poster <-
    str_extract_all(tweet.text, "(RT)(?:\\b\\W@\\w+)")
  
  #save user who posted original tweet in vector
  user.originaltw[[i]] = gsub("(RT @)", "", tweet.original.poster, ignore.case = TRUE)
  
  #save user who retweeted in vector
  user.retweet[[i]] = tweets.df[retweets.ids[i], "screen_name"]
}

rm(tweets.text)

user.originaltw <- unlist(user.originaltw)
user.retweet <- unlist(user.retweet)

# generate edgelist
edgelist.df <- as.data.frame(cbind(user.retweet, user.originaltw))

detach("package:dplyr", unload = TRUE)
#get # of ties for each retweeter and original tweeter
user.originaltw.freq <- count(user.originaltw)
user.retweet.freq <- count(user.retweet)
library(dplyr)

#change colnames of data frame to tiesin and tiesout
colnames(user.originaltw.freq) <- c("x", "ties_in")
colnames(user.retweet.freq) <- c("x", "ties_out")

#build a data frame with # of ties for each user
user.ties.df <- merge(user.originaltw.freq,
                      user.retweet.freq,
                      by = "x",
                      all = TRUE)

rm(user.originaltw.freq)
rm(user.retweet.freq)

user.ties.df$x <- as.character(user.ties.df$x)

vertices.df <-
  merge(
    users.df,
    user.ties.df,
    by.x = "screen_name",
    by.y = "x",
    all.x = TRUE
  )

rm(user.ties.df)

#change all NA values to 0
vertices.df$ties_in[is.na(vertices.df$ties_in)] <- 0
vertices.df$ties_out[is.na(vertices.df$ties_out)] <- 0
#calculate sum of ties
vertices.df$ties_sum <- vertices.df$ties_in + vertices.df$ties_out

#filter those users who dont have at least 3 or more connections
vertices.df <- subset(vertices.df, vertices.df$ties_sum >= 3)

colnames(vertices.df) <- c(
  "Id",
  "usr_Id",
  "geo_enabled",
  "user_created_at",
  "time_zone",
  "user_lang",
  "status_count",
  "followers_count",
  "favourites_count",
  "friends_count",
  "ties_in",
  "ties_out",
  "ties_sum"
)
colnames(edgelist.df) <- c("Source", "Target")

# save both data frames to files
save(vertices.df, file = "Data/Turkey/Vertices2New.RData")
save(edgelist.df, file = "Data/Turkey/Edges2New.RData")

head(edgelist.df)
head(vertices.df)
edgelist.df <-
  subset(
    edgelist.df,
    (edgelist.df$Source %in% vertices.df$Id) &
      (edgelist.df$Target %in% vertices.df$Id)
  )

tweets_sc_st.df <- as.data.frame(vertices.df[, c(1, 2)])
colnames(tweets_sc_st.df) < -c("Source", "user_id_Source")
edgelist.df <-
  merge(
    edgelist.df,
    tweets_sc_st.df,
    by = "Source",
    all.x = TRUE,
    all.y = FALSE
  )

colnames(tweets_sc_st.df) <- c("Target", "user_id_Target")
edgelist.df <-
  merge(
    edgelist.df,
    tweets_sc_st.df,
    by = "Target",
    all.x = TRUE,
    all.y = FALSE
  )

edgelist.df <- edgelist.df[3:4]

save(edgelist.df, file = "Edgelist2New.RData")


### SAMPLE SET
knoten.df <- sample_frac(vertices.df, size = 0.1)
kanten.df <-
  subset(
    edgelist.df,
    (edgelist.df$Source %in% knoten.df$usr_Id) &
      (edgelist.df$Target %in% knoten.df$usr_Id)
  )

write.csv2(knoten.df, file = paste(c(
  workingDT, "/Data/Turkey/SampleKnoten.csv"
), collapse = ""))
write.csv2(kanten.df, file =  paste(c(
  workingDT, "/Data/Turkey/SampleKanten.csv"
), collapse = ""))
