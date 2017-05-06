# Load Libraries
library(stringr)
library(igraph)
library(streamR)
library(plyr)

file <- "/users/flori/fimecho/Data/T0418.csv"
filename.out<-"/users/flori/fimecho/Data/User_Hashtag.csv"
#read csv file to get tweets into dataframe
tweets.df <- read.csv2(file = file, header = TRUE, stringsAsFactors = FALSE)

#shrink dataframe to columns we might use later
tweets.df <- subset(tweets.df[,c("X.1", "X", "text", "truncated", "id_str", "source",
                                 "created_at", "lang", "listed_count", "location",
                                 "user_id_str", "geo_enabled", "user_created_at",
                                 "statuses_count", "followers_count", "favourites_count",
                                 "time_zone", "user_lang", "friends_count", "screen_name",
                                 "expanded_url", "url")])

# from the data frame, we only need the column text to find out which rows are retweets
tweets.text <- tweets.df[,"text"]



# Extract all the Hashtags per Tweet
tweets.hashtags  <- str_extract_all(tweets.text,"[#]{1}(\\w+)")
# Extract tweet ids which include a hashtag
tweets.hashtags.ids <- grep("[#]{1}(\\w+)",tweets.text)
# Extract usernames from tweet dataframe
user.originaltw<-tweets.df["screen_name"]

#Remove empty vectors in hashtag list
tweets.hashtags<-tweets.hashtags[lapply(tweets.hashtags,length)>0]

tweets.hashtags

UsrHashtag<-c("User","Hashtag")

for (i in 1:length(tweets.hashtags))
{
  for (j in 1:length(tweets.hashtags[[i]]))
  {
    UsrHashtag<-rbind(UsrHashtag,c(user.originaltw[[tweets.hashtags.ids[i],1]],tweets.hashtags[[i]][j]))
  }
}
#Remove first row of matrix
UsrHashtag = UsrHashtag[-1,]
head(UsrHashtag)


UHFreq.df<-as.data.frame(count(UsrHashtag))
write.csv2(UHFreq.df, file = filename.out)
