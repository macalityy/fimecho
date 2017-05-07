# Load Libraries
library(stringr)
library(igraph)
library(streamR)
library(plyr)
library(dplyr)
file <- "/users/flori/fimecho/Data/Turkey/Turkeyall.csv"
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

##Filter for Maximum Hashtag-Frequency
##Read CSV File into UserHashtagFrequency Dataframe (UHF)
UHF.df <- read.csv2(file = filename.out, header = TRUE, stringsAsFactors = FALSE)

##Identification of unique users
Users.df <- UHF.df %>% distinct(x.1)

##Select Maximum 1 used hashtags per user
##Returns Dataframe with most frequently used hashtag per user (Multiple Maximum HashtagsFrequency possible)
UHTop1Freq.df <- UHF.df %>% group_by(x.1)%>% top_n(n = 1, wt= freq)
UHTop2Freq.df <- UHF.df %>% group_by(x.1)%>% top_n(n = 2, wt= freq)
UHTop3Freq.df <- UHF.df %>% group_by(x.1)%>% top_n(n = 3, wt= freq)
UHTop4Freq.df <- UHF.df %>% group_by(x.1)%>% top_n(n = 4, wt= freq)
UHTop5Freq.df <- UHF.df %>% group_by(x.1)%>% top_n(n = 5, wt= freq)

##Remove one time used hashtags
UHFreqFiltered.df <- subset(UHF.df, freq!="1")
UHTop1FreqFiltered.df <- as.data.frame(UHFreqFiltered.df %>% group_by(x.1) %>% top_n(n=1))
UHTop2FreqFiltered.df <- UHFreqFiltered.df %>% group_by(x.1)%>% top_n(n = 2, wt= freq)

#Remove Hashtag-Frequencies of not selected hashtags
#Only Selected Hashtag Frequencies are relevant
UHFreqFilHashtags.df <- subset(UHF.df, 
                                 x.2=="#hayir"|
                                 x.2== "#turkeyreferendum"|
                                 x.2=="#referendum"|
                                 x.2=="#Turkey"|
                                 x.2=="#turkeyschoice"|
                                 x.2=="#evet"|
                                 x.2=="#turkish")

UHTop7FreqFilHashtags.df <- as.data.frame(UHFreqFilHashtags.df %>% group_by(x.1) %>% top_n(n=7))
#Remove X Column (created by Write out and Read in as csv-File)
UHTop5FreqFilHashtags.df <- subset(UHTop7FreqFilHashtags.df, select = c(x.1,x.2,freq))
SelectedHashtagFreqperUser <- dcast(UHTop7FreqFilHashtags.df, x.1 ~ x.2, value.var="freq")


###END