# Load Libraries
library(stringr)
library(igraph)
library(streamR)
library(plyr)
library(dplyr)
path<-"/users/flori/fimecho/Data/"
file <- paste(path,"Turkey/Turkeyall.csv",sep="")
filename.out<-paste(path,"User_HashtagTurkeyAll.csv",sep="")
nodelist.filename <- paste(path,"KantenTurkey.csv",sep="")




#read csv file to get tweets into dataframe
tweets.df <- read.csv2(file = file, header = TRUE, stringsAsFactors = FALSE)

#shrink dataframe to columns we might use later
tweets.df <- subset(tweets.df[ ,c("X.1", "X", "text", "screen_name", "user_id_str")])

###Optional
#Check if #UserIDs==#UserScreenNames
    #numUsersSN<-tweets.df %>% distinct(screen_name)
    #numUsersID<-tweets.df %>% distinct(user_id_str)


##OPTIONAL
#####Remove Tweets from Users not in Filtered Network (From RetweetGraph.R Filter Value)

      #read nodelist
      nodelist.df <- read.csv2(file = nodelist.filename, header = TRUE, stringsAsFactors = FALSE)
      #unique User-tweeted list
      nodelist2.df <- nodelist.df %>% distinct(user.from.name)
      
      
      tweetsFiltered.df <- merge(x = nodelist2.df, y = tweets.df,  by.x = "user.from.name", by.y = "screen_name", all.x = TRUE)



# from the data frame, we only need the column text to find out which rows are retweets
#tweets.text <- tweetsFiltered.df[,"text"]
tweets.text <- tweets.df[,"text"]


# Extract all the Hashtags per Tweet
tweets.hashtags  <- str_extract_all(tweets.text,"[#]{1}(\\w+)")
#Remove empty vectors(entries) in hashtag list
tweets.hashtags<-tweets.hashtags[lapply(tweets.hashtags,length)>0]
# Extract tweet ids which include a hashtag
tweets.hashtags.ids <- grep("[#]{1}(\\w+)",tweets.text)
# Extract usernames from tweet dataframe
#user.originaltw<-tweetsFiltered.df["user.from.name"]
user.originaltw<-tweets.df["screen_name"]

head(tweets.hashtags)
        
        SumHashtags<-0
        for (i in 1:length(tweets.hashtags))
        {
          SumHashtags<-SumHashtags+length(tweets.hashtags[[i]])
        }
        UserHashtagTable <- as.table(matrix("", ncol = 2, nrow = SumHashtags))  
        
        colnames(UserHashtagTable) <- c("User", "Hashtag")
        head(UserHashtagTable)
        
        rownu<-0
        
        for (i in 1:length(tweets.hashtags))
        {
          for (j in 1:length(tweets.hashtags[[i]]))
          {
            rownu<-rownu+1
            UserHashtagTable[rownu,1]<-user.originaltw[[tweets.hashtags.ids[i],1]]
            UserHashtagTable[rownu,2]<-tweets.hashtags[[i]][j]
          }
        }
        head(UserHashtagTable)



UserHashtagTableAsDF<-as.data.frame.matrix(UserHashtagTable)

UserHashtagTableAsDF[,2] = toupper(UserHashtagTableAsDF[,2])
#if:Error in n() : This function should not be called directly
#detach()
library(dplyr)
grp_cols <- c("User","Hashtag")

# Convert character vector to list of symbols
dots <- lapply(grp_cols, as.symbol)
UHF<-UserHashtagTableAsDF %>%
  group_by_(.dots=dots) %>%
  summarise(n = n())

write.csv2(UHF, file = "/users/flori/fimecho/Data/UHFreq_TurkeyAll.csv")
write.csv2(tweets.hashtags, file = "/users/flori/fimecho/Data/Hashtags_TurkeyAll.csv")

