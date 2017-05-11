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

#if:Error in n() : This function should not be called directly
#detach()
library(dplyr)
grp_cols <- c("User","Hashtag")

# Convert character vector to list of symbols
dots <- lapply(grp_cols, as.symbol)
UHF<-UserHashtagTableAsDF %>%
  group_by_(.dots=dots) %>%
  summarise(n = n())

write.csv2(UHF, file = filename.out)

##Filter for Maximum Hashtag-Frequency
##Read CSV File into UserHashtagFrequency Dataframe (UHF)
UHF <- read.csv2(file = filename.out, header = TRUE, stringsAsFactors = FALSE)

##Identification of unique users
Users.df <- UHF %>% distinct(User)

##Select Maximum 1 used hashtags per user
##Returns Dataframe with most frequently used hashtag per user (Multiple Maximum HashtagsFrequency possible)
UHTop1Freq.df <- UHF %>% group_by(User)%>% top_n(n = 1, wt= n)


UHTop2Freq.df <- UHF %>% group_by(User)%>% top_n(n = 2, wt= n)
UHTop3Freq.df <- UHF %>% group_by(User)%>% top_n(n = 3, wt= n)
UHTop4Freq.df <- UHF %>% group_by(User)%>% top_n(n = 4, wt= n)
UHTop5Freq.df <- UHF %>% group_by(User)%>% top_n(n = 5, wt= n)

##Remove one time used hashtags
UHFreqFiltered.df <- subset(UHF, n!="1")
UHTop1FreqFiltered.df <- UHFreqFiltered.df %>% group_by(User) %>% top_n(n = 1, wt= n)
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



Hashtags.df <- as.data.frame(unlist(tweets.hashtags))
Hashtags.df <- mutate_each(Hashtags.df, funs(toupper))
Hashtags.df<-as.data.frame(table(Hashtags.df))


Hashtags.df<-Hashtags.df[Hashtags.df$Freq>100,]
table(Hashtags.df)
Hashtags.df$Var1 <- 1:16873
hist(Hashtags.df$Var1, Hashtags.df$Freq, breaks = 16873)
plot(Hashtags.df$x, Hashtags.df$Freq)
