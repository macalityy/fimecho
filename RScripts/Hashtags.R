# Load Libraries
library(stringr)
library(igraph)
library(streamR)
library(plyr)
library(dplyr)
workingDT<-getwd()
file <- paste(workingDT,"/Data/Turkey/Turkeyall.csv",sep="")
filename.out<-paste(workingDT,"/Data/User_HashtagTurkeyAll.csv",sep="")
nodelist.filename <- paste(workingDT,"/Data/KantenTurkey.csv",sep="")




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
#Tweets.df contains already only filtered user_id_str
      #read nodelist
      #nodelist.df <- read.csv2(file = nodelist.filename, header = TRUE, stringsAsFactors = FALSE)
      #unique User-tweeted list
      #nodelist2.df <- nodelist.df %>% distinct(user.from.name)
      #tweetsFiltered.df <- merge(x = nodelist2.df, y = tweets.df,  by.x = "user.from.name", by.y = "screen_name", all.x = TRUE)



# from the data frame, we only need the column text to find out which rows are retweets
#tweets.text <- tweetsFiltered.df[,"text"]
tweets.text <- tweets.df[,"text"]


# Extract all the Hashtags per Tweet
tweets.hashtags <- str_extract_all(tweets.text,"[#]{1}(\\w+)")
#Remove empty vectors(entries) in hashtag list
tweets.hashtags <- tweets.hashtags[lapply(tweets.hashtags,length)>0]
# Extract tweet ids which include a hashtag
tweets.hashtags.ids <- grep("[#]{1}(\\w+)",tweets.text)
# Extract UserIDs from tweet dataframe
#user.originaltw<-tweetsFiltered.df["user.from.name"]
user.originaltw<-tweets.df["user_id_str"]
head(tweets.hashtags)

#Calculate Sum of all used Hashtags, required for the dimension of table creation
SumHashtags<-0
for (i in 1:length(tweets.hashtags))
{
  SumHashtags<-SumHashtags+length(tweets.hashtags[[i]])
}


#Create Table
UserHashtagTable <- as.table(matrix("", ncol = 2, nrow = SumHashtags))  
colnames(UserHashtagTable) <- c("User", "Hashtag")
head(UserHashtagTable)

#fill table, with User_id_str and used hashtag
#Result: One Row for each hashtag, assigned to the user
rownu<-0
for (i in 1:length(tweets.hashtags))
{
  for (j in 1:length(tweets.hashtags[[i]]))
  {
    rownu<-rownu+1
    UserHashtagTable[rownu,1]<-as.character(user.originaltw[tweets.hashtags.ids[i],1])
    UserHashtagTable[rownu,2]<-tweets.hashtags[[i]][j]
  }
}
head(UserHashtagTable)
#convert Table to DataFrame
UserHashtagTableAsDF<-as.data.frame.matrix(UserHashtagTable)

#Uppercase of all hashtags
UserHashtagTableAsDF[,"Hashtag"] = toupper(UserHashtagTableAsDF[,"Hashtag"])
UserHashtagTableAsDF2<-UserHashtagTableAsDF
##RegEx Matches,encoding based problems
#HAY%-->#HAYIR
UserHashtagTableAsDF2[,"Hashtag"] <- gsub(UserHashtagTableAsDF2[,"Hashtag"],pattern = "((#HAY).*)", replacement = "#HAYIR")
#EVE%-->#EVET
UserHashtagTableAsDF2[,"Hashtag"] <- gsub(UserHashtagTableAsDF2[,"Hashtag"],pattern = "((#EVE).*)", replacement = "#EVET")
#TURKISHREF%, #TUERKEIREF%, #TURKEYREF% -->#TURKEYREFERENDUM
UserHashtagTableAsDF2[,"Hashtag"] <- gsub(UserHashtagTableAsDF2[,"Hashtag"],pattern = "((#TURKISHREF).*)|((#TUERKEIREF).*)|((#TURKEYREF).*)", replacement = "#TURKEYREFERENDUM")
#TURKIYE, #TÜRKEI, #TUERKEI, #TURQUIE, #TÜRKIYE -->#TURKEY
UserHashtagTableAsDF2[,"Hashtag"] <- gsub(UserHashtagTableAsDF2[,"Hashtag"],pattern = "((#TURKIYE))|((#TÜRKEI))|((#TUERKEI))|((#TURQUIE))|((#TÜRKIYE)) ", replacement = "#TURKEY")


#REFR%, #REFER% -->#REFERENDUM
UserHashtagTableAsDF2[,"Hashtag"] <- gsub(UserHashtagTableAsDF2[,"Hashtag"],pattern ="((#REFR).*)|((#REFER).*)", replacement = "#REFERENDUM")
#TURKEYSC%-->#TURKEYSCHOICE
UserHashtagTableAsDF2[,"Hashtag"] <- gsub(UserHashtagTableAsDF2[,"Hashtag"],pattern = "((#TURKEYSC).*)", replacement = "#TURKEYSCHOICE")

#Group by 2 columns (User, Hashtag) and count lines(=Frequency)
#if:Error in n() : This function should not be called directly
#detach()
#library(dplyr)
grp_cols <- c("User","Hashtag")
# Convert character vector to list of symbols
dots <- lapply(grp_cols, as.symbol)
UHF<-UserHashtagTableAsDF %>%  group_by_(.dots=dots) %>%  summarise(n = n())
UHF2<-UserHashtagTableAsDF2 %>%  group_by_(.dots=dots) %>%  summarise(n = n())

#Save Used Hashtags to Data Frame
Hashtags.df<-as.data.frame(toupper(unlist(tweets.hashtags)))
colnames(Hashtags.df)<-"Hashtag"
Hashtags2.df<-Hashtags.df
#HAY%-->#HAYIR
Hashtags2.df[,"Hashtag"] <- gsub(Hashtags2.df[,"Hashtag"],pattern = "((#HAY).*)", replacement = "#HAYIR")
#EVE%-->#EVET
Hashtags2.df[,"Hashtag"] <- gsub(Hashtags2.df[,"Hashtag"],pattern = "((#EVE).*)", replacement = "#EVET")
#TURKISHREF%, #TUERKEIREF%, #TURKEYREF% -->#TURKEYREFERENDUM
Hashtags2.df[,"Hashtag"] <- gsub(Hashtags2.df[,"Hashtag"],pattern = "((#TURKISHREF).*)|((#TUERKEIREF).*)|((#TURKEYREF).*)", replacement = "#TURKEYREFERENDUM")
#TURKIYE, #TÜRKEI, #TUERKEI, #TURQUIE, #TÜRKIYE -->#TURKEY
Hashtags2.df[,"Hashtag"] <- gsub(Hashtags2.df[,"Hashtag"],pattern = "((#TURKIYE))|((#TÜRKEI))|((#TUERKEI))|((#TURQUIE))|((#TÜRKIYE)) ", replacement = "#TURKEY")
#REFR%, #REFER% -->#REFERENDUM
Hashtags2.df[,"Hashtag"] <- gsub(Hashtags2.df[,"Hashtag"],pattern ="((#REFR).*)|((#REFER).*)", replacement = "#REFERENDUM")
#TURKEYSC%-->#TURKEYSCHOICE
Hashtags2.df[,"Hashtag"] <- gsub(Hashtags2.df[,"Hashtag"],pattern = "((#TURKEYSC).*)", replacement = "#TURKEYSCHOICE")
#Save as .RData
save(UHF, file = paste(c(workingDT, "/Data/Filtered Data/UserHashtagFrequency.RData"), collapse = ""))
save(UHF2, file = paste(c(workingDT, "/Data/Filtered Data/UserHashtagFrequency2.RData"), collapse = ""))
save(Hashtags.df, file = paste(c(workingDT, "/Data/Filtered Data/Hashtags.RData"), collapse = ""))
save(Hashtags2.df, file = paste(c(workingDT, "/Data/Filtered Data/Hashtags2.RData"), collapse = ""))
