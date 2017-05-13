# Load Libraries
library(stringr)
library(igraph)
library(streamR)
library(plyr)
library(dplyr)
library(reshape2)
path<-"/users/flori/fimecho/Data/"
file <- paste(path,"Turkey/Turkeyall.csv",sep="")
filename.out<-paste(path,"User_HashtagTurkeyAll.csv",sep="")
nodelist.filename <- paste(path,"KantenTurkey.csv",sep="")

##Filter for Maximum Hashtag-Frequency
##Read CSV File into UserHashtagFrequency Dataframe (UHF)
UHF <- read.csv2(file = "/users/flori/fimecho/Data/UHFreq_TurkeyAll.csv", header = TRUE, stringsAsFactors = FALSE)

tweets.hashtags <- read.csv2(file = "/users/flori/fimecho/Data/Hashtags_TurkeyAll.csv", header = TRUE, stringsAsFactors = FALSE)

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
UHFreqFilHashtags.df <- subset(UHF, 
                               Hashtag=="#HAYIR"|
                                 Hashtag== "#TURKEYREFERENDUM"|
                                 Hashtag=="#REFERENDUM"|
                                 Hashtag=="#TURKEY"|
                                 Hashtag=="#TURKEYSCHOICE"|
                                 Hashtag=="#EVET"|
                                 Hashtag=="#TURKISH")


#Remove X Column (created by Write out and Read in as csv-File)
UHFreqFilHashtags.df <- subset(UHFreqFilHashtags.df, select = c(User, Hashtag,n))
SelectedHashtagFreqperUser <- dcast(UHFreqFilHashtags.df, User ~ Hashtag, value.var="n")
Hashtags.df <- toupper(Hashtags.df)


#####All HASHTAGs Analysis Start

##TODO Read In All Hashtags

Hashtags.df <- as.data.frame(unlist(tweets.hashtags))
#Necessary if not Done already
#Hashtags.df <- mutate_each(Hashtags.df, funs(toupper))
Hashtags.df<-as.data.frame(table(Hashtags.df))


Hashtags.df<-Hashtags.df[Hashtags.df$Freq>100,]

hist(Hashtags.df$Freq)


plot(Hashtags.df)
