# Load Libraries
library(stringr)
library(igraph)
library(streamR)
library(plyr)
library(dplyr)
library(reshape2)
detach()

##Filter for Maximum Hashtag-Frequency
##Read CSV File into UserHashtagFrequency Dataframe (UHF)
#UHF <- read.csv2(file = "/users/flori/fimecho/Data/UHFreq_TurkeyAll.csv", header = TRUE, stringsAsFactors = FALSE)
#tweets.hashtags <- read.csv2(file = "/users/flori/fimecho/Data/Hashtags_TurkeyAll.csv", header = TRUE, stringsAsFactors = FALSE)



##Identification of unique users
Users.df <- UHF %>% distinct(User)


##Select Maximum 1 used hashtags per user
##Returns Dataframe with most frequently used hashtag per user (Multiple Maximum HashtagsFrequency possible)
UHTop1Freq.df <- UHF %>% group_by(User)%>% top_n(n = 1, wt= n)


##Remove one time used hashtags
UHFreqFiltered.df <- subset(UHF, n!="1")
UHTop1FreqFiltered.df <- UHFreqFiltered.df %>% group_by(User) %>% top_n(n = 1, wt= freq)

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
# Convert character vector to list of symbols
HashtagsFilteredFrequency<-UHFreqFilHashtags.df %>%
  group_by_(.dots="Hashtag") %>%
  summarise(n = n())


#Remove X Column (created by Write out and Read in as csv-File)
#not Necessary with Use of .RData File
#UHFreqFilHashtags.df <- subset(UHFreqFilHashtags.df, select = c(User, Hashtag,n))
SelectedHashtagFreqperUser <- dcast(UHFreqFilHashtags.df, User ~ Hashtag, value.var="n")


SelectedHashtagFreqperUser[,2:8][is.na(SelectedHashtagFreqperUser[,2:8])] <- 0
SelectedHashtagFreqperUser$Sum<- rowSums(SelectedHashtagFreqperUser[,2:8])


colnames(SelectedHashtagFreqperUser)<-c("User","EVET","HAYIR","REFERENDUM","TURKEY","TURKEYREFERENDUM","TURKEYSCHOICE","TURKISH","Sum")

RelFreq.df<-mutate(SelectedHashtagFreqperUser[,1:9], EVET=EVET/Sum, HAYIR=HAYIR/Sum, REFERENDUM=REFERENDUM/Sum, TURKEY=TURKEY/Sum, TURKEYREFERENDUM=TURKEYREFERENDUM/Sum, TURKEYSCHOICE=TURKEYSCHOICE/Sum, TURKISH=TURKISH/Sum)
#Max relative Frequency
RelFreq.df$max<- apply(RelFreq.df[,2:8], 1, max)

RelFreq.df$Hashtag<-colnames(RelFreq.df[,2:8])[max.col(RelFreq.df[,2:8],ties.method="first")]

##Additional Constraints: If Users most freqently used hashtag <=50% relative frequency -->uses more than one hashtag
for(i in 1:nrow(RelFreq.df)){
  if(RelFreq.df[i,11]<= 0.5){
    RelFreq.df[i,10]<-NA
  }
}
##If users has used < 2 hashtags in total Hashtag also removed
for(i in 1:nrow(RelFreq.df)){
  if(RelFreq.df[i,9]<2){
    RelFreq.df[i,10]<-NA
  }
}

save(RelFreq.df, file = "/users/flori/fimecho/UserHashtagRelativeFrequency.RData")
#####All HASHTAGs Analysis Start

#Necessary if not Done already
#Hashtags.df <- mutate_each(Hashtags.df, funs(toupper))
Hashtags.df<-as.data.frame(table(Hashtags.df))


Hashtags.df<-Hashtags.df[Hashtags.df$Freq>100,]

hist(Hashtags.df$Freq)


plot(Hashtags.df)
