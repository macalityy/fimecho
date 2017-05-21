# Load Libraries
library(stringr)
library(igraph)
library(streamR)
library(plyr)
library(dplyr)
library(reshape2)
workingDT<-getwd()

load(paste(c(workingDT, "/Data/Filtered Data/VerticesComm.RData"), collapse = ""))
load(paste(c(workingDT, "/Data/Filtered Data/UserHashtagFrequency2.RData"), collapse = ""))
load(paste(c(workingDT, "/Data/Filtered Data/UserHashtagFrequency.RData"), collapse = ""))
load(paste(c(workingDT, "/Data/Filtered Data/Hashtags2.RData"), collapse = ""))
#load("~/fimecho/Data/Seminar/Vertices.RData")


##Filter for Maximum Hashtag-Frequency
##Read CSV File into UserHashtagFrequency Dataframe (UHF)
#UHF <- read.csv2(file = "/users/flori/fimecho/Data/UHFreq_TurkeyAll.csv", header = TRUE, stringsAsFactors = FALSE)
#tweets.hashtags <- read.csv2(file = "/users/flori/fimecho/Data/Hashtags_TurkeyAll.csv", header = TRUE, stringsAsFactors = FALSE)

##Identification of unique users
Users.df <- UHF2 %>% distinct(User)


##Abolute maximum used hashtag per user
##Select Maximum 1 used hashtags per user
##Returns Dataframe with most frequently used hashtag per user (Multiple Maximum HashtagsFrequency possible)
#UHTop1Freq.df <- UHF %>% group_by(User)%>% top_n(n = 1, wt= n)

##OPTIONAL:Remove one time used hashtags
#UHFreqFiltered.df <- subset(UHF, n!="1")
#UHTop1FreqFiltered.df <- UHFreqFiltered.df %>% group_by(User) %>% top_n(n = 1, wt= freq)

#Remove Hashtag-Frequencies of not selected hashtags
#Only Selected Hashtag Frequencies are relevant
UHFreqFilHashtags2.df <- subset(UHF2, 
                               Hashtag=="#HAYIR"|
                                 Hashtag== "#TURKEYREFERENDUM"|
                                 Hashtag=="#REFERENDUM"|
                                 Hashtag=="#TURKEY"|
                                 Hashtag=="#TURKEYSCHOICE"|
                                 Hashtag=="#EVET")
UHFreqFilHashtags.df <- subset(UHF, 
                               Hashtag=="#HAYIR"|
                                 Hashtag== "#TURKEYREFERENDUM"|
                                 Hashtag=="#REFERENDUM"|
                                 Hashtag=="#TURKEY"|
                                 Hashtag=="#TURKEYSCHOICE"|
                                 Hashtag=="#EVET")
#barplot of hashtag usage
HashtagsFilteredFrequency <- aggregate(UHFreqFilHashtags.df$n~UHFreqFilHashtags.df$Hashtag, UHFreqFilHashtags.df, sum)
HashtagsFilteredFrequency2 <- aggregate(UHFreqFilHashtags2.df$n~UHFreqFilHashtags2.df$Hashtag, UHFreqFilHashtags2.df, sum)
HashtagsFilteredFrequency$After<-HashtagsFilteredFrequency2[,2]
rownames(HashtagsFilteredFrequency)<- HashtagsFilteredFrequency$`UHFreqFilHashtags.df$Hashtag`
colnames(HashtagsFilteredFrequency)<-c("Hashtag", "Before", "After")
HashtagsFilteredFrequency<- HashtagsFilteredFrequency[,-1]
par(mar=c(10,8,1,1))
barplot(t(HashtagsFilteredFrequency), beside=T,  
        cex.names=1, las=2, col=c("darkblue","red"), 
        legend = rownames(t(HashtagsFilteredFrequency)), 
        ylab="Total Frequence of Hashtag", 
        mgp=c(4,0.5,0))
box(bty="l")
 
#UNTEN-FALSCH n()->Counts number of observations not aggregates Number in Column n
#Frequency of Use of Selected Hashtags 
#HashtagsFilteredFrequency<-UHFreqFilHashtags.df%>%  group_by_(.dots="Hashtag") %>%  summarise(n = n())

#Transpose Matrix,add one column for each hashtag, count frequency
SelectedHashtagFreqperUser <- dcast(UHFreqFilHashtags2.df, User ~ Hashtag, value.var="n")

#remove NAs in frequency for later calculation
SelectedHashtagFreqperUser[,2:7][is.na(SelectedHashtagFreqperUser[,2:7])] <- 0
#Calculate RowSum for Calulcation rel. Frequency
SelectedHashtagFreqperUser$Sum<- rowSums(SelectedHashtagFreqperUser[,2:7])

#Remove Hashtags from Columnames
colnames(SelectedHashtagFreqperUser)<-c("User","EVET","HAYIR","REFERENDUM","TURKEY","TURKEYREFERENDUM","TURKEYSCHOICE","Sum")

#Calculate relative Frequency
RelFreq.df<-mutate(SelectedHashtagFreqperUser[,1:8], EVET=EVET/Sum, HAYIR=HAYIR/Sum, REFERENDUM=REFERENDUM/Sum, TURKEY=TURKEY/Sum, TURKEYREFERENDUM=TURKEYREFERENDUM/Sum, TURKEYSCHOICE=TURKEYSCHOICE/Sum)
#write out Max relative Frequency into new column
RelFreq.df$max<- apply(RelFreq.df[,2:7], 1, max)
#for each rwo if max column-> write out column name in new Column Hashtag
RelFreq.df$MaxUsedHashtag<-colnames(RelFreq.df[,2:7])[max.col(RelFreq.df[,2:7],ties.method="first")]

##Additional Constraints: If Users most freqently used hashtag <=50% relative frequency -->uses more than one hashtag
for(i in 1:nrow(RelFreq.df)){
  if(RelFreq.df[i,"max"]<= 0.5){
    RelFreq.df[i,"MaxUsedHashtag"]<-NA
  }
}
##If users has used < 2 hashtags in total Hashtag also removed
for(i in 1:nrow(RelFreq.df)){
  if(RelFreq.df[i,"Sum"]<2){
    RelFreq.df[i,"MaxUsedHashtag"]<-NA
  }
}
colnames(SelectedHashtagFreqperUser)<-c("User","ABS_EVET","ABS_HAYIR","ABS_REFERENDUM","ABS_TURKEY","ABS_TURKEYREFERENDUM",
                                        "ABS_TURKEYSCHOICE", "Sum")
UserHashtagFrequency.df<-cbind(RelFreq.df, SelectedHashtagFreqperUser[,2:7])
save(UserHashtagFrequency.df, file = "/users/flori/fimecho/Data/Filtered Data/UserHashtagRelativeFrequency2.RData")

#Merge UserHashtagFrequency.df to Vertices.df Left Join
UserHashtagFrequency.df<-subset(UserHashtagFrequency.df[, c("User", "MaxUsedHashtag","ABS_EVET","ABS_HAYIR",
                                                                 "ABS_REFERENDUM","ABS_TURKEY",
                                                                 "ABS_TURKEYREFERENDUM","ABS_TURKEYSCHOICE")])

vertices.df<- merge(x=vertices.df, y=UserHashtagFrequency.df, by.x="Id", by.y = "User", all.x=TRUE)
save(vertices.df, file = paste(c(workingDT, "/Data/Filtered Data/VerticesCommWHashtags.RData"), collapse = ""))


#####All HASHTAGs Analysis Start###########################################################################################

#Necessary if not Done already
#Hashtags.df <- mutate_each(Hashtags.df, funs(toupper))
Hashtags2.df<-as.data.frame(table(Hashtags2.df))
##Extract list of unique hashtags
HashtagsUnique<-Hashtags2.df[,"Hashtags2.df"]
write.csv2(HashtagsUnique, file=paste(c(workingDT, "/Data/Filtered Data/Hashtags_Unique2.csv"), collapse = ""))
Hashtags2.df<-Hashtags2.df[Hashtags2.df$Freq>100,]

par(mai=c(0.5,1.5,0,0.1))
barplot(Hashtags2.df$Freq, names.arg = Hashtags2.df$Hashtags.df, horiz=TRUE, las=1, cex.names=0.4)


#####Hashtags in Communities
vertices.df=subset(vertices.df[, c("Id", "ml_comm", "MaxUsedHashtag")])
UserFrequency<-as.data.frame(table(vertices.df$ml_comm))
UserFrequency<-UserFrequency[ order(-UserFrequency$Freq),]
head(UserFrequency)
UserFrequency<-UserFrequency[UserFrequency$Freq>=1000,]
vertices.dfFiltered<- subset(vertices.df, (vertices.df$ml_comm %in% UserFrequency$Var1))
table(vertices.dfFiltered$ml_comm)

HashtagsPerCommunity<-as.data.frame(table(vertices.dfFiltered$ml_comm, vertices.dfFiltered$MaxUsedHashtag))
colnames(HashtagsPerCommunity)<-c("Community", "Hashtag", "Freq")
HashtagsPerCommunity <- dcast(HashtagsPerCommunity, Community ~ Hashtag, value.var="Freq")

rownames(HashtagsPerCommunity)<- HashtagsPerCommunity$Community
HashtagsPerCommunity<-HashtagsPerCommunity[,-1]

HashtagsPerCommunity$Sum<-rowSums(HashtagsPerCommunity)
HashtagsPerCommunity2<-HashtagsPerCommunity/HashtagsPerCommunity$Sum
HashtagsPerCommunity<- HashtagsPerCommunity[,-7]
HashtagsPerCommunity2<- HashtagsPerCommunity2[,-7]

par(mar=c(3, 3, 2, 10))
barplot(t(HashtagsPerCommunity), beside=T,  
        cex.names=1, las=2, col = c("blue", "green", "red", "darkorange", "gold", "cyan"),
        ylab="Total Frequence of Hashtag",
        legend = rownames(t(HashtagsPerCommunity)),
        mgp=c(4,0.5,0),
        args.legend = list(x = "topright", bty = "n", inset=c(-0.3, 0)))
box(bty="l")

par(mar=c(3, 3, 2, 10))
barplot(t(HashtagsPerCommunity2), beside=T,  
        cex.names=1, las=2, col = c("blue", "green", "red", "darkorange", "gold", "cyan"),
        ylab="Total Frequence of Hashtag",
        legend = rownames(t(HashtagsPerCommunity2)),
        mgp=c(4,0.5,0),
        args.legend = list(x = "topright", bty = "n", inset=c(-0.3, 0)))
box(bty="l")


