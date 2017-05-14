# Load Libraries
library(stringr)
library(igraph)
library(streamR)
library(plyr)
library(dplyr)
library(reshape2)

##Filter for Maximum Hashtag-Frequency
##Read CSV File into UserHashtagFrequency Dataframe (UHF)
#UHF <- read.csv2(file = "/users/flori/fimecho/Data/UHFreq_TurkeyAll.csv", header = TRUE, stringsAsFactors = FALSE)
#tweets.hashtags <- read.csv2(file = "/users/flori/fimecho/Data/Hashtags_TurkeyAll.csv", header = TRUE, stringsAsFactors = FALSE)



##Identification of unique users
Users.df <- UHF %>% distinct(User)


##Select Maximum 1 used hashtags per user
##Returns Dataframe with most frequently used hashtag per user (Multiple Maximum HashtagsFrequency possible)
UHTop1Freq.df <- UHF %>% group_by(User)%>% top_n(n = 1, wt= n)


##OPTIONAL:Remove one time used hashtags
#UHFreqFiltered.df <- subset(UHF, n!="1")
#UHTop1FreqFiltered.df <- UHFreqFiltered.df %>% group_by(User) %>% top_n(n = 1, wt= freq)

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
#Frequency of Use of Selected Hashtags 
HashtagsFilteredFrequency <- aggregate(UHFreqFilHashtags.df$n~UHFreqFilHashtags.df$Hashtag, UHFreqFilHashtags.df, sum)

#UNTEN-FALSCH n()->Counts number of observations not aggregates Number in Column n
#Frequency of Use of Selected Hashtags 
#HashtagsFilteredFrequency<-UHFreqFilHashtags.df%>%  group_by_(.dots="Hashtag") %>%  summarise(n = n())

#barplot of hashtagusage
barplot(HashtagsFilteredFrequency$`UHFreqFilHashtags.df$n`, names.arg = HashtagsFilteredFrequency$`UHFreqFilHashtags.df$Hashtag`)

#Remove X Column (created by Write out and Read in as csv-File)
#not Necessary with Use of .RData File
#UHFreqFilHashtags.df <- subset(UHFreqFilHashtags.df, select = c(User, Hashtag,n))

#Transpose Matrix,add one column for each hashtag, count frequency
SelectedHashtagFreqperUser <- dcast(UHFreqFilHashtags.df, User ~ Hashtag, value.var="n")

#remove NAs in frequency for later calculation
SelectedHashtagFreqperUser[,2:8][is.na(SelectedHashtagFreqperUser[,2:8])] <- 0
#Calculate RowSum for Calulcation rel. Frequency
SelectedHashtagFreqperUser$Sum<- rowSums(SelectedHashtagFreqperUser[,2:8])

#Remove Hashtags from Columnames
colnames(SelectedHashtagFreqperUser)<-c("User","EVET","HAYIR","REFERENDUM","TURKEY","TURKEYREFERENDUM","TURKEYSCHOICE","TURKISH","Sum")

#Calculate relative Frequency
RelFreq.df<-mutate(SelectedHashtagFreqperUser[,1:9], EVET=EVET/Sum, HAYIR=HAYIR/Sum, REFERENDUM=REFERENDUM/Sum, TURKEY=TURKEY/Sum, TURKEYREFERENDUM=TURKEYREFERENDUM/Sum, TURKEYSCHOICE=TURKEYSCHOICE/Sum, TURKISH=TURKISH/Sum)
#write out Max relative Frequency into new column
RelFreq.df$max<- apply(RelFreq.df[,2:8], 1, max)
#for each rwo if max column-> write out column name in new Column Hashtag
RelFreq.df$Hashtag<-colnames(RelFreq.df[,2:8])[max.col(RelFreq.df[,2:8],ties.method="first")]

##Additional Constraints: If Users most freqently used hashtag <=50% relative frequency -->uses more than one hashtag
for(i in 1:nrow(RelFreq.df)){
  if(RelFreq.df[i,"max"]<= 0.5){
    RelFreq.df[i,"Hashtag"]<-NA
  }
}
##If users has used < 2 hashtags in total Hashtag also removed
for(i in 1:nrow(RelFreq.df)){
  if(RelFreq.df[i,"Sum"]<2){
    RelFreq.df[i,"Hashtag"]<-NA
  }
}
save(RelFreq.df, file = "/users/flori/fimecho/Data/Filtered Data/UserHashtagRelativeFrequency.RData")


RelFreqMerge.df<-subset(RelFreq.df[, c("User", "Hashtag")])
vertices.df<- merge(x=vertices.df, y=RelFreqMerge.df, by.x="usr_Id", by.y = "User", all.x=TRUE)
save(vertices.df, file = "/users/flori/fimecho/Data/Filtered Data/Vertices.RData")

#####All HASHTAGs Analysis Start###########################################################################################

#Necessary if not Done already
#Hashtags.df <- mutate_each(Hashtags.df, funs(toupper))
Hashtags.df<-as.data.frame(table(Hashtags.df))


Hashtags.df<-Hashtags.df[Hashtags.df$Freq>100,]

hist(Hashtags.df$Freq)
par(mai=c(0.5,1.5,0,0.1))
barplot(Hashtags.df$Freq, names.arg = Hashtags.df$Hashtags.df, horiz=TRUE, las=1, cex.names=0.4)

plot(Hashtags.df)
