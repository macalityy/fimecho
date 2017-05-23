# Load Libraries
library(stringr)
library(plyr)
library(dplyr)
library(ggplot2)
library(scales)

workingDT<-getwd()
file <- paste(workingDT,"/Data/Turkey/Turkeyall.csv",sep="")
filename.out<-paste(workingDT,"/Data/User_HashtagTurkeyAll.csv",sep="")
nodelist.filename <- paste(workingDT,"/Data/KantenTurkey.csv",sep="")




#read csv file to get tweets into dataframe
#tweets.df <- read.csv2(file = file, header = TRUE, stringsAsFactors = FALSE)

#shrink dataframe to columns we might use later
tweets.df <- subset(tweets.df[ ,c("id_str", "created_at")])

Sys.getlocale()
Sys.setlocale("LC_TIME", "English")

tweets.df$DateHourMin<-format(strptime(tweets.df$created_at, "%a %b %d %H:%M:%S %z %Y", tz="GMT"),'%Y-%m-%d %H %M')
tweets.df$DateHour<-format(strptime(tweets.df$created_at, "%a %b %d %H:%M:%S %z %Y", tz="GMT"),'%Y-%m-%d %H')
tweets.df$created_at<-as.POSIXct(tweets.df$created_at, format = "%a %b %d %H:%M:%S %z %Y", tz = "GMT")
tweets.df$Date<-format(strptime(tweets.df$created_at, "%a %b %d %H:%M:%S %z %Y", tz="GMT"),'%Y-%m-%d')
tweets.df$Hour<-format(strptime(tweets.df$created_at, "%a %b %d %H:%M:%S %z %Y", tz="GMT"),'%H')


DateTimeFreq.df<- as.data.frame(table(tweets.df$DateHour))
DateTimeFreq.df$Hours<-format(strptime(DateTimeFreq.df$Var1, "%Y-%m-%d %H", tz="GMT"),'%H')


for(i in 1:nrow(DateTimeFreq.df)){
  DateTimeFreq.df$Absolut[i]=sum(DateTimeFreq.df[1:i,2])
}
for(i in 1:nrow(DateTimeFreq.df)){
  DateTimeFreq.df$Percent[i]=DateTimeFreq.df$Absolut[i]/sum(DateTimeFreq.df$Freq)
}
library(date)
hist(as.POSIXct(strptime(tweets.df$DateHourMin, "%Y-%m-%d %H %M", tz="GMT")), breaks=10000,main="",format="%H %M")
#axis.Date(1,as.POSIXct(strptime(tweets.df$DateHourMin)))
#axis.Date(1, at=seq(as.Date("2017-04-14"), as.Date("2017-04-18"), by="days"), format="%d %H %M")

par(mar = rep(4, 4))
barData<-DateTimeFreq.df$Freq
y <- lineData<-DateTimeFreq.df$Percent
x <- barplot(barData, 
             axes = TRUE,
             col = "blue", 
             xlab = "",
             ylab = "")[, 1]
axis(1, at = x, labels = DateTimeFreq.df$Hours)
par(new = TRUE)
plot(x = x, y = y, type = "l", col = "red", axes = FALSE, xlab = "", ylab = "")
axis(4, at = c(pretty(lineData), max(lineData)), las = 2) 
box()    




###SUBSET of tweets.df do find user frequency
# now either they are tweeted on 16.04.2017 before 14:58:47
# or are not from 16.04.2017
tweetsAfter.df<-tweets.df[which(as.POSIXct(tweets.df$created_at, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")>=as.POSIXct("2017-04-16 14:58:47", tz = "GMT")), ]
tweetsBefore.df<-subset(tweets.df, !(tweets.df$id_str %in% tweetsAfter.df$id_str)) 

save(tweetsAfter.df, file = paste(c(workingDT, "tweetsAfter.RData"), collapse = ""))
save(tweetsBefore.df, file = paste(c(workingDT, "tweetsBefore.RData"), collapse = ""))


UsersTotal.df <- tweets.df %>% distinct(user_id_str)
UsersBefore.df <- tweetsBefore.df %>% distinct(user_id_str)
UsersAfter.df <- tweetsAfter.df %>% distinct(user_id_str)
UsersAfterWithBefore.df<-subset(UsersAfter.df, (UsersAfter.df$user_id_str %in% UsersBefore.df$user_id_str))


UserFrequencies.df<-count(UsersTotal.df)
UserFrequencies.df$Before<-count(UsersBefore.df)
UserFrequencies.df$After<-count(UsersAfter.df)
UserFrequencies.df$AfterMergeBefore<-count(UsersAfterWithBefore.df)
colnames(UserFrequencies.df)<-c("Total", "Before", "After", "AfterWBefore")
save(UserFrequencies.df, file = paste(c(workingDT, "/Data/Filtered Data/UserFrequencies.RData"), collapse = ""))
