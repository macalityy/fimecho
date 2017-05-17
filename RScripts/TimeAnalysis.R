# Load Libraries
library(stringr)
library(plyr)
library(dplyr)

file <- paste(path,"Turkey/Turkeyall.csv",sep="")
filename.out<-paste(path,"User_HashtagTurkeyAll.csv",sep="")
nodelist.filename <- paste(path,"KantenTurkey.csv",sep="")




#read csv file to get tweets into dataframe
#tweets.df <- read.csv2(file = file, header = TRUE, stringsAsFactors = FALSE)

#shrink dataframe to columns we might use later
tweets.df <- subset(tweets.df[ ,c("X.1", "created_at")])

Sys.getlocale()
Sys.setlocale("LC_TIME", "English")
tweets.df$DateHour<-format(strptime(tweets.df$created_at, "%a %b %d %H:%M:%S %z %Y", tz="GMT"),'%Y-%m-%d %H')
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

tweets16After20UTC.df<-tweets.df[which(tweets.df$Date == "2017-04-16"& tweets.df$Hour >= 20), ]
tweets16AT20UTC.df<-tweets.df[which(tweets.df$Date == "2017-04-16"& tweets.df$Hour == 20), ]
tweets17.df<-tweets.df[tweets.df$Date>="2017-04-17", ]
tweetsAfter.df<-rbind(tweets16After20UTC.df,tweets17.df)
tweetsBefore.df<-subset(tweets.df, !(tweets.df$id_str %in% tweetsAfter.df$id_str)) 

UsersTotal.df <- tweets.df %>% distinct(user_id_str)
UsersBefore.df <- tweetsBefore.df %>% distinct(user_id_str)
UsersAfter.df <- tweetsAfter.df %>% distinct(user_id_str)
UsersAfterWithBefore.df<-subset(UsersAfter.df, (UsersAfter.df$user_id_str %in% UsersBefore.df$user_id_str))

