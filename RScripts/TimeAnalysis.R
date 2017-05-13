# Load Libraries
library(stringr)
library(igraph)
library(streamR)
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

library(dplyr)
DateTimeFreq.df<- as.data.frame(table(tweets.df$DateHour))
DateTimeFreq.df$Hours<-format(strptime(DateTimeFreq.df$Var1, "%Y-%m-%d %H", tz="GMT"),'%H')


for(i in 1:nrow(DateTimeFreq.df)){
  DateTimeFreq.df$Absolut[i]=sum(DateTimeFreq.df[1:i,2])
}
for(i in 1:nrow(DateTimeFreq.df)){
  DateTimeFreq.df$Percent[i]=DateTimeFreq.df$Absolut[i]/sum(DateTimeFreq.df$Freq)
}

barData<-DateTimeFreq.df$Freq
y <- lineData<-DateTimeFreq.df$Percent
x <- barplot(barData, 
             axes = FALSE,
             col = "blue", 
             xlab = "",
             ylab = "")[, 1]
axis(1, at = x, labels = DateTimeFreq.df$Hours)
ats <- c(0, 100)
axis(4, labels = paste0(ats, "%"))
#axis(3, at = x, labels = NA) 
par(new = TRUE)
plot(x = x, y = y, type = "l", col = "red", axes = FALSE, xlab = "", ylab = "")
axis(2, at = c(pretty(lineData), max(lineData)), las = 2) 
#mtext(text="Lines of code by Programmer", side = 3, line = 1)
box()    