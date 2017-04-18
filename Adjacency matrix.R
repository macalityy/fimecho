#####################From here to 60th line Rscript is as same as who retweets whom#########################
library(igraph)
library(stringr)
library(twitteR)

#setting workplaceand reading .csv file
setwd("c:/Users/User/Desktop/rdata")
tweets<-read.csv2("tweets.csv",header=TRUE, sep=";", stringsAsFactors = FALSE)

#reading only text to find RT
tweets_findrt<-tweets[,"text"]

#reading only who posted
who_post<-tweets[,"screen_name"]
who_post

#finding Who_retweet including @
who_retweet<-regmatches(tweets_findrt, gregexpr('@\\w+', tweets_findrt))
who_retweet
#finding who retweet without @
who_retweet<-regmatches(tweets_findrt, gregexpr('@\\K\\w+', tweets_findrt, perl=T))
as.character(who_retweet)
#################but data includes also no-Retweet Data. so we have to define which data have RT########   

#definding which data have RT
grep("(RT|via)((?:\\b\\W*@\\w+)+)", tweets_findrt, 
     ignore.case=TRUE, value=TRUE)
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)", 
                   tweets_findrt, ignore.case=TRUE)
rt_patterns

#view only topmost 10 tuples
head(tweets_findrt[rt_patterns],10)

#changing datatype as list and reading only name of Who_post and who_retweet 
who_retweet = as.list(1:length(rt_patterns))
who_post = as.list(1:length(rt_patterns))
for (i in 1:length(rt_patterns)){ 
  # get tweet with retweet entity
  twit = tweets_findrt[[rt_patterns[i]]]
  # get retweet source 
  poster = str_extract_all(twit,
                           "(RT|via)((?:\\b\\W*@\\w+)+)")
  #remove ':'
  poster = gsub(":", "", unlist(poster)) 
  # name of retweeted user
  who_post[[i]] = gsub("(RT @|via @)", "", poster, ignore.case=TRUE) 
  who_retweet[[i]]=tweets[rt_patterns[i],"screen_name"]
}
#view names
who_post
who_retweet

#transfer datatype as character
who_post = unlist(who_post)
who_retweet = unlist(who_retweet)

#binding who_post and who_retweet
retweeter_poster = cbind(who_retweet, who_post)
retweeter_poster

#####################From here to 60th line Rscript is as same as who retweets whom#######################

#Applying Adjacency Matrix
retweeter_poster
a<-get.adjacency(graph.edgelist(as.matrix(retweeter_poster),directed=TRUE))
g<-graph.adjacency(a,mode="directed")
par(bg="white")
plot(g)

