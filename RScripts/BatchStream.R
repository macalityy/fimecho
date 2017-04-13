#load libraries
library("ROAuth")
library("rjson")
library("RCurl")
library("bitops")
library("streamR")

# Load authentification into workspace
load("tw_oauth.Rdata")

args <- commandArgs(trailingOnly = TRUE)
tw_track <- args[1]
tw_duration <- as.numeric(args[2])
tw_file <- args[3]

#set filename
tw_filename <- paste(c("/data/", tw_track, tw_file, ".json"), collapse = "")
    
#get twitter data
filterStream(file.name = tw_filename, track = tw_track, timeout = tw_duration, oauth = tw_oauth)

#parseData
tweets <- parseTweets(tw_file, simplify = FALSE)
#write in CSV
write.csv2(tweets, file = paste(c("/data/", tw_track, tw_file, ".csv")))
#delete JSON file
file.remove(tw_filename)