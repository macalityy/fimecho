#!/usr/bin/env Rscript
#load libraries
library("ROAuth")
library("rjson")
library("bitops")
library("RCurl")
library("streamR")

# Load authentification into workspace
load("/home/rstudio/fimecho/RScripts/tw_oauth.Rdata")

#get arguments from cmd line
args <- commandArgs(trailingOnly = TRUE)
#get hashtags and searchstrings from args
tw_track <- unlist(strsplit(args[1], ", "))
#get duration from args
tw_duration <- as.numeric(args[2])

#set filename
current.time <- format(Sys.time(), "%Y_%m_%d_%H_%M")
tw_filename <- paste(c("Data/", tw_track, "_", current.time, ".json"), collapse = "")
    
#get twitter data
filterStream(file.name = tw_filename, track = tw_track, timeout = tw_duration, oauth = tw_oauth)

#parseData in DataFrame
tweets.df <- parseTweets(tw_filename, simplify = FALSE)
#delete JSON file
file.remove(tw_filename)
#write in CSV
tw_filename <- paste(c("Data/", tw_track, "_", current.time, ".csv"), collapse = "")
write.csv2(tweets.df, file = tw_filename)

