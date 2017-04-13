#!/usr/bin/env Rscript
#load libraries
library("ROAuth")
library("rjson")
library("bitops")
library("RCurl")
library("streamR")

# Load authentification into workspace
load("tw_oauth.Rdata")

args <- commandArgs(trailingOnly = TRUE)
tw_track <- args[1]
tw_duration <- as.numeric(args[2])

#set filename
current.time <- format(Sys.time(), "%Y_%m_%d_%H_%M")
tw_filename <- paste(c("Data/", tw_track, "_", current.time, ".json"), collapse = "")
    
#get twitter data
filterStream(file.name = tw_filename, track = tw_track, timeout = tw_duration, oauth = tw_oauth)

#parseData
tweets <- parseTweets(tw_filename, simplify = FALSE)
#delete JSON file
file.remove(tw_filename)
#write in CSV
tw_filename <- paste(c("Data/", tw_track, "_", current.time, ".csv"), collapse = "")
write.csv2(tweets, file = tw_filename)

