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
tw_duration <- args[2]
tw_file <- args[3]

#set filename
tw_file <- paste(c("/data/", tw_track, tw_file, ".json"), collapse = "")
    
#get twitter data
filterStream(file.name = tw_file, track = tw_track, timeout = tw_duration, oauth = tw_oauth)