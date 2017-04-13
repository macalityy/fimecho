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


# calculate seconds out of hours given
  tw_duration <- tw_duration * 3600
  tw_i <- 0
  # 
  while( tw_duration >= 0) {
    if (tw_duration >= 3600) {
      tw_timeout <- 3600
    } else {
      tw_timeout <- tw_duration
    }
    
    #increment filename
    tw_i <- tw_i + 1
    #set filename
    tw_file <- paste(c(tw_track, tw_i, ".json"), collapse = "")
    
    #get twitter data
    filterStream(file.name = tw_file, track = tw_track, timeout = tw_timeout, oauth = tw_oauth)
    #reduce left duration
    tw_duration <- tw_duration - tw_timeout
  }

