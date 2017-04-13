#load libraries
library("ROAuth")
library("rjson")
library("RCurl")
library("bitops")
library("streamR")

# Load authentification into workspace
load("tw_oauth.Rdata")

# get user input hashtags
cat("Which hashtags should be tracked?")
tw_track <- readLines(con = stdin(), n = 1)
tw_track <- as.character(unlist(strsplit(tw_track, ",")))

# get user input duration
cat("How many hours until timeout")
tw_duration <- readLines(con = stdin(), n = 1)
tw_duration <- as.numeric(unlist(strsplit(tw_duration, ",")))

cat("Script will start with the following parameters:")
cat("Hashtags: ", tw_track)
cat("Timeout Duration in Hours: ", tw_duration)
cat("Ok? (y/n)")

input <- readLines(con = stdin(), n = 1)
input <- tolower(input)

if (input == "y") {
  # continue
  # calculate seconds out of hours given
  tw_duration <- tw_duration * 3600
  
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
    tw_file <- tw_track + tw_i + ".json"
    #get twitter data
    filterStream(file.name = tw_file, track = tw_track, timeout = tw_timeout, oauth = tw_oauth)
    #reduce left duration
    tw_duration <- tw_duration - tw_timeout
  }
  
} else {
  cat("Exit...")
}