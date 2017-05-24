############################################################
############################################################
# This script is used for Translation
# it won't run from beginning to end without manual
# adjustments
############################################################
############################################################

library("dplyr")
library("httr")
library("xml2")
library("tm")


load("Data/Seminar/Tweets.RData")
save(tweets.df, file = "Data/Seminar/Tweets.RData")


# get IDs of those tweets which are not english
to.translate <- which(tweets.after$lang != "en")

################################################################
###### TRANSLATION VIA https://api.microsofttranslator.com/V2/Http.svc/Translate

# first restrieve an access token
get.token <- function() {
  url.token <-
    "https://api.cognitive.microsoft.com/sts/v1.0/issueToken"
  # flo
  param.token <-
    list("Subscription-Key" = "525b0b6dfa784ff8a3c7068c6e421fc7")
  # old param.token <- list("Subscription-Key" = "bf01c74f5835401993e7576964444c7e")
  # older param.token <- list("Subscription-Key" = "b891aa65c63944dab2cce6d65f4e6ae5")
  
  if (exists("token.time") == FALSE) {
    token.time <- Sys.time()
  }
  
  if (exists("token") == FALSE) {
    token.req <- POST(url.token, query = param.token)
    if (http_error(token.req) == FALSE &&
        status_code(token.req) == 200)
    {
      token.time <- Sys.time()
      token <- rawToChar(content(token.req))
    }
  }
  return(token)
}


url <- "https://api.microsofttranslator.com/V2/Http.svc/Translate"

j <- 2000

for (i in 19563:length(to.translate)) {
  j <- j + 1
  
  if (j >= 2000) {
    parameters <-
      list(
        appid = paste("Bearer", get.token()),
        from = "",
        to = "en",
        text = ""
      )
    j <- 0
  }
  
  if (as.character(tweets.after[to.translate[i], "lang"]) !=
      as.character(tweets.after[to.translate[i], "user_lang"])) {
    if (as.character(tweets.after[to.translate[i], "lang"]) == "tr") {
      parameters$from <- "tr"
    } else {
      parameters$from <- tweets.after[to.translate[i], "user_lang"]
    }
    
  } else {
    parameters$from <- tweets.after[to.translate[i], "lang"]
  }
  
  parameters$text <- tweets.after[to.translate[i], "text"]
  request <- GET(url, query = parameters)
  
  if (http_error(request) == FALSE &
      status_code(request) == 200) {
    tweets.after[to.translate[i], "translation"] <-
      as.character(as_list(content(request))[1])
  } else {
    as_list(content(request))
    errors <- rbind(errors, tweets.after[to.translate[i], ])
  }
}

################################################################

# check if we forgot anything!
not.translated <- subset(tweets.after,
                         (tweets.after$lang != "en") &
                          (!tweets.after$id_str %in% translations$id_str))


################################################################
# ###### TRANSLATION VIA https://tech.yandex.com/translate/
# # define url and basic parameters
# url <- "https://translate.yandex.net/api/v1.5/tr.json/translate"
# parameters <- list(key = key, lang = "en", text ="")
#
#
# for(i in 1:length(to.translate))
# {
#   # get tweet text
#   parameters$text = tw[to.translate[i], "text"]
#   # POST request with url and parameters
#   request <- POST(url, query = parameters)
#
#   # if request was succesful, then save the translate text
#   if (http_error(request) == FALSE && status_code(request)) {
#     tw[i, "translation"] <- content(request)$text
#   }
# }
# ######################## END OF TRANSLATION
################################################################



