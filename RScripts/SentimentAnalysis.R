############################################################
############################################################
# This script is used for Sentiment Analysis
############################################################
############################################################
save(tweets.after, file = "translations.RData")


library("dplyr")
library("httr")
library("xml2")
library("tm")


load("Data/Seminar/Tweets.RData")
save(tweets.df, file = "Data/Seminar/Tweets.RData")

# get rid of tweets older than 16.04.2017
tweets.after <- subset( tweets.df, format( strptime( tweets.df$created_at,
                                                      "%a %b %d %H:%M:%S %z %Y",
                                                      tz="GMT"),'%d') == 16 )
                       
# now either they are tweeted on 16.04.2017 before 20:00
# or are not from 16.04.2017
tweets.after <- subset( tweets.after, ( format( strptime( tweets.after$created_at,
                                          "%a %b %d %H:%M:%S %z %Y",
                                          tz = "GMT" ),'%d' ) == 16 
                                      & format( strptime( tweets.after$created_at,
                                          "%a %b %d %H:%M:%S %z %Y",
                                          tz = "GMT" ),'%H' ) < 18 )
                                      | format( strptime( tweets.after$created_at,
                                          "%a %b %d %H:%M:%S %z %Y",
                                          tz = "GMT" ),'%d' ) > 16 )


tweets.after <- subset( tweets.after, format( strptime( tweets.after$created_at,
                                                            "%a %b %d %H:%M:%S %z %Y",
                                                            tz = "GMT" ),'%H' ) >= 18 )


# get IDs of those tweets which are not english
to.translate <- which(tweets.after$lang != "en")
#################################################################
#################################################################
if ( difftime(Sys.time(), token.time, units = "secs") > 540 ) {
  token.req <- POST(url.token, query = param.token)
  if (http_error( token.req ) == FALSE && status_code( token.req ) == 200)
  {
    token.time <- Sys.time()
    token <- rawToChar( content( token.req ) )
  }
#################################################################
#################################################################
  

################################################################
###### TRANSLATION VIA https://api.microsofttranslator.com/V2/Http.svc/Translate

# first restrieve an access token
get.token <- function() {
  url.token <- "https://api.cognitive.microsoft.com/sts/v1.0/issueToken"
  # flo
  param.token <- list("Subscription-Key" = "f776294aebb2485dbb5e5bb3b54d634f")
  # old param.token <- list("Subscription-Key" = "bf01c74f5835401993e7576964444c7e")
  # older param.token <- list("Subscription-Key" = "b891aa65c63944dab2cce6d65f4e6ae5")
  
  if (exists("token.time") == FALSE) {
    token.time <- Sys.time()
  }
  
  if (exists("token") == FALSE) {
    token.req <- POST(url.token, query = param.token)
    if (http_error( token.req ) == FALSE && status_code( token.req ) == 200)
    {
      token.time <- Sys.time()
      token <- rawToChar( content( token.req ) )
    }
  }
  return(token)
}


url <- "https://api.microsofttranslator.com/V2/Http.svc/Translate"

j <- 2000

for (i in 1717:length(to.translate)) {
  j <- j + 1
  
  if (j >= 2000) {
    parameters <- list(appid = paste("Bearer",get.token()), from = "", to = "en", text = "")
    j <- 0
  }
  
    if ( as.character(tweets.after[to.translate[i], "lang"]) !=
          as.character(tweets.after[to.translate[i], "user_lang"]) ) {
      if ( as.character(tweets.after[to.translate[i], "lang"]) == "tr" ) {
        parameters$from <- "tr"
      } else {
        parameters$from <- tweets.after[to.translate[i], "user_lang"]
      }
      
    } else {
      parameters$from <- tweets.after[to.translate[i], "lang"]
    }
    
    parameters$text <- tweets.after[to.translate[i], "text"]
    request <- GET(url, query = parameters)
    
    if ( http_error( request ) == FALSE & status_code( request ) == 200) {
      tweets.after[to.translate[i],"translation"] <- 
        as.character( as_list( content( request ) )[1])
    } else {
      as_list( content( request ) )
      errors <- rbind( errors, tweets.after[to.translate[i],] )
    }
}

################################################################


################################################################
###### TRANSLATION VIA https://tech.yandex.com/translate/
# define url and basic parameters
url <- "https://translate.yandex.net/api/v1.5/tr.json/translate"
parameters <- list(key = key, lang = "en", text ="")


for(i in 1:length(to.translate))
{
  # get tweet text
  parameters$text = tw[to.translate[i], "text"]
  # POST request with url and parameters
  request <- POST(url, query = parameters)
  
  # if request was succesful, then save the translate text
  if (http_error(request) == FALSE && status_code(request)) {
    tw[i, "translation"] <- content(request)$text
  }
}
######################## END OF TRANSLATION
################################################################





# first of all we need to prepare the tweet texts
texts <- as.character(tweets.df[tweets.df$lang != "en","text"])

# only original posts
retweets  <- grepl("(RT)(?:\\b\\W@\\w+)", tweets.df$text, ignore.case = TRUE )
texts <- texts[!retweets]

# remove retweets and Original Poster
texts <- gsub("(RT)(?:\\b\\W@\\w+)", "", texts)
# remove links
texts <- gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", texts)
# remove hashtags
texts <- gsub("[#]{1}(\\w+)", "", texts)
# remove punctuation
texts <- gsub("[[:punct:]]", "", texts)
# remove numerics
texts <- gsub("[[:digit:]]", "", texts)
# remove beginning whitespaces
texts <- gsub("^\\s+|\\s+$", "", texts)
# remove duplicate whitespaces
texts <- gsub("\\s+", " ", texts)



######## TESTING

load("Data/microsoft_api.RData")

tw <- tweets.df[3,]
tw$text <- as.character(tw$text)

texts <- texts_temp

# encode texts
Encoding(texts) <- "UTF-8"
texts <- iconv(texts, "ASCII", "UTF-8", sub = "")

texts <- tolower(texts)

sum(nchar(texts))

texts <- as.data.frame(texts)

test <- cbind(tweets.df, texts_temp)
test$texts_temp <- as.character(test$texts_temp)
test <- test[test$lang != "en",]


test$texts_temp <- iconv(test$texts_temp, to="UTF-8")


texts2 <- iconv(texts, to = "UTF-8")

# convert to lower case
texts <- tolower(texts)


