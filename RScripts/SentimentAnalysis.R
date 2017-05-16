############################################################
############################################################
# This script is used for Sentiment Analysis
############################################################
############################################################

library("dplyr")
library("httr")
library("tm")

load("Data/Seminar/Tweets.RData")

# get IDs of those tweets which are not english
to.translate <- which(tweets.df$lang != "en")

################################################################
###### TRANSLATION VIA https://api.microsofttranslator.com/V2/Http.svc/Translate

# first restrieve an access token
url.token <- "https://api.cognitive.microsoft.com/sts/v1.0/issueToken"

param.token <- list("Subscription-Key" = "b891aa65c63944dab2cce6d65f4e6ae5")

token.req <- POST(url.token, query = param.token)

if (http_error( token.req ) == FALSE && status_code( token.req )) {
  microsoft.token <- content( token.req )
}


url <- "https://api.microsofttranslator.com/V2/Http.svc/Translate"

parameters <- list(appid = "", to = "en", text ="")
parameters$appid <- as.character(paste(c("Bearer", " ", microsoft.token), collapse = ""))

parameters$text <- "Ich bin ein Berliner"

request <- POST(url, query = parameters)

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

translateR::getMicrosoftLanguages()
translateR::translate(content.vec = tw$text,
          microsoft.client.id = "FIM_Bubbles",
          microsoft.client.secret = "Yxb4yjbFPu3Y5lpaLxbQIbyY4vZBTbeoZAl43dnE0Bg=",
          source.lang = tr,
          target.lang = en)

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


