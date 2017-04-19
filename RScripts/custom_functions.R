#cleans tweets from unnecessary data and symbols
tweetCleanup <- function(dataframe) {
  
  #remove -
  dataframe$text <- gsub("—", " ", dataframe$text)
  #remove &
  dataframe$text <- gsub("&", " ", dataframe$text)
  #remove punctuation
  dataframe$text <- gsub("[[:punct:]]", " ", dataframe$text)
  #remove digits
  dataframe$text <- gsub("[[:digit:]]", "", dataframe$text)
  #remove http links
  dataframe$text <- gsub("http\\w+", "", dataframe$text)
  # ??
  dataframe$text <- gsub("\n", " ", dataframe$text)
  # ??
  dataframe$text <- gsub("[ \t]{2,}", "", dataframe$text)
  # ??
  dataframe$text <- gsub("^\\s+|\\s+$", "", dataframe$text)
  # ??
  #dataframe$text <- tolower(dataframe$text)
  
  return(dataframe)
}
