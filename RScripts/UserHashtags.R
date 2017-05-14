library(stringr)

tw.df <- tweets.df[,c("text", "user_id_str")]


#get all hashtags per tweets
tw.df$text <- str_extract_all(tw.df$text,"[#]{1}(\\w+)")

hash.df <- data.frame()

for (i in 1:nrow(tw.df))
{
  #get current row
  row <- tw.df[i, ]
  #unlist all hashtags
  hashtags <- unlist(row$text)
  
  #create a second vector with length of hashtags
  user <- as.list(1:length(hashtags))
  user[] <- row$user_id_str
  
  # create matrix with this user and his hashtags
  hashtags <- cbind(user, hashtags)
  
  #bind the matrix to all other users
  hash.df <- rbind(hash.df, hashtags)
  
}

rm(hashtags)

