############################################################
############################################################
# This script is used to Split the Dataset
############################################################
############################################################

# get rid of tweets older than 16.04.2017
tweets.before <- subset( tweets.df, format( strptime( tweets.df$created_at,
                                                     "%a %b %d %H:%M:%S %z %Y",
                                                     tz="GMT"),'%d') <= 16 )

# now either they are tweeted on 16.04.2017 before 20:00 (18 in UTC)
# or are not from 16.04.2017
tweets.before <- subset( tweets.before, ( format( strptime( tweets.before$created_at,
                                                          "%a %b %d %H:%M:%S %z %Y",
                                                          tz = "GMT" ),'%d' ) == 16 
                                        & format( strptime( tweets.before$created_at,
                                                            "%a %b %d %H:%M:%S %z %Y",
                                                            tz = "GMT" ),'%H' ) < 15 )
                        | format( strptime( tweets.before$created_at,
                                            "%a %b %d %H:%M:%S %z %Y",
                                            tz = "GMT" ),'%d' ) < 16 )

tweets.after <- subset( tweets.df, format( strptime( tweets.df$created_at,
                                                      "%a %b %d %H:%M:%S %z %Y",
                                                      tz="GMT"),'%d') >= 16 )


tweets.after <- subset( tweets.after, ( format( strptime( tweets.after$created_at,
                                                            "%a %b %d %H:%M:%S %z %Y",
                                                            tz = "GMT" ),'%d' ) == 16 
                                          & format( strptime( tweets.after$created_at,
                                                              "%a %b %d %H:%M:%S %z %Y",
                                                              tz = "GMT" ),'%H' ) >= 14 )
                         | ( format( strptime( tweets.after$created_at,
                                             "%a %b %d %H:%M:%S %z %Y",
                                             tz = "GMT" ),'%d' ) == 17 
                             & format( strptime( tweets.after$created_at,
                                                 "%a %b %d %H:%M:%S %z %Y",
                                                 tz = "GMT" ),'%H' ) <= 22 ))


save(tweets.after, file = "Data/Seminar/TweetsAfter.RData")
save(tweets.before, file = "Data/Seminar/TweetsBefore.RData")

tweets.after <-
  tweets.after[which(
    as.POSIXct(tweets.after$created_at, format = "%d-%m-%Y %H:%M:%S", tz = "GMT") >=
      as.POSIXct("2017-04-16 14:58:47", tz = "GMT")
  ),]

save(tw.dayafter, file = "Data/Seminar/TweetsAfterAndDayAfter.RData")




