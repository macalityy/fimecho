################################################################################
################################################################################
# Analysis of Communities and Sentiments
################################################################################
################################################################################

rm(list = ls())

set.seed(42)

library(igraph)

load("Data/Seminar/Tweets.RData")
load("Data/Seminar/VerticesComm.RData")
load("Data/Seminar/Edgelist_no.RData")
load("Data/Seminar/mu.RData")



########################################################
# WT Analysis
########################################################
wt.sizes <- as.data.frame(sizes(wt.comm))
# Number of Communities
nrow(wt.sizes)
# Mean, Median, Min and Max
summary(wt.sizes$Freq)
# Standard Deviation
sd(wt.sizes$Freq)
# mu
sum(mu.df$WT_ties_ext) / sum(mu.df$WT_ties_f_sum)


#different filters
filter_wt <- vector()
for (i in 1000:1) {
  filter_wt[i] <- nrow(wt.sizes[wt.sizes$Freq > i,])
}

nrow(wt.sizes[wt.sizes$Freq > 1,])
nrow(wt.sizes[wt.sizes$Freq > 10,])
nrow(wt.sizes[wt.sizes$Freq > 100,])
nrow(wt.sizes[wt.sizes$Freq > 1000,])

lg_comm <- wt.sizes[wt.sizes$Freq > 1000,]
colnames(lg_comm) <- c("Id", "Freq")
lg_comm
# how many users are in the largest ones:
sum(lg_comm$Freq) / nrow(vertices.df)

# subset users of largest communities
users_largest <-
  subset(vertices.df, vertices.df$wt_comm %in% lg_comm$Id)

# how many edges are from users of these communities
nrow(subset(
  edgelist.df,
  (
    edgelist.df$Source %in% users_largest$Id |
      edgelist.df$Target %in% users_largest$Id
  )
)) / nrow(edgelist.df)

# get size of largest communities
table(users_largest$wt_comm)

# aggregate attributes for communities
aggr <-
  aggregate(users_largest[, c("status_count",
                              "followers_count",
                              "ties_in",
                              "ties_out",
                              "ties_sum")], list(users_largest$wt_comm), mean)
aggr$Id <- c(1:nrow(aggr))
aggr <- aggr[, c(7, 1:6)]

aggr <- merge(aggr, lg_comm, by.x = "Group.1", by.y = "Id")

aggr

barplot(
  as.matrix(aggr[, c(3:4)]),
  ylim = c(0, 40000),
  names.arg = c("Mean published tweets", "Mean followers"),
  beside = TRUE
)

# aggregate complete communities
aggr_complete <- aggregate(vertices.df[, c("status_count",
                                           "followers_count",
                                           "ties_in",
                                           "ties_out",
                                           "ties_sum")], list(vertices.df$wt_comm), mean)
colnames(aggr_complete)[1] <- "Id"

aggr_complete <-
  merge(aggr_complete, wt.sizes, by.x = "Id", by.y = "Community.sizes")

summary(aggr_complete)

# plot community size > 10
dev.new(width = 5, height = 5)
plot(
  c(1:nrow(wt.sizes[wt.sizes$Freq > 0,])),
  sort(wt.sizes[wt.sizes$Freq > 0, "Freq"], decreasing = TRUE),
  main = "Walktrap community sizes",
  xlab = "Community ID",
  ylab = "# Members",
  ylim = c(0, 15000)
)


plot(
  log(aggr_complete$status_count) ~ log(aggr_complete$followers_count) ,
  ylab = "Followers (log scale)",
  xlab = "Tweets published (log scale)",
  main = "Relationship between number of followers and number of tweets posted in communities"
)
smoothScatter(
  log(vertices.df$status_count) ~ log(vertices.df$followers_count),
  xlab = "Followers (log scale)",
  ylab = "Tweets published (log scale)",
  main = "Relationship between number of followers and number of tweets posted"
)
plot(
  log(aggr_complete$status_count) ~ log(aggr_complete$Freq),
  ylab = "Tweets published (log scale)",
  xlab = "Community Size (log scale)",
  main = "Relationship between community size and tweets published"
)
plot(
  log(aggr_complete$ties_sum) ~ log(aggr_complete$Freq),
  ylab = "Degree centrality (log scale)",
  xlab = "Community Size (log scale)",
  main = "Relationship between community size and degree centrality"
)

# members clustered by 10 largest communities
sum(head(wt.sizes[order(wt.sizes$Freq, decreasing = TRUE),], n = 10)$Freq) / nrow(vertices.df)


#### Sentiment
# ave sentiment in clusters
temp <- vertices.df[!is.na(vertices.df$ave_sentiment),]
wt_sentiment <-
  aggregate(temp$ave_sentiment, list(temp$wt_comm), mean)

temp <- as.data.frame(table(temp$wt_comm))
colnames(wt_sentiment) <- c("Id", "ave_sentiment")

wt_sentiment <-
  merge(wt_sentiment, temp, by.x = "Id", by.y = "Var1")

aggr_complete <-
  merge(aggr_complete,
        wt_sentiment,
        by = "Id",
        all.x = TRUE)
colnames(aggr_complete)[7:9] <-
  c("size", "ave_sentiment", "users_w_sentiment")

# compute percentage of users with sentiment
aggr_complete$rel_users_w_sentiment <-
  aggr_complete$users_w_sentiment / aggr_complete$size


# use classify function from SentimentAnalysis.R
aggr_complete$class <- sapply(aggr_complete$ave_sentiment, classify)

# plot classification of clusters
barplot((table(aggr_complete$class) * 100 / nrow(aggr_complete)),
        col = c("grey", "red", "grey", "green"),
        ylim = c(0, 40),
        names.arg = c("Not Assessed", "Negative", "Neutral", "Positive"),
        main = "Community Classification"
)

temp <-
  as.matrix(aggregate(vertices.df$class, list(vertices.df$wt_comm), table))
temp <- as.data.frame(temp)
colnames(temp) <-
  c("Id", "not_assessed", "negative", "neutral", "positive")

aggr_complete <- merge(aggr_complete, temp, by = "Id")


save(aggr_complete, file = "Data/Seminar/CommunitiesAnalysis.RData")
load("Data/Seminar/CommunitiesAnalysis.RData")

# now get the data for the 5 largest communities
aggr <-
  head(aggr_complete[order(aggr_complete$size, decreasing = TRUE), ], n = 5)

plot(density(aggr_complete[!is.na(aggr_complete$ave_sentiment),]$ave_sentiment),
     main = "Distribution of Sentiment in WT communities")


# !! Relative Amount of Classes in largest communities
col <- c("gray", "tomato", "wheat", "springgreen4")

barplot(
  t(as.matrix((aggr[, c(12:15)]) * 100 / aggr$size)),
  beside = TRUE,
  ylim = c(0, 50),
  names.arg = c("#2", "#1", "#5", "#186", "#20"),
  ylab = "Percentage",
  col = col,
  main = "Percentage of Sentiment Classes within Communities"
)

abline(h = seq(0, 50, 5),
       col = "snow4",
       lty = "dotted")

barplot(
  t(as.matrix((aggr[, c(12:15)]) * 100 / aggr$size)),
  beside = TRUE,
  ylim = c(0, 50),
  col = col,
  names.arg = c("#2", "#1", "#5", "#186", "#20"),
  add = TRUE
)

legend(
  "top",
  inset = .05,
  legend = c("Not Assessed", "Negative", "Neutral", "Positive"),
  fill = col
)


# plot ave sentiment distribution
plot(density(wt_sentiment[!is.na(wt_sentiment$ave_sentiment), "ave_sentiment"]),
     main = "Distribution of average sentiment in WT communities")


##################################################
# HOMOPHILY CALCULATION
##################################################

##### 1 Define an Edgelist with columns: source, target, target comm, target class
homo_edge <-
  merge(
    edgelist.df,
    vertices.df[, c("Id", "wt_comm", "class")],
    by.x = "Source",
    by.y = "Id",
    all.x = TRUE,
    all.y = FALSE
  )
colnames(homo_edge)[(ncol(homo_edge) - 1):ncol(homo_edge)] <-
  c("source_comm", "source_class")
homo_edge <-
  merge(
    homo_edge,
    vertices.df[, c("Id", "wt_comm", "class")],
    by.x = "Target",
    by.y = "Id",
    all.x = TRUE,
    all.y = FALSE
  )
colnames(homo_edge)[(ncol(homo_edge) - 1):ncol(homo_edge)] <-
  c("target_comm", "target_class")

##### 2 Omit Edges and Vertices which are not Assessed
homo_edge <-
  subset(
    homo_edge,
    homo_edge$source_class != "Not Assessed" &
      homo_edge$target_class != "Not Assessed"
  )

##### 3 Get set of sentiments and omit "Not Assessed"
sentiments <- levels(homo_edge$source_class)
sentiments <- sentiments[sentiments != "Not Assessed"]

##### 4 Calculate homophily for each community
for (i in 1:nrow(aggr_complete)) {
  # define homophily
  homophily <- 0.0
  
  # get current community
  row <- aggr_complete[i,]
  # get all edges where community members are the source
  temp <- homo_edge[homo_edge$source_comm == row$Id, ]
  
  # for every sentiment s of set of sentiments S
  for (j in 1:length(sentiments)) {
    homophily <-
      homophily + ((# total vertices with same sentiment in this community
        nrow(vertices.df[(vertices.df$wt_comm == row$Id &
                            vertices.df$class == sentiments[j]),])
        / # total vertices in community
          nrow(vertices.df[vertices.df$wt_comm == row$Id,]))
        * (# edges within same sentiment
          nrow(temp[(temp$source_class == sentiments[j] &
                       temp$target_class == sentiments[j]), ])
          /  # total edges of this sentiment
            nrow(temp[temp$source_class == sentiments[j], ])))
  }
  # set homophily value for community
  aggr_complete[i, "homophily"] <- homophily
}

#################### END OF HOMOPHILY CALCULATION

## note: exchange aggr_complete for aggr to only have top 5 largest communities

##### ANALYSIS
summary(aggr$homophily)
sd(aggr$homophily)



summary(aggr_complete$homophily)
sd(aggr_complete$homophily, na.rm = TRUE)

temp <- aggr_complete[!is.na(aggr_complete$homophily),]

plot(aggr_complete[!is.na(aggr_complete$homophily), "homophily"], ylab = "Homophily", main = "Homophily in Communities", pch = 16)
plot(
  density(aggr_complete[!is.na(aggr_complete$homophily), "homophily"]),
  xlim = c(-0.1, 1),
  main = "Homophily distribution",
  lwd = 1.5
)

plot(
  temp$homophily ~ temp$size,
  pch = 16,
  xlab = "Community Size",
  ylab = "Homophily",
  main = "Homophily compared to community size"
)

cor(temp$homophily, temp$ties_out)
cor(temp$homophily, temp$size)
cor(temp$homophily, temp$ties_in)
cor(temp$rel_users_w_sentiment, temp$homophily)

plot(temp$rel_users_w_sentiment, temp$homophily, pch = 16)
abline(lm(temp$homophily ~ temp$rel_users_w_sentiment))

plot(temp$homophily ~ temp$ties_out)
plot(temp$homophily ~ temp$size)



########################################################
# ML Analysis
########################################################
ml.sizes <- as.data.frame(sizes(ml.comm))
# Number of Communities
nrow(ml.sizes)
# Mean, Median, Min and Max
summary(ml.sizes$Freq)
# Standard Deviation
sd(ml.sizes$Freq)
# mu for ML
sum(mu.df$ML_ties_ext) / sum(mu.df$ML_ties_f_sum)

#different filters
filter_ml <- vector()
for (i in 1000:1) {
  filter_ml[i] <- nrow(ml.sizes[ml.sizes$Freq > i,])
}

nrow(ml.sizes[ml.sizes$Freq > 1,])
nrow(ml.sizes[ml.sizes$Freq > 10,])
nrow(ml.sizes[ml.sizes$Freq > 100,])
nrow(ml.sizes[ml.sizes$Freq > 1000,])

# members clustered by 10 largest communities
sum(head(ml.sizes[order(ml.sizes$Freq, decreasing = TRUE),], n = 10)$Freq) / nrow(vertices.df)


# ave sentiment in clusters
ml_sentiment <-
  aggregate(vertices.df$ave_sentiment, list(vertices.df$ml_comm), mean)
colnames(ml_sentiment) <- c("ml_comm", "ave_sentiment")

# plot community size > 10
dev.new(width = 5, height = 5)
plot(
  c(1:nrow(ml.sizes[ml.sizes$Freq > 0,])),
  sort(ml.sizes[ml.sizes$Freq > 0, "Freq"], decreasing = TRUE),
  main = "Multi-Level community sizes",
  xlab = "Community ID",
  ylab = "# Members",
  ylim = c(0, 15000)
)

# plot ave sentiment distribution
plot(density(ml_sentiment[!is.na(ml_sentiment$ave_sentiment), "ave_sentiment"]),
     main = "Distribution of average Sentiment in ML communities")

# plot size difference
plot(
  ts(filter_ml),
  col = "green",
  xlab = "Minimum Community Size",
  ylab = "Number of Communities",
  ylim = c(0, 100),
  xlim = c(1000, 0),
  main = "Comparison of community sizes"
)
par(new = TRUE)
lines(ts(filter_wt), col = "red")
legend(
  "topleft",
  inset = .05,
  title = "Community Detection Algorithm",
  c("ML", "WT"),
  fill = c("green", "red"),
  horiz = TRUE
)
