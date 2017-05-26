################################################################################
################################################################################
# Analysis of Communities and Sentiments
################################################################################
################################################################################

library(igraph)

load("Data/Seminar/Tweets.RData")
load("Data/Seminar/VerticesAssessed.RData")
load("Data/Seminar/Edgelist_no.RData")
load("Data/Seminar/mu.RData")


# create IGraph from Edgelist and Vertices
tr.graph <-
  graph_from_data_frame(edgelist.df, directed = TRUE, vertices = vertices.df)

# convert Graph to undirected Graph
tr.graph <- as.undirected(tr.graph, "each")

wt.comm <- walktrap.community(tr.graph)
ml.comm <- multilevel.community(tr.graph)
im.comm <- infomap.community(tr.graph)
lp.comm <- label.propagation.community(tr.graph)

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
nrow(wt.sizes[wt.sizes$Freq > 1,])
nrow(wt.sizes[wt.sizes$Freq > 10,])
nrow(wt.sizes[wt.sizes$Freq > 100,])

# members clustered by 10 largest communities
sum(head(wt.sizes[order(wt.sizes$Freq, decreasing = TRUE),], n = 10)$Freq) / nrow(vertices.df)
# ave sentiment in clusters
wt_sentiment <-
  aggregate(vertices.df$ave_sentiment, list(vertices.df$wt_comm), mean)
colnames(wt_sentiment) <- c("wt_comm", "ave_sentiment")

# plot community size > 10
plot(
  c(1:nrow(wt_sentiment[wt_sentiment$Freq > 10,])),
  sort(ml.sizes[wt_sentiment$Freq > 10, "Freq"], decreasing = TRUE),
  main = "Walktrap Communities with more than 10 members",
  xlab = "Communities",
  ylab = "# Members",
  ylim = c(0, 15000)
)

# plot ave sentiment distribution
plot(density(wt_sentiment[!is.na(wt_sentiment$ave_sentiment), "ave_sentiment"]),
     main = "Average Sentiment Distribution in WT Communities")



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
nrow(ml.sizes[ml.sizes$Freq > 1,])
nrow(ml.sizes[ml.sizes$Freq > 10,])
nrow(ml.sizes[ml.sizes$Freq > 100,])

# members clustered by 10 largest communities
sum(head(ml.sizes[order(ml.sizes$Freq, decreasing = TRUE),], n = 10)$Freq) / nrow(vertices.df)

# 

# ave sentiment in clusters
ml_sentiment <-
  aggregate(vertices.df$ave_sentiment, list(vertices.df$ml_comm), mean)
colnames(ml_sentiment) <- c("ml_comm", "ave_sentiment")

# plot community size > 10
plot(
  c(1:nrow(ml.sizes[ml.sizes$Freq > 10,])),
  sort(ml.sizes[ml.sizes$Freq > 10, "Freq"], decreasing = TRUE),
  main = "Multi-Level Communities with more than 10 members",
  xlab = "Communities",
  ylab = "# Members",
  ylim = c(0, 15000)
)

# plot ave sentiment distribution
plot(density(ml_sentiment[!is.na(ml_sentiment$ave_sentiment), "ave_sentiment"]),
     main = "Average Sentiment Distribution in ML Communities")
