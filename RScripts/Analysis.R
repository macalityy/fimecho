################################################################################
################################################################################
# Analysis of Communities and Sentiments
################################################################################
################################################################################

rm(list = ls())

set.seed(42)

library(igraph)

load("Data/Seminar/Tweets.RData")
load("Data/Seminar/VerticesAssessed.RData")
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
  filter_wt[i] <- nrow(wt.sizes[wt.sizes$Freq > i, ])
}

nrow(wt.sizes[wt.sizes$Freq > 1, ])
nrow(wt.sizes[wt.sizes$Freq > 10, ])
nrow(wt.sizes[wt.sizes$Freq > 100, ])
nrow(wt.sizes[wt.sizes$Freq > 1000, ])

# members clustered by 10 largest communities
sum(head(wt.sizes[order(wt.sizes$Freq, decreasing = TRUE), ], n = 10)$Freq) / nrow(vertices.df)
# ave sentiment in clusters
wt_sentiment <-
  aggregate(vertices.df$ave_sentiment, list(vertices.df$wt_comm), mean)
colnames(wt_sentiment) <- c("wt_comm", "ave_sentiment")
# plot ave sentiment distribution
plot(density(wt_sentiment[!is.na(wt_sentiment$ave_sentiment), "ave_sentiment"]),
     main = "Distribution of average sentiment in WT communities")

# plot community size > 10
dev.new(width = 5, height = 5)
plot(
  c(1:nrow(wt.sizes[wt.sizes$Freq > 0, ])),
  sort(wt.sizes[wt.sizes$Freq > 0, "Freq"], decreasing = TRUE),
  main = "Walktrap community sizes",
  xlab = "Community ID",
  ylab = "# Members",
  ylim = c(0, 15000),
)


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
  filter_ml[i] <- nrow(ml.sizes[ml.sizes$Freq > i, ])
}

nrow(ml.sizes[ml.sizes$Freq > 1, ])
nrow(ml.sizes[ml.sizes$Freq > 10, ])
nrow(ml.sizes[ml.sizes$Freq > 100, ])
nrow(ml.sizes[ml.sizes$Freq > 1000, ])

# members clustered by 10 largest communities
sum(head(ml.sizes[order(ml.sizes$Freq, decreasing = TRUE), ], n = 10)$Freq) / nrow(vertices.df)


# ave sentiment in clusters
ml_sentiment <-
  aggregate(vertices.df$ave_sentiment, list(vertices.df$ml_comm), mean)
colnames(ml_sentiment) <- c("ml_comm", "ave_sentiment")

# plot community size > 10
dev.new(width = 5, height = 5)
plot(
  c(1:nrow(ml.sizes[ml.sizes$Freq > 0, ])),
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
plot(ts(filter_ml),
     col = "green",
     xlab = "Minimum Community Size",
     ylab = "Number of Communities",
     ylim = c(0, 100),
     xlim = c(1000,0),
     main = "Comparison of community sizes")
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



