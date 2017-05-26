################################################################################
################################################################################
# Performance Analysis of Communities
################################################################################
################################################################################

library(igraph)

load("Data/Seminar/VerticesAssessed.RData")
load("Data/Seminar/Edgelist_no.RData")

# create IGraph from Edgelist and Vertices
tr.graph <- graph_from_data_frame(edgelist.df, directed = TRUE, vertices = vertices.df)
# convert Graph to undirected Graph 
tr.graph <- as.undirected(tr.graph, "each")

perf.wt <- data.frame()
perf.im <- data.frame()
perf.lp <- data.frame()
perf.ml <- data.frame()

pb <- txtProgressBar(min = 0, max = 10, style = 3)
for(i in 1:10) {
  temp <- system.time(walktrap.community(tr.graph))
  perf.wt[i,"user"] = temp[1]
  perf.wt[i,"system"] = temp[2]
  perf.wt[i,"elapsed"] = temp[3]
  perf.wt[i,"sum"] = sum(temp[1:3])
  
  temp <- system.time(label.propagation.community(tr.graph))
  perf.lp[i,"user"] = temp[1]
  perf.lp[i,"system"] = temp[2]
  perf.lp[i,"elapsed"] = temp[3]
  perf.lp[i,"sum"] = sum(temp[1:3])
  
  temp <- system.time(infomap.community(tr.graph))
  perf.im[i,"user"] = temp[1]
  perf.im[i,"system"] = temp[2]
  perf.im[i,"elapsed"] = temp[3]
  perf.im[i,"sum"] = sum(temp[1:3])
  
  temp <- system.time(multilevel.community(tr.graph))
  perf.ml[i,"user"] = temp[1]
  perf.ml[i,"system"] = temp[2]
  perf.ml[i,"elapsed"] = temp[3]
  perf.ml[i,"sum"] = sum(temp[1:3])
  
  # update progress bar
  setTxtProgressBar(pb, i)
}

im <- colMeans(perf.im)
lp <- colMeans(perf.lp)
ml <- colMeans(perf.ml)
wt <- colMeans(perf.wt)

means <- as.data.frame(rbind(im, lp, ml, wt))

save(means, file = "Data/Seminar/PerformanceMeansCDA.RData")
