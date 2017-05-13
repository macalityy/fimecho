

library(igraph)
library(dplyr)

graph.edgelist(as.matrix(edgelist.df), directed = TRUE)

table.v <- table(vertices.df$Id)

tr.graph <- graph_from_data_frame(edgelist.df, directed = TRUE, vertices = vertices.df)



tr.graph <- as.undirected(tr.graph, "each")
ml.comm <- multilevel.community(tr.graph)

comm.sizes <- sizes(ml.comm)
comm.sizes <- as.data.frame(comm.sizes)

comm.sizes1 <- comm.sizes[comm.sizes$Freq > 10,]
comm.membership <- membership(ml.comm)

communities <- communities(ml.comm)
communities.df <- data.frame(communities)
communities.df$community <- c(1:9482)

all.members <- data.frame()

for(i in 1:nrow(communities.df)) {
  row <- communities.df[i, ]
  members <- unlist(row$communities)
  
  comm.no <- as.list(1:length(members))
  comm.no[] <- row$community
  
  community <- cbind(comm.no, members)
  
  all.members <- rbind(all.members, community)                     
  
}


plot(c(1:9482), comm.sizes$Freq, xlab = "Community", ylab = "# of Members")

