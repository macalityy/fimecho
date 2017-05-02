#load librariers
library(igraph)

#get arguments from cmd line
args <- commandArgs(trailingOnly = TRUE)

#get path to RData Graph
file <- unlist(args[1])

#load rt_graph from file
load(file)

# get vertex names
vertex.labels = get.vertex.attribute(rt.graph, "name", index=V(rt.graph))

# choose layout for plot
plot.layout = layout.fruchterman.reingold(rt.graph)

# plot the igraph object
par(bg="gray15", mar=c(1,1,1,1))
plot(rt.graph, layout=plot.layout,
     vertex.color="gray25",
     vertex.size=2,
     vertex.label = vertex.labels,
     vertex.label.family="sans",
     vertex.shape="none",
     vertex.label.color=hsv(h=0, s=0, v=.95, alpha=0.5),
     vertex.label.cex=0.85,
     edge.arrow.size=0.05,
     edge.arrow.width=0.1,
     edge.width=0.1,
     edge.color=hsv(h=.95, s=1, v=.7, alpha=0.5))
