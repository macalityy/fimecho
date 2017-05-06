#read edgelist 
edge<-read.csv2("TurkeyFilterGephi.csv",header=TRUE, sep=";", stringsAsFactors = FALSE)

library(igraph)
#select only column 2 and 3
edge_list<-edge[2:3]
adjacency.direct<-get.adjacency(graph.edgelist(as.matrix(edge_list),directed=TRUE))


#######################################################################################################
#Community Detection, 4 Methods 

#undirected 
turkey.graph<-graph.adjacency(adjacency.direct,weighted=NULL,mode="undirected")
turkey.wtc <- walktrap.community(turkey.graph,step=6,modularity=TRUE,membership = TRUE)
turkey.mul <- multilevel.community(turkey.graph)
turkey.inf <- infomap.community(turkey.graph, nb.trials = 10, modularity = TRUE)
turkey.lab <- label.propagation.community(turkey.graph, weights = NULL, initial = NULL, fixed = NULL)

#directed only 2 methods(walktrap and infomap)
turkey.graph.d<-graph.adjacency(adjacency.direct,weighted=NULL,mode="directed")
turkey.wtc.d <- walktrap.community(turkey.graph.d,step=10,modularity=TRUE,membership = TRUE)
#turkey.mul.d <- multilevel.community(turkey.graph.d) why not? impossible to directed graph, only for undirected graph
turkey.inf.d <- infomap.community(turkey.graph.d)
#turkey.lab.d <- label.propagation.community(turkey.graph.d) Why not? error, it takes so long to run. 

#####################################################################################################
#making Dataframe based on Group / Subsetting Dataset based on Frequency 

m.wtc <-data.frame(print(membership(turkey.wtc)))
m.mul <-data.frame(print(membership(turkey.mul)))
m.inf <-data.frame(print(membership(turkey.inf)))
m.lab <-data.frame(print(membership(turkey.lab)))

m.wtc.d <-data.frame(print(membership(turkey.wtc.d)))
m.inf.d <-data.frame(print(membership(turkey.inf.d)))



c.wtc <-communities(turkey.wtc)
c.mul <-communities(turkey.mul)
c.inf <-communities(turkey.inf)
c.lab <-communities(turkey.lab)

c.wtc.d <-communities(turkey.wtc.d)
c.inf.d <-communities(turkey.inf.d)



wtc.comm <-data.frame(c.wtc)
mul.comm <-data.frame(c.mul)
inf.comm <-data.frame(c.inf)
lab.comm <-data.frame(c.lab)

wtc.comm.d <-data.frame(c.wtc.d)
inf.comm.d <-data.frame(c.inf.d)

#Subsetting Dataset based on Frequency 
#Example of walktrap community detection undirected. we can apply step by step all the other methods results. 

library(plyr)
library(dplyr)

colnames(m.wtc)[1]<-"community"
count(m.wtc,community)
a<-subset(m.wtc, ave(community, community, FUN = length) >50)
count(a,community)
arrange(a,desc(community))
