rm(list=ls())

library(igraph)
library(dplyr)

load("C:/Users/User/Dropbox/FIM_Seminar_Bubble/12_Data/Filtered Data/Edgelist.RData")
load("C:/Users/User/Dropbox/FIM_Seminar_Bubble/12_Data/Filtered Data/Vertices.RData")
load("C:/Users/User/Dropbox/FIM_Seminar_Bubble/12_Data/Filtered Data/Tweets.RData")


tweets_sc_st.df<- as.data.frame(vertices.df[,c(1,2)])
colnames(tweets_sc_st.df)<-c("Source","user_id_Source")
edgelist.df<-merge(edgelist.df, tweets_sc_st.df, by="Source",all.x = TRUE,all.y=FALSE)
colnames(tweets_sc_st.df)<- c("Target","user_id_Target")
edgelist.df<-merge(edgelist.df, tweets_sc_st.df, by="Target",all.x =TRUE, all.y=FALSE)

edgelist.df<-edgelist.df[3:4]
save(edgelist.df, file = "Edgelist_no.RData")

graph.edgelist(as.matrix(edgelist.df), directed = TRUE)

table.v <- table(vertices.df$usr_Id)
table.v<-as.data.frame(table.v)
vertices.df<-vertices.df[,c(2,1,3,4,5,6,7,8,9,10,11,12,13)]

tr.graph <- graph_from_data_frame(edgelist.df, directed = TRUE, vertices = vertices.df)

tr.graph <- as.undirected(tr.graph, "each")

##############################################################################
# CDA undirected
#when you want to see the Processing time- system.time({Function})
mul.comm <- multilevel.community(tr.graph)
wal.comm <- walktrap.community(tr.graph)
inf.comm <- infomap.community(tr.graph)
lab.comm <- label.propagation.community(tr.graph)


##############################################################################
#Multilevel CDA
comm.sizes <- sizes(mul.comm)
comm.sizes <- as.data.frame(comm.sizes)

comm.sizes1 <- comm.sizes[comm.sizes$Freq > 10,]
comm.membership <- membership(mul.comm)

communities <- communities(mul.comm)
communities.df <- data.frame(communities)
communities.df$community <- c(1:1009)

all.members.mul <- data.frame()

for(i in 1:nrow(communities.df))
{
  row <- communities.df[i, ]
  members <- unlist(row$communities)
  
  comm.no <- as.list(1:length(members))
  comm.no[] <- row$community
  
  community <- cbind(comm.no, members)
  
  all.members.mul <- rbind(all.members.mul, community)                     
  
}

save(all.members.mul,file="all.members.mul.RData")
plot(c(1:1009), comm.sizes$Freq, xlab = "MultiLevel_Community", ylab = "# of Members")

#########################################################################
#walktrap CDA
comm.sizes <- sizes(wal.comm)
comm.sizes <- as.data.frame(comm.sizes)

comm.sizes1 <- comm.sizes[comm.sizes$Freq > 10,]
comm.membership <- membership(wal.comm)

communities <- communities(wal.comm)
communities.df <- data.frame(communities)
communities.df$community <- c(1:1863)

all.members.wal <- data.frame()

for(i in 1:nrow(communities.df))
{
  row <- communities.df[i, ]
  members <- unlist(row$communities)
  
  comm.no <- as.list(1:length(members))
  comm.no[] <- row$community
  
  community <- cbind(comm.no, members)
  
  all.members.wal <- rbind(all.members.wal, community)                     
  
}

save(all.members.wal,file="all.members.wal.RData")
plot(c(1:1863), comm.sizes$Freq, xlab = "Walktrap_Community", ylab = "# of Members")

#########################################################################
#infomap CDA
comm.sizes <- sizes(inf.comm)
comm.sizes <- as.data.frame(comm.sizes)

comm.sizes1 <- comm.sizes[comm.sizes$Freq > 10,]
comm.membership <- membership(inf.comm)

communities <- communities(inf.comm)
communities.df <- data.frame(communities)
communities.df$community <- c(1:7240)

all.members.inf <- data.frame()

for(i in 1:nrow(communities.df))
{
  row <- communities.df[i, ]
  members <- unlist(row$communities)
  
  comm.no <- as.list(1:length(members))
  comm.no[] <- row$community
  
  community <- cbind(comm.no, members)
  
  all.members.inf <- rbind(all.members.inf, community)                     
  
}

save(all.members.inf,file="all.members.inf.RData")
plot(c(1:7240), comm.sizes$Freq, xlab = "infomap_Community", ylab = "# of Members")


#########################################################################
#labelpropagation CDA
comm.sizes <- sizes(lab.comm)
comm.sizes <- as.data.frame(comm.sizes)

comm.sizes1 <- comm.sizes[comm.sizes$Freq > 10,]
comm.membership <- membership(lab.comm)

communities <- communities(lab.comm)
communities.df <- data.frame(communities)
communities.df$community <- c(1:1965)

all.members.lab <- data.frame()

for(i in 1:nrow(communities.df))
{
  row <- communities.df[i, ]
  members <- unlist(row$communities)
  
  comm.no <- as.list(1:length(members))
  comm.no[] <- row$community
  
  community <- cbind(comm.no, members)
  
  all.members.lab <- rbind(all.members.lab, community)                     
  
}

save(all.members.lab,file="all.members.lab.RData")
plot(c(1:1965), comm.sizes$Freq, xlab = "Labelpropagation_Community", ylab = "# of Members")



####################################################################################
tr.graph.d <- graph_from_data_frame(edgelist.df, directed = TRUE, vertices = vertices.df)


##############################################################################
# CDA directed
#when you want to see the Processing time- system.time({Function})
#mul.comm.d <- multilevel.community(tr.graph.d)-----not available for directed
wal.comm.d <- walktrap.community(tr.graph.d)
inf.comm.d <- infomap.community(tr.graph.d)
lab.comm.d <- label.propagation.community(tr.graph.d)



#########################################################################
#walktrap CDA directed
comm.sizes <- sizes(wal.comm.d)
comm.sizes <- as.data.frame(comm.sizes)

comm.sizes1 <- comm.sizes[comm.sizes$Freq > 10,]
comm.membership <- membership(wal.comm.d)

communities <- communities(wal.comm.d)
communities.df <- data.frame(communities)
communities.df$community <- c(1:1863)

all.members.wal.d <- data.frame()

for(i in 1:nrow(communities.df))
{
  row <- communities.df[i, ]
  members <- unlist(row$communities)
  
  comm.no <- as.list(1:length(members))
  comm.no[] <- row$community
  
  community <- cbind(comm.no, members)
  
  all.members.wal.d <- rbind(all.members.wal.d, community)                     
  
}

save(all.members.wal.d,file="all.members.wal.d.RData")
plot(c(1:1863), comm.sizes$Freq, xlab = "Walktrap_Community_directed", ylab = "# of Members")

#########################################################################
#infomap CDA
comm.sizes <- sizes(inf.comm.d)
comm.sizes <- as.data.frame(comm.sizes)

comm.sizes1 <- comm.sizes[comm.sizes$Freq > 10,]
comm.membership <- membership(inf.comm.d)

communities <- communities(inf.comm.d)
communities.df <- data.frame(communities)
communities.df$community <- c(1:7240)

all.members.inf.d <- data.frame()

for(i in 1:nrow(communities.df))
{
  row <- communities.df[i, ]
  members <- unlist(row$communities)
  
  comm.no <- as.list(1:length(members))
  comm.no[] <- row$community
  
  community <- cbind(comm.no, members)
  
  all.members.inf.d <- rbind(all.members.inf.d, community)                     
  
}

save(all.members.inf.d,file="all.members.inf.d.RData")
plot(c(1:7240), comm.sizes$Freq, xlab = "infomap_Community.directed", ylab = "# of Members")


#########################################################################
#labelpropagation CDA
comm.sizes <- sizes(lab.comm.d)
comm.sizes <- as.data.frame(comm.sizes)

comm.sizes1 <- comm.sizes[comm.sizes$Freq > 10,]
comm.membership <- membership(lab.comm.d)

communities <- communities(lab.comm.d)
communities.df <- data.frame(communities)
communities.df$community <- c(1:1965)

all.members.lab <- data.frame()

for(i in 1:nrow(communities.df))
{
  row <- communities.df[i, ]
  members <- unlist(row$communities)
  
  comm.no <- as.list(1:length(members))
  comm.no[] <- row$community
  
  community <- cbind(comm.no, members)
  
  all.members.lab.d <- rbind(all.members.lab.d, community)                     
  
}

save(all.members.lab.d,file="all.members.lab.d.RData")
plot(c(1:1965), comm.sizes$Freq, xlab = "Labelpropagation_Community_directed", ylab = "# of Members")



##################################################################################
#User_MUL_WAL_LAB_INF Tabelle machen 

load("C:/Users/User/Dropbox/FIM_Seminar_Bubble/20_Content/Community Detection/all.members.inf.RData")
load("C:/Users/User/Dropbox/FIM_Seminar_Bubble/20_Content/Community Detection/all.members.wal.RData")
load("C:/Users/User/Dropbox/FIM_Seminar_Bubble/20_Content/Community Detection/all.members.lab.RData")
load("C:/Users/User/Dropbox/FIM_Seminar_Bubble/20_Content/Community Detection/all.members.mul.RData")
 
allvertices<-tweets_sc_st.df
colnames(allvertices)<-c("username","user_id_string")
colnames(all.members.inf)<-c("comm.no.inf","user_id_string")
colnames(all.members.lab)<-c("comm.no.lab","user_id_string")
colnames(all.members.mul)<-c("comm.no.mul","user_id_string")
colnames(all.members.wal)<-c("comm.no.wal","user_id_string")


allvertices<-merge(allvertices,all.members.inf,by= "user_id_string", all.x=TRUE, all.y = FALSE)
allvertices<-merge(allvertices,all.members.lab,by= "user_id_string", all.x=TRUE, all.y = FALSE)
allvertices<-merge(allvertices,all.members.mul,by= "user_id_string", all.x=TRUE, all.y = FALSE)
allvertices<-merge(allvertices,all.members.wal,by= "user_id_string", all.x=TRUE, all.y = FALSE)


