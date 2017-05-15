
library(igraph)
library(dplyr)

load("~/fimecho/Data/Seminar/Edgelist_no.RData")
load("~/fimecho/Data/Seminar/Tweets.RData")
load("~/fimecho/Data/Seminar/Vertices.RData")


# create IGraph from Edgelist and Vertices
tr.graph <- graph_from_data_frame(edgelist.df, directed = TRUE, vertices = vertices.df)

# convert Graph to undirected Graph 
tr.graph <- as.undirected(tr.graph, "each")

##############################################################################
# CDA undirected
#when you want to see the Processing time- system.time({Function})
ml.comm <- multilevel.community(tr.graph)
wt.comm <- walktrap.community(tr.graph)
im.comm <- infomap.community(tr.graph)
lp.comm <- label.propagation.community(tr.graph)


##############################################################################
#Multilevel CDA

#get size of communities
comm.sizes <- as.data.frame(sizes(ml.comm))

#count number of communities with specific size
nrow(comm.sizes[comm.sizes$Freq > 2742,])
comm.sizes1 <- comm.sizes[comm.sizes$Freq > 2742,]

#get members of all communities
comm.membership <- membership(ml.comm)

communities.df <- as.data.frame(communities(ml.comm))
communities.df$community <- c(1:1009)

# rearrange colums
communities.df <- communities.df[,c(2,1)]
colnames(communities.df) <- c("community", "members")

all.members.mul <- data.frame()

for(i in 1:nrow(communities.df))
{
  row <- communities.df[i, ]
  members <- unlist(row$members)
  
  comm.no <- as.list(1:length(members))
  comm.no[] <- row$community
  
  community <- cbind(comm.no, members)
  
  all.members.mul <- rbind(all.members.mul, community)
  
  rm(row)
  rm(members)
  rm(comm.no)
}

# convert member column to character
all.members.mul$members <- as.character(all.members.mul$members)

# merge community to vertices.df
vertices.df <- merge(vertices.df, all.members.mul, by.x = "Id", by.y = "members", all.x = TRUE)
colnames(vertices.df)[14] <- "ml_comm"
vertices.df$ml_comm <- as.character(vertices.df$ml_comm)

save(vertices.df, file = "Data/Seminar/VerticesComm.RData")


############ SAMPLING
test.vertices <- sample_frac(vertices.df, 0.1)
test.edges <- subset(edgelist.df,
                     (edgelist.df$Source %in% test.vertices$Id) &
                       (edgelist.df$Target %in% test.vertices$Id))

write.csv2(test.vertices, "Data/Knoten.csv")
write.csv2(test.edges, "Data/Kanten.csv")

sample <- graph_from_data_frame(test.edges, directed = TRUE, vertices = test.vertices)
######### SAMPLING


##### Begin of User Language
# Get User Language for Communities
lang <- as.data.frame(table(vertices.df[,"user_lang"]))

for(i in 1:nrow(comm.sizes1))
{
  # count the frequency of languages in this community
  temp <- as.data.frame(table(vertices.df[vertices.df$ml_comm == comm.sizes1[i,"Community.sizes"],
                                    "user_lang"]))
  # change the colnames
  colnames(temp) <- c("Var1", comm.sizes1[i,"Community.sizes"])
  
  # merge it with all languages frame
  lang <- merge(lang, temp, by = "Var1", all.x = TRUE)
}

rm(temp)
  
# plot the frequency of languages for each language in each community
barplot(height = as.matrix(t(y[,3:10])), width = 10, names.arg = y[,"Var1"], 
        xlim = c(0,12000), space = c(5,10),
        xlab = "# of Members", ylab = "Language",
        beside = TRUE, horiz = TRUE, xpd = FALSE)

#### End of User Language



# save membership to file
save(all.members.mul,file="all.members.mul.RData")

# Plot Communities with their # of members
plot(c(1:1009), comm.sizes$Freq, xlab = "MultiLevel_Community", ylab = "# of Members")

# Filtered Plot
plot(c(1:nrow(comm.sizes[comm.sizes$Freq > 1000,])),
     comm.sizes[comm.sizes$Freq > 1000, "Freq"],
     xlab = "ML Community", ylab = "# of Members")

#########################################################################
#walktrap CDA
comm.sizes <- sizes(wt.comm)
comm.sizes <- as.data.frame(comm.sizes)

comm.sizes1 <- comm.sizes[comm.sizes$Freq > 10,]
comm.membership <- membership(wt.comm)

communities.df <- data.frame(communities(wt.comm))
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

# merge community to vertices.df
vertices.df <- merge(vertices.df, all.members.wal, by.x = "Id", by.y = "members", all.x = TRUE)
colnames(vertices.df)[17] <- "wt_comm"
vertices.df$wt_comm <- as.character(vertices.df$wt_comm)

save(vertices.df, file = "Data/Seminar/VerticesComm.RData")

save(all.members.wal,file="all.members.wal.RData")
plot(c(1:1863), comm.sizes$Freq, xlab = "Walktrap_Community", ylab = "# of Members")

#########################################################################
#infomap CDA
comm.sizes <- sizes(im.comm)
comm.sizes <- as.data.frame(comm.sizes)

comm.sizes1 <- comm.sizes[comm.sizes$Freq > 10,]
comm.membership <- membership(im.comm)

communities.df <- data.frame(communities(im.comm))
communities.df$community <- c(1:7164)

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

# merge community to vertices.df
vertices.df <- merge(vertices.df, all.members.inf, by.x = "Id", by.y = "members", all.x = TRUE)
colnames(vertices.df)[16] <- "im_comm"
vertices.df$im_comm <- as.character(vertices.df$im_comm)

save(vertices.df, file = "Data/Seminar/VerticesComm.RData")


save(all.members.inf,file="all.members.inf.RData")
plot(c(1:7240), comm.sizes$Freq, xlab = "infomap_Community", ylab = "# of Members")


#########################################################################
#labelpropagation CDA
comm.sizes <- sizes(lp.comm)
comm.sizes <- as.data.frame(comm.sizes)

comm.sizes1 <- comm.sizes[comm.sizes$Freq > 10,]
comm.membership <- membership(lp.comm)

communities.df <- data.frame(communities(lp.comm))
communities.df$community <- c(1:1970)

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

# merge community to vertices.df
vertices.df <- merge(vertices.df, all.members.lab, by.x = "Id", by.y = "members", all.x = TRUE)
colnames(vertices.df)[15] <- "lp_comm"
vertices.df$lp_comm <- as.character(vertices.df$lp_comm)

save(vertices.df, file = "Data/Seminar/VerticesComm.RData")



save(all.members.lab,file="all.members.lab.RData")
plot(c(1:1965), comm.sizes$Freq, xlab = "Labelpropagation_Community", ylab = "# of Members")



####################################################################################
tr.graph.d <- graph_from_data_frame(edgelist.df, directed = TRUE, vertices = vertices.df)


##############################################################################
# CDA directed
#when you want to see the Processing time- system.time({Function})
#ml.comm.d <- multilevel.community(tr.graph.d)-----not available for directed
wt.comm.d <- walktrap.community(tr.graph.d)
im.comm.d <- infomap.community(tr.graph.d)
lp.comm.d <- label.propagation.community(tr.graph.d)



#########################################################################
#walktrap CDA directed
comm.sizes <- sizes(wt.comm.d)
comm.sizes <- as.data.frame(comm.sizes)

comm.sizes1 <- comm.sizes[comm.sizes$Freq > 10,]
comm.membership <- membership(wt.comm.d)

communities <- communities(wt.comm.d)
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
comm.sizes <- sizes(im.comm.d)
comm.sizes <- as.data.frame(comm.sizes)

comm.sizes1 <- comm.sizes[comm.sizes$Freq > 10,]
comm.membership <- membership(im.comm.d)

communities <- communities(im.comm.d)
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
comm.sizes <- sizes(lp.comm.d)
comm.sizes <- as.data.frame(comm.sizes)

comm.sizes1 <- comm.sizes[comm.sizes$Freq > 10,]
comm.membership <- membership(lp.comm.d)

communities <- communities(lp.comm.d)
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

usr.comm <- vertices.df[,c(1,14,15,16,17)]
save(usr.comm, file = "Data/Seminar/UserCommunities.RData")


