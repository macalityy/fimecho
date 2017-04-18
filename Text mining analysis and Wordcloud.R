#Turkeyreferendum start #setting workspace and reading a file
setwd("c:/users/user/Desktop/rdata")
Turkey<-read.csv2("#hayir#turkeyreferendum#referendum#Turkey#turkeyschoice#evet#EVETveEsbabiHakiki_2017_04_14_09_00.csv",header = TRUE, sep = ";", stringsAsFactors = FALSE)

#installing text mining packages
install.packages("tm")
install.packages("dplyr")
library(dplyr)
library(tm)

#view not all data, only constrainted data set
dim(Turkey)
Turkey_df <- tbl_df(Turkey)
Turkey_df

#Filter only lang=englisch
Turkey_en<-filter(Turkey_df,Turkey$lang == "en")

#use textmining Package
corp=Corpus(VectorSource(Turkey_en$text))
corp
inspect(corp[1:5])

#TermDocumentMatirx 
tdm<-TermDocumentMatrix(corp);tdm
m=as.matrix(tdm);m

#making Matirx with frequently emerging words 
corp2<-TermDocumentMatrix(corp, control=list(wordLengths=c(2,Inf)))
corp2

#data cleaning
corp2<-tm_map(corp,stripWhitespace)
corp2<-tm_map(corp2,tolower)#converting upper case(capital alphabet) into lower case 
corp2<-tm_map(corp2,removeNumbers)#removing numbers
corp2<-tm_map(corp2,removePunctuation)#removing punctuation
sword2<-c(stopwords('en'),"and","but","not","the")
sword2
corp2<-tm_map(corp2,removeWords,sword2)
corp2<-tm_map(corp2,PlainTextDocument)#selecting only plain text
inspect(corp2[1:2])
corp2

#TermDocumentMatrix again
tdm<-TermDocumentMatrix(corp2);tdm
m<-as.matrix(tdm);m

#defining colnames
colnames(m)<-c(1:2)#(4 is the number of documents, so you can apply your own data differently)
m

#finding Frequency of words
findFreqTerms(tdm,2) #the words, that emerge more than 2times 
sort(rowSums(m),decreasing = T)
sort(colSums(m), decreasing = T)

#(finding Correlationship between words)
findAssocs(tdm,"apple",0.5)

#igraph
m ;t(m)
adjmatrix<-m %*% t(m)
adjmatrix
library(igraph)
g1<-graph.adjacency(adjmatrix, weighted = T, mode= "undirected")
g1
plot(g1)
#removing loops
g2<-simplify(g1)
plot(g2)
#schoener machen
degree(g2)
V(g2)$degree<-degree(g2)
V(g2)$label.cex<-3*(V(g2)$degree / max(V(g2)$degree))
V(g2)$size<-10*(V(g2)$degree / max(V(g2)$degree))
E(g2)$width<-2*(E(g2)$weight / max(E(g2)$weight))
plot(g2)
#removing Vertices 
g3 <- delete.vertices(g2,V(g2)[degree(g2)<=20])
plot(g3)

####################################################
#wordcloud 

#matrix-> data.frame converting
m.df<-as.data.frame(m)
m.df
wordresult<-sort(rowSums(m.df),decreasing=TRUE)
wordresult<-wordresult[1:30]

install.packages("wordcloud")
library(wordcloud)

word<-names(wordresult)
wordcloud(word,wordresult)
