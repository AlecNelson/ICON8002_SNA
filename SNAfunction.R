sna <-
function(input_datapath, vertex_datapath, edge_datapath){
# Read in dataframes 
vertex<- read.csv(vertex_datapath,header=T, row.names = 1)
edge<- read.csv(edge_datapath,header=T, row.names = 1)

# Check for discrepancies in data
str(vertex) # look at structure of data
summary(vertex) # summary of data with variables and variable attributes
head(vertex) # look at the first n rows of data
colnames(vertex) #look at column names

str(edge)
summary(edge)
head(edge)
colnames(edge)


# Before we merge these data, we need to make sure 'ego' and 'alter' are the
# same across data sets. We can compare each row using the == syntax. 
# The command below should return TRUE for every row if all ego rows
# are the same :
unique(sort(vertex$ego)) == unique(sort(edge$ego))

# We can just have R return which row entries are not equal using the syntax below:
which(unique(sort(vertex$ego)) != unique(sort(edge$ego)))

##### Edge and vertex dataframes contain different sets of names #####

#### Isn't it possible that egos and alters lists are not the same between datasets? #####

###########################################
############ Graphing network #############
###########################################

network <- graph.data.frame(d = edge, vertices = vertex)
summary(network)
in.degree<-degree(network,mode="in")
# Network graph output in pdf
pdf("Social Network Output Graph.pdf", width=8, height=8)
par(mai=c(1,1,1,1))
plot(network,
     edge.arrow.size=.5,
     vertex.color=vertex$profession,
     vertex.size=((in.degree)*1.5),
     vertex.label=NA,
     vertex.label.cex=0.7,
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main='Test Data Connections (color by profession)',
     margin=0.0001)
dev.off()

}
