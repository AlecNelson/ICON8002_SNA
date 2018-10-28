#Key Data Processing Functions for ICON 8002
# 10/27/18

############################
#Type in the base directory and input datapaths below
basedirectory <- 
input_datapath <- 

vertex_datapath <- "vertex_test_df.csv"
edge_indiv_datapath <- "edge_indiv_test_df.csv"
edge_org_datapath <- "edge_org_test_df.csv"

setwd(input_datapath)

#List packages used
list.of.packages <- c("igraph","randomNames","fabricatr","plyr","RColorBrewer","keyplayer","sna","mixedsort")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages)} 

#Load all packages
lapply(list.of.packages, require, character.only = TRUE)
########################################################
########################################################

## input data structure
## 2 data sheets (csv files), 1 for vertices, 1 for connections/edges
## attributes can be numeric and characters, continuous and categorical

# The attribute data for this script is in a comma-separated-value
# (CSV) file. read.csv() loads a CSV file into a data frame
# object. In this case, we do have a header row, so we set
# header=T, which tells R that the first row of data contains
# column names.

vertex_df <- read.csv(vertex_datapath, header=T, row.names = 1)
edge_indiv_df <- read.csv(edge_indiv_datapath, header=T, row.names = 1)
edge_org_df <- read.csv(edge_org_datapath, header=T, row.names = 1)

########################################################
# Before we merge these data, we need to make sure 'ego' and 'alter' are the
# same across data sets. We can compare each row using the == syntax. 
# The command below should return TRUE for every row if all ego rows
# are the same :
unique(as.character(vertex_df$ego)) == unique(c(as.character(edge_indiv_df$ego),as.character(edge_indiv_df$alter)))
########################################################

#Combine edge and vertex attribute information into igraph format
graph_complete <- graph.data.frame(d = edge_indiv_df, vertices = vertex_df)
#Symmetrize graph to remove directed edges
graph_complete_symmetrized <- as.undirected(graph_complete, mode='collapse')
# Simplify graph to remove loops and prepare variables
graph_complete_simpl <- simplify(graph_complete)
graph_complete_simpl_symm <- as.undirected(graph_complete_simpl, mode='collapse')
in.degree<-igraph::degree(graph_complete,mode="in")

summary(graph_complete)

#V(graph_complete)$profession.df
# igraph::get.vertex.attribute(graph_complete,'profession.df')
# unique(igraph::get.vertex.attribute(graph_complete,'profession.df'))

########################################################
# The write.graph() function exports a graph object in various
# formats readable by other programs. 

#setwd(basedirectory)
#write.graph(graph_complete, file='graph_sna_full.dl', format="pajek")

# For a more general file type (e.g., importable to Excel),
# use the "edgelist" format. Note that neither of these will
# write the attributes; only the ties are maintained.

#setwd(basedirectory)
#write.graph(graph_complete, file='graph_sna_full.txt', format="edgelist")

########################################################
########################################################
### Plotting functions
########################################################
########################################################

### Plot network with no alterations
### Fruchterman.reingold fault layout, simplified (i.e., no loops), directed
setwd(basedirectory)
pdf("SNA_Output_Original.pdf")
plot.network.original <- plot(graph_complete_simpl,
    layout = layout.fruchterman.reingold,
     #edge.color=edge_test$connection,
     edge.arrow.size=.1,
     #vertex.color=profession.colors,
     #vertex.size=((in.degree)*1.5),
     vertex.size=5,
     vertex.label=NA,
     vertex.label.cex=0.7,
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main="Network Plot (Generated Data)",
     #frame=TRUE,
     margin=0.0001)
dev.off()

### Plot network with no alterations
### Nicely layout, simplified (i.e., no loops), directed
setwd(basedirectory)
pdf("SNA_Output_Oringinal_Nicely.pdf")
plot(graph_complete_simpl,
     #edge.color=edge_test$connection,
     edge.arrow.size=.1,
     #vertex.color=profession.colors,
     #vertex.size=((in.degree)*1.5),
     vertex.size=5,
     vertex.label=NA,
     vertex.label.cex=0.7,
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main="Network Plot_Nicely",
     #frame=TRUE,
     margin=0.0001,
     layout = layout_nicely)
dev.off()

### Plot network by Profession (colors)
### Nicely layout, simplified (i.e., no loops), directed, vertices colored by profession
layout.graph <- layout_(graph_complete_simpl, nicely())
layout.graph<-norm_coords(layout.graph, ymin=-1, ymax=1, xmin=-1, xmax=1)

## Playing around with colors
colr.palette <- brewer.pal(n = 9, name = "Spectral")
profession.colors <- colr.palette[vertex_df$profession.df]

setwd(basedirectory)
pdf("SNA_Output_Professions.pdf")
plot(graph_complete_simpl,
     layout = layout.graph,
     rescale = F,
     #edge.color=edge_test$connection,
     edge.arrow.size=.1,
     vertex.color=profession.colors,
     #vertex.size=((in.degree)*1.5),
     vertex.size=5,
     vertex.label=NA,
     vertex.label.cex=0.5,
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main="SNA by Profession",
     #frame=TRUE,
     margin = 0.0001)
legend(x=-1.5, y = -0.85, legend = levels(vertex_df$profession), 
       col = profession.colors, pch=19, pt.cex=0.8, cex=0.8, bty="n", ncol=1)
dev.off()

########################################################
#### NOTE: Need to figure out colors between vertices and legend. Don't currently match - BB 10.18.18 
########################################################

V(graph_complete_symmetrized)$community <- vertex_df$profession.df
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen","blue","pink","green","purple"), alpha=.6)
plot(graph_complete_symmetrized, vertex.color=colrs[V(graph_complete_symmetrized)$community], vertex.label=vertex_df$profession.df)

V(graph_complete_symmetrized)$community <- V(graph_complete)$profession.df
colrs <- adjustcolor( c("red", "tomato", "gold", "yellowgreen","blue","pink","green","purple","grey50"), alpha=.6)
plot(graph_complete_symmetrized, vertex.color=colrs[V(graph_complete_symmetrized)$community])

### Plotting by community/profession with vertices weighted by in.degree
V(graph_complete_symmetrized)$community <- vertex_df$profession.df
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen","blue","pink","green","purple"), alpha=.6)

setwd(basedirectory)
pdf("SNA_Output_Community_Professions_InDegree.pdf")
plot(graph_complete_symmetrized,
     #edge.color=edge_test$connection,
     edge.arrow.size=.5,
     vertex.color=colrs[V(graph_complete_symmetrized)$community],
     vertex.size=((in.degree)*1.5),
     vertex.color=colrs,
     #vertex.size=((in.degree)*1.5),
     vertex.size=3,
     #vertex.label=vertex_df$profession.df,
     vertex.label=NA,
     vertex.label.cex=0.7,
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main='Test Data Connections (color by profession/community)',
     #frame=TRUE,
     margin=0.0001)

legend(x=-1.5, y = -0.85, unique(vertex_df$profession), pch=19,
       col= colrs[V(graph_complete_symmetrized)$community], pt.cex=0.8, cex=0.8, bty="n", ncol=1)

dev.off()

##### Plot of Vertices weighted by total degree
setwd(basedirectory)
pdf("SNA_Output_Professions_Vertex_TotalDegree.pdf")
plot(graph_complete_simpl,
     layout = layout.graph,
     rescale = F,
     #edge.color=edge_test$connection,
     edge.arrow.size=.1,
     #vertex.color=vertex.test.df$profession.df,
     #vertex.size=((in.degree)*1.5),
     vertex.size=igraph::degree(graph_complete_simpl),
     vertex.label=NA,
     vertex.label.cex=0.5,
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main="SNA Vertices Weighted by Degree",
     #frame=TRUE,
     margin = 0.0001)
legend(x=-1.5, y = -0.85, unique(vertex_df$profession), pch=19,
       col= categorical_pal(9), pt.cex=0.8, cex=0.8, bty="n", ncol=1)
dev.off()


########################################################
###### NOTE: Check legend. Legend colors do not match profession colors in network. Can check by adding vertex labels. 
########################################################

cut.off <- round(mean(edge_indiv_df$q3.years.worked.with.eiq))
graph_complete.years <- delete_edges(graph_complete, E(graph_complete)[q3.years.worked.with.eiq<cut.off])

layout.graph <- layout_(graph_complete.years, nicely())
layout.graph<-norm_coords(layout.graph, ymin=-1, ymax=1, xmin=-1, xmax=1)

setwd(basedirectory)
pdf("SNA_Output_work_years.pdf")
plot(graph_complete.years,
     layout=(layout.graph*1.1),
     rescale=F, 
     #edge.color=edge_test$connection,
     edge.arrow.size=.01,
     vertex.color=vertex_df$profession,
     #vertex.size=((in.degree)*0.9),
     vertex.size=3,
     #vertex.label=vertex_df$profession.df,
     vertex.label=NA,
     vertex.label.cex=0.6,
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main=paste0('Network of worked-together-with ',cut.off,' years connection'),
     #frame=TRUE,
     margin=0.0001)
dev.off()

#Clustering function to add weights to edges with shared profession
G_Grouped = graph_complete_symmetrized
E(G_Grouped)$weight = 1
professions.list<-unique(V(G_Grouped)$profession.df)
## Add edges with high weight between all nodes in the same group
for(i in 1:length(professions.list)) {
  GroupV = which(V(G_Grouped)$profession.df == professions.list[i])
  G_Grouped = add_edges(G_Grouped, combn(GroupV, 2), attr=list(weight=1.5))
  #print(paste0("Ran loop for profession ",professions.list[i]))
} 

## Now create a layout based on G_Grouped
LO = layout_with_fr(G_Grouped)
LO<-norm_coords(LO, ymin=-1, ymax=1, xmin=-1, xmax=1)

setwd(basedirectory)
pdf("SNA_Output_group_layout.pdf")
plot(graph_complete_symmetrized,
     layout=(LO*1.0),
     rescale=F, 
     #edge.color=edge_test$connection,
     edge.arrow.size=.5,
     vertex.color=vertex_df$profession.df,
     #vertex.size=((in.degree)*0.7),
     vertex.size=3,
     #vertex.label=vertex_df$profession.df,
     vertex.label=NA,
     vertex.label.cex=0.7,
     vertex.label.color= adjustcolor("black", 0.6),
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main='Test Data Connections (color by profession)',
     #frame=TRUE,
     margin=0.0001)
dev.off()

#Add graph of strong/weak connections between professional groups
V(graph_complete_symmetrized)$profession.df
E(graph_complete_symmetrized)$profession.df

strength(graph_complete_symmetrized)
graph_attr(graph_complete_symmetrized)

E(graph_complete)[inc(V(graph_complete)[profession.df==professions.list[1]])]
g2 <- subgraph.edges(graph_complete, E(graph_complete)[inc(V(graph_complete)[profession.df==professions.list[1]])])

plot(g2,
     layout=layout_in_circle,
     rescale=T, 
     edge.color=adjustcolor("black", 0.1),
     edge.arrow.size=0.1,
     vertex.color=vertex_df$profession.df,
     #vertex.size=((in.degree)*0.7),
     vertex.size=3,
     #vertex.label=vertex_df$profession.df,
     vertex.label=NA,
     vertex.label.cex=0.7,
     vertex.label.color= adjustcolor("black", 0.5),
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main='Test Data Connections (color by profession)',
     #frame=TRUE,
     margin=0.0001)

########################################################
########################################################
### Network Metrics and Keyplayer functions
########################################################
########################################################

degree_complete<-igraph::degree(graph_complete)
degree_max<-which.max(igraph::degree(graph_complete))
degree_min<-which.min(igraph::degree(graph_complete))

diameter_complete<-diameter(graph_complete)
closeness_complete<-igraph::closeness(graph_complete)
reciprocity_complete<-reciprocity(graph_complete)
ecount_complete<-ecount(graph_complete)
vcount_complete<-vcount(graph_complete)
density_complete<-edge_density(graph_complete)

########################################################
###Create nice table output of these statistics - AHN 10/27 #########
########################################################

# Reachability can only be computed on one vertex at a time. To
# get graph-wide statistics, change the value of "vertex"
# manually or write a for loop. (Remember that, unlike R objects,
# igraph objects are numbered from 0.)

reachability <- function(g, m) {
  reach_mat = matrix(nrow = vcount(g), ncol = vcount(g))
  for (i in 1:vcount(g)) {
    reach_mat[i,] = 0
    this_node_reach <- subcomponent(g, (i - 1), mode = m)
    for (j in 1:(length(this_node_reach))) {
      alter = this_node_reach[j] + 1
      reach_mat[i, alter] = 1}}
  return(reach_mat)
}

#reach_full_in <- reachability(graph_complete_symmetrized, 'in')
#reach_full_out <- reachability(graph_complete, 'out')

########################################################
#Keyplayer functions:
#Convert igraph object into matrix object, which can be read by sna package
matrix_complete<-as.matrix(as_adjacency_matrix(graph_complete))
matrix_inv_non_zero <- matrix_complete
# Inverse the non-zero tie status
matrix_inv_non_zero[matrix_complete != 0] <- 1 / matrix_complete[matrix_complete != 0] 
# Symmetrized version of matrix
matrix_complete_symm <- symmetrize(matrix_complete)
#Setting all zero ties to 1
matrix_non_zero_one <- matrix_complete
matrix_non_zero_one[matrix_complete != 0] <- 1


evcent(matrix_inv_non_zero, gmode = "digraph", ignore.eval = FALSE, use.eigen = TRUE)
evcent(matrix_complete_symm)
#Fragmentation centrality measures the extent to which a network is fragmented after a node is removed from the network 
#fragment(matrix_inv_non_zero)

mreach.degree(matrix_complete, M = 1)
which.max(mreach.degree(matrix_complete, M = 1)[,3])[1]
mreach.closeness(matrix_inv_non_zero)
which.max(mreach.closeness(matrix_inv_non_zero)[,3])[1]

#Determine Keyplayers via different statistical mesurements
## Set size of set group to number of keyplayers wanted per metric
kpset(matrix_complete, size = 1, type = "degree", cmode = "indegree", method = "max")
kpset(matrix_complete, size = 1, type = "degree", cmode = "indegree", binary = TRUE, method = "max") 
kpset(matrix_complete, size = 1, type = "mreach.degree", cmode = "indegree", M = 1,binary = TRUE)
kpset(matrix_inv_non_zero, size = 1, type = "mreach.closeness", cmode = "indegree", M = 1)
kpset(matrix_complete, size = 1, type = "degree", cmode = "indegree", parallel = TRUE,cluster = 2)

kpset(matrix_complete, size = 1, type = "degree", cmode = "outdegree", method = "max")
kpset(matrix_complete, size = 1, type = "degree", cmode = "outdegree", binary = TRUE, method = "max")
kpset(matrix_complete, size = 1, type = "mreach.degree", cmode = "outdegree", M = 1,binary = TRUE)
kpset(matrix_inv_non_zero, size = 1, type = "mreach.closeness", cmode = "outdegree", M = 1)
kpset(matrix_complete, size = 1, type = "degree", cmode = "outdegree", parallel = TRUE,cluster = 2)

kpset(matrix_complete, size = 1, type = "degree", cmode = "total", method = "max")
kpset(matrix_complete, size = 1, type = "degree", cmode = "total", binary = TRUE, method = "max")
kpset(matrix_complete, size = 1, type = "mreach.degree", cmode = "total", M = 1,binary = TRUE)
kpset(matrix_inv_non_zero, size = 1, type = "mreach.closeness", cmode = "total", M = 1)
kpset(matrix_complete, size = 1, type = "degree", cmode = "total", parallel = TRUE,cluster = 2)

rownames(matrix_complete)[177] #from second model in first group above. 

#Determine Centrality between specific nodes
# kpcent(matrix_complete, c(2, 18), type = "degree", cmode = "total", method = "max")
# kpcent(matrix_complete, c(2, 67), type = "degree", cmode = "total", method = "min")
# kpcent(matrix_complete, c(2, 3), type = "degree", cmode = "total", method = "min", binary = TRUE)
# kpcent(matrix_complete, c(2, 3), type = "mreach.degree", cmode = "total", M = 1, binary = TRUE)
# kpcent(W, c(2, 3), type = "mreach.closeness", cmode = "total", M = 1, binary = TRUE)


##### Plotting network with key player as identified by above model.
layout.graph <- layout_(graph_complete_simpl, nicely())
layout.graph<-norm_coords(layout.graph, ymin=-1, ymax=1, xmin=-1, xmax=1)
setwd(basedirectory)
pdf("SNA_Output_KeyPlayer.pdf")
plot(graph_complete_simpl,
     layout=layout.graph,
     rescale=T,
     #edge.color=edge_test$connection,
     edge.arrow.size=.1,
     vertex.color=ifelse(vertex_df$ego == as.character(vertex_df$ego[177]), "Firebrick1", "Gray60"),
     #vertex.size=((in.degree)*1.5),
     #vertex.size=(igraph::degree(graph_complete)*0.5),
     vertex.size=ifelse(vertex_df$ego == as.character(vertex_df$ego[177]), 8, 4),
     vertex.label= ifelse(vertex_df$ego == as.character(vertex_df$ego[177]), as.character(vertex_df$ego), NA),
     #label.color = "Firebrick1",
     vertex.label=NA,
     vertex.label.cex=ifelse(vertex_df$ego == as.character(vertex_df$ego[177]), .5, NA),
     vertex.label.dist=0,
     vertex.label.degree=0,
     main='SNA with Key Player',
     #frame=TRUE,
     margin=0.0001)

legend(x=-1.5, y = -0.85, "Key Player: Calvin Barnes", pch=19,
       col= "Firebrick1", pt.cex=0.8, cex=0.8, bty="n", ncol=1)

dev.off()

## Need to figure out how to separate/spread overlapping vertices so that key players show up. 
## Also need to figure out how to add multiple key players

#Display information as matrix format
#graph_complete[]
matrix_complete<-as.matrix(as_adjacency_matrix(graph_complete))

Keyplayer.list<-c(2,4,5,9,14,16,17,18,28,63,67)

data_logistic_df<-vertex_df

Keyplay.bool=ifelse(rownames(data_logistic_df) %in% Keyplayer.list,1,0)
data_logistic_df<-cbind(Keyplay.bool,data_logistic_df)
colnames(data_logistic_df)

data_logistic_df <- subset(data_logistic_df,
                             select=c(1,3,10,11,12,13,14,15))

model <- glm(Keyplay.bool ~.,family=binomial(link='logit'),data=data_logistic_df)
summary(model)

#################################################################
#################################################################

