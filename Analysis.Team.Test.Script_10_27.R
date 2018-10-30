#Key Data Processing Functions for ICON 8002
# 10/29/18

############################
#Type in the base directory and input datapaths below
basedirectory <- "C:\\Users\\ahn11803\\Documents\\GitHub\\ICON8002_SNA"
input_datapath <- "C:\\Users\\ahn11803\\Documents\\GitHub\\ICON8002_SNA\\Data"

vertex_datapath <- "vertex_test_df_10_28_18.csv"
edge_indiv_datapath <- "edge_indiv_test_df_10_28_18.csv"
edge_org_datapath <- "edge_org_test_df.csv"

setwd(input_datapath)

#List packages used
list.of.packages <- c("igraph","randomNames","fabricatr","plyr","RColorBrewer","keyplayer","sna","MASS","naturalsort")

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
sort(unique(as.character(vertex_df$ego))) == sort(unique(c(as.character(edge_indiv_df$ego),as.character(edge_indiv_df$alter))))
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

# V(graph_complete_symmetrized)$community <- vertex_df$profession.df
# colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen","blue","pink","green","purple"), alpha=.6)
# plot(graph_complete_symmetrized, vertex.color=colrs[V(graph_complete_symmetrized)$community], vertex.label=vertex_df$profession.df)
# 
# V(graph_complete_symmetrized)$community <- V(graph_complete)$profession.df
# colrs <- adjustcolor( c("red", "tomato", "gold", "yellowgreen","blue","pink","green","purple","grey50"), alpha=.6)
# plot(graph_complete_symmetrized, vertex.color=colrs[V(graph_complete_symmetrized)$community])

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

#Graph-wide statistics
Diameter_stat_complete<-diameter(graph_complete)
print(paste0("The network has a diameter of ",Diameter_stat_complete,". This measure is the length of the longest geodesic (the largest distance between any two vertices in a connected graph)."))

Reciprocity_stat_complete<-reciprocity(graph_complete)
print(paste0("The network has a reciprocity of ",Reciprocity_stat_complete,". The measure of reciprocity defines the proportion of mutual connections, in a directed graph."))

Density_stat_complete<-edge_density(graph_complete)
print(paste0("The network has a density of ",Density_stat_complete,". This measure is the ratio of the number of edges and the number of possible edges."))

Transitivity_stat_complete<-transitivity(graph_complete)
print(paste0("The network has a transitivity of ",Transitivity_stat_complete,". This measure is the probability that the adjacent vertices of a vertex are connected. This is sometimes also called the clustering coefficient."))

# ecount_complete<-ecount(graph_complete)
# vcount_complete<-vcount(graph_complete)
#Shortest path between nodes
#sp_full_in <- shortest.paths(graph_complete, mode='in')
#sp_full_out <- shortest.paths(graph_complete, mode='out')

stats_overall_graph<-ls(pattern = "_stat_complete")
stat_overall_names<-vector()

for(k in 1:length(stats_overall_graph)){
  stat_overall_names<-c(stat_overall_names,strsplit(stats_overall_graph[k],"_")[[1]][1])
}

Stat_overall_table<-as.data.frame(cbind(as.vector(stat_overall_names),as.vector(mget(stats_overall_graph))),row.names = FALSE)
names(Stat_overall_table)<-c("Statistic Name","Value")

#Individual statistics
#degree_complete<-igraph::degree(graph_complete)
Degree_max_stat_indiv<-which.max(igraph::degree(graph_complete))
print(paste0("The vertex with the greatest degree is ",as.character(vertex_df$ego[Degree_max_stat_indiv])," (number: ",Degree_max_stat_indiv,"). This measure is the number of its adjacent edges."))

Degree_min_stat_indiv<-which.min(igraph::degree(graph_complete))
print(paste0("The vertex with the fewest degree is ",as.character(vertex_df$ego[Degree_min_stat_indiv])," (number: ",Degree_min_stat_indiv,"). This measure is the number of its adjacent edges."))

#closeness_complete<-igraph::closeness(graph_complete)
Closeness_max_stat_indiv<-which.max(igraph::closeness(graph_complete))
print(paste0("The vertex with the greatest closeness is ",as.character(vertex_df$ego[Closeness_max_stat_indiv])," (number: ",Closeness_max_stat_indiv,"). This measures how many steps is required to access every other vertex from a given vertex."))

Closeness_min_stat_indiv<-which.min(igraph::closeness(graph_complete))
print(paste0("The vertex with the least closeness is ",as.character(vertex_df$ego[Closeness_min_stat_indiv])," (number: ",Closeness_min_stat_indiv,"). This measures how many steps is required to access every other vertex from a given vertex."))

stats_indiv_graph<-ls(pattern = "_stat_indiv")
stat_indiv_names<-vector()

for(k in 1:length(stats_indiv_graph)){
  stat_indiv_names<-c(stat_indiv_names,substr(stats_indiv_graph[k],1,(nchar(stats_indiv_graph[k])-11)))}

indiv_vals<-as.numeric(as.vector(mget(stats_indiv_graph)))

stat_indiv_vertices<-vector()
for(j in 1:length(indiv_vals)){
  stat_indiv_vertices<-c(stat_indiv_vertices,as.character(vertex_df$ego[indiv_vals[j]]))}

Stat_indiv_table<-as.data.frame(cbind(as.vector(stat_indiv_names),stat_indiv_vertices,indiv_vals),row.names = FALSE)
names(Stat_indiv_table)<-c("Statistic Name","Vertex Name","Value")


# Reachability can only be computed on one vertex at a time. To
# get graph-wide statistics, change the value of "vertex"
# manually or write a for loop. (Remember that, unlike R objects,
# igraph objects are numbered from 0.)

# reachability <- function(g, m) {
#   reach_mat = matrix(nrow = vcount(g),ncol = vcount(g))
#   for (i in 1:vcount(g)) {
#     reach_mat[i,] = 0
#     this_node_reach <- subcomponent(g, (i), mode = m)
#     for (j in 1:(length(this_node_reach))) {
#       alter = this_node_reach[j]
#       reach_mat[i, alter] = 1}}
#   return(reach_mat)
# }

# reach_full_in <- reachability(graph_complete, 'in')
# reach_full_out <- reachability(graph_complete, 'out')

# for(i in 1:length(V(graph_complete))){
#   print(length(subcomponent(graph_edge, i, mode = "in")))
# }

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

# mreach.degree(matrix_complete, M = 1)
# which.max(mreach.degree(matrix_complete, M = 1)[,3])[1]
# mreach.closeness(matrix_inv_non_zero)
# which.max(mreach.closeness(matrix_inv_non_zero)[,3])[1]

#Determine Keyplayers via different statistical mesurements
## Set size of set group to number of keyplayers wanted per metric
keyplayer_num<-3

##################
# Start the clock!
ptm <- proc.time()
##################

kp_closeness<-kpset(matrix_complete,size=keyplayer_num,type="closeness",parallel=TRUE,cluster=2,method="min")
kp_betweenness<-kpset(matrix_complete,size=keyplayer_num,type="betweenness",parallel=TRUE,cluster=2,method="min")
kp_degree<-kpset(matrix_complete,size=keyplayer_num,type="degree",cmode="total",parallel=TRUE,cluster=2,method="max")
kp_eigenvector<-kpset(matrix_complete,size=keyplayer_num,type="evcent",parallel=TRUE,cluster=2,method="max")

##################
#Check the time elapsed
proc.time() - ptm
##################

rownames(matrix_complete)[kp_closeness$keyplayers[1:keyplayer_num]] 


########################################################
###### NOTE: Edge values can be included in this calculation in the "binary" option 
########################################################

# kp_in_degree_max<-kpset(matrix_complete, size = keyplayer_num, type = "degree", cmode = "indegree", method = "max")
# kp_in_degree_max_bin<-kpset(matrix_complete, size = keyplayer_num, type = "degree", cmode = "indegree", binary = TRUE, method = "max") 
# kp_in_mreach<-kpset(matrix_complete, size = keyplayer_num, type = "mreach.degree", cmode = "indegree", M = 1,binary = TRUE)
# kp_in_mreach_close<-kpset(matrix_inv_non_zero, size = keyplayer_num, type = "mreach.closeness", cmode = "indegree", M = 1)
# kp_in_degree_parallel<-kpset(matrix_complete, size = keyplayer_num, type = "degree", cmode = "indegree", parallel = TRUE,cluster = 2)
# 
# kp_out_degree_max<-kpset(matrix_complete, size = keyplayer_num, type = "degree", cmode = "outdegree", method = "max")
# kp_out_degree_max_bin<-kpset(matrix_complete, size = keyplayer_num, type = "degree", cmode = "outdegree", binary = TRUE, method = "max")
# kp_out_mreach<-kpset(matrix_complete, size = keyplayer_num, type = "mreach.degree", cmode = "outdegree", M = 1,binary = TRUE)
# kp_out_mreach_close<-kpset(matrix_inv_non_zero, size = keyplayer_num, type = "mreach.closeness", cmode = "outdegree", M = 1)
# kp_out_degree_parallel<-kpset(matrix_complete, size = keyplayer_num, type = "degree", cmode = "outdegree", parallel = TRUE,cluster = 2)
# 
# kp_total_degree_max<-kpset(matrix_complete, size = keyplayer_num, type = "degree", cmode = "total", method = "max")
# kp_total_degree_max_bin<-kpset(matrix_complete, size = keyplayer_num, type = "degree", cmode = "total", binary = TRUE, method = "max")
# kp_total_mreach<-kpset(matrix_complete, size = keyplayer_num, type = "mreach.degree", cmode = "total", M = 1,binary = TRUE)
# kp_total_mreach_close<-kpset(matrix_inv_non_zero, size = keyplayer_num, type = "mreach.closeness", cmode = "total", M = 1)
# kp_total_degree_parallel<-kpset(matrix_complete, size = keyplayer_num, type = "degree", cmode = "total", parallel = TRUE,cluster = 2)

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
                             select=c(1,3,11,14))

model <- glm(Keyplay.bool ~.,family=binomial(link='logit'),data=data_logistic_df)
summary(model)

#################################################################
#################################################################

