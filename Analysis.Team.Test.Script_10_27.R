#Key Data Processing Functions for ICON 8002
# 11/8/18

#To-DO List (11/9/18):
  #1. Merge GLM-ready datasets with summarized edge attributes
  #2. Format GLM models for AICc model selection algorithms
  #3. Interface with question numbering labels for subsetting predictor vars
  #4. Produce GLMs according to sub-network deliniation
  #5. Adjust ego/alter labels to reflect numeric instead of named values
  #6. Add column sum of keyplayer bool values

############################
#Type in the base directory and input datapaths below
basedirectory <-  "/Users/alecnelson/Documents/GitHub/ICON8002_SNA"
input_datapath <- "/Users/alecnelson/Documents/GitHub/ICON8002_SNA/Data"

vertex_datapath <- "vertex_test_df_11_02_18.csv"
edge_indiv_datapath <- "edge_individual_test_df_11_02_18.csv"
#edge_org_datapath <- "edge_org_test_df.csv"

setwd(input_datapath)

#List packages used
list.of.packages <- c("igraph","randomNames","fabricatr","plyr","RColorBrewer","keyplayer","sna","MASS","naturalsort","stringr","Rmisc")

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
#edge_org_df <- read.csv(edge_org_datapath, header=T, row.names = 1)

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

#Create sub-graphs based on profession attribute
vertex.professional.i<-igraph::get.vertex.attribute(graph_complete,'q1.profession.df.vq')

for(i in 1:length(unique(igraph::get.vertex.attribute(graph_complete,'q1.profession.df.vq')))){
  profession.i<-unique(igraph::get.vertex.attribute(graph_complete,'q1.profession.df.vq'))[i]
  subgraph.vector.i<-which(vertex.professional.i==profession.i)
  subgraph.i<-igraph::induced_subgraph(graph_complete, subgraph.vector.i)
  profession.i<-gsub(" ", "_", profession.i, fixed = TRUE)
  subgraph.name<-paste0("sub_g_",profession.i)
  assign(subgraph.name,subgraph.i)
}

subgraph_list<-ls(pattern = "sub_g_")
summary(get(subgraph_list[1]))


plot.network.original <- plot(get(subgraph_list[5]),
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
                              main=subgraph_list[5],
                              #frame=TRUE,
                              margin=0.0001)


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
#setwd(basedirectory)
#pdf("SNA_Output_Original.pdf")
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
#dev.off()

### Plot network with no alterations
### Nicely layout, simplified (i.e., no loops), directed
#setwd(basedirectory)
#pdf("SNA_Output_Oringinal_Nicely.pdf")
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
#dev.off()

### Plot network by Profession (colors)
### Nicely layout, simplified (i.e., no loops), directed, vertices colored by profession
layout.graph <- layout_(graph_complete_simpl, nicely())
layout.graph<-norm_coords(layout.graph, ymin=-1, ymax=1, xmin=-1, xmax=1)

## Playing around with colors
colr.palette <- brewer.pal(n = 9, name = "Spectral")
profession.colors <- colr.palette[vertex_df$profession.df]

#setwd(basedirectory)
#pdf("SNA_Output_Professions.pdf")
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
#dev.off()

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

#setwd(basedirectory)
#pdf("SNA_Output_Community_Professions_InDegree.pdf")
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

#dev.off()

##### Plot of Vertices weighted by total degree
#setwd(basedirectory)
#pdf("SNA_Output_Professions_Vertex_TotalDegree.pdf")
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
#dev.off()


########################################################
###### NOTE: Check legend. Legend colors do not match profession colors in network. Can check by adding vertex labels. 
########################################################

cut.off <- round(mean(edge_indiv_df$q3.years.worked.with.eiq))
graph_complete.years <- delete_edges(graph_complete, E(graph_complete)[q3.years.worked.with.eiq<cut.off])

layout.graph <- layout_(graph_complete.years, nicely())
layout.graph<-norm_coords(layout.graph, ymin=-1, ymax=1, xmin=-1, xmax=1)

#setwd(basedirectory)
#pdf("SNA_Output_work_years.pdf")
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
#dev.off()

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

#setwd(basedirectory)
#pdf("SNA_Output_group_layout.pdf")
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
#dev.off()

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
print(paste0("The vertex with the greatest degree is ",as.character(vertex_df$ego[Degree_max_stat_indiv])," (Degree of ",as.numeric(igraph::degree(graph_complete)[Degree_max_stat_indiv]),"). This measure is the number of its adjacent edges."))

Degree_min_stat_indiv<-which.min(igraph::degree(graph_complete))
print(paste0("The vertex with the fewest degree is ",as.character(vertex_df$ego[Degree_min_stat_indiv])," (Degree of ",as.numeric(igraph::degree(graph_complete)[Degree_min_stat_indiv]),"). This measure is the number of its adjacent edges."))

#closeness_complete<-igraph::closeness(graph_complete)
Closeness_max_stat_indiv<-which.max(igraph::closeness(graph_complete))
print(paste0("The vertex with the greatest closeness is ",as.character(vertex_df$ego[Closeness_max_stat_indiv])," (Closeness of ",as.numeric(igraph::degree(graph_complete)[Closeness_max_stat_indiv]),"). This measures how many steps is required to access every other vertex from a given vertex."))

Closeness_min_stat_indiv<-which.min(igraph::closeness(graph_complete))
print(paste0("The vertex with the least closeness is ",as.character(vertex_df$ego[Closeness_min_stat_indiv])," (Closeness of: ",as.numeric(igraph::degree(graph_complete)[Closeness_min_stat_indiv]),"). This measures how many steps is required to access every other vertex from a given vertex."))

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

#evcent(matrix_inv_non_zero, gmode = "digraph", ignore.eval = FALSE, use.eigen = TRUE)
#evcent(matrix_complete_symm)

#Fragmentation centrality measures the extent to which a network is fragmented after a node is removed from the network 
#fragment(matrix_inv_non_zero)

#Determine Keyplayers via different statistical mesurements
## Set size of set group to number of keyplayers wanted per metric
keyplayer_num<-3
processer_cores<-4

##################
# Start the clock!
ptm <- proc.time()
##################

kp_closeness<-kpset(matrix_complete,size=keyplayer_num,type="closeness",parallel=TRUE,cluster=processer_cores,method="min")
kp_betweenness<-kpset(matrix_complete,size=keyplayer_num,type="betweenness",parallel=TRUE,cluster=processer_cores,method="min")
kp_degree<-kpset(matrix_complete,size=keyplayer_num,type="degree",cmode="total",parallel=TRUE,cluster=processer_cores,method="max")
kp_eigenvector<-kpset(matrix_complete,size=keyplayer_num,type="evcent",parallel=TRUE,cluster=processer_cores,method="max")

##################
#Check the time elapsed
proc.time() - ptm
##################

closeness_kp_num<-kp_closeness$keyplayers[1:keyplayer_num]
betweenness_kp_num<-kp_betweenness$keyplayers[1:keyplayer_num]
degree_kp_num<-kp_degree$keyplayers[1:keyplayer_num]
eigenvector_kp_num<-kp_eigenvector$keyplayers[1:keyplayer_num]

####################################
### Example Keyplayers - 11/2/18
closeness_kp_num<-c(54,67,70)
betweenness_kp_num<-c(100,102,110)
degree_kp_num<-c(74,100,102)
eigenvector_kp_num<-c(1,72,100)
####################################

Keyplayer.list<-c(closeness_kp_num,betweenness_kp_num,degree_kp_num,eigenvector_kp_num)
Keyplayer.list<-unique(Keyplayer.list)

closeness_kp_names<-rownames(matrix_complete)[closeness_kp_num]
print(sprintf("The egos identified as keyplayers via the Closeness metric are: %s. This metric suggests a rapid diffusion of information.",paste(closeness_kp_names,collapse="; ")))

betweenness_kp_names<-rownames(matrix_complete)[betweenness_kp_num]
print(sprintf("The egos identified as keyplayers via the Betweenness metric are: %s. This metric suggests a brokering of information or initiatives between disconnected groups.",paste(betweenness_kp_names,collapse="; ")))

degree_kp_names<-rownames(matrix_complete)[degree_kp_num]
print(sprintf("The egos identified as keyplayers via the Degree metric are: %s. This metric suggests a direct connection to complex knowledge and initiatives.",paste(degree_kp_names,collapse="; ")))

eigenvector_kp_names<-rownames(matrix_complete)[eigenvector_kp_num]
print(sprintf("The egos identified as keyplayers via the Eigenvector metric are: %s. This metric suggests a facilitation of widespread diffusion of information to important others.",paste(eigenvector_kp_names,collapse="; ")))

Overlap_vec<-c(closeness_kp_names,betweenness_kp_names,degree_kp_names,eigenvector_kp_names)

Overlap_vec<-unique(Overlap_vec[duplicated(Overlap_vec)])
print(sprintf("The egos identified as keyplayers via multiple Overlapping metrics are: %s. This suggests the role of a keyplayer through multiple functions.",paste(Overlap_vec,collapse="; ")))

Metrics_list<-list(closeness_kp_names,betweenness_kp_names,degree_kp_names,eigenvector_kp_names,Overlap_vec)

Closeness_vec<-c("Closeness",closeness_kp_names)
Betweenness_vec<-c("Betweenness",betweenness_kp_names)
Degree_vec<-c("Degree",degree_kp_names)
Eigenvector_vec<-c("Eigenvector",eigenvector_kp_names)
#Overlap_vec<-c("Overlap",Overlap_vec)

Keyplayer_df<-as.data.frame(rbind(Closeness_vec,Betweenness_vec,Degree_vec,Eigenvector_vec),row.names = F)

names(Keyplayer_df)<-c("Statistic",LETTERS[1:keyplayer_num])

########################################################
###### NOTE: Edge values can be included in this calculation in the "binary" option 
########################################################

Keyplayer_names<-igraph::get.vertex.attribute(graph_complete_simpl)$name[Keyplayer.list]

##### Plotting network with key player as identified by above model.
layout.graph <- layout_(graph_complete_simpl, nicely())
layout.graph<-norm_coords(layout.graph, ymin=-1, ymax=1, xmin=-1, xmax=1)

colrs <- c("blue", "green", "red","yellow","purple",adjustcolor("Gray60", alpha=.2))
ego_names<-igraph::get.vertex.attribute(graph_complete_simpl)$name
ego_col<-rep(colrs[length(colrs)],length(ego_names))

for(m in 1:length(Metrics_list)){
  colr_set_m<-which(ego_names %in% Metrics_list[[m]])
  ego_col[colr_set_m]<-colrs[m]
}

#setwd(basedirectory)
#pdf("SNA_Output_KeyPlayer.pdf")
plot(graph_complete_simpl,
     layout=layout.graph,
     rescale=T,
     edge.color="Gray80",
     edge.arrow.size=.01,
     vertex.color=ego_col,
     #vertex.size=((in.degree)*1.5),
     #vertex.size=(igraph::degree(graph_complete)*0.5),
     vertex.size=ifelse((igraph::get.vertex.attribute(graph_complete_simpl)$name %in% Keyplayer_names), 8, 4),
     vertex.label= ifelse((igraph::get.vertex.attribute(graph_complete_simpl)$name %in% Keyplayer_names), as.character(igraph::get.vertex.attribute(graph_complete_simpl)$name), NA),
     vertex.label.color = "blue",
     vertex.label=NA,
     vertex.label.cex=ifelse(vertex_df$ego == as.character(vertex_df$ego[177]), .5, NA),
     vertex.label.dist=0,
     vertex.label.degree=0,
     main='Network with highlighted Key Players',
     #frame=TRUE,
     margin=0.0001)

legend(x=-1.5, y = 0, c("Closeness","Betweenness","Degree","Eigenvector","Overlap"), pch=19,
       col= c("blue", "green", "red","yellow","purple"), pt.cex=1.5, cex=0.8, bty="n", ncol=1)

#dev.off()

########################################################
## Need to figure out how to separate/spread overlapping vertices so that key players show up. 
########################################################

#Display information as matrix format
#graph_complete[]
matrix_complete<-as.matrix(as_adjacency_matrix(graph_complete))

data_logistic_df<-vertex_df

Keyplay.bool=ifelse(rownames(data_logistic_df) %in% Keyplayer.list,1,0)
data_logistic_df<-cbind(Keyplay.bool,data_logistic_df)
colnames(data_logistic_df)


#1. Update GLM-ready datasets to include summarized edge attributes

edge_list<-as_edgelist(graph_complete, names = TRUE)
ego_list<-unique(edge_list[,1])
edge_summary_df<-data.frame(matrix(ncol=(length(igraph::edge_attr_names(graph_complete))+1),nrow=length(ego_list)))
edge_summary_df[1]<-ego_list
colnames(edge_summary_df)[1]<-"ego"
colnames(edge_summary_df)[(1:length(igraph::edge_attr_names(graph_complete)))+1]<-igraph::edge_attr_names(graph_complete)

for(i in 1:length(igraph::edge_attr_names(graph_complete))){
  edge_var_i<-igraph::edge_attr_names(graph_complete)[i]
  mode_class_i<-summary(igraph::get.edge.attribute(graph_complete))[i,3]
  for(j in 1:length(ego_list)){
    #attr_edge_vector<-vector()
    attr_pos_j<-which(edge_list[,1]==ego_list[j])
    if(mode_class_i=="numeric"){
      edge_vect_i<-as.numeric(unlist(igraph::get.edge.attribute(graph_complete)[i]))
      edge_vect_ij<-edge_vect_i[attr_pos_j]
      edge_val_ij<-mean(edge_vect_ij)
      edge_summary_df[j,(i+1)]<-edge_val_ij
      #attr_edge_vector<-c(attr_edge_vector,edge_val_ij)
      #print("Num: Subset edge vector by which rows by ego....") 
    }else if(mode_class_i=="character"){
      edge_vect_i<-as.character(unlist(igraph::get.edge.attribute(graph_complete)[i]))
      edge_vect_ij<-edge_vect_i[attr_pos_j]
      edge_val_ij<-length(unique(edge_vect_ij))
      edge_summary_df[j,(i+1)]<-edge_val_ij
      #attr_edge_vector<-c(attr_edge_vector,edge_val_ij)
      #print("Char: Summarize unique values by which rows by ego....") 
    }else{
      print("Error: Could not identify mode correctly")
    }
    #print(paste0("Data summarized for ",ego_list[j]))
    }
  }

str(edge_summary_df)

########################################################
###### NOTE: Use this example section to add alter "NA" rows and rbind->cbind
########################################################
# tag for vertex sheet questions = ".vq"
vq<-ls(pattern = ".vq")
vq<-naturalsort(vq)
vertex.test.df <-as.data.frame(cbind(ego.df, profession.df,as.data.frame(mget(vq))))
names(vertex.test.df)[1]="ego"

Out_Network_Names<-unique(alter.test.df[which(alter.test.df$alter_type=="Out-Network"),2])
for(k in 1:length(Out_Network_Names)){
  Name.k<-as.character(Out_Network_Names[k])
  Out_Network<-as.character("Out-Network")
  Out.row.k<-c(Name.k,Out_Network,rep("N/A",(length(colnames(vertex.test.df))-2)))
  Out.row.k<-as.data.frame(t(Out.row.k))
  colnames(Out.row.k)<-colnames(vertex.test.df)
  vertex.test.df<-rbind(vertex.test.df,Out.row.k)
}
######################################################


data_logistic_total_df <- subset(data_logistic_df,
                             select=c(1,3,11,14))

model <- glm(Keyplay.bool ~.,family=binomial(link='logit'),data=data_logistic_total_df)
summary(model)

########################################################

Closeness.bool=ifelse((data_logistic_df$ego) %in% Metrics_list[[1]],1,0)
Betweenness.bool=ifelse((data_logistic_df$ego) %in% Metrics_list[[2]],1,0)
Degree.bool=ifelse((data_logistic_df$ego) %in% Metrics_list[[3]],1,0)
Eigenvector.bool=ifelse((data_logistic_df$ego) %in% Metrics_list[[4]],1,0)
Overlap.bool=ifelse((data_logistic_df$ego) %in% Metrics_list[[5]],1,0)

Metrics_logistic_df<-cbind(Closeness.bool,Betweenness.bool,Degree.bool,Eigenvector.bool,Overlap.bool,data_logistic_df)
colnames(Metrics_logistic_df)


########################################################
###### TASK: Add SUM column after keyplayer stats_bool
########################################################

Attribute_test<-c(8,9,10)

Closeness_logistic_df <- subset(Metrics_logistic_df,select=c(1,Attribute_test))
Betweenness_logistic_df <- subset(Metrics_logistic_df,select=c(2,Attribute_test))
Degree_logistic_df <- subset(Metrics_logistic_df,select=c(3,Attribute_test))
Eigenvector_logistic_df <- subset(Metrics_logistic_df,select=c(4,Attribute_test))
Overlap_logistic_df <- subset(Metrics_logistic_df,select=c(5,Attribute_test))



#Added Write.csv() functionality w/ Keyplayer attributes
date.text<-substr(vertex_datapath,(nchar(vertex_datapath)-11),(nchar(vertex_datapath)-4))
vertex.df.keyplayer<-paste0("vertex_df_keyplayer_",date.text,".csv")
write.csv(Metrics_logistic_df,vertex.df.keyplayer)

#Model subset and selection

Closeness_model <- glm(Closeness.bool ~.,family=binomial(link='logit'),data=Closeness_logistic_df)
summary(Closeness_model)
Betweenness_model <- glm(Betweenness.bool ~.,family=binomial(link='logit'),data=Betweenness_logistic_df)
summary(Betweenness_model)
Degree_model <- glm(Degree.bool ~.,family=binomial(link='logit'),data=Degree_logistic_df)
summary(Degree_model)
Eigenvector_model <- glm(Eigenvector.bool ~.,family=binomial(link='logit'),data=Eigenvector_logistic_df)
summary(Eigenvector_model)
Overlap_model <- glm(Overlap.bool ~.,family=binomial(link='logit'),data=Overlap_logistic_df)
summary(Overlap_model)

# Use 95% confidence interval for Estimated effect size (95% confidence intervals) of attributes
str(Closeness_model)
Closeness_model$coefficients
Closeness_model$residuals
Closeness_model$fitted.values

closeness_CI<-CI(Closeness_model$effects,ci=0.95)

#################################################################
#################################################################

