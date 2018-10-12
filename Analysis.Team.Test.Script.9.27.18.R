#Data Processing for ICON 8002
# 10/4/18

############################
#basedirectory <- "C:\\Users\\ahn11803\\Documents\\GitHub\\ICON8002_SNA"
basedirectory <- "/Users/alecnelson/Documents/GitHub/ICON8002_SNA"

#inputdata_path <- "C:\\Users\\ahn11803\\Documents\\GitHub\\ICON8002_SNA\\Data"
input_datapath <- "/Users/alecnelson/Documents/GitHub/ICON8002_SNA/Data"

vertex_datapath <- "vertex_test_df.csv"
edge_indiv_datapath <- "edge_indiv_test_df.csv"
edge_org_datapath <- "edge_org_test_df.csv"

setwd(input_datapath)

#List packages used
list.of.packages <- c("igraph","randomNames","fabricatr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages)} 

#Load all packages
lapply(list.of.packages, require, character.only = TRUE)
########################################################
########################################################

## input data structure
## 2 data sheets (csv files), 1 for vertices, 1 for connections/edges
## this seems to be common practice
## attributes can be numeric and characters, continuous and categorical

# The attribute data for this script is in a comma-separated-value
# (CSV) file. read.csv() loads a CSV file into a data frame
# object. In this case, we do have a header row, so we set
# header=T, which tells R that the first row of data contains
# column names.

vertex_test <- read.csv(vertex_datapath, header=T, row.names = 1)
edge_indiv_test <- read.csv(edge_indiv_datapath, header=T, row.names = 1)
edge_org_test <- read.csv(edge_org_datapath, header=T, row.names = 1)

str(vertex_test)
summary(vertex_test)
head(vertex_test)
colnames(vertex_test)

str(edge_indiv_test)
summary(edge_indiv_test)
head(edge_indiv_test)
colnames(edge_indiv_test)

# Before we merge these data, we need to make sure 'ego' and 'alter' are the
# same across data sets. We can compare each row using the == syntax. 
# The command below should return TRUE for every row if all ego rows
# are the same :
unique(sort(vertex_test$ego)) == unique(sort(edge_indiv_test$ego))
unique(sort(vertex_test$ego)) == unique(sort(edge_org_test$ego))

# We can just have R return which row entries are not equal using the syntax below:
which(unique(sort(vertex_test$ego)) != unique(sort(edge_indiv_test$ego)))
which(unique(sort(vertex_test$ego)) != unique(sort(edge_org_test$ego)))


#Check names and ensure consistent with question inputs
#names(edge_test)[3:5] <- c("friendship_tie", "reports_to_tie") 

#Consider subsetting to just certain types of connections or categories
# edge_test_subset <- subset(edge_test, 
#                                    (high_fives > 0 & beer_gift != "none"))
# head(edge_test_subset)

#Combine edge and vertex attribute information into igraph format
test.graph <- graph.data.frame(d = edge_indiv_test, vertices = vertex_test)
summary(test.graph)
test.graph
V(test.graph)

#Get a list of vertex attribute responses
names(vertex_test)
get.vertex.attribute(test.graph,'profession.df')
unique(get.vertex.attribute(test.graph,'profession.df'))
# colrs <- c("gray50", "tomato", "gold","blue")
# V(test.graph)$color <- colrs[V(test.graph)$profession]

# Get an edge list or a matrix:
as_edgelist(test.graph, names=T)
as_adjacency_matrix(test.graph, attr="q3.years.worked.with.eiq")

# Or data frames describing nodes and edges:
as_data_frame(test.graph, what="edges")
as_data_frame(test.graph, what="vertices")


#Can convert to undirected
test.graph_symmetrized <- as.undirected(test.graph, mode='collapse')

in.degree<-degree(test.graph,mode="in")

plot(test.graph_symmetrized,
     #edge.color=edge_test$connection,
     edge.arrow.size=.5,
     vertex.color=vertex_test$profession.df,
     vertex.size=((in.degree)*1.5),
     vertex.label=NA,
     vertex.label.cex=0.7,
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main='Test Data Connections (color by profession)',
     #frame=TRUE,
     margin=0.0001)

legend(x=-1.5, y=-1.1, unique(vertex_test$profession), pch=21,
       col="#777777", pt.bg=unique(vertex_test$profession), pt.cex=2, cex=.8, bty="n", ncol=1)


# We can also plot the communities without relying on their built-in plot:
    #see more at: http://kateto.net/network-visualization
V(test.graph_symmetrized)$community <- vertex_test$profession.df
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen","blue","pink","green","purple"), alpha=.6)
plot(test.graph_symmetrized, vertex.color=colrs[V(test.graph_symmetrized)$community])

plot(test.graph_symmetrized,
     #edge.color=edge_test$connection,
     edge.arrow.size=.5,
     vertex.color=colrs[V(test.graph_symmetrized)$community],
     vertex.size=((in.degree)*1.5),
     vertex.label=vertex_test$profession.df,
     vertex.label.cex=0.7,
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main='Test Data Connections (color by profession/community)',
     #frame=TRUE,
     margin=0.0001)

# legend(1,
#        1.25,
#        legend = c('Profession'),
#        col = colrs[V(test.graph_symmetrized)$community],
#        lty=1,
#        cex = .7)
# dev.off()

# icon_beer_none_rm <- delete.edges(icon.graph, 
#                           E(icon.graph)[get.edge.attribute(icon.graph,
#                                         name = "beer_gift") == "none"])

# plot(icon_beer_none_rm,
#      edge.color=edge_test$beer_gift,
#      vertex.color=vertex_test$favorite_beer,
#      edge.arrow.size=.2,
#      vertex.size=((in.degree)*10),main='ICON Beers Given ("None" removed)')
# 
# plot(icon.graph,edge.color=edge_test$high_five,edge.width=edge_test$high_fives,edge.arrow.size=.2,main='ICON High-Fives (weighted)')

## network metrics
degree(test.graph)
which.max(degree(test.graph))
which.min(degree(test.graph))
diameter(test.graph)
closeness(test.graph)
reciprocity(test.graph)
ecount(test.graph)
vcount(test.graph)
edge_density(test.graph)

#Display information as matrix format
test.graph[]

setwd(input_datapath)
pdf("SNA_Output_test_1200_vertex_nicely.pdf")

test.graph_symmetrized <- as.undirected(test.graph, mode='collapse')

in.degree<-degree(test.graph,mode="in")
#layout.graph <- layout_(test.graph, with_dh())
layout.graph <- layout_(test.graph, with_graphopt())
layout.graph <- layout_(test.graph, with_lgl())

layout.graph <- layout_(test.graph, with_drl())
layout.graph <- layout_(test.graph, with_fr())

#layout_nicely tries to choose an appropriate layout function for the supplied graph, and uses that
# to generate the layout. The current implementation works like this:
#   1. If the graph has a graph attribute called ‘layout’, then this is used. If this attribute is an R
# function, then it is called, with the graph and any other extra arguments.
# 2. Otherwise, if the graph has vertex attributes called ‘x’ and ‘y’, then these are used as coordinates
# If the graph has an additional ‘z’ vertex attribute, that is also used.
# 3. Otherwise, if the graph is connected and has less than 1000 vertices, the Fruchterman-Reingold
# layout is used, by calling layout_with_fr.
# 4. Otherwise the DrL layout (Distributed Recursive (Graph) Layout) is used, layout_with_drl is called.
layout.graph <- layout_(test.graph, nicely())
layout.graph <- layout_(test.graph_symmetrized, nicely())
layout.graph<-norm_coords(layout.graph, ymin=-1, ymax=1, xmin=-1, xmax=1)

plot(test.graph_symmetrized,
     layout=(layout.graph*1.1),
     rescale=F, 
     #edge.color=edge_test$connection,
     edge.arrow.size=.5,
     vertex.color=vertex_test$profession.df,
     #vertex.size=((in.degree)*1.5),
     vertex.size=3,
     #vertex.label=vertex_test$profession.df,
     vertex.label=NA,
     vertex.label.cex=0.7,
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main='Test Data Connections (color by profession)',
     #frame=TRUE,
     margin=0.0001)

dev.off()


hist(edge_indiv_test$q3.years.worked.with.eiq)
mean(edge_indiv_test$q3.years.worked.with.eiq)
sd(edge_indiv_test$q3.years.worked.with.eiq)

cut.off <- round(mean(edge_indiv_test$q3.years.worked.with.eiq))
test.graph.years <- delete_edges(test.graph, E(test.graph)[q3.years.worked.with.eiq<cut.off])

layout.graph <- layout_(test.graph.years, nicely())
layout.graph<-norm_coords(layout.graph, ymin=-1, ymax=1, xmin=-1, xmax=1)

pdf("SNA_Output_work_years.pdf")

plot(test.graph.years,
     layout=(layout.graph*1.1),
     rescale=F, 
     #edge.color=edge_test$connection,
     edge.arrow.size=.01,
     vertex.color=vertex_test$profession,
     vertex.size=((in.degree)*0.9),
     #vertex.size=3,
     #vertex.label=vertex_test$profession.df,
     vertex.label=NA,
     vertex.label.cex=0.6,
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main=paste0('Network of worked-together-with ',cut.off,' years connection'),
     #frame=TRUE,
     margin=0.0001)

dev.off()

#Clustering function to add weights to edges with shared profession
G_Grouped = test.graph_symmetrized
E(G_Grouped)$weight = 1
professions.list<-unique(V(G_Grouped)$profession.df)
## Add edges with high weight between all nodes in the same group
for(i in 1:length(professions.list)) {
  GroupV = which(V(G_Grouped)$profession.df == professions.list[i])
  G_Grouped = add_edges(G_Grouped, combn(GroupV, 2), attr=list(weight=5))
  print(paste0("Ran loop for profession ",professions.list[i]))
} 

## Now create a layout based on G_Grouped
LO = layout_with_fr(G_Grouped)
LO<-norm_coords(LO, ymin=-1, ymax=1, xmin=-1, xmax=1)


setwd(input_datapath)
pdf("SNA_Output_group_layout.pdf")

plot(test.graph_symmetrized,
     layout=(LO*1.0),
     rescale=F, 
     #edge.color=edge_test$connection,
     edge.arrow.size=.5,
     vertex.color=vertex_test$profession.df,
     vertex.size=((in.degree)*0.7),
     vertex.size=3,
     vertex.label=vertex_test$profession.df,
     #vertex.label=NA,
     vertex.label.cex=0.7,
     vertex.label.color= adjustcolor("black", 0.6),
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main='Test Data Connections (color by profession)',
     #frame=TRUE,
     margin=0.0001)

dev.off()

#Add graph of strong/weak connections between professional groups






# The write.graph() function exports a graph object in various
# formats readable by other programs. There is no explicit
# option for a UCINET data type, but you can export the graph
# as a Pajek object by setting the 'format' parameter to 'pajek.'
# Note that the file will appear in whichever directory is set 
# as the default in R's preferences, unless you previously 
# changed this via setwd().
write.graph(test.graph, file='test_full.dl', format="pajek")

# For a more general file type (e.g., importable to Excel),
# use the "edgelist" format. Note that neither of these will
# write the attributes; only the ties are maintained.
write.graph(test.graph, file='test_full.txt', format="edgelist")

#################################################################
#################################################################

#VERTEX ATTRIBUTE GENERATOR
#Name generator
ego.df<-randomNames(1200, which.names="both"#,ethnicity = c(3:5)
                    ,name.order="last.first",name.sep=", ")

#Profession generator
profession<-c("Commercial fisherman","Commercial crabbers or dealer","Dock and fish house", "Shellfish gatherer")
profession.df<-sample(profession,100,replace=TRUE,prob = c(0.53,.20,.15,.12))

##################
#Issues question:
issues.economic.vq<-round(runif(100, min = 0, max=1))
issues.environmental.vq<-round(runif(100, min = 0, max=1))
issues.social.vq<-round(runif(100, min = 0, max=1))
issues.political.vq<-round(runif(100, min = 0, max=1))
issues.other.vq<-round(runif(100, min = 0, max=1))
issues.other.txt.vq<-randomNames(100, which.names="first")


ls()
vq<-ls(pattern = ".vq")
##################
vertex.test.df <-as.data.frame(cbind(ego.df,as.data.frame(mget(vq))))
# vertex.test.df <-as.data.frame(cbind(ego.df,profession,issues.economic,issues.environmental,issues.social,issues.political,issues.other,issues.other.txt))
names(vertex.test.df)[1]="ego"

write.csv(vertex.test.df,"vertex_test_df.csv")

#################################################################

#EDGE ATTRIBUTE GENERATOR

#Vertex ego list
ego.df<-as.vector(vertex.test.df$ego)

max_connections=7
alter.test.df<-data.frame()

for(i in 1:length(ego.df)){
  ego.df.rm<-ego.df[!ego.df== ego.df[i]]
  alter.i<-sample(ego.df.rm,sample(1:max_connections,1),replace = FALSE)
  alter.df.i<-cbind(rep(ego.df[i],length(alter.i)),alter.i)
  alter.test.df<-rbind(alter.test.df,alter.df.i)
}

str(alter.test.df)

names(alter.test.df)<-c("ego","alter")

##################
#Interactions question:
interaction.freq<-round(runif(nrow(alter.test.df), min = 0, max=3))


##################
alter.test.df <-as.data.frame(cbind(alter.test.df,interaction.freq))

write.csv(alter.test.df,"edge_test_df.csv")

######################################################
######################################################
###EXPERIMENTAL ZONE###
######################################################
######################################################

categorical_example <- fabricate(
  N = 6,
  p1 = runif(N, 0, 1),
  p2 = runif(N, 0, 1),
  p3 = runif(N, 0, 1),
  cat = draw_categorical(N = N, prob = cbind(p1, p2, p3))
)

survey_data <- fabricate(
  N = 100,
  Q1 = draw_likert(x = rnorm(N), type = 7),
  Q2 = draw_likert(x = rnorm(N), type = 5),
  Q3 = draw_likert(x = rnorm(N), type = 4),
  Q4 = draw_likert(x = rnorm(N), breaks = c(-Inf, -0.8, 0, 1, 2, Inf))
)

table(survey_data$Q2)

fabricate(N = 3,
          x = 5 * rnorm(N),
          ordered = draw_ordered(x = x,
                                 breaks = c(-Inf, -1, 1, Inf),
                                 break_labels = c("Not at all concerned",
                                                  "Somewhat concerned",
                                                  "Very concerned")))


# Reachability can only be computed on one vertex at a time. To
# get graph-wide statistics, change the value of "vertex"
# manually or write a for loop. (Remember that, unlike R objects,
# igraph objects are numbered from 0.)

reachability <- function(g, m) {
  reach_mat = matrix(nrow = vcount(g), 
                     ncol = vcount(g))
  for (i in 1:vcount(g)) {
    reach_mat[i,] = 0
    this_node_reach <- subcomponent(g, (i - 1), mode = m)
    
    for (j in 1:(length(this_node_reach))) {
      alter = this_node_reach[j] + 1
      reach_mat[i, alter] = 1
    }
  }
  return(reach_mat)
}

reach_full_in <- reachability(krack_full, 'in')
reach_full_out <- reachability(krack_full, 'out')
reach_full_in
reach_full_out

reach_advice_in <- reachability(krack_advice, 'in')
reach_advice_out <- reachability(krack_advice, 'out')
reach_advice_in
reach_advice_out

reach_friendship_in <- reachability(krack_friendship, 'in')
reach_friendship_out <- reachability(krack_friendship, 'out')
reach_friendship_in
reach_friendship_out

reach_reports_to_in <- reachability(krack_reports_to, 'in')
reach_reports_to_out <- reachability(krack_reports_to, 'out')
reach_reports_to_in
reach_reports_to_out



# Community detection (by optimizing modularity over partitions):

clp <-cluster_fast_greedy(test.graph_symmetrized)
V(test.graph_symmetrized)$community <- clp$membership
class(clp)
plot(clp, test.graph_symmetrized)


plot(clp,test.graph_symmetrized,
     layout=(layout.graph*1.1),
     rescale=F, 
     #edge.color=edge_test$connection,
     edge.arrow.size=.5,
     vertex.color=vertex_test$profession,
     #vertex.size=((in.degree)*1.5),
     vertex.size=3,
     #vertex.label=vertex_test$profession.df,
     vertex.label=NA,
     vertex.label.cex=0.7,
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main='Test Data Connections (color by profession)',
     #frame=TRUE,
     margin=0.0001)


layoutVertexByAttr <- function(graph, wc, cluster.strength=1,layout=layout.auto) {  
  g <- graph.edgelist(get.edgelist(graph))
  E(g)$weight <- 1
  attr <- cbind(id=1:vcount(g), val=wc)
  g <- g + vertices(unique(attr[,2])) + igraph::edges(unlist(t(attr)), weight=cluster.strength)
  l <- layout(g, weights=E(g)$weight)[1:vcount(graph),]
  return(l)
}

l = layoutVertexByAttr(test.graph_symmetrized, vertex_test$profession.df, cluster.strength=1, layout=layout.kamada.kawai)

plot.igraph(test.graph_symmetrized, vertex.color=vertex_test$profession.df, layout=l)

GroupByVertex01 = function(Groups, spacing = 5) {
  Position = (order(Groups) + spacing*Groups)
  Angle    = Position * 2 * pi / max(Position)
  matrix(c(cos(Angle), sin(Angle)), ncol=2)
}

GBV1 = GroupByVertex01(V(test.graph_symmetrized)$community)
#plot(g2, vertex.color=rainbow(3)[V(test.graph_symmetrized)$community], layout=GBV1)

GroupByVertex02 = function(Groups) {
  numGroups = length(unique(Groups))
  GAngle    = (1:numGroups) * 2 * pi / numGroups
  Centers   = matrix(c(cos(GAngle), sin(GAngle)), ncol=2)
  x = y = c()
  for(i in 1:numGroups) {
    curGroup = which(Groups == unique(Groups)[i])
    VAngle = (1:length(curGroup)) * 2 * pi / length(curGroup)
    x = c(x, Centers[i,1] + cos(VAngle) / numGroups )
    y = c(y, Centers[i,2] + sin(VAngle) / numGroups)
  }
  matrix(c(x, y), ncol=2)
}

GBV2 = GroupByVertex02(V(test.graph_symmetrized)$community)
plot(test.graph_symmetrized, vertex.color=rainbow(3)[V(test.graph_symmetrized)$community], layout=GBV2)

plot(test.graph_symmetrized,
     layout=(GBV2*1.0),
     rescale=F, 
     #edge.color=edge_test$connection,
     edge.arrow.size=.5,
     #vertex.color=get.vertex.attribute(test.graph,'profession.df'),
     #vertex.size=((in.degree)*1.5),
     vertex.size=3,
     vertex.label=get.vertex.attribute(test.graph,'profession.df'),
     #vertex.label=NA,
     vertex.label.cex=0.7,
     vertex.label.dist=1,
     vertex.label.degree=-0.6,
     main='Test Data Connections (color by profession)',
     #frame=TRUE,
     margin=0.0001)




EL = structure(c(1, 5, 4, 2, 7, 4, 7, 6, 6, 2, 9, 6, 3, 10,
                 7, 8, 3, 9, 8, 5, 3, 4, 10, 13, 12, 12, 13, 12, 13, 15, 15,
                 11, 11, 14, 14, 11, 11, 11, 15, 15, 11, 11, 13, 13, 11, 13),
               .Dim = c(23L, 2L))

g2 = graph_from_edgelist(EL, directed = FALSE)

V(g2)

Groups = c(rep(1, 10), 2,2,3,3,3)
Groups = c(3,4,3,5,5,5,3,4,4,2,3,4,5,2,2)
Groups = sort(Groups)
plot(g2, vertex.color=rainbow(3)[Groups])



GBV1 = GroupByVertex01(Groups)
plot(g2, vertex.color=rainbow(3)[Groups], layout=GBV1)

GBV2 = GroupByVertex02(Groups)
plot(g2, vertex.color=rainbow(3)[Groups], layout=GBV2)


set.seed(1234)
G = erdos.renyi.game(20, 0.25)
V(G)$Group1 = sample(3,20, replace=TRUE)
plot(G, vertex.color=rainbow(3, alpha=0.4)[V(G)$Group1])

G_Grouped = G
E(G_Grouped)$weight = 1

## Add edges with high weight between all nodes in the same group
for(i in unique(V(G)$Group1)) {
  GroupV = which(V(G)$Group1 == i)
  G_Grouped = add_edges(G_Grouped, combn(GroupV, 2), attr=list(weight=5))
} 

## Now create a layout based on G_Grouped
set.seed(567)
LO = layout_with_fr(G_Grouped)

## Use the layout to plot the original graph
plot(G, vertex.color=rainbow(3, alpha=0.4)[V(G)$Group1], layout=LO)

##################
#G = erdos.renyi.game(20, 0.25)
#V(G)$Group1 = sample(3,20, replace=TRUE)
G<-test.graph_symmetrized
#V(G)$profession.df
G_Grouped = test.graph_symmetrized
E(G_Grouped)$weight = 1
professions.list<-unique(V(G_Grouped)$profession.df)
## Add edges with high weight between all nodes in the same group
for(i in 1:length(professions.list)) {
  GroupV = which(V(G_Grouped)$profession.df == professions.list[i])
  G_Grouped = add_edges(G_Grouped, combn(GroupV, 2), attr=list(weight=5))
  print(paste0("Ran loop for profession ",professions.list[i]))
} 

## Now create a layout based on G_Grouped
LO = layout_with_fr(G_Grouped)


plot(G, vertex.color=rainbow(3)[V(G)$profession.df])
## Use the layout to plot the original graph
plot(G, vertex.color=rainbow(3, alpha=0.4)[V(G)$profession.df], layout=LO)



