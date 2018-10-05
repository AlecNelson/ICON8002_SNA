#Data Processing for ICON 8002
# 10/4/18

############################
#basedirectory <- "C:\\Users\\ahn11803\\Documents\\GitHub\\ICON8002_SNA"
basedirectory <- "/Users/alecnelson/Documents/GitHub/ICON8002_SNA"

#inputdata_path <- "C:\\Users\\ahn11803\\Documents\\GitHub\\ICON8002_SNA\\Data"
input_datapath <- "/Users/alecnelson/Documents/GitHub/ICON8002_SNA/Data"

vertex_datapath<-"vertex_test_df.csv"
edge_datapath<-"edge_test_df.csv"

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

vertex_test<- read.csv(vertex_datapath,header=T, row.names = 1)
edge_test<- read.csv(edge_datapath,header=T, row.names = 1)

str(vertex_test)
summary(vertex_test)
head(vertex_test)
colnames(vertex_test)

str(edge_test)
summary(edge_test)
head(edge_test)
colnames(edge_test)

# Before we merge these data, we need to make sure 'ego' and 'alter' are the
# same across data sets. We can compare each row using the == syntax. 
# The command below should return TRUE for every row if all ego rows
# are the same :
unique(sort(vertex_test$ego)) == unique(sort(edge_test$ego))

# We can just have R return which row entries are not equal using the syntax below:
which(unique(sort(vertex_test$ego)) != unique(sort(edge_test$ego)))

#Check names and ensure consistent with question inputs
#names(edge_test)[3:5] <- c("friendship_tie", "reports_to_tie") 

#Consider subsetting to just certain types of connections or categories
# edge_test_subset <- subset(edge_test, 
#                                    (high_fives > 0 & beer_gift != "none"))
# head(edge_test_subset)

#Combine edge and vertex attribute information into igraph format
test.graph <- graph.data.frame(d = edge_test, vertices = vertex_test)
summary(test.graph)
test.graph
V(test.graph)

#Get a list of vertex attribute responses
names(vertex_test)
get.vertex.attribute(test.graph,'profession')
# colrs <- c("gray50", "tomato", "gold","blue")
# V(test.graph)$color <- colrs[V(test.graph)$profession]

#Can convert to undirected
test.graph_symmetrized <- as.undirected(test.graph, mode='collapse')

in.degree<-degree(test.graph,mode="in")

plot(test.graph_symmetrized,
     #edge.color=edge_test$connection,
     edge.arrow.size=.5,
     vertex.color=vertex_test$profession,
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
V(test.graph_symmetrized)$community <- vertex_test$profession
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)
plot(test.graph_symmetrized, vertex.color=colrs[V(test.graph_symmetrized)$community])

# legend(1,
#        1.25,
#        legend = c('Profession'),
#        col = vertex_test$profession,
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
diameter(test.graph)
closeness(test.graph)
reciprocity(test.graph)
ecount(test.graph)
vcount(test.graph)
edge_density(test.graph)

#Display information as matrix format
test.graph[]

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
ego.df<-randomNames(100, which.names="both",ethnicity = c(3:5),
                    name.order="last.first",name.sep=", ")

#Profession generator
profession<-c("Commercial fisherman","Commercial crabbers or dealer","Dock and fish house", "Shellfish gatherer")
profession.df<-sample(profession,100,replace=TRUE,prob = c(0.53,.20,.15,.12))

##################
#Issues question:
issues.economic<-round(runif(100, min = 0, max=1))
issues.environmental<-round(runif(100, min = 0, max=1))
issues.social<-round(runif(100, min = 0, max=1))
issues.political<-round(runif(100, min = 0, max=1))
issues.other<-round(runif(100, min = 0, max=1))
issues.other.txt<-randomNames(100, which.names="first")


ls()
##################
vertex.test.df <-as.data.frame(cbind(ego.df,profession,issues.economic,issues.environmental,issues.social,issues.political,issues.other,issues.other.txt))
names(vertex.test.df)[1]="ego"

write.csv(vertex.test.df,"vertex_test_df.csv")

#################################################################

#EDGE ATTRIBUTE GENERATOR

#Vertex ego list
ego.df<-as.vector(vertex.test.df$ego)

max_connections=5
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

