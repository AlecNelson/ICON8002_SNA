#Data Processing for ICON 8002
# 10/4/18

############################
#basedirectory <- "C:\\Users\\ahn11803\\Documents\\GitHub\\ICON8002_SNA"
basedirectory <- "/Users/alecnelson/Documents/GitHub/ICON8002_SNA"

#inputdata_path <- "C:\\Users\\ahn11803\\Documents\\GitHub\\ICON8002_SNA\\Data"
input_datapath <- "/Users/alecnelson/Documents/GitHub/ICON8002_SNA/Data"

vertex_datapath<-"vertex_test.csv"
edge_datapath<-"attribute_test1.csv"

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

vertex_test<- read.csv(vertex_datapath,header=T)
edge_test<- read.csv(edge_datapath,header=T)

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
# are the same for advice and friendship:
unique(sort(vertex_test$ego)) == unique(sort(edge_test$ego))

# We can just have R return which row entries are not equal using the syntax below:
which(unique(sort(vertex_test$ego)) != unique(sort(edge_test$ego)))

#Check names and ensure consistent with question inputs
names(edge_test)[3:5] <- c("friendship_tie", "reports_to_tie") 

#Consider subsetting to just certain types of connections or categories
edge_test_subset <- subset(edge_test, 
                                   (high_fives > 0 & beer_gift != "none"))
head(edge_test_subset)

#Combine edge and vertex attribute information into igraph format
icon.graph <- graph.data.frame(d = edge_test, vertices = vertex_test)
summary(icon.graph)
icon.graph
V(icon.graph)

#Get a list of edge attribute responses
get.edge.attribute(icon.graph,'beer_gift')

#Can convert to undirected
icon.graph_symmetrized <- as.undirected(icon.graph, mode='collapse')

in.degree<-degree(icon.graph,mode="in")

plot(icon.graph,
     edge.color=edge_test$beer_gift,
     vertex.color=vertex_test$favorite_beer,
     edge.arrow.size=.2,
     vertex.size=((in.degree)*10),main='ICON Beers Given')

legend(1, 
       1.25,
       legend = c('Beer Types'), 
       col = vertex_test$favorite_beer, 
       lty=1,
       cex = .7)
dev.off() 

icon_beer_none_rm <- delete.edges(icon.graph, 
                          E(icon.graph)[get.edge.attribute(icon.graph,
                                        name = "beer_gift") == "none"])

plot(icon_beer_none_rm,
     edge.color=edge_test$beer_gift,
     vertex.color=vertex_test$favorite_beer,
     edge.arrow.size=.2,
     vertex.size=((in.degree)*10),main='ICON Beers Given ("None" removed)')

plot(icon.graph,edge.color=edge_test$high_five,edge.width=edge_test$high_fives,edge.arrow.size=.2,main='ICON High-Fives (weighted)')

## network metrics
degree(icon.graph)
diameter(icon.graph)
closeness(icon.graph)
reciprocity(icon.graph)
ecount(icon.graph)
vcount(icon.graph)
edge_density(icon.graph)

#Display information as matrix format
icon.graph[]

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


##################
vertex.test.df <-cbind(ego.df,profession,issues.economic,issues.environmental,issues.social,issues.political,issues.other,issues.other.txt)

write.csv(vertex.test.df,"vertex_test_df.csv")

#################################################################

#EDGE ATTRIBUTE GENERATOR

#Vertex ego list
ego.df<-vertex.test.df[,1]

max_connections=5
alter.test.df<-data.frame()

for(i in 1:length(ego.df)){
  ego.df.rm<-ego.df[!ego.df== ego.df[i]]
  alter.i<-sample(ego.df.rm,sample(1:max_connections,1),replace = FALSE)
  alter.df.i<-cbind(rep(ego.df[i],length(alter.i)),alter.i)
  alter.test.df<-rbind(alter.test.df,alter.df.i)
}

names(alter.test.df)<-c("ego","alter")

write.csv(alter.test.df,"atler_test_df.csv")

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
