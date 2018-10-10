#Social Network Analysis Tool Script (ICON 8002)
# 10/6/18

########################################################
########################################################
basedirectory <- 
input_datapath <- 

vertex_datapath<-"vertex_test_df.csv"
edge_datapath<-"edge_test_df.csv"

setwd(input_datapath)

#List packages used
list.of.packages <- c("igraph","randomNames","fabricatr")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages)} 

#Load all packages
lapply(list.of.packages, require, character.only = TRUE)

#Load SNA function
source("SNAfunction.R")

##############################################
############## Run SNA function ##############
##############################################

sna(input_datapath=input_datapath, vertex_datapath=vertex_datapath, edge_datapath=edge_datapath)





######################################## Creating SNA function ####################################################

sna<-function(input_datapath, vertex_datapath, edge_datapath){
  
  ##############################################
  ########## Import and checking data ##########
  ##############################################
  
  ## input data structure
  ## 2 data sheets (csv files), 1 for vertices, 1 for connections/edges
  ## this seems to be common practice
  ## attributes can be numeric and characters, continuous and categorical
  
  # The attribute data for this script is in a comma-separated-value
  # (CSV) file. read.csv() loads a CSV file into a data frame
  # object. In this case, we do have a header row, so we set
  # header=T, which tells R that the first row of data contains
  # column names.
  
  
  # Read in dataframes 
  vertex<- read.csv(vertex_datapath,header=T, row.names = 1)
  edge<- read.csv(edge_datapath,header=T, row.names = 1)
  
  # Check for discrepancies in data (should this be included in the final package?)
  
  # str(vertex) # look at structure of data
  # summary(vertex) # summary of data with variables and variable attributes
  # head(vertex) # look at the first n rows of data
  # colnames(vertex) #look at column names
  # 
  # str(edge)
  # summary(edge)
  # head(edge)
  # colnames(edge)
  
  
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

# saving SNA function
dump("sna", file="SNAfunction.R")

############################################ End SNA function ####################################################

