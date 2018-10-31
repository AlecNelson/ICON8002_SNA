#Social Network Analysis Tool Script (ICON 8002)
# 10/31/18

#####################################
########## Set up analysis ##########
#####################################
basedirectory <- "C:/Users/solit/Documents/GitHub/ICON8002_SNA"
input_datapath <- "C:/Users/solit/Documents/GitHub/ICON8002_SNA/Data" # folder where data and outputs are stored

# Input name of the data files that will be used in analysis
vertex_datapath <- "vertex_test_df_10_28_18.csv" # vertex (ego) data frame with attributes
edge_indiv_datapath <- "edge_indiv_test_df_10_28_18.csv" # edge-related data/attributes between individuals
edge_org_datapath <- "edge_org_test_df.csv" # edge between individuals and organizations

# set working directory to the data folder
setwd(input_datapath) 

#List packages used for the analysis
list.of.packages <- c("igraph","fabricatr", "gtool", "keyplayer", "rmarkdown", "knitr", "RColorBrewer","sna","MASS","naturalsort")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # new packages that were not already installed

# Install new packages
if(length(new.packages)){install.packages(new.packages)} 

#Load all packages for analysis
lapply(list.of.packages, require, character.only = TRUE)

#Load SNA function
source("SNAfunction.R")

# Run SNA function
sna(input_datapath=input_datapath, vertex_datapath=vertex_datapath, edge_datapath=edge_datapath)





######################################## Creating SNA function ####################################################

sna<-function(input_datapath, vertex_datapath, edge_datapath){
  
  ##############################################
  ########## Import and checking data ##########
  ##############################################
  

  # The attribute data for this script is in a comma-separated-value
  # (CSV) file. read.csv() loads a CSV file into a data frame
  # object. In this case, we do have a header row, so we set
  # header=T, which tells R that the first row of data contains
  # column names.
  
  # Read in dataframes 
  vertex_df <- read.csv(vertex_datapath, header=T, row.names = 1)
  edge_indiv_df <- read.csv(edge_indiv_datapath, header=T, row.names = 1)
  edge_org_df <- read.csv(edge_org_datapath, header=T, row.names = 1)
  
  # Before we merge these data, we need to make sure 'ego' and 'alter' are the
  # same across data sets. We can compare each row using the == syntax. 
  # The command below should return TRUE for every row if all ego rows
  # are the same :
  sort(unique(as.character(vertex_df$ego))) == sort(unique(c(as.character(edge_indiv_df$ego),as.character(edge_indiv_df$alter))))

  # We can just have R return which row entries are not equal using the syntax below:
  which(unique(sort(vertex$ego)) != unique(sort(edge$ego)))
  
  ######################################
  ########## Network Analysis ##########
  ######################################
  
  # Constructing network and check network structure
  #Combine edge and vertex attribute information into igraph format
  graph_complete <- graph.data.frame(d = edge_indiv_df, vertices = vertex_df)
  
  #Symmetrize graph to remove directed edges
  graph_complete_symmetrized <- as.undirected(graph_complete, mode='collapse')
  
  # Simplify graph to remove loops and prepare variables
  graph_complete_simpl <- simplify(graph_complete)
  graph_complete_simpl_symm <- as.undirected(graph_complete_simpl, mode='collapse')

  # Summary of network
  summary(graph_complete)
  
  # Exporting edge list
  
  setwd(basedirectory)
  write.graph(graph_complete, file='graph_sna_full.txt', format="edgelist")
  
  # Calculating network attributes
  vertex_degree<-as.data.frame(degree(network))
  colnames(vertex_degree)<-"Degree"
  diameter<-diameter(network)
  closeness<-as.data.frame(closeness(network))
  colnames(closeness)<-"Closeness"
  reciprocity<-reciprocity(network)
  ecount<-ecount(network)
  vcount<-vcount(network)
  edge_density<-edge_density(network)
  in.degree<-as.data.frame(degree(network,mode="in"))
  colnames(in.degree)<-"In Degree"

  
  ######################################
  ########## Graphing network ##########
  ######################################
  
  # Plotting original network
  ## Fruchterman.reingold fault layout, simplified (i.e., no loops), directed
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
  
  
  ## Nicely layout, simplified (i.e., no loops), directed

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
  
  # Plot network with vertex colored based on profession
  ## Nicely layout, simplified (i.e., no loops), directed, vertices colored by profession
  layout.graph <- layout_(graph_complete_simpl, nicely())
  layout.graph<-norm_coords(layout.graph, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
  colr.palette <- brewer.pal(n = 9, name = "Spectral") # picking color palette to graph with
  profession.colors <- colr.palette[vertex_df$profession.df] # Assigning colors to professions
  
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
         col = profession.colors, pch=19, pt.cex=0.8, cex=0.8, bty="n", ncol=1) # Adding legend to figure
  
  
  
  # Plotting by community/profession with vertices weighted by in.degree
  V(graph_complete_symmetrized)$community <- vertex_df$profession.df
  colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen","blue","pink","green","purple"), alpha=.6)
  
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
       main='SNA with vertices grouped by profession (color) and weighted by in-degree',
       #frame=TRUE,
       margin=0.0001)
  
  legend(x=-1.5, y = -0.85, unique(vertex_df$profession), pch=19,
         col= colrs[V(graph_complete_symmetrized)$community], pt.cex=0.8, cex=0.8, bty="n", ncol=1) # Add legend to figure
  
  # Plot with vertices weighted by total degree
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
  
  
  ########################################################
  ###### NOTE: Check legend. Legend colors do not match profession colors in network. Can check by adding vertex labels. 
  ########################################################
  
  
  ####################################
  ########## Network Output ##########
  ####################################

  # Output network attributes and figures in an RMarkdown document
  rmarkdown::render("Rmarkdown_test.Rmd","pdf_document")
 
}

# saving SNA function
dump("sna", file="SNAfunction.R")

############################################ End SNA function ####################################################

