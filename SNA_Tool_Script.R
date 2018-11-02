#Social Network Analysis Tool Script (ICON 8002)
# 10/31/18

#####################################
########## Set up analysis ##########
#####################################
basedirectory <- "C:/Users/solit/Documents/GitHub/ICON8002_SNA"
input_datapath <- "C:/Users/solit/Documents/GitHub/ICON8002_SNA/Data" # folder where data and outputs are stored

# Input name of the data files that will be used in analysis
vertex_datapath <- "vertex_test_df_11_01_18.csv" # vertex (ego) dataframe with attributes
edge_indiv_datapath <- "edge_individual_test_df_11_01_18.csv" # edge-related data/attributes between individuals
edge_org_datapath <- "edge_org_test_df.csv" # edge between individuals and organizations

# set working directory to the data folder
setwd(input_datapath) 

# Install and load packages needed for the analysis

list.of.packages <- c("igraph","fabricatr", "keyplayer", "rmarkdown", "knitr", "RColorBrewer","sna","MASS","naturalsort", "pander") # List packages used for the analysis
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # new packages that were not already installed

# Install new packages
if(length(new.packages)){install.packages(new.packages)} 

# Load all packages for analysis
lapply(list.of.packages, require, character.only = TRUE)

#Load SNA function
# This is the function that will run the social network analysis, calculate relevant statistics, and produce figures
# that are helpful in visualizing the network (e.g. types of relationships between vertices, vertex attributes, etc.)
source("SNAfunction.R")

# Run SNA function
sna(input_datapath=input_datapath, vertex_datapath=vertex_datapath, edge_indiv_datapath=edge_indiv_datapath) #, keyplayer = TRUE)




######################################## Creating SNA function ####################################################

sna<-function(input_datapath, vertex_datapath, edge_indiv_datapath){#, edge_org_datapath){
  
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
  data.check<-sort(unique(as.character(vertex_df$ego))) == sort(unique(c(as.character(edge_indiv_df$ego),as.character(edge_indiv_df$alter))))
  
#if(any(data.check==FALSE))
 # stop("Please double check your data to make sure the egos and alters match") # Adding error message to tell users that ego/alter names do not match
  
  # We can just have R return which row entries are not equal using the syntax below:
  which(unique(sort(as.character(vertex_df$ego))) != sort(unique(c(as.character(edge_indiv_df$ego),as.character(edge_indiv_df$alter)))))

  
  ######################################
  ########## Network Analysis ##########
  ######################################
  
  # Constructing network and check network structure
  # Combine edge and vertex attribute information into igraph format
  graph_complete <- graph.data.frame(d = edge_indiv_df, vertices = vertex_df)
  
  #Symmetrize graph (network) to remove directed edges
  graph_complete_symmetrized <- as.undirected(graph_complete, mode='collapse')
  
  # Simplify graph (network) to remove loops and prepare variables
  graph_complete_simpl <- simplify(graph_complete)
  graph_complete_simpl_symm <- as.undirected(graph_complete_simpl, mode='collapse')

  # Summary of network
  summary(graph_complete)
  
  # Exporting edge list
  
  #setwd(basedirectory)
  #write.graph(graph_complete, file='graph_sna_full.txt', format="edgelist")
  
  
  ####################################################
  ########## Calculating network attributes ##########
  ####################################################
  
  # Vertex-related network attributes
  
  ## Degree centrality
  vertex_degree<-as.data.frame(igraph::degree(graph_complete))
  colnames(vertex_degree)<-"Degree"
  Degree_max_stat_indiv<-which.max(igraph::degree(graph_complete)) # individual with most degrees
  Degree_min_stat_indiv<-which.min(igraph::degree(graph_complete)) # individual with fewest degrees
  #write.csv(vertex_degree, file="SNA_Total_Degree.csv")
  
  in.degree<-igraph::degree(graph_complete,mode="in")
  #colnames(in.degree)<-"In Degree"
  #write.csv(in.degree, file="SNA_In_Degree.csv")
  
  
  ## Closeness centrality
  closeness<-as.data.frame(igraph::closeness(graph_complete))
  colnames(closeness)<-"Closeness"
  #write.csv(closeness, file="SNA_Closeness.csv")
  Closeness_max_stat_indiv<-which.max(igraph::closeness(graph_complete)) # individual with highest closeness measure
  Closeness_min_stat_indiv<-which.min(igraph::closeness(graph_complete)) # indidivudal with lowest closeness measure

  # Network attributes
  
  ## Diameter
  Diameter_stat_complete<-diameter(graph_complete)

  ## Reciprocity
  Reciprocity_stat_complete<-reciprocity(graph_complete)
  
  ## Edge density
  Density_stat_complete<-edge_density(graph_complete)
  
  ## Transitivity
  Transitivity_stat_complete<-transitivity(graph_complete)
  
  ## edge count
  ecount_stat_complete<-ecount(graph_complete)
  
  ## vertex count
  vcount_stat_complete<-vcount(graph_complete)

#   stats_overall_graph<-ls(pattern = "_stat_complete")
#   stat_overall_names<-vector()
  
  # for(k in 1:length(stats_overall_graph)){
  #   stat_overall_names<-c(stat_overall_names,strsplit(stats_overall_graph[k],"_")[[1]][1])
  # }
  # 
  # Stat_overall_table<-as.data.frame(cbind(as.vector(stat_overall_names),as.vector(mget(stats_overall_graph))),row.names = FALSE)
  # names(Stat_overall_table)<-c("Statistic Name","Value")
  
  ######################################
  ########## Graphing network ##########
  ######################################
  
  # Plotting original network
  ## Fruchterman.reingold fault layout, simplified (i.e., no loops), directed
  # plot.network.original <- plot(graph_complete_simpl,
  #                               layout = layout.fruchterman.reingold,
  #                               #edge.color=edge_test$connection,
  #                               edge.arrow.size=.1,
  #                               #vertex.color=profession.colors,
  #                               #vertex.size=((in.degree)*1.5),
  #                               vertex.size=5,
  #                               vertex.label=NA,
  #                               vertex.label.cex=0.7,
  #                               vertex.label.dist=1,
  #                               vertex.label.degree=-0.6,
  #                               main="Network Plot",
  #                               #frame=TRUE,
  #                               margin=0.0001)
  
  
  ## Nicely layout, simplified (i.e., no loops), directed

  original.nicely<-plot(graph_complete_simpl,
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
  professions<-sort(unique(V(graph_complete)$q1.profession.df.vq))
  
  layout.graph <- layout_(graph_complete_simpl, nicely())
  layout.graph<-norm_coords(layout.graph, ymin=-1, ymax=1, xmin=-1, xmax=1)
  
  colr.palette <- brewer.pal(n = length(professions), name = "Spectral") # picking color palette to graph with
  profession.colors <- colr.palette[vertex_df$q1.profession.df.vq] # Assigning colors to professions
  
  plot(graph_complete_simpl,
       layout = layout.graph,
       rescale = F,
       edge.arrow.size=.1,
       vertex.color=profession.colors,
       vertex.size=5,
       #vertex.label=V(graph_complete)$q1.profession.df.vq,
       vertex.label.cex=0.5,
       vertex.label.dist=1,
       vertex.label.degree=-0.6,
       main="SNA by Profession",
       #frame=TRUE,
       margin = 0.0001)

  legend(x=-1, y = -1, legend = professions,
         col = colr.palette, pch=19, pt.cex=0.8, cex=0.8, bty="n", ncol=2) # Adding legend to figure

  
  # Plotting by community/profession with vertices weighted by in.degree

   # plot(graph_complete_symmetrized,
   #      #edge.color=edge_test$connection,
   #      edge.arrow.size=.5,
   #      vertex.color=profession.colors,
   #      vertex.size=((in.degree)*1.5),
   #      vertex.label=V(graph_complete)$q1.profession.df.vq,
   #      vertex.label.cex=0.7,
   #      vertex.label.dist=1,
   #      vertex.label.degree=-0.6,
   #      main='SNA with vertices grouped by profession (color) and weighted by in-degree',
   #      #frame=TRUE,
   #      margin=0.0001)
   # 
   # legend(x=-1.1, y = -1.1, professions, pch=19,
   #        col= colr.palette, pt.cex=0.8, cex=0.8, bty="n", ncol=2) # Add legend to figure

  # Plot with vertices weighted by total degree

  plot(graph_complete_simpl,
       layout = layout.graph,
       rescale = F,
       edge.arrow.size=.1,
       vertex.color=profession.colors,
       vertex.size=igraph::degree(graph_complete_simpl),
       #vertex.label=V(graph_complete)$q1.profession.df.vq,
       vertex.label.cex=0.5,
       vertex.label.dist=1,
       vertex.label.degree=-0.6,
       main="SNA Vertices Weighted by Degree",
       #frame=TRUE,
       margin = 0.0001)
  legend(x=-1, y = -1, professions, pch=19,
         col= colr.palette, pt.cex=0.8, cex=0.8, bty="n", ncol=2)

  
  # Plot depicting how long vertices have worked with each other with specified cut-off 
  cut.off <- round(mean(edge_indiv_df$q14.wk.relationship.quality.eiq))
  graph_complete.years <- delete_edges(graph_complete, E(graph_complete)[q14.wk.relationship.quality.eiq
<cut.off])
  
  layout.graph.yrs.wk <- layout_(graph_complete.years, nicely())
  layout.graph.yrs.wk<-norm_coords(layout.graph.yrs.wk, ymin=-1, ymax=1, xmin=-1, xmax=1)
  

  plot(graph_complete.years,
       layout=(layout.graph.yrs.wk*1.1),
       rescale=F, 
       edge.arrow.size=.01,
       vertex.color=profession.colors,
       #vertex.size=((in.degree)*0.9),
       vertex.size=3,
       #vertex.label=vertex_df$profession.df,
       vertex.label=NA,
       vertex.label.cex=0.6,
       vertex.label.dist=1,
       vertex.label.degree=-0.6,
       main=paste0('Network of worked-together-with ',cut.off,' years connection'),
       margin=0.0001)
  legend(x=-1.1, y = -1.1, professions, pch=19,
         col= colr.palette, pt.cex=0.8, cex=0.8, bty="n", ncol=2)
  
  #Clustering function to add weights to edges with shared profession
  G_Grouped = graph_complete_symmetrized
  E(G_Grouped)$weight = 1
  professions.list<-unique(V(G_Grouped)$q1.profession.df.vq)
  ## Add edges with high weight between all nodes in the same group
  for(i in 1:length(professions.list)) {
    GroupV = which(V(G_Grouped)$q1.profession.df.vq == professions.list[i])
    G_Grouped = add_edges(G_Grouped, combn(GroupV, 2), attr=list(weight=1.5))
    #print(paste0("Ran loop for profession ",professions.list[i]))
  } 
  
  ## Now create a layout based on G_Grouped
  LO = layout_with_fr(G_Grouped)
  LO<-norm_coords(LO, ymin=-1, ymax=1, xmin=-1, xmax=1)

  plot(graph_complete_symmetrized,
       layout=(LO*1.0),
       rescale=F, 
       #edge.color=edge_test$connection,
       edge.arrow.size=.5,
       vertex.color=profession.colors,
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
       margin=0.01)
  legend(x=-1.1, y = -1.1, professions, pch=19,
         col= colr.palette, pt.cex=0.8, cex=0.8, bty="n", ncol=2)
  
  #Add graph of strong/weak connections between professional groups
  V(graph_complete_symmetrized)$q1.profession.df.vq
  E(graph_complete_symmetrized)$q1.profession.df.vq
  
  strength(graph_complete_symmetrized)
  graph_attr(graph_complete_symmetrized)
  
  E(graph_complete)[inc(V(graph_complete)[q1.profession.df.vq==professions.list[1]])]
  g2 <- subgraph.edges(graph_complete, E(graph_complete)[inc(V(graph_complete)[q1.profession.df.vq==professions.list[1]])])
  
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
        margin=0.0001)
  
  
  
  ####################################################
  ########## Identifying network keyplayers ##########
  ####################################################
  
  # if(keyplayer=TRUE){
   #Keyplayer functions:
   #Convert igraph object into matrix object, which can be read by sna package
   matrix_complete<-as.matrix(as_adjacency_matrix(graph_complete))
   
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
   ### Example Keyplayers - 11/1/18
   closeness_kp_num<-c(97,126,186)
   betweenness_kp_num<-c(31,153,196)
   degree_kp_num<-c(141,153,168)
   eigenvector_kp_num<-c(92,153,173)
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
   Overlap_vec<-Overlap_vec[duplicated(Overlap_vec)]
   print(sprintf("The egos identified as keyplayers via multiple Overlapping metrics are: %s. This suggests the role of a keyplayer through multiple functions.",paste(Overlap_vec,collapse="; ")))
   
   Metrics_list<-list(closeness_kp_names,betweenness_kp_names,degree_kp_names,eigenvector_kp_names,Overlap_vec)
   
   Closeness_vec<-c("Closeness",closeness_kp_names)
   Betweenness_vec<-c("Betweenness",betweenness_kp_names)
   Degree_vec<-c("Degree",degree_kp_names)
   Eigenvector_vec<-c("Eigenvector",eigenvector_kp_names)
   #Overlap_vec<-c("Overlap",Overlap_vec)
   
   Keyplayer_df<-as.data.frame(rbind(Closeness_vec,Betweenness_vec,Degree_vec,Eigenvector_vec),row.names = F)
   names(Keyplayer_df)<-c("Statistic",seq(1:keyplayer_num))
   
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
  # }
  
   
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
   
  
  ####################################
  ########## Network Output ##########
  ####################################

  # Output network attributes and figures in an RMarkdown document
  rmarkdown::render("Rmarkdown_test.Rmd","pdf_document")

}

# saving SNA function
dump("sna", file="SNAfunction.R")

############################################ End SNA function ####################################################

rm(list=ls())
