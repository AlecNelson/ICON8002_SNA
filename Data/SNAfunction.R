sna <-
function(input_datapath, vertex_datapath, edge_indiv_datapath, edge_org_datapath){
  
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
  
if(any(data.check==FALSE))
  stop("Please double check your data to make sure the egos and alters match") # Adding error message to tell users that ego/alter names do not match
  
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
  write.csv(vertex_degree, file="SNA_Total_Degree.csv")
  
  in.degree<-as.data.frame(igraph::degree(graph_complete,mode="in"))
  colnames(in.degree)<-"In Degree"
  write.csv(in.degree, file="SNA_In_Degree.csv")
  
  
  ## Closeness centrality
  closeness<-as.data.frame(igraph::closeness(graph_complete))
  colnames(closeness)<-"Closeness"
  write.csv(closeness, file="SNA_Closeness.csv")
  Closeness_max_stat_indiv<-which.max(igraph::closeness(graph_complete)) # individual with highest closeness measure
  Closeness_min_stat_indiv<-which.min(igraph::closeness(graph_complete)) # indidivudal with lowest closeness measure

  # Network attributes
  
  ## Diameter
  diameter<-diameter(graph_complete)
  
  ## reciprocity
  reciprocity<-reciprocity(graph_complete)
  
  ## edge count
  ecount<-ecount(graph_complete)
  
  ## vertex count
  vcount<-vcount(graph_complete)
  
  ## edge density
  edge_density<-edge_density(graph_complete)


  
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
                                main="Network Plot",
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
  
  colr.palette <- brewer.pal(n = length(unique(sort(vertex_df$profession.df))), name = "Spectral") # picking color palette to graph with
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
  
  legend(x=-1.5, y = -0.85, legend = levels(vertex_df$profession.df), 
         col = colr.palette, pch=19, pt.cex=0.8, cex=0.8, bty="n", ncol=1) # Adding legend to figure
  
  ###### Need to double check if colors match professions! #######
  
  
  # Plotting by community/profession with vertices weighted by in.degree
  V(graph_complete_symmetrized)$community <- vertex_df$profession.df
  colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen","blue","pink","green","purple"), alpha=.6)
  
  # plot(graph_complete_symmetrized,
  #      #edge.color=edge_test$connection,
  #      edge.arrow.size=.5,
  #      vertex.color=colrs[V(graph_complete_symmetrized)$community],
  #      vertex.size=((in.degree)*1.5),
  #      vertex.color=colrs,
  #      #vertex.size=((in.degree)*1.5),
  #      vertex.size=3,
  #      #vertex.label=vertex_df$profession.df,
  #      vertex.label=NA,
  #      vertex.label.cex=0.7,
  #      vertex.label.dist=1,
  #      vertex.label.degree=-0.6,
  #      main='SNA with vertices grouped by profession (color) and weighted by in-degree',
  #      #frame=TRUE,
  #      margin=0.0001)
  # 
  # legend(x=-1.5, y = -0.85, unique(vertex_df$profession), pch=19,
  #        col= colrs[V(graph_complete_symmetrized)$community], pt.cex=0.8, cex=0.8, bty="n", ncol=1) # Add legend to figure
  
  # Plot with vertices weighted by total degree

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
