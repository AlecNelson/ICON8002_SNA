sna <-
function(input_datapath, vertex_datapath, edge_datapath, keyplayer, network){
  
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
  edge_df <- read.csv(edge_datapath, header=T, row.names = 1)
  #edge_org_df <- read.csv(edge_org_datapath, header=T, row.names = 1)
  
  # Before we merge these data, we need to make sure 'ego' and 'alter' are the
  # same across data sets. We can compare each row using the == syntax. 
  # The command below should return TRUE for every row if all ego rows
  # are the same :
sort(unique(as.character(vertex_df$ego))) == sort(unique(c(as.character(edge_df$ego),as.character(edge_df$alter))))
  
  # We can have R return which row entries are not equal using the syntax below:
  which(unique(sort(as.character(vertex_df$ego))) != sort(unique(c(as.character(edge_df$ego),as.character(edge_df$alter)))))

  
  ######################################
  ########## Network Analysis ##########
  ######################################

  # Constructing network and check network structure
  # Combine edge and vertex attribute information into igraph format (four types of graphs)

  graph_complete <- graph.data.frame(d = edge_df, vertices = vertex_df) # Complete network without modifications
  graph_simpl <- simplify(graph_complete) # Simplified network to remove loops and prepare variables
  graph_symm<-as.undirected(graph_complete, mode='collapse') # symmetrized network with no directed connections
  graph_simpl_symm <- as.undirected(graph_simpl, mode='collapse') # network that is both simplified and symmetrized
  #network.opt<-c(graph_complete, graph_simpl, graph_symm, graph_simpl_symm)

  if(network=="complete"){
    soc.network<-graph_complete}
  else if(network=="simplified"){
    soc.network<-graph_simpl}
  else if(network=="symmetrized"){
    soc.network<-graph_symm}
  else if(network=="simpl.symm"){
    soc.network<-graph_simpl_symm}
  else {
    stop("Please indicate the type of network you would like for the analysis")
  }

#soc.network<-graph.data.frame(d=edge_df, vertices = vertex_df)
  # Summary of network
  # this command provides summary of the vertex and edge data frames, including the input attributes
  # for both vertices and edges
  summary(soc.network) 

  
  ####################################################
  ########## Calculating network statistics ##########
  ####################################################
  
  # Vertex-related network statistics
  
  ## Degree centrality
  vertex_degree<-igraph::degree(soc.network)
  Degree_max_stat_indiv<-which.max(igraph::degree(soc.network)) # individual with most degrees
  Degree_min_stat_indiv<-which.min(igraph::degree(soc.network)) # individual with fewest degrees
  
  
  in.degree<-igraph::degree(soc.network,mode="in")

  ## Closeness centrality
  closeness<-as.data.frame(igraph::closeness(soc.network))
  Closeness_max_stat_indiv<-which.max(igraph::closeness(soc.network)) # individual with highest closeness measure
  Closeness_min_stat_indiv<-which.min(igraph::closeness(soc.network)) # indidivudal with lowest closeness measure

  # Network attributes
  
  ## Diameter
  Diameter_stat_complete<-diameter(soc.network)

  ## Reciprocity
  Reciprocity_stat_complete<-reciprocity(soc.network)
  
  ## Edge density
  Density_stat_complete<-edge_density(soc.network)
  
  ## Transitivity
  Transitivity_stat_complete<-transitivity(soc.network)
  
  ## edge count
  ecount_stat_complete<-ecount(soc.network)
  
  ## vertex count
  vcount_stat_complete<-vcount(soc.network)

  ## Putting together statistics into a table
   stats_overall_graph<-ls(pattern = "_stat_complete")
   stat_overall_names<-vector()

   for(k in 1:length(stats_overall_graph)){
     stat_overall_names<-c(stat_overall_names,strsplit(stats_overall_graph[k],"_")[[1]][1])
   }
  
   Stat_overall_table<-as.data.frame(cbind(as.vector(stat_overall_names),as.vector(mget(stats_overall_graph))),row.names = FALSE)
   names(Stat_overall_table)<-c("Statistic Name","Value")
  
  ######################################
  ########## Graphing network ##########
  ######################################
  
  # Plotting original network
  # igraph provides many different layouts for visualizing the social network.
  # The layout chosen here is the "Nicely layout"
   layout.graph <- layout_(soc.network, nicely())
   layout.graph<-norm_coords(layout.graph, ymin=-1, ymax=1, xmin=-1, xmax=1)
   
plot(soc.network,
       layout = layout.graph,
       edge.arrow.size=.1,
       vertex.size=5,
       vertex.label=NA,
       vertex.label.cex=0.7,
       vertex.label.dist=1,
       vertex.label.degree=-0.6,
       main="Network Plot_Nicely",
       margin=0.0001)
  
  # Plot network with vertex colored based on profession
  ## Nicely layout, simplified (i.e., no loops), directed, vertices colored by profession
  professions<-sort(unique(V(soc.network)$q1.profession.df.vq))
    colr.palette <- brewer.pal(n = length(professions), name = "Spectral") # picking color palette to graph with
  profession.colors <- colr.palette[vertex_df$q1.profession.df.vq] # Assigning colors to professions
  
  plot(soc.network,
       layout = layout.graph,
       rescale = F,
       edge.arrow.size=.1,
       vertex.color=profession.colors,
       vertex.size=5,
       vertex.label=NA,
       vertex.label.cex=0.5,
       vertex.label.dist=1,
       vertex.label.degree=-0.6,
       main="Network Plot Vertices Colored by Profession",
       margin = 0.0001)

  legend(x=-1, y = -1, legend = professions,
         col = colr.palette, pch=19, pt.cex=0.8, cex=0.8, bty="n", ncol=2) # Adding legend to figure

  
  # Plot with vertices weighted by total degree

  plot(soc.network,
       layout = layout.graph,
       rescale = F,
       edge.arrow.size=.1,
       vertex.color=profession.colors,
       vertex.size=igraph::degree(soc.network),
       vertex.label=NA,
       vertex.label.cex=0.5,
       vertex.label.dist=1,
       vertex.label.degree=-0.6,
       main="Network Plot Vertices Weighted by Degree",
       margin = 0.0001)
  legend(x=-1, y = -1, professions, pch=19,
         col= colr.palette, pt.cex=0.8, cex=0.8, bty="n", ncol=2)

  # Plot depicting how long vertices have worked with each other with specified cut-off 
   cut.off <- round(mean(edge_df$q2.years.worked.with.eiq))
   graph.years.wk <- delete_edges(soc.network, E(soc.network)[q2.years.worked.with.eiq
 <cut.off])

   layout.graph.yrs.wk <- layout_(graph.years.wk, nicely())
   layout.graph.yrs.wk<-norm_coords(layout.graph.yrs.wk, ymin=-1, ymax=1, xmin=-1, xmax=1)


   plot(graph.years.wk,
        layout=(layout.graph.yrs.wk*1.1),
        rescale=F,
        edge.arrow.size=.01,
        vertex.color=profession.colors,
        vertex.size=3,
        vertex.label=NA,
        vertex.label.cex=0.6,
        vertex.label.dist=1,
        vertex.label.degree=-0.6,
        main=paste0('Network of worked-together-with ',cut.off,' years connection'),
        margin=0.0001)
   legend(x=-1.1, y = -1.1, professions, pch=19,
          col= colr.palette, pt.cex=0.8, cex=0.8, bty="n", ncol=2)

  #Clustering function to add weights to edges with shared profession
  G_Grouped = soc.network
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

  plot(soc.network,
       layout=(LO*1.0),
       rescale=F, 
       edge.arrow.size=.5,
       vertex.color=profession.colors,
       vertex.size=3,
       vertex.label=NA,
       vertex.label.cex=0.7,
       vertex.label.color= adjustcolor("black", 0.6),
       vertex.label.dist=1,
       vertex.label.degree=-0.6,
       main='Network Plot Professions Grouped',
       margin=0.01)
  legend(x=-1.1, y = -1.1, professions, pch=19,
         col= colr.palette, pt.cex=0.8, cex=0.8, bty="n", ncol=2)
  
  # #Add graph of strong/weak connections between professional groups
  # V(soc.network)$q1.profession.df.vq
  # E(soc.network)$q1.profession.df.vq
  # 
  # strength(soc.network)
  # graph_attr(soc.network)
  # 
  # E(soc.network)[inc(V(soc.network)[q1.profession.df.vq==professions.list[1]])]
  # g2 <- subgraph.edges(soc.network, E(soc.network)[inc(V(soc.network)[q1.profession.df.vq==professions.list[1]])])
  # 
  #  plot(g2,
  #       layout=layout_in_circle,
  #       rescale=T, 
  #       edge.color=adjustcolor("black", 0.1),
  #       edge.arrow.size=0.1,
  #       vertex.color=profession.colors,
  #       vertex.size=3,
  #       vertex.label=NA,
  #       vertex.label.cex=0.7,
  #       vertex.label.color= adjustcolor("black", 0.5),
  #       vertex.label.dist=1,
  #       vertex.label.degree=-0.6,
  #       main='Network Plot Strong and Weak Connections',
  #       margin=0.0001)
  
  
  
  ####################################################
  ########## Identifying network keyplayers ##########
  ####################################################
  
   if(keyplayer==TRUE){
   #Keyplayer functions:
   #Convert igraph object into matrix object, which can be read by sna package
   matrix_complete<-as.matrix(as_adjacency_matrix(soc.network))
   
   #Determine Keyplayers via different statistical mesurements
   ## Set size of set group to number of keyplayers wanted per metric
   
   
   keyplayer_num<-3
   processer_cores<-2
   
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
   print(proc.time() - ptm)
   ##################
   
   closeness_kp_num<-kp_closeness$keyplayers[1:keyplayer_num]
   betweenness_kp_num<-kp_betweenness$keyplayers[1:keyplayer_num]
   degree_kp_num<-kp_degree$keyplayers[1:keyplayer_num]
   eigenvector_kp_num<-kp_eigenvector$keyplayers[1:keyplayer_num]

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
   
   Keyplayer_names<-igraph::get.vertex.attribute(soc.network)$name[Keyplayer.list]
   
   ##### Plotting network with key player as identified by above model.
   layout.graph <- layout_(soc.network, nicely())
   layout.graph<-norm_coords(layout.graph, ymin=-1, ymax=1, xmin=-1, xmax=1)
   
   colrs <- c("blue", "green", "red","yellow","purple",adjustcolor("Gray60", alpha=.2))
   ego_names<-igraph::get.vertex.attribute(soc.network)$name
   ego_col<-rep(colrs[length(colrs)],length(ego_names))
   
   for(m in 1:length(Metrics_list)){
     colr_set_m<-which(ego_names %in% Metrics_list[[m]])
     ego_col[colr_set_m]<-colrs[m]
   }

   # Network graph with keyplayers
   
   plot(soc.network,
        layout=layout.graph,
        rescale=T,
        edge.color="Gray80",
        edge.arrow.size=.01,
        vertex.color=ego_col,
        vertex.size=ifelse((igraph::get.vertex.attribute(soc.network)$name %in% Keyplayer_names), 8, 4),
        vertex.label= ifelse((igraph::get.vertex.attribute(soc.network)$name %in% Keyplayer_names), as.character(igraph::get.vertex.attribute(soc.network)$name), NA),
        vertex.label.color = "blue",
        vertex.label=NA,
        vertex.label.cex=ifelse(vertex_df$ego == as.character(vertex_df$ego[177]), .4, NA),
        vertex.label.dist=4,
        vertex.label.degree=0,
        main='Network with highlighted Key Players',
        margin=0.0001)
   
   legend(x=-1, y = -0.7, c("Closeness","Betweenness","Degree","Eigenvector","Overlap"), pch=19,
          col= c("blue", "green", "red","yellow","purple"), pt.cex=1.5, cex=0.8, bty="n", ncol=1)
   
   
   matrix_complete<-as.matrix(as_adjacency_matrix(soc.network))
   
   data_logistic_df<-vertex_df
   
   Keyplay.bool=ifelse(rownames(data_logistic_df) %in% Keyplayer.list,1,0)
   data_logistic_df<-cbind(Keyplay.bool,data_logistic_df)
   colnames(data_logistic_df)
   
   
   #1. Update GLM-ready datasets to include summarized edge attributes
   
   edge_list<-as_edgelist(soc.network, names = TRUE)
   ego_list<-unique(edge_list[,1])
   edge_summary_df<-data.frame(matrix(ncol=(length(igraph::edge_attr_names(soc.network))+1),nrow=length(ego_list)))
   edge_summary_df[1]<-ego_list
   colnames(edge_summary_df)[1]<-"ego"
   colnames(edge_summary_df)[(1:length(igraph::edge_attr_names(soc.network)))+1]<-igraph::edge_attr_names(soc.network)
   
   for(i in 1:length(igraph::edge_attr_names(soc.network))){
     edge_var_i<-igraph::edge_attr_names(soc.network)[i]
     mode_class_i<-summary(igraph::get.edge.attribute(soc.network))[i,3]
     for(j in 1:length(ego_list)){
       #attr_edge_vector<-vector()
       attr_pos_j<-which(edge_list[,1]==ego_list[j])
       if(mode_class_i=="numeric"){
         edge_vect_i<-as.numeric(unlist(igraph::get.edge.attribute(soc.network)[i]))
         edge_vect_ij<-edge_vect_i[attr_pos_j]
         edge_val_ij<-mean(edge_vect_ij)
         edge_summary_df[j,(i+1)]<-edge_val_ij
         #attr_edge_vector<-c(attr_edge_vector,edge_val_ij)
         #print("Num: Subset edge vector by which rows by ego....") 
       }else if(mode_class_i=="character"){
         edge_vect_i<-as.character(unlist(igraph::get.edge.attribute(soc.network)[i]))
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
   # date.text<-substr(vertex_datapath,(nchar(vertex_datapath)-11),(nchar(vertex_datapath)-4))
   # vertex.df.keyplayer<-paste0("vertex_df_keyplayer_",date.text,".csv")
   #write.csv(Metrics_logistic_df,vertex.df.keyplayer)
   
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
   # str(Closeness_model)
   # Closeness_model$coefficients
   # Closeness_model$residuals
   # Closeness_model$fitted.values
   
   #closeness_CI<-CI(Closeness_model$effects,ci=0.95)
   
   }
  
  ####################################
  ########## Network Output ##########
  ####################################

  # Output network attributes and figures in an RMarkdown document
  if(keyplayer==TRUE){
    rmarkdown::render("SNA_RMarkdown_Output_with_Keyplayers.Rmd","word_document", paste("SNA_Output (with keyplayers) ",Sys.Date(), ".docx", sep = ""))}
   else{
    rmarkdown::render("SNA_RMarkdown_Output_without_Keyplayers.Rmd","word_document", paste("SNA_Output ",Sys.Date(), ".docx", sep = ""))
   }
}
