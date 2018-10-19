sna <-
function(input_datapath, vertex_datapath, edge_datapath){
  
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
  
  #######################################
  ######### Network Analysis ############
  #######################################
  
  # Constructing network and check network structure
  network <- graph.data.frame(d = edge, vertices = vertex)
  summary(network)
  
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

  #########################################
  ############ Network Output #############
  #########################################

  rmarkdown::render("Rmarkdown_test.Rmd","pdf_document")
 
}
