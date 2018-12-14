#Analysis Model Data Processing Functions for SNA
# Alec Nelson
# 12/14/18

############################
#Type in the base directory and input datapaths below
input_datapath <- 

vertex_datapath <- 
edge_indiv_datapath <-

setwd(input_datapath)

#List packages used
list.of.packages <- c("igraph","randomNames","fabricatr","plyr","RColorBrewer","keyplayer","sna","MASS","naturalsort","stringr","Rmisc","rstudioapi")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages)} 

#Load all packages
lapply(list.of.packages, require, character.only = TRUE)
########################################################
########################################################

vertex_df <- read.csv(vertex_datapath, header=T, row.names = 1)
edge_indiv_df <- read.csv(edge_indiv_datapath, header=T, row.names = 1)

#Combine edge and vertex attribute information into igraph format
graph_complete <- graph.data.frame(d = edge_indiv_df, vertices = vertex_df)
#Symmetrize graph to remove directed edges
graph_complete_symmetrized <- as.undirected(graph_complete, mode='collapse')
# Simplify graph to remove loops and prepare variables
graph_complete_simpl <- simplify(graph_complete)
graph_complete_simpl_symm <- as.undirected(graph_complete_simpl, mode='collapse')

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

#Determine Keyplayers via different statistical mesurements
## Set size of set group to number of keyplayers wanted per metric
keyplayer_num<-3
processer_cores<-2

kp_closeness<-kpset(matrix_complete,size=keyplayer_num,type="closeness",parallel=TRUE,cluster=processer_cores,method="min")
kp_betweenness<-kpset(matrix_complete,size=keyplayer_num,type="betweenness",parallel=TRUE,cluster=processer_cores,method="min")
kp_degree<-kpset(matrix_complete,size=keyplayer_num,type="degree",cmode="total",parallel=TRUE,cluster=processer_cores,method="max")
kp_eigenvector<-kpset(matrix_complete,size=keyplayer_num,type="evcent",parallel=TRUE,cluster=processer_cores,method="max")

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
########################################################

Keyplayer_names<-igraph::get.vertex.attribute(graph_complete_simpl)$name[Keyplayer.list]

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

Total.sum.bool=Closeness.bool+Betweenness.bool+Degree.bool+Eigenvector.bool

Metrics_logistic_df<-cbind(Closeness.bool,Betweenness.bool,Degree.bool,Eigenvector.bool,Overlap.bool,Total.sum.bool,data_logistic_df)
colnames(Metrics_logistic_df)

########################################################
########################################################

#Select particular attributes to test in models
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