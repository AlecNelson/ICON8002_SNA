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
########################################################
########################################################
