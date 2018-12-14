#Social Network Analysis and Keyplayer Tool (SNAaKT)
#Created by: ICON8002 2018 Cohort
#Last updated: 12/13/2018

#####################################
########## Set up analysis ##########
#####################################

# Input name of the data files that will be used in analysis
vertex_datapath <- "vertex_df.csv" # vertex (ego) dataframe with attributes
edge_datapath <- "edge_df.csv" # edge-related data/attributes between individuals

# Install and load packages needed for the analysis
list.of.packages <- c("igraph","fabricatr", "keyplayer", "rmarkdown", "knitr", "RColorBrewer","sna","MASS","naturalsort", "pander") # List packages used for the analysis
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])] # new packages that were not already installed

# Install new packages
if(length(new.packages)){install.packages(new.packages)} 

# Load all packages for analysis
lapply(list.of.packages, require, character.only = TRUE)

##################################
########## SNA function ##########
##################################

# Load sna function
# This is the function that will run the social network analysis, calculate relevant statistics, and produce figures
# that are helpful in visualizing the network (e.g. types of relationships between vertices, vertex attributes, etc.)
source("SNAfunction.R")

# Run sna function
sna(vertex_datapath=vertex_datapath, edge_datapath=edge_datapath, keyplayer = TRUE, network="simplified")




