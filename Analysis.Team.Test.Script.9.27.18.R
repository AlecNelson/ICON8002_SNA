## igraph.test
#10/3/18

## input data structure
## 2 data sheets (csv files), 1 for vertices, 1 for connections/edges
## this seems to be common practice
## attributes can be numeric and characters, continuous and categorical


getwd()
setwd("/Users/alecnelson/Documents/UGA_Files/Fall_2018/ICON_8002")

vertex_test<- read.csv("vertex_test.csv",header=T)
edge_test<- read.csv("attribute_test1.csv",header=T)

str(vertex_test)
summary(vertex_test)

str(edge_test)
summary(edge_test)

icon.graph <- graph.data.frame(d = edge_test, vertices = vertex_test)
summary(icon.graph)
icon.graph
V(icon.graph)


get.edge.attribute(icon.graph,'beer_gift')

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

get.edge.attribute(icon.graph, 'beer')

plot(icon.graph,edge.color=edge_test$high_five,edge.width=edge_test$high_fives,edge.arrow.size=.2,main='ICON High-Fives (weighted)')

## network metrics
degree(icon.graph)
diameter(icon.graph)
closeness(icon.graph)
reciprocity(icon.graph)
ecount(icon.graph)
vcount(icon.graph)
edge_density(icon.graph)

icon.graph[]
