library(igraph)
G = graph(c(1,"A",2,"B",3,"C",2,"A",1,"C",4,"B"),directed =F)
plot(G,layout=layout.random, edge.color = "red")
V(G)$type = c(T,F,T,F,T,F,T)
V(G)$color = ifelse(V(G)$type , "lightblue","salmon")
V(G)$shape = ifelse(V(G)$type , "square","circle")
plot(G,layout=layout.bipartite, edge.color = "red")


g = graph(c(1,2,1,3,2,5,4,2,3,4,4,5))
write_graph(g, "/home/dheeraj/mygraph.csv", format = "edgelist")
g2 = read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/facebook_combined.txt", format = "edgelist")
tkplot (g2, vertex.color = "green", edge.color = "red", edge.arrow.size = 1, layout = layout_with_kk, rescale=FALSE, xlim=c(-5,5), ylim=c(-5,5), vertex.size=15, vertex.label.color = "black")


actors_df = read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/Actors.csv")
actors_df
relations_df = read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/relations.csv")
relations_df
relations_df = relations_df[,c(2,3)]
actors_df = actors_df[,c(2,3,4)]
actors_df
g <- graph_from_data_frame(relations_df,directed = TRUE,vertices = actors_df)
plot(g)

edf = as_data_frame(g , what = "edges")
vdf = as_data_frame(g , what = "vertices")
write.csv(edf, "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/edges.csv" )
write.csv(vdf, "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/vertices.csv" )

G= read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/karate.gml",format="gml")
tkplot (G, vertex.color = "green", edge.color = "blue")
library(png)
img1 = readPNG("")
V(g)$raster = replicate(vcount(g), img1, simplify = T)
plot(g, vertex.shapes(roa), vertex.lable = NA)

#assortativity
#transitivity
#Connectedness = diameter
#Igraph