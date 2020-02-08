library(igraph)
g = read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/Friendship-network_data_2013.csv", format = "edgelist")
V(g)
E(g)
tkplot (g, vertex.label = NA, vertex.color = "green", reciprocity= TRUE, edge.color = "red", edge.arrow.size = 0.5, layout = layout_with_kk, rescale=FALSE, xlim=c(-5,5), ylim=c(-5,5), vertex.size=2, vertex.label.color = "black")
reciprocity(g) 

g1 = as.undirected(g, mode = "mutual")
V(g1)   #vertex for undirected
E(g1)
tkplot (g1, vertex.size = 2, vertex.label = NA, edge.color = "red", vertex.color = "green", edge.width = 0.5, layout = layout_with_kk)
components(g1)
comps = decompose(g1)
comps
tkplot(comps[[2]])

g3 = delete.vertices (g1, V(g1) [V(g1)$degree == 0])
tkplot (g1, vertex.size = 2, vertex.label = NA, edge.color = "red", vertex.color = "green", edge.width = 0.5, layout = layout_with_kk)

# Plot degree distribution
t = table(degree(g3))
plot(t / sum(t),xlab="Degree", ylab="Frequency")

g2 = read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/metadata_2013.txt", header = F, sep = "\t")
V(g)
E(g)

