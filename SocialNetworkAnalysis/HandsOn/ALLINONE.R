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
tesla = comps[[2]]
tkplot(tesla)

g3 = delete.vertices (g1, V(g1) [V(g1)$degree == 0])
tkplot (g1, vertex.size = 2, vertex.label = NA, edge.color = "red", vertex.color = "green", edge.width = 0.5, layout = layout_with_kk)

# Plot degree distribution
t = table(degree(g3))
plot(t / sum(t),xlab="Degree", ylab="Frequency")

g2 = read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/metadata_2013.txt", header = F, sep = "\t")
V(g)
E(g)

V(g1)$degree = igraph::degree (g1)
V(g1)$degree

#getting betweeness
V(g1)$betweeness = igraph::betweenness(g1)
V(g1)$betweeness

#getting closeness
V(g1)$closeness = igraph::closeness(g1)
V(g1)$closeness 

Topdeg = order(V(g1)$degree,decreasing=T)[1:5]
Topclose = order(V(g1)$closeness,decreasing=T)[1:5]
Topbw = order(V(g1)$betweenness,decreasing=T)[1:5]

Top = intersect(Topdeg,Topclose)
Top = intersect(Top,Topbw)

plot (g1, mark.groups=Top, mark.col="yellow", vertex.size = 2, vertex.label = NA, edge.color = "red", vertex.color = "black", edge.width = 0.5, layout = layout_with_kk)

V(g1)[g1 [,1]]$gender = g1 [,3]
# Delete vertices which do not have gender specified
g4 = delete.vertices (g1, V(g1)[is.na(V(g1)$gender)])
g4 = delete.vertices (g4, V(g4)$gender == "3")

V(g4)$type = ifelse(V(g4)$gender == "1", T , F)
# plot the undirected graph based on gender: Male (blue), Female (red)
V(g4)$color = ifelse (V(g4)$type, "blue", "red")
tkplot (g4, vertex.size = 5, vertex.label = NA, edge.color = "black", vertex.color = V(g4)$color, edge.width = 0.5, layout = layout_with_kk)

  


V(tesla)$degree = igraph::degree (tesla)
V(tesla)$degree

#getting betweeness
V(tesla)$betweeness = igraph::betweenness(tesla)
V(tesla)$betweeness

#getting closeness
V(tesla)$closeness = igraph::closeness(tesla)
V(tesla)$closeness 

tesla_dist = table (V(tesla)$degree)
plot (tesla_dist, xlab = "degree", ylab = "frequency")
