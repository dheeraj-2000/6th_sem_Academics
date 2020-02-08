library(igraph)
#####################READING THE FRIENDSHIP GRAPH############################33
g = read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/Friendship-network_data_2013.csv", format = "edgelist")
V(g)                      # GET THE NUMBER OF VERTICES
E(g)                      # GET THE NUMBER OF EDGES
tkplot (g, vertex.label = NA, vertex.color = "green", reciprocity= TRUE, edge.color = "red", edge.arrow.size = 0.5, layout = layout_with_kk, rescale=FALSE, xlim=c(-5,5), ylim=c(-5,5), vertex.size=2, vertex.label.color = "black")
###############  PROPERTY ####
reciprocity(g) 


################ MAKING THE GRAPH AS A UNDIRECTED ################33
g1 = as.undirected(g, mode = "mutual")
V(g1)   #vertex for undirected
E(g1)   # GET THE NUMBER OF EDGES FOR UNDIRECTED
tkplot (g1, vertex.size = 2, vertex.label = NA, edge.color = "red", vertex.color = "green", edge.width = 0.5, layout = layout_with_kk)

################ COMPONENTS IN THE UNDIRECTED GRAPH ####################3
components(g1)
comps = decompose(g1)
comps
tesla = comps[[2]]
tkplot(tesla)


# Plot degree distribution FOR UNDIRECTED GRAPH G1
t = table(degree(g1))
plot(t / sum(t),xlab="Degree", ylab="Frequency")


############################ READING METADATA GRAPH ################################
g2 = read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/metadata_2013.txt", header = F, sep = "\t")
V(g)                 #VERTICES IN THE METADATA
E(g)                  #EDGES IN THE METADATA


V(g1)$degree = igraph::degree (g1)
V(g1)$degree

#################### FOR UNDIRECTED GRAPH G1
#getting betweeness OF UNDIRECTED GRAPH G1
V(g1)$betweeness = igraph::betweenness(g1)
V(g1)$betweeness

#getting closeness
V(g1)$closeness = igraph::closeness(g1)
V(g1)$closeness 

###################### GETTING TOP NODES FOR BETWEEN,CLOSENESS AND DEGREE ###################
Topdeg = order(V(g1)$degree,decreasing=T)[1:5]
Topclose = order(V(g1)$closeness,decreasing=T)[1:5]
Topbw = order(V(g1)$betweenness,decreasing=T)[1:5]

Top = intersect(Topdeg,Topclose)
Top = intersect(Top,Topbw)

################### PLOT WHILE CONISDERING ALL ABOVE 3 CASES
plot (g1, mark.groups=Top, mark.col="yellow", vertex.size = 2, vertex.label = NA, edge.color = "red", vertex.color = "black", edge.width = 0.5, layout = layout_with_kk)

#################### LABELLING MALE AND FEMALE TO THE FRIENDSHIP GRAPH
for(i in g2 [,1]){
  if(!(i %in% V(g))){
    print(i)
    remove <- which(g2$V1==i)
    print(remove)
    g2 = g2 [-remove,]
    
  }
}
g2
V(g1)[g2 [,1]]$gender = g2 [,3]
V(g1)$gender
# Delete vertices which do not have DESCRIPTION ABOUT GENDER
g4 = delete.vertices (g1, V(g1)[is.na(V(g1)$gender)])
g4 = delete.vertices (g4, V(g4)$gender == "3")

V(g4)$type = ifelse(V(g4)$gender == "1", T , F)
# plot the undirected graph based on gender: Male (blue), Female (red)
V(g4)$color = ifelse (V(g4)$type, "blue", "red")
V(g4)$type
plot (g4, vertex.size = 5, vertex.label = NA, edge.color = "black", vertex.color = V(g4)$color, edge.width = 0.5, layout = layout_with_kk)

  

################# GET THE CLOSENESS, BETWEENESS TOPNODES OF COMPONENT (GRAPHNEMAE TESLA) ##########
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

# Finding top nodes using all the three attributes
Topdeg = order(V(tesla)$degree,decreasing=T)[1:5]
Topclose = order(V(tesla)$closeness,decreasing=T)[1:5]
Topbw = order(V(tesla)$betweenness,decreasing=T)[1:5]
Top = intersect(Topdeg,Topclose)
Top = intersect(Top,Topbw)

plot (g, mark.groups=Top, mark.col="yellow", vertex.size = 2, vertex.label = NA, edge.color = "red", vertex.color = "green", edge.width = 0.5, layout = layout_with_kk)

