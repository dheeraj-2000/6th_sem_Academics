library(igraph)
g = read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/Friendship-network_data_2013.csv", format = "edgelist")
V(g)
E(g)
plot (g, vertex.label = NA, vertex.color = "green", reciprocity= TRUE, edge.color = "red", edge.arrow.size = 0.5, layout = layout_with_kk, rescale=FALSE, xlim=c(-5,5), ylim=c(-5,5), vertex.size=2, vertex.label.color = "black")
reciprocity(g)               #check friendship between both
g1 = as.undirected(g, mode = "mutual")
V(g1)
E(g1)
tkplot (g1, vertex.label = NA, vertex.color = "green", reciprocity= TRUE, edge.color = "red", edge.arrow.size = 0.5, layout = layout_with_kk, rescale=FALSE, xlim=c(-5,5), ylim=c(-5,5), vertex.size=2, vertex.label.color = "black")
components(g1)
comps = decompose(g1)
comps
tkplot(comps[[2]])

g2 = read.delim("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/metadata_2013.txt", sep = "\t")
metadata_df = read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/metadata_2013.csv", sep = "\t")
friendship_df = read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/Friendship-network_data_2013.csv", sep = " ")
metadata_df = metadata_df[,c(1,3)]
friendship_df = friendship_df[,c(1,2)]
g <- graph_from_data_frame(friendship_df,directed = TRUE,vertices = metadata_df[3])
tkplot (g, vertex.color = "green", reciprocity= TRUE, edge.color = "red", edge.arrow.size = 0.5, layout = layout_with_kk, rescale=FALSE, xlim=c(-5,5), ylim=c(-5,5), vertex.size=20, vertex.label.color = "black")

#friendship network

make_random <- function(num, prob){
  g6 = make_empty_graph(n = num)
  for(i in 1:(num-1)){
    for(j in (i+1):num){
      r = runif(1)
         if(r<=prob){
             g6 = g6+edge(i,j)
      }
    }
  }
  return (g6.is.connected)
}
#r1 = make_random(10, 0.3)
#r1
#plot(r1)
library(igraph)
pl <- function(n){
   v_size <- vector()
   prob <- vector()

   for(i in 1:n){
     v = i+1
     for(j in 1:100){
        p = (j+1)/100
        check = is_connected (v, p)
        if (is.connected (v, p)){
            v_size.append (v)
            prob.append (p)
            break()}
}}}

r1 = pl (v_size, prob)
plot_nVSp (100)

