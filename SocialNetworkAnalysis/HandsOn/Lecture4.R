library(igraph)
#friendship_df = read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/karate.txt", sep = " ")

graph <- watts.strogatz.game(2,100,5, 0.05)
plot(graph, layout = layout_on_grid)

V(graph)
E(graph)
transitivity(graph)
diameter(graph)

#random graph 
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
  return (g6)
}
g6 = make_random(45, 0.3)
g6
plot(g6)
deg_vec = igraph::degree(g6)
deg_vec
plot(table(deg_vec))
