
#degree distribution for random network

library(igraph)
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
g6 = make_random(200, 0.3)
g6
plot(g6)
deg_vec = igraph::degree(g6)
deg_vec
plot(table(deg_vec))



#degree distribution for real world facebook network


library(igraph)
g2 = read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/degree distribution/fb1.txt",format = "edgelist")
t = table(degree(g2))
plot(t / sum(t),xlab="Degree", ylab="Frequency")


#degree distribution for scale free network


library(igraph)
G = read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/fb1.txt",format = "edgelist")
t = table(degree(G))
plot(t / sum(t),xlab="Degree", ylab="Frequency")
sample_pa(n, power = 1, m = NULL, out.dist = NULL, out.seq = NULL,
          out.pref = FALSE, zero.appeal = 1, directed = TRUE,
          algorithm = c("psumtree", "psumtree-multiple", "bag"),
          start.graph = NULL)
g <- sample_pa(10000)
degree_distribution(g)
t = table(degree(g))
plot(t / sum(t),xlab="Degree", ylab="Frequency")

