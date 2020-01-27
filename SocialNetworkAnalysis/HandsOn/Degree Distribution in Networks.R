
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



