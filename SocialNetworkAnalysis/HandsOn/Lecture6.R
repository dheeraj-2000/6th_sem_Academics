
library(igraph)
g1 <- graph( edges=c(1,2, 2,3, 3, 1), n=3, directed=F ) 

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


g6 <- make_empty_graph(num = 100)
for(i in 1:(num-1)){
  for(j in (i+1):num){
      
}}

g6 = graph(c(1,2,2,3,1,3), directed =F)
for(i in 1:100){
  
  
}
degree(g6)[1]/sum(degree(g6))
plot(g6)


generate_graph <- function(num, connections){
  g6 = make_empty_graph(n = num)
  for(i in )
}



gx = generate_graph(50, 2)

g <- sample_degseq(rep(2,100))
degree(g)
is_simple(g)   # sometimes TRUE, but can be FALSE
g2 <- sample_degseq(1:10, 10:1)
degree(g2, mode="out")
degree(g2, mode="in")
plot(g2)
