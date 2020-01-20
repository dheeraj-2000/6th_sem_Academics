library(igraph)
number <- c()
probability <- c()
for(i in 1:100){
  g6 = make_empty_graph(i)
  for(j in 1:100){
    prob = j/100
    for(k in 1:(i-1)){
      for(l in (k+1):i){
        r = runif(1)
        if(r<=prob){
          g6 = g6+edge(k,l)
        }
      }
    }
    if(is.connected(g6)){
      number <- c(number, i)
      probability <- c(probability, prob)
      break
    }
  }
}
plot(number, probability,main="n vs p graph", type = "l", xlim=c(0,100), col="red")

     legend("topright",
            c("n vs p "),
            fill=c("red")
     )




