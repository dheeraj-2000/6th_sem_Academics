library(igraph)
library(png)
m = make_empty_graph(n = 50)
i =1
while(i<=49){
  m = m + edge(i, i+1)
  i = i+1
  fname = "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/plot/plot"
  png(paste(fname, i, ".png", sep = ""),600, 600)
  plot(m, layout= layout.grid)
  dev.off()
}


make_random <- function(num, prob){
  g6 = make_empty_graph(n = num)
  for(i in 1:(num-1)){
    for(j in (i+1):num){
      r = runif(1)
      if(r<=prob){
        g6 = g6+edge(i,j)
        fname = "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/plot2/plot"
        png(paste(fname, i, ".png", sep = ""),600, 600)
        plot(g6, layout= layout.grid)
        dev.off()
      }
    }
  }
  return (g6)
}
g6 = make_random(45, 0.3)


library(igraph)
make_random <- function(num, prob){
  g6 = make_empty_graph(n = num)
  for(i in 1:(num-1)){
    for(j in (i+1):num){
      r = runif(1)
      if(r<=prob){
        g6 = g6+edge(i,j)
        if(degree(g6)[i] >=1 && degree(g6)[i]<=2)
          V(g6)$color[i] = "red"
        if(degree(g6)[i] >=3 && degree(g6)[i]<=5)
          V(g6)$color[i] = "blue"
        else
          V(g6)$color[i] = "yellow"
        fname = "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/plot2/plot"
        png(paste(fname, i, ".png", sep = ""),600, 600)
        plot(g6, layout= layout.grid, vertex.size = igraph::degree(g6)*3)
        dev.off()
      }
    }
  }
  return (g6)
}
g6 = make_random(50, 0.1)


