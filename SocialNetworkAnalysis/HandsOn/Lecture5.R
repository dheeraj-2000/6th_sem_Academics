library(igraph)
library(png)
g = mkae_empty_graph(n = 50)
i =1
while(i<=49){
  g = g + edge(i, i+1)
  i = i+1
  fname = "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/plot/plot"
  png(paste(fname, i, ".png", sep = ""),600, 600)
  plot (g, vertex.color = "green", edge.color = "red", edge.arrow.size = 0.5, layout = layout_with_kk, rescale=FALSE, xlim=c(-5,5), ylim=c(-5,5), vertex.size=70, vertex.label.color = "black")
  dev.off()
}