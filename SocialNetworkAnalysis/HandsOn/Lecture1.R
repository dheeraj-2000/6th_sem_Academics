library(igraph)
g =  make_empty_graph(n = 100)
i= 1
while(i<100){
  g = g+ edge(i, i+1)
  i=i+1
}
tkplot(g , layout = layout.grid, vertex.size=20, edge.arrow.size=.5, vertex.color = "green",  vertex.label.color = "black", edge.arrow.color = "white")
