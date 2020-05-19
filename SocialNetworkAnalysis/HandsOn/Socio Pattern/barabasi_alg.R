library(igraph)

############################## using inbuilt function  #################################

G = barabasi.game (100, directed = F)
t = table (degree (G))
#plot (t, xlab = "Degree", ylab = "Frequency", main="Barabsi-Albert SFN using barabasi.game", edge.color = "red")
tkplot(G,layout = layout_with_kk)

#################### using algorithm ############################################
make_random <- function(i ,n , G){
  for (k in i:n){
    for(j in 1:k-1){
      j=j+1
      degseq = degree(G)[j]/sum(degree(G))
      r=runif(1)
      if(r< degseq){
        G = G  + edges(k,j)
      }
    }}
  return(G)
}
G = make_empty_graph(100)
G = G + edges(c(1,2,2,3,3,1))
i=4
G = make_random(i,100,G)
tkplot(G, layout=layout.grid)
df = table(degree(G))
plot(df, xlab = "Degree", ylab = "Frequency")

