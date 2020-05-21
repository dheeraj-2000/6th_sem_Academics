



library (igraph)

##############################################################import graph

#G = read_graph ("/home/samroadie/Desktop/sna_project/karate.gml", format = "gml")

G = read_graph("/home/samroadie/Desktop/sna_project/facebook_combined.txt", format = "edgelist", directed = F)
#G = read_graph("/home/samroadie/Desktop/sna_project/Friendship-network_data_2013.csv", format = "edgelist", directed = F)


V(G)
g = G - V(G)[which(degree(G)==0)]


#####################################################Bucketing
it = 1
c = coreness(g)
v = length(V(g))
i = 1
buckets <- list()
while(v>0)
{
  
  if (it %in% c)
  {
    tempo = c()
    tempo = c(tempo,V(g)[which(coreness(g)==it)])
    buckets[[i]] = tempo
    v = v - length(V(g)[which(coreness(g)==it)])
    print("v is ")
    print(v)
    i=i+1
  }
  it = it+1
}

print(buckets)
############################################################# Random  Number Generation
sampleWithoutSurprises <- function(x) {
  if (length(x) <= 1) {
    return(x)
  } else {
    return(sample(x,1))
  }
}

#####################leakage power




leak_pow <- c()
shell <- c()
for (i in 1:length(buckets)){
  shell <- c(shell,i)
  max_jump <- c()
  for (j in buckets[[i]])
  {
    if((max(V(g)$core[neighbors(g,j)]) - V(g)$core[j])>0){
      maxjump =  max(V(g)$core[neighbors(g,j)]) - V(g)$core[j]
    }
    else
    {
      maxjump = 0
    }
    max_jump <- c(max_jump,maxjump)  
  }
  leak_pow <- c(leak_pow,mean(max_jump))
}




leak_pow
shell
scaled <- leak_pow/max(leak_pow)
ldf <- cbind(shell,scaled)
ldf
ldf <- as.data.frame(ldf)
ggplot(ldf,aes(x = shell,y = scaled)) + geom_line(color = "blue")
leak_pow/max(leak_pow)
