library (igraph)
library(ggplot2)


#####################################################Bucketing
G = read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/Project/facebook_combined1.txt", format = "edgelist", directed = F)
g = G - V(G)[which(degree(G)==0)]
length(V(g))
#plot(g, layout = layout.kamada.kawai)
it = 1
c = coreness(g)
v = length(V(g))
i = 1
buckets <- list()
while(v>0){
  
  if (it %in% c)
  {
    tempo = c()
    tempo = c(tempo,V(g)[which(coreness(g)==it)])
    buckets[[i]] = tempo
    v = v - length(V(g)[which(coreness(g)==it)])
    #print("v is ")
    print(length(V(g)[which(coreness(g)==it)]))
    i=i+1
    gplot(aes(x = it,y = length(V(g)[which(coreness(g)==it)])))
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


##################################################################

g = read_graph ("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/Project/karate.gml", format = "gml")
plot(g, layout = layout.kamada.kawai)
#################################################### SHELL BASED HILL CLIMBING APPROACh
V(g)$core = coreness(g) 
V(g)$visited  = FALSE
numsteps = 0
current <- sampleWithoutSurprises(buckets[[1]])
print(current)
V(g)$visited[current] = TRUE
while (!current %in% buckets[[length(buckets)]]) {
  
  print(current)
  print(V(g))
  v1 = sampleWithoutSurprises(intersect(neighbors(g,current)[which(V(g)$core[neighbors(g,current)] == max(V(g)$core[neighbors(g,current)]))] , V(g)[which(V(g)$visited == FALSE)] ))
  
  
  if(V(g)$core[v1] <= V(g)$core[current]){
    v2 = sampleWithoutSurprises(intersect(neighbors(g,current),V(g)[which(V(g)$visited == FALSE)]))
    current = v2
  }
  
  else{
    current = v1
  }
  numsteps = numsteps + 1
  
}
print(numsteps)

###########################################Intershell Hill Climbing with Intrashell Degree Based Approach(SA)


V(g)$core = coreness(g) 
V(g)$visited  = FALSE
numsteps = 0
current = sampleWithoutSurprises(buckets[[1]])

V(g)$visited[current] = TRUE
while (!current %in% buckets[[length(buckets)]]) {
  print(current)
  v1 = sampleWithoutSurprises(intersect(neighbors(g,current)[which(V(g)$core[neighbors(g,current)] == max(V(g)$core[neighbors(g,current)]))] , V(g)[which(V(g)$visited == FALSE)] ))
  
  if(V(g)$core[v1] <= V(g)$core[current]){
    v2 = sampleWithoutSurprises(intersect(neighbors(g,current)[which(degree(g)[neighbors(g,current)] == max(degree(g)[neighbors(g,current)]))],V(g)[which(V(g)$visited == FALSE)]))
    current = v2
  }
  
  else{
    current = v1
  }
  numsteps = numsteps + 1
  
}
print(numsteps)
#######################################################################################################################################











