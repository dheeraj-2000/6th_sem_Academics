library (igraph)

##############################################################import graph

#g = read_graph ("/home/samroadie/Desktop/sna_project/karate.gml", format = "gml")

G = read_graph("/home/samroadie/Desktop/sna_project/facebook_combined.txt", format = "edgelist", directed = F)
#G = read_graph("/home/samroadie/Desktop/sna_project/Friendship-network_data_2013.csv", format = "edgelist", directed = F)


V(G)
g = G - V(G)[which(degree(G)==0)]
V(g)
plot(g, layout = layout.kamada.kawai)
degree(g)

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

#################################################### SHELL BASED HILL CLIMBING APPROACh


k = length(buckets[[1]])
r = 0
j =1 
initisb_node <- c()
steps <- c()
while(j<=k){
V(g)$core = coreness(g) 
check = 0
V(g)$visited  = FALSE
numsteps = 0
current <- buckets[[1]][j]
initisb_node <- c(initisb_node,current)
print("current is")
print(current)
V(g)$visited[current] = TRUE
#assumption last node is core
#if use psuedocore then might be a problem
while (!current %in% buckets[[length(buckets)]]) {
  
 
  #used randomness in case of large set
  print(current)
  v1 = sampleWithoutSurprises(intersect(neighbors(g,current)[which(V(g)$core[neighbors(g,current)] == max(V(g)$core[neighbors(g,current)]))] , V(g)[which(V(g)$visited == FALSE)] ))
  if(length(v1)==0 || length(current)==0){
    check = 1
    numsteps =0
    break
  }
  
  if(V(g)$core[v1] <= V(g)$core[current]){
    
    v2 = sampleWithoutSurprises(intersect(neighbors(g,current),V(g)[which(V(g)$visited == FALSE)] ))
    V(g)$visited[current] = TRUE
    current = v2
    #print(current)
    print("insideif")
    #print(coreness(g)[current])
  }
  
  else{
    prev = current
    current = v1
    V(g)$visited[current] = TRUE
    print("else")
    #print(current)
    #print(coreness(g)[current])
    
  }
  numsteps = numsteps + 1
  
}
#print(numsteps)
if (check == 1 && numsteps == 0)
{
   check = 0
   steps <- c(steps,NA)
   numsteps = 0
}
else
{
    print("number of steps is")
    print(numsteps)
    steps <- c(steps ,numsteps)
    r= r+1
    numsteps = 0
}

j = j + 1

}

print(r)














###########################################Intershell Hill Climbing with Intrashell Degree Based Approach(SA)


k = length(buckets[[1]])
r = 0
j =1 
initisdb_node <- c()
dsteps <- c()
while(j<=k){
  V(g)$core = coreness(g) 
  check = 0
  numsteps = 0
  current <- buckets[[1]][j]
  initisdb_node <- c(initisdb_node,current)
  print("current is")
  print(current)
  V(g)$visited  = FALSE
  V(g)$visited[current] = TRUE
while (!current %in% buckets[[length(buckets)]]) {
  #print(current)
  v1 = sampleWithoutSurprises(intersect(neighbors(g,current)[which(V(g)$core[neighbors(g,current)] == max(V(g)$core[neighbors(g,current)]))] , V(g)[which(V(g)$visited == FALSE)] ))
  #print(v1)
  
  if(length(v1)==0 || length(current)==0){
    check=1
    numsteps=0
    break
  }
  
  if(V(g)$core[v1] <= V(g)$core[current]){
    
    v2 <- intersect(neighbors(g,current)[which(degree(g)[neighbors(g,current)] == max(degree(g)[neighbors(g,current)]))],V(g)[which(V(g)$visited == FALSE)])
   # print(v2)
   # print("inside if")
    current = v2
    V(g)$visited[current] = TRUE
    #print(current)
  }
  
  else{
    current = v1
    V(g)$visited[current] = TRUE
    #print(current)
    #print("inside else")
  }
  if(length(current)==0){
    check=1
    numsteps=0
    break
  }
  
  
  numsteps = numsteps + 1
  
}
  #print(numsteps)
  if (check == 1 && numsteps == 0)
  {
    check = 0
    dsteps <- c(dsteps,NA)
    numsteps = 0
  }
  else
  {
    print("number of steps is")
    dsteps <- c(dsteps,numsteps)
    print(numsteps)
    r= r+1
    numsteps = 0
  }
  
  j = j + 1
  
}

print(r)

#######################################################################################################################################OLD METHODS













