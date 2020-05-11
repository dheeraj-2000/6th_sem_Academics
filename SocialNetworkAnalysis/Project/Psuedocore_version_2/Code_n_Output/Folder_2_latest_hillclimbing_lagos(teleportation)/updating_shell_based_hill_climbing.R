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

#################################################### SHELL BASED HILL CLIMBING APPROACH
num_step <- c()
init_node<- c()
for (i in buckets[[1]]){
  numsteps = 0
  V(g)$core = coreness(g)
  V(g)$visited  = FALSE
  print("start node")
  init_node<- c(init_node,i)
  current <- i
  V(g)$visited[current] = TRUE
  while (!current %in% buckets[[length(buckets)]]) {
    
    
    #used randomness in case of large set
    print("current")
    print(current)
    print("coreness of current")
    print(V(g)$core[current])
    v1 = sampleWithoutSurprises(intersect(neighbors(g,current)[which(V(g)$core[neighbors(g,current)] == max(V(g)$core[neighbors(g,current)]))] , V(g)[which(V(g)$visited == FALSE)] ))
    if(length(v1)==0){
      v1 = sampleWithoutSurprises(V(g))
      while(V(g)$visited[v1] == TRUE)
      {
        print("inside random1")
        print(v1)
        v1 = sampleWithoutSurprises(V(g))
      }
    }
    
    if(V(g)$core[v1] <= V(g)$core[current]){
      
      v2 = sampleWithoutSurprises(intersect(neighbors(g,current), V(g)[which(V(g)$visited == FALSE)] ))
      if(length(v2) == 0)
      {
        v2 = sampleWithoutSurprises(V(g))
        while(V(g)$visited[v2] == TRUE)
        {
          print("inside random2")
          print(v2)
          print(V(g)$visited[v2])
          v2 = sampleWithoutSurprises(V(g))
        }
      }
      current = v2
      V(g)$visited[current] = TRUE
    }
    
    else{
    
      current = v1
      V(g)$visited[current] = TRUE
      
    }
    numsteps = numsteps + 1
    
  }
  print("numsteps")
  print(numsteps)
  num_step<-c(num_step,numsteps)
}
  
shedf<- cbind(init_node,num_step)
shedf
write.csv(shedf, "/home/samroadie/Desktop/sna_project/shell_based_steps.csv")
neighbors(g,2080)
##############################leakage power
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

#####
#dfs
#stack/queue to track all
#unable to go forward
#so go back
###############

#changing bound 
#changing iterations
###################
#number of nodes and leakage power
#density of shell and lekage power

#############################
#use leakage power to make hill climb algo
######
#####probability to moving to that shell proportional to leakage power of that shell

##########################
#DFS comparison with teleportation

###############################################################################################Degree based

num_step <- c()
init_node<- c()
for (i in buckets[[1]]){
  numsteps = 0
  V(g)$core = coreness(g)
  V(g)$visited  = FALSE
  print("start node")
  init_node<- c(init_node,i)
  current <- i
  V(g)$visited[current] = TRUE
  while (!current %in% buckets[[length(buckets)]]) {
    
    
    #used randomness in case of large set
    print("current")
    print(current)
    print("coreness of current")
    print(V(g)$core[current])
    v1 = sampleWithoutSurprises(intersect(neighbors(g,current)[which(V(g)$core[neighbors(g,current)] == max(V(g)$core[neighbors(g,current)]))] , V(g)[which(V(g)$visited == FALSE)] ))
    if(length(v1)==0){
      v1 = sampleWithoutSurprises(V(g))
      while(V(g)$visited[v1] == TRUE)
      {
        print("inside random1")
        print(v1)
        v1 = sampleWithoutSurprises(V(g))
      }
    }
    
    if(V(g)$core[v1] <= V(g)$core[current]){
      
      v2 <- intersect(neighbors(g,current)[which(degree(g)[neighbors(g,current)] == max(degree(g)[neighbors(g,current)]))],V(g)[which(V(g)$visited == FALSE)])
      if(length(v2) == 0)
      {
        v2 = sampleWithoutSurprises(V(g))
        while(V(g)$visited[v2] == TRUE)
        {
          print("inside random2")
          print(v2)
          print(V(g)$visited[v2])
          v2 = sampleWithoutSurprises(V(g))
        }
      }
      current = v2
      V(g)$visited[current] = TRUE
    }
    
    else{
      
      current = v1
      V(g)$visited[current] = TRUE
      
    }
    numsteps = numsteps + 1
    
  }
  print("numsteps")
  print(numsteps)
  num_step<-c(num_step,numsteps)
}
degshe <- cbind(init_node,numsteps)
