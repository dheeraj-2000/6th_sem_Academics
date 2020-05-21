# IMPORTING DATASETS

g = read_graph("/home/samroadie/Desktop/sna_project/facebook_combined.txt", format = "edgelist", directed = F)

#g = read_graph("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/Final Project Files/Datasets Used/Facebook/facebook_combined.txt", format = "edgelist", directed = F)

#g <- read_graph("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/email.txt", format = "edgelist", directed = F)


#g <- read_graph("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/Final Project Files/Datasets Used/Weblog/weblog.txt", format = "edgelist", directed = F)

#g <- read_graph("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/Final Project Files/Datasets Used/Road Network/road.txt", format = "edgelist", directed = F)



############################################################################################################
####Preprocessing

g = g - V(g)[which(degree(g)==0)]
g
V(g)

#########################################################################################################Bucketing
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
#############################################################shell based
V(g)$core = coreness(g)
max_iter = V(g)$core[buckets[[25]]]
max_iter
num_step <- c()
init_node<- c()
for (i in buckets[[1]]){
  numsteps = 0
  V(g)$core = coreness(g)
  V(g)$visited  = FALSE
  print("start node")
  comeout = 0
  init_node<- c(init_node,i)
  current <- i
  V(g)$visited[current] = TRUE
  while (TRUE) {
    
    print("current")
    print(current)
    print("coreness of current")
    print(V(g)$core[current])
    
    for(i in max_iter:length(buckets)){
      
      if(current %in% buckets[[i]]){
        comeout = 1
        break
      }
      
    }
    if(comeout == 1)
    {
      break
    }
    #used randomness in case of large set
    
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
shell_based <- cbind(init_node,num_step)
shell_based
#########################################################################################################degree_based
V(g)$core = coreness(g)
max_iter = V(g)$core[buckets[[25]]]
max_iter
num_step <- c()
init_node<- c()
for (i in buckets[[1]]){
  numsteps = 0
  V(g)$core = coreness(g)
  V(g)$visited  = FALSE
  print("start node")
  init_node<- c(init_node,i)
  comeout = 0
  current <- i
  V(g)$visited[current] = TRUE
  while (TRUE) {
    
    print("current")
    print(current)
    print("coreness of current")
    print(V(g)$core[current])
    
    for(i in max_iter:length(buckets)){
      
      if(current %in% buckets[[i]]){
        comeout = 1
        break
      }
      
    }
    if(comeout == 1)
    {
      break
    }
    
    #used randomness in case of large set
   
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
degree_shell <- cbind(init_node,numsteps)
degree_shell
