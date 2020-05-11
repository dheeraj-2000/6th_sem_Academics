
# IMPORTING DATASETS

g = read_graph("/home/samroadie/Desktop/sna_project/facebook_combined.txt", format = "edgelist", directed = F)

dat <- read.table("/home/samroadie/Desktop/sna_project/dataset/fb-pages-food.edges", skip=1, sep=",")
g <- graph_from_data_frame(dat)

g <- read_graph("/home/samroadie/Desktop/sna_project/karate.gml",format = "gml")

g <- read_graph("/home/samroadie/Desktop/sna_project/dataset/weblog.txt", format = "edgelist", directed = F)

g <- read_graph("/home/samroadie/Desktop/sna_project/dataset/road.txt", format = "edgelist", directed = F)

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
##############################################################
num_step <- c()
init_node<- c()
for (i in buckets[[1]]){
  numsteps = 0
  V(g)$core = coreness(g)
  V(g)$visited  = FALSE
  print("start node")
  init_node<- c(init_node,i)
  current <- i
  print(current)
  stack <- c(current)
  flag = 0
  while (length(stack) != 0 ) {
    current = stack[length(stack)]
    stack <- stack[-length(stack)]
    if(V(g)$visited[current] == FALSE ){
      V(g)$visited[current] = TRUE }
    
    if(current %in% buckets[[length(buckets)]]){
      flag = 1
      break
    }
    
    #used randomness in case of large set
    print("current")
    print(current)
    print("coreness of current")
    print(V(g)$core[current])
    v1 <- intersect(neighbors(g,current)[which(V(g)$core[neighbors(g,current)] == max(V(g)$core[neighbors(g,current)]))] , V(g)[which(V(g)$visited == FALSE)] )
   
    print(length(v1))
    if(length(v1) == 0)
    {
      print("inside")
      if(length(stack)==0)
      {break}
      current = stack[length(stack)]
      stack <- stack[-length(stack)]
      V(g)$visited[current] = TRUE 
      #v1 <- intersect(neighbors(g,current)[which(V(g)$core[neighbors(g,current)] == max(V(g)$core[neighbors(g,current)]))] , V(g)[which(V(g)$visited == FALSE)] )
      v1 <- intersect(neighbors(g,current)[which(V(g)$core[neighbors(g,current)] == max(V(g)$core[neighbors(g,current)]))] , V(g)[which(V(g)$visited == FALSE)] )
      numsteps = numsteps+1
      while(length(v1)==0){
        if(length(stack)==0)
        { 
          flag = 2
          break
          
        }
        current = stack[length(stack)]
        stack <- stack[-length(stack)]
        V(g)$visited[current] = TRUE 
        
        v1 <- intersect(neighbors(g,current)[which(V(g)$core[neighbors(g,current)] == max(V(g)$core[neighbors(g,current)]))] , V(g)[which(V(g)$visited == FALSE)] )
        numsteps = numsteps+1
        print(" yaa i am backtracking")
      }
      if (flag == 2)
      {
        break
      }
    }
    
    
    
    if(V(g)$core[sampleWithoutSurprises(v1)] <= V(g)$core[current]){
      
      v2 <- intersect(neighbors(g,current), V(g)[which(V(g)$visited == FALSE)] )
      
      stack<-c(stack,v2)
    }
    
    else{
      stack <- c(stack,v1)
    }
    
    
    numsteps = numsteps + 1
    
  }
  print("numsteps")
  print(numsteps)
  if (flag == 1){
    num_step<-c(num_step,numsteps)
  }
  else{
    num_step<-c(num_step,NA)
  }
}

shedf2<- cbind(init_node,num_step)
shedf2
##############################################################################################Degree based
num_step <- c()
init_node<- c()
for (i in buckets[[1]]){
  numsteps = 0
  V(g)$core = coreness(g)
  V(g)$visited  = FALSE
  print("start node")
  init_node<- c(init_node,i)
  current <- i
  stack <- c(current)
  flag = 0
  while (length(stack)!=0) {
    
    current = stack[length(stack)]
    stack <- stack[-length(stack)]
    V(g)$visited[current] = TRUE
    
    if(current %in% buckets[[length(buckets)]]){
      flag = 1
      break
    }
    
    
    print("current")
    print(current)
    print("coreness of current")
    print(V(g)$core[current])
    v1 <- intersect(neighbors(g,current)[which(V(g)$core[neighbors(g,current)] == max(V(g)$core[neighbors(g,current)]))] , V(g)[which(V(g)$visited == FALSE)] )
    print(length(v1))
    if(length(v1) == 0)
    {
      print("inside")
      if(length(stack)==0)
      {break}
      current = stack[length(stack)]
      stack <- stack[-length(stack)]
      V(g)$visited[current] = TRUE 
      v1 <- intersect(neighbors(g,current)[which(V(g)$core[neighbors(g,current)] == max(V(g)$core[neighbors(g,current)]))] , V(g)[which(V(g)$visited == FALSE)] )
      numsteps = numsteps+1
      while(length(v1)==0){
        if(length(stack)==0)
        {flag =2
        break}
        current = stack[length(stack)]
        stack <- stack[-length(stack)]
        V(g)$visited[current] = TRUE 
        v1 <- intersect(neighbors(g,current)[which(V(g)$core[neighbors(g,current)] == max(V(g)$core[neighbors(g,current)]))] , V(g)[which(V(g)$visited == FALSE)] )
        numsteps = numsteps+1
        print(" yaa i am backtracking")
      }
      if(flag == 2)
      {break}
    }
    
    
    if(V(g)$core[sampleWithoutSurprises(v1)] <= V(g)$core[current]){
      print("inside if")
      v2 <- intersect(neighbors(g,current)[which(degree(g)[neighbors(g,current)] == max(degree(g)[neighbors(g,current)]))],V(g)[which(V(g)$visited == FALSE)])
      print("v2")
      print(v2)
      print(length(v2))
      
      
      stack<-c(stack,v2)
    }
    
    else{
      
      stack<-c(stack,v1)
      
    }
    numsteps = numsteps + 1
    
  }
  print("numsteps")
  print(numsteps)
  if (flag == 1){
    num_step<-c(num_step,numsteps)
  }
  else{
    num_step<-c(num_step,NA)
  }
  
}
degshe <- cbind(init_node,num_step)
##############################################################################################################