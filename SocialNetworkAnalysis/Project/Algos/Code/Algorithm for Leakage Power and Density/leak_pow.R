# IMPORTING DATASETS

#g = read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/Dataset/facebook_combined.txt", format = "edgelist", directed = F)


#g <- read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/Dataset/weblog.txt", format = "edgelist", directed = F)

#g <- read_graph("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/email.txt", format = "edgelist", directed = F)

#g <- read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/Dataset/road.txt", format = "edgelist", directed = F)

############################################################################################################
####Preprocessing

g = g - V(g)[which(degree(g)==0)]
g
V(g)

######################################################################################################### Bucketing
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
###################################################################################### Leakage Power
leak_pow <- c()
shell <- c()
V(g)$core = coreness(g)
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
leakage_power <- leak_pow
################################### density calculation and plot
shellno <- c()
density <- c()
nodecount<- c()
edgecount<- c()
for (i in 1:length(buckets)){
  g1 <- induced_subgraph(g,buckets[[i]] , impl = c("auto"))
  shellno <- c(shellno,i)
  ndcnt <- length(V(g1))
  nodecount <- c(nodecount,ndcnt)
  edgcnt <- length(E(g1))
  edgecount<- c(edgecount,edgcnt)
  max_edges = 1
  if(ndcnt > 1){
    max_edges = (ndcnt*(ndcnt-1))/2}
  densitycurr = edgcnt /max_edges
  density <- c(density , densitycurr)
}
density
nodecount
edgecount

######################################################### plotting
df <- cbind(shellno,nodecount,density,leakage_power)
df
df = as.data.frame(df)
df

#write.csv(df, "/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/leak_plot/fb.csv")
#write.csv(df, "/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/leak_plot/web.csv")
#write.csv(df, "/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/leak_plot/emails.csv")


library(ggplot2)
ggplot(df,aes(x = shellno,y = leakage_power)) + geom_line(color = "blue") + ggtitle("Plot of leakage power vs shellno for email network")

ggplot(df, aes(x=nodecount, y=leakage_power)) + geom_point() + ggtitle("Plot of leakage power vs nodecount for email network")


ggplot(df, aes(x=density, y=leakage_power)) + geom_point(shape = 3) + ggtitle("Plot of leakage power vs density for email network")








