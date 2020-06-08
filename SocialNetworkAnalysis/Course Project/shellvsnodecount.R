library (igraph)
g = read_graph("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Course Project/Final Submitted report/Code/Datasets Used/Facebook friendship Network/facebook_combined.txt", format = "edgelist", directed = F)

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

NodeCount <- c()
shell <- c()
for (i in 1:length(buckets)){
  print
  
}