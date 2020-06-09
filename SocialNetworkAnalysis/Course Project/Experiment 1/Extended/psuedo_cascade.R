
library (igraph)

##############################################################import graph



g = read_graph("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Course Project/Final Submitted report/Code/Datasets Used/Facebook friendship Network/facebook_combined.txt", format = "edgelist", directed = F)



g=G
g = G - V(G)[which(degree(G)==0)]
V(g)
#plot(g, layout = layout.kamada.kawai)
#degree(g)

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






##########################################################



ic <- function(g,s){
  jst_inf <- c(s)
  infected <- c(s)
  while(1){
    if (length(jst_inf)==0)
      return(infected)
    temp <- c()
    for(i in jst_inf){
      for (j in neighbors(g,i)) {
        r= runif(1)
        if (r<0.5 && (!j %in% infected) && (!j %in% temp)){
          
          temp <- c(temp,j)
        }
      }
    }
    for (k in temp){
      infected <- c(infected,k)
    }
    
    jst_inf <- temp
  }
}



#s <- c()
inspow <- c()
node <- c()
  for (m in buckets[[25]]){
    print(m)
    node <- c(node,m)
    seed <- m
    print("ok1")
    list1 = ic(g,seed)
    print("ok2")
    len <- length(list1)
    print("ok3")
    inspow <- c(inspow,len)
    print(inspow)
    print("ok4")
  }




df <- cbind(node, inspow)
df <- as.data.frame(df)
boxplot(df$inspow)
library(ggplot2)
ggplot(df,aes(x = sm,y = pow)) + geom_line(color = "red")
df
write.csv(df, "/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Course Project/Experiment 1/Extended/temp16_39.csv")
df <- read.csv("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Course Project/Experiment 1/Extended/temp15.csv")

