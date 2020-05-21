
library (igraph)

##############################################################import graph


# IMPORTING DATASETS

#g = read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/Dataset/facebook_combined.txt", format = "edgelist", directed = F)


#g <- read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/Dataset/weblog.txt", format = "edgelist", directed = F)

#g <- read_graph("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/email.txt", format = "edgelist", directed = F)




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






########################################################## CASCADING POWER



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



s <- c()
pow <- c()
std <- c()
for( l in 1:length(buckets)){
  s <- c(s,l)
  print(l)
  inspow <- c()
  for (m in buckets[[l]]){
    print(m)
    seed <- m
    print("ok1")
    list1 = ic(g,seed)
    print("ok2")
    len <- length(list1)
    print("ok3")
    inspow <- c(inspow,len)
    
    print("ok4")
  }
  pow <- c(pow ,mean(inspow))
  std <- c(std ,sd(inspow))
  print(pow)
  print(std)
}
df <- cbind(s, pow ,std)
df
df <- as.data.frame(df)
#write.csv(df, "/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Project/Algos/leak_plot/fb.csv")

