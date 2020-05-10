
library (igraph)

##############################################################import graph

# = read_graph ("/home/samroadie/Desktop/sna_project/karate.gml", format = "gml")

G = read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/Project/Psuedocore_version_2/Code_n_Output/Cascading/facebook_combined.txt", format = "edgelist", directed = F)
#G = read_graph("/home/samroadie/Desktop/sna_project/Friendship-network_data_2013.csv", format = "edgelist", directed = F)


V(G)
g = G - V(G)[which(degree(G)==0)]
#V(g)
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



s <- c()
pow <- c()

for( l in length(buckets)){
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
  print(inspow)
  print("ok4")
  }
  pow <- c(pow ,mean(inspow))
  #std <- c(std ,std(inspow))
  print(pow)
}
length(pow)
sm <-s
df <- cbind(sm, pow)
df <- as.data.frame(df)
df
library(ggplot2)
ggplot(df,aes(x = sm,y = pow)) + geom_line(color = "red")
df
write.csv(df, "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/Project/Psuedocore_version_2/Code_n_Output/Cascading/temp.csv")
ggplot(df,aes(x = sm,y = pow)) + geom_line(color = "red")

