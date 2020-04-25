
library (igraph)

##############################################################import graph

# = read_graph ("/home/samroadie/Desktop/sna_project/karate.gml", format = "gml")

g = read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/Project/facebook_combined.txt", format = "edgelist", directed = F)
#G = read_graph("/home/samroadie/Desktop/sna_project/Friendship-network_data_2013.csv", format = "edgelist", directed = F)


V(g)
#g = G - V(G)[which(degree(G)==0)]
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
#yahan 1 ki jagah 16 kardena
for( l in 16:length(buckets)){
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
  print(pow)
}
#### select karke yahan tak chalana
length(pow)
#yeh tum lena 
#jahan tak loop chala us se ek kam n ki value
sm <-s[1:5]
df <- cbind(sm, pow)
df <- as.data.frame(df)
library(ggplot2)
ggplot(df,aes(x = sm,y = pow)) + geom_line(color = "red")

write.csv(df, "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/Project/temp16_20.csv")
  df
buckets
