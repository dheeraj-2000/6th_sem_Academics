library (igraph)

g = read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/Project/facebook_combined1.txt")
plot(g, layout = layout.kamada.kawai)
V(g)
max(degree(V(g)))
it = 1
c = coreness(g)
v = length(V(g))
v
buckets <- list()
while(v>0)
{
  
  if (it %in% c)
  {
    buckets = append(buckets,list(V(g)[which(coreness(g)==it)]))
    v = v - length(V(g)[which(coreness(g)==it)])
    print("v is ")
    print(v)
    it = it+1
  }
  
}

print(buckets)










"""

g= graph(c(1,2,3,11,4,5,5,6,5,7,5,8,5,9,5,10,10,11,10,13,11,13,12,14,12,15,13,14,13,15,13,16,13,17,14,15,14,16,15,16),directed=FALSE)
plot(g, layout = layout.kamada.kawai)
h = g
it = 1 
temp <- c()
buckets <- list()
while(1)

  {
  if(!it %in% degree(h))
  {
    it = it+1
    buckets <- append(B,list(temp)) 
    temp = c()
    
  }
  if(it %in% degree(h))
  {
    print(degree(h))
    for (i in V(h)[which(degree(h)==it)]){
        temp <- c(temp,i)
        
    }
     print(temp)
     h = h - V(h)[which(degree(h)==it)]
  }
  if(gorder(h) == 0){
    break
  }
  
}
print(buckets)





while length(V(g) != 0){ 

{
  for(i in degree(g) {
    
    if (count==0)
    {
      B[i] = 
    }
  
   
  
       # bd$i = c(bd$i,V(g)[which(degree(g)==i)])
       g = g - V(g)[which(degree(g)==16)]
  }
  
  B = c(B,bd$i)
  
}
}
  

#g = g - V(g)[which(degree(g)==16)
#V(g)[which(degree(g)==16)]
bd = c()
i=1
bd$i = 2     
bd$i
V(g)[which(degree(g)==2)]

B[1] <- c(B,list(V(g)[which(degree(g)==2)])
B[2]<-6
B[6]<-5
B[3]<-list(V(g)[which(degree(g)==2)])
B     
s <- c(B[[3]])
s
s <- c(s,29)
append(s,28)
s.append(s,5)
B.insert(4)
V(g)[which(degree(g)==2)]
B[[1]] = c(B[[1]],28)


a <- list()
b <- list()
append(a,b)
a
B[]
h

"""



