library(ggplot2)
V1 = c(1,2,3,4)
V2 = c(5,6,7,8)

df = cbind.data.frame(V1, V2)
df
ggplot(df, aes(x = V1, y = V2)) + geom_point()
V3 = c(7,8,9,10)
df = cbind.data.frame(df, V3)
ggplot(df, aes(x = V1, y = V2, size = V2)) + geom_point()
ggplot(df, aes(x = V1, y = V2, size = V3)) + geom_point()
ggplot(df, aes(x = V1, y = V2, color = V3)) + geom_point()
ggplot(df, aes(x = V1, y = V2, color = V3, size = V3 )) + geom_point(shape = 24)



library(igraph)
Rfb1 = erdos.renyi.game(4039,0.0108)
Rfb2 = erdos.renyi.game(4039,0.03)
d1 <- table(degree(Rfb1))
d2 <- table(degree(Rfb2))
dgree1 <- cbind.data.frame(d1)
dgree2 <- cbind.data.frame(d2)
dgree2

#degree.df$degree <- as.numeric(as.character(degree.df$degree))
ggplot(dgree1, aes(x = Var1, y = Freq )) + geom_point() + geom_point(data = dgree2, color = 'red')


#degree centrality ,  

#Cd(i) = sigma Aij
#Cd*(i) = sigma Aij/ n-1

#closeness centrality, 
#Cc = 1/ d12+ d13 + d14
 #Cc(i) = 1/d(ij)
 
#betweeness

g = read_graph("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/SocialNetworkAnalysis/HandsOn/karate.gml", format = "gml")
V(g)$degree = igraph::degree(g)
V(g)[2]$degree
order(V(g)$degree)
V(g)[12]$gegree
order((V(g)$degree), decreasing = T)

top_deg <- order((V(g)$degree), decreasing = T)[1:5]
top_deg
V(g)$closeness = igraph::closeness(g)

topclose = order((V(g)$closeness), decreasing = T)[1:5]
topclose

V(g)$betweeness = igraph::betweenness(g)
topbw = order((V(g)$betweeness), decreasing = T)[1:5]
topbw

topnodes = intersect(intersect(top_deg, topclose), topbw)
topnodes

plot(g, mark.groups = topnodes, mark.col = 'green')
plot(g, mark.groups = (c(1,2), c(3,4)), mark.col = 'green')
plot(g, mark.groups = c((1,2), (3,4)), mark.col = 'green')




Hi

hey

arr = [12, 1, 3, 5, 89, 24]
max = arr[0];
for (int i = 1; i< sizeof(arr); i++){
  if(arr[i] >max)
    max = arr[i];
  else 
    continue;
}


swap(int a, int b){
  int temp = a
  a = b
  b = temp;
  
}
