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