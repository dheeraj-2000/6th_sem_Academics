library(igraph)
library(ggplot2)

df <- read.csv("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Course Project/Experiment 1/Extended/weblog/shell10/shell_10_nodes_pow.csv")
df

node <- df$x

y_.1 <- df$at_point_10
y_.25 <- df$at_point_25
y_.40 <- df$at_point_40
y_.50 <- df$at_point_50
y_.80 <- df$at_point_80
min(node)
min(y_.1)
max(node)
max(y_.80)


lablist<-lablist<-as.vector(c(1:126))
lablist
#f781bf pink
#a65628 brown
#ff7f00 orange
#984ea3 purple
#4daf4a green 
#377eb8 blue
plot(node, y_.1, type="o", pch = 20, col="#f781bf", xlim=c(1, 7), ylim = c(-250, 621), xlab="Nodes in shell no. 10", ylab="Diffusion Capacity",
     main = "Diffusion capacity of each 7 Nodes in shell 10 \n(considered as Pseudocore) of Weblog network \n [for given p values, min=1, max=611]")
  #ticks = temp
  #ticks
  #box()
  #axis(side = 1, at = ticks, labels=ticks, cex.axis=1.05)
  #text(srt = 45, xpd=TRUE)
  #axis(1, at=seq(1, 126, by=1))
  #text(seq(1, 126, by=1), par("usr")[3] - 0.2, labels= lablist, srt = 45, pos = 1, xpd = TRUE)
  #axis(side = 2)
  lines(node, y_.25, type="o",pch = 20, col="#a65628") + 
  lines(node, y_.40, type="o",pch = 20, col="green") + 
  lines(node, y_.50, type="o",pch = 20, col="red")
  lines(node, y_.80, type="o",pch = 20, col="#377eb8")
legend("bottomright", legend=c("at p = 0.80", "at p = 0.50", "at p = 0.40", "at p = 0.25", "at p = 0.10"),lty=1:1, cex=0.8,lwd =1.8,
       col=c("#377eb8", "red", "green", "#a65628", "#f781bf"))



