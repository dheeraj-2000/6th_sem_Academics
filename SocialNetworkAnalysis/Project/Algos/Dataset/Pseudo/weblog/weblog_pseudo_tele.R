library(igraph)
library(ggplot2)

degree_ <- c(1,4
)
y1 <- c(
        1,1
        
)

shel <- c(1,
          2,
          3,
          4
          
)
y2 <- c(0.6896551724,
        0.9252873563,
        0.9885057471,
        1
)
plot(degree_,y1,type="l",col="red", xlim=c(.9, 5), ylim = c(0.5, 1.01),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithm for weblog using Teleportation (Pseudocore)") + 
  lines(shel,y2,col="blue")
legend("bottomright", legend=c("Degree based", "Shell Based"),lty=1:1, cex=0.8 ,
       col=c("red", "blue"))


########################### dfs

degree_ <- c(1,
             2,
             3,
             4
)
y1 <- c(
  0.6896551724,
  0.9310344828,
  0.9942528736,
  1
  
)

shel <- c(1,
          2,
          3,
          4,
          5
          
)
y2 <- c(0.6896551724,
        0.9252873563,
        0.9712643678,
        0.9770114943,
        1
)
plot(degree_,y1,type="l",col="red", xlim=c(.9, 5), ylim = c(0.6, 1.01),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithm for weblog using dfs (Pseudocore)") + 
  lines(shel,y2,col="blue")
legend("bottomright", legend=c("Degree based", "Shell Based"),lty=1:1, cex=0.8 ,
       col=c("red", "blue"))
