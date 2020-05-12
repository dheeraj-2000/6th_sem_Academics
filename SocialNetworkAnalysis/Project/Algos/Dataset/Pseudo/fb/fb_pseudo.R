library(igraph)
library(ggplot2)

degree_ <- c(7,37
)
y1 <- c(
  1,1
  
)

shel <- c(1,
          2,
          3,
          4,
          5,
          6,
          9,
          12,
          19,
          22,
          31,
          37
          
)
y2 <- c(0.4666666667,
        0.8266666667,
        0.88,
        0.8933333333,
        0.9066666667,
        0.92,
        0.9333333333,
        0.9466666667,
        0.96,
        0.9733333333,
        0.9866666667,
        1
)
plot(degree_,y1,type="l",col="red", xlim=c(.9, 38), ylim = c(0.5, 1.01),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithm for facebook using Teleportation (Pseudocore)") + 
  lines(shel,y2,col="blue")
legend("bottomright", legend=c("Degree based", "Shell Based"),lty=1:1, cex=0.8 ,
       col=c("red", "blue"))


########################### dfs

degree_ <- c(1,
             2,
             3,
             8
)
y1 <- c(
  0.4666666667,
  0.8266666667,
  0.88,
  0.9066666667
  
)

shel <- c(1,
          2,
          3,
          62,
          97
          
)
y2 <- c(0.4666666667,
        0.8266666667,
        0.88,
        0.9066666667,
        1
)
plot(degree_,y1,type="l",col="red", xlim=c(.9, 98), ylim = c(0.4, 1.01),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithm for Facebook using dfs (Pseudocore)") + 
  lines(shel,y2,col="blue")
legend("bottomright", legend=c("Degree based", "Shell Based"),lty=1:1, cex=0.8 ,
       col=c("red", "blue"))
