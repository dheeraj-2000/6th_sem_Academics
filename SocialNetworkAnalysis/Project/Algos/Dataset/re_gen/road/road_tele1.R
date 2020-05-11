library(igraph)
library(ggplot2)

degree_ <- c(2)
y1 <- c(
        1
)

shel <- c(1
          2
          3
          4
          5
          
)
y2 <- c(0.5985915493
        0.8521126761
        0.9436619718
        0.9788732394
        1
)
plot(degree_,y1,type="l",col="red", xlim=c(.9, 9), ylim = c(0.5, 1.01),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithm for Road network using dfs") + 
  lines(shel,y2,col="blue")
legend("bottomright", legend=c("Shell Based", "Degree based"),lty=1:1, cex=0.8 ,
       col=c("blue", "red"))
