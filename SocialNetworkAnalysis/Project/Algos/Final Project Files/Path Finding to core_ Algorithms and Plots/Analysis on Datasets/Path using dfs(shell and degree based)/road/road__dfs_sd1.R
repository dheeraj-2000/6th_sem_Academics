library(igraph)
library(ggplot2)

x1 <- c(1,
        2,
        3,
        4,
        5,
        6,
        7)
y1 <- c(0.5985915493,
        0.8309859155,
        0.9084507042,
        0.9225352113,
        0.9507042254,
        0.9577464789,
        0.9647887324)

x2 <- c(1,
        2,
        3,
        4,
        5,
        6,
        7)
y2 <- c(0.5985915493,
        0.8028169014,
        0.8943661972,
        0.9295774648,
        0.9647887324,
        0.9718309859,
        0.985915493)
plot(x1,y1,type="l",col="red", xlim=c(.9, 9), ylim = c(0.5, 1.01),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithm for Road network using dfs") + 
  lines(x2,y2,col="blue")
legend("bottomright", legend=c("Shell Based", "Degree based"),lty=1:1, cex=0.8 ,
       col=c("blue", "red"))
