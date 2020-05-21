library(igraph)
library(ggplot2)

x_ <- c(1,
        2,
        3,
        4,
        5,
        6,
        7)
y_ <- c(0.5985915493,
        0.8309859155,
        0.9084507042,
        0.9225352113,
        0.9507042254,
        0.9577464789,
        0.9647887324)

x <- c(1,
        2,
        3,
        4,
        5,
        6,
        7)
y <- c(0.5985915493,
        0.8028169014,
        0.8943661972,
        0.9295774648,
        0.9647887324,
        0.9718309859,
        0.985915493)


degree_ <- c(1,
             2,
             3,
             4)
y1 <- c(
  0.5985915493,
  0.8591549296,
  0.985915493,
  1
)

shel <- c(1,
          2,
          3,
          4,
          5
          
)
y2 <- c(0.5985915493,
        0.8521126761,
        0.9436619718,
        0.9788732394,
        1
)
plot(x_,y_,type="o",pch = 20,col="red", xlim=c(.9, 7.5), ylim = c(0.56, 1.01),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithms for Road network to reach core") + 
  lines(x,y,type="o",pch = 20,col="blue") +lines(degree_,y1,type="o",pch = 20,col="green") + lines(shel,y2,type="o",pch = 20,col="purple")
legend("bottomright", legend=c("Degree based dfs", "Shell Based dfs", "Degree based teleport", "Shell Based teleport"),lty=1:1, cex=0.8 ,
       col=c("red", "blue", "green", "purple"))
