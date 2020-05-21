library(igraph)
library(ggplot2)

x_ <- c(1,
        2,
        3,
        4)
y_ <- c(  0.6896551724,
          0.9310344828,
          0.9942528736,
          1)

x <- c(1,
       2,
       3,
       4,
       5)
y <- c(0.6896551724,
       0.9252873563,
       0.9712643678,
       0.9770114943,
       1)


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
          4
          
)
y2 <- c(0.6896551724,
        0.9252873563,
        0.9885057471,
        1
)

plot(x_,y_,type="l",col="red", xlim=c(0.9, 5), ylim = c(0.6, 1.07),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithms for weblog (Pseudocore) ") + 
  lines(x,y,col="blue")+ lines(degree_,y1,col="green") + lines(shel,y2,col="purple")
legend("bottomright", legend=c("Degree based dfs", "Shell Based dfs", "Degree based teleport", "Shell Based teleport"),lty=1:1, cex=0.8 ,
       col=c("red", "blue", "green", "purple"))

