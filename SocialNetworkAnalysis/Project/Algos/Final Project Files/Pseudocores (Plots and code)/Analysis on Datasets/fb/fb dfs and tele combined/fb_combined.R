library(igraph)
library(ggplot2)

x_ <- c(1,
        2,
        3,
        8)
y_ <- c(0.4666666667,
        0.8266666667,
        0.88,
        0.9066666667)

x <- c(1,
       2,
       3,
       62,
       97)
y <- c(0.4666666667,
       0.8266666667,
       0.88,
       0.9066666667,
       1)


degree_ <- c(1,
             2,
             3,
             4
)
y1 <- c(
  0.4666666667,
  0.8266666667,
  0.96,
  1
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

plot(x_,y_,type="o",pch = 20,col="red", xlim=c(1, 98), ylim = c(0.4, 1.01),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithms for Facebook to reach Pseudo-core ") + 
  lines(x,y,type="o",pch = 20,col="blue")+ lines(degree_,y1,type="o",pch = 20,col="green") + lines(shel,y2,type="o",pch = 20,col="purple")
legend("bottomright", legend=c("Degree based dfs", "Shell Based dfs", "Degree based teleport", "Shell Based teleport"),lty=1:1, cex=0.8 ,
       col=c("red", "blue", "green", "purple"))

