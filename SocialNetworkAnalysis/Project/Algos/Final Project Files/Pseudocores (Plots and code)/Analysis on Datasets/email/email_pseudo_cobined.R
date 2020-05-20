library(igraph)
library(ggplot2)

x_ <- c(1,
        2,
        3)
y_ <- c(0.6193548387,
        0.9741935484,
        1)

x <- c(1,
       2,
       3)
y <- c(0.6193548387,
       0.9741935484,
       1)


degree_ <- c(1,
             2,
             3
             
)
y1 <- c(
  0.6193548387,
  0.9741935484,
  1
)

shel <- c(1,
          2,
          3
          
)
y2 <- c(0.6193548387,
        0.9741935484,
        1
)

plot(x_,y_,type="o",pch = 20,col="red", xlim=c(1, 3.3), ylim = c(0.6, 1.01),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithms for Email-Univ to reach Pseudo-core ") + 
  lines(x,y,type="o",pch = 20,col="blue")+ lines(degree_,y1,type="o",pch = 20,col="green") + lines(shel,y2,type="o",pch = 20,col="purple")
legend("bottomright", legend=c("Degree based dfs", "Shell Based dfs", "Degree based teleport", "Shell Based teleport"),lty=1:1, cex=0.8 ,
       col=c("red", "blue", "green", "purple"))


