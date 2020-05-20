library(igraph)
library(ggplot2)

x_ <- c(1,
        2,
        3,
        4,
        14)
y_ <- c(  0.1604938272,
          0.3395061728,
          0.3796296296,
          0.3858024691,
          0.3919753086)

x <- c(1,
       2,
       3,
       4,
       7)
y <- c(0.1604938272,
       0.3395061728,
       0.3858024691,
       0.3919753086,
       0.3950617284)


degree_ <- c(1,15
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
          7,
          8,
          10,
          12,
          13,
          14,
          15
          
)
y2 <- c(0.1604938272,
        0.487654321,
        0.7932098765,
        0.8580246914,
        0.9012345679,
        0.9351851852,
        0.950617284,
        0.9660493827,
        0.9691358025,
        0.9814814815,
        0.9907407407,
        0.9938271605,
        1
)

plot(x_,y_,type="l",col="red", xlim=c(0.9, 15), ylim = c(0.15, 1.0),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithms for Hamster (Pseudocore) ") + 
  lines(x,y,col="blue")+ lines(degree_,y1,col="green") + lines(shel,y2,col="purple")
legend("bottomright", legend=c("Degree based dfs", "Shell Based dfs", "Degree based teleport", "Shell Based teleport"),lty=1:1, cex=0.8 ,
       col=c("red", "blue", "green", "purple"))

