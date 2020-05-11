library(igraph)
library(ggplot2)

degree_ <- c(4)
y1 <- c(
        1
)

shel <- c(1,
          2,
          3,
          4,
          5,
          6,
          7,
          8,
          9,
          10,
          11,
          
)
y2 <- c(0.07471264368,
        0.2931034483,
        0.4885057471,
        0.683908046,
        0.8275862069,
        0.908045977,
        0.9425287356,
        0.9770114943,
        0.9827586207,
        0.9942528736,
        1
)
plot(degree_,y1,type="l",col="red", xlim=c(.9, 9), ylim = c(0.5, 1.01),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithm for weblog using teleportation") + 
  lines(shel,y2,col="blue")
legend("bottomright", legend=c("Shell Based", "Degree based"),lty=1:1, cex=0.8 ,
       col=c("blue", "red"))
