library(igraph)
library(ggplot2)

degree_ <- c(1,
             2,
             3,
             4,
             5,
             6
)
y1 <- c(0.07471264368,
        0.2988505747,
        0.5229885057,
        0.908045977,
        0.9770114943,
        1
        
)

shel <- c(1,
          2,
          3,
          4,
          5,
          6,
          7,
          9
          
)
y2 <- c(0.07471264368,
        0.2931034483,
        0.5,
        0.6551724138,
        0.7298850575,
        0.9770114943,
        0.9942528736,
        1
)
plot(degree_,y1,type="l",col="red", xlim=c(.9, 9), ylim = c(0.5, 1.01),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithm for weblog using teleportation") + 
  lines(shel,y2,col="blue")
legend("bottomright", legend=c("Degree based", "Shell Based"),lty=1:1, cex=0.8 ,
       col=c("red", "blue"))
