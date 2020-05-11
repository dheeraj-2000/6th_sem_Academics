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



degree_tele <- c(4,11)
y1tele <- c(1,1)

sheltele <- c(1,
              2,
              3,
              4,
              5,
              6,
              7,
              8,
              9,
              10,
              11
              
)
y2tele <- c(0.07471264368,
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
plot(degree_,y1,type="l",col="red", xlim=c(.9, 12), ylim = c(-0.15, 1.01),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithms for weblog") + 
  lines(shel,y2,col="blue") + lines(degree_tele,y1tele,col="green") + lines(sheltele,y2tele,col="purple")
legend("bottomright", legend=c("Degree based dfs", "Shell Based dfs", "Degree based teleport", "Shell Based teleport"),lty=1:1, cex=0.8 ,
       col=c("red", "blue", "green", "purple"))
