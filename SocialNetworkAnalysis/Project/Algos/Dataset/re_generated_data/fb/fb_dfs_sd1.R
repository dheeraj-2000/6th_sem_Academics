library(igraph)
library(ggplot2)

x1 <- c(1,5,286,287,292)
y1 <- c(0.09333333333,0.16, 0.48,0.88, 0.9066666667)

x2 <- c(1,
        10,
        11,
        15,
        16,
        75,
        98)
y2 <- c(0.09333333333,
        0.24,
        0.6533333333,
        0.8266666667,
        0.88,
        0.9066666667,
        1)
plot(x1,y1,type="l",col="red", xlim=c(0, 400), ylim = c(0, 1.2),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithm for Facebook using dfs ") + 
lines(x2,y2,col="blue")
legend(320,0.19, legend=c("Shell Based", "Degree based"),lty=1:1, cex=0.8,, title("fdfc") ,
       col=c("blue", "red"))
