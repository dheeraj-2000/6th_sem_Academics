library(igraph)
library(ggplot2)

degree_ <- c(7,59)
y1 <- c(1,1)

shel <- c(2,
          3,
          4,
          5,
          6,
          7,
          8,
          9,
          10,
          11,
          12,
          13,
          14,
          15,
          16,
          17,
          19,
          20,
          21,
          22,
          23,
          24,
          25,
          26,
          28,
          30,
          33,
          34,
          45,
          59
          
)
y2 <- c(0.03870967742,
        0.2064516129,
        0.3096774194,
        0.3741935484,
        0.4129032258,
        0.464516129,
        0.5290322581,
        0.5677419355,
        0.6064516129,
        0.6322580645,
        0.664516129,
        0.6967741935,
        0.7290322581,
        0.7677419355,
        0.7935483871,
        0.8129032258,
        0.8258064516,
        0.8322580645,
        0.8774193548,
        0.9032258065,
        0.9161290323,
        0.935483871,
        0.9419354839,
        0.9483870968,
        0.9612903226,
        0.9741935484,
        0.9806451613,
        0.9870967742,
        0.9935483871,
        1
)
plot(degree_,y1,type="l",col="red", xlim=c(1, 60), ylim = c(0.0, 1.01),  xlab="Number of steps taken",ylab="Cumulative fraction of instances", main = "Comparison of algorithm for Email-Univ Network using teleportation") + 
  lines(shel,y2,col="blue")
legend("bottomright", legend=c("Shell Based", "Degree based"),lty=1:1, cex=0.8 ,
       col=c("blue", "red"))
