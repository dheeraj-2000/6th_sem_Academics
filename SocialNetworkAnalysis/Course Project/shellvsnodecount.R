library (igraph)
library(ggplot2)
#g = read_graph("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Course Project/Final Submitted report/Code/Datasets Used/Facebook friendship Network/facebook_combined.txt", format = "edgelist", directed = F)
g <- read_graph("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Course Project/Final Submitted report/Code/Datasets Used/Weblog Network/weblog.txt", format = "edgelist", directed = F)

g = g - V(g)[which(degree(g)==0)]
g
V(g)

######################################################################################################### Bucketing
it = 1
c = coreness(g)
v = length(V(g))
i = 1
buckets <- list()
while(v>0)
{
  
  if (it %in% c)
  {
    tempo = c()
    tempo = c(tempo,V(g)[which(coreness(g)==it)])
    buckets[[i]] = tempo
    v = v - length(V(g)[which(coreness(g)==it)])
    print("v is ")
    print(v)
    i=i+1
  }
  it = it+1
}

print(buckets)

NodeCount <- c()
shell <- c()
for (i in 1:length(buckets)){
  shell <- c(shell,i)
  count <- length(buckets[[i]])
  NodeCount <- c(NodeCount, count)
}
df <- cbind(shell,NodeCount)
df

df1 <- data.frame(Shell_number = shell, Number_of_nodes = NodeCount)
df1
barplot(x=shell, y=NodeCount)
ggplot(df1, aes(Shell_number, Number_of_nodes)) +
  geom_col(color = "red", fill = "yellow") +ggtitle(  "Shell Number vs Number of Nodes\n in Weblog network")
title(main = "main")
  write.csv(df, "/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/SocialNetworkAnalysis/Course Project/shellvsNodeC.csv")

