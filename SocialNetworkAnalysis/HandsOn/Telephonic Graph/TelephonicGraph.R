i=j=k=1
part1<- c()
part2<- c()
part3<- c()
while(i<100){
  if(i>=1 && i<=37)
    part1 <- c(part1,i)
  
  else if(i>=38 && i<=68 ){
    part2<- c(part2,i)
    j=j+1}
  
  else{
    part3<- c(part3,i);
    k=k+1}
    i=i+1
}

g = make_empty_graph(n=100)
i=1

while(2*i+1 <= 37){
  g = g+ edge(part1[i],part1[2*i])
  g = g+ edge(part1[i],part1[2*i+1])
  i=i+1
}
i=1
while(2*i+1 <= 31){
  g = g+ edge(part2[i],part2[2*i])
  g = g+ edge(part2[i],part2[2*i+1])
  i=i+1}
i=1
while(2*i+1 <= 31){
  g = g+ edge(part3[i],part3[2*i])
  g = g+ edge(part3[i],part3[2*i+1])
  i=i+1
}

g = g + edge(100,part1[1])
g = g + edge(100,part2[1])
g = g + edge(100,part3[1])
plot (g, vertex.color = "green", edge.color = "red", edge.arrow.size = 0.5, layout = layout_with_kk, rescale=FALSE, xlim=c(-5,5), ylim=c(-5,5), vertex.size=70, vertex.label.color = "black")

