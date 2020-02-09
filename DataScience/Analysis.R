library(tabulizer)
read_pdf <- extract_areas("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/CRS_2016.pdf", pages = 11, header = F)
req_table <- my_table[[1]]  
df <- as.data.frame(req_table, header = T)
df
