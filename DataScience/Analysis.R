library(tabulizer)
original_data <- "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/CRS_2016.pdf"
read_pdf <- extract_areas(data.file,pages = 11,output = "data.frame",header = F)
read_pdf
read_pdf <- as.data.frame(read_pdf)
df
colnames(read_pdf) <- c("year","NVER_Live_Births","NVER_Still_Births","NVER_Deaths","CRS_Births","CRS_deaths","% of CRS vs SRS _Births",
                   "% of CRS vs SRS_deaths")
read_pdf