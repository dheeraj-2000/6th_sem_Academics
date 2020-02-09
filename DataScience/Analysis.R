library(tabulizer)
library(dplyr)
original_data <- "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/CRS_2016.pdf"
read_pdf <- extract_areas(data.file,pages = 11,output = "data.frame",header = F)
read_pdf
read_pdf <- as.data.frame(read_pdf)
df
colnames(read_pdf) <- c("year","event_Reg_LBirth","event_Reg_SBirth","event_Reg_Deaths","CRS_Births","CRS_deaths","%ofCRS_SRS_Births",
                   "%ofCRS_SRS_Deaths")
read_pdf

write.csv(read_pdf, "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/new_data.csv")

nw_read <- read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/new_data.csv")
nw_read
nw_read$X <- NULL
nw_read
tail(nw_read, -11)

min(nw_read[["NVER_Live_Births"]])
max(nw_read[["NVER_Live_Births"]])
mean(nw_read[["NVER_Live_Births"]])
median(nw_read[["NVER_Live_Births"]])
var(nw_read[["NVER_Live_Births"]])
sd(nw_read[["NVER_Live_Births"]])
IQR(nw_read[["NVER_Live_Births"]])

