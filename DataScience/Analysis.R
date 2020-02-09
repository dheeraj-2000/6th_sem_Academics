library(tabulizer)
library(dplyr)

original_data <- "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/CRS_2016.pdf"
read_pdf <- extract_areas(original_data,pages = 11,output = "data.frame",header = F)
read_pdf
read_pdf <- as.data.frame(read_pdf)
colnames(read_pdf) <- c("year","event_Reg_LBirth","event_Reg_SBirth","event_Reg_Deaths","CRS_Births","CRS_deaths","percent_ofCRS_SRS_Births",
                   "percent_ofCRS_SRS_Deaths")
read_pdf
m <- tail(read_pdf, -11)
write.csv(m, "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/new_data.csv")
nw_read <- read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/new_data.csv")
nw_read
nw_read$X <- NULL
nw_read

min(nw_read[["event_Reg_LBirth"]])
max(nw_read[["event_Reg_LBirth"]])
mean(nw_read[["event_Reg_LBirth"]])
median(nw_read[["event_Reg_LBirth"]])
mode(nw_read[["event_Reg_LBirth"]])
var(nw_read[["event_Reg_LBirth"]])
sd(nw_read[["event_Reg_LBirth"]])
IQR(nw_read[["event_Reg_LBirth"]])

min(nw_read[["event_Reg_SBirth"]])
max(nw_read[["event_Reg_SBirth"]])
mean(nw_read[["event_Reg_SBirth"]])
median(nw_read[["event_Reg_SBirth"]])
mode(nw_read[["event_Reg_SBirth"]])
var(nw_read[["event_Reg_SBirth"]])
sd(nw_read[["event_Reg_SBirth"]])
IQR(nw_read[["event_Reg_SBirth"]])


min(nw_read[["event_Reg_Deaths"]])
max(nw_read[["event_Reg_Deaths"]])
mean(nw_read[["event_Reg_Deaths"]])
median(nw_read[["event_Reg_Deaths"]])
mode(nw_read[["event_Reg_Deaths"]])
var(nw_read[["event_Reg_Deaths"]])
sd(nw_read[["event_Reg_Deaths"]])
IQR(nw_read[["event_Reg_Deaths"]])


min(nw_read[["CRS_Births"]])
max(nw_read[["CRS_Births"]])
mean(nw_read[["CRS_Births"]])
median(nw_read[["CRS_Births"]])
mode(nw_read[["CRS_Births"]])
var(nw_read[["CRS_Births"]])
sd(nw_read[["CRS_Births"]])
IQR(nw_read[["CRS_Births"]])


min(nw_read[["CRS_deaths"]])
max(nw_read[["CRS_deaths"]])
mean(nw_read[["CRS_deaths"]])
median(nw_read[["CRS_deaths"]])
mode(nw_read[["CRS_deaths"]])
var(nw_read[["CRS_deaths"]])
sd(nw_read[["CRS_deaths"]])
IQR(nw_read[["CRS_deaths"]])


min(nw_read[["percent_ofCRS_SRS_Births"]])
max(nw_read[["percent_ofCRS_SRS_Births"]])
mean(nw_read[["percent_ofCRS_SRS_Births"]])
median(nw_read[["percent_ofCRS_SRS_Births"]])
mode(nw_read[["percent_ofCRS_SRS_Births"]])
var(nw_read[["percent_ofCRS_SRS_Births"]])
sd(nw_read[["percent_ofCRS_SRS_Births"]])
IQR(nw_read[["percent_ofCRS_SRS_Births"]])


min(nw_read[["percent_ofCRS_SRS_Deaths"]])
max(nw_read[["percent_ofCRS_SRS_Deaths"]])
mean(nw_read[["percent_ofCRS_SRS_Deaths"]])
median(nw_read[["percent_ofCRS_SRS_Deaths"]])
mode(nw_read[["percent_ofCRS_SRS_Deaths"]])
var(nw_read[["percent_ofCRS_SRS_Deaths"]])
sd(nw_read[["percent_ofCRS_SRS_Deaths"]])
IQR(nw_read[["percent_ofCRS_SRS_Deaths"]])

original_data <- "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/CRS_2016.pdf"
District_Urban <- extract_areas(original_data,pages = 16,output = "data.frame",header = F)
District_Urban
District_Urban <- as.data.frame(District_Urban)
df
colnames(read_pdf) <- c("year","event_Reg_LBirth","event_Reg_SBirth","event_Reg_Deaths","CRS_Births","CRS_deaths","percent_ofCRS_SRS_Births",
                        "percent_ofCRS_SRS_Deaths")

original_data <- "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/CRS_2016.pdf"
read_pdf <- extract_areas(original_data,pages = 17,output = "data.frame",header = F)
read_pdf
read_pdf <- as.data.frame(read_pdf)
df
colnames(read_pdf) <- c("year","event_Reg_LBirth","event_Reg_SBirth","event_Reg_Deaths","CRS_Births","CRS_deaths","percent_ofCRS_SRS_Births",
                        "percent_ofCRS_SRS_Deaths")
