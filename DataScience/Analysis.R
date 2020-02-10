library(tabulizer)
library(dplyr)
library(ggplot2)

#original_data <- "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/CRS_2016.pdf"
#read_pdf <- extract_areas(original_data,pages = 11,output = "data.frame",header = F)
#read_pdf
#read_pdf <- as.data.frame(read_pdf)
#colnames(read_pdf) <- c("year","event_Reg_LBirth","event_Reg_SBirth","event_Reg_Deaths","CRS_Births","CRS_deaths","percent_ofCRS_SRS_Births",
 #                  "percent_ofCRS_SRS_Deaths")
#read_pdf
#m <- tail(read_pdf, -11)
#write.csv(m, "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/new_data.csv")
nw_read <- read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/new_data.csv")
nw_read
nw_read$X <- NULL
nw_read
#line plot
ggplot(nw_read,aes(x = year,y = event_Reg_LBirth)) + geom_line(color = "red") + ggtitle("Number of vital events registered  Live Birth VS Year")

#scatter plot
attach(nw_read)
plot(year, percent_ofCRS_SRS_Deaths, main="Scatterplot_year vs percent_ofCRS_SRS_Deaths ",
     xlab="year ", ylab="percent_ofCRS_SRS_Deaths ", pch=19)
#counts <- table(nw_read$event_Reg_Deaths)
#barplot(counts)

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
colnames(District_Urban) <- c("Districts","Reg_birth","Birth_rate","Reg_death","Death_rate","Reg_infant_death","Reg_still_birth","Still_birth_rate")
District_Urban
write.csv(District_Urban, "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/district_urbn.csv", row.names = FALSE)
dist_urban_read <- read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/district_urbn.csv")
dist_urban_read


original_data <- "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/CRS_2016.pdf"
District_Rural <- extract_areas(original_data,pages = 17,output = "data.frame",header = F)
District_Rural
District_Rural <- as.data.frame(District_Rural)
colnames(District_Rural) <- c("Districts","Birth_reg","Birth_rate","Death_reg","Death_rate","Reg_infant_death","Still_birth_reg","Still_birth_rate")
District_Rural
write.csv(District_Rural, "/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/district_rural.csv", row.names = FALSE)
dist_rural_read <- read.csv("/home/dheeraj/Desktop/Lecture/6th_sem_Academics/DataScience/district_rural.csv")
dist_rural_read
#detect outliers
boxplot(District_Rural$Reg_infant_death)
num = as.numeric(District_Rural$Reg_infant_death)
outvalues = boxplot(num)$out
which(District_Rural$Reg_infant_death %in% outvalues)  #according to the output District having index 27 that is UMKUR have highest registered infant death
#remove outlier and plot again
removed = District_Rural$Reg_infant_death[!(District_Rural$Reg_infant_death %in% outvalues)]
#box plot of registered infant death without outliers
boxplot(removed)
