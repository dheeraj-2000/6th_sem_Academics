library(tidyverse)
library(tabulizer)
library(dplyr)
library(ggplot2)
library(reshape2)


data.file <- "/home/stark/Desktop/Link to Clg/Sem 6/Data Analytics/DA LAB/CRS-2016.pdf"

tab <- extract_areas(data.file,pages = 11,output = "data.frame",header = F)
#after above line ,specified page number will be opened just select the data columns by draging mouse pointer,don't select headers
tab
tab <- as.data.frame(tab)
colnames(tab) <- c("year","NVER_Live_Births","NVER_Still_Births","NVER_Deaths","CRS_Births","CRS_deaths","% of CRS vs SRS _Births",
                   "% of CRS vs SRS_deaths")
tab

tab %>% summarise(meanliveBirths = mean(NVER_Live_Births))
tab %>% summarise(medianliveBirths = median(NVER_Live_Births))
tab %>% summarise(maxliveBirths = max(NVER_Live_Births))
tab %>% summarise(minliveBirths = min(NVER_Live_Births))
tab %>% summarise(stdliveBirths = sd(NVER_Live_Births)) 
tab %>% summarise(IQRliveBirths = IQR(NVER_Live_Births))

tab %>% summarise(meanCRSBirths = mean(CRS_Births))
tab %>% summarise(medianCRSBirths = median(CRS_Births))
tab %>% summarise(maxCRSBirths = max(CRS_Births))
tab %>% summarise(minCRSBirths = min(CRS_Births))
tab %>% summarise(stdCRSBirths = sd(CRS_Births)) 
tab %>% summarise(IQRCRSBirths = IQR(CRS_Births))


D_Birth_Death <- extract_areas(data.file,pages = 16,output = "data.frame",header = F)
D_Birth_Death_ub <- as.data.frame(D_Birth_Death)
colnames(D_Birth_Death_ub) <- c("Districts","Birth_reg","Birth_rate","Death_reg","Death_rate","Reg_infant_death","Still_birth_reg","Still_birth_rate")

D_Birth_Death_ru <- extract_areas(data.file,pages = 17,output = "data.frame",header = F)
D_Birth_Death_ru <- as.data.frame(D_Birth_Death_ru)
colnames(D_Birth_Death_ru) <- c("Districts","Birth_reg","Birth_rate","Death_reg","Death_rate","Reg_infant_death","Still_birth_reg","Still_birth_rate")

#detect outliers
boxplot(D_Birth_Death_ru$Reg_infant_death)
num = as.numeric(D_Birth_Death_ru$Reg_infant_death)
outvalues = boxplot(num)$out
which(D_Birth_Death_ru$Reg_infant_death %in% outvalues)

#remove outlier and plot again
removed = D_Birth_Death_ru$Reg_infant_death[!(D_Birth_Death_ru$Reg_infant_death %in% outvalues)]
#box plot of registered infant death without outliers
boxplot(removed)

#Plotting bar graph for urban and rural birth rate
ub_ru <- data.frame(D_Birth_Death_ub$Birth_rate,D_Birth_Death_ru$Birth_rate,D_Birth_Death_ub$Districts)
colnames(ub_ru) <- c("Urban","Rural","Districts")
m_ub_ru <- melt(ub_ru,id.vars = "Districts")

ggplot(m_ub_ru,aes(x = Districts , y = value,fill = variable))+ylab("Birth Rate") + geom_bar(stat = "identity",position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plotting line graph of NVER_death vs year
ggplot(tab,aes(x = year,y = NVER_Deaths)) + geom_line(color = "red") + ggtitle("Number of vital events registered  Deaths VS Year")

#Scatter plot of death rate
ub_ru_d <- data.frame(D_Birth_Death_ub$Death_rate,D_Birth_Death_ru$Death_rate,D_Birth_Death_ub$Districts)
colnames(ub_ru_d) <- c("Urban","Rural","Districts")
m_ub_ru_d <- melt(ub_ru_d,id.vars = "Districts")

ggplot(m_ub_ru,aes(x = Districts , y = value,color = variable))+ylab("Death Rate") + geom_point() + theme(axis.text.x = element_text(angle = 90, hjust = 1))



