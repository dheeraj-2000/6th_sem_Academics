library(dplyr)
library(ggplot2)


#################### Assignment on simple linear regression #######################

data = read.csv("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/Linear_Model/mental_data.csv")
data_ = data.frame(data)
data_



################# Question: 1, Draw a scatterplot matrix with the three variables of interest, and comment briefly on the relationship between each pair of variables
data_ = subset(data_, select = -c(index))
png("/home/dheeraj/my_projects/my_project_env/practice/6th_sem_Academics/Linear_Model/scatter.png", width=600, height=600, units = "px")
pairs(data_, col= "green", alpha = "0.9")
dev.off()



################# Question: 2, Run a simple linear regression of the mental impairment index on the life events index, interpret the slope, and test its significance using a t-test.
model_1 <- lm(mentalImpair ~ lifeEvents, data = data_)
summary(model_1)
#from the output, we can see that
# Intercept is 23.309  and slope is 0.0898
# SO we can write the regression line eqn as : mentalImpair = 0.0898 * lifeEvents + 23.309
