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
##### Conlcusion: Mental impairment shows a positive association with stressful life events and a negative association with SES

################# Question: 2, Run a simple linear regression of the mental impairment index on the life events index, interpret the slope, and test its significance using a t-test.
model_1 <- lm(mentalImpair ~ lifeEvents, data = data_)
summary(model_1)
#from the output, we can see that
# Intercept is 23.309  and slope is 0.0898
# SO we can write the regression line eqn as : mentalImpair = 0.0898 * lifeEvents + 23.309

################# Question: 3, What proportion of the variation across subjects in the index of mental health is explained by the life events index? How is this proportion related to Pearson's correlation coefficient?
model_2 <- lm(mentalImpair ~ 1, data = data_)
rss <- function(model) sum(residuals(model)^2)
p <- rss(model_1)/rss(model_2)
c(p, sqrt(p))

#### Output is: [1] 0.8614518 0.9281443
# That means The proportion of variance from the sums of squares, is 0.1385 or 14%, and is e same as R-squared, which for simple regression is the squared of Pearson's r.


################# Question: 4, Check the linearity of this relationship by adding a quadratic term.........
data_ <- mutate(data_, lifeCsq = (lifeEvents - mean(lifeEvents))^2)
model_3 <- lm(mentalImpair ~ lifeEvents + lifeCsq, data = data_)
summary(model_3)

### COnclusion: The t-statistic of -0.33 on 37 d.f. is clearly not significant, so we have no evidence against the linearity of the association


################# Question: 5, Regress the index of mental impairment on SES, to verify the hypothesis that whether or not money buys happiness....
model_4 <- lm(mentalImpair ~ ses, data = data_)
cor(data_$mentalImpair, data_$ses)
### OUTPUT : [1] -0.3985676

std <- function(x) (x - mean(x))/sqrt(var(x))
model_s <- lm(std(mentalImpair) ~ std(ses), data = data_)
summary(model_s)

### Conclusion:  The estimated regression coefficient shows that high SES tends to be associated with lower mental impairment. Pearson's correlation coefficient is –0.40, indicating a moderate negative association.
### we standardize the variables subtracting the mean and dividing by the standard deviation, the constant becomes zero and the slope coincides with Pearson's correlation coefficient.