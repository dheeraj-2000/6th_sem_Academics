library(dplyr)

################# Two sample Z-Test for Test of significant in difference between means ###############
#### Here Given values are MEAN, SSAMPLE SIZE and STANDARD DEVIATION of Both Samples #############

##### Considering the Examples given in DR. ATHE's TESTING OF HYPOTHESIS Pg.No. 35  #####


# There is no significant difference between the means of two paddy fields

# Given DATA for Field No 1

field_1_Sample_Size <- 60
StandDeviation_1 <- 1.15
field_1_Sample_MEAN <- 18.5

# Given DATA for Field No 2

field_2_Sample_Size <- 60
StandDeviation_2 <- 1.15
field_2_Sample_MEAN <- 20.3

# calculating the Z-statistic at 5% LOS (i.e, Tabulated value is 1.96)

Z_Calculated <- (field_2_Sample_MEAN - field_1_Sample_MEAN) / 
  sqrt((StandDeviation_1^2/ field_1_Sample_Size) + (StandDeviation_2^2 / field_2_Sample_Size))

Z_Calculated
## SINCE we got calculated value as 8.57 which is greater than tabulated value(1.96) So reject Null Hypothesis that means both the fields have significant difference





################# Two sample T-Test for Test of significant in difference between means ###############
#### Here Given values are Potato Plant yield Tubes fro two different varieties #############

##### Considering the Examples given in DR. ATHE's TESTING OF HYPOTHESIS Pg.No. 41  #####


# Consider Null Hypothesis as mean number of tubes of the variety_1 significantly differ from the variety_2

## Given Data is
Variety_1 <- c(2.2, 2.5, 1.9, 2.6, 2.6, 2.3, 1.8, 2.0, 2.1, 2.4, 2.3)
Variety_2 <- c(2.8, 2.5, 2.7, 3.0, 3.1, 2.3, 2.4, 3.2, 2.5, 2.9)


# Create a data frame for the above both variety
my_data <- data.frame( 
  Types=c(rep("Variety_1",11),rep("Variety_2",10)),
  num_tubes = c(Variety_1,  Variety_2)
)

my_data

group_by(my_data, Types) %>%
summarise(
  sample_size = n(),
  sample_mean = mean(num_tubes, na.rm = TRUE),
  sample_sd = sd(num_tubes, na.rm = TRUE)
)

t.test(Variety_1, Variety_2)

#### So in the output we can see that calculated value (3.8) is greater than Tabulated value (2.09) at 5%LOS, and hence
#we conclude that the mean number of tubes of the variety_1  significantly not differ from the variety_2 



################# Two sample F-Test for Test of significant in difference between means ###############
####  Given values are two types of water to irrigate the gram plants #############

##### Considering the Examples given in DR. ATHE's TESTING OF HYPOTHESIS Pg.No. 45  #####


## Given Data is
Tap_water <- c(3.5, 4.2, 2.8, 5.2, 1.7, 2.6, 3.5, 4.2, 5.0, 5.2)
Saline_water <- c(1.9, 2.6, 2.3, 4.3, 4.0, 4.2, 3.8, 2.9, 3.7)

# Create a data frame for the above both variety
my_data <- data.frame( 
  Types_of_irrigation=c(rep("Saline_water",9),rep("Tap_water",10)),
  Height_of_Plants = c(Saline_water, Tap_water)
)

my_data

group_by(my_data, Types_of_irrigation) %>%
  summarise(
    sample_size = n(),
    sample_mean = mean(Height_of_Plants, na.rm = TRUE),
    sample_sd = sd(Height_of_Plants, na.rm = TRUE)
  )

res.ftest <- var.test(Tap_water, Saline_water)
res.ftest

## So in the output we can see that calculated value(1.78) is less than tabulate value(which is 3.23) So accept the null Hypothesis 
##hence we conclude that the variances of the two systems of irrigation are homogeneous



################# Chi-Square Test to Test the significant difference between means ###############
####  Given values are number of two types plants with two charactor as their leaf colour  #############

##### Considering the Examples given in DR. ATHE's TESTING OF HYPOTHESIS Pg.No. 49  #####

M <- as.table(rbind(c(99, 36), c(20, 5)))
dimnames(M) <- list(Flowe_color = c("White_Flower", "Red_Flower"),
                    Shape_of_leaf = c("Flat_leaf","Cirled_leaf"))
(Xsq <- chisq.test(M))
Xsq$observed   # observed counts (same as M)
Xsq$expected   # expected counts under the null

## So we can see in the output as Calulated value is (0.20) which ia less than tabulated value (3.84) at 5% LOS. Null hypothesis is
##accepted and hence we conclude that two characters, flower colour and shape of leaf are
##independent of each other
