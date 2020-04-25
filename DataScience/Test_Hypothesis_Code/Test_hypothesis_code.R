library(dplyr)

################# Two sample Z-Test for Test of significant in difference between means ###############
#### Here Given values are MEAN, SSAMPLE SIZE and STANDARD DEVIATION of Both Samples #############

##### Considering the Examples given in DR. ATHE's TESTING OF HYPOTHESIS Pg.No. 35  #####

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
## SINCE we got calculated value as 8.57 which is greater than tabulated So reject Null Hypothesis that means both the fields have significant difference



################# Two sample T-Test for Test of significant in difference between means ###############
#### Here Given values are Potato Plant yield Tubes fro two different varieties #############

##### Considering the Examples given in DR. ATHE's TESTING OF HYPOTHESIS Pg.No. 41  #####


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

#### So in the output we can see thsere is difference between the tubes value of both the varieties 



################# Two sample F-Test for Test of significant in difference between means ###############
####  Given values are two types of water to irrigate the gram plants #############

##### Considering the Examples given in DR. ATHE's TESTING OF HYPOTHESIS Pg.No. 45  #####


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

