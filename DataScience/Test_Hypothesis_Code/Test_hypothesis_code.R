################# Two sample Z-Test or Test of significant for difference of means ###############
#### Here Given values are MEAN, SSAMPLE SIZE and STANDARD DEVIATION of Both Samples #############

##### Considering the Examples given in DR. ATHE's TESTING OF HYPOTHESIS Pg.No. 35#####

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



################# Two sample Z-Test or Test of significant for difference of means ###############
#### Here Given values are MEAN, SSAMPLE SIZE and STANDARD DEVIATION of Both Samples #############

##### Considering the Examples given in DR. ATHE's TESTING OF HYPOTHESIS Pg.No. 35#####
