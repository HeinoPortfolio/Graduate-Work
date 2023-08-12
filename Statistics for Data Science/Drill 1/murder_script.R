# Author:       Matthew Heino
# Course:       MSDS 531
# Instructor:   Dr. Derek Phair
#
# Assignment:
#   
#   From the Murder data file, use the variable murder, which is the murder 
#   rate (per 100,000 population) for each state in the U.S. in 2017 according 
#   to the FBI Uniform Crime Reports. At first, do not use the observation 
#   for D.C. (DC). Using software:
#
#   Find the mean and standard deviation and interpret their values.
#   
#   Find the five-number summary, and construct the corresponding boxplot.
#   
#   Now include the observation for D.C. What is affected more by 
#   this outlier: The mean or the median? 
  

#==============================================================================

#Read in the data from the DAT file using read.delim.
murder_df <- read.delim("Murder.dat", sep="") 

#Print the contents of the whole dataframe.
print (murder_df)
View(murder_df)
print (head(murder_df, 5))
print (tail(murder_df, 5))
print (summary(murder_df))


# Frame without the inclusion of the DC observation.
murder_noDC_df <- murder_df[1:50, ]
print(murder_noDC_df)

# print information about the new dataframe.
print (summary(murder_noDC_df))

# Calculations without DC observation. =========================================
# Mean without the DC
print("No DC mean: ")
print(mean(murder_noDC_df$murder))

# Standard deviation
print("No DC standard deviation: ")
print(sd(murder_noDC_df$murder))

# Create the fivenumber summary
# Five number summary: minimum, 1st quartile, median, 3rd quartile, and maximum
#version # 1
print (fivenum(murder_noDC_df$murder))
#version 2
print (summary(murder_noDC_df$murder))

#Create the boxplot
no_DC_box <- boxplot(murder_noDC_df$murder, main = "Murder Rate "
                     , xlab = "Per 100,000 population", ylab = "Murder"
                     ,col ="blue", border = "black", horizontal = TRUE
                     , notch = TRUE, outcol = "red" )

print(no_DC_box)


print("=============================================================")

# Calculation with DC Observation.==============================================
print("With DC mean: ")
print(mean(murder_df$murder))

print("With DC standard deviation: ")
print(sd(murder_df$murder))


#version 2
print (summary(murder_df$murder))


# Show a boxplot with DC.
DC_box <- boxplot(murder_df$murder, main = "Murder Rate with DC "
                     , xlab = "Per 100,000 population", ylab = "Murder"
                     ,col ="blue", border = "black", horizontal = TRUE
                     , notch = TRUE, outcol = "red" )


print(no_DC_box)

