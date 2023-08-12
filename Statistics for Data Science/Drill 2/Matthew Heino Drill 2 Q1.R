#===============================================================================
#
# Author:       Matthew Heino
# Course:       MSDS 531
# Instructor:   Dr. Derek Phair
#
# Assignment:
#   
#   1 a)  Based on a descriptive graphic, describe the shape of the sample data 
#         distribution. 
#         
#         Find and interpret point estimates of the population 
#         mean and standard deviation.
#   
#     b)  Construct a 95% confidence interval for μ, using R software. 
#   
# Note: Text based answers will be found in a separate attachment.
#
#==============================================================================
library(ggplot2)
library(DescTools)
library(ggpubr)


#Read data into the dataframe.
chicago_df <- read.table("Chicago.dat", header = TRUE)


#Create a histogram of the Chicago data.

par(bg="lightgrey")
chicago_hist <- hist(chicago_df$income, xlab = 'Income (USD) in thousands'
                     ,ylab = 'Number of per income Level'
                     , col = 'cornflowerblue', main="Chicago Income")


abline(v = mean(chicago_df$income),                       # Add line for mean
       col = "blue",
       lwd = 3)
abline(v = median(chicago_df$income),                     # Add line for median
       col = "green",
       lwd = 3)

text(x=mean(chicago_df$income + .30), y=4.5, paste("Mean = ", mean(chicago_df$income))
     , col = "black", srt = 90)

text(x=median(chicago_df$income + .30), y=3, paste("Median = ", median(chicago_df$income))
     , col = "white", srt = 90)
legend("topright", c("Mean", "Median"), fill=c("blue", "green"))


# The point estimates of the population mean.
# Answer: 20.33333
mean_pop <- mean(chicago_df$income)
print(paste("Point estimate of the population mean: ", mean_pop))

sd_pop <- sd(chicago_df$income)
print(paste("Point estimate of the standard deviation: ", sd_pop))

#===============================================================================
# b)  Construct a 95% confidence interval for μ, using R software.
# Without using t_test function.
# Find the length of the sample

n <- length(chicago_df$income)

#Find the mean for income.

x_bar <- mean(chicago_df$income)

# Find the standard deviation for the income.

std_dev <- sd(chicago_df$income)

# calculate the margin of error.

margin <- qt(0.975, df=n-1)*std_dev/sqrt(n)

#Calculate the bounds (upper and lower).

low_bnd <- x_bar - margin
up_bnd <- x_bar + margin


print(paste("95% confidence interval: ", low_bnd, up_bnd))

# Test with the t_test function.
print(t.test(chicago_df$income, conf.level = 0.95))

#===============================================================================
