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
#
#   2)  Using data for the 17 girls who received the family therapy: 
# 
#     a)  Conduct a descriptive statistical analysis using graphs and numerical 
#         summaries. 
#
#     b)  Construct a 95% confidence interval for the difference between the 
#         population mean weight changes for the family therapy and the control. 
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

chicago_his <- ggplot(data = chicago_df, aes(x = income)) +
  geom_histogram(aes(fill= after_stat(count)), binwidth = 2) +
  geom_vline(aes(xintercept=mean(income)),linewidth = 1,linetype = "dashed"
             , color = "red")+
  annotate("text", x=mean(chicago_df$income)+.30, y=2.5
           , label="Mean of Chicago Income", angle=90, size=5, color="white")+
  geom_vline(aes(xintercept=median(income)),linewidth = 1,linetype = "dashed"
             , color = "green")+
  annotate("text", x=median(chicago_df$income)+.30, y=3.0
           , label="Median of Chicago Income", angle=90, size=5, color="white")+
  scale_x_continuous(name = "Income in thousands ",breaks = seq(0, 30, 10)
                     , limits= c(15, 30)) +
  scale_y_continuous(name = "Count") +
  ggtitle("Frequency of Income in Chicago") +
  scale_fill_gradient("Count", low = "cornflowerblue", high = "blue")
 

# show the plot
show(chicago_his)

# The point estimates of the population mean.
# Answer: 20.33333
mean_pop <- mean(chicago_df$income)
print(paste("Point estimate of the population mean: ",mean_pop))

# The point estimate of the standard deviation

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
#===============================================================================
# Question 2 
#===============================================================================

#Read data into the dataframe.

anor_df <- read.table("Anorexia.dat", header = TRUE)
anor_df <- anor_df[anor_df$therapy == 'f',]

paired_plt <- ggpaired(anor_df, cond1 = "before", cond2 = "after"
                       , title = "Therapy Results",fill="condition"
                       , xlab= "Results of Therapy"
                       , palette = c("blue", "cornflowerblue"),line.color = "red"
                       , ggtheme = theme_pubr())  


show(paired_plt)


#Create a five number summary

fiv_num <- sapply(anor_df[c("before","after")], fivenum)
print(fiv_num)

# Show a histogram of the dataframe contents.

aft_bef_plt <- ggplot(anor_df) + 
  geom_histogram(aes(x=before, fill="blue"), bins= 6, position = "identity"
                 , alpha =.3) +
  geom_histogram(aes(x=after, fill="corflowerblue"), bins=6, position = "identity"
                 , alpha =.3) + 
  scale_fill_manual(name="Treatment", values=c("blue","cornflowerblue"),
                    labels = c("Before", "After")) + 
  labs(title ="Histogram of Before and After Therapy", x = "Before and After"
       , y = "Count")


show(aft_bef_plt)

#===============================================================================
# B) Construct a 95% confidence interval for the difference between the 
# population mean 
# using t_test function

before_mn <- mean(anor_df$before)
after_mn <- mean(anor_df$after)

# Calculate the standard deviation of the data, square it, and divide by n = 17.
# Using the z score of 1.960.

upper_bnd <- (before_mn - after_mn) + 1.960 * (sqrt((((sd(anor_df$before)^2) / 17) 
                                                     + ((sd(anor_df$after)^2) / 17))))

lower_bnd <- (before_mn - after_mn) - 1.960 * (sqrt((((sd(anor_df$before)^2) / 17) 
                                                     + ((sd(anor_df$after)^2) / 17))))

print(paste("95% confidence interval: ", lower_bnd, upper_bnd))



# Using t_test function ======================================
t_test = t.test(anor_df$before, anor_df$after, conf.level = 0.95)
print(t_test)

# Using mean_diff.=======================================================
mean_diff <- MeanDiffCI(anor_df$before, anor_df$after, conf.level = 0.95
                        , na.rm=TRUE)

print(mean_diff)


