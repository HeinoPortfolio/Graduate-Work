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
#===============================================================================

library(ggplot2)
library(DescTools)
#library(dplyr)
#library(GGally)
library(ggpubr)
#library(sqldf)

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

# Construct a 95% confidence interval for the difference between the population 
# mean 
before_mn <- mean(anor_df$before)
after_mn <- mean(anor_df$after)

# Calculate the standard deviation of the data, square it, and divide by n = 17.
# Using the z score of 1.960.

upper_bnd <- (before_mn - after_mn) + 1.960 * (sqrt((((sd(anor_df$before)^2) / 17) 
                                                    + ((sd(anor_df$after)^2) / 17))))

lower_bnd <- (before_mn - after_mn) - 1.960 * (sqrt((((sd(anor_df$before)^2) / 17) 
                                                     + ((sd(anor_df$after)^2) / 17))))


print(upper_bnd)
print(lower_bnd)


# using t_test function ======================================

t_test = t.test(anor_df$before, anor_df$after, conf.level = 0.95)
show(t_test)



mean_diff <- MeanDiffCI(anor_df$before, anor_df$after, conf.level = 0.95
                        , na.rm=TRUE)

print(mean_diff)

