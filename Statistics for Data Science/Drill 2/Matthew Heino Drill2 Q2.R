#===============================================================================
#
# Author:       Matthew Heino
# Course:       MSDS 531
# Instructor:   Dr. Derek Phair
#
# Assignment:
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

anor_df <- read.table("Anorexia.dat", header = TRUE)
anor_df1 <- anor_df[anor_df$therapy == 'f',]

# Display the paired plot boxplot.
paired_plt <- ggpaired(anor_df1, cond1 = "before", cond2 = "after"
                       , title = "Therapy Results",fill="condition"
                       , xlab= "Results of Therapy"
                       , palette = c("blue", "cornflowerblue")
                       ,line.color = "red"
                       , ggtheme = theme_pubr()) + 
  geom_boxplot(outlier.colour="red", outlier.shape=1, outlier.size=8, 
               fill=c(c("blue", "cornflowerblue"))) +
  theme_gray(base_size = 14)
  
show(paired_plt)



#Show a histogram of the dataframe contents.

aft_bef_plt <- ggplot(anor_df1) + 
  geom_histogram(aes(x=before, fill="blue"), bins= 6, position = "identity"
                 , alpha =.3) +
  geom_histogram(aes(x=after, fill="corflowerblue"), bins=6, position = "identity"
                 , alpha =.3) + 
  scale_fill_manual(name="Treatment", values=c("blue","cornflowerblue"),
                    labels = c("Before", "After")) + 
  labs(title ="Histogram of Before and After Therapy", x = "Before and After"
       , y = "Count")


show(aft_bef_plt)

#Show the data for the paired plot.
plt_dat <- ggplot_build(paired_plt)$data[1]
show(plt_dat)

#Create a five number summary

fiv_num <- sapply(anor_df1[c("before","after")], fivenum)
print(fiv_num)
#===============================================================================
# B) Construct a 95% confidence interval for the difference between the 
# population mean 
# using t_test function.

anor_df2 <- subset(anor_df, anor_df$therapy == 'f' | anor_df$therapy == 'c')
anor_df2$diff <- (anor_df2$before - anor_df2$after)

mean_diff <- t.test(diff ~ therapy, data=anor_df2, conf.level =.95)
print(mean_diff)

print("The confidence interval: ")
print(paste("The lower bound is ", mean_diff$conf.int[1], "and the upper bound "
            , mean_diff$conf.int[2]))

#x <- anor_df2$diff[anor_df2$therapy == 'c']
#y <- anor_df2$diff[anor_df2$therapy == 'f']

#print(MeanDiffCI(x, y, conf.level=0.95, na.rm=TRUE))



