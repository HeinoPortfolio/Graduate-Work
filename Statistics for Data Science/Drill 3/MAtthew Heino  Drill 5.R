#===============================================================================
#
# Author:       Matthew Heino
# Course:       MSDS 531
# Instructor:   Dr. Derek Phair
#
# Assignment:
#   
#   For the Houses data at Index of Datasets consider Y = selling price, 
#   x1 = tax bill (in dollars), and x2 = whether the house is new:
#
#   a)  Form the scatter plot of y and x1. Then answer, does the normal GLM 
#       structure of constant variability in y seem appropriate? If not, 
#       how does it seem to be violated?
#
#   b)  Using the identity link function, fit the 
#
#       i)    normal GLM
#       ii)   gamma GLM 
#       iii)  For each model, interpret the effect of x2. 
#
#   c)  For each model, describe how the estimated variability in selling prices
#       varies as the mean selling price varies from 100 thousand to 500 
#       thousand dollars. 
#
#   d)  Which model is preferred according to AIC?
#   
#   
# Note: Text based answers will be found in a separate attachment.
#
#==============================================================================
library(ggplot2)

houses_df <- read.table("Houses.dat", header = TRUE)

# Extract the columns that are needed.
houses_df2 <- houses_df[ ,c("price", "taxes", "new")]


# a) Creation of the scatter plot for the y = selling price and x1 = tax bill.
# using ggplot and geom_point().

prices_plt <- ggplot(houses_df2, aes(x=taxes, y = price, color= factor(new)) ) +
  geom_point(size = 3) +
  scale_color_manual(name = " Condition", labels=c("Existing", "New"),
                     values = c("blue","cornflowerblue")) +
  ggtitle("Taxes vs. Price ") + 
  labs(y= "Selling Price ($ thousands)", x = "Taxe Bill ($thousands)") +
  theme_bw() +
  stat_smooth(method= "lm", formula = y ~ x)

print(prices_plt)


#===============================================================================