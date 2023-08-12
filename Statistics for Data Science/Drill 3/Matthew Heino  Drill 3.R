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
library(AICcmodavg)


houses_df <- read.table("Houses.dat", header = TRUE)

# Extract the columns that are needed.
houses_df2 <- houses_df[ ,c("price", "taxes", "new")]


# a) Creation of the scatter plot for the y = selling price and x1 = tax bill.
# using ggplot and geom_point().

prices_plt <- ggplot(houses_df2, aes(x=taxes, y = price, color= factor(new))) +
 geom_point(size = 3) +
  scale_color_manual(name = "Condition", labels=c("Existing", "New"),
                     values = c("blue","red")) +
  ggtitle("Taxes vs. Price ") + 
  labs(y= "Selling Price ($ thousands)", x = "Taxe Bill ($ thousands)") +
  theme_bw() +
  geom_rug(color="black")

show(prices_plt)


#===============================================================================
#Identity Function  using identity.
# i) normal GLM

normal_glm <- glm(price ~ taxes, family=gaussian(link = "identity")
                  , data=houses_df2)



# With new column added ===================

normal_new_glm <- glm(price ~ taxes + new, family=gaussian(link = "identity")
                  , data=houses_df2)

# Print the results using normal GLM. Shows the result without the New column 
# added and with the new column.

print(summary(normal_glm))
print(summary(normal_new_glm))

# ii) Gamma GLM with and without the new column.

gamma_glm <-  glm(price ~ taxes, family=Gamma(link = "identity")
                  , data=houses_df2)
gamma_new_glm <-  glm(price ~ taxes + new, family=Gamma(link = "identity")
                  , data=houses_df2)

print(summary(gamma_glm))
print(summary(gamma_new_glm))


# Show a GLM plot
prices_plt2 <- prices_plt +
  geom_smooth(formula = y ~ x,  method= "glm", 
              method.args = list(family = "gaussian"), fullrange = TRUE)+
  ggtitle("Taxes vs. Price with GLM")


show(prices_plt2)


# Using aictab to show tabulated data about the GLMs.
# Used to show the resuults in aggreagate.
models_lst <- list(normal_glm,normal_new_glm, gamma_glm, gamma_new_glm)
mod.names <- c("normal_glm","normal_new_glm","gamma_glm","gamma_new_glm" )

aic_res <- aictab(cand.set = models_lst, modnames = mod.names)

show(aic_res)





