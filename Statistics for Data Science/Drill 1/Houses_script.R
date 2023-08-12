# Author:       Matthew Heino
# Course:       MSDS 531
# Instructor:   Dr. Derek Phair
#
# Assignment:
#  
#   1)  Construct a frequency distribution and a histogram.
#
#   2)  Find the percentage of observations that fall within one standard 
#   deviation of the mean.
#   
#   3) Construct a box plot. 
#  
#   Use the selling price for analysis.
#
#==============================================================================

#Read in the data from the DAT file using read.delim.
houses_df <- read.delim("Houses.dat", sep="") 

# View the contents
#View(houses_df)

# Show summary of the houses dataframe.
#print (summary(houses_df))

# Create a frequency distribution.
#sales_freq <- table(houses_df$price)
#print(sales_freq)

# Create a histogram
prices_hist <- hist(houses_df$price, xlab = "Price (USD)"
                   , main = "Histogram of House Prices" ,col = "blue"
                    , border = "white", breaks = "Sturges")
abline(v = mean(houses_df$price), col='red', lwd = 3)

# Find the mean of the sales prices
sales_mean <- mean(houses_df$price)
#print(sales_mean)

#Find the standard deviation
sales_sd <- sd(houses_df$price)
#print(sales_sd)

#create a subset
upper_bound <- sales_mean + sales_sd
#print(upper_bound)

lower_bound <- sales_mean - sales_sd
#print(lower_bound)

subset_house_prices_df <- subset(houses_df, houses_df$price >= lower_bound & houses_df$price <= upper_bound)
dev_cnt <- nrow(subset_house_prices_df)
#print(dev_cnt)

house_cnt <- nrow(houses_df)
#print(house_cnt)

perc_1dev <- (dev_cnt / nrow(houses_df)) * 100
print(perc_1dev)

#create a boxplot
house_box <- boxplot(houses_df$price, main = "Housing Sales Prices "
                     , xlab = "Per $1,000 ", ylab = "Sales Price"
                     ,col ="blue", border = "black", horizontal = TRUE
                     , notch = TRUE, outcol = "red" )

print(house_box)


