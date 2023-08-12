


#===============================================================================
#
# Author:       Matthew Heino
# Course:       MSDS 531
# Instructor:   Dr. Derek Phair
#
# Assignment:
#   
#   1)  Fit a KNN classification model to the wine data, using pH, alcohol, 
#       fixed.acidity, and residual.sugar as explanatory variables. 
#
#   2)  Evaluate its performance using 10-fold cross-validation, using AUC to
#       choose the best k. 
#        
#   
#   Note:   You may need to un-comment the the install.packages code to install 
#           the packages if they are not already installed on your machine.  
#           
#           Text based answers will be found in a separate attachment.

#
#   1.  Provide the commands in plain text that you used to solve the problem.
#   2.  Attach the figure that resulted after command: plots$roc
#   3.  Output after executed command: plots$optres[[1]][13,]
#   4.  Attach the figure that resulted after command: plots$cc 


#==============================================================================

# Note: You may need to un-comment these commands to run the program.
#install.packages('caret', dependencies = TRUE)
#install.packages('MLeval', dependencies = TRUE)

library(caret) 
library(MLeval)

# Import data about white and red wines: 

white <- read.csv("https://tinyurl.com/winedata1",sep = ";") 
red <- read.csv("https://tinyurl.com/winedata2",sep = ";") 


# Add a type variable: 

white$type <- "white" 
red$type <- "red" 

#Merge the datasets: 

wine <- rbind(white, red) 
wine$type <- factor(wine$type) 

#summary(wine)

tc <- trainControl(method = "cv",
                   number = 10,
                   summaryFunction = twoClassSummary,
                   savePredictions = TRUE,
                   classProbs = TRUE)

m <- train(type ~ pH + alcohol + fixed.acidity + residual.sugar,
           data = wine,
           trControl = tc,
           method = "knn",
           metric = "ROC",
           tuneLength = 15,
           preProcess = c("center","scale"))

show(m)

# Show the curve
plots <- evalm(m, gnames ="kNN")

# Show the ROC result
#print("ROC :")
plots$roc
 

# Output the confidence level 
print(plots$optres[[1]][13, ])


# Show the calibration curves
show(plots$cc)
 
