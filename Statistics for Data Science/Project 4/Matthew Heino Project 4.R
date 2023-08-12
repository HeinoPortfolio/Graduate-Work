#===============================================================================
#
# Author:       Matthew Heino
# Course:       MSDS 531
# Instructor:   Dr. Derek Phair
#
# Assignment:
#
#  
#     Try to recreate with R or Octave, as close as possible, the data from the 
#     figure P4p4F1.pdf. Functions needed are: runif (R) for 
#     uniform distribution and rnorm (R) for the normal 
#     distribution.
#
#
# Note: Text based answers will be found in a separate attachment.
#
#==============================================================================
set.seed(24)
n <- 150

x1 <- runif(n, 2,8)
y1 <- rnorm(n, mean = 15.6, sd = 2.0)

x2 <- runif(n, 7.5,13)
y2 <- rnorm(n, mean = 4, sd = 2.0)

x3 <- runif(n, 13,18)
y3 <- rnorm(n, mean =15.4, sd = 2.5)

x4 <- runif(n+450,-1,20)
y4 <- rnorm(n+450, mean = 10, sd = 19)


group_plt <- plot(x = jitter(c(x1, x2, x3, x4), 3), y = c(y1, y2, y3, y4),
                  xlab = "x1",
                  ylab = "x2",
                  xaxt='n',
                  yaxt='n',
                  xlim = c(0,20),
                  ylim = c(0,20),
                  main = "Training Set", 
                  pch = c(1,3))

axis(side=1, at=seq(0, 20, by = 2))
axis(side=2, at=seq(0, 22, by = 2), las = 2)

show(group_plt)