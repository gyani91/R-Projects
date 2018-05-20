# Install all necessary packages #

library(DMwR)
library(lattice)
library(Hmisc)
library(purrr)
library(ggplot2)

data(algae)

# Histogram #
hist(algae$mxPH, probability = T, main="Histogram of mxPH", xlab='mxPH')
print('# Histogram #')

# Boxplot #
boxplot(algae$oPO4, ylab = 'Orthosphosphate (oP04)')
rug(jitter(algae$oPO4), side = 2)
abline(h = mean(algae$oPO4, na.rm = T), lty = 2)
print('# Boxplot #')

# Conditioned Boxplot #
bwplot(size ~ a1, data = algae, ylab= 'River Size', xlab='Algal A1')

# Box-percentile #
bwplot(size ~ a1, data=algae, panel=panel.bpplot, probs=seq(.01,.49,by=.01),
       datadensity=TRUE, ylab='River Size',xlab='Algal A1')
print('# Box-percentile #')

# -- Line Chart -- #
# Use 'Orange' dataset #
# Convert factor to numeric for convenience #
Orange$Tree = as.numeric(Orange$Tree) 
ntrees = max(Orange$Tree)

# Get the range for the x and y axis 
xrange = range(Orange$age) 
yrange = range(Orange$circumference) 

# Set up the plot #
plot(xrange, yrange, type="n", xlab="Age (days)",
     ylab="Circumference (mm)" ) 
colors = rainbow(ntrees) 
linetype = c(1:ntrees) 
plotchar = seq(18, 18 + ntrees, 1)

# Add the lines #
for (i in 1:ntrees) { 
  tree = subset(Orange, Tree==i) 
  lines(tree$age, tree$circumference, type="b", lwd=1.5,
        lty=linetype[i], col=colors[i], pch=plotchar[i]) 
} 

# Add a title and subtitle #
title("Tree Growth", "A simple example")

# Add a legend #
legend(xrange[1], yrange[2], 1:ntrees, cex=0.8, col=colors,
       pch=plotchar, lty=linetype, title="Tree")

print('# Line Chart #')
# -- Line Chart (end) -- #

cars_sales = c(20, 50, 10, 15, 75)

# Bar plots #
barplot(cars_sales, main="Cars Sales", xlab="Days",  
        ylab="Total", names.arg=c("Mon","Tue","Wed","Thu","Fri"), 
        border="blue", density=c(10,20,30,40,50))

# -- lirbary(ggplot2) -- #

# -- Boxplots -- #
# Use 'mtcars' dataset #
# Create factors with value labels #
mtcars$gear = factor(mtcars$gear,levels=c(3,4,5),
                      labels=c("3gears","4gears","5gears")) 
mtcars$am = factor(mtcars$am,levels=c(0,1),
                    labels=c("Automatic","Manual")) 
mtcars$cyl = factor(mtcars$cyl,levels=c(4,6,8),
                     labels=c("4cyl","6cyl","8cyl")) 

# Observations (points) are overlayed and jittered
qplot(gear, mpg, data=mtcars, geom=c("boxplot", "jitter"), 
      fill=gear, main="Mileage by Gear Number",
      xlab="", ylab="Miles per Gallon")

print('# Boxplot ggplot2 #')

# -- Line Chart -- #
datn = read.table(header=TRUE, text='
supp dose length
  OJ  0.5  13.23
  OJ  1.0  22.70
  OJ  2.0  26.06
  VC  0.5   7.98
  VC  1.0  16.77
  VC  2.0  26.14
')

ggplot(data=datn, aes(x=dose, y=length, group=supp, colour=supp)) +
  geom_line() +
  geom_point()

print('# Line Chart ggplot2 #')

# Bonus: Advanced R - map() #
# More info: https://www.rdocumentation.org/packages/purrr/versions/0.2.4/topics/map #
# Apply a function to each one of the elements of the input and return a vector of the same lenght #
# Piping in R using %>% #
(y = 0:50 %>%
  map_dbl(function(x) x^2 + 2*x + 1))