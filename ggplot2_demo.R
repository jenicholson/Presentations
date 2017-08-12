##########################################################################################################
##       An Introduction to qplot() in the ggplot2 package        ########################################
##
## The ggplot2 package is an advanced graphics package that implements the grammar of graphics. There 
## are two main functions in the package.  The first is qplot().  This function is for making a quick 
## plot.  It tends to guess as to what you want and may not give you exackly what you want.  The function 
## ggplot() gives greater control of the plot at the cost of having to specify every detail.
##
## See "docs.ggplot2.org/current/" for more details
##
## Layers in ggplot2
## 1. Data
## 2. Geometric Object (geom)
## 3. Statistical Transformation (stat)
## 4. Scales
## 5. Coordinate System
#########################################################################################################

require(ggplot2)
require(RColorBrewer)  #Color palettes for plotting
str(diamonds)

#a bar chart
qplot(cut, data=diamonds, fill=cut, 
      main="Bar Chart of Diamond Cut")

#a stacked bar chart
qplot(cut, data=diamonds, fill=clarity, geom="bar", main="Bar Chart of Diamond Cut and Clarity")

#a first histogram, note fill sets the color inside the bar, colour sets the boundary color
qplot(price, data=diamonds, geom="histogram", main="Histogram of Diamond Prices", 
      fill=I("cornflowerblue"), colour = I("black"))

#try a density plot
qplot(price, data=diamonds, geom="density", fill=cut, main="Density of Price by Cut")

#a different density plot with colors changed
qplot(price, data=diamonds, geom="density", fill=color, main="Density of Price by Color") +
  scale_fill_brewer(palette="Spectral")

#box plot
qplot(cut, price, data=diamonds, geom="boxplot", fill=cut, main="Price by Cut")

#take a sample to make the scatter plot look better
set.seed(12345)
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]

#do a scatter plot
(q1 <- qplot(carat, price, data=dsamp, colour=clarity, main="Carat vs Price for Sample of Diamonds"))
  
#now play with the color of the points
q1 + scale_colour_brewer()
q1 + scale_colour_brewer(palette="Paired")

#now try seperate regressions for each type of clarity
qplot(carat, price, data=dsamp, geom=c("point", "smooth"),
      method="lm", formula=y~poly(x, 2), colour=clarity,
      main="Regression of Price on Carat for each Clarity")

#multiple plots on the page
qplot(carat, price, data=dsamp, colour=clarity, facets=~cut)


#per request some time series
#first save airquality since we are going to manupulate it 
a1 <- airquality
a1$NewDate <- as.Date(paste(a1$Month,a1$Day,"1973", sep="/"), "%m/%d/%Y")
qplot(NewDate, Ozone, data=subset(a1, !is.na(Ozone)), geom="line", main="Ozone over Time")

#another example using economics data in ggplot2 package
qplot(date, unemploy, data=economics, geom="path", main="Unemployment over Time", ylab="Unemployment (Thousands)")

#example using a time series object
str(AirPassengers)
qplot(time(AirPassengers), AirPassengers, geom="line", main="Monthly International Airline Passengers")
