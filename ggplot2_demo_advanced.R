##########################################################################################################
##       An Introduction to ggplot() in the ggplot2 package        #######################################
##
## The ggplot2 package is an advanced graphics package that implements the grammar of graphics. This 
## demo introduces the ggplot() function.  See the previous demo for an introduction to the qplot().
##  
## 
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
library(GGally)  #additional plots
library(grid)  #needed for unit function

str(diamonds)

#a bar chart
ggplot(data=diamonds, aes(cut, fill=cut)) + geom_bar() + labs(title="Bar Chart of Diamond Cut") +
  theme(plot.title=element_text(size=rel(2)))

#a stacked bar chart
ggplot(data=diamonds, aes(cut, fill=clarity)) + geom_bar() + labs(title="Bar Chart of Diamond Cut and Clarity") +
  theme(plot.title=element_text(size=rel(2)))

#a histogram
ggplot(data=diamonds, aes(price)) + geom_histogram(fill="orange") + labs(title="Histogram of Price") +
  theme(plot.title=element_text(size=rel(2.5)), axis.title=element_text(size=rel(1.5))) 

#a density plot
ggplot(data=diamonds, aes(price, fill=color)) + geom_density() + 
  labs(title="Density Plot of Price by Diamond Color") + 
  scale_fill_brewer(palette="Spectral", name="Color") +
  theme(plot.title=element_text(size=rel(2.5)), 
        axis.title=element_text(size=rel(1.5)),
        legend.key.height=unit(1, "cm"),
        legend.title=element_text(size=rel(1.5)),
        legend.text=element_text(size=rel(1.5)),
        panel.background = element_rect(fill = "white"))

#multiple plots on the page
ggplot(data=diamonds, aes(price)) + geom_histogram() + facet_grid(cut~.)

#take a sample to make the scatter plot look better
set.seed(12345)
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]

#A scatter plot
g <- ggplot(data=dsamp, aes(carat, price, colour=clarity)) + geom_point() 
g + scale_color_brewer(type="seq", palette=3)
g + scale_color_brewer(palette="Set1")

#pairs plot from the GGally package
ggpairs(
  dsamp[,1:5],
  upper = list(continuous = "density", combo = "box"),
  lower = list(continuous = "points", combo = "dot"),
  color = "cut",
  alpha = 0.4,
  title = "Diamonds"
)

#an example using aggregated data
Toy.dat <- data.frame(Company=c(rep("Toyota",2), rep("Ford",2)),
                      Item=c(rep("ROA % (Net)", 4)),
                      Value=c(4.74, 2.91, 3.06, 3.65),
                      Fiscal=c("Period End", "Period Beginning", "Period Beginning", "Period End"),
                      Year=c(2014, 2013, 2012, 2013))

p1 <- ggplot(Toy.dat, aes(x=Fiscal,y=Value, fill=Company, ymax=5)) 
p1 <- p1 + geom_bar(stat = "identity", position="dodge") 
p1 <- p1 + labs(x="Most Recent Fiscal Period", y="ROA % (Net)")
p1 <- p1 + labs(title="Comparison of ROA % (Net)") 
p1 <- p1 + theme(plot.title=element_text(size=rel(3)), axis.title=element_text(size=rel(2.5)), 
                 axis.text=element_text(size=rel(2)), 
                 legend.key.height=unit(3, "cm"),
                 legend.title=element_text(size=rel(1.5)),
                 legend.text=element_text(size=rel(1.5)))
p1 <- p1 + scale_fill_manual(values=c("blue", "red")) 
p1 + geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=3, size=15, colour="white")