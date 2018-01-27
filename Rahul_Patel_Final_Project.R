#####################################
#
#IST-719
#Information Visualization
#Author: Rahul Patel
#Purpose: Final project
#
####################################

# libraries

library(edgebundleR)
library(circlize)
library(packcircles)
library(ggplot2)
library(ggthemes)
library(RColorBrewer)
library(zoo)
library(highcharter)
library(ggthemes)
library(grid)
library(gridExtra)
library(tidyverse)
library(dplyr)

# Download the data from https://www.kaggle.com/c/house-prices-advanced-regression-techniques/data 
# We will only consider train dataset for this project

# Load the data
train <- file.choose()
train <- read.csv(train,sep = ',', header = TRUE)

# Plot 1

# Density Plot for Sale Price
# We use ggplot for the density plot
options(scipen=999)
ggplot(train, aes(SalePrice)) + geom_density(fill='#3A3F4A') + theme_pander() + scale_colour_pander()+
  scale_x_continuous(breaks = c(0, 100000,200000, 300000, 400000, 500000, 600000, 700000)) +
  scale_y_continuous(breaks = round(seq(min(train$SalePrice), max(train$SalePrice), by = 50000),50000))

# Plot 2

# Density Plot for Total Rooms above ground
ggplot(train, aes(TotRmsAbvGrd)) + geom_density(fill='#3A3F4A') + theme_pander() + scale_colour_pander()+
  scale_x_continuous(breaks = round(seq(min(train$TotRmsAbvGrd), max(train$TotRmsAbvGrd), by = 1),1)) +
  scale_y_continuous(breaks = round(seq(min(train$TotRmsAbvGrd), max(train$TotRmsAbvGrd), by = 1),1))


# Plot 3

#Time Series Chart
dataDf1 <- data.frame(SalePrice = train$SalePrice, Date = as.yearmon(paste(train$MoSold, train$YrSold), "%m %Y"), stringsAsFactors = F)
dataDf1 <- zoo(dataDf1$SalePrice, order.by = dataDf1$Date)
TimeSeries <- as.zooreg(aggregate(dataDf1, as.yearmon, median), freq = 12)
dataDf1 <- as.ts(TimeSeries)
hchart(dataDf1) 

# Plot 4

# Tree Map
train %>% 
  group_by( Neighborhood ) %>% 
  summarise( housesSold = n() ) %>% 
  hchart( "treemap", hcaes(value = housesSold, x = Neighborhood, color = housesSold ),
         allowDrillToNode = TRUE, layoutAlgorithm = "squarified" )


# Plot 5

#Bubble Chart

# Create data
bubbledf <- train

temp <- aggregate(bubbledf$SalePrice, by=list(bubbledf$TotRmsAbvGrd), FUN=mean)
colnames(temp) <- c("TotRmsAbvGrd","Mean_SalePrice")

# Generate the layout. This function return a frame with one line per bubble. 
# It gives its center (x and y) and its radius, proportional of the value

packing <- circleProgressiveLayout(temp$Mean_SalePrice, sizetype='area')

# We can add these packing information to the initial data frame
temp = cbind(temp, packing)

# Check that radius is proportional to value. We don't want a linear relationship, since it is the AREA that must be proportionnal to the value
plot(temp$radius, temp$Mean_SalePrice)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.

dat.gg <- circleLayoutVertices(packing, npoints=10000)

# Make the plot
ggplot() + 
  
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), alpha = 0.5) +
  
  #scale_colour_brewer(pallete = "Reds")+
  
  # Add text in the center of each bubble + control its size
  geom_text(data = temp, aes(x, y, size= 9, label = TotRmsAbvGrd)) +
  scale_size_continuous(range = c(1,4)) +
  
  # General theme:
  theme_void() + 
  #  theme(legend.position="none") +
  coord_equal()


# Plot 6
# Chord Diagram

datadf = train
edgebundle(cor(datadf[, integVars]), cutoff = 0.1, tension = 0.2, nodesize = c(30,30))
classes <- sapply(names(datadf),function(x){class(datadf[[x]])})
integVars <- which(classes == "integer")
datacor = cor(datadf[, integVars])

chordDiagram(datacor)
circos.info()

train$SalePrice
grid.col = c(SalePrice = "red")
chordDiagram(datacor, grid.col = grid.col)

devtools::install_github("mattflor/chorddiag")
library(chorddiag)

groupColors <- c("#000000", "#FFDD89", "#957244", "#F26223")

chorddiag(datacor, groupColors = groupColors, groupnamePadding = 100)

getPalette <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(15, "YlOrRd"))
colorCount <- length(integVars)

chorddiag::chorddiag(datacor, margin = 100, showTicks =FALSE
                     , groupnameFontsize = 15       
                     , groupnamePadding = 10
                     , groupThickness = .06
                     , chordedgeColor = "none"
                     , groupColors =getPalette(colorCount))     



# Plot 7

# Scatter Plots

# Equation for linear model of the graph
lm_eqn = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}


p1 = ggplot(train, aes(x = GrLivArea, y = SalePrice)) +
  xlab("Living Area in square feet") + 
  geom_point(col="#375E97") +
  stat_smooth(se=FALSE,method = "lm", formula = y ~ x, size = 1)+
  theme(text = element_text(size=10))
#+
# geom_text(aes(x = 50, y = 300, label = lm_eqn(lm( SalePrice~ GrLivArea, train))), parse = TRUE)
plot(p1)

p2 = ggplot(train, aes(x = GarageArea, y = SalePrice)) +
  xlab("Garage Area in square feet") + 
  geom_point(col="#FB6542")+
  stat_smooth(se=FALSE,method = "lm", formula = y ~ x, size = 1)+
  theme(text = element_text(size=10))
# +
#   geom_text(aes(x = 50, y = 300, label = lm_eqn(lm( SalePrice~ GarageArea, train))), parse = TRUE)
plot(p2)


p3 = ggplot(train, aes(x = TotalBsmtSF, y = SalePrice)) +
  xlab("Basement Area in square feet") + 
  geom_point(col="#FFBB00")+
  stat_smooth(se=FALSE, method = "lm", formula = y ~ x, size = 1)+
  theme(text = element_text(size=10))
# +
#   geom_text(aes(x = 50, y = 300, label = lm_eqn(lm( SalePrice~ TotalBsmtSF, train))), parse = TRUE)
plot(p3)

p4 = ggplot(train, aes(x = OpenPorchSF, y = SalePrice)) +
  xlab("Open porch Area in square feet") + 
  geom_point(col="#3F681C")+
  stat_smooth(se=FALSE, method = "lm", formula = y ~ x, size = 1)+
  theme(text = element_text(size=10))
# +
#   geom_text(aes(x = 50, y = 300, label = lm_eqn(lm( SalePrice~ OpenPorchSF, train))), parse = TRUE)
plot(p4)