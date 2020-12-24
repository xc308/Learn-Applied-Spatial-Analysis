setwd("~/Learn-Applied-Spatial-Analysis")

#-------------------------------------#
# Plotting attributes and map legends
#-------------------------------------#
# when a specific color, size or symbol type referes
# to a specific numeric value or a category lable of an attribut
# a map legend is needed to communicate these info

#dev.off()
install.packages("gstat")
library(gstat)

data(meuse) # 155 * 14

coordinates(meuse) <- ~x+y # convert data.frame obj into SpatialPointsDataFrame
class(meuse) # "SpatialPointsDataFrame"
head(meuse[, 4], 3) # extract the 4 th col of data frame which is zinc
head(meuse[, c(1, 4)], 3)
#        coordinates cadmium zinc
#1 (181072, 333611)    11.7 1022
#2 (181025, 333558)     8.6 1141
#3 (181165, 333537)     6.5  640
head(meuse[, ], 3)


# find the prediction grid, i.e. where the interpolated value will be printed
data(meuse.grid) # dataframe
gridded(meuse.grid) <- ~x+y
class(meuse.grid) # SpatialPixelsDataFrame
head(meuse[, ], 3)
plot(meuse.grid)

# now use funtion autoKrige to perform ordinary kriging
# using zinc values from the meuse dataset, 
# then print the interpolate result to the prediction grid (meuse.grid)


zn.idw <- idw(zinc ~ 1, meuse, meuse.grid) 
# idw(formula, location, data frame) 
# [inverse distance weighted interpolation]

class(zn.idw)
# SpatialPixelsDataFrame
range(zn.idw$var1.pred)

par(mfrow = c(1,1))
grays <- gray.colors(4, start = 0.55, end = 0.95)
image(zn.idw, col = grays, breaks = c(100, 200, 400, 800, 1800))
# must have one more breaks than color

plot(meuse.pol, add = TRUE)
plot(meuse, pch  = 1, cex = sqrt(meuse$zinc)/40, add = TRUE)

range(meuse$zinc) # 113 1839
legVals <- c(100, 200, 500, 1000, 2000)
legend("left", legend = legVals, pch = 1, 
       pt.cex = sqrt(legVals)/40, bty = "n",
       title = "Mearsured") 
# legend for symbol

range(zn.idw$var1.pred) #  128.4345 1805.7757
legend("right", legend = c("100 - 200", "200 - 400", "400 - 800", "800 - 1800"),
       fill = grays, bty = "n", title = "Interpolated")
# legend for color

