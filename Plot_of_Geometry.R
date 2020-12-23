################################
# 3.1 Traditional plot system
#################################

install.packages('sp')
library(sp)


#--------------------------------------------------------#
# Chunk 0 : 3.1.1 Plotting points, lines, polygons, grids
#--------------------------------------------------------#
par(mai = c(0.2,0.2,0.2,0.2))
#plot.new()


## Points
data(meuse)
coordinates(meuse) <- c('x', 'y') 
# change from dataframe to SpatialPointsDataFrame

plot(meuse)
title("points")


## Lines
cc <- coordinates(meuse) # retrieve spatial coords
m.sl <- SpatialLines(list(Lines(list(Line(cc)), 'line1')))
# formal class SpatialLines
plot(m.sl)
title("Lines")         
            

## Polygons                     
data("meuse.riv")                     
meuse.lst <- list(Polygons(list(Polygon(meuse.riv)), "meuse.riv"))
meuse.pol <- SpatialPolygons(meuse.lst) # formal class SpatialPolygons

plot(meuse.pol, col = "grey")
title("polygons")


## Grids
data(meuse.grid)
head(meuse.grid)

coordinates(meuse.grid) <- c('x', 'y')
# turn meuse.grid into SpatialPointsDataFrame

meuse.pix <- as(meuse.grid, "SpatialPixels")
# FormalClass SpatialPixels

image(meuse.pix, col = "grey")
title("Grid")

image(meuse.pix, col = "lightgrey")
plot(meuse.pol, col = "grey", add = TRUE)
plot(meuse, add = TRUE)



#-----------------------------------------#
# Chunk 1: 3.1.2 Axes and Layout Elements
#------------------------------------------#
par(mai = c(0.5, 0.5, 0.2, 0))
layout(matrix(c(1, 2), 1, 2)) 
# same as par(mfrow = c(1, 2))

plot(meuse.pol, axes = TRUE)
plot(meuse.pol, axes = FALSE)

axis(1, at = c(178000 + 0:2 * 2000), cex.axis = 0.7)
axis(2, at = c(326000 + 0:3 * 4000), cex.axis = 0.7)

box()


oldpar <- par(no.readonly = TRUE) 
# only parameters can be used in a subsequent par() call are returned.
layout(matrix(c(1, 2), 1, 2))

plot(meuse, axes = TRUE, cex = 0.6)
plot(meuse.pol, add = TRUE)
title("sample location")

par(mar = c(0,0,0,0) + 0.1)
plot(meuse, axes = FALSE, cex = 0.6)
plot(meuse.pol, add = TRUE)
box()
par(oldpar)
par(mfrow = c(1,1))

## provide a scalar bar and a north arrow instead of axes
plot(meuse)
plot(meuse.pol, add = TRUE)

SpatialPolygonsRescale(layout.scale.bar(), 
                       offset = locator(1),
                       scale = 1000,
                       fill = c("transparent", "black"),
                       plot.grid = FALSE)
text(locator(1), "0")
text(locator(1), "1km")

# try a different scale length
SpatialPolygonsRescale(layout.scale.bar(),
                       offset = locator(1),
                       scale = 500,
                       fill = c("transparent", "black"),
                       plot.grid = FALSE)
text(locator(1), "0")
text(locator(1), "500m")

SpatialPolygonsRescale(layout.north.arrow(), 
                       offset = locator(1), 
                       scale = 400,
                       plot.grid = FALSE)
