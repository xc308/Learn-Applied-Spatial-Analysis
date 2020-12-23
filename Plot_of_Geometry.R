################################
# 3.1 Traditional plot system
#################################

install.packages('sp')
library(sp)


#------------------------------------------------#
# Chunk 0 : Plotting points, lines, polygons, grids
#--------------------------------------------------#
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







