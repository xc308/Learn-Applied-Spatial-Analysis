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



#---------------------------------------------------#
# Chunk 2: 3.1.3 Degrees in Axes Lables and Ref Grid
#---------------------------------------------------#

# grid lines long/lat grids, these are non-straight lines
# which is accompalished by generating a grid for unprojected data
# then projecting it, and then plotting over the map shown
install.packages("maptools")
install.packages("maps")
install.packages("rgdal")
library(maptools)
library(maps)
library(rgdal)

world <- map("world", interior = FALSE, xlim = c(-179, 179),
    ylim = c(-89, 89), plot = FALSE)

# convert map objects to suitable one defined in sp package
world_p <- pruneMap(world, xlim = c(-179, 179))

llCRS <- CRS("+proj=longlat +ellps=WGS84")
world_sp <- map2SpatialLines(world_p, proj4string = llCRS)
proj_new <- CRS("+proj=moll")

world_proj <- spTransform(world_sp, proj_new)
world_grid <- gridlines(world_sp, easts = c(-179, seq(-150, 150, 50), 179.5),
          norths = seq(-75, 75, 15), 
          ndiscr = 100)

world_grid_proj <- spTransform(world_grid, proj_new)

at_sp <- gridat(world_sp, easts = 0, norths = seq(-75, 75, 15),
       offset = 0.3)

at_proj <- spTransform(at_sp, proj_new)

plot(world_proj, col = "grey60")
plot(world_grid_proj, add = TRUE, lty = 3, col = "grey70")

text(coordinates(at_proj), pos = at_proj$pos,
     offset = at_proj$offset, 
     labels = parse(text = as.character(at_proj$labels)), 
     cex = 0.6)





