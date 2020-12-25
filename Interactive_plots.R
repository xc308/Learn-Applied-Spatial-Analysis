########################
# 3.4 Interacive Plots #
########################

# The main functionality is centered around
# which information is present at the location 
# where the mouse is clicked.


#-----------------------------------#
# 3.4.1 Interacting with Base Graph
#-----------------------------------#
# two functions
# 1. locator(): return location of points clicked, in coord x and y
# 2. identify(): plots and returns the row numbers of 
# the items nearest to the location clicked 
# left mouse start, right mouse end

plot(meuse)
meuse.id <- identify(coordinates(meuse))

plot(meuse)
region <- locator(type = "o")
n <- length(region$x)
p <- Polygon(cbind(region$x, region$y)[c(1 : n, 1), ], hole = FALSE)
# > c(1 : n, 1)
#[1] 1 2 3 4 5 6 1
# Formal class Polygon

ps <- Polygons(list(p), ID = "region") # create spatialPolygons from list of polygons
# Formal class Polygons

sps <- SpatialPolygons(list(ps))
# create SpatialPolygons obj from a list of polygons

plot(meuse[sps, ], pch = 16, cex = 0.5, add = TRUE, col = "red")

# to identify particular polygons, use locator
install.packages("maptools")
library(maptools)

prj <- CRS("+proj=longlat +datum=NAD27")
nc_shp <- system.file("shapes/sids.shp", package = "maptools")[1]

nc <- rgdal::readOGR(nc_shp)

plot(nc)

pt <- locator(type = "p")
print(pt)

pt.sp <- SpatialPoints(cbind(pt$x, pt$y))
over(pt.sp, nc)


#------------------------------------------------#
# 3.4.2 Interacting with spplot and lattice plots
#------------------------------------------------#

# To select points with spplot
ids <- spplot(meuse, "zinc", identify = TRUE)
ids # show the points selected and return its row numbers

spplot.locator(type = "p")
# [1] 101.8125 102.0329

