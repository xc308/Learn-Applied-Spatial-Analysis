##########################################
# 3.2 Trellis/Lattice Plots with spplot
##########################################

# Apart from the traditional plot method provided by sp
# 2nd method: spplot
# providings plotting of spatial data with attributes through Trellis graphics system
# by package lattice
# con: a bit hard as additional info like legend, lines, text, etc
# needs to be thought out at first
# advantage: many maps can be composed into single graphs easily and efficiently


#----------------------------------#
# 3.2.1 A straight trellis example 
#----------------------------------#

# two interpolation scenarios for zinc variables
# one is direct scale, and the other is log scale

# Method 1 : levelplot from lattice
install.packages("sp")
install.packages("lattice")
install.packages("gstat")
library(sp)
library(lattice)
library(gstat)


class(meuse) # SpatialPointsDataFrame
class(meuse.grid) # SpatialPixelsDataFrame

# cokriging of the 4 heavy metal variables
# https://rdrr.io/cran/gstat/src/demo/cokriging.R
meuse.g <- gstat(id = "zn", formula = log(zinc) ~ 1, data = meuse, nmax = 10)
meuse.g <- gstat(meuse.g, id = "cu", log(copper) ~ 1, meuse, nmax = 10)
meuse.g <- gstat(meuse.g, id = "cd", log(cadmium) ~ 1, meuse, nmax = 10)
meuse.g <- gstat(meuse.g, id = "pb", log(lead) ~ 1, meuse, nmax = 10)

meuse.g <- gstat(meuse.g, model = vgm(1, "Sph", 900, 1), fill.all = T)
x <- variogram(meuse.g, cutoff = 1000)
meuse.fit <- fit.lmc(x, meuse.g)
plot(x, model = meuse.fit)
z <- predict(meuse.fit, newdata = meuse.grid)
# z: a large SpatialPixelsDataFrame

# level plot
spplot(z[c("zn.pred", "cu.pred")], main = "Log-zinc predictions & Log-cu predictions")


## now try on meuse data in the book

#levelplot(z ~ x + y | name, spmap.to.lev(zn[c("direct", "log")]), , asp = "iso")

zn.idw <- idw(zinc ~ 1, meuse, meuse.grid) 
zn.idw.log <- idw(log(zinc) ~ 1, meuse, meuse.grid) 
zn.idw$var1.pred.log <- zn.idw.log$var1.pred
zn.idw$var1.var.log <- zn.idw.log$var1.var

names(zn.idw$var1.pred) <- "direct-interpolate"
names(zn.idw$var1.pred.log) <- "log-interpolate"


class(zn.idw) # SpatialPixelsDataFrame
class(zn.idw.log) # SpatialPixelsDataFrame

#zn.idw$var1.pred.log
spplot(zn.idw[c("var1.pred", "var1.pred.log")], cuts = 9)


class(meuse) # SpatialPointsDataFrame
spplot(meuse[c("cadmium", "copper", "lead", "zinc")])

meuse$cadmium
meuse$copper
meuse$lead
meuse$zinc


#-----------------------------------------------#
# 3.2.2 Plotting points, lines, polygons, grids
#------------------------------------------------#

install.packages("maptools")
library(maptools)

data("meuse.grid") # dataframe
coordinates(meuse.grid) <- c("x", "y") # spatialPointsDataFrame

head(meuse.grid[, ], 3)
meuse.grid <- as(meuse.grid, "SpatialPixelsDataFrame")

im <- as.image.SpatialGridDataFrame(meuse.grid["dist"])
# as.image.Spatial.. Convert to image data structure

contourLines(im) # calculate the contour lines
spcl <- ContourLines2SLDF(contourLines(im))
# Formal Class SpatialLinesDataFrame

spplot(spcl)


