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



#-----------------------------------------------#
# 3.2.3 Adding ref and layout elements to plots
#------------------------------------------------#

# spplot takes a single argument , splayout
# to annotate plots with lines, points, poly, text, combinations 
# sp.layout contains either a single layout item
# or a list of layout items

# a single layout item is a list of object
# its 1st component is the name of the layout function: sp.points, sp.polygons, sp.lines, sp.text
# followed by the object to be plotted
# then the arguments to adjust, color, symbols, size etc

# example
river <- list("sp.polygons", meuse.pol)
north_arrow <- list("SpatialPolygonsRescale", layout.north.arrow(),
     offset = c(178750, 332500), scale = 400)

scale_bar <- list("SpatialPolygonsRescale", layout.scale.bar(),
     offset = c(180200, 329800), scale = 1000, 
     fill = c("transparent", "black"))

txt1 <- list("sp.text", c(180200, 329950), "0")
txt2 <- list("sp.text", c(181200, 329950), "1 km")
pts <- list("sp.points", meuse, pch = 4, cex = 0.3, col = "black")

# a list of layout items be the argument of sp.layout
meuse.layout <- list(river, north_arrow, scale_bar, txt1, txt2, pts)

class(meuse.grid) # SpatialPointsDataFrame
spplot(zn.idw[c("var1.pred")], sp.layout = meuse.layout)
#zn.idw$var1.pred


#-------------------------------#
# 3.4.2 Arranging Panel Layout #
#--------------------------------#
spplot(zn.idw[c("var1.pred")], sp.layout = meuse.layout,
       layout = c(3, 3), skip = c(F, T, T, F, F, T, F, F, F) )

layout = c(3, 3)

spplot(meuse[c("cadmium", "copper", "lead", "zinc")],
       sp.layout = meuse.layout,
       layout = c(2, 2), 
       skip = c(F, F, F, F))


#-----------------------------#
# 3.3 ggplot2 and latticExtra #
#-----------------------------#

# ggplot takes an object and try to convert it 
# to a data.frame using method fortify

install.packages("ggplot2")
library(ggplot2)

methods(fortify)
# [18] fortify.SpatialLinesDataFrame*   
# [19] fortify.SpatialPolygons*         
# [20] fortify.SpatialPolygonsDataFrame*
# [14] fortify.Polygon*                 
# [15] fortify.Polygons* 
# [12] fortify.map*
# [9] fortify.Line*                    
# [10] fortify.Lines*

# so for SpatialPointsDataFrame objects such as meuse
# we need to do conversion dataframe ourselves
m <- as(meuse, "data.frame")
head(m)

ggplot(m, aes(x, y)) + geom_point() + coord_equal()
# aes: specifiy which variables to be plotted on x and y axis
# geom_point(): dictates the plot shall be a scatter plot
# cood_equal(): make sure that units along the x-axis equal those along y-axis

## gg : grammar of graphics 


# latticeExtra also uses + operator
# as objects returned by spplot are trellis/lattice/grid objects 

install.packages("latticeExtra")
library(latticeExtra)

p <- spplot(meuse["zinc"])
m <- SpatialPolygonsDataFrame(meuse.pol, data.frame(col = 1), match.ID = FALSE)
head(m, 3)
l <- spplot(m) # list
l 
l + p
p + l

## rasterVis further uses latticExtra to improve vis of spatial data
# esp raster maps 





