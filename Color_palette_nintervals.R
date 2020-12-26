########################################
# 3.5 Color Palettes and Class Intervals 
########################################

#----------------------#
# 3.5.1 Color Palettes #
#----------------------#
# for quantitative data, shades in single col preferred
# can be created by colorRampPalette, which creates a col interpolating function
# taking the required number of shades as argument
rw.color <- colorRampPalette(c("red", "white"))
# a funtion(n)
yow.color <- colorRampPalette(c("yellow", "orange", "white"))



image(meuse.grid["dist"], col = rw.color(10))
image(meuse.grid["dist"], col = topo.colors(10))
image(meuse.grid["dist"], col = yow.color(10))


#meuse.grid$dist

install.packages("RColorBrewer")
library(RColorBrewer)
example("brewer.pal")


#-----------------------#
# 3.5.2 Class Intervals
#-----------------------#

install.packages("classInt")
library(classInt)

pal <- brewer.pal(5, "Reds") # Chr

q5 <- classIntervals(meuse$zinc, n = 5, style = "quantile")
q5

diff(q5$brks)

plot(q5, pal = pal)
# many moderate values of zinc all be assigned to darkest red
# from the breaks of q5 , they are not uniformly distributed

# fisher - jenks is based on minimising the within-class variance
fj5 <- classIntervals(meuse$zinc, n =5, style = "fisher")
fj5
diff(fj5$brks) 

plot(fj5, pal = pal)
# values of zinc now much more uniformly distributed across different red shades


# use equal style of classIntervals
eq5 <- classIntervals(meuse$zinc, n = 5, style = "equal")
diff(eq5$brks)
plot(eq5, pal = pal)

# pretty style: equal intervals with larger breaks
pretty5 <- classIntervals(meuse$zinc, n = 5, style = "pretty")
diff(pretty5$brks)
plot(pretty5, pal = pal)

pretty100 <- classIntervals(meuse$zinc, n = 100, style = "pretty")
diff(pretty100$brks)
plot(pretty100, pal = pal)
# approximately continous


# once we are happy with the chosen interval and palette
# can use findColours function to build a vector of colours and attributes
# which can be used in constructing a lengend

q5Colors <- findColours(q5, pal = pal)
q5Colors
# [1] "#A50F15" "#A50F15" "#DE2D26"...
# [154] "#FEE5D9" "#FB6A4A"
# attr(,"palette")
# [1] "#FEE5D9" "#FCAE91" "#FB6A4A"
# [4] "#DE2D26" "#A50F15"
# attr(,"table")
# [113,186.8) [186.8,246.4) 
# 31            31 
# [246.4,439.6) [439.6,737.2) 
# 31            31 
# [737.2,1839] 
# 31 

plot(meuse, col = q5Colors, pch = 19)
legend("topleft", fill = attr(q5Colors, "palette"),
       legend = names(attr(q5Colors, "table")), 
       bty = "n")

fj5Colors <- findColours(fj5, pal = pal)
plot(meuse, col = fj5Colors, pch = 19, cex = 0.8)
legend("topleft", fill = attr(fj5Colors, "palette"),
       legend = names(attr(fj5Colors, "table")),
       bty = "n")

# using quantile color, all the sites along the river are polluted
# using fj color, better differentiate different sites

pre100Colors <- findColours(pretty100, pal = pal)
plot(meuse, col = pre100Colors, pch = 19, cex = 0.8)
legend("topleft", fill = attr(pre100Colors, "palette"),
       legend = names(attr(pre100Colors, "table")),
       bty = "n")

# for image, we specify the breaks argument
# the default class interval style used by image is to 
# devide the range into a number of classes equal width
# 


## spplot
## spplot methods for line, polygons, grids, pass pretty = TRUE
## to ensure the color breaks coninside with legend values

# for points data, use cuts argument
# for lines and polygons, grids, use at argument

# to control the key tic marks and labels, need to specify
# colorkey 
cuts <- (0:10)/10
spplot(meuse.grid, "dist", 
       colorkey = list(labels = list(at = cuts)),
       at = cuts)






