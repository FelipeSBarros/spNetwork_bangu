# install.packages("sfnetworks")
# install.packages("spNetwork")

library(sf)
library(sfnetworks)
library(spNetwork)
library(tmap)


# Bangu Bounds
bangu <- read_sf("../Data/poligono_Bangu.shp")
bangu <- st_transform(bangu, 32723)
plot(st_geometry(bangu))

# Violence Data
fc = read_sf("../Data/tiroteios_Bangu.shp")
fc <- st_transform(fc, 32723)

plot(st_geometry(bangu))
plot(st_geometry(fc), add=T)

# Street network
streets = read_sf("../Data/bangu_graph.gpkg", layer='edges')
streets <- st_transform(streets, 32723)

plot(st_geometry(bangu))
plot(st_geometry(streets), add=T)
plot(st_geometry(fc), col='red', add=T)

# conver to SpatialDataFrame
fc <- as(fc, "Spatial")
streets <- as(streets, "Spatial")
bangu <- as(bangu, "Spatial")

# Identigying multipart street features
which(rgeos::gIsSimple(streets, byid = T)==FALSE)
streets <- streets[-which(rgeos::gIsSimple(streets, byid = T)==FALSE),]

# calculating some lixels to use as sampling points

lixels <- lixelize_lines(streets, 150)
samples <- lines_center(lixels)

# Network Kernel Density Estimation
densities <- nkde.mc(streets,
                  events = fc,
                  w = rep(1, nrow(fc)),
                  samples = samples,
                  kernel_name = "quartic",
                  bw = 30,
                  method = "discontinuous",
                  grid_shape = c(1,1),
                  max_depth = 10,
                  verbose = TRUE)

densities <- densities*1000
samples$density <- densities

tm_shape(samples) + 
  tm_dots(col = "density", size = 0.005, palette = "YlOrRd",
          style = "fisher") +
  tm_shape(fc) +
  tm_dots()

