Fulanus_bbox <- ggmap::make_bbox(lat = Lat, lon = Lon, data = FulanusDistribution)

Fulanus_big <- ggmap::get_map(location = bc_bbox, source = "google", maptype = "terrain")

ggmap::ggmap(Fulanus_big) +
  ggplot2::geom_jitter(data = FulanusDistribution, mapping = ggplot2::aes(x = Lon, y = Lat), size = 0.5)

library(raster)
library(ggplot2)
library(ggmap)

r <- raster(....) # any raster you want to plot
rtp <- rasterToPolygons(r)
rtp@data$id <- 1:nrow(rtp@data)   # add id column for join

rtpFort <- fortify(rtp, data = rtp@data)
rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data

bm <- ggmap(get_map(location = "Shanghai", maptype = "hybrid", zoom = 10))

bm + geom_polygon(data = rtpFortMer,
  aes(x = long, y = lat, group = group, fill = layer),
  alpha = 0.5,
  size = 0) +  ## size = 0 to remove the polygon outlines
  scale_fill_gradientn(colours = topo.colors(255))
