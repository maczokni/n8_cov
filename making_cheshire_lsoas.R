# Script for making the cheshire_lsoas geojson file

library(sf)

force_boundaries <- st_read("cheshire.kml") 
lsoas <- st_read("https://opendata.arcgis.com/datasets/e9d10c36ebed4ff3865c4389c2c98827_0.geojson")

cheshire_lsoas <- st_intersects(force_boundaries, lsoas)
cheshire_lsoas <- lsoas[unlist(cheshire_lsoas),]

plot(st_geometry(cheshire_lsoas))
plot(st_geometry(force_boundaries),  add = T)

st_write(cheshire_lsoas, "cheshire_lsoas.geojson")

