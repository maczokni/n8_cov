# Script for making the cheshire_lsoas geojson file

library(sf)

force_boundaries <- st_read("cheshire.kml") 
lsoas <- st_read("https://opendata.arcgis.com/datasets/e9d10c36ebed4ff3865c4389c2c98827_0.geojson")

cheshire_lsoas <- st_intersects(force_boundaries, lsoas)
cheshire_lsoas <- lsoas[unlist(cheshire_lsoas),]

plot(st_geometry(cheshire_lsoas))
plot(st_geometry(force_boundaries),  add = T, col = 'blue')


nadia_lsoa <- st_read("/Users/reka/Downloads/nk_dis-main/data/england_lsoa_2011.shp")

nadia_lsoa <- st_transform(nadia_lsoa, st_crs(force_boundaries))


plot(st_geometry(force_boundaries), col = 'blue')
plot(st_geometry(nadia_lsoa),  add = T)



cheshire_lsoas <- cheshire_lsoas %>% filter(LSOA11CD %in% c(nadia_lsoa$code))

st_write(cheshire_lsoas, "cheshire_lsoas.geojson")

