# Script for making the cheshire_lsoas geojson file

library(sf)
library(readxl)
library(ggplot2)

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


plot(st_geometry(cheshire_lsoas), col = 'green')

imd <- read_xlsx("/Users/reka/Dropbox (The University of Manchester)/N8_covid/analysis/n8_covid/File_1_-_IMD2019_Index_of_Multiple_Deprivation.xlsx", sheet = "IMD2019") %>%  janitor::clean_names()

cheshire_lsoas <- left_join(cheshire_lsoas, imd, by = c("LSOA11NM" = "lsoa_name_2011"))

ggplot() + 
  geom_sf(data = cheshire_lsoas, aes(fill = as.factor(index_of_multiple_deprivation_imd_decile)), 
          lwd = 0.01, colour = "white") + 
  scale_fill_brewer(palette = "RdYlBu") + 
  labs(fill = "IMD decile") + 
  theme_void()

st_write(cheshire_lsoas, "cheshire_lsoas.geojson")

