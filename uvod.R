library(sf)
library(raster)
library(tidyverse)
#library(rworldmap)
library(rnaturalearth)
library(readxl)
# -------------------------------------------------------------------------
# 
# -------------------------------------------------------------------------
# projekce
# https://epsg.io/

world <- ne_countries(scale = "medium", returnclass = "sf")
dem <- raster('GIS_data/grids/dem_100.tif')
dem_wgs84 <- projectRaster(dem, crs = '+proj=longlat +datum=WGS84 +no_defs +type=crs')

# plot
plot(dem)
plot(dem_wgs84)

# agregate
dem_1000 <- aggregate(dem, 100)
res(dem_1000)
values(dem_1000)

# -------------------------------------------------------------------------
# 
# -------------------------------------------------------------------------
df <- read_xlsx('data_csv/CNFD-selection-2023-01-09-IA.xlsx')
cnfd_head <- read_xlsx('data_csv/CNFD-selection-2023-01-09-IA.xlsx') %>%
  select(PlotID, ESy.v2, deg_lon, deg_lat, Temperature) %>%
  filter(grepl('^L', ESy.v2)) %>%
  st_as_sf(coords = c("deg_lon", "deg_lat"), crs = 4326)

cnfd_head %>% st_transform(32633)

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = cnfd_head, aes(colour = Temperature), size = 2) +
  scale_colour_viridis_c(option = 'A') + 
  coord_sf(xlim = c(10, 20), ylim = c(47, 52), expand = FALSE) +
  theme_bw()

