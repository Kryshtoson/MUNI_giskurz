library(sf)
library(raster)
library(tidyverse)
library(rnaturalearth)
library(readxl)
# -------------------------------------------------------------------------
# Rastery
# -------------------------------------------------------------------------
# PEGS vzorce (https://epsg.io/)
# vlastnosti rasteru
# projekce, transformace mezi projekcemi, rozlišení a agregování rasterů

world <- ne_countries(scale = "medium", returnclass = "sf") # rnaturalearth data
dem <- raster('GIS_data/grids/dem_100.tif') # lokální data
dem_wgs84 <- projectRaster(dem, crs = '+proj=longlat +datum=WGS84 +no_defs +type=crs')

# plot
plot(dem)
plot(dem_wgs84)

# agregate
dem_1000 <- aggregate(dem, 100)
res(dem_1000)
values(dem_1000)

# -------------------------------------------------------------------------
# Vektory
# -------------------------------------------------------------------------
# generování bodového vektoru z tabulky
# práce s tabulkou
# projekce, EPGS kódy
# jednoduché zobrazení rasteru

df <- read_xlsx('data_csv/CNFD-selection-2023-01-09-IA.xlsx')
cnfd_head <- read_xlsx('data_csv/CNFD-selection-2023-01-09-IA.xlsx') %>%
  select(PlotID, ESy.v2, deg_lon, deg_lat, Temperature) %>%
  filter(grepl('^L', ESy.v2)) %>%
  st_as_sf(coords = c("deg_lon", "deg_lat"), crs = 4326)

cnfd_head %>% st_transform(32633)

finalni_mapa<- ggplot() +
  geom_sf(data = world) +
  geom_sf(data = cnfd_head, aes(colour = Temperature), size = 2) +
  scale_colour_viridis_c(option = 'A') + 
  coord_sf(xlim = c(10, 20), ylim = c(47, 52), expand = FALSE) +
  theme_bw()

finalni_mapa

# ulozeni mapy do slozky Mapy, nastaveni velikosti
ggsave(plot = finalni_mapa, width = 8, height = 7.2 , dpi = 300, filename = "./Mapy/mapa_zajmove_uzemi.tiff")  

# -------------------------------------------------------------------------
# polygony z Google earth
# -------------------------------------------------------------------------

read_sf('GIS_data\\Palava.kml') %>% 
  ggplot() + 
  geom_sf()

