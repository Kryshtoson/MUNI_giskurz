library(raster)
library(spatialEco)
library(whitebox)
library(readxl)
library(sf)
library(tidyverse)

stromy <- c('Quercus petraea agg.',
  'Quercus robur',
  'Quercus pubescens agg.',
  'Quercus rubra',
  'Quercus cerris',
  'Acer platanoides',
  'Acer campestre',
  'Acer pseudoplatanus',
  'Tilia cordata',
  'Tilia platyphyllos',
  'Fraxinus excelsior',
  'Fraxinus angustifolia',
  'Carpinus betulus',
  'Betula pendula',
  'Fagus sylvatica',
  'Sorbus aucuparia',
  'Sorbus torminalis',
  'Sorbus domestica',
  'Sorbus aria agg.',
  'Ulmus laevis',
  'Ulmus minor',
  'Salix euxina',
  'Salix alba',
  'Alnus glutinosa',
  'Picea abies',
  'Abies alba',
  'Pinus sylvestris'
  )

# funkce pro vytvoreni zkratek jmen druhu (Acer pseudoplatanus = Ace.pse)
make_names <- function(v){
  paste(str_sub(word(v), 1,3), 
        str_sub(word(v, 2), 1,3), 
        sep = '.')
}

# -------------------------------------------------------------------------
# data
# -------------------------------------------------------------------------
# digitalni model reliefu 100 m rozliseni
dem_path <- 'GIS_data/grids/dem_100.tif'
dem <- raster(dem_path)

# hlavičky čnfd snímků
cnfd_head <- read_xlsx('data_csv/CNFD-selection-2023-01-09-IA.xlsx')

# druhová data
# vybereme jen nektere stromy
# koordinaty jmse pouzili k vytvoreni sf filu
spe <- read_delim('woody.spp.txt') %>% filter(Species %in% stromy) %>% 
  mutate(spe_abb = make_names(Species)) %>% 
  left_join(cnfd_head %>% select(PlotID, deg_lon, deg_lat)) %>% 
  st_as_sf(coords = c("deg_lon", "deg_lat"), crs = 4326) %>% 
  st_transform(32633)

# vybereme pouze snimky, kde DEM ma nejake hodnoty
spe_selected <- spe[!is.na(raster::extract(dem, spe)),]

# v hlavickach jsou i EIV, tak si je muzeme zobrazit v grafu
spe_selected %>% 
  left_join(cnfd_head %>% select(PlotID, Light, Moisture)) %>% 
  group_by(spe_abb) %>% 
  summarise_at(c('Light', 'Moisture'), mean) %>% 
  ggplot(aes(Light, Moisture)) + 
  geom_text(aes(label = spe_abb))

# -------------------------------------------------------------------------
# topografie, morfometrické parametry
# -------------------------------------------------------------------------
wbt_fill_single_cell_pits(
  dem = dem_path, 
  output = 'twi_meta/my_rast_filled.tif'
)

wbt_breach_depressions_least_cost(
  dem = dem_path, 
  output = 'twi_meta/my_rast_filled_breached.tif', 
  dist = 5
)

wbt_d8_flow_accumulation(
  input = 'twi_meta/my_rast_filled_breached.tif', 
  output = 'twi_meta/my_rast_d8fa.tif', 
  out_type = 'cells'
)

twi <- log((raster('twi_meta/my_rast_d8fa.tif'))/tan(terrain(dem, 'slope', 'radians')))
twi[is.infinite(twi)]<-NA
names(twi) <- 'twi'

# -------------------------------------------------------------------------
# ostatni morfometricke parametry
# -------------------------------------------------------------------------
slp <- terrain(dem, 'slope', 'degrees')
tpi <- tpi(dem, w = 3)
names(tpi) <- 'tpi'
hli <- hli(dem)
names(hli) <- 'hli'
tri <- tri(dem, s = 3)
names(tri) <- 'tri'
vrm <- vrm(dem, s = 3)
names(vrm) <- 'vrm'

# -------------------------------------------------------------------------
# extract
# -------------------------------------------------------------------------
plot(dem)
plot(spe_selected, add = T)

data <- raster::extract(stack(dem, twi, slp, tpi, hli, tri, vrm), spe_selected)

bind_cols(spe_selected, data) %>% 
  as_tibble() %>% 
  select(spe_abb, dem_100:vrm)  %>% 
  group_by(spe_abb) %>% 
  summarise_all(function(x){mean(x, na.rm = T)}) -> dd

mean_EIV <- spe_selected %>% left_join(cnfd_head %>% select(PlotID, Light, Moisture)) %>% 
  group_by(spe_abb) %>% 
  summarise_at(c('Light', 'Moisture'), mean) 

dd %>% left_join(mean_EIV) %>% 
  ggplot(aes(twi, Moisture)) + 
  geom_text(aes(label = spe_abb))

# -------------------------------------------------------------------------
# 
# -------------------------------------------------------------------------
