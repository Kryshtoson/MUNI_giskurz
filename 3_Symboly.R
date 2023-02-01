# Mapa s digitalnim vyskovym modelem jako podkladem
# vegetacnimi snimky rozdelenymi podle formacni skupiny/ klasifikace  
# s barvou podle prumerne indikacni hodnoty pro reakci

# Potrebne knihovny
library(sf)       # knihovna pro praci s prostorovymi objekty (sf)
library(RCzechia) # knihovna obsahujici sf objekty z ArcČR
library(dplyr)    # knihovna pro manipulaci s daty (napr. filtrovani)
library(raster)   # knihovna pro praci s rastry
library(ggplot2)    # knihovna pro tvorbu map, grafů
library(ggspatial)  # pridani meritka, smerovky do mapy
library(ggplot2)    # knihovna pro tvorbu map, grafů
library(scatterpie) # tvorba kolacovych grafu jako symbolu
library(ggspatial)  # pridani meritka, smerovky do mapy


# Vegetacni data  ----------------------------------------------------------------------
# Vektorova data (format shp, sf)
# vegetacni snimky ve formatu csv
releves <- read.csv("./data_csv/CNFD_snimky.csv", # nacteni csv souboru
                    header = TRUE,                # prvni radek jako hlavicka
                    sep = ";")                    # nastaveni oddelovace


releves_sf<- st_as_sf(releves,                             # prevod df snimku na prostorovy objekt
                      coords = c("Longitude", "Latitude"), # sloupec se zem. delkou a zem. sirkou
                      crs = 4326)                          # souradnicovem system, v kterem jsou souradnice (WGS84)

releves_sf<- st_transform(releves_sf, 
                          crs = 32633)                     # zmena souradnicoveho systemu na WGS84/UTM33N 

# Podkladova data ----------------------------------------------------------------------
# data z ArcČR 

# kraje
kraje <- kraje()                                    # sf objekt kraje ČR
jmk <- filter(kraje, 
              NAZ_CZNUTS3 == 'Jihomoravský kraj')   # vyber konkretniko kraje

jmk<- st_transform(jmk, 
                   crs = st_crs(releves_sf))        # zmena souradnicoveho systemu na sour. system releves_sf (WGS84/UTM33N)

bb_jmk <- st_bbox(jmk)                              # rozsah zajmoveho uzemi definovanymi krajnimi souradnicemi jmk
bb_jmk <- st_as_sfc(bb_jmk)                         # vytvoreni prostoroveho objektu z bb_jmk

# reky
vodni_toky <- reky()                                          # sf objekt reky ČR  
vodni_toky<- st_transform(vodni_toky, 
                          crs = st_crs(releves_sf))           # zmena souradnicoveho systemu na sour. system releves_sf
vodni_toky_major<- filter(vodni_toky, 
                          Major == "TRUE" )                   # vyber pouze hlavnich rek ČR

# orez hlavnich ceskych rek jen pro zajmove uzemi
vodni_toky_jmk <- st_intersection(vodni_toky_major, bb_jmk)   # orezani hlavnich rek ČR na bounding box JM kraje 


# Digitalni vyskovy model DEM ----------------------------------------------------------------------
# Rastrova data (tif)
dem <- raster("./GIS_data/grids/dem_100.tif")   # rastr digitalniho vyskoveho modelu (rozliseni 100 x 100 m)
dem                                             # zakladni informace o rastru (rozlisení - resolution, rozsah - extent, koordinacní system - crs)

plot(dem)                                       # vykresleni rastru

#dem<- projectRaster (dem, 
                     #crs = crs(releves_sf))    # zmena sourad. systemu rastru 

# Orez rastru
dem_jmk1 <- crop(dem, extent(jmk))              # orez rastru podle hranicniho obdelniku kolem jmk (knihovna raster)
dem_jmk2 <- mask(dem, jmk)                      # orez podle hranic jmk (knihovna raster)

par(mfrow=c(1,2))                               # 2 grafy vedle sebe v jednom okne
plot(dem_jmk1)
plot(dem_jmk2)



# vytvoreni palet pro dem a pro naskalovani barvy symbolu podle prumerne pudni reakce  
palette_dem <- c("#8FCE00", "#FFE599", "#F6B26B",	"#996633", "#993300")       # paleta pro dem (HEX)
palette_reaction <- c ("#FF0000", "#FF9933", "#FFFF33", "#99FF33", "#3399FF") # paleta pro pudni reakci



# funkce pro pridani nekolika skalovani obrysu nebo vyplne  #########
# dle atributu v ggplot2 (nic se nemusi upravovat)
# ve skriptu ggplot se prida: new_scale("fill") nebo new_scale ("colour")


new_scale <- function(new_aes) {
  structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
}

#' Convenient functions
new_scale_fill <- function() {
  new_scale("fill")
}

new_scale_color <- function() {
  new_scale("colour")
}

new_scale_colour <- function() {
  new_scale("colour")
}

#' Special behaviour of the "+" for adding a `new_aes` object
#' It changes the name of the aesthethic for the previous layers, appending
#' "_new" to them. 
ggplot_add.new_aes <- function(object, plot, object_name) {
  plot$layers <- lapply(plot$layers, bump_aes, new_aes = object)
  plot$scales$scales <- lapply(plot$scales$scales, bump_aes, new_aes = object)
  plot$labels <- bump_aes(plot$labels, new_aes = object)
  plot
}


bump_aes <- function(layer, new_aes) {
  UseMethod("bump_aes")
}

bump_aes.Scale <- function(layer, new_aes) {
  old_aes <- layer$aesthetics[remove_new(layer$aesthetics) %in% new_aes]
  new_aes <- paste0(old_aes, "_new")
  
  layer$aesthetics[layer$aesthetics %in% old_aes] <- new_aes
  
  if (is.character(layer$guide)) {
    layer$guide <- match.fun(paste("guide_", layer$guide, sep = ""))()
  }
  layer$guide$available_aes[layer$guide$available_aes %in% old_aes] <- new_aes
  layer
}

bump_aes.Layer <- function(layer, new_aes) {
  original_aes <- new_aes
  
  old_aes <- names(layer$mapping)[remove_new(names(layer$mapping)) %in% new_aes]
  new_aes <- paste0(old_aes, "_new")
  
  old_geom <- layer$geom
  
  old_setup <- old_geom$handle_na
  new_setup <- function(self, data, params) {
    colnames(data)[colnames(data) %in% new_aes] <- original_aes
    old_setup(data, params)
  }
  
  new_geom <- ggplot2::ggproto(paste0("New", class(old_geom)[1]), old_geom,
                               handle_na = new_setup)
  
  new_geom$default_aes <- change_name(new_geom$default_aes, old_aes, new_aes)
  new_geom$non_missing_aes <- change_name(new_geom$non_missing_aes, old_aes, new_aes)
  new_geom$required_aes <- change_name(new_geom$required_aes, old_aes, new_aes)
  new_geom$optional_aes <- change_name(new_geom$optional_aes, old_aes, new_aes)
  
  layer$geom <- new_geom
  
  old_stat <- layer$stat
  
  old_setup2 <- old_stat$handle_na
  new_setup <- function(self, data, params) {
    colnames(data)[colnames(data) %in% new_aes] <- original_aes
    old_setup2(data, params)
  }
  
  new_stat <- ggplot2::ggproto(paste0("New", class(old_stat)[1]), old_stat,
                               handle_na = new_setup)
  
  new_stat$default_aes <- change_name(new_stat$default_aes, old_aes, new_aes)
  new_stat$non_missing_aes <- change_name(new_stat$non_missing_aes, old_aes, new_aes)
  new_stat$required_aes <- change_name(new_stat$required_aes, old_aes, new_aes)
  new_stat$optional_aes <- change_name(new_stat$optional_aes, old_aes, new_aes)
  
  layer$stat <- new_stat
  
  layer$mapping <- change_name(layer$mapping, old_aes, new_aes)
  layer
}

bump_aes.list <- function(layer, new_aes) {
  old_aes <-  names(layer)[remove_new(names(layer)) %in% new_aes]
  new_aes <- paste0(old_aes, "_new")
  
  names(layer)[names(layer) %in% old_aes] <- new_aes
  layer
}

change_name <- function(list, old, new) {
  UseMethod("change_name")
}

change_name.character <- function(list, old, new) {
  list[list %in% old] <- new
  list
}

change_name.default <- function(list, old, new) {
  nam <- names(list)
  nam[nam %in% old] <- new
  names(list) <- nam
  list
}

change_name.NULL <- function(list, old, new) {
  NULL
}

remove_new <- function(aes) {
  stringi::stri_replace_all(aes, "", regex = "(_new)*")
}






# Mapa 1 ----------------------------------------------------------------------
# pomoci knihovny ggplot2    

# rastrova data potreba prevest na dataframe
dem_pts <- rasterToPoints(dem_jmk1, spatial = TRUE)  # prevod rastru na body s atributem nadmorske vysky
dem_df <- data.frame(dem_pts)                        # prevod prostoroveho objektu na dataframe



mapa_symboly <- ggplot() +
                geom_raster(data = dem_df ,            # dataframe dem
                            aes(x = x, y = y,          # x - zem. delka, y - zem. sirka,
                            fill = dem_100)) +         # atribut, ktery chceme znazornit
                scale_fill_gradientn(colours = palette_dem,                  # kontinualni naskalovani barvy rastru
                                     name = "Nadmořská výška [m n. m.]")  +  # nadpis legendy
                geom_sf(data = vodni_toky_jmk,         # sf objekt vodnich toku
                        col = "blue",                  # barva obrysu
                        size = 0.7) +                  # sirka obrysu
                new_scale("fill") +                    # nejdrive potreba spustit skript Funkce_multiple_color_or_fill_scale
                geom_sf(data = releves_sf,             # sf objekt vegetacnich snimku
                        color = "black",               # barva obrysu
                        size = 2.25,                   # velikost symbolu  
                        aes(shape = FSB,               # typ symbolu podle atributu FSB / formacni skupina biotopu (les, travnik..)
                            fill = Mean_reaction))+    # barva vyplne podle atributu prumerna indikacni h. pro reakci
                            #, size = Number_of_sp ))  # velikost podle atributu poctu druhu    
                            #scale_size("Počet druhů", # nadpis legendy
                            #breaks = c(20,40,80,101), # intervaly pro velikost symbolu podle poctu druhu
                            #labels = c("< 20", 
                                       #"20–40", 
                                       #"41-80", 
                                       #"> 80"))+       # popis v legende 
                  scale_shape_manual("Formační skupina biotopů",   # nadpis legendy
                                     values = c("K" = 21,          # nastaveni symbolu pro kroviny
                                                "L" = 22,          # nastaveni symbolu pro lesy
                                                "M" = 23,          # nastaveni symbolu pro mokrady
                                                "T" = 24),         # nastaveni symbolu pro travniky 
                                     labels = c ("Křoviny", "Lesy", "Mokřady", "Trávníky")) + # popis v legende
                   scale_fill_stepsn (colours = palette_reaction,   # paleta pro skalovani barvy symbolu diskretne
                                      breaks = seq(1, 8, by = 2),   # nastaveni intervalu
                                      name = "Průměrná ind. hod. pro reakci") + # nadpis legendy
                   new_scale("colour") +
                   geom_sf(data = jmk,                              # sf objekt JM kraj
                           fill = "transparent",                    # pruhledna vypln polygonu
                           aes(colour = NAZ_CZNUTS3),               # automaticka tvorba legendy
                           size = 0.6) +                            # sirka obrysu
                   scale_color_manual("",                           # prazdny nadpis v legende
                                      values = c("Jihomoravský kraj" = "red")) + # nastaveni obrysu JM kraje
                   annotation_scale(location="br",                  # vlozeni meritka do praveho dolniho rohu
                                    pad_x = unit(0.3, "in"), 
                                    pad_y = unit (0.3,"in"),        # prostor mezi mapovym ramem a meritkem mapy
                                    width_hint = 0.22) + 
                   theme(plot.margin = unit(c(0,0,0,0), "cm"),      # nastaveni okraju mapy (horni,levy,dolni,pravy)
                         legend.justification = c(1,1),
                         legend.position = c("right"),              # pozice legendy
                         panel.background = element_blank()) +      # pruhledne pozadi
                   theme_void()                                     # odstraneni souradnicove site z mapoveho pole
                     
mapa_symboly


# vytvoreni histogramu pro pocet snimku podel gradientu reakce, vlozeni vedle mapy
colors <- c(rep("#FF0000",1), rep("#FF9933",10), rep("#99FF33",10), rep("#3399FF", 4)) # nastaveni barev pro sloupce v histogramu

legend_plot <- ggplot(releves,                           # dataframe
                      aes(x = Mean_reaction)) +          # atribut znazornen na ose x   
               geom_histogram (bins = 25,                # histogram s 25 sloupci   
                               fill = colors)+           # vypln 
               xlab("Průměrná ind. hod. pro reakci") +   # nazev osy x
               ylab("Počet vegetačních snímků")+         # nazev osy y
               theme(axis.title=element_text(size=6),    # velikost popisku osy
               legend.position = "none")                 # bez legendy 

legend_grob <- ggplotGrob(legend_plot)                   # prevod grafu na grob, aby ho bylo mozne vlozit do mapy

# vytvoreni finalni mapy s histogramem
finalni_mapa <- mapa_symboly + 
                annotation_custom(grob = legend_grob,      # grob graf objekt
                                  xmin = 694157.8 + 8000,  # pozice histogramu v mapove kompozici (dle sour. systemu)
                                  xmax = 694157.8 + 34000, 
                                  ymin = 5386649 - 8000,
                                  ymax = 5386649+16000) 

# ulozeni mapy
ggsave(plot = finalni_mapa, width = 14, height = 9, dpi = 300, filename = "./Mapy/mapa_symboly.tiff")  






# Mapa 2  ----------------------------------------------------------------------
# mapa snimku z travniku, kde jsou snimky zobrazeny jako kolacove grafy znazornujici podil x, xx, xxx charakteristik --

# vegetacni snimky travniku
releves_invazni_stat <- read.csv("./data_csv/snimky_invaz_stat.csv", 
                        header = TRUE,                                   # nacteni dat s hlavickou
                        sep = ";")                                       # nastaveni oddelovace

releves_invazni_stat_sf <- st_as_sf(releves_invazni_stat,                # df pro prevod na sf
                                   coords = c("Longitude", "Latitude"),  # sloupce se souradnicemi
                                   crs = 4326)                           # souradnicovy system v kterem jsou souradnice

releves_invazni_stat_sf<- st_transform(releves_invazni_stat_sf, 
                                        crs = 32633)                     # zmena koordinacniho systemu z WGS84 na WGS84/UTM33N

releves_invazni_stat_utm33<-st_coordinates(releves_invazni_stat_sf)      # extrakce souradnic ze sf objektu 

colnames(releves_invazni_stat_utm33) <- c("Longitude", "Latitude")       # pojmenovani sloupcu 
releves_invazni_stat_utm33 <- cbind(releves_invazni_stat[c(1,4:7)],releves_invazni_stat_utm33 ) # spojeni df souradnic ve WGS84/UTM33N a poctem druhu podle inv. statusu


# Vytvoreni mapy

mapa_invaz_stat <- ggplot() + 
                   geom_raster(data = dem_df,                                    # dataframe dem
                               aes(x = x, y = y, fill = dem_100)) +              # x - zem. delka, y - zem. sirka, fill - atribut
                   scale_fill_gradientn(colours = palette_dem,                   # kontinualni skalovani, nastaveni barevne skaly
                                        name = "Nadmořská výška [m n. m.]") +    # nadpis legendy
                   geom_sf(data = vodni_toky_jmk,                                # sf objekt vodnich toku v JM kraji
                           col="blue",                                           # barva cary
                           size = 0.7) +                                         # sirka cary
                   geom_sf(data = jmk,                                           # sf objekt JM kraj  
                           fill = "transparent",                                 # pruhledna vypln polygonu
                   aes(colour = NAZ_CZNUTS3),                                    # barva obrysu podle NAZ_CZNUTS3
                       size = 0.6) +                                             # sirka cary          
  
                   scale_color_manual("",                                        # prazdny nadpis v legende
                                      values = c("Jihomoravský kraj" = "red")) + # nastaveni barvy obrysu 
                   new_scale("fill") +
                   geom_scatterpie(data = releves_invazni_stat_utm33,            # kolacove grafy, data musi byt df
                                   aes(Longitude, Latitude),                     # sloupec se zem. delkou a sirkou
                                   cols = c("X", "XX", "XXX"),                   # kategorie, ktere maji byt v grafu
                                   alpha = 0.8,                                  # pruhlednost kolac. grafu
                                   pie_scale = 0.93) +                           # velikost kolac. grafu
                   scale_fill_manual( "Nadpis legendy",                          # nadpis legendy         
                                       breaks = c("X", "XX", "XXX"),             # kategorie v kolacovem grafu
                                       labels = c("X", "XX", "XXX"),             # popisky v legende
                                       values = c("X" = "white",                 # prirazeni barvy ke kategorii
                                                  "XX" = "orange",
                                                  "XXX" = "red"))+
                   annotation_scale(location="br",                               # vlozeni meritka do praveho dolniho rohu
                                    pad_x = unit(0.3, "in"), 
                                    pad_y = unit (0.3,"in"),                     # prostor mezi mapovym ramem a meritkem mapy
                                    width_hint = 0.22) +
                  labs(title = "PODÍL XX VE SNÍMCÍCH TRAVINNÉ VEGETACE \n PODLE XX", # nadpis mapy - vždy KAPITÁLKY, \n - novy radek
                      #subtitle = "Podnadpis",                                       # podnadpis   
                       caption = "Zdroj: ČNFD",                                      # tiraz
                       fill = NULL)  +
                  theme_bw() +                                                       # pruhledne pozadi mapoveho pole
                  theme(legend.position = c(0.3, -0.3),                              # pozice legendy v mapovem poli
                        legend.justification = c(0.3, -0.3),
                        legend.direction = "horizontal",                             # smer legendy (defaultne - "vertical")
                        panel.grid = element_blank(),                                # pruhledna barva mapoveho pole 
                        panel.border = element_blank(),                              # bez okraje
                        axis.title = element_blank(),                                # bez popisku os      
                        axis.text = element_blank(),                                 
                        axis.ticks = element_blank(),
                        plot.title = element_text(size=14, hjust = 0.5))             # nadpis mapy, hjust - zarovnani doprostred
  
mapa_invaz_stat

# ulozeni mapy
ggsave(plot = mapa_invaz_stat, width = 8, height = 8.2, dpi = 300, filename = "./Mapy/mapa_invaz_stat.tiff")  


