# Mapa s digitalnim vyskovym modelem jako podkladem
# vegetacnimi snimky rozdelenymi podle formacni skupiny/ klasifikace  
# s barvou podle prumerne indikacni hodnoty pro reakci

# Priprava dat ----------------------------------------------------------------------

# Potrebne knihovny
library(sf)       # knihovna pro praci s prostorovymi objekty (sf)
library(RCzechia) # knihovna obsahujici sf objekty z ArcČR
library(dplyr)    # knihovna pro manipulaci s daty (napr. filtrovani)
library(raster)   # knihovna pro praci s rastry

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



# Mapa 1 ----------------------------------------------------------------------
# pomoci knihovny ggplot2
library(ggplot2)    # knihovna pro tvorbu map, grafů
library(ggspatial)  # pridani meritka, smerovky do mapy
       

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

##### tady by si mohli zkusit udelat tu mapu s vetsimi symboly treba, jen z casti dat (filter)




# Mapa 2  ----------------------------------------------------------------------
# mapa snimku z travniku, kde jsou snimky zobrazeny jako kolacove grafy znazornujici podil x, xx, xxx charakteristik --

# Priprava dat 
# potrebne knihovny 
library(sf) # knihovna pro praci s prostorovymi objekty (sf)

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
library(ggplot2)    # knihovna pro tvorbu map, grafů
library(scatterpie) # tvorba kolacovych grafu jako symbolu
library(ggspatial)  # pridani meritka, smerovky do mapy

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


