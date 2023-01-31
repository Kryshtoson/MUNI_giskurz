### Mapa zajmoveho uzemi se znazornenymi snimky a malou mapou s polohu zajmoveho uzemi v ramci ČR

# Potrebne knihovny ----------------------------------------------------------------------
library(sf)       # knihovna pro praci s prostorovymi objekty (sf) 
library(RCzechia) # knihovna obsahujici sf objekty z ArcČR
library(dplyr)    # knihovna pro manipulaci s daty (napr. filtrovani)

# Nacteni vegetacnich dat ----------------------------------------------------------------------
# Vektorova data (format shp, sf)
# vegetacni snimky ve formatu csv, souradnice, reakce, pocet druhu

#### dotaz odkud je reakce, zdroj?
#### co je FSB ?
#### slo by zmenit vegetacni snimek aby nebyl hacek, dela mi problem ve vysledne mape, popripade jak osetrit, aby se tisklo spravne?

releves <- read.csv("./data_csv/CNFD_snimky.csv",  # nacteni csv souboru 
                    header = TRUE,                 # prvni radek jako hlavicka 
                    sep = ";")                     # sep - oddelovac   

class(releves)   # typ objektu releves
head(releves)    # prvnich sest radku v df
tail(releves)    # poslednich sest radku v df
glimpse (releves) # alternativa k predchozim radkum k zobrazeni dat


releves_sf<- st_as_sf(releves, 
                      coords = c("Longitude", "Latitude"), 
                      crs = 4326)       # prevod df na prostorovy objekt v souradnicovem systemu WGS84
releves_sf<- st_transform(releves_sf, 
                          crs = 32633)  # zmena souradnicoveho systemu na WGS84/UTM33N (sour. system vhodny pro ČR)
glimpse (releves)

# Nacteni podkladovych dat ----------------------------------------------------------------------
# data z ArcČR 
kraje <- kraje()                                           # sf objekt kraje ČR
jmk <- filter(kraje, NAZ_CZNUTS3 == 'Jihomoravský kraj')   # vyber konkretniko kraje
jmk<- st_transform(jmk, 
                   crs = st_crs(releves_sf))               # zmena souradnicoveho systemu na sour. system releves_sf (WGS84/UTM33N)

# bounding box JM kraje - orezani na vybrane uzemi
bb_jmk <- st_bbox(jmk)                                     # rozsah zajmoveho uzemi definovany krajnimi souradnicemi jmk
bb_jmk <- st_as_sfc(bb_jmk)                                # vytvoreni prostoroveho objektu z bounding_box
bb_jmk_mercator <- st_transform(bb_jmk,  
                                crs = st_crs(3857))        # zmena souradnicoveho systemu na souradnicovy system podkladove mapy (WGS 84 / Pseudo-Mercator)


# Mapa pomoci knihovny ggplot2 ----------------------------------------------------------------------
#knihovny
library(basemaps)  # knihovna pro pristup k otevrenym zdrojum podkladovych map (OSM, ESRI,...)
library(ggplot2)   # knihovna pro tvorbu map, grafů
library(ggspatial) # pridani meritka, smerovky do mapy
library(sf)
library(cowplot)   # knihovna pro vlozeni prehledove mapky


get_maptypes()     # nabidka dostupnych mapovych sluzeb a typu map v knihovne basemaps



# Mapa zajmoveho uzemi s vyznacenim kraje a vegetacnimi snimky na podkladove mape z open street maps (osm)
mapa_zajmove_uzemi <- ggplot() + 
                      basemap_gglayer(bb_jmk_mercator,
                                      map_service = "osm",
                                      map_type = "streets") +        # podkladova mapa z osm
                      scale_fill_identity() +                        # nacte barvy, ktere jsou v puvodni podkladove mape a nevytvari legendu
                      geom_sf(data = jmk,                            # vlozeni sf objektu JM kraje 
                              color = "red",                         # cerveny obrys
                              fill = "transparent",                  # pruhledna vypln
                              size = 1.2,                            # sirka obrysu
                              aes(shape = NAZ_CZNUTS3)) +            # automaticka tvorba legendy            
                      geom_sf(data = releves_sf,                     # vlozeni sf objektu
                              shape = 21,                            # vyber symbolu
                              colour = "black",                      # barva obrysu
                              fill = "white",                        # vypln symbolu
                              aes(size = Object)) +                  # automaticka tvroba legendy
                      coord_sf(crs = st_crs (3857)) +                # nastaveni souradnicoveho systemu pro sf objekty
                      theme_void() +                                 # odstraneni souradnicove site z mapoveho pole
                      annotation_scale(location = "bl",              # vlozeni meritka do leveho spodniho rohu
                                       pad_x = unit(0.5, "in"), 
                                       pad_y = unit (0.5,"in")) +    # prostor mezi mapovym ramem a meritkem mapy
                      annotation_north_arrow (location = "tr", 
                                              which_north = "true",  # vlozeni smerovky do praveho horniho rohu
                                              #rotation = -6,        # rotace smerovky, pokud neni mapa orientovana na sever (např. sour. system S-JTSK / Krovak East North)
                                              height = unit(2, "cm"), 
                                              width = unit(2, "cm"),   
                                              pad_x = unit(0.4, "in"), 
                                              pad_y = unit(0.4, "in"),  # prostor mezi mapovym ramem a smerovkou
                                              style = ggspatial::north_arrow_nautical) +  # styl smerovky
                      theme(legend.position = c(0.12,0),                # pozice legendy v mapovem poli (souradnice x,y) 
                            legend.title = element_blank(),             # odstraneni nadpisu v legende 
                            plot.margin = unit(c(1.3,0,0,0), "cm"))     # nastaveni okraju mapy (horni,levy,dolni,pravy)
          
mapa_zajmove_uzemi        

#### asi by bylo dobre zminit, kde se daji najit symboly atd
#### pripadne by chtelo vymyslet, co by mohli primo zkusit vsechno zmenit

# Mapa znazornujici polohu zajmoveho uzemi v ramci ČR 

# vytvoreni obdelniku, v nemz jsou obsazeny vsechny vegetacni snimky
bb_releves <- st_bbox (releves_sf)  # vrcholy hranicniho obdelniku kolem snimku
bb_releves <- st_as_sfc(bb_releves) # vytvoreni prostoroveho objektu z hranicniho obdelniku (knihovna sf)

mapa_ČR <- ggplot() + 
           geom_sf(data = kraje,        # sf objekt kraje ČR
                   fill = "#FFFFCC") +  # vypln (HEX) 
           geom_sf(data = jmk,          # sf objekt JM kraj 
                   color = "red",       # obrys
                   fill = "#FFCC99",    # vypln (HEX)
                   size = 1.1) +        # sirka obrysu 
           geom_sf(data = releves_sf,   # sf objekt vegetacni snimky
                   shape= 21,           # typ symbolu
                   color="black",       # barva obrysu
                   fill = "white",      # barva vyplne
                   size = 0.80) +       # velikost symbolu  
           geom_sf(data = bb_releves,   # bounding box kolem snimku
                   fill = NA,           # bez vyplne
                   color = "black",     # obrys
                   size = 0.8) +        # sirka obrysu
           theme_void()                 # odstraneni souradnicove site z mapoveho pole

##### navrhuju prejmenovat na mapa_CZ aby tam nebyl ten hacek

# vlozeni prehledove mapy (mapa_ČR) do mapoveho pole mapy zajmoveho uzemi (mapa_zajmove_uzemi)
finalni_mapa <-  ggdraw() +
                 draw_plot(mapa_zajmove_uzemi) + # hlavni mapove pole
                 draw_plot(mapa_ČR,              # prehledova mapa
                           x = 0.02, 
                           y = 0.80,             # x, y - poloha mapy v mapovem poli
                           width = 0.22, 
                           height = 0.24)        # rozmery prehledove mapky



# ulozeni mapy do slozky Mapy, nastaveni velikosti
ggsave(plot = finalni_mapa, width = 8, height = 7.2 , dpi = 300, filename = "./Mapy/mapa_zajmove_uzemi.tiff")  
