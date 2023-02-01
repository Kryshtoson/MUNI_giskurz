# Mapa znazornujici pocet assesmentu ve fytochorionech -> nepravý kartogram -

# potrebne knihovny 
library(sf) # knihovna pro praci s prostorovymi objekty (sf)
library(ggplot2)     # knihovna pro tvorbu map, grafů
library(ggspatial)   # pridani meritka, smerovky do mapy
library(scatterpie)  # tvorba kolacovych grafu jako symbolu
library(RCzechia)      # knihovna obsahujici sf objekty z ArcČR
library(tidyverse)     # knihovna pro manipulaci s daty (napr. filtrovani)


# priprava dat -----------------------------------------------------------------------------
fytochoriony <- read_sf("./GIS_data/shapes/Regiony_wgs2.shp") # nacteni shp jako sf objektu 
# v tomto priprade pouzity regiony vytvorene pro mapovani expanznich druhu

# Mapa pomoci ggplot2 ----------------------------------------------------------------------

mapa_fytochoriony <- ggplot() + 
                     geom_sf(data = fytochoriony,                # sf objekt fytochoriony
                             aes(fill = n)) +                    # vypln polygonu podle atributu n 
                     geom_sf_label(data = fytochoriony,          # pridani popisku k polygonum 
                                   aes(label = n)) +             # atribut pouzity jako popisek
                     scale_fill_viridis_c("n") +                 # nastaveni palety a nazvu legendy
                     annotation_scale(location="bl",             # vlozeni meritka do leveho dolniho rohu
                                      pad_x = unit(0.2, "in"), 
                                      pad_y = unit (0.3,"in"),   # prostor mezi mapovym ramem a meritkem mapy
                                      width_hint = 0.22)+        # podil plochy mapy, kterou bude meritko zabirat
                    theme(legend.position = c(1,1),              # pozice legendy v mapovem poli
                          legend.justification = c(1,1),         # pozice rohu legendy 
                          panel.background = element_blank()) +  # prazdne pozadi
                    theme_void()                                 # odstraneni souradnicove site z mapoveho pole

mapa_fytochoriony

# ulozeni mapy
ggsave(plot = mapa_fytochoriony, width = 14, height = 9, dpi = 300, filename = "./Mapy/mapa_fytochoriony.tiff")


 

# Kartodiagramy vyjadrujici podil pomoci kolacovych grafu ----------------------------------------------------------------------
# podil X, XX a XXX promenych v krajich ČR (smyslena data) -

# csv data
kraje_data <- read.csv("./data_csv/kraje_invaz_stat.csv", 
                             header = TRUE,                      # prvni radek jako hlavicka
                             sep = ";")                          # nastaveni oddelovace

# data z ArcČR
kraje <- kraje()                                                 # sf objekt kraje ČR
kraje_utm33<- st_transform(kraje,                                # zmena koordinacniho systemu
                           crs = 32633)                          # EPSG noveho souradnicoveho systemu, 


# propojeni sf objektu kraju s dataframe
kraje_data_utm33 <- merge(kraje_utm33, kraje_data)

                  

# vytvoreni centroidu kazdeho kraje pro sf objekt
kraje_data_utm33 <- 
  kraje_data_utm33 %>% 
  mutate(lon = map_dbl(geometry, ~ st_centroid(.x)[[1]]),
         lat = map_dbl(geometry, ~ st_centroid(.x)[[2]])) 

st_geometry(kraje_data_utm33) <- NULL                           # odstraneni geometrie ze sf objektu -> zustane jen df 


# mapa pomoci ggplot 

mapa_kartodiagram <- ggplot() +
                     geom_sf(data = kraje_utm33,                           # sf objekt kraju 
                             fill = "#FFFFCC",                             # barva vyplne 
                             aes(color = NAZ_CZNUTS3)) +                   # automaticka legenda
                     scale_color_manual("",                                # prazdny nadpis v legende
                                        values = c("kraje" = "black")) +   # barva obrysu a popisek v legende 
                     geom_scatterpie(data = kraje_data_utm33,              # df s daty, ktere chceme znazornit v kolacovem grafu
                                         aes(x = lon, y = lat),            # sloupec se zemepisnou delkou a sirkou 
                                         cols = c("X", "XX", "XXX")) +     # kategorie znazornene v kolacovem grafu
                     scale_fill_manual("XXX",                              # nadpis legendy     
                                       breaks = c("X", "XX", "XXX"),       # kategorie znazornene v legende
                                       labels = c("X", "XX", "XXX"),       # popisek v legende
                                       values = c("X" = "white",           # nastaveni barvy pro kazdou kategorii
                                                  "XX" = "orange",
                                                  "XXX" = "red")) +
                    annotation_scale(location = "br",                      # vlozeni meritka do praveho dolniho rohu
                                     pad_x = unit(0.3, "in"), 
                                     pad_y = unit (0.3,"in"),              # prostor mezi mapovym ramem a meritkem mapy
                                     width_hint = 0.22) +                  # podil plochy mapy, kterou bude meritko zabirat
                    scale_x_continuous("Zem. delka") +                     # popis osy x
                    scale_y_continuous("Zem. sirka")+                      # popis osy y
                    theme(legend.position = c(1,1),                        # pozice legendy
                          legend.justification = c(1,1),                   # pozice rohu legendy 
                          panel.background = element_blank())              # prazdne pozadi panelu
                      

mapa_kartodiagram

# ulozeni mapy
ggsave(plot = mapa_kartodiagram, width = 14, height = 9, dpi = 300, filename = "./Mapy/mapa_invazni_stat_kraje.tiff")





