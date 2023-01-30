### Vyjadreni kontinualnich a diskretnich dat v gridove siti (kvadranty Pladias) pro uzemi ČR 
## (Vysvetleni ruznych typu scale_fill/color* dostupnych v ggplot2 -> https://stackoverflow.com/questions/70942728/understanding-color-scales-in-ggplot2) 

# potrebne knihovny 
library(sf)            # knihovna pro praci s prostorovymi objekty (sf)
library(RColorBrewer)  # knihovna nabizejici palety barev
library(ggplot2)       # knihovna pro tvorbu map, grafů
library(ggspatial)     # pridani meritka, smerovky do mapy

#### mozna muzeme vsechny knihovny dat nahoru, at je to pohromade / stejne je tam popisek, ja to tak pouzivam radsi

# gridova data CZ ---------------------------------------------------------------------------------------
kvadranty <- read_sf("./GIS_data/shapes/kvadranty_Pladias.shp")  # nacteni shp jako sf objektu


# Mapa poctu druhu v kvadrantech CZ ----------------------------------------------------------------------
# Mapa poctu druhu v kvadrantech Pladias (absolutni hodnoty) -
breaks = c(54, 150, 300, 500, 700, 900, 1200)  # nastaveni intervalu kategorii, do kterych budou data delena (scale_fill_fermenter)

mapa_pocet_druhu_kv <- ggplot() + 
                       geom_sf(data = kvadranty,                  # sf objekt kvadranty Pladias
                               aes(fill = celkova_di)) +          # vypln kvadrantu podle vybraneho atributu
                      #scale_fill_distiller("Počet druhů",        # skalovani vyplne pro kontinualni data
                        # palette = "RdYlBu") +                   # pouziva palety z knihovny RColorBrewer
                      scale_fill_fermenter("Počet druhů",         # skalovani pro kontinualni data 
                                            palette = "Spectral", # pouziva palety z knihovny RColorBrewer 
                                            breaks = breaks) +    # rozdeli data do intervalu 
                      annotation_scale(location="bl",             # vlozeni meritka do leveho dolniho rohu
                                       pad_x = unit(0.3, "in"),   
                                       pad_y = unit (0.3,"in"),   # prostor mezi mapovym ramem a meritkem mapy
                                       width_hint = 0.22) +       # podil plochy mapy, kterou bude meritko zabirat
                      theme(legend.position = c(1,1),             # pozice legendy v mapovem poli
                            legend.justification = c(1,1),        # pozice rohu legendy
                            panel.background = element_blank())   # prazdne pozadi panelu

# ulozeni mapy
ggsave(plot = mapa_pocet_druhu_kv, width = 14, height = 9, dpi = 300, filename = "./Mapy/pocet_druhu_kv.tiff")



# Znazorneni rezidui v kvadrantech Pladiasu -
mapa_rezidua_kv <- ggplot() + 
                   geom_sf(data = kvadranty,                  # sf objekt kvadranty Pladias
                           aes(fill = resid )) +              # vypln kvadrantu podle vybraneho atributu
                   scale_fill_gradient2("Rezidua",            # typ skalovani pro kontinualni data, nadpis legendy
                                        low = "#3399FF",      # barva pro nizke hodnoty
                                        mid = "#FFFFFF",      # barva pro stredni hodnoty
                                        high = "#CC0000",     # barva pro vysoke hodnoty
                                        midpoint = 0) +       # stredni hodnota
                   annotation_scale(location = "bl",          # vlozeni meritka do leveho dolniho rohu
                                    pad_x = unit(0.3, "in"),  
                                    pad_y = unit (0.3,"in"),  # prostor mezi mapovym ramem a meritkem mapy
                                    width_hint = 0.22) +      # podil plochy mapy, kterou bude meritko zabirat
                   theme(legend.position = c(1,1),            # pozice legendy v mapovem poli
                         legend.justification = c(1,1),       # pozice rohu legendy
                         panel.background = element_blank())  # prazdne pozadi panelu

# ulozeni mapy
ggsave(plot = mapa_rezidua_kv, width = 14, height = 9, dpi = 300, filename = "./Mapy/mapa_rezidua_kvadranty.tiff")


# Znazorneni vyskytu ----------------------------------------------------------------------
# Znazorneni vyskytu Lilium martagon v kvadrantech Pladiasu -

# Priprava dat 
Lilium_martagon <- read.csv("./data_csv/Lilium_martagon.csv", 
                            header = TRUE,                        # prvni radek jako hlavicka
                            sep = ";")                            # nastaveni oddelovace

Lilium_martagon_Pladias <- merge(kvadranty, Lilium_martagon)      # propojeni sf objektu a df pomoci sloupce se stejnym nazvem (code)


# Mapa vyskytu Lilium martagon v kvadrantech Pladiasu
mapa_Lilium_martagon <- ggplot() + 
                        geom_sf(data = Lilium_martagon_Pladias,                 # sf objekt kvadrantu Pladias 
                                aes(fill = presence )) +                        # vypln kvadrantu podle atributu presence
                        scale_fill_manual("Lilium martagon",                    # manualni skalovani, nadpis legendy
                                          values = c("yes" = "black",           # nastaveni barvy pro danou hodnotu atributu
                                                    "no" = "white"),
                                          labels = c("presence", "absence")) +  # popisky v legende 
                        annotation_scale(location = "bl",                       # vlozeni meritka do leveho dolniho rohu
                                         pad_x = unit(0.3, "in"), 
                                         pad_y = unit (0.3,"in"),               # prostor mezi mapovym ramem a meritkem mapy
                                         width_hint = 0.22) +                   # podil plochy mapy, kterou bude meritko zabirat
                        theme(legend.position = c(1,1),                         # pozice legendy v mapove kompozici
                              legend.justification = c(1,1),                    # pozice rohu legendy
                              legend.title = element_text(face = "italic"),     # nadpis legendy bude kurzivou
                              panel.background = element_blank())               # prazdne pozadi panelu

# ulozeni mapy
ggsave(plot = mapa_Lilium_martagon, width = 14, height = 9, dpi = 300, filename = "./Mapy/mapa_Lilium_martagon.tiff")


# Mapy vyvoje vybrane charakteristiky v case ----------------------------------------------------------------------
# Shannon index pro Land Use) v kvadrantech Pladiasu (1850 až 2006)

# Potrebna data
Shannon_index <- read.csv("./data_csv/Shannon_index_LU.csv", 
                          header = TRUE,                      # prvni radek jako hlavicka
                          sep = ";")                          # nastaveni oddelovace

Shannon_index_Pladias<-merge(kvadranty, Shannon_index)        # propojeni sf objektu a df pomoci sloupce se stejnym nazvem (code)

breaks <- c(0.2, 0.5, 0.8, 1.1, 1.4)                          # nastaveni intervalu, do kterych budou data delena                                

# Mapa
mapy_Shannon_index <- ggplot()+
                      geom_sf(data = Shannon_index_Pladias,                  # sf objekt kvadranty Pladiasu
                              aes(fill = Shannon_index)) +                   # vypln kvadrantu podle zadaneho atributu
                      scale_fill_fermenter("Shannon index pro Land Use",     # skalovani vhodne pro kontinualni data
                                            palette = "RdYlGn",              # pouziva palety z knihovny RColorBrewer
                                            direction = 1,                   # prevraceni palety
                                            breaks = breaks) +               # rozdeleni dat do intervalu
                      facet_wrap(. ~ Obdobi) +                               # sloupec v datech, pro jehoz kategorie budou tvoreny jednotlive mapy
                      annotation_scale(location = "bl",                      # vlozeni meritka do leveho dolniho rohu
                                       pad_x = unit(0.2, "in"), 
                                       pad_y = unit (0,"in"),                # prostor mezi mapovym ramem a meritkem mapy
                                       width_hint = 0.18) +                  # podil plochy mapy, kterou bude meritko zabirat
                      theme_void()                                           # odstraneni souradnicove site z mapoveho pole

# ulozeni mapy
ggsave(plot = mapy_Shannon_index, width = 15, height = 6, dpi = 300, filename = "./Mapy/mapy_Shannon.tiff")
