library(tidyr)
library(dplyr)
library(ggplot2)
library(shiny)
library(skimr)
library(psych)
library(reshape2)
library(gridExtra)
library(data.table)
library(GA)
library(ggplot2)
library(data.table)
library(stringi)
library(lubridate)
library(Rcpp)
library(fst)
library(tmap)
library(sf)
library(ggplot2)
library(maptools)
library(rgeos)
library(raster)
library(rgdal)
library(colorspace)
library(RColorBrewer)

setwd("d:/2_granty_projekty/2_Bezici/2023_SrUrb/01_reseni_projektu/04_waste_interaction/")
okresy = sf::st_read("okresy.shp")
srcDTA =  read.csv(file = "Vypousteni_do_po_ExportTable.csv",sep = ",",fileEncoding = "UTF-8")

# Filtrace dat na základě podmínek pro sloupce naz_mista a ucel_uziti
filtered_data <- srcDTA %>%
  filter((grepl("ČOV|COV|Na", naz_mista, ignore.case = TRUE)) |
           (grepl("ČOV|Na", ucel_uziti, ignore.case = TRUE)))

# Výstup filtrovaných dat
head(filtered_data)
colnames(srcDTA)


monthly_data <- srcDTA %>%
  select(okres, MNOZ_01:MNOZ_12)

# Převod dat z wide do long formátu, aby bylo možné je snadno vizualizovat
long_data <- srcDTA %>%
  pivot_longer(cols = mnoz_01:mnoz_12, names_to = "month", values_to = "volume")

# Souhrn objemů za jednotlivé měsíce podle okresů
summarized_data <- long_data %>%
  group_by(okres, month) %>%
  summarise(total_volume = mean(volume, na.rm = TRUE))

# Vytvoření grafu pomocí ggplot
ggplot(summarized_data, aes(x = month, y = total_volume, fill = okres)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Suma objemů za jednotlivé měsíce podle okresů",
       x = "Měsíc",
       y = "Celkový objem") +
  theme_minimal()


# Načtení shapefile souboru s okresy
okresy = st_read('okresy.shp')

# Ověření a oprava geometrie, pokud je potřeba
okresy = st_make_valid(okresy)

# Extrakce relevantních dat do data.table
okresy_dta = data.table(okresy)

# Výběr dat týkajících se populace a plochy
okresy_area = okresy_dta[, .(KOD_OKRES, Shape_Area, POCET_OBYV)]

# Oprava případných chybějících hodnot
okresy_area[is.na(POCET_OBYV), POCET_OBYV := 0]

# Vytvoření jednoduché vizualizace mapy s barevnou škálou podle počtu obyvatel
m = tm_layout() +
  tm_shape(okresy) +
  tm_polygons(col = 'POCET_OBYV', breaks = seq(0, 500000, by = 50000), alpha = 0.7, 
              title = 'Počet obyvatel', palette = 'RdBu') +
  tm_legend(legend.outside = TRUE, legend.stack = "vertical", legend.outside.position = 'right') +
  tm_compass()

# Uložení mapy
tmap_save(m, 'path_to_save/population_map.png', dpi = 350, width = 14, height = 6.5)
