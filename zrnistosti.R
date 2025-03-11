library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(scales)
library(plotly)
library(readr)
library(tidyr)

#dir
#setwd("S:/Private/_PROJEKTY/2017_Strix_svahy/1_reseni_projektu/00_vyhodnoceni/leden2021finale/ruzne_zasypani/zrnitost/")
setwd("d:/5_papers/2023_zasypane_geotextilie/_dta_work/")


#grainsize_distribution
#grain <- read.csv("S:/Private/_PROJEKTY/2017_Strix_svahy/1_reseni_projektu/00_vyhodnoceni/leden2021finale/ruzne_zasypani/laser_data/143Strix_labDS_vse2.csv", sep = ";", stringsAsFactors = F, header = T)
grain = read.csv("zasypani_zrnitosti.csv", sep = ";", header = T, check.names = F)
xx =colnames(grain)
xx[3] = "misto"
colnames(grain) = xx
grain <- grain %>%
  mutate(variant = recode(variant,
                                 "bare_soil" = "BSOIL",
                                 "bare_RECP" = "BRECP",
                                 "filled_RECP" = "FRECP",
                                 "full_backfill" = "4MFIL",
                                 "1m_backfill" = "1MFIL",
                                 "3m_backfill" = "3MFIL"))


grain_nocumu =  grain[,1:14]
# Convert cumulative data (cols 15 to 115) to non-cumulative
grain_nocumu[, 15:115] <- t(apply(grain[, 15:115], 1, function(x) c(x[1], diff(x))))
colnames(grain_nocumu) = colnames(grain)
# Filtrujeme řádky podle hodnoty ve sloupci "record"

grain_nocumu$PKcode = paste(grain_nocumu$variant, grain_nocumu$intensity,
                            grain_nocumu$init_state, sep = "_")
grain_nocumu$SampleName_variant = paste(grain_nocumu$`Sample Name`, grain_nocumu$variant, grain_nocumu$init_state, sep = "_")

# Vybereme sloupce, které chceme zprůměrovat
columns_to_average <- names(grain_nocumu)[15:115]  # Sloupce 15-115

# PRVNÍCH 5 řádků pro každou hodnotu sample_name
mean_before <- grain_nocumu %>%
  group_by(SampleName_variant) %>%
  slice_head(n = 5) %>%  # Vybereme prvních 5 řádků v každé skupině
  summarise(across(all_of(columns_to_average), mean, na.rm = TRUE)) %>%
  mutate(Group = "First 5")  # Označíme tuto skupinu

# POSLEDNÍCH 5 řádků pro každou hodnotu sample_name
mean_after <- grain_nocumu %>%
  group_by(SampleName_variant) %>%
  slice_tail(n = 5) %>%  # Vybereme posledních 5 řádků v každé skupině
  summarise(across(all_of(columns_to_average), mean, na.rm = TRUE)) %>%
  mutate(Group = "Last 5")  # Označíme tuto skupinu

mean_before$bef_after = "before"
mean_after$bef_after = "after"

# Spojíme oba výsledky do jednoho datového rámce
mean_all <- bind_rows(mean_before, mean_after)
mean_all <- mean_all %>%
  separate(SampleName_variant, into = c("misto", "Date", "sim", "slope", "intensity", 
                                        "init_stateCZ", "surface", "init_state"), sep = "_", remove = FALSE)
mean_all$slope = as.numeric(mean_all$slope)
mean_all$intensity = as.numeric(mean_all$intensity)

grain_mean_long <- mean_all %>%
  pivot_longer(cols = 10:110,
               names_to = "particlesize", 
               values_to = "percentage")# %>%

for (i in unique(grain_mean_long$surface)){
    print(i)
    xx = grain_mean_long[grain_mean_long$surface == i,]
    print(unique(xx$SampleName_variant))
    toplot = ggplot(grain_mean_long[grain_mean_long$surface == i,], aes(x = as.numeric(particlesize),
                                          y = percentage, color = init_state, linetype = bef_after)) +
              geom_line(size = 1) +
              #xlim(0, 630) +
              scale_x_log10() +  # Logaritmická osa x pro lepší přehlednost
              labs(x = "Velikost částic (μm)", y = "(%)", title = paste(i, "Průběh křivky zrnitosti")) +
              facet_grid(intensity ~ .)  +# Rozdělení do panelů podle dvou faktorů
              scale_x_log10(breaks = c(50, 630, 2000))+
              theme_minimal()
              
            
    plot(toplot)
  
      }



# Transformace dat - převod širokého formátu na dlouhý
grain_long <- grain_nocumu %>%
  pivot_longer(cols = 15:115,
               names_to = "particlesize", 
               values_to = "percentage")# %>%





  
# Vykreslení ggplotem
ggplot(grain_long, aes(x = as.numeric(particlesize), y = percentage, color = variant)) +
  geom_point(size = 1) +
  scale_x_log10() +  # Logaritmická osa x pro lepší přehlednost
  labs(x = "Velikost částic (μm)", y = "Kumulativní objem (%)", title = "Průběh křivky zrnitosti") +
  facet_grid(init_state ~ intensity)  +# Rozdělení do panelů podle dvou faktorů
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000))+
  theme_minimal()

grain_long_sel = grain_long[grain_long$intensity == 60,]
grain_long_sel = grain_long_sel[grain_long_sel$init_state == "D1",]
grain_long_sel = grain_long_sel[grain_long_sel$variant == "bare_soil",]

grain_mean_longs = grain_mean_long[grain_mean_long$surface =="BSOIL",]
ggplot(grain_mean_longs, aes(x = as.numeric(particlesize), y = percentage, color = surface, linetype = bef_after)) +
  geom_line(size = 1) +
  scale_x_log10() +  # Logaritmická osa x pro lepší přehlednost
  labs(x = "Velikost částic (μm)", y = "(%)", title = "Průběh křivky zrnitosti") +
  facet_grid(init_state ~ intensity)  +# Rozdělení do panelů podle dvou faktorů
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000))+
  theme_minimal()
varnames <- names(grain)


vars1 <- c("record_number", "sample_name", "misto", "date", "sim", "slope", "intensity", "init_state", "variant",  "measurement_time", "Dx_10", "Dx_50", "Dx_90", "Dx_35")
vars2 <- varnames[15:115] %>% str_replace(pattern = "X", replacement = "") %>% as.numeric() %>% signif(digits = 3) %>% as.character()
names(grain) <- c(vars1, vars2)