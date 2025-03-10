###### INICIALIZACE ######
library(tidyr)
library(dplyr)
library(ggplot2)
library(shiny)
#library(skimr)
library(stringr)
library(tidyr)
library(tools)
library(dplyr)
library(ggplot2)
library(zoo)
#library(gridExtra)
#library(psych)
#library(gdata)
library(tibble)
require(data.table)
library(tidyr)
library(tools)
library(dplyr)
library(ggplot2)
library(zoo)
#library(gridExtra)
#library(gdata)
library(tibble)
#library(corrplot)
require(data.table)
library(cluster)
#library(factoextra)
#library(ggsankey)
#library(clusterCrit)
#library(forcats)
# library(DT)

##### ?TEN? DAT ######

# setwd("E:/Projekty_a_granty/TACR_Strix/")

setwd("d:/5_papers/2023_zasypane_geotextilie/ruzne_zasypani/")


### Na?ti dataset s pr?b?hy odtok? a koncentrac?
qsres <- read.csv("zasypani_odtoky_MN.csv", stringsAsFactors = F, sep = ";", dec = ".", header = T)
#all_in <- read.csv("xlab_data_clean.csv", stringsAsFactors = F)
grain = read.csv("d:/5_papers/2023_zasypane_geotextilie/_dta_work/grain_size_export.csv", stringsAsFactors = F, sep = ";", dec = ",", header = T)

####READ GRAIN SIZE RES DTA#####

qsres$TIMESTAMP = as.POSIXct(strptime(qsres$datum, "%Y-%m-%d"))
#all_in$TIMESTAMP = as.POSIXct(strptime(all_in$datum, "%Y-%m-%d"))

#all_in_sel = subset(all_in, all_in$datum == "2020-11-02" | all_in$datum == "2020-11-05")

#qsres = rbind(qsres, all_in_sel)
qsres$poc_stav = substr(qsres$poc_stav, 1, 4)

qsres <- qsres %>% filter(!is.na(doba_s))
# Add a new column with corresponding names as runoff
qsres <- qsres %>%
  mutate(opat_typ_grain = recode(opat_typ,
                                 "uhor" = "BSOIL",
                                 "H4VP" = "BRECP",
                                 "H4MZ" = "FRECP",
                                 "H4MP" = "4MFIL",
                                 "H1MP" = "1MFIL",
                                 "H3MP" = "3MFIL"))

qsres <- qsres %>%
  mutate(poc_stav = recode(poc_stav,
                                 "such" = "dry",
                                 "mokr" = "wet"))
qsres <- qsres %>%
  mutate(poc_stav_int = paste(intenzita, poc_stav, sep = "_"))
qsres <- qsres %>%
  mutate(poc_stav_int = recode(poc_stav_int,
                               "60_dry" = "1 60 dry",
                               "60_wet" = "2 60 wet",
                               "120_wet" = "3 120 wet",
                               "160_wet" = "4 160 wet"))


xx = str(qsres)
qsresnF = qsres
qsres <- qsres %>% mutate(lokalita = as.factor(lokalita),
                          datum = as.Date(datum,tryFormats = c("%d.%m.%Y")),
                          simcislo = as.factor(simcislo),
                          # plocha = as.factor(plocha),
                          sklon_st = str_replace(sklon_st,"20","22"),
                          sklon_st = as.factor(sklon_st),
                          opat_typ = as.factor(opat_typ),
                          poc_stav = as.factor(poc_stav),
                          intenzita = as.factor(intenzita),
                          repetice = as.factor(repetice),
                          system = as.factor(system),
                          ID = as.factor(ID))
                          #NAvalue = as.factor(NAvalue))



#qsstat <- read.csv("W_special_zasypani_stat.csv", stringsAsFactors = F)

# select total value at the end

qsresnF <- qsresnF %>%
  mutate(max_ID = paste(opat_typ, poc_stav, intenzita, sep = "_"))

#qsresnF <- qsresnF %>% mutate(opatreni = ifelse(opat_typ %in% c('UHO2','uhor' ), FALSE, TRUE))
maxrun = data.frame()
names = unique(qsresnF$max_ID)
for (id in names){
  print(id)
  sl <- qsresnF[qsresnF$max_ID == id,]
  max_row <- sl %>% filter(t2_min == max(t2_min, na.rm = TRUE))
  maxrun <- rbind(maxrun, max_row)
}
qsresnF$poc_stav_int_time = paste(qsresnF$poc_stav_int, qsresnF$interval, sep = "_" )
uhor_only = qsresnF[qsresnF$opat_typ_grain == "BSOIL",]
uhor_only <- uhor_only[, c("poc_stav_int_time", "S2cum_g", "Qcum_l")]
colnames(uhor_only) = c("poc_stav_int_time", "BareS2cum_g", "BareQcum_l")
qsresnF <- merge(qsresnF, uhor_only, by = "poc_stav_int_time", all.x = TRUE)
qsresnF$SLR = qsresnF$S2cum_g/qsresnF$BareS2cum_g
qsresnF$WLR = qsresnF$Qcum_l/qsresnF$BareQcum_l

grain <- grain %>%
  mutate(poc_stav_en = recode(init_state,
                              "D1" = "dry",
                              "D2" = "dry",
                              "D3" = "dry",
                              "W1" = "wet",
                              "W2" = "wet",
                              "W3" = "wet"
  ))
grain <- grain %>%
  mutate(poc_stav_int = paste(intensity, poc_stav_en, sep = "_"))
grain <- grain %>%
  mutate(poc_stav_int = recode(poc_stav_int,
                              "60_dry" = "1 60 dry",
                              "60_wet" = "2 60 wet",
                              "120_wet" = "3 120 wet",
                              "160_wet" = "4 160 wet"))
         

summary_table <- grain %>%
  group_by(poc_stav_int, aggregates, particlesize, variant_runoff, init_state_num) %>%
  summarize(
    median_percentage = median(percentage)
  ) %>%
  ungroup()
summary_table$opat_typ_grain = summary_table$variant_runoff
summary_table <- summary_table %>%
  mutate(grain_id = paste(opat_typ_grain, poc_stav_int, init_state_num, particlesize, sep = "_"))
sdata_wide <- summary_table %>%
  pivot_wider(names_from = aggregates, values_from = c(median_percentage, particlesize))
sdata_wide$ratio = sdata_wide$median_percentage_without - sdata_wide$median_percentage_with

sdata_wide <- sdata_wide %>%
  mutate(init_state_char = recode(init_state_num,
                               "5" = "05",
                               "15" = "15",
                               "30" = "30",
                               ))
sdata_wide <- sdata_wide %>%
  mutate(g_ID = paste(poc_stav_int, init_state_char, sep = "_"))
###### - plot data
qsresnF_sel = qsresnF#[qsresnF$opat_typ_grain != "BSOIL",]
summary_table_sel = summary_table#[summary_table$opat_typ_grain != "BSOIL",]
summary_table_sel <- summary_table_sel %>% drop_na()
sdata_wide_sel = sdata_wide#[sdata_wide$opat_typ_grain == "BSOIL",]


scaleFactorq <- max(qsresnF_sel$prutok2_l.min) / max(qsresnF_sel$smyv1_g.l)
scaleFactor <- median(qsresnF_sel$Qcum_l) / median(qsresnF_sel$S2cum_g)
scaleFactor_grain <- max(qsresnF_sel$Qcum_l) / 60
xxplot  = ggplot() + #(qsresnF_sel$cas_do_odtok + (qsresnF_sel$t1_min + qsresnF_sel$t2_min)/2)))+
  geom_line (data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$Qcum_l), method="loess", col="black") +
  geom_point (data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$Qcum_l), method="loess", col="black") +
  geom_line(data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$S2cum_g * scaleFactor), method="loess", col="grey", show.legend = TRUE) +
  geom_point(data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$S2cum_g * scaleFactor), method="loess", col="grey") +
  geom_point(data = summary_table_sel, mapping = aes(x = summary_table_sel$init_state_num, y = summary_table_sel$median_percentage*scaleFactor_grain, color = as.character(summary_table_sel$particlesize), shape = as.character(summary_table_sel$aggregates))) +
  
  #                   stat_summary(mapping = aes(x = grain$init_state_num , y = grain$percentage, colour = grain$aggregates, shape = as.factor(grain$particlesize)), fun = median, geom="point", show.legend = FALSE, vjust=-1, size = 10)+
  scale_y_continuous(name="\u03A3 l", sec.axis=sec_axis(~./scaleFactor, name="g/l")) +
  facet_grid (opat_typ_grain ~ poc_stav_int) +
  ylim (0, 350) +
  theme_bw()




plot(xxplot)

xxplot_Q  = ggplot() + #(qsresnF_sel$cas_do_odtok + (qsresnF_sel$t1_min + qsresnF_sel$t2_min)/2)))+
  geom_line (data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$Qcum_l), method="loess", col="black") +
  geom_point (data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$Qcum_l, colour = qsresnF_sel$opat_typ_grain), method="loess") +
  #geom_point (data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$perkolace*10), method="loess", col = "blue", size = 1) +
  facet_grid (poc_stav_int ~ opat_typ_grain) +
  labs(title = "Runoff", x = "Time", y = "Cumulative runoff [l]")+
   
  theme_bw()




plot(xxplot_Q)


xxplot_PQ  = ggplot() + #(qsresnF_sel$cas_do_odtok + (qsresnF_sel$t1_min + qsresnF_sel$t2_min)/2)))+
   #geom_line (data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$Qcum_l), method="loess", col="black") +
   #geom_point (data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$Qcum_l, colour = qsresnF_sel$opat_typ_grain), method="loess") +
   geom_point (data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$perkolace, colour = qsresnF_sel$opat_typ_grain), method="loess", size = 1) +
   facet_grid (poc_stav_int ~ opat_typ_grain) +
   labs(title = "Percolation", x = "Time", y = "Cumulative percolation [l]")+
   
   theme_bw()

plot(xxplot_PQ)


xxplot_S  = ggplot() + #(qsresnF_sel$cas_do_odtok + (qsresnF_sel$t1_min + qsresnF_sel$t2_min)/2)))+
   geom_line(data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$S2cum_g), method="loess", col="grey", show.legend = TRUE) +
   geom_point(data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$S2cum_g, colour = qsresnF_sel$opat_typ_grain), method="loess") +
   facet_grid (poc_stav_int ~ opat_typ_grain, scales = "free_y") +
   #ylim (0, 37000) +
   labs(title = "Soil loss", x = "Time", y = "Cumulative soil loss [g]")+
   theme_bw()

plot(xxplot_S)

qsresnF_selNoBare = qsresnF_sel[qsresnF_sel$opat_typ_grain != "BSOIL",]
xxplot_SLR  = ggplot() + #(qsresnF_selNoBare$cas_do_odtok + (qsresnF_selNoBare$t1_min + qsresnF_selNoBare$t2_min)/2)))+
   geom_line(data = qsresnF_selNoBare, mapping = aes(x = qsresnF_selNoBare$t2_min, y=qsresnF_selNoBare$SLR), method="loess", col="grey", show.legend = TRUE) +
   geom_point(data = qsresnF_selNoBare, mapping = aes(x = qsresnF_selNoBare$t2_min, y=qsresnF_selNoBare$SLR, colour = qsresnF_selNoBare$opat_typ_grain), method="loess") +
   facet_grid (poc_stav_int ~ opat_typ_grain, scales = "free_y") +
   ylim (0, 4) +
   labs(title = "SLR", x = "Time", y = "Soil loss ratio")+
   theme_bw()
plot(xxplot_SLR)

xxplot_WLR  = ggplot() + #(qsresnF_selNoBare$cas_do_odtok + (qsresnF_selNoBare$t1_min + qsresnF_selNoBare$t2_min)/2)))+
   geom_line(data = qsresnF_selNoBare, mapping = aes(x = qsresnF_selNoBare$t2_min, y=qsresnF_selNoBare$WLR), method="loess", col="grey", show.legend = TRUE) +
   geom_point(data = qsresnF_selNoBare, mapping = aes(x = qsresnF_selNoBare$t2_min, y=qsresnF_selNoBare$WLR, colour = qsresnF_selNoBare$opat_typ_grain), method="loess") +
   facet_grid (poc_stav_int ~ opat_typ_grain, scales = "free_y") +
   #ylim (0, 1) +
   labs(title = "Runoff coeficient", x = "Time", y = "Runoff coeficient [-]")+
   theme_bw()
plot(xxplot_WLR)

scaleFactor_grain2 <- max(qsresnF_sel$Qcum_l) / 80

xxplot2 <- ggplot() +
  geom_line(data = qsresnF_sel, mapping = aes(x = t2_min, y = prutok1_l.min), col = "black") +
  geom_point(data = qsresnF_sel, mapping = aes(x = t2_min, y = prutok1_l.min), col = "black") +
  geom_line(data = qsresnF_sel, mapping = aes(x = t2_min, y = sedflux1_g.min * scaleFactorq), col = "grey", show.legend = TRUE) +
  geom_point(data = qsresnF_sel, mapping = aes(x = t2_min, y = sedflux1_g.min * scaleFactorq), col = "grey") +
  geom_point(data = summary_table_sel, mapping = aes(x = init_state_num, y = median_percentage * scaleFactor_grain2, color = as.character(particlesize), shape = as.character(aggregates))) +
  facet_grid(opat_typ_grain ~ poc_stav_int) + #, scales = "free_y") +
  scale_y_continuous(
    name = "q - black [l/min]", 
    sec.axis = sec_axis(~./scaleFactor_grain2, name = "flux - gray [g/min], PSD - colours [% Undersize]")
  ) +
  theme_bw() +
  labs(title = "Runoff, Sediment flux and Particle Size Distribution", x = "Time")

# Print the plot
print(xxplot2)

ratioplot =  ggplot() + 
  geom_point(data = sdata_wide_sel, aes(x = sdata_wide_sel$g_ID, y = ratio, colour =  as.character(sdata_wide_sel$particlesize_with)), shape = 1, size  = 4) + 
  geom_point(data = sdata_wide_sel, aes(x = sdata_wide_sel$g_ID,y = sdata_wide_sel$median_percentage_without, colour =  as.character(sdata_wide_sel$particlesize_with)), shape = 2, size  = 1) + 
  geom_jitter(data = sdata_wide_sel, aes(x = sdata_wide_sel$g_ID, y = sdata_wide_sel$median_percentage_with, colour =  as.character(sdata_wide_sel$particlesize_with)), shape = 0, size  = 1.5) + 
  theme_bw()+
  labs(title = "Particle Size Distribution", x = "data seqence", y = "[%]")+
  #ylim (0, 15) +
  facet_grid(opat_typ_grain ~ ., scales = "free_y")+
  theme(axis.text.x = element_text(angle = 80, hjust = 1))

plot(ratioplot)


yyplot  = ggplot() + #(qsresnF_sel$cas_do_odtok + (qsresnF_sel$t1_min + qsresnF_sel$t2_min)/2)))+
  geom_line (data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$prutok1_l.min), method="loess", col="black") +
  geom_point (data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$prutok1_l.min), method="loess", col="black") +
  geom_line(data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$smyv1_g.l * scaleFactorq), method="loess", col="grey", show.legend = TRUE) +
  geom_point(data = qsresnF_sel, mapping = aes(x = qsresnF_sel$t2_min, y=qsresnF_sel$smyv1_g.l * scaleFactorq), method="loess", col="grey") +
  geom_point(data = sdata_wide_sel, mapping = aes(x = sdata_wide_sel$init_state_num, y = sdata_wide_sel$ratio*scaleFactor_grain, colour = as.character(sdata_wide_sel$particlesize_with))) +
  
  #                   stat_summary(mapping = aes(x = grain$init_state_num , y = grain$percentage, colour = grain$aggregates, shape = as.factor(grain$particlesize)), fun = median, geom="point", show.legend = FALSE, vjust=-1, size = 10)+
  #scale_y_continuous(name="\u03A3 l", sec.axis=sec_axis(~./scaleFactor, name="\u03A3 g")) +
  #scale_y_continuous(name="\u03A3 l", sec.axis=sec_axis(name="\u03A3 g")) +
  facet_grid (opat_typ_grain ~ poc_stav_int) +
  ylim (0, 25) +
  theme_bw()




plot(yyplot)






### ?ten? dat z anal?z dmt
#sxdmt <- read.csv("DMT_strix_komplet_leden2020_2.csv", sep = ";", dec = ",")
#str(sxdmt)
#sxdmt <- sxdmt %>% mutate(# repetice = as.factor(repetice),
#                          opatreni = as.logical(opatreni),
#                          V = abs(V_objem_odnosu))

# ### Pokud u? jsou data na?ten? z p?edchoz?ho skriptu
# qsres <- QSdata_all
# qsstat <- QSstat_all


### P?ipojen? dat z dmt
#qsstat <- qsstat %>% left_join(y = select(sxdmt, ID:V), by = "ID") %>% 
#                      mutate(ID = as.factor(ID),
#                             NAvalues = as.factor(NAvalues))
                      #qsres <- mutate(qsres, V = 0)


#### APLIKACE PRO ZOBRAZOV?N? PR?B?H? ODTOKU A DAL??CH VELI?IN ####

# Define UI for application that plots the runoff variables from selected experimental settings
ui <- fluidPage(

  # Sidebar layout with a input and output definitions 
  sidebarLayout(
    
    # Inputs
    sidebarPanel(width = 3, style = "padding: 10px;",
      
      h4("Mapování proměnných v grafu"),
                 
      wellPanel(style = "padding: 5px;",
        # Select variable on X axis
        selectInput(inputId = "x", 
                  label = "Proměnné na ose x:",
                  choices = c("čas  t1" = "t1_min", "čas  t2" = "t2_min", "Interval" = "interval",
                              "Typ opatření" = "opat_typ", "Poč.stav (č.sim)" = "simcislo", "Sklon (?)" = "sklon_st", "Intenzita (mm/h)" = "intenzita", 
                              "Opatření ANO/NE" = "opatreni", "Systém" = "system", "Repetice" = "repetice",
                              "Pr?tok t1 (l/min)" = "prutok1_l.min", "Konc.sedimentu t1 (g/l)" = "smyv1_g.l", 
                              "L?tkov? tok t1 (g/min)" = "sedflux1_g.min", "Odteklé množství t2 (l)" = "Qcum_l", 
                              "Celkový smyv 1 t2 (g)" = "S1cum_g", "Celkový smyv 2 t2 (g)" = "S2cum_g",
                              "Pom?r odnosu 1 t2 (%)" = "S1cumRel", "Pom?r odnosu 2 t2 (%)" = "S2cumRel"), #, "Objem DMT" = "V"),
                  selected = "t1_min"),
                
        # Select variable on Y axis
        selectInput(inputId = "y", 
                  label = "Proměnné na ose y:",
                  choices = c("čas  t1" = "t1_min", "čas  t2" = "t2_min", "Interval" = "interval",
                              "Typ opatření" = "opat_typ", "Poč.stav (č.sim)" = "simcislo", "Sklon (?)" = "sklon_st", "Intenzita (mm/h)" = "intenzita", 
                              "Opatření ANO/NE" = "opatreni", "Systém" = "system", "Repetice" = "repetice",
                              "Průtok t1 (l/min)" = "prutok1_l.min", "Konc.sedimentu t1 (g/l)" = "smyv1_g.l", 
                              "Látkový tok t1 (g/min)" = "sedflux1_g.min", "Odtekl? mno?stv? t2 (l)" = "Qcum_l", 
                              "Celkový smyv 1 t2 (g)" = "S1cum_g", "Celkový smyv 2 t2 (g)" = "S2cum_g",
                              "Poměr odnosu 1 t2 (%)" = "S1cumRel", "Poměr odnosu 2 t2 (%)" = "S2cumRel",
                              "V_objem_odnosu", "V abs" = "V", "hmotnost_smyvu", "DEM1_mean", "DEM1_std", "DEM2_mean", "DEM2_std", "DEMd_mean", "DEMd_std", 
                              "slp1_mean", "slp2_mean", "slp1_std", "slp2_std", "slpd_mean", "slpd_std", 
                              "asp1_mean", "asp2_mean", "aspd_mean", "plc1_mean", "plc2_mean", "plcd_mean", "prc1_mean", "prc2_mean", "prcd_mean"),
                  selected = "prutok1_l.min"),
                
        # Select setting for color
        selectInput(inputId = "color", 
                  label = "Barva podle:",
                  choices = c("Typ opatření" = "opat_typ", "Sklon (°)" = "sklon_st", "Po?.stav (č.sim)" = "simcislo", "Intenzita (mm/h)" = "intenzita",  
                              "Opat?en? ANO/NE" = "opatreni", "Systém" = "system", "Repetice" = "repetice", "ID simulace" = "ID"),
                  selected = "opat_typ"),
        
        # Select setting for linetype
        selectInput(inputId = "ltype", 
                    label = "Typ čáry podle:",
                    choices = c("Typ opatření" = "opat_typ", "Sklon (°)" = "sklon_st", "Poč.stav (č.sim)" = "simcislo", "Intenzita (mm/h)" = "intenzita",  
                                "Opatření ANO/NE" = "opatreni", "Systém" = "system", "Repetice" = "repetice", "ID simulace" = "ID"),
                    selected = "opatreni"),
      
        # Select setting for rows
        selectInput(inputId = "row", 
                  label = "Rozlišit v řádcích:",
                  choices = c("Typ opatření" = "opat_typ", "Sklon (°)" = "sklon_st", "Poč.stav (č.sim)" = "simcislo", "Intenzita (mm/h)" = "intenzita",  
                              "Opatření ANO/NE" = "opatreni", "Systém" = "system", "Repetice" = "repetice", "ID simulace" = "ID", "."),
                  selected = "."),
      
        # Select setting for columns
        selectInput(inputId = "col", 
                  label = "Rozlišit?it ve sloupcích:",
                  choices = c("Typ opatření" = "opat_typ", "Sklon (°)" = "sklon_st", "Poč.stav (č.sim)" = "simcislo", "Intenzita (mm/h)" = "intenzita",  
                              "Opatření ANO/NE" = "opatreni", "Systém" = "system", "Repetice" = "repetice", "ID simulace" = "ID", "."),
                  selected = "sklon_st"),
        
        # Specify scales types in facets
        checkboxGroupInput(inputId = "scales",
                           label = "Rozvolni rozsah os:",
                           choices = c("x" = 1, "y" = 2),
                           selected = 2,
                           inline = T),
      
        # Highlight missing datapoints
        checkboxInput(inputId = "mis",
                    label = "Zvýrazni chybějící datové body:",
                    value = T)
      ),
      
      # h3 header for filtering
      h4("Filtr datasetu"),

      wellPanel(style = "padding: 5px;",
        # Select levels of opat_typ
        checkboxGroupInput(inputId = "lev_opat_typ",
                    label = "Typ opatření:",
                    choices = levels(qsres$opat_typ),
                    selected = levels(qsres$opat_typ),
                    inline = T),
      
        # Select levels of sklon_st
        checkboxGroupInput(inputId = "lev_sklon",
                         label = "Sklon plochy:",
                         choices = levels(qsres$sklon_st),
                         selected = levels(qsres$sklon_st),
                         inline = T),

        # Select initial conditions
        checkboxGroupInput(inputId = "lev_simcislo",
                         label = "Počáteční stav:",
                         choices = c("s" = 1, "m" = 2, "s+rýhy" = 3, "m+rýhy" = 4),
                         selected = levels(qsres$simcislo),
                         inline = T),
        
        # Select rainfall intensity
        checkboxGroupInput(inputId = "lev_intenzita",
                           label = "Intenzita deště:",
                           choices = levels(qsres$intenzita),
                           selected = levels(qsres$intenzita),
                           inline = T),
        
        # Select repetition numbers
        checkboxGroupInput(inputId = "lev_rep",
                           label = "Repetice:",
                           choices = levels(qsres$repetice),
                           selected = levels(qsres$repetice),
                           inline = T),

        # Select plots with/out measures
        checkboxGroupInput(inputId = "lev_opatreni",
                         label = "Plochy s/bez opat?en?:",
                         choices = c(T, F),
                         selected = c(T, F),
                         inline = T),
        
        # Select location of measurements
        checkboxGroupInput(inputId = "lev_loc",
                           label = "Lokalita:",
                           choices = levels(qsres$lokalita),
                           selected = levels(qsres$lokalita),
                           inline = T)
        ),
      
      # Action button for applying the filter
      actionButton(inputId = "filter_dataset", 
                   label = "Filtruj a zobraz dataset")
    ),
    

    mainPanel(width = 9,
      # Outputs
      mainPanel(
        tabsetPanel(type = "tabs",
                    # Tab 1: Line plot of the time series
                    tabPanel("Průběhy veličin",
                        br(),  # Visual separation
      
                        plotOutput(outputId = "timeseries", 
                                   hover = "plot1_hover",
                                   height = "800px"),
                        
                        verbatimTextOutput(outputId = "bpoint_info1")
                    ),
                    
                    # Tab 2: Scatterplot of the cumulative statistics
                    tabPanel("Bodov? statistiky",
                        br(),  # Visual separation
                             
                        plotOutput(outputId = "scatterplot",
                                   hover = "plot2_hover",
                                   height = "800px"),

                        verbatimTextOutput(outputId = "bpoint_info2")
                    ),
                    
                    # Tab 3: Summary statistics of preselected variables and settings
                    tabPanel("Summary",
                        br(),
                        
                        verbatimTextOutput(outputId = "summary") # summary of filtered dataset
                    )
                    
        ), style = "width: 100%"
      )
    )
    
  )
  
)


# Define server function required to create the scatterplot
server <- function(input, output) {
  # Turn off the inline histogramms in skimr summary
  skim_with(numeric = list(hist = NULL))
  
  # Create reactive dataframes - filtered subsets for generating the plots
  fres <- eventReactive(
    eventExpr = input$filter_dataset,
    valueExpr = {
      req(input$lev_opat_typ)
      qsres %>% filter(opat_typ %in% input$lev_opat_typ,
                      sklon_st %in% input$lev_sklon,
                      simcislo %in% input$lev_simcislo,
                      intenzita %in% input$lev_intenzita,
                      repetice %in% input$lev_rep,
                      opatreni %in% input$lev_opatreni,
                      lokalita %in% input$lev_loc)})
  
  fstat <- eventReactive(
    eventExpr = input$filter_dataset,
    valueExpr = {
      req(input$lev_opat_typ)
      qsstat %>% filter(opat_typ %in% input$lev_opat_typ,
                       sklon_st %in% input$lev_sklon,
                       simcislo %in% input$lev_simcislo,
                       intenzita %in% input$lev_intenzita,
                       repetice %in% input$lev_rep,
                       opatreni %in% input$lev_opatreni,
                       lokalita %in% input$lev_loc)})
  
  # Create reactive filtered subset of predefined columns for generating the summary
  ffres <- reactive(fres() %>%
                      ungroup() %>%
                      select(opat_typ, opatreni, sklon_st, simcislo, intenzita, repetice, NAvalue,
                             prutok1_l.min, smyv1_g.l, sedflux1_g.min, Qcum_l, S1cum_g, S2cum_g, S1cumRel, S2cumRel))
  
  # ffstat <- reactive(fstat() %>% 
  #                     ungroup() %>% 
  #                     select(interval, opat_typ, plocha, simcislo, opatreni, prutok_l.min, smyv_g.l, Qcum_l, Scum_g, sedflux_g.min, ScumRel))
  
  myplot1 <- eventReactive(
    eventExpr = fres(),
    valueExpr = {
      ggplot(data = fres(), aes_string(x = input$x, y = input$y, group = "ID", col = input$color, linetype = input$ltype)) +
          geom_line() +
          geom_point() +
          facet_grid(paste(input$row, "~", input$col, sep = " "),
                     scales = c("fixed", "free_x", "free_y", "free")[which(sum(as.numeric(input$scales)) == c(0,1,2,3))]) +
          theme(legend.position = "bottom")
          
      })
  
  myplot2 <- eventReactive(
    eventExpr = fstat(),
    valueExpr = {
      ggplot(data = fstat(), aes_string(x = input$x, y = input$y, col = input$color, shape = input$ltype)) +
        geom_point(size = 3) +
        facet_grid(paste(input$row, "~", input$col, sep = " "),
                   scales = c("fixed", "free_x", "free_y", "free")[which(sum(as.numeric(input$scales)) == c(0,1,2,3))]) +
        theme(legend.position = "bottom")
  })
  
  output$timeseries <- renderPlot({
    if (input$mis == T) {
      myplot1() + geom_point(aes(shape = NAvalue), size = 3, col = "red") + guides(shape = F)
    } else{
    myplot1()
    }
    })
  
  output$bpoint_info1 <- renderPrint({
    nearPoints(fres(), coordinfo = input$plot1_hover) %>% 
      select(ID) %>% ungroup() %>% as.data.frame()
  })
  
  output$scatterplot <- renderPlot({myplot2()})

  output$bpoint_info2 <- renderPrint({
    nearPoints(fstat(), coordinfo = input$plot2_hover) %>%
      select(ID) %>% ungroup() %>% as.data.frame()
  })

  output$summary <- renderPrint({
    print(skim(ungroup(ffres())))
  })

}


# Create a Shiny app object
shinyApp(ui = ui, server = server)





xxplot  = ggplot(qsresnF, aes(x = qsresnF$t1_min, y=qsresnF$Qcum_l), colour = "red") + #(qsresnF$cas_do_odtok + (qsresnF$t1_min + qsresnF$t2_min)/2)))+
                   geom_line() +
#                   geom_jitter(aes(x = qsresnF$t1_min, y=qsresnF$S2cum_g * scaleFactor), method="loess", col="red") +
#                   stat_summary(aes(x = grain$init_state_num , y = grain$percentage, colour = grain$aggregates, shape = as.factor(grain$particlesize)), fun = median, geom="point", show.legend = FALSE, vjust=-1, size = 10)+
#                   scale_y_continuous(name="l/min", sec.axis=sec_axis(~./scaleFactor, name="g/l")) +
                   facet_grid (qsresnF$opat_typ_grain ~ qsresnF$poc_stav_int)


plot(xxplot)
grain_data_long <- grain %>%
  pivot_longer(cols = particlesize),
               names_to = "particlesize",
               values_to = "value")


gplot = ggplot() + #grain, aes(x = grain$init_state_num , y = grain$percentage, colour = grain$aggregates, shape = as.factor(particlesize))) +
  stat_summary(aes(x = grain$init_state_num , y = grain$percentage, colour = grain$aggregates, shape = as.factor(grain$particlesize)), fun = median, geom="point", show.legend = TRUE, vjust=-1, size = 10)
 
plot(gplot)

scale_x_log10(name = "Undersize of particles [\u03BCm]", breaks = c(2.13e+00, 9.86e+00, 5.18e+01), labels = c("2", "10", "50"))#, fill = Vodni_tok)) + geom_bar() + facet_grid(size ~ ., scales = "free"))





max_out = maxrun[,c(5, 6, 11, 15, 29, 35, 37)]
write.csv(max_out, "max_runoff.csv")
eDTA1 <- eDTA %>% mutate(opat_typ = ifelse(poc_stav %in% c("such\xe1" ), "bare_dry", "bare_wet"))

eDTA = eDTA[c(3, 5, )]

x <- subset(qsres, "opatreni" = "FALSE")

vyber_R <- select(qsres, lokalita, simcislo, repetice, sklon_st, intenzita, ID, t1_min, t2_min, prutok1_l.min, Qcum_l)


 
x = qsres [qsres$opatreni == "FALSE",]
xx = x [x$simcislo == "1",]
xxx =  xx [xx$lokalita == "Jir",]


vyber_R <- select(xxx, lokalita, simcislo, repetice, sklon_st, intenzita, ID, t1_min, t2_min, prutok1_l.min, Qcum_l)

write.csv(vyber_R, "vyber_R.csv")

mean(vyber_R$sklon_st == 30 & vyber_R$t2_min == "2.5", vyber_R$prutok1_l.min)
  







