library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)
library(tidyverse)
library(scales)
library(plotly)



#dir
#setwd("S:/Private/_PROJEKTY/2017_Strix_svahy/1_reseni_projektu/00_vyhodnoceni/leden2021finale/ruzne_zasypani/zrnitost/")
setwd("d:/5_papers/2023_zasypane_geotextilie/_dta_work/")


#grainsize_distribution
#grain <- read.csv("S:/Private/_PROJEKTY/2017_Strix_svahy/1_reseni_projektu/00_vyhodnoceni/leden2021finale/ruzne_zasypani/laser_data/143Strix_labDS_vse2.csv", sep = ";", stringsAsFactors = F, header = T)
grain = read.csv("zasypani_zrnitosti.csv", sep = ";", header = T, check.names = F)

varnames <- names(grain)


vars1 <- c("record_number", "sample_name", "misto", "date", "sim", "slope", "intensity", "init_state", "variant",  "measurement_time", "Dx_10", "Dx_50", "Dx_90", "Dx_35")
vars2 <- varnames[15:115] %>% str_replace(pattern = "X", replacement = "") %>% as.numeric() %>% signif(digits = 3) %>% as.character()
names(grain) <- c(vars1, vars2)

#water and sediment


#genera_pairing data





grain_stat <- grain %>% group_by(sample_name) %>% arrange(measurement_time) %>% summarize(nmin = min(record_number))



grain2 <- grain %>% 
  left_join(y = grain_stat) %>%                                # join minimum RecNo for each sample
  mutate(sID = record_number - nmin + 1) %>%                      # calculate order (sID) of each row with respect to the starting RecNo
  select(-nmin) %>%                                               # discard the minimum RecNo statistic
  filter(sID < 6 | sID > 20) %>%                                  # select only first 5 and last 5 measurements for each sample
  mutate(aggregates = ifelse(sID < 6, "with", "without"),                 # mark first 5 measurements as "start" and last 5 as "end"
         sample_name = as.factor(sample_name))



grain3 <- grain2 %>%                        
  gather(key = "particlesize", value = "percentage", vars2) %>%   # gather all the fractions into single field "percentage" distincted in the "particlesize" field
  mutate(particlesize = as.numeric(particlesize)) %>%
  group_by(record_number, sample_name, misto, date, sim, slope, intensity, init_state, variant,  aggregates, measurement_time, Dx_10, Dx_50, Dx_90, Dx_35, particlesize) %>%                       # group the dataframe according to three fields and inside these classes calculate the means of fraction percentages
  summarize(percentage = mean(percentage),
            D10 = mean(Dx_10),
            D35 = mean(Dx_35),
            D50 = mean(Dx_50),
            D90 = mean(Dx_90),)

grain_box <- grain3 %>% filter(particlesize == 9.86e+00 | particlesize == 2.13e+00 | particlesize == 5.18e+01)
grain_box_new <- grain_box
grain_box_new$init_state <- factor(grain_box_new$init_state,      # Reordering group factor levels
                                 levels = c("D1", "D2", "D3", "W1", "W0", "W2", "W3"))
grain_box_new$variant <- factor(grain_box_new$variant,
                                levels = c("bare_soil", "bare_RECP", "filled_RECP", "full_backfill", "1m_backfill", "3m_backfill"))
# Add a new column with corresponding numeric values
grain_box_new <- grain_box_new %>%
  mutate(init_state_num = recode(init_state,
                                 "D1" = 5,
                                 "D2" = 15,
                                 "D3" = 30,
                                 "W1" = 5,
                                 "W0" = 0,
                                 "W2" = 15,
                                 "W3" = 30))
# Add a new column with corresponding names as runoff
grain_box_new <- grain_box_new %>%
  mutate(variant_runoff = recode(variant,
                                 "bare_soil" = "BSOIL",
                                 "bare_RECP" = "BRECP",
                                 "filled_RECP" = "FRECP",
                                 "full_backfill" = "4MFIL",
                                 "1m_backfill" = "1MFIL",
                                 "3m_backfill" = "3MFIL"))
write.csv2(grain_box_new, "grain_size_export.csv")

##GRAFY

#Initial state X Intensity 
plot1 = ggplot(data = grain_box_new %>% filter(aggregates == "with"),
       aes(x = particlesize, y = percentage, group = particlesize, col = aggregates)) +
  stat_summary(fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "aquamarine3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = grain_box_new %>% filter(aggregates == "with"),
               fun = median, geom="point", col = "aquamarine3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  stat_summary(data = grain_box_new %>% filter(aggregates == "without"),
               fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "deeppink3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = grain_box_new %>% filter(aggregates == "without"),
               fun = median, geom="point", col = "deeppink3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  #scale_color_manual(values = c("aquamarine3", "deeppink3")) +
  facet_grid(init_state ~ intensity) +
  scale_x_log10(name = "Undersize of particles [\u03BCm]", breaks = c(2.13e+00, 9.86e+00, 5.18e+01), labels = c("2", "10", "50")) +
  scale_y_continuous(name = "Cumulative volume [%]") +
  labs(title = "Initial state X Intensity", color = "Aggregates") + 
  theme_bw()

plot(plot1)

ggsave("all_init_st_intensity.png", plot1, 
       device = NULL,
       path = NULL,
       scale = 1,
       width = 100,
       height = 100,
       units =  "mm",
       dpi = 300,
       limitsize = TRUE)


#bare_soil
bare_soil <- grain_box_new %>% filter(variant == "bare_soil")

plot2 = ggplot(data = bare_soil %>% filter(aggregates == "with"),
       aes(x = particlesize, y = percentage, group = particlesize, col = init_state)) +
  stat_summary(fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "aquamarine3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = bare_soil %>% filter(aggregates == "with"),
               fun = median, geom="point", col = "aquamarine3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  stat_summary(data = bare_soil %>% filter(aggregates == "without"),
               fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "deeppink3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = bare_soil %>% filter(aggregates == "without"),
               fun = median, geom="point", col = "deeppink3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  #scale_color_manual(values = c("aquamarine3", "deeppink3")) +
  facet_grid(init_state ~ intensity) +
  scale_x_log10(name = "Particle size [?m]", breaks = c(2.13e+00, 9.86e+00, 5.18e+01), labels = c("2", "10", "50")) +
  scale_y_continuous(name = "Cumulative volume [%]") +
  labs(title = "Bare soil", color = "Initial state") + 
  theme_bw()

plot(plot2)

ggsave("bare_soil.png", plot2, 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "mm",
       dpi = 300,
       limitsize = TRUE,)

#bare_RECP
bare_RECP <- grain_box_new %>% filter(variant == "bare_RECP")

plot3 = ggplot(data = bare_RECP %>% filter(aggregates == "with"),
       aes(x = particlesize, y = percentage, group = particlesize, col = init_state)) +
  stat_summary(fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "aquamarine3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = bare_RECP %>% filter(aggregates == "with"),
               fun = median, geom="point", col = "aquamarine3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  stat_summary(data = bare_RECP %>% filter(aggregates == "without"),
               fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "deeppink3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = bare_RECP %>% filter(aggregates == "without"),
               fun = median, geom="point", col = "deeppink3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  #scale_color_manual(values = c("aquamarine3", "deeppink3")) +
  facet_grid(init_state ~ intensity) +
  scale_x_log10(name = "Particle size [?m]", breaks = c(2.13e+00, 9.86e+00, 5.18e+01), labels = c("2", "10", "50")) +
  scale_y_continuous(name = "Cumulative volume [%]") +
  labs(title = "Bare RECP", color = "Initial state") + 
  theme_bw()

plot(plot3)

ggsave("bare_RECP.png", plot3, 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "mm",
       dpi = 300,
       limitsize = TRUE,)

#Filled_RECP
filled_RECP <- grain_box_new %>% filter(variant == "filled_RECP")

plot4 = ggplot(data = filled_RECP %>% filter(aggregates == "with"),
       aes(x = particlesize, y = percentage, group = particlesize, col = init_state)) +
  stat_summary(fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "aquamarine3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = filled_RECP %>% filter(aggregates == "with"),
               fun = median, geom="point", col = "aquamarine3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  stat_summary(data = filled_RECP %>% filter(aggregates == "without"),
               fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "deeppink3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = filled_RECP %>% filter(aggregates == "without"),
               fun = median, geom="point", col = "deeppink3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  #scale_color_manual(values = c("aquamarine3", "deeppink3")) +
  facet_grid(init_state ~ intensity) +
  scale_x_log10(name = "Particle size [?m]", breaks = c(2.13e+00, 9.86e+00, 5.18e+01), labels = c("2", "10", "50")) +
  scale_y_continuous(name = "Cumulative volume [%]") +
  labs(title = "Filled RECP", color = "Initial state") + 
  theme_bw()

plot(plot4)

ggsave("filled_RECP.png", plot4, 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "mm",
       dpi = 300,
       limitsize = TRUE,)

#Full_backfill
full_backfill <- grain_box_new %>% filter(variant == "full_backfill")

plot5 = ggplot(data = full_backfill %>% filter(aggregates == "with"),
       aes(x = particlesize, y = percentage, group = particlesize, col = init_state)) +
  stat_summary(fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "aquamarine3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = full_backfill %>% filter(aggregates == "with"),
               fun = median, geom="point", col = "aquamarine3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  stat_summary(data = full_backfill %>% filter(aggregates == "without"),
               fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "deeppink3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = full_backfill %>% filter(aggregates == "without"),
               fun = median, geom="point", col = "deeppink3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  #scale_color_manual(values = c("aquamarine3", "deeppink3")) +
  facet_grid(init_state ~ intensity) +
  scale_x_log10(name = "Particle size [?m]", breaks = c(2.13e+00, 9.86e+00, 5.18e+01), labels = c("2", "10", "50")) +
  scale_y_continuous(name = "Cumulative volume [%]") +
  labs(title = "Full backfill", color = "Initial state") + 
  theme_bw()

plot(plot5)



ggsave("full_backfill.png", plot5, 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "mm",
       dpi = 300,
       limitsize = TRUE,)

#1m_backfill
one_m_backfill <- grain_box_new %>% filter(variant == "1m_backfill")

plot6 = ggplot(data = one_m_backfill %>% filter(aggregates == "with"),
       aes(x = particlesize, y = percentage, group = particlesize, col = init_state)) +
  stat_summary(fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "aquamarine3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = one_m_backfill %>% filter(aggregates == "with"),
               fun = median, geom="point", col = "aquamarine3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  stat_summary(data = one_m_backfill %>% filter(aggregates == "without"),
               fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "deeppink3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = one_m_backfill %>% filter(aggregates == "without"),
               fun = median, geom="point", col = "deeppink3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  #scale_color_manual(values = c("aquamarine3", "deeppink3")) +
  facet_grid(init_state ~ intensity) +
  scale_x_log10(name = "Particle size [?m]", breaks = c(2.13e+00, 9.86e+00, 5.18e+01), labels = c("2", "10", "50")) +
  scale_y_continuous(name = "Cumulative volume [%]") +
  labs(title = "1m backfill", color = "Initial state") + 
  theme_bw()

plot(plot6)

ggsave("1m_backfill.png", plot6, 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "mm",
       dpi = 300,
       limitsize = TRUE,)

#3m_backfill
three_m_backfill <- grain_box_new %>% filter(variant == "3m_backfill")

plot7 = ggplot(data = three_m_backfill %>% filter(aggregates == "with"),
       aes(x = particlesize, y = percentage, group = particlesize, col = init_state)) +
  stat_summary(fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "aquamarine3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = three_m_backfill %>% filter(aggregates == "with"),
               fun = median, geom="point", col = "aquamarine3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  stat_summary(data = three_m_backfill %>% filter(aggregates == "without"),
               fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "deeppink3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = three_m_backfill %>% filter(aggregates == "without"),
               fun = median, geom="point", col = "deeppink3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  #scale_color_manual(values = c("aquamarine3", "deeppink3")) +
  facet_grid(init_state ~ intensity) +
  scale_x_log10(name = "Particle size [?m]", breaks = c(2.13e+00, 9.86e+00, 5.18e+01), labels = c("2", "10", "50")) +
  scale_y_continuous(name = "Cumulative volume [%]") +
  labs(title = "3m backfill", color = "Initial state") + 
  theme_bw()

plot(plot7)

ggsave("3m_backfill.png", plot7, 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "mm",
       dpi = 300,
       limitsize = TRUE,)


#60_mm/h
int60 <- grain_box_new %>% filter(intensity == "60")

plot8 = ggplot(data = int60 %>% filter(aggregates == "with"),
       aes(x = particlesize, y = percentage, group = particlesize, col = init_state)) +
  stat_summary(fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "aquamarine3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = int60 %>% filter(aggregates == "with"),
               fun = median, geom="point", col = "aquamarine3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  stat_summary(data = int60 %>% filter(aggregates == "without"),
               fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "deeppink3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = int60 %>% filter(aggregates == "without"),
               fun = median, geom="point", col = "deeppink3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  #scale_color_manual(values = c("aquamarine3", "deeppink3")) +
  facet_grid(init_state ~ variant_runoff) +
  scale_x_log10(name = "Particle size [?m]", breaks = c(2.13e+00, 9.86e+00, 5.18e+01), labels = c("2", "10", "50")) +
  scale_y_continuous(name = "Cumulative volume [%]") +
  labs(title = "Intensity 60 mm/h", color = "Initial state") + 
  theme_bw()

plot(plot8)

ggsave("60_mmh.png", plot8, 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "mm",
       dpi = 300,
       limitsize = TRUE,)

#120_mm/h
int120 <- grain_box_new %>% filter(intensity == "120")

plot9 = ggplot(data = int120 %>% filter(aggregates == "with"),
       aes(x = particlesize, y = percentage, group = particlesize, col = init_state)) +
  stat_summary(fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "aquamarine3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = int120 %>% filter(aggregates == "with"),
               fun = median, geom="point", col = "aquamarine3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  stat_summary(data = int120 %>% filter(aggregates == "without"),
               fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 2, col = "deeppink3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = int120 %>% filter(aggregates == "without"),
               fun = median, geom="point", col = "deeppink3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  #scale_color_manual(values = c("aquamarine3", "deeppink3")) +
  facet_grid(init_state ~ variant) +
  scale_x_log10(name = "Particle size [?m]", breaks = c(2.13e+00, 9.86e+00, 5.18e+01), labels = c("2", "10", "50")) +
  scale_y_continuous(name = "Cumulative volume [%]") +
  labs(title = "Intensity 120 mm/h", color = "Initial state") + 
  theme_bw()

plot(plot9)

ggsave("120_mmh.png", plot9, 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "mm",
       dpi = 300,
       limitsize = TRUE,)

#160_mm/h
int160 <- grain_box_new %>% filter(intensity == "160")

plot10 = ggplot(data = int160 %>% filter(aggregates == "with"),
       aes(x = particlesize, y = percentage, group = particlesize)) +
  stat_summary(fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 1.5, col = "aquamarine3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = int160 %>% filter(aggregates == "with"),
               fun = median, geom="point", col = "aquamarine3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  stat_summary(data = int160 %>% filter(aggregates == "without"),
               fun = median, geom="text", show.legend = TRUE, 
               vjust=-1, size = 1.5, col = "deeppink3", aes( label=round(..y.., digits=1))) +
  stat_summary(data = int160 %>% filter(aggregates == "without"),
               fun = median, geom="point", col = "deeppink3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  #scale_color_manual(values = c("aquamarine3", "deeppink3", "slategrey")) +
  facet_grid(init_state ~ variant) +
  scale_x_log10(name = "Particle size [?m]", breaks = c(2.13e+00, 9.86e+00, 5.18e+01), labels = c("2", "10", "50")) +
  scale_y_continuous(name = "Cumulative volume [%]") +
  labs(title = "Intensity 160 mm/h", color = "Initial state") + 
  theme_bw()

plot(plot10)

ggsave("160_mmh.png", plot10, 
       device = NULL,
       path = NULL,
       scale = 1,
       width = NA,
       height = NA,
       units = "mm",
       dpi = 300,
       limitsize = TRUE,)



##test

bare_RECP_with_60_D1 <- bare_RECP %>% filter(aggregates == "with", intensity == "60", init_state == "D1")

ggplot(data = bare_RECP_with_60_D1,
       aes(x = particlesize, y = percentage, group = particlesize, col = init_state)) +
  stat_summary(fun = median, colour="red", geom="text", show.legend = FALSE, 
               vjust=-0.7, size = 2.5, aes( label=round(..y.., digits=1)))+
  #stat_summary(data = int160 %>% filter(aggregates == "without"),
   #            fun = median, geom="point", shape = 10, show.legend = TRUE, 
    #           size = 3.5) +
  #scale_color_manual(values = c("aquamarine3", "deeppink3")) +
  facet_grid(init_state ~ variant) +
  scale_x_log10(name = "Particle size [?m]", breaks = c(2.13e+00, 9.86e+00, 5.18e+01), labels = c("2", "10", "50")) +
  scale_y_continuous(name = "Cumulative volume [%]") +
  labs(title = "Intensity 160 mm/h", color = "Initial state") + 
  theme_bw()

ggplot(data = bare_RECP_with_60_D1,
       aes(x = particlesize, y = percentage, group = particlesize, col = init_state)) +
geom_point() +
  stat_summary(fun = median, colour="red", geom="text", show.legend = FALSE, 
               vjust=-0.7, size = 2.5, aes( label=round(..y.., digits=1))) +
  facet_grid(init_state ~ variant) +
  scale_x_log10(name = "Particle size [?m]", breaks = c(2.13e+00, 9.86e+00, 5.18e+01), labels = c("2", "10", "50")) +
  scale_y_continuous(name = "Cumulative volume [%]") +
  labs(title = "Intensity 160 mm/h", color = "Initial state") + 
  theme_bw()


#XXX
plotxx = ggplot(data = grain_box_new %>% filter(aggregates == "with"),
               aes(x = particlesize, y = percentage, group = particlesize, col = aggregates)) +
  stat_summary(data = grain_box_new %>% filter(aggregates == "with"),
               fun = median, geom="point", col = "aquamarine3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  stat_summary(data = grain_box_new %>% filter(aggregates == "without"),
               fun = median, geom="point", col = "deeppink3", shape = 19, show.legend = TRUE, 
               size = 2.5) +
  #scale_color_manual(values = c("aquamarine3", "deeppink3")) +
  facet_grid(init_state ~ intensity) +
  scale_x_log10(name = "Undersize of particles [\u03BCm]", breaks = c(2.13e+00, 9.86e+00, 5.18e+01), labels = c("2", "10", "50")) +
  scale_y_continuous(name = "Cumulative volume [%]") +
  labs(title = "Initial state X Intensity", color = "Aggregates") + 
  theme_bw()

plot(plotxx)

