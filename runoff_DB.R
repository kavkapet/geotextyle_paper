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

setwd("d:/2_granty_projekty/2_Bezici/0_DS/datbaze_data/")
srcDTA =  read.csv(file = "runoff_sediment_intervals_20240925_en.csv",sep = ";",fileEncoding = "UTF-8")
srcDTA <- subset(srcDTA, plot.ID != 170)
srcDTA$TIMESTAMP = as.POSIXct(strptime(srcDTA$date, "%d.%m.%Y"))
srcDTA$t1_t_form <- as.POSIXct(srcDTA$t1, format = "%H:%M:%S")
srcDTA$t2_t_form <- as.POSIXct(srcDTA$t2, format = "%H:%M:%S")
srcDTA$interval_t_form <- as.POSIXct(srcDTA$interval.duration, format = "%H:%M:%S")

srcDTA$month <- format(srcDTA$TIMESTAMP, "%m")
srcDTA$cover  <- case_when(
  srcDTA$crop %in% c("cultivated fallow", "bare soil") ~ "bare",
  srcDTA$crop %in% c("Geotextile Macmat 8.1", "Geotextile Enkamat 7010", "Geotextile K700", "Geotextile Biomac-c", "Geotextile Enkamat 7020", "Geotextile Macmat 18.1", "Macmat 18 fill", "Jute", "Triangle", "Enkamat 7020 filled", "Fortrac 3D filled", "Fortrac 3D") ~ "geotex",
  TRUE ~ "vege")


# Assign "bare_soil" if crop is NA
srcDTA_clean <- srcDTA %>%
  filter(!is.na(crop) & !is.na(SS.flux..g.min.1. & total.discharge..l. > 2000 & locality == c("Jirkov (STRIX)")))


# Create a boxplot using ggplot2
xplot <- ggplot(srcDTA_clean, aes(x = t1_t_form, y = rainfall.total..mm., color = cover)) +
  geom_smooth() +
  labs(title = "Total rainfall",
       x = "Time",
       y = "Total rainfall (mm)",
       fill = "X") +
  facet_grid(initial.cond. ~ .) + 
  theme_minimal() + 
  xlim(as.POSIXct("00:00:00", format = "%H:%M:%S"),
       as.POSIXct("00:35:00", format = "%H:%M:%S"))  # Set limits for one hour


# Display the plot
print(xplot)
# Create a boxplot using ggplot2
xplot <- ggplot(srcDTA_clean, aes(x = t1_t_form, y = total.discharge..l., color = cover)) +
  geom_smooth() +
  labs(title = "Total Discharge",
       x = "Time",
       y = "Total Discharge (l)",
       fill = "X") +
  facet_grid(initial.cond. ~ .) + 
  theme_minimal() + 
  xlim(as.POSIXct("00:00:00", format = "%H:%M:%S"),
       as.POSIXct("00:35:00", format = "%H:%M:%S"))  # Set limits for one hour

# Display the plot
print(xplot)
# Create a boxplot using ggplot2
xplot <- ggplot(srcDTA_clean, aes(x = t1_t_form, y = sediment.yield.g., color = cover)) +
  geom_smooth() +
  labs(title = "Total Sediment",
       x = "Time",
       y = "Sediment yield (g)",
       fill = "X") +
  facet_grid(initial.cond. ~ .) + 
  theme_minimal() + 
  xlim(as.POSIXct("00:00:00", format = "%H:%M:%S"),
       as.POSIXct("00:35:00", format = "%H:%M:%S"))  # Set limits for one hour

# Display the plot
print(xplot)

  
# Create a boxplot using ggplot2
xplot <- ggplot(srcDTA_clean, aes(x = t2_t_form, y = flow.rate..l.min.1., color = cover)) +
  geom_smooth() +
  labs(title = "Flow rate",
       x = "Time",
       y = "flow.rate..l.min.1.",
       fill = "X") +
  facet_grid(initial.cond. ~ .) + 
  theme_minimal() + 
  xlim(as.POSIXct("00:00:00", format = "%H:%M:%S"),
       as.POSIXct("00:35:00", format = "%H:%M:%S"))  # Set limits for one hour

# Display the plot
print(xplot)
# Create a boxplot using ggplot2
xplot <- ggplot(srcDTA_clean, aes(x = t2_t_form, y = SS.flux..g.min.1., color = cover)) +
  geom_smooth() +
  labs(title = "Sflux",
       x = "Time",
       y = "SS.flux..g.min.1.",
       fill = "X") +
  facet_grid(initial.cond. ~ .) + 
  theme_minimal() + 
  xlim(as.POSIXct("00:00:00", format = "%H:%M:%S"),
       as.POSIXct("00:35:00", format = "%H:%M:%S"))  # Set limits for one hour

# Display the plot
print(xplot)
# Create a boxplot using ggplot2
xplot <- ggplot(srcDTA_clean, aes(x = t1_t_form, y = SS.concentration..g.l.1., color = cover)) +
  geom_smooth() +
  labs(title = "Sediment_concentration",
       x = "Time",
       y = "SS.concentration..g.l.1.",
       fill = "X") +
  facet_grid(initial.cond. ~ .) + 
  theme_minimal() + 
  xlim(as.POSIXct("00:00:00", format = "%H:%M:%S"),
       as.POSIXct("00:35:00", format = "%H:%M:%S"))  # Set limits for one hour

# Display the plot
print(xplot)



