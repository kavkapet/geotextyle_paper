# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(plotly)
library(tidyverse)
library(scales)
library(plotly)
setwd("d:/5_papers/2024_grain_size_methodology/")

# Load the data
grain <- read.csv("all_undersize2.csv", sep = ";", header = T, dec = ".", fileEncoding = "ISO-8859-1")
varnames <- names(grain)


vars1 <- c("record_number", "sample_name", "Measurement.Date.Time", "D_10", "D_50", "D_90", "D_35")
vars2 <- varnames[8:107] %>% str_replace(pattern = "X", replacement = "")
names(grain) <- c(vars1, vars2, "Concentration", "Concentration", "Invalid.Snaps.Reported")
names(grain) <- make.unique(names(grain))
grain$sample_name = substr(grain$sample_name, 15, nchar(grain$sample_name))
grain$sample_name1 = grain$sample_name
#grain$size_in = substr(grain$sample_name, 1, 3)

# Assuming your dataframe is named 'grain'
# Split the sample_name column into separate columns based on the underscore separator
grain <- grain %>%
  separate(sample_name1, into = c("size_in", "method", "repetetion", "extra"), sep = "_", fill = "right")

sel_measuredta = read.csv("sel_measure.csv", sep = ";", header = F, dec = ".", fileEncoding = "ISO-8859-1")
sel_measuredta = t(sel_measuredta)
sel_measuredta = as.data.frame(sel_measuredta)

colnames(sel_measuredta) = c("record_number", "dispergant", "measuredistance")
sel_measuredta$record_number = as.numeric(sel_measuredta$record_number)

grain1 <- grain %>% filter(record_number %in% sel_measuredta$record_number)


grain2 <- left_join(grain1, sel_measuredta, by = "record_number")
grain2 <- grain2 %>%
  mutate(clear_method = ifelse(method == "NaOHkonc", repetetion, substr(method, nchar(method), nchar(method))))
grain2 <- grain2 %>%
  mutate(clear_method = replace_na(clear_method, "1"))
grain2$repetetion = grain2$clear_method
grain2 <- grain2 %>%
  mutate(method = gsub("SOP[1-4]", "SOP", method))
#sel_measure = c("5", "19", "35", "40", "56", "71", "76", "91", "106", "111", "126", "141", "144", "189", "204", "209", "224", "239", "244", "249", "254", "259", "264", "269", "274", "279", "284", "289", "294", "299")
grain3 = grain2 %>% pivot_longer(cols = 8:107, names_to = "particlesize", values_to = "percentage")
#grain <- grain %>%
 # mutate(dispergant = recode(dispergant,
  #                               "0" = "water",
   #                              "1" = "water",
    #                             "2" = "ultrasonic",
     #                            "3" = "dispergant",
      #                           "solution" = "dispergant",
       #                          .missing = "water"
        #                         ))


# Display the first few rows of the dataframe to verify
head(grain)

mean_values <- grain2 %>%
  group_by(method, size_in, dispergant) %>%
  summarise(across(4:107, mean, na.rm = TRUE))

mean_values3 = mean_values %>% pivot_longer(cols = 8:107, names_to = "particlesize", values_to = "percentage")

mean_values4 <- mean_values3 %>%
  unite("code", method, size_in, sep = "=")
mean_values4_sel = mean_values4[, c(1,2,7,8)]

mean_wider = mean_values4_sel %>% pivot_wider(names_from = particlesize, values_from = percentage)
tmean = as.data.frame(t(mean_wider))

new_colnames <- paste(tmean[1, ], tmean[2, ], sep = "=")
tmean <- tmean[-c(1, 2), ]
colnames(tmean) = new_colnames

colnames(tmean, tmean[])

# Filter the rows based on record_number

plot1 <- ggplot(data = grain3, aes(x = as.numeric(particlesize), y = percentage, color = dispergant, linetype = method)) +
  geom_line() +
  scale_x_log10() +
  #facet_wrap(~Measurement.Date.Time, scales = "free_y") +
  facet_grid(size_in ~ repetetion) +
  labs(title = "Particle Size Distribution", x = "Particle Size (mm)", y = "Percentage Undersize") +
  theme_minimal()
plot(plot1)

plot2 <- ggplot()+ 
  geom_line(data = grain3, aes(x = as.numeric(particlesize), y = percentage, color = dispergant, linetype = method)) +
  geom_line(data = mean_values3, aes(x = as.numeric(particlesize), y = percentage, color = dispergant, linetype = method)) +
  scale_x_log10() +
  #facet_wrap(~Measurement.Date.Time, scales = "free_y") +
  facet_grid(size_in ~ repetetion) +
  labs(title = "Particle Size Distribution2", x = "Particle Size (mm)", y = "Percentage Undersize") +
  theme_minimal()
plot(plot2)

plot2 <- ggplot()+ 
  geom_line(data = grain3, aes(x = as.numeric(particlesize), y = percentage, color = dispergant, linetype = method)) +
  geom_line(data = mean_values3, aes(x = as.numeric(particlesize), y = percentage, color = dispergant, linetype = method)) +
  scale_x_log10() +
  #facet_wrap(~Measurement.Date.Time, scales = "free_y") +
  facet_grid(size_in ~ repetetion) +
  labs(title = "Particle Size Distribution2", x = "Particle Size (mm)", y = "Percentage Undersize") +
  theme_minimal()
plot(plot2)

plot3 <- ggplot(data = mean_values3, aes(x = as.numeric(particlesize), y = percentage, color = dispergant, linetype = method)) +
  geom_line()+
  scale_x_log10() +
  #facet_wrap(~Measurement.Date.Time, scales = "free_y") +
  facet_grid(size_in ~ .) +
  labs(title = "Particle Size Distribution3_mean", x = "Particle Size (mm)", y = "Percentage Undersize") +
  theme_minimal()
plot(plot3)

