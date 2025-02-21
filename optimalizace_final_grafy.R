
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(GA)  # Genetic Algorithm library
library(hydroGOF)
library(doParallel)


#num_cores = detectCores() - 2




# Load data
setwd("d:/2_granty_projekty/2_Bezici/0_DS/datbaze_data/")
srcDTA =  read.csv(file = "optiamalized_K_S_CN_Ia.csv",sep = ",",fileEncoding = "UTF-8")

CNmodel <- function(CN, Ia, Hrain_mm) {
  CN <- CN
  rIa <- Ia
  #browser()
  A = 25.4 * (1000/CN-10)
  Ia = A*rIa
  He_mm <- ifelse(Ia > Hrain_mm, 0, (Hrain_mm - Ia)^2 / (Hrain_mm - Ia + A))
  
  return(He_mm)
  #)
  
}

srcDTA$CUM_runoff_mm = CNmodel(CN = srcDTA$best_CN, Ia = srcDTA$best_Ia, srcDTA$rainfall.total..mm.)
srcDTA$CUM_runoff_m3 = srcDTA$CUM_runoff_mm*srcDTA$area/1000

plot(x = srcDTA$CUM_runoff_m3, y = srcDTA$CC_Runoff_m3)

subNSE = srcDTA[srcDTA$nse_CN>0,]

plot(x = subNSE$CUM_runoff_m3, y = subNSE$CC_Runoff_m3)

plotplot = ggplot(data = subNSE, aes(x = subNSE$t1_sec,  y = subNSE$nse_CN, color =subNSE$crop, size = subNSE$C, label = "")) +#, fill = rain5dnu))  +
  geom_point() +
  geom_text(hjust=0, vjust=2, size = 2, color = "black") +
  #scale_color_gradient(low="blue", high="green")+
  theme_minimal() +                               # Use a clean theme
  #ylim(0,1) +
  #scale_color_manual(values = c("blue", "red", "green")) + # Optional: specify colors for each condition
  theme(legend.title = element_text(size = 12),   # Customize legend
        legend.text = element_text(size = 10))

plot(plotplot)


plotmes =  plotplot + facet_grid( ~ theme_minimal() +                               # Use a clean theme
  ylim(0,1) +
  scale_color_manual(values = c("blue", "red", "green")) + # Optional: specify colors for each condition
  theme(legend.title = element_text(size = 12),   # Customize legend
        legend.text = element_text(size = 10))
  )
plot(plotmes)  

plotplot = plotplot + 
  geom_point(data = erosion_date, aes(x = mean60MinRain,  y = mean10MinRain) , color = "red", size = 6, shape=10) +
  labs(title = paste("Situation", cochci, erosion_date$date_form, "day", erosion_date$day_maxRain, "hour", erosion_date$max60MinRain, "ten min", erosion_date$max10MinRain,"dapi1max",  erosion_date$dapi1max))
plot(plotplot)




xx = CNmodel(params = dd, Hrain_mm = subset_data$CC_Rain_m3/subset_data$area*1000)
Hrain_mm <- subset_data$CC_Rain_m3/subset_data$area*1000
plot(x = subset_data$t1_sec, y = subset_data$CC_Rain_m3/subset_data$area*1000, col = "gray")
points(x = subset_data$t1_sec, y = subset_data$CC_Runoff_m3/subset_data$area*1000, col = "blue")
points(x = subset_data$t1_sec, y = xx, col = "red")
points(x = subset_data$t1_sec, y = Hrain_mm, col = "green")
plot(x = subset_data$t1_sec, y = xx, col = "red")
ynse_value <- hydroGOF::NSE(subset_data$CC_Runoff_m3/subset_data$area*1000, xx)

results_df_to_mergeCN =  best_solutions_dfCN
data_combinedCN = merge(data_combined, results_df_to_mergeCN, by = "run.ID")
data_combinedCN$OptimA =  25.4 * (1000/data_combinedCN$best_CN-10)
data_combinedCN$Ia = data_combinedCN$OptimA * data_combinedCN$best_Ia
data_combinedCN$Hrain_mm = data_combinedCN$CC_Rain_m3/data_combinedCN$area*1000
data_combinedCN$OptimHe_mm = (data_combinedCN$Hrain_mm - data_combinedCN$Ia)^2/(data_combinedCN$Hrain_mm - data_combinedCN$Ia + data_combinedCN$OptimA)
data_combinedCN$OptimHe_mm <- ifelse(data_combinedCN$Ia > data_combinedCN$Hrain_mm, 0, data_combinedCN$OptimHe_mm)
#data_combinedCN$optimazedTotInf_m3 = data_combinedCN$optimazedInf_mm*data_combinedCN$CC_int_time_sec*data_combinedCN$area


data_combinedCN <- data_combinedCN %>%
  group_by(run.ID) %>%
  arrange(t2) %>%  # Order rows by t2 within each run.ID group
  mutate(CNcumulative_optimazedTotRunoff_m3 = cumsum(OptimHe_mm*area/1000)) %>%
  ungroup()
# Calculate NSE for each run.ID group
nse_results <- data_combinedCN %>%
  group_by(run.ID) %>%
  summarize(CN_NSE = hydroGOF::NSE(CNcumulative_optimazedTotRunoff_m3, CC_Rain_m3), .groups = 'drop')

# Join the NSE results back to the original dataframe
data_combinedCN <- left_join(data_combinedCN, nse_results, by = "run.ID")
ready = data_combinedCN[data_combinedCN$CN_NSE>0.25,]
zzz = data_combinedCN[,c(1,5,12,49, 50, 51, 52, 54,55,56,57,58,59,60,61, 53)]


# Calculate NSE for each run.ID group, if you haven't already
nse_plot <- data_combinedCN %>%
  group_by(run.ID, initial.cond.) %>%
  summarize(CN_NSE = hydroGOF::NSE(CNcumulative_optimazedTotRunoff_m3, CC_Rain_m3), .groups = 'drop')

# Plot the data with coloring by initial.cond

ggplot(nse_plot, aes(x = run.ID, y = CN_NSE, color = initial.cond.)) +
  geom_point(size = 3) +                          # Scatter plot points with size adjustment
  #  geom_line(aes(group = initial.cond.)) +          # Connect points with lines, grouped by initial.cond
  labs(title = "Nash-Sutcliffe Efficiency (NSE) by Run ID and Initial Condition",
       x = "Run ID",
       y = "Nash-Sutcliffe Efficiency (NSE)") +
  theme_minimal() +                               # Use a clean theme
#  ylim(-5,1) +
  scale_color_manual(values = c("blue", "red", "green")) + # Optional: specify colors for each condition
  theme(legend.title = element_text(size = 12),   # Customize legend
        legend.text = element_text(size = 10))
