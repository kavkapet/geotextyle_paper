# Load necessary libraries
library(readxl)    # For reading Excel files
library(minpack.lm)
library(dplyr)# For non-linear least squares optimization
######GA optimalizace
library(GA)
library(ggplot2)
library(data.table)

# Load data
setwd("d:/2_granty_projekty/2_Bezici/0_DS/datbaze_data/")
data =  read.csv(file = "runoff_sediment_intervals_20240925_en.csv",sep = ";",fileEncoding = "UTF-8")

#data <- read.csv2(file_path, header = TRUE, sep = ";", dec = ",")

#dry_data <- read_excel(file_path, sheet = "dry", col_names = TRUE) # Skipping the first row with metadata
#wet_data <- read_excel(file_path, sheet = "wet", col_names = TRUE)
#dry_data$init = 1
#wet_data$init = 2

# Combine dry and wet data for the analysis

#data_combined <- rbind(dry_data, wet_data)
srcDTA = data

# Clean data: remove rows with NAs and convert columns to numeric
#data_combined <- na.omit(data_combined)
#data_combined$runoff <- as.numeric(data_combined$`runoff l/min`)
#data_combined$soilloss <- as.numeric(data_combined$`soilloss g/min`)
srcDTA$TIMESTAMP = as.POSIXct(strptime(srcDTA$date, "%d.%m.%Y"))
srcDTA$t1_t_form <- as.POSIXct(srcDTA$t1, format = "%H:%M:%S")
srcDTA$t2_t_form <- as.POSIXct(srcDTA$t2, format = "%H:%M:%S")
srcDTA$dt_t_form <- as.POSIXct(srcDTA$interval.duration, format = "%H:%M:%S")
srcDTA$month <- format(srcDTA$TIMESTAMP, "%m")
srcDTA$dt_min <- format(srcDTA$dt_t_form, "%M") + format(srcDTA$dt_t_form, "%M")/60

srcDTA$cover  <- case_when(
  srcDTA$crop %in% c("cultivated fallow", "bare soil") ~ "bare",
  srcDTA$crop %in% c("Geotextile Macmat 8.1", "Geotextile Enkamat 7010", "Geotextile K700", "Geotextile Biomac-c", "Geotextile Enkamat 7020", "Geotextile Macmat 18.1", "Macmat 18 fill", "Jute", "Triangle", "Enkamat 7020 filled", "Fortrac 3D filled", "Fortrac 3D") ~ "geotex",
  TRUE ~ "vege")
data_combined = srcDTA

data_combined$runoff <- as.numeric(data_combined$flow.rate..l.min.1.)
data_combined$soilloss <- as.numeric(data_combined$`SS.flux..g.min.1.`)
data_combined$slope <- 0.09  # In this case, slope can be treated as variable (you can change it)
data_combined$slope = data_combined$`plot.slope....`/100  # In this case, slope can be treated as variable (you can change it)
data_combined$K <- 0.57  # In this case, slope can be treated as variable (you can change it)
data_combined <- data_combined %>%
  mutate(BBCH = ifelse(cover == "bare" & is.na(BBCH), 0, BBCH),  # Existing condition
         BBCH = ifelse(cover == "geotex", 50, BBCH))


data_combined$C <- (100 - data_combined$BBCH)/100   # In this case, slope can be treated as variable (you can change it)
data_combined <- data_combined %>% mutate(C = ifelse(is.na(C), 0.95, C))
data_combined <- data_combined %>% filter(!is.na(soilloss) & !is.na(runoff))

# Define the model function: soilloss = Z * Q^X * S^Y
model_function <- function(params, runoff, slope, K, C) {
  Z <- params[1]
  X <- params[2]
  Y <- params[3]
  (Z) * (runoff^X) * (slope^Y) * K * C
}

# Define the fitness function for GA optimization
fitness_function <- function(params, data_subset) {
  predicted_soil_loss <- model_function(params, data_subset$runoff, data_subset$slope, data_subset$K, data_subset$C)
  return(-sum((data_subset$soilloss - predicted_soil_loss)^2))  # Sum of squared residuals
}

# Set up the boundaries for the parameters (Z, X, and Y)
lower_bounds <- c(1, 0, 0)  # Lower bounds for Z, X, and Y
upper_bounds <- c(100, 10, 10)  # Upper bounds for Z, X, and Y


un_list <- c("crop", "initial.cond.")

# Create the unique combinations and count rows for each combination
unique_combinations <- data_combined %>%
  group_by(across(all_of(un_list))) %>%
  summarize(count = n(), .groups = 'drop')
ggplot(unique_combinations, aes(x = crop, y = count, fill = as.factor(initial.cond.))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count of Unique Combinations",
       x = "Plodina",
       y = "Count",
       fill = "Poc Stav") +
  theme_minimal()
unique_combinations = as.matrix(unique_combinations)
# Create an empty dataframe to store the results

results_df = data.frame()

for (i in 1:nrow(unique_combinations)) {
#for (i in 1:1) {
  # Subset data based on `plodina` and `poc_vlhkost` !!!!!tady_bacha, na to co se optimalizuje
  subset_data <- data_combined %>%
    filter(crop == unique_combinations[i, 1], initial.cond. == unique_combinations[i,2])
  print(i)
  
  # Define the fitness function inside the loop (without passing data_subset as an argument)
  fitness_function_in_loop <- function(params) {
    fitness_function(params, subset_data)
  }
  
  # Run the genetic algorithm (GA) optimization for the subset
  ga_result <- ga(type = "real-valued", 
                  fitness = fitness_function_in_loop,  # Now pass the fitness function defined inside the loop
                  lower = lower_bounds, 
                  upper = upper_bounds, 
                  popSize = 50, 
                  maxiter = 1000)
  
  # Extract the optimized parameters
  optimal_params <- ga_result@solution
  
  # Append the results to the dataframe
  results_df <- rbind(results_df, data.frame(plodina = unique_combinations[i,1], 
                                             poc_vlhkost = unique_combinations[i,2], 
                                             Z = optimal_params[1], 
                                             X = optimal_params[2], 
                                             Y = optimal_params[3]
                                             ))
}


# Extract the optimized parameters
#optimal_params <- ga_result@solution


# Calculate predicted soil loss
#predicted_soil_loss <- model_function(optimal_params, data_combined$runoff, data_combined$slope, data_combined$K, data_combined$C)

# Store the predicted values back in the original dataframe

# Print the resulting dataframe with best fits
colnames(results_df) = c(un_list, "Z", "X", "Y")
print(results_df)

data_combined =  inner_join(data_combined, results_df, by = un_list)
data_combined$n = 1/data_combined$Z
#(1/n) * (runoff^X) * (slope^Y) * K * C
data_combined$predicted_soilloss_g_l_min = (1/data_combined$n)*(data_combined$runoff^data_combined$X)*(data_combined$slope^data_combined$Y)*data_combined$K*data_combined$C#   model_function(paramss, data_combined$runoff, data_combined$slope, data_combined$K, data_combined$C, )
# Print the resulting dataframe with best fits
# Optionally, save the results as a CSV file
# write.csv(results_df, "best_fits_per_group.csv", row.names = FALSE)

# Plot the observed vs. predicted soil loss
data_combined$init = 1
plot(data_combined$soilloss, data_combined$predicted_soilloss_g_l_min, pch = 16, col = as.factor(data_combined$month),
     xlab = "Observed Soil Loss (g/min)", 
     ylab = "Predicted Soil Loss (g/min)", 
     main = "Observed vs Predicted Soil Loss with 1:1 Line")

# Add 1:1 line for reference (ideal fit line)
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)  # a = intercept, b = slope; red dashed line

# Add the regression equation as text on the plot
equation <- paste0("Soil Loss = ", round(results_df[3], 2), 
                   " * Runoff^", round(results_df[4], 2), 
                   " * Slope^", round(results_df[3], 2))
text(x = max(data_combined$soilloss) * 0.5, 
     y = max(data_combined$predicted_soilloss_g_l_min) * 0.9, 
     labels = equation, col = "black", cex = 1.2)

# Optionally, add a legend
legend("topleft", legend = c("1:1 Line", "Data"), col = c("red", "blue"), lty = c(2, NA), pch = c(NA, 16))
data_combined$soiloss_martin = 2.11*data_combined$runoff^2.035*(data_combined$slope*100)^1.36*0.57/10
plot(data_combined$soilloss, data_combined$soiloss_martin, pch = 10, col = data_combined$init, ylim = c(0,5000))
points(data_combined$soilloss, data_combined$predicted_soilloss_g_l_min, pch = 9, col = data_combined$init+2)

abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)  # a = intercept, b = slope; red dashed line


xplot= ggplot(data_combined, aes(x = cover, y = predicted_soilloss_g_l_min, color = as.factor(initial.cond.))) +
  geom_boxplot() +
  labs(title = "Calculated",
       x = "Plodina",
       y = "Count",
       fill = "Poc Stav") +
  theme(axis.text.x = element_text(angle = 45))+
  theme_minimal()
yplot= ggplot(data_combined, aes(x = cover, y = soilloss, color = as.factor(initial.cond.))) +
  geom_boxplot() +
  labs(title = "Measured",
       x = "Plodina",
       y = "Count",
       fill = "Poc Stav") +
  theme(axis.text.x = element_text(angle = 45))+
  theme_minimal()
  
plot(yplot)#####
gridExtra::grid.arrange(xplot, yplot, ncol = 1)

#subset_data <- data_combined %>% filter(plodina == unique_combinations[i, 1], poc_vlhkost == unique_combinations[i,2])

summary_stats <- data_combined %>%
  group_by(cover, initial.cond.) %>%
  summarize(
    mean_runoff = mean(runoff, na.rm = TRUE),
    sd_runoff = sd(runoff, na.rm = TRUE),
    mean_soilloss = mean(soilloss, na.rm = TRUE),
    sd_soilloss = sd(soilloss, na.rm = TRUE),
    mean_predicted_soilloss = mean(predicted_soilloss_g_l_min, na.rm = TRUE),
    sd_predicted_soilloss = sd(predicted_soilloss_g_l_min, na.rm = TRUE),
    mean_X = mean(X, na.rm = TRUE),
    sd_X = sd(X, na.rm = TRUE),
    mean_Y = mean(Y, na.rm = TRUE),
    sd_Y = sd(Y, na.rm = TRUE),
    mean_Z = mean(Z, na.rm = TRUE),
    sd_Z = sd(Z, na.rm = TRUE),
    mean_C = mean(C, na.rm = TRUE),
    sd_C = sd(C, na.rm = TRUE),
    .groups = 'drop'  # Optional, avoids group-level data being kept
  )
print(summary_stats)


plot(summary_stats$mean_soilloss, summary_stats$mean_predicted_soilloss, pch = 10, col = as.factor(summary_stats$cover))
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)  # a = intercept, b = slope; red dashed line
points(data_combined$soilloss, data_combined$predicted_soilloss_g_l_min, pch = 9, col = data_combined$init+2)


#####Sel otoceny_simulator_simulace_366
sim366 = data_combined[1 == c(366),]

  