# Load necessary libraries
library(readxl)    # For reading Excel files
library(minpack.lm)
library(dplyr)# For non-linear least squares optimization
######GA optimalizace
library(GA)

# Load data
file_path <- "d:/0_Smoderp/IA_test.xlsx"
file_path <- "d:/2_granty_projekty/2_Bezici/0_DS/datbaze_data/runoff_sediment.csv"

data <- read.csv2(file_path, header = TRUE, sep = ";", dec = ",")

#dry_data <- read_excel(file_path, sheet = "dry", col_names = TRUE) # Skipping the first row with metadata
#wet_data <- read_excel(file_path, sheet = "wet", col_names = TRUE)
#dry_data$init = 1
#wet_data$init = 2

# Combine dry and wet data for the analysis

data_combined <- rbind(dry_data, wet_data)
data_combined = data

# Clean data: remove rows with NAs and convert columns to numeric
data_combined <- na.omit(data_combined)
#data_combined$runoff <- as.numeric(data_combined$`runoff l/min`)
#data_combined$soilloss <- as.numeric(data_combined$`soilloss g/min`)
data_combined$runoff <- as.numeric(data_combined$`prutok_l_min`)
data_combined$soilloss <- as.numeric(data_combined$`ztrata_pudy_g_min`)
data_combined$slope <- 9.0  # In this case, slope can be treated as variable (you can change it)

# Define the slope constant
slope <- 9.0

# Define the model function: soilloss = Z * Q^X * S^Y
model_function <- function(params, runoff, slope) {
  Z <- params[1]
  X <- params[2]
  Y <- params[3]
  Z * (runoff^X) * (slope^Y) * 0.57
}


# Define the fitness function for GA optimization
fitness_function <- function(params, data_subset) {
  slope <- 9  # Assuming constant slope, change if necessary
  predicted_soil_loss <- model_function(params, runoff, slope)
  return(-sum((data_subset$ztrata_pudy_g_min - predicted_soil_loss)^2))  # Sum of squared residuals
}

# Set up the boundaries for the parameters (Z, X, and Y)
lower_bounds <- c(0, 0, 0)  # Lower bounds for Z, X, and Y
upper_bounds <- c(100, 10, 10)  # Upper bounds for Z, X, and Y

# Run the genetic algorithm (GA) optimization
ga_result <- ga(type = "real-valued", 
                fitness = fitness_function, 
                lower = lower_bounds, 
                upper = upper_bounds, 
                popSize = 50, 
                maxiter = 100)

# Calculate the predicted values using the optimized parameters
predicted_soilloss <- model_function(optimal_params, data_combined$runoff, data_combined$slope)
data_combined$predicted_soilloss = predicted_soilloss

# Create an empty dataframe to store the results
results_df <- data.frame(plodina = character(), 
                         poc_vlhkost = numeric(), 
                         Z = numeric(), 
                         X = numeric(), 
                         Y = numeric(),
                         stringsAsFactors = FALSE)

# Loop through unique combinations of `plodina` and `poc_vlhkost`
unique_combinations <- unique(data[, c("plodina", "poc_vlhkost")])


for (i in 1:nrow(unique_combinations)) {
  # Subset data based on `plodina` and `poc_vlhkost`
  subset_data <- data %>% filter(plodina == unique_combinations$plodina[i], poc_vlhkost == unique_combinations$poc_vlhkost[i])
  
  # Run the genetic algorithm (GA) optimization for the subset
  ga_result <- ga(type = "real-valued", 
                  fitness = function(params) fitness_function(params, subset_data), 
                  lower = lower_bounds, 
                  upper = upper_bounds, 
                  popSize = 50, 
                  maxiter = 1000)
  
  # Extract the optimized parameters
  optimal_params <- ga_result@solution
  
  # Append the results to the dataframe
  results_df <- rbind(results_df, data.frame(plodina = unique_combinations$plodina[i], 
                                             poc_vlhkost = unique_combinations$poc_vlhkost[i], 
                                             Z = optimal_params[1], 
                                             X = optimal_params[2], 
                                             Y = optimal_params[3]))
}


# Print the resulting dataframe with best fits

print(results_df)

# Optionally, save the results as a CSV file
# write.csv(results_df, "best_fits_per_group.csv", row.names = FALSE)

# Plot the observed vs. predicted soil loss
plot(data_combined$soilloss, predicted_soilloss, pch = 16, col = data_combined$init,
     xlab = "Observed Soil Loss (g/min)", 
     ylab = "Predicted Soil Loss (g/min)", 
     main = "Observed vs Predicted Soil Loss with 1:1 Line")

# Add 1:1 line for reference (ideal fit line)
abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)  # a = intercept, b = slope; red dashed line

# Add the regression equation as text on the plot
equation <- paste0("Soil Loss = ", round(optimal_params[1], 2), 
                   " * Runoff^", round(optimal_params[2], 2), 
                   " * Slope^", round(optimal_params[3], 2))
text(x = max(data_combined$soilloss) * 0.5, 
     y = max(predicted_soilloss) * 0.9, 
     labels = equation, col = "black", cex = 1.2)

# Optionally, add a legend
legend("topleft", legend = c("1:1 Line", "Data"), col = c("red", "blue"), lty = c(2, NA), pch = c(NA, 16))
data_combined$soiloss_martin = 2.11*data_combined$`runoff l/min`^2.035*data_combined$slope^1.36*0.57/10
plot(data_combined$soilloss, data_combined$soiloss_martin, pch = 10, col = data_combined$init)
points(data_combined$soilloss, data_combined$predicted_soilloss, pch = 9, col = data_combined$init+2)

abline(a = 0, b = 1, col = "black", lwd = 2, lty = 2)  # a = intercept, b = slope; red dashed line






# Define the fitness function for GA optimization
fitness_function <- function(params, data_subset) {
  slope <- 9  # Assuming constant slope, change if necessary
  predicted_soil_loss <- model_function(params, data_subset$prutok_l_min, slope)
  return(-sum((data_subset$ztrata_pudy_g_min - predicted_soil_loss)^2))  # Sum of squared residuals
}

# Set up the boundaries for the parameters (Z, X, and Y)
lower_bounds <- c(0, 0, 0)  # Lower bounds for Z, X, and Y
upper_bounds <- c(100, 10, 10)  # Upper bounds for Z, X, and Y

# Create an empty dataframe to store the results
results_df <- data.frame(plodina = character(), 
                         poc_vlhkost = numeric(), 
                         Z = numeric(), 
                         X = numeric(), 
                         Y = numeric(),
                         stringsAsFactors = FALSE)

# Loop through unique combinations of `plodina` and `poc_vlhkost`
unique_combinations <- unique(data[, c("plodina", "poc_vlhkost")])

for (i in 1:nrow(unique_combinations)) {
  # Subset data based on `plodina` and `poc_vlhkost`
  subset_data <- data %>%
    filter(plodina == unique_combinations$plodina[i], poc_vlhkost == unique_combinations$poc_vlhkost[i])
  
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
  results_df <- rbind(results_df, data.frame(plodina = unique_combinations$plodina[i], 
                                             poc_vlhkost = unique_combinations$poc_vlhkost[i], 
                                             Z = optimal_params[1], 
                                             X = optimal_params[2], 
                                             Y = optimal_params[3]))
}

# Print the resulting dataframe with best fits
print(results_df)

# Optionally, save the results as a CSV file
# write.csv(results_df, "best_fits_per_group.csv", row.names = FALSE)
# Define the fitness function for GA optimization
fitness_function <- function(params, data_subset) {
  slope <- 9  # Assuming constant slope, change if necessary
  predicted_soil_loss <- model_function(params, subset_data$ru runoff, slope)
  return(-sum((data_subset$ztrata_pudy_g_min - predicted_soil_loss)^2))  # Sum of squared residuals
}
