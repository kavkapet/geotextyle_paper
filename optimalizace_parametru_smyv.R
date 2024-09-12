# Load necessary libraries
library(readxl)    # For reading Excel files
library(minpack.lm)
library(dplyr)# For non-linear least squares optimization
######GA optimalizace
library(GA)
library(ggplot2)
library(data.table)

# Load data
file_path <- "d:/0_Smoderp/IA_test.xlsx"
file_path <- "d:/2_granty_projekty/2_Bezici/0_DS/datbaze_data/runoff_sediment.csv"

data <- read.csv2(file_path, header = TRUE, sep = ";", dec = ",")

#dry_data <- read_excel(file_path, sheet = "dry", col_names = TRUE) # Skipping the first row with metadata
#wet_data <- read_excel(file_path, sheet = "wet", col_names = TRUE)
#dry_data$init = 1
#wet_data$init = 2

# Combine dry and wet data for the analysis

#data_combined <- rbind(dry_data, wet_data)
data_combined = data

# Clean data: remove rows with NAs and convert columns to numeric
#data_combined <- na.omit(data_combined)
#data_combined$runoff <- as.numeric(data_combined$`runoff l/min`)
#data_combined$soilloss <- as.numeric(data_combined$`soilloss g/min`)
data_combined$runoff <- as.numeric(data_combined$`prutok_l_min`)
data_combined$soilloss <- as.numeric(data_combined$`ztrata_pudy_g_min`)
data_combined$slope <- 0.09  # In this case, slope can be treated as variable (you can change it)
data_combined$K <- 0.57  # In this case, slope can be treated as variable (you can change it)
data_combined <- data_combined %>% mutate(BBCH = ifelse(plodina == "úhor udržovaný" & is.na(BBCH), 0, BBCH))
data_combined$C <- (100 - (100 - data_combined$BBCH))/100   # In this case, slope can be treated as variable (you can change it)
data_combined <- data_combined %>% mutate(C = ifelse(is.na(C), 0.95, C))

data_combined <- data_combined %>%
  filter(!is.na(soilloss) & !is.na(runoff))


# Define the model function: soilloss = Z * Q^X * S^Y
model_function <- function(params, runoff, slope, K, C) {
  Z <- params[1]
  X <- params[2]
  Y <- params[3]
  Z * (runoff^X) * (slope^Y) * K * C
}

# Define the fitness function for GA optimization
fitness_function <- function(params, data_subset) {
  predicted_soil_loss <- model_function(params, data_subset$runoff, data_subset$slope, data_subset$K, data_subset$C)
  return(-sum((data_subset$soilloss - predicted_soil_loss)^2))  # Sum of squared residuals
}

# Set up the boundaries for the parameters (Z, X, and Y)
lower_bounds <- c(0, 0, 0)  # Lower bounds for Z, X, and Y
upper_bounds <- c(100, 10, 10)  # Upper bounds for Z, X, and Y


un_list <- c("plodina", "poc_stav")

# Create the unique combinations and count rows for each combination
unique_combinations <- data_combined %>%
  group_by(across(all_of(un_list))) %>%
  summarize(count = n(), .groups = 'drop')
ggplot(unique_combinations, aes(x = plodina, y = count, fill = as.factor(poc_stav))) +
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
  # Subset data based on `plodina` and `poc_vlhkost` !!!!!tady_bacha, na to co se optimalizuje
  subset_data <- data_combined %>%
    filter(plodina == unique_combinations[i, 1], poc_stav == unique_combinations[i,2])
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
optimal_params <- ga_result@solution


# Calculate predicted soil loss
predicted_soil_loss <- model_function(optimal_params, data_combined$runoff, data_combined$slope, data_combined$K)

# Store the predicted values back in the original dataframe

# Print the resulting dataframe with best fits
colnames(results_df) = c(un_list, "X", "Y", "Z", "C")
print(results_df)

data_combined =  inner_join(data_combined, results_df, by = un_list)

paramss = data_combined[,c(23, 24, 25, 26)]


predicted_soilloss = model_function(paramss, data_combined$runoff, data_combined$slope, data_combined$K)
data_combined$predicted_soilloss =  predicted_soilloss[,1]
# Print the resulting dataframe with best fits

# Optionally, save the results as a CSV file
# write.csv(results_df, "best_fits_per_group.csv", row.names = FALSE)

# Plot the observed vs. predicted soil loss
data_combined$init = 1
plot(data_combined$soilloss, data_combined$predicted_soilloss, pch = 16, col = data_combined$init,
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
data_combined$soiloss_martin = 2.11*data_combined$runoff^2.035*(data_combined$slope*100)^1.36*0.57/10
plot(data_combined$soilloss, data_combined$soiloss_martin, pch = 10, col = data_combined$init)
points(data_combined$soilloss, data_combined$predicted_soilloss, pch = 9, col = data_combined$init+2)

abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)  # a = intercept, b = slope; red dashed line


xplot= ggplot(data_combined, aes(x = plodina, y = C.y, color = as.factor(poc_stav))) +
  geom_point() +
  labs(title = "Count of Unique Combinations",
       x = "Plodina",
       y = "Count",
       fill = "Poc Stav") +
  theme(axis.text.x = element_text(angle = 45))+
  theme_minimal()
  
plot(xplot)#####
subset_data <- data_combined %>% filter(plodina == unique_combinations[i, 1], poc_vlhkost == unique_combinations[i,2])
