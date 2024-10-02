# Load necessary libraries
library(readxl)    # For reading Excel files
library(minpack.lm)
library(dplyr)# For non-linear least squares optimization
######GA optimalizace
library(GA)
library(ggplot2)
library(data.table)
rmf = "d:/rm_to_word/rm.md"
wmf = "d:/rm_to_word/"
rmarkdown::render(rmf, output_format = "word_document", wmf)
# Load data
setwd("d:/2_granty_projekty/2_Bezici/0_DS/datbaze_data/")
srcDTA =  read.csv(file = "runoff_sediment_intervals_20240925_en.csv",sep = ";",fileEncoding = "UTF-8")

# Clean data: remove rows with NAs and convert columns to numeric
#data_combined <- na.omit(data_combined)
#data_combined$runoff <- as.numeric(data_combined$`runoff l/min`)
#data_combined$soilloss <- as.numeric(data_combined$`soilloss g/min`)
srcDTA$TIMESTAMP = as.POSIXct(strptime(srcDTA$date, "%d.%m.%Y"))
srcDTA$t1_t_form <- as.POSIXct(srcDTA$t1, format = "%H:%M:%S")
srcDTA$t2_t_form <- as.POSIXct(srcDTA$t2, format = "%H:%M:%S")
srcDTA$dt_t_form <- as.POSIXct(srcDTA$interval.duration, format = "%H:%M:%S")
srcDTA$month <- format(srcDTA$TIMESTAMP, "%m")
#srcDTA$dt_min <- format(srcDTA$dt_t_form, "%M") + format(srcDTA$dt_t_form, "%M")/60

srcDTA$cover  <- case_when(
  srcDTA$crop %in% c("cultivated fallow", "bare soil") ~ "bare",
  srcDTA$crop %in% c("Geotextile Macmat 8.1", "Geotextile Enkamat 7010", "Geotextile K700", "Geotextile Biomac-c", "Geotextile Enkamat 7020", "Geotextile Macmat 18.1", "Macmat 18 fill", "Jute", "Triangle", "Enkamat 7020 filled", "Fortrac 3D filled", "Fortrac 3D") ~ "geotex",
  TRUE ~ "vege")


data_combined = srcDTA
data_combined$area = data_combined$plot.length..m. * data_combined$plot.width..m.
data_combined$runoff <- as.numeric(data_combined$flow.rate..l.min.1.)
data_combined$runoffhighMM <- data_combined$total.discharge..l. / data_combined$area
data_combined <- data_combined %>%
  mutate(crop = ifelse(is.na(crop), "Unknown", crop))

data_combined$soilloss <- as.numeric(data_combined$`SS.flux..g.min.1.`)
data_combined$slope = data_combined$`plot.slope....`/100  # In this case, slope can be treated as variable (you can change it)
data_combined <- data_combined %>%
  mutate(BBCH = ifelse(cover == "bare" & is.na(BBCH), 0, BBCH),  # Existing condition
         BBCH = ifelse(cover == "geotex", 50, BBCH))


data_combined$C <- (100 - data_combined$BBCH)/100   # In this case, slope can be treated as variable (you can change it)
data_combined <- data_combined %>% mutate(C = ifelse(is.na(C), 0.95, C))
data_combined <- data_combined %>% filter(!is.na(soilloss) & !is.na(runoff) & !is.na(rainfall.total..mm.))

# Define the model function: soilloss = Z * Q^X * S^Y

# Function to calculate runoff based on CN
calc_runoff <- function(P, CN, Iax) {
  S <- (25400 / CN) - 254  # Retention potential S
  Ia <- Iax * S            # Initial abstraction Ia
  
  # Add condition to return NaN if P <= 0
  if (P <= 0) {
    return(NaN)  # Return NaN if rainfall P is less than or equal to 0
  } else if (P <= Ia) {
    return(0)  # If rainfall P is less than or equal to Ia, no runoff occurs
  } else {
    return(((P - Ia)^2) / (P - Ia + S))  # Runoff Q
  }
}





# Fitness function for GA optimization
fitness_function <- function(params) {
  CN <- params[1]    # Extract CN
  Iax <- params[2]   # Extract Iax
  Q_modeled <- sapply(P_observed, calc_runoff, CN = CN, Iax = Iax)  # Modeled runoff
  return(-sum((Q_modeled - Q_observed)^2, na.rm = TRUE))  # Minimize negative sum of squared differences
}



# Set up the boundaries for the parameters (Z, X, and Y)
lower_bounds <- c(30, 0.1)  # Lower bounds for Z, X, and Y
upper_bounds <- c(99, 0.2)  # Upper bounds for Z, X, and Y


un_list <- c("crop", "initial.cond.", "locality")

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

# Loop through each unique combination of crop, initial.cond., and locality
for (i in 1:nrow(unique_combinations)) {
#for (i in 5:10) {

  # Subset data for the current unique combination
  subset_data <- data_combined %>%
    filter(crop == unique_combinations[i, 1], 
           initial.cond. == unique_combinations[i, 2], 
           locality == unique_combinations[i, 3])
  
  # Extract the observed rainfall and runoff for the current subset
  P_observed <- subset_data$rainfall.total..mm.
  Q_observed <- subset_data$runoffhighMM
 
  
  # Skip this combination if there's not enough data
  #if (length(P_observed) < 2 | length(Q_observed) < 2) {
   # next  # Skip to the next iteration if not enough data
  #}

  
  print(paste("Start for combination:", i))
  # Run the GA optimization for the current subset
  # Run the GA optimization
  ga_result <- ga(
    type = "real-valued",      # We're optimizing a real number (CN)
    fitness = fitness_function,  # Fitness function to minimize
    lower = lower_bounds,       # Lower bound for CN
    upper = upper_bounds,       # Upper bound for CN
    popSize = 50,              # Population size
    maxiter = 1000,            # Maximum number of iterations
    run = 100                  # Number of consecutive generations without improvement
  )
  # Extract the optimized CN value
  optimal_CN <- ga_result@solution
  print(optimal_CN)
  
  # Append the results for the current combination to the results dataframe
  results_df <- rbind(results_df, data.frame(crop = unique_combinations[i, 1], 
                                             initial_cond = unique_combinations[i, 2], 
                                             locality = unique_combinations[i, 3],
                                             optimized_CN = optimal_CN))
}
  
  
  

# Extract the optimized parameters
# Print the resulting dataframe with best fits

colnames(results_df) = c(un_list, "CN", "Iax")
print(results_df)

data_combined =  inner_join(data_combined, results_df, by = un_list)
data_combined$S = (25400 / data_combined$CN) - 254
data_combined$Ia = data_combined$S*data_combined$Iax
data_combined$Cond = data_combined$rainfall.total..mm. - data_combined$Ia
# Use mapply to apply the calc_runoff function row by row
data_combined$runoffhighCN <- mapply(
  calc_runoff, 
  P = data_combined$rainfall.total..mm.,  # Pass the rainfall column
  CN = data_combined$CN,                  # Pass the optimized CN from results_df
  Iax = data_combined$Iax                 # Pass the optimized Iax from results_df
)
fwrite(data_combined, 'all_simulation.csv')
fwrite(results_df, 'CN_Iax_optimalization.csv')

for (i in 1:nrow(unique_combinations)) {
  print(i)
  print(unique_combinations[i, 1])
  
  # Extract the crop, initial.cond., and locality values from the unique_combinations dataframe
  crop_val <- as.character(unique_combinations[i, 1])
  initial_cond_val <- as.character(unique_combinations[i, 2])
  locality_val <- as.character(unique_combinations[i, 3])
  
  # Subset data_combined based on the current values of crop, initial.cond., and locality
  subset_data <- data_combined %>%
    filter(crop == crop_val, 
           initial.cond. == initial_cond_val, 
           locality == locality_val)
  
  # Create a name using the extracted values
  name <- paste(crop_val, initial_cond_val, locality_val, sep = "_")
  
  # Plot the CN deviation
  png(paste(name, "CNdev.png"), width = 800, height = 600)
  plot(subset_data$t1_t_form, subset_data$runoffhighCN, col = "red")
  points(subset_data$t1_t_form, subset_data$runoffhighMM, col = "blue")
  legend("topright", legend = c(name, "red = simulated, blue = measured"), pch = 1)
  dev.off()
  
  # Plot the CN vs runoff
  png(paste(name, "CN_toCN.png"), width = 800, height = 600)
  plot(subset_data$runoffhighMM, subset_data$runoffhighCN, col = "green")
  abline(a = 0, b = 1, col = "red", lwd = 2, lty = 2)
  legend("topright", legend = c(name), pch = 1)
  dev.off()
}  

plot = ggplot(data = data_combined, aes(x = data_combined$cover,  y = data_combined$CN, color = data_combined$cover)) + #, size = dapi5mean , label = date_form))
  geom_boxplot()+
  geom_point()+
  facet_grid(data_combined$initial.cond. ~.)+
  ggsave(paste(i,"CN.png"))
plot(plot)


