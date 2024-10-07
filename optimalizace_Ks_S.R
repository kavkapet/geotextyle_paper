# Load necessary libraries
library(ggplot2)
library(dplyr)
library(GA)  # Genetic Algorithm library

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
data_combined$rain.intensity..mm.h.1. = as.numeric(data_combined$rain.intensity..mm.h.1.)

data_combined$soilloss <- as.numeric(data_combined$`SS.flux..g.min.1.`)
data_combined$slope = data_combined$`plot.slope....`/100  # In this case, slope can be treated as variable (you can change it)
data_combined <- data_combined %>%
  mutate(BBCH = ifelse(cover == "bare" & is.na(BBCH), 0, BBCH),  # Existing condition
         BBCH = ifelse(cover == "geotex", 50, BBCH))


data_combined$C <- (100 - data_combined$BBCH)/100   # In this case, slope can be treated as variable (you can change it)
data_combined <- data_combined %>% mutate(C = ifelse(is.na(C), 0.95, C))

# Convert flow rate from l/min to mm/h
# 1 liter = 0.001 m³
# Flow rate (m³/h) = flow.rate.l.min.1 * 0.001 * 60
# Depth (mm/h) = (Flow rate m³/h) / Area m² * 1000
data_combined$flow_rate_m3_h = data_combined$flow.rate..l.min.1. * 0.001 * 60 # Convert l/min to m³/h
data_combined$flow_rate_mm_h = (data_combined$flow_rate_m3_h / data_combined$area) * 1000 # Convert flow rate to mm/h
data_combined$infiltration_intensity_m_s1 = (data_combined$rain.intensity..mm.h.1. - data_combined$flow_rate_mm_h)/3600 # Calculate infiltration intensity
data_combined$t1_hour = as.numeric(format(data_combined$t1_t_form, "%H")) + as.numeric(format(data_combined$t1_t_form, "%M"))/60 + as.numeric(format(data_combined$t1_t_form, "%S"))/3600 
data_combined$t1_sec = as.numeric(format(data_combined$t1_t_form, "%H"))*3600 + as.numeric(format(data_combined$t1_t_form, "%M"))*60 + as.numeric(format(data_combined$t1_t_form, "%S")) 
data_combined <- data_combined %>% filter(
    !is.na(soilloss) & 
    !is.na(runoff) & 
    !is.na(rainfall.total..mm.) & 
    !is.na(rain.intensity..mm.h.1.) & 
    !data_combined$t1_hour <= 0)
#data_combined <- data_combined %>% filter(data_combined$run.ID == 311)


# Set lower and upper bounds for S and K




un_list <- c("crop", "initial.cond.", "run.ID")

# Create the unique combinations and count rows for each combination
unique_combinations <- data_combined %>%
  group_by(across(all_of(un_list))) %>%
  summarize(count = n(), .groups = 'drop')
# Define the Philip model for infiltration intensity
# I(t) = (S / (2 * sqrt(t))) + K
philip_model <- function(params, Ti) {
  S <- params[1]
  K <- params[2]
  S = ifelse (S >= 0,S,0)
  #browser()
  #XX = (S / (2 * sqrt(Ti))) + K
  #ifelse(time >= 0,return(0),
  return((S / (2 * sqrt(Ti))) + K)
  #)
  
}

# Define the objective function to minimize
# It calculates the sum of squared residuals between observed and modeled infiltration intensity
objective_function <- function(params) {
  #browser()
  S <- params[1]
  K <- params[2]
  Ti <- subset_data$t1_hour
  modeled_intensity <- philip_model(params, Ti)
  residuals <- subset_data$infiltration_intensity - modeled_intensity
  return(sum(residuals^2))
}

results_df = data.frame()

for (i in 1:nrow(unique_combinations)) {
  #for (i in 5:10) {
  
  # Subset data for the current unique combination
  xxID = as.integer(unique_combinations[i, 3])
  subset_data = data_combined[data_combined$run.ID == xxID,]
  #browser()
  print(paste("Start for combination:", i, nrow(subset_data)))
  
  lower_bounds_dry <- c(4*10^-4,1*10^-7) # Lower bounds for Sorptivity (S) and Hydraulic Conductivity (K)
  upper_bounds_dry <- c(1*10^-3,1*10^-5) # Upper bounds for Sorptivity (S) and Hydraulic Conductivity (K)
  # Run the Genetic Algorithm to optimize S and K
  lower_bounds_wet <- c(1*10^-5,1*10^-7) # Lower bounds for Sorptivity (S) and Hydraulic Conductivity (K)
  upper_bounds_wet <- c(4*10^-4,1*10^-5) # Upper bounds for Sorptivity (S) and Hydraulic Conductivity (K)
  # Run the Genetic Algorithm to optimize S and K
  if (subset_data$initial.cond[1] == "dry") {
    lower_bounds = lower_bounds_dry
    upper_bounds = upper_bounds_dry
    # Code to run if condition is TRUE
  } else {
    lower_bounds = lower_bounds_wet
    upper_bounds = upper_bounds_wet
    # Code to run if condition is FALSE
  }
  midpoints = (lower_bounds + upper_bounds)/2
  initial_population <- matrix(nrow = 30, ncol = length(lower_bounds_dry))
    for (i in 1:30) {
    initial_population[i, ] <- midpoints + runif(length(midpoints), -midpoints/2 , midpoints/2) # Adjust the range as needed
    }
  browser()
  ga_result <- tryCatch({
    ga(
      type = "real-valued",
      fitness = function(params) -objective_function(params), # GA maximizes, so negate the objective function
      lower = lower_bounds,
      upper = upper_bounds,
      popSize = 100,      # Population size
      maxiter = 1000,    # Maximum number of iterations
      run = 100,         # Stop if no improvement after 100 iterations
      seed = 123,         # Set seed for reproducibility
      suggestions = initial_population
    )
  }, error = function(e) {
    # Handle the error: print message and continue with the next iteration
    print(paste("Error in GA for combination", i, ":", e$message))
    return(NULL)  # Return NULL if GA fails
  })
  
 
  # Extract the optimized Ks value
  optimal_KsS <- ga_result@solution
  #print(optimal_KsS)
  
  # Append the results for the current combination to the results dataframe
  results_df <- rbind(results_df, data.frame(crop = unique_combinations[i, 1], 
                                             initial_cond = unique_combinations[i, 2], 
                                             locality = unique_combinations[i, 3],
                                             optimized_KsS = optimal_KsS))
  
}
# Extract the optimal parameters from the GA result

xlower_bounds_dry <- c(4*10^-4,1*10^-7) # Lower bounds for Sorptivity (S) and Hydraulic Conductivity (K)
xupper_bounds_dry <- c(1*10^-3,1*10^-5) # Upper bounds for Sorptivity (S) and Hydraulic Conductivity (K)
# Run the Genetic Algorithm to optimize S and K
xlower_bounds_wet <- c(1*10^-5,1*10^-7) # Lower bounds for Sorptivity (S) and Hydraulic Conductivity (K)
xupper_bounds_wet <- c(4*10^-4,1*10^-5) # Upper bounds for Sorptivity (S) and Hydraulic Conductivity (K)


plot = ggplot(results_df, aes(x = run.ID, y = optimized_KsS.x2, colour = initial.cond.)) +
  geom_point()#+
  geom_hline(yintercept = xlower_bounds_dry[2], colour = "red")+
#  geom_hline(yintercept = xlower_bounds_wet[2], colour = "green")+
  geom_hline(yintercept = xupper_bounds_dry[2], colour = "blue")
#  geom_hline(yintercept = xupper_bounds_wet[2], colour = "gray")
  
  
                 
plot(plot)

plot = ggplot(results_df, aes(x = run.ID, y = optimized_KsS.x1, colour = initial.cond.)) +
  geom_point()+
  geom_hline(yintercept = xlower_bounds_dry[1], colour = "red")+
  geom_hline(yintercept = xlower_bounds_wet[1], colour = "green")+
  geom_hline(yintercept = xupper_bounds_dry[1], colour = "blue")+
  geom_hline(yintercept = xupper_bounds_wet[1], colour = "gray")
  
  
                 
plot(plot)

best_params <- ga_result@solution
S_opt <- best_params[1]
K_opt <- best_params[2]

cat("Optimal Sorptivity (S):", S_opt, "mm/h^(1/2)\n")
cat("Optimal Hydraulic Conductivity (K):", K_opt, "mm/h\n")

# Predict the infiltration intensity using the optimized S and K
data <- data %>%
  mutate(
    fitted_infiltration_intensity = philip_model(c(S_opt, K_opt), time)
  )

# Plot observed vs fitted infiltration intensity
ggplot(data, aes(x = time)) +
  geom_point(aes(y = infiltration_intensity), color = "blue", size = 2) +
  geom_line(aes(y = fitted_infiltration_intensity), color = "red", size = 1) +
  labs(title = "Observed vs Fitted Infiltration Intensity (GA Optimization)",
       x = "Time (hours)",
       y = "Infiltration Intensity (mm/h)",
       caption = "Blue points: Observed | Red line: Fitted") +
  theme_minimal()

# Calculate goodness-of-fit metrics
RSS <- sum((data$infiltration_intensity - data$fitted_infiltration_intensity)^2)
TSS <- sum((data$infiltration_intensity - mean(data$infiltration_intensity))^2)
R_squared <- 1 - RSS/TSS
cat("R-squared:", R_squared, "\n")
