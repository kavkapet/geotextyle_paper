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
srcDTA$runoff_start_t_form <- as.POSIXct(srcDTA$time.to.runoff, format = "%H:%M:%S")
zero_time <- as.POSIXct("00:00:00", format = "%H:%M:%S")

srcDTA$dt_t_form <- as.POSIXct(srcDTA$interval.duration, format = "%H:%M:%S")
srcDTA$tot_time_t_form = as.POSIXct(ifelse(srcDTA$t2_t_form == zero_time| is.na(srcDTA$t2_t_form), srcDTA$runoff_start_t_form, srcDTA$dt_t_form))

srcDTA$tot_time_t_form <- as.POSIXct(
  ifelse(is.na(srcDTA$tot_time_t_form), srcDTA$runoff_start_t_form, srcDTA$tot_time_t_form))

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

data_combined$t1_hour = as.numeric(format(data_combined$t1_t_form, "%H")) + as.numeric(format(data_combined$t1_t_form, "%M"))/60 + as.numeric(format(data_combined$t1_t_form, "%S"))/3600 
data_combined$t1_sec = as.numeric(format(data_combined$t1_t_form, "%H"))*3600 + as.numeric(format(data_combined$t1_t_form, "%M"))*60 + as.numeric(format(data_combined$t1_t_form, "%S")) 
data_combined$CC_int_time_sec = as.numeric(format(data_combined$tot_time_t_form, "%H"))*3600 + as.numeric(format(data_combined$tot_time_t_form, "%M"))*60 + as.numeric(format(data_combined$tot_time_t_form, "%S")) 


data_combined <- data_combined %>% filter(
    !is.na(soilloss) | 
    !is.na(runoff) | 
    !is.na(rainfall.total..mm.) | 
    !is.na(rain.intensity..mm.h.1.) | 
    !data_combined$t1_hour <= 0)
  
data_combined <- data_combined[!is.na(data_combined$total.discharge..l.), ]

# Convert flow rate from l/min to mm/h
# 1 liter = 0.001 m³
# Flow rate (m³/h) = flow.rate.l.min.1 * 0.001 * 60
# Depth (mm/h) = (Flow rate m³/h) / Area m² * 1000
data_combined$CC_Rain_m3 = data_combined$rainfall.total..mm./1000*data_combined$area
data_combined$CC_Runoff_m3 = data_combined$total.discharge..l./1000
data_combined$CC_Inf_m3 = data_combined$CC_Rain_m3 - data_combined$CC_Runoff_m3
data_combined$CC_Inf_m3 = ifelse (data_combined$CC_Inf_m3 >= 0,data_combined$CC_Inf_m3,data_combined$CC_Rain_m3)


data_combined$flow_rate_m3_h = data_combined$runoff * 0.001 * 60 # Convert l/min to m³/h
data_combined$flow_rate_mm_h = (data_combined$flow_rate_m3_h / data_combined$area) * 1000 # Convert flow rate to mm/h
#data_combined$infiltration_intensity_m_s1 = (data_combined$rainInt_m_s - data_combined$runofInt_m_s) # Calculate infiltration intensity
#data_combined$infiltration_intensity_m_s1 = ifelse (data_combined$infiltration_intensity_m_s1 >= 0,data_combined$infiltration_intensity_m_s1,data_combined$rainInt_m_s)

#data_combined <- data_combined %>% filter(data_combined$run.ID == 311)


# Set lower and upper bounds for S and K
#plot(x = subset_data$interval.., y =modeled_intensity, col = "red", ylim = c(0, 0.03))
#points(x = subset_data$interval.., y =subset_data$infiltration_intensity_m_s1, ylim = c(0, 0.03))


un_list <- c("crop", "initial.cond.", "run.ID")

# Create the unique combinations and count rows for each combination
unique_combinations <- data_combined %>%
  group_by(across(all_of(un_list))) %>%
  summarize(count = n(), .groups = 'drop')
# Define the Philip model for infiltration intensity
# I(t) = (S / (2 * sqrt(t))) + K
philip_model <- function(params, Ti) {
  S <- params[2]
  K <- params[1]
  S = ifelse (S >= 0,S,0)
  #browser()
  #XX = (S / (2 * sqrt(Ti))) + K
  #ifelse(time >= 0,return(0),
  return((0.01* S / (2 * sqrt(Ti))) + K)
  #)
  
}

# Define the objective function to minimize
# It calculates the sum of squared residuals between observed and modeled infiltration intensity
objective_function <- function(params) {
  #browser()
  S <- params[2]
  K <- params[1]
  Ti <- subset_data$t1_sec
  dTi <- subset_data$CC_int_time_sec
  area = subset_data$area
  inf_intensity <- philip_model(params, Ti)
 #browser()
  CC_modeled_intensity = cumsum(inf_intensity*dTi*area)
  residuals <- subset_data$CC_Inf_m3 - CC_modeled_intensity
  #browser()
  #plot(x =  subset_data$t1_t_form, y = subset_data$CC_Rain_m3)
  #points(x =  subset_data$t1_t_form, y = subset_data$CC_Runoff_m3, col = "blue")
  #points(x =  subset_data$t1_t_form, y = CC_modeled_intensity, col = "red")
  #plot(x =  subset_data$t1_t_form, y = residuals, col = "green")
  #plot(x =  subset_data$t1_t_form, y = inf_intensity, col = "green")
  
  return(sum(residuals^2))
}




#lower_bounds_dry <- c(1*10^-7,4*10^-5) # Lower bounds for Sorptivity (S) and Hydraulic Conductivity (K [m/s])#
#upper_bounds_dry <- c(1*10^-5, 1*10^-2) # Upper bounds for Sorptivity (S) and Hydraulic Conductivity (K)
#lower_bounds_wet <- c(1*10^-7, 1*10^-8) # Lower bounds for Sorptivity (S) and Hydraulic Conductivity (K)
#upper_bounds_wet <- c(5*10^-5, 4*10^-3) # Upper bounds for Sorptivity (S) and Hydraulic Conductivity (K)
lower_bounds_dry <- c(1*10^-10,4*10^-10) # Lower bounds for Sorptivity (S) and Hydraulic Conductivity (K [m/s])
upper_bounds_dry <- c(1, 1*10^-1) # Upper bounds for Sorptivity (S) and Hydraulic Conductivity (K)
lower_bounds_wet <- c(1*10^-7, 1*10^-10) # Lower bounds for Sorptivity (S) and Hydraulic Conductivity (K)
upper_bounds_wet <- c(1, 1) # Upper bounds for Sorptivity (S) and Hydraulic Conductivity (K)

results_df = data.frame()

#for (xID in 1:nrow(unique_combinations)) {
for (xID in 394:394) {
  
  # Subset data for the current unique combination
  xxID = as.integer(unique_combinations[xID, 3])
  subset_data = data_combined[data_combined$run.ID == xxID,]
  #browser()
  print(paste("Start for combination:", xID, nrow(subset_data), xxID))
  
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
  #midpoints = c((mean(upper_bounds[1]+lower_bounds[1])/2), (mean(upper_bounds[2] + lower_bounds[2]))/2)
  #initial_population <- matrix(nrow = 100, ncol = length(lower_bounds_dry))
  initial_population[rdm, ] <- lower_bounds + runif(length(lower_bounds), 0, 1) * (upper_bounds - lower_bounds)
    for (rdm in 1:100) {
      #initial_population[j, ] <- midpoints + runif(length(midpoints), -midpoints, midpoints)
      initial_population[rdm, ] <- midpoints + runif(length(midpoints), -midpoints , midpoints) # Adjust the range as needed
    }
  #browser()
  ga_result <- tryCatch({
    ga(
      type = "real-valued",
      fitness = function(params) - objective_function(params),
      lower = lower_bounds,
      upper = upper_bounds,
      popSize = 1000,           # Increase population size for more diversity
      maxiter = 2000,          # More generations for broader search
      run = 400,               # Stop if no improvement after 100 generations
      seed = 123,              # Set seed for reproducibility
      suggestions = initial_population,  # Use a broad initial population
      pmutation = 0.3,         # Increase mutation rate for more random changes
      pcrossover = 0.6,         # Lower crossover rate to rely less on parents
      monitor = FALSE
    )

  }, error = function(e) {
    # Handle the error: print message and continue with the next iteration
    print(paste("Error in GA for combination", xID, ":", e$message))
    return(NULL)  # Return NULL if GA fails
  })
  
 
  # Extract the optimized Ks value
  #browser()
  summary(ga_result)
  optimal_KsS <- ga_result@solution
  #print(optimal_KsS)
  
  # Append the results for the current combination to the results dataframe
  resline = data.frame(crop = unique_combinations[xID, 1], 
                                             initial_cond = unique_combinations[xID, 2], 
                                             locality = unique_combinations[xID, 3],
                                             optimized_KsS = optimal_KsS)
  print(resline[1,])
  #browser()
  results_df <- rbind(results_df, resline)
  #browser()
}
# Extract the optimal parameters from the GA result

results_df_to_merge =  results_df[,c(3,4,5)]
data_combined = merge(data_combined, results_df_to_merge, by = "run.ID")
data_combined$optimazedInf = philip_model(params = c(data_combined$optimized_KsS.x1, data_combined$optimized_KsS.x2), Ti = data_combined$t1_hour)
data_combined$optimazedTotInf = data_combined$optimazedInf*data_combined$CC_int_time_sec*data_combined$area


data_combined <- data_combined %>%
  group_by(run.ID) %>%
  arrange(t2) %>%  # Order rows by t2 within each run.ID group
  mutate(cumulative_optimazedTotInf = cumsum(optimazedTotInf)) %>%
  ungroup()


plotx <- ggplot() +
  geom_point(data = data_combined, aes(x = t1_hour, y = cumulative_optimazedTotInf)) + 
  geom_point(data = data_combined, aes(x = t1_hour, y = data_combined$CC_Rain_m3), color = "red") +
  geom_point(data = data_combined, aes(x = t1_hour, y = data_combined$CC_Runoff_m3), color = "blue") +
  geom_point(data = data_combined, aes(x = t1_hour, y = data_combined$CC_Inf_m3), color = "green") +
  labs(x = "Time (hours)", y = "Infiltration Intensity (m/s)", title = "Comparison of Optimized and Measured Infiltration Intensity") +
  theme_minimal()+
  #ylim(0,0.06)+
  facet_grid(data_combined$run.ID ~ data_combined$initial.cond.)

print(plotx)


                 
plot(plot)

results_df_to_merge =  results_df[,c(3,4,5)]
data_combined = merge(data_combined, results_df_to_merge, by = "run.ID")
data_combined$optimazedInf = philip_model(params = c(data_combined$optimized_KsS.x1, data_combined$optimized_KsS.x2), Ti = data_combined$t1_hour)

plot(x = data_combined$infiltration_intensity_m_s1, y = data_combined$optimazedInf)
plot(data_combined$t1_hour, data_combined$optimazedInf)
points(data_combined$t1_hour, data_combined$infiltration_intensity_m_s1, col = "red")

plot = ggplot(results_df, aes(x = run.ID, y = optimized_KsS.x1, colour = initial.cond.)) +
  geom_point()+
  geom_hline(yintercept = lower_bounds_dry[1], colour = "red")+
  geom_hline(yintercept = lower_bounds_wet[1], colour = "green")+
  geom_hline(yintercept = upper_bounds_dry[1], colour = "blue")+
  geom_hline(yintercept = upper_bounds_wet[1], colour = "gray")
  
  
                 
plot(plot)

plot = ggplot(results_df, aes(x = run.ID, y = optimized_KsS.x2, colour = initial.cond.)) +
  geom_point()+
  scale_y_log10() +
  geom_hline(yintercept = lower_bounds_dry[2], colour = "red")+
  geom_hline(yintercept = lower_bounds_wet[2], colour = "green")+
  geom_hline(yintercept = upper_bounds_dry[2], colour = "blue")+
  geom_hline(yintercept = upper_bounds_wet[2], colour = "gray")


plotx = ggplot()+
geom_point(data_combined, aes(x = data_combined$t1_hour, y = data_combined$optimazedInf)) + 
geom_point(data_combined, aes(x = data_combined$t1_hour, y = data_combined$infiltration_intensity_m_s1, color = "red"))

# Predict the infiltration intensity using the optimized S and K
data <- data %>%
  mutate(
    fitted_S = 
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

# Initialize the results dataframe if not already done
results_df <- data.frame(crop = character(), 
                         initial_cond = character(), 
                         locality = numeric(), 
                         optimized_Ks = numeric(),
                         optimized_S = numeric())

for (i in 1:nrow(unique_combinations)) {
  
  # Subset data for the current unique combination
  xxID <- as.integer(unique_combinations[i, 3])
  subset_data <- data_combined[data_combined$run.ID == xxID, ]
  
  print(paste("Start for combination:", i, nrow(subset_data)))
  
  # Define bounds for "dry" and "wet" conditions
  lower_bounds_dry <- c(4e-4, 1e-7)
  upper_bounds_dry <- c(1e-3, 1e-5)
  lower_bounds_wet <- c(1e-5, 1e-7)
  upper_bounds_wet <- c(4e-4, 1e-5)
  
  # Set bounds based on the condition
  if (!is.na(subset_data$initial.cond[1]) && subset_data$initial.cond[1] == "dry") {
    lower_bounds <- lower_bounds_dry
    upper_bounds <- upper_bounds_dry
  } else if (!is.na(subset_data$initial.cond[1]) && subset_data$initial.cond[1] == "wet") {
    lower_bounds <- lower_bounds_wet
    upper_bounds <- upper_bounds_wet
  } else {
    print(paste("Skipping combination", i, "due to missing initial condition"))
    next  # Skip this iteration if initial condition is missing
  }
  
  # Calculate midpoints and generate initial population
  midpoints <- (lower_bounds + upper_bounds) / 2
  initial_population <- matrix(nrow = 30, ncol = length(lower_bounds))
  for (j in 1:30) {  # Changed the inner loop variable to `j`
    initial_population[j, ] <- midpoints + runif(length(midpoints), -midpoints , midpoints)
  }
  
  # Run the Genetic Algorithm with error handling
  ga_result <- tryCatch({
    ga(
      type = "real-valued",
      fitness = function(params) -objective_function(params),
      lower = lower_bounds,
      upper = upper_bounds,
      popSize = 100,
      maxiter = 1000,
      run = 100,
      seed = 123,
      suggestions = initial_population
    )
  }, error = function(e) {
    # Handle the error: print message and continue with the next iteration
    print(paste("Error in GA for combination", i, ":", e$message))
    return(NULL)
  })
  
  # Check if ga_result is NULL and skip if GA failed
  if (is.null(ga_result)) {
    print(paste("Skipping combination", i, "due to GA failure"))
    next
  }
  
  # Extract the optimized values
  optimal_KsS <- ga_result@solution
  #browser()
  # Append the results for the current combination to the results dataframe
  results_df <- rbind(results_df, data.frame(unique_combinations[i, 1:3], optimal_KsS[1], optimal_KsS[2])
  )
}

plotx <- ggplot() +
  geom_point(data = subset_data, aes(x = t1_hour, y = modeled_intensity)) + 
  geom_point(data = subset_data, aes(x = t1_hour, y = infiltration_intensity_m_s1), color = "red") +
  geom_point(data = subset_data, aes(x = t1_hour, y = rainInt_m_s), color = "blue") +
  labs(x = "Time (hours)", y = "Infiltration Intensity (m/s)", title = "Comparison of Optimized and Measured Infiltration Intensity") +
  theme_minimal()+
  ylim(0,0.002)+
  facet_grid(subset_data$initial.cond. ~.)

print(plotx)
