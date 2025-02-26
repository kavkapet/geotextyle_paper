
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(GA)  # Genetic Algorithm library
library(hydroGOF)

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


data_fall <- data_combined %>% filter(
    is.na(soilloss) | 
    is.na(interval.duration) | 
    is.na(runoff) | 
    is.na(rainfall.total..mm.) | 
    is.na(rain.intensity..mm.h.1.) | 
    data_combined$t1_hour <= 0 |
    data_combined$soilloss < 0)
X <- data_combined[is.na(data_combined$total.discharge..l.), ]
Y <- data_combined[is.na(data_combined$dt_t_form), ]
Z = bind_rows(data_fall, X, Y)
data_fall= distinct(Z)

write.csv(data_fall, "data_fall_nodata.csv")

    #!data_combined$CC_int_time_sec <= 0)
data_combined = anti_join(data_combined, data_fall)


# Convert flow rate from l/min to mm/h
# 1 liter = 0.001 m³
# Flow rate (m³/h) = flow.rate.l.min.1 * 0.001 * 60
# Depth (mm/h) = (Flow rate m³/h) / Area m² * 1000
data_combined$CC_Rain_m3 = data_combined$rainfall.total..mm./1000*data_combined$area
data_combined$CC_Runoff_m3 = data_combined$total.discharge..l./1000
data_combined$CC_Inf_m3 = data_combined$CC_Rain_m3 - data_combined$CC_Runoff_m3
data_combined$CC_Inf_m3 = ifelse (data_combined$CC_Inf_m3 >= 0,data_combined$CC_Inf_m3,data_combined$CC_Rain_m3)
data_combined$CC_control = data_combined$CC_Rain_m3 - data_combined$CC_Runoff_m3

data_fall_minusrunoff <- data_combined %>% filter(
  data_combined$CC_control < 0)
write.csv(data_fall_minusrunoff, "data_fall_minusrunoff.csv")

data_fall_date <- data_combined %>% filter(
  data_combined$TIMESTAMP == c("2022-07-14"))
write.csv(data_fall_date, "data_fall_date.csv")

data_combined = anti_join(data_combined, data_fall_minusrunoff)
data_combined = anti_join(data_combined, data_fall_date)


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

write.csv(data_combined, "data_combined.csv")
# I(t) = (S / (2 * sqrt(t))) + K
philip_model <- function(params, Ti) {
  S <- params[2]
  K <- params[1]
  S = ifelse (S >= 0,S,0)
  #browser()
  #XX = (S / (2 * sqrt(Ti))) + K
  #ifelse(time >= 0,return(0)
  return((S / (2 * sqrt(Ti))) + K)
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
  CC_modeled_inf_m3 = cumsum(inf_intensity*dTi*area)
  nse_value <- hydroGOF::NSE(CC_modeled_inf_m3, subset_data$CC_Inf_m3)
  residuals <- subset_data$CC_Inf_m3 - CC_modeled_inf_m3
#  plot(x =  subset_data$t1_t_form, y = subset_data$CC_Rain_m3)
 # points(x =  subset_data$t1_t_form, y = subset_data$CC_Runoff_m3, col = "blue")
  #points(x =  subset_data$t1_t_form, y = CC_modeled_inf_m3, col = "red")
  #points(x =  subset_data$t1_t_form, y = subset_data$CC_Inf_m3, col = "green")
  #plot(x =  subset_data$t1_t_form, y = residuals, col = "green")
  sumres = (sum(residuals^2))
  #nic <<- append(nic, sumres)
  #browser()
  return(sumres)
  #return(nse_value)
}




lower_bounds_dry <- c(0,4*10^-15) # Lower bounds for Sorptivity (S) and Hydraulic Conductivity (K [m/s])
upper_bounds_dry <- c(1*10^-4, 1*10^-3) # Upper bounds for Sorptivity (S) and Hydraulic Conductivity (K)
# Run the Genetic Algorithm to optimize S and K
lower_bounds_wet <- c(0, 4*10^-15) # Lower bounds for Sorptivity (S) and Hydraulic Conductivity (K)
upper_bounds_wet <- c(1*10^-4, 4*10^-3) # Upper bounds for Sorptivity (S) and Hydraulic Conductivity (K)
nic = c()
results_df = data.frame()
# Initialize an empty data frame to store the best solutions
best_solutions_df <- data.frame(
  #crop = character(),
  #initial_cond = character(),
  run.ID = numeric(),
  best_S = numeric(),
  best_K = numeric(),
  best_fitness = numeric(),
  stringsAsFactors = TRUE
)
unique_run_ids <- sort(unique(data_combined$run.ID))
# Loop through the combinations
for (xID in unique_run_ids) {
#for (xID in 464:464) {
  #if (unique_combinations[xID, "initial.cond."] != "very wet") {
  #  next  # Skip this iteration if it's not "very wet"
  #}
  
  # Subset data for the current unique combination
  #xxID <- as.integer(unique_combinations[xID, 3])
  subset_data <- data_combined[data_combined$run.ID == xID,]
  
  print(paste("Start for combination:", xID, nrow(subset_data), subset_data$locality[1]))
  
  # Set bounds based on initial condition
  if (subset_data$initial.cond.[1] == "dry") {
    lower_bounds <- lower_bounds_dry
    upper_bounds <- upper_bounds_dry
  } else {
    lower_bounds <- lower_bounds_wet
    upper_bounds <- upper_bounds_wet
  }
  
  # Initialize population
  initial_population <- matrix(nrow = 500, ncol = length(lower_bounds))
  #for (rdm in 1:500) {
  #  initial_population[rdm, ] <- lower_bounds + runif(length(lower_bounds), 0, 1) * (upper_bounds - lower_bounds)
  #}
  
  initial_population[,1] <- runif(500, lower_bounds[1], upper_bounds[1])
  initial_population[,2] <- runif(500, lower_bounds[2], upper_bounds[2])
  # Run the GA with tryCatch for error handling
  ga_result <- tryCatch({
    ga(
      type = "real-valued",
      fitness = function(params) - objective_function(params),
      lower = lower_bounds,
      upper = upper_bounds,
      popSize = 500,
      maxiter = 500,
      run = 1000,
      seed = 123,
      suggestions = initial_population,
      pmutation = 0.02,
      pcrossover = 0.8,
      monitor = FALSE,
      optimArgs = list(method = "L-BFGS-B", 
                       poptim = 0.1,
                       pressel = 0.25,
                       control = list(fnscale = -1, maxit = 1000)),
      optim = TRUE
      
    )
  }, error = function(e) {
    print(paste("Error in GA for combination", xID, ":", e$message))
    return(NULL)
  })
  
  # If GA was successful, save the best solution
  if (!is.null(ga_result)) {
    optimal_KsS <- if (!is.null(ga_result@solution)) ga_result@solution else c(NA, NA)
    best_fitness <- if (!is.null(ga_result@fitnessValue)) ga_result@fitnessValue else NA
    #browser()
    # Append the best solution and fitness to the data frame
    best_solutions_df <- rbind(best_solutions_df, data.frame(
      #crop = unique_combinations[xID, 1], 
      #initial_cond = unique_combinations[xID, 2], 
      run.ID = xID,
      best_S = optimal_KsS[2],
      best_K = optimal_KsS[1],
      best_fitness = best_fitness
    ))
  } else {
    print(paste("Skipping combination", xID, "due to GA failure"))
    
    # Append NA values to indicate a failure in GA for this combination
    best_solutions_df <- rbind(best_solutions_df, data.frame(
      #crop = unique_combinations[xID, 1], 
      #initial_cond = unique_combinations[xID, 2], 
      run.ID = xID,
      best_S = NA,
      best_K = NA,
      best_fitness = NA
    
    ))

  }

}




# Extract the optimal parameters from the GA result

results_df_to_merge =  best_solutions_df
data_combined_save = data_combined
#data_combined =data_combined_save
data_combined = merge(data_combined, results_df_to_merge, by = "run.ID")
data_combined$optimazedInf_mm = philip_model(params = c(data_combined$best_K, data_combined$best_S), Ti = data_combined$t1_sec)
data_combined$xx =  ((data_combined$best_S / (2 * sqrt(data_combined$t1_sec))) + data_combined$best_K)
data_combined$optimazedInf_mm = data_combined$xx
data_combined$optimazedTotInf_m3 = data_combined$optimazedInf_mm*data_combined$CC_int_time_sec*data_combined$area
write.csv(data_combined, "data_combined.csv")


data_combined <- data_combined %>%
  group_by(run.ID) %>%
  arrange(t2) %>%  # Order rows by t2 within each run.ID group
  mutate(cumulative_optimazedTotInf_m3 = cumsum(optimazedTotInf_m3)) %>%
  ungroup()
data_combined$runIN_interval = data_combined$
write.csv(data_combined, "data_combined.csv")
#data_combined =  read.csv(file = "data_combined.csv",sep = ",",fileEncoding = "UTF-8")

# Calculate NSE for each run.ID group
nse_plot =  data_combined[data_combined$run.ID != 244, ]
nse_plot1 <- nse_plot %>%
  group_by(run.ID) %>%
  summarize(Inf_NSE = hydroGOF::NSE(cumulative_optimazedTotInf_m3, CC_Inf_m3), .groups = 'drop')
nse_plot = merge(x = nse_plot, y = nse_plot1, by = "run.ID")

# Join the NSE results back to the original dataframe
ready = data_combined[data_combined$Inf_NSE>0.25,]
zzz = data_combined[,c(1,5,12,49, 50, 51, 52, 54,55,56,57,58,59,60,61, 53)]


# Calculate NSE for each run.ID group, if you haven't already


# Plot the data with coloring by initial.cond

xplot = ggplot(nse_plot, aes(x = run.ID, y = Inf_NSE, color = initial.cond.)) +
  geom_point(size = 3) +                          # Scatter plot points with size adjustment
#  geom_line(aes(group = initial.cond.)) +          # Connect points with lines, grouped by initial.cond
  labs(title = "Nash-Sutcliffe Efficiency (NSE) by Run ID and Initial Condition",
       x = "Run ID",
       y = "Nash-Sutcliffe Efficiency (NSE)") +
  theme_minimal() +                               # Use a clean theme
  ylim(0,1) +
  scale_color_manual(values = c("blue", "red", "green")) + # Optional: specify colors for each condition
  theme(legend.title = element_text(size = 12),   # Customize legend
        legend.text = element_text(size = 10))
  

plot(xplot)

plotx <- ggplot() +
  geom_point(data = data_combined, aes(x = t1_hour, y = data_combined$cumulative_optimazedTotInf, color  = "red")) + 
#  geom_point(data = data_combined, aes(x = t1_hour, y = data_combined$CC_Rain_m3), color = "gray") +
 # geom_point(data = data_combined, aes(x = t1_hour, y = data_combined$CC_Runoff_m3), color = "blue") +
  #geom_point(data = data_combined, aes(x = t1_hour, y = data_combined$CC_Inf_m3), color = "green") +
  labs(x = "Time (hours)", y = "Infiltration Intensity (m/s)", title = "Comparison of Optimized and Measured Infiltration Intensity") +
  theme_minimal()+
  #ylim(0,0.06)+
  facet_grid(data_combined$run.ID ~ .)

plot(plotx)



########################NIC

ggplot(data_combined, aes(x = t1, y = Inf_NSE_individual, color = initial.cond.)) +
  geom_point(size = 3) +                          # Scatter plot points with size adjustment
  #  geom_line(aes(group = initial.cond.)) +          # Connect points with lines, grouped by initial.cond
  labs(title = "Nash-Sutcliffe Efficiency (NSE) by Run ID and Initial Condition",
       x = "Run ID",
       y = "Nash-Sutcliffe Efficiency (NSE)") +
  theme_minimal() +                               # Use a clean theme
  scale_color_manual(values = c("blue", "red", "green")) + # Optional: specify colors for each condition
  theme(legend.title = element_text(size = 12),   # Customize legend
        legend.text = element_text(size = 10))+
  ylim(0,1)

results_df_to_merge =  results_df[,c(3,4,5)]
data_combined = merge(data_combined, results_df_to_merge, by = "run.ID")
data_combined$optimazedInf = philip_model(params = c(data_combined$best_K, data_combined$best_S), Ti = data_combined$t1_hour)

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

##### ADD CN 
CNdta = read.csv(file = "output.csv",sep = ",",fileEncoding = "UTF-8")
CNdta$runID_time = paste(as.character(CNdta$run.ID), as.character(CNdta$interval..), sep = "_")
CNdta_sel = CNdta[,c(65,58:64)]
colnames(CNdta_sel) = c("runID_time","P_mm", "Q_measured_mm", "CN_optimized", "X_optimized", "Ia_optimized", "NSE_CN", "RMSE_CN")
data_combined$runID_time = paste(as.character(data_combined$run.ID), as.character(data_combined$interval..), sep = "_")

allDTA = merge(data_combined, CNdta_sel, by = "runID_time")
allDTA <- allDTA %>%
  mutate(year = substr(TIMESTAMP, 1, 4))  # 

nse_plot =  allDTA[allDTA$run.ID != 244, ]
nse_plot1 <- nse_plot %>%
  group_by(run.ID) %>%
  summarize(Inf_NSE = hydroGOF::NSE(cumulative_optimazedTotInf_m3, CC_Inf_m3), .groups = 'drop')
nse_plot = merge(x = nse_plot, y = nse_plot1, by = "run.ID")

plotx <- ggplot() +
  geom_point(data = nse_plot, aes(x = run.ID, y = Inf_NSE)) + 
  geom_point(data = nse_plot, aes(x = (run.ID+0.5), y = NSE_CN), color = "red") +
  #geom_point(data = nse_plot, aes(x = run.ID, y = rainInt_m_s), color = "blue") +
  labs(x = "Run ID", y = "NSE (black = Infiltration KS), red = CN)", title = "Optimalization") +
  theme_minimal()+
  ylim(0.25,1)+
  facet_grid(nse_plot$initial.cond. ~.)

print(plotx)

# Filtrace: řádky, kde alespoň jedna hodnota je pod 0.25
both_good <- nse_plot %>%
  filter(Inf_NSE >= 0.25 & NSE_CN >= 0.25)
both_goodRuns = unique(both_good$run.ID)
srcDTARuns = unique(srcDTA$run.ID)



# Filtrace: řádky, kde alespoň jedna hodnota je pod 0.25
filtered_nse_plot <- nse_plot %>%
  filter(Inf_NSE < 0.25 | NSE_CN < 0.25)

# 1) Obě hodnoty pod 0.25
both_below <- filtered_nse_plot %>%
  filter(Inf_NSE < 0.25 & NSE_CN < 0.25) %>%
  #slice_head(n = 3) %>%
  select(run.ID)

# 🥈 2) Pouze `nse1` pod 0.25
only_KS_below <- filtered_nse_plot %>%
  filter(Inf_NSE < 0.25 & NSE_CN >= 0.25) %>%
  #slice_head(n = 3) %>%
  select(run.ID)

#  3) Pouze `nse2` pod 0.25
only_CN_below <- filtered_nse_plot %>%
  filter(NSE_CN < 0.25 & Inf_NSE >= 0.25) %>%
  #slice_head(n = 3) %>%
  select(run.ID)

#Vystupy
cat("🔵 ID s oběma NSE < 0.25:\n")
print(unique(both_below$run.ID))

cat("\n🟢 ID s pouze nse1 < 0.25:\n")
print(unique(only_KS_below$run.ID))

cat("\n🟠 ID s pouze nse2 < 0.25:\n")
print(unique(only_CN_below$run.ID))

fallplot <- ggplot() +
  geom_point(data = filtered_nse_plot, aes(x = year, y = locality, colour =simulator.ID)) + 
  #geom_point(data = filtered_nse_plot, aes(x = (run.ID+0.5), y = NSE_CN), color = "red") +
  #geom_point(data = nse_plot, aes(x = run.ID, y = rainInt_m_s), color = "blue") +
  labs(x = "Run ID", y = "NSE (black = Infiltration KS), red = CN)", title = "Optimalization") +
  theme_minimal()+
  #ylim(0.25,1)+
  facet_grid(filtered_nse_plot$initial.cond. ~.)

print(fallplot)

fall_size <- filtered_nse_plot %>%
  group_by(year, locality, simulator.ID, initial.cond.) %>%
  summarise(count = n(), .groups = "drop")

fallplot_size <- ggplot(fall_size) +
  geom_point(aes(x = year, y = locality, colour = as.character(simulator.ID), size = count)) + 
  labs(
    x = "Year",
    y = "Locality",
    title = "Optimization (Size = Number of Rows)"
  ) +
  theme_minimal() +
  facet_grid(initial.cond. ~ .) +  # Používání dat přímo z plot_data
  scale_size_continuous(name = "Count")  # Legenda velikosti bodů
plot(fallplot_size)

