
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(GA)  # Genetic Algorithm library
library(hydroGOF)
library(doParallel)


#num_cores = detectCores() - 2




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
    !is.na(interval.duration) | 
    !is.na(runoff) | 
    !is.na(rainfall.total..mm.) | 
    !is.na(rain.intensity..mm.h.1.) | 
    !data_combined$t1_hour <= 0) #|
    #!data_combined$CC_int_time_sec <= 0)
  
data_combined <- data_combined[!is.na(data_combined$total.discharge..l.), ]
data_combined <- data_combined[!is.na(data_combined$dt_t_form), ]


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
  #ifelse(time >= 0,return(0)
  return((0.1*S / (2 * sqrt(Ti))) + K)
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




lower_bounds_dry <- c(1*10^-8,4*10^-5) # Lower bounds for Sorptivity (S) and Hydraulic Conductivity (K [m/s])
upper_bounds_dry <- c(1*10^-4, 1*10^-2) # Upper bounds for Sorptivity (S) and Hydraulic Conductivity (K)
# Run the Genetic Algorithm to optimize S and K
lower_bounds_wet <- c(1*10^-8, 4*10^-15) # Lower bounds for Sorptivity (S) and Hydraulic Conductivity (K)
upper_bounds_wet <- c(1*10^-4, 4*10^-4) # Upper bounds for Sorptivity (S) and Hydraulic Conductivity (K)
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
#cl <-makeCluster(num_cores)

#clusterExport(cl = cl, varlist = c("objective_function", "philip_model"))






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
    #clusterExport(cl, varlist = c("objective_function"))
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
      #parallel = cl,
      optimArgs = list(method = "L-BFGS-B", 
                       poptim = 0.1,
                       pressel = 0.25,
                       control = list(fnscale = -1, maxit = 1000)),
      optim = TRUE
      
    )
    #stopCluster(cl)
  }
  
  
  , error = function(e) {
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


#stopCluster(cl)

# Extract the optimal parameters from the GA result

results_df_to_merge =  best_solutions_df
data_combined = merge(data_combined, results_df_to_merge, by = "run.ID")
data_combined$optimazedInf_mm = philip_model(params = c(data_combined$best_K, data_combined$best_S), Ti = data_combined$t1_sec)
data_combined$xx =  ((0.1*data_combined$best_S / (2 * sqrt(data_combined$t1_sec))) + data_combined$best_K)
data_combined$optimazedInf_mm = data_combined$xx
data_combined$optimazedTotInf_m3 = data_combined$optimazedInf_mm*data_combined$CC_int_time_sec*data_combined$area


data_combined <- data_combined %>%
  group_by(run.ID) %>%
  arrange(t2) %>%  # Order rows by t2 within each run.ID group
  mutate(cumulative_optimazedTotInf_m3 = cumsum(optimazedTotInf_m3)) %>%
  ungroup()
# Calculate NSE for each run.ID group
nse_results <- data_combined %>%
  group_by(run.ID) %>%
  summarize(Inf_NSE = hydroGOF::NSE(cumulative_optimazedTotInf_m3, CC_Inf_m3), .groups = 'drop')

# Join the NSE results back to the original dataframe
data_combined <- left_join(data_combined, nse_results, by = "run.ID")
ready = data_combined[data_combined$Inf_NSE>0.25,]
zzz = data_combined[,c(1,5,12,49, 50, 51, 52, 54,55,56,57,58,59,60,61, 53)]


# Calculate NSE for each run.ID group, if you haven't already
nse_plot <- data_combined %>%
  group_by(run.ID, initial.cond.) %>%
  summarize(Inf_NSE = hydroGOF::NSE(cumulative_optimazedTotInf_m3, CC_Inf_m3), .groups = 'drop')

# Plot the data with coloring by initial.cond

ggplot(nse_plot, aes(x = run.ID, y = Inf_NSE, color = initial.cond.)) +
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
  
write.csv(data_combined, "K_S_optimalization.csv")
#plotx <- ggplot() +
 # geom_point(data = data_combined, aes(x = t1_hour, y = data_combined$cumulative_optimazedTotInf, color  = "red")) + 
#  geom_point(data = data_combined, aes(x = t1_hour, y = data_combined$CC_Rain_m3), color = "gray") +
 # geom_point(data = data_combined, aes(x = t1_hour, y = data_combined$CC_Runoff_m3), color = "blue") +
  #geom_point(data = data_combined, aes(x = t1_hour, y = data_combined$CC_Inf_m3), color = "green") +
#  labs(x = "Time (hours)", y = "Infiltration Intensity (m/s)", title = "Comparison of Optimized and Measured Infiltration Intensity") +
 # theme_minimal()+
  #ylim(0,0.06)+
  #facet_grid(data_combined$run.ID ~ .)

#plot(plotx)



########################NIC
#data_combinedCN =  read.csv(file = "K_S_optimalization.csv",sep = ",",fileEncoding = "UTF-8")
pyGA = read.csv(file = "optiamalized_K_S_CN_Ia.csv")
plot(pyGA$nse_CN, ylim = c(0,1))
pyGA$OptimA =  25.4 * (1000/pyGA$best_CN-10)
pyGA$Ia = pyGA$OptimA * pyGA$best_Ia
pyGA$OptimHe_mm = (pyGA$rainfall.total..mm. - pyGA$Ia)^2/(pyGA$rainfall.total..mm. - pyGA$Ia + pyGA$OptimA)
pyGA$OptimHe_mm <- ifelse(pyGA$Ia > pyGA$rainfall.total..mm., 0, pyGA$OptimHe_mm)
pyGA$CC_Opti_Runoff_m3 = pyGA$OptimHe_mm * pyGA$area/1000

pyGAnse = pyGA[pyGA$nse_CN>0,]
pyGAnse = pyGAnse[pyGAnse$Inf_NSE>0,]

unique(pyGAnse$run.ID)

plotx <- ggplot(data = pyGAnse, aes(x = pyGAnse$Inf_NSE, y = pyGAnse$nse_CN, color  = pyGAnse$initial.cond.)) +
  geom_violin()+
  geom_boxplot(width=0.1)+
  labs(x = "Time (hours)", y = "Infiltration Intensity (m/s)", title = "Comparison of Optimized and Measured Infiltration Intensity") +
  #ylim(0,0.06)+
  facet_grid(pyGAnse$initial.cond. ~ .)+
  theme_minimal()

plot(plotx)
plotx <- ggplot() +
  
  geom_boxplot(data = pyGAnse, aes(y = pyGAnse$Inf_NSE, color  = pyGAnse$initial.cond.))+
  geom_boxplot(data = pyGAnse, aes(x = pyGAnse$nse_CN))+
  labs(x = "Time (hours)", y = "Infiltration Intensity (m/s)", title = "Comparison of Optimized and Measured Infiltration Intensity") +
  #ylim(0,0.06)+
  facet_grid(pyGAnse$initial.cond. ~ .)+
  theme_minimal()

plot(plotx)


pyGAnse = pyGA
plot(pyGAnse$CC_Runoff_m3, pyGAnse$CC_Opti_Runoff_m3)# ylim = c(0,1))

plotx <- ggplot() +
 geom_point(data = pyGAnse, aes(x = t1_hour, y = pyGAnse$cumulative_optimazedTotInf, color  = "red")) + 
  geom_point(data = pyGAnse, aes(x = t1_hour, y = pyGAnse$CC_Rain_m3), color = "gray") +
 geom_point(data = pyGAnse, aes(x = t1_hour, y = pyGAnse$CC_Runoff_m3), color = "blue") +
geom_point(data = pyGAnse, aes(x = t1_hour, y = pyGAnse$CC_Inf_m3), color = "green") +
  labs(x = "Time (hours)", y = "Infiltration Intensity (m/s)", title = "Comparison of Optimized and Measured Infiltration Intensity") +
#ylim(0,0.06)+
facet_grid(pyGAnse$initial.cond. ~ .)+
  theme_minimal()
  
plot(plotx)

plotx <- ggplot() +
 geom_point(data = pyGAnse, aes(pyGAnse$Inf_NSE, y = pyGAnse$nse_CN, color  = "red")) + 
  #geom_point(data = pyGAnse, aes(x = t1_hour, y = pyGAnse$Inf_NSE), color = "gray") +
# geom_point(data = pyGAnse, aes(x = t1_hour, y = pyGAnse$CC_Runoff_m3), color = "blue") +
#geom_point(data = pyGAnse, aes(x = t1_hour, y = pyGAnse$CC_Inf_m3), color = "green") +
  labs(x = "Time (hours)", y = "Infiltration Intensity (m/s)", title = "Comparison of Optimized and Measured Infiltration Intensity") +
ylim(0,1)+
xlim(0,1)+
facet_grid(pyGAnse$initial.cond. ~ .)+
  theme_minimal()
  
plot(plotx)



data_combinedCN = data_combined


CNmodel <- function(params, Hrain_mm) {
    CN <- params[1]
    rIa <- params[2]
  browser()
    A = 25.4 * (1000/CN-10)
    Ia = A*rIa
    He_mm <- ifelse(Ia > Hrain_mm, 0, (Hrain_mm - Ia)^2 / (Hrain_mm - Ia + A))
    
    return(He_mm)
  #)
  
}


CNobjective_function <- function(params) {
  #browser()
  CN <- params[1]
  rIa <- params[2]
  area = subset_data$area
  Hrain_mm <- subset_data$CC_Rain_m3/area*1000
  dTi <- subset_data$CC_int_time_sec
  
  He_mm <- CNmodel(params, Hrain_mm)
  #browser()
  CC_CN_modeled_runoff_m3 = cumsum(He_mm*dTi*area)/1000
 
  nse_value <- hydroGOF::NSE(CC_CN_modeled_runoff_m3, subset_data$CC_Runoff_m3)
  #print(paste("NSE:", nse_value, CN, rIa))
  residuals <- subset_data$CC_Runoff_m3 - CC_CN_modeled_runoff_m3
#    plot(x =  subset_data$t1_t_form, y = subset_data$CC_Rain_m3)
 #  points(x =  subset_data$t1_t_form, y = subset_data$CC_Runoff_m3, col = "blue")
  #points(x =  subset_data$t1_t_form, y = CC_modeled_inf_m3, col = "red")
  #points(x =  subset_data$t1_t_form, y = subset_data$CC_Inf_m3, col = "green")
  #plot(x =  subset_data$t1_t_form, y = residuals, col = "green")
  sumres = (sum(residuals^2))
  #nic <<- append(nic, sumres)
  #browser()
  #return(sumres)
  return(nse_value)
}


CNlower_bounds_dry <- c(0,0.2) # Lower bounds for Sorptivity (S) and Hydraulic Conductivity (K [m/s])
CNupper_bounds_dry <- c(80, 0.4) # Upper bounds for Sorptivity (S) and Hydraulic Conductivity (K)
# Run the Genetic Algorithm to optimize S and K
CNlower_bounds_wet <- c(0, 0.2) # Lower bounds for Sorptivity (S) and Hydraulic Conductivity (K)
CNupper_bounds_wet <- c(99, 0.4) # Upper bounds for Sorptivity (S) and Hydraulic Conductivity (K)
nic = c()
CNresults_df = data.frame()
# Initialize an empty data frame to store the best solutions
best_solutions_dfCN <- data.frame(
  #crop = character(),
  #initial_cond = character(),
  run.ID = numeric(),
  best_CN = numeric(),
  best_Ia = numeric(),
  best_fitnessCN = numeric(),
  nse_CN = numeric(),
  stringsAsFactors = TRUE
)
CNunique_run_ids <- sort(unique(data_combinedCN$run.ID))
# Loop through the combinations
for (CNxID in CNunique_run_ids) {
#for (CNxID in 3:4) {
  #if (unique_combinations[xID, "initial.cond."] != "very wet") {
  #  next  # Skip this iteration if it's not "very wet"
  #}
  
  # Subset data for the current unique combination
  #xxID <- as.integer(unique_combinations[xID, 3])
  subset_data <- data_combinedCN[data_combinedCN$run.ID == CNxID,]
  
  print(paste("Start for combination:", CNxID, nrow(subset_data), subset_data$locality[1]))
  
  # Set bounds based on initial condition
  if (subset_data$initial.cond.[1] == "dry") {
    lower_bounds <- CNlower_bounds_dry
    upper_bounds <- CNupper_bounds_dry
  } else {
    lower_bounds <- CNlower_bounds_wet
    upper_bounds <- CNupper_bounds_wet
  }
  
  # Initialize population
  initial_population <- matrix(nrow = 200, ncol = length(lower_bounds))
  #for (rdm in 1:500) {
  #  initial_population[rdm, ] <- lower_bounds + runif(length(lower_bounds), 0, 1) * (upper_bounds - lower_bounds)
  #}
  
  initial_population[,1] <- runif(200, lower_bounds[1], upper_bounds[1])
  initial_population[,2] <- runif(200, lower_bounds[2], upper_bounds[2])
  # Run the GA with tryCatch for error handling
  ga_result <- tryCatch({
    ga(
      type = "real-valued",
      #fitness = function(params) - CNobjective_function(params),
      fitness = function(params) CNobjective_function(params),
      lower = lower_bounds,
      upper = upper_bounds,
      popSize = 200,
      maxiter = 1000,
      run = 500,
      seed = 123,
      suggestions = initial_population,
      pmutation = 0.02,
      pcrossover = 0.8,
      monitor = FALSE,
      #optimArgs = list(method = "L-BFGS-B", 
       #                poptim = 0.1,
        #               pressel = 0.25,
         #              control = list(fnscale = -1, maxit = 1000)),
      optim = TRUE
      
    )
  }, error = function(e) {
    print(paste("Error in GA for combination", CNxID, ":", e$message))
    return(NULL)
  })
  
  # If GA was successful, save the best solution
  #browser()
  if (!is.null(ga_result)) {
    xoptimal_CNIa <- if (!is.null(ga_result@solution)) colMeans(ga_result@solution) else c(NA, NA)
    xbest_fitness <- if (!is.null(ga_result@fitnessValue)) ga_result@fitnessValue else NA
    xCC_CN_modeled_He = cumsum(CNmodel(params = xoptimal_CNIa, subset_data$CC_Rain_m3/subset_data$area*1000))
    xCC_CN_modeled_runoff_m3 = cumsum(xCC_CN_modeled_He*subset_data$area)/1000 
    xnse_value <- hydroGOF::NSE(xCC_CN_modeled_runoff_m3, subset_data$CC_Runoff_m3)
    #browser()
    # Append the best solution and fitness to the data frame
    best_solutions_dfCN <- rbind(best_solutions_dfCN, data.frame(
      #crop = unique_combinations[xID, 1], 
      #initial_cond = unique_combinations[xID, 2], 
      run.ID = CNxID,
      best_CN = xoptimal_CNIa[1],
      best_Ia = xoptimal_CNIa[2],
      best_fitness = xbest_fitness, 
      nse_CN = xnse_value
    ))
    #browser()
    
    
    
    
  } else {
    print(paste("Skipping combination", CNxID, "due to GA failure"))
    
    # Append NA values to indicate a failure in GA for this combination
    
    best_solutions_dfCN <- rbind(best_solutions_dfCN, data.frame(
      #crop = unique_combinations[xID, 1], 
      #initial_cond = unique_combinations[xID, 2], 
      run.ID = CNxID,
      best_S = NA,
      best_K = NA,
      best_fitness = NA
      
    ))
    
  }
  
}

dd = c(79.72, 0,2)

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
