# GA Infiltration Model in R

library(GA)
library(dplyr)
library(hydroGOF)
# library(lubridate) removed

# Load data
data_combined <- read.csv("runoff_sediment_intervals_20240925_en.csv", sep = ";", fileEncoding = "UTF-8")

# Convert and compute columns
data_combined$TIMESTAMP <- as.POSIXct(strptime(data_combined$date, "%d.%m.%Y"))
data_combined$t1_t_form <- as.POSIXct(data_combined$t1, format = "%H:%M:%S")
data_combined$t2_t_form <- as.POSIXct(data_combined$t2, format = "%H:%M:%S")
data_combined$runoff_start_t_form <- as.POSIXct(data_combined$time.to.runoff, format = "%H:%M:%S")
data_combined$dt_t_form <- as.POSIXct(data_combined$interval.duration, format = "%H:%M:%S")

zero_time <- as.POSIXct("00:00:00", format = "%H:%M:%S")
#zero_time = format(zero_timeTform, "%H:%M:%S") 

data_combined$dt_t_form <- as.POSIXct(data_combined$interval.duration, format = "%H:%M:%S")
data_combined$tot_time_t_form = as.POSIXct(ifelse(data_combined$t2_t_form == zero_time| is.na(data_combined$t2_t_form), data_combined$runoff_start_t_form, data_combined$dt_t_form))
data_combined$month <- format(srcDTA$TIMESTAMP, "%m")

#####NE

data_combined$tot_time_t_form <- as.POSIXct(
  ifelse(is.na(data_combined$tot_time_t_form), data_combined$runoff_start_t_form, data_combined$tot_time_t_form))
data_combined$tot_time_t_form <- ifelse(is.na(data_combined$t2_t_form) | data_combined$t2_t_form == zero_time,
                                        data_combined$runoff_start_t_form,
                                        data_combined$dt_t_form)
data_combined$tot_time_t_form <- ifelse(is.na(data_combined$tot_time_t_form),
                                        data_combined$runoff_start_t_form,
                                        data_combined$tot_time_t_form)
####ANO
# Cover classification
data_combined$cover <- ifelse(data_combined$crop %in% c("cultivated fallow", "bare soil"), "bare",
                              ifelse(data_combined$crop %in% c("Geotextile Macmat 8.1", "Geotextile Enkamat 7010", "Geotextile K700",
                                                               "Geotextile Biomac-c", "Geotextile Enkamat 7020", "Geotextile Macmat 18.1",
                                                               "Macmat 18 fill", "Jute", "Triangle", "Enkamat 7020 filled",
                                                               "Fortrac 3D filled", "Fortrac 3D"), "geotex", "vege"))

# Additional calculated columns
data_combined$area <- data_combined$plot.length..m. * data_combined$plot.width..m.
data_combined$runoff <- as.numeric(data_combined$flow.rate..l.min.1.)
data_combined$runoffhighMM <- data_combined$total.discharge..l. / data_combined$area
data_combined$crop <- ifelse(is.na(data_combined$crop), "Unknown", data_combined$crop)
data_combined$rain.intensity..mm.h.1. <- as.numeric(data_combined$rain.intensity..mm.h.1.)
data_combined$soilloss <- as.numeric(data_combined$SS.flux..g.min.1.)
data_combined$slope <- data_combined$plot.slope.... / 100

data_combined$BBCH <- ifelse(data_combined$cover == "bare" & is.na(data_combined$BBCH), 0,
                             ifelse(data_combined$cover == "geotex", 50, data_combined$BBCH))
data_combined$C <- ifelse(is.na((100 - data_combined$BBCH) / 100), 0.95,
                          (100 - data_combined$BBCH) / 100)

data_combined$t1_sec <- as.numeric(format(data_combined$t1_t_form, "%H")) * 3600 +
  as.numeric(format(data_combined$t1_t_form, "%M")) * 60 +
  as.numeric(format(data_combined$t1_t_form, "%S"))

data_combined$CC_int_time_sec <- as.numeric(format(data_combined$tot_time_t_form, "%H")) * 3600 +
  as.numeric(format(data_combined$tot_time_t_form, "%M")) * 60 +
  as.numeric(format(data_combined$tot_time_t_form, "%S"))

data_combined$CC_Rain_m3 <- data_combined$rainfall.total..mm. / 1000 * data_combined$area
data_combined$CC_Runoff_m3 <- data_combined$total.discharge..l. / 1000
data_combined$CC_Inf_m3 <- pmax(data_combined$CC_Rain_m3 - data_combined$CC_Runoff_m3, 0)
data_combined$CC_control <- data_combined$CC_Rain_m3 - data_combined$CC_Runoff_m3

# Filter valid records
data_combined <- data_combined[!is.na(data_combined$soilloss) &
                                 !is.na(data_combined$dt_t_form) &
                                 !is.na(data_combined$runoff) &
                                 !is.na(data_combined$rainfall.total..mm.) &
                                 !is.na(data_combined$rain.intensity..mm.h.1.) &
                                 !is.na(data_combined$total.discharge..l.) &
                                 data_combined$t1_sec > 0 &
                                 data_combined$soilloss >= 0 &
                                 data_combined$CC_control >= 0, ]





# Philip infiltration model
philip_model <- function(K, S, Ti) {
  S <- ifelse(S >= 0, S, 0)
  (S / (2 * sqrt(Ti))) + K
}

# Surface water balance
BilanSurface <- function(Rain, Vege, surBil, Infiltration) {
  RearRain <- pmax(0, Rain - Vege)
  Sur <- pmax(0, RearRain - Infiltration)
  pmax(0, Sur - surBil)
}

# NSE calculation
calc_nse <- function(sim, obs) {
  1 - sum((sim - obs)^2) / sum((obs - mean(obs))^2)
}

# Objective function with runoff start penalty
objective_function <- function(params) {
  K <- params[1]; S <- params[2]; Imax <- params[3]; LAI <- params[4]; RetSur <- params[5]
  area <- subset_data$area
  cumRainm3 <- subset_data$CC_Rain_m3
  rainInt <- subset_data$rain.intensity..mm.h.1.
  Ti <- subset_data$t1_sec
  dTi <- subset_data$CC_int_time_sec
  
  Imaxm3 <- Imax / 1000 * area
  RetVegm3 <- ifelse(cumRainm3 * LAI > Imaxm3, 0, cumRainm3 * LAI)
  iter0inf <- philip_model(K, S * 1.05, Ti)
  InfIter0 <- cumsum(iter0inf * dTi * area)
  RetSurm3_0 <- RetSur / 1000 * area[1]
  SruBilm3_0 <- BilanSurface(cumRainm3, RetVegm3, RetSurm3_0, InfIter0)
  RetTotal_m3 <- cumRainm3 - SruBilm3_0
  
  # runoff start match penalty
  t_runoff_start <- unique(subset_data$runoff_start_t_form)
  t_runoff_sec <- as.numeric(format(t_runoff_start, "%H")) * 3600 +
    as.numeric(format(t_runoff_start, "%M")) * 60 +
    as.numeric(format(t_runoff_start, "%S"))
  
  exceeds_Imax <- which(cumRainm3 * LAI >= Imaxm3)
  if (length(exceeds_Imax) > 0) {
    timestep_veg_limit <- Ti[exceeds_Imax[1]]
    timediv <- timestep_veg_limit - t_runoff_sec
    timeres <- timediv^2
    sim_runoff_time <- timestep_veg_limit
  } else {
    timeres <- 1e10
    sim_runoff_time <- NA
  }
  
  observed <- subset_data$CC_Inf_m3
  modeled <- RetTotal_m3
  if (length(observed) < 2 || all(observed == observed[1])) return(1e10)
  
  sumres <- sum((observed - modeled)^2) / length(SruBilm3_0) * timeres
  assign("last_sim_time", sim_runoff_time, envir = .GlobalEnv)
  assign("last_nse", calc_nse(modeled, observed), envir = .GlobalEnv)
  return(sumres)
}

# Store results
store_result <- function(ga_result, run_id, best_solutions_df) {
  if (!is.null(ga_result)) {
    sol <- ga_result@solution
    new_result <- data.frame(
      run.ID = run_id,
      best_K = sol[1], best_S = sol[2], best_Imax = sol[3],
      best_LAI = sol[4], best_RetSur = sol[5],
      best_fitness = ga_result@fitnessValue,
      NSE = last_nse,
      simulated_runoff_start_sec = last_sim_time
    )
    best_solutions_df <- rbind(best_solutions_df, new_result)
    saveRDS(list(result = ga_result, NSE = last_nse, simulated_runoff_start_sec = last_sim_time, params = sol), 
            file = paste0("GA_result_runID_", run_id, ".rds"))
  }
  return(best_solutions_df)
}

# Optimization across all IDs
best_solutions_df <- data.frame()
unique_ids <- unique(data_combined$run.ID)


lower_bounds_dry <- c(1e-7, 0, 0, 0, 2)
upper_bounds_dry <- c(1e-5, 1e-3, 1, 1, 10)
lower_bounds_wet <- c(1e-7, 0, 0, 0, 1)
upper_bounds_wet <- c(4e-5, 1e-3, 1, 1, 10)
  
for (xID in unique_run_ids) {
#for (xID in 464:464) {
#for (xID in unique_run_ids[21:21]) {
    cat("\n--- Running GA for run.ID:", xID, "---\n")
    subset_data <- data_combined[data_combined$run.ID == xID, ]
    if (nrow(subset_data) == 0) next
    
    bounds <- if (subset_data$initial.cond.[1] == "dry")
      list(lower = lower_bounds_dry, upper = upper_bounds_dry)
    else
      list(lower = lower_bounds_wet, upper = upper_bounds_wet)
    
    initial_population <- t(replicate(500, bounds$lower + runif(5, 0.2, 0.8) * (bounds$upper - bounds$lower)))
    
    ga_result <- tryCatch({
      ga(
        type = "real-valued",
        fitness = function(params) -objective_function(params),
        lower = bounds$lower,
        upper = bounds$upper,
        popSize = 500,
        maxiter = 1000,
        run = 500,
        seed = 123 + xID,
        suggestions = initial_population,
        pmutation = 0.08,
        pcrossover = 0.85,
        elitism = 5,
        optim = TRUE,
        optimArgs = list(method = "L-BFGS-B", poptim = 0.2, pressel = 0.3, control = list(fnscale = -1, maxit = 1000)),
        monitor = FALSE
      )
    }, error = function(e) {
      message("Error for run.ID ", xID, ": ", e$message)
      return(NULL)
    })
    
    if (!is.null(ga_result)) {
      sol <- ga_result@solution
      K <- sol[1]; S <- sol[2]; Imax <- sol[3]; LAI <- sol[4]; RetSur <- sol[5]
      
      area <- subset_data$area
      cumRainm3 <- subset_data$CC_Rain_m3
      rainInt <- subset_data$rain.intensity..mm.h.1.
      Ti <- subset_data$t1_sec
      dTi <- subset_data$CC_int_time_sec
      
      Imaxm3 <- Imax / 1000 * area
      RetVegm3 <- ifelse(cumRainm3 * LAI > Imaxm3, 0, cumRainm3 * LAI)
      t_shift <- ifelse(any(cumRainm3 * LAI >= Imaxm3), Ti[which(cumRainm3 * LAI >= Imaxm3)[1]], max(Ti))
      Ti_shifted <- Ti - t_shift
      inf_intensity <- ifelse(Ti_shifted <= 0, rainInt / 3600000, philip_model(K, S, pmax(1, Ti_shifted)))
      CC_modeled_inf_m3 <- cumsum(inf_intensity * dTi * area)
      SruBilm3 <- BilanSurface(cumRainm3, RetVegm3, RetSur / 1000 * area[1], CC_modeled_inf_m3)
      RetTotal_m3 <- cumRainm3 - SruBilm3
      
      NSE <- calc_nse(RetTotal_m3, subset_data$CC_Inf_m3)
      
      cat("Finished run.ID:", xID, "with NSE =", round(NSE, 4), "\n")
    }
     best_solutions_df <- store_result(ga_result, xID, best_solutions_df)}

write.csv(best_solutions_df, "best_solutions_summary.csv", row.names = FALSE)

