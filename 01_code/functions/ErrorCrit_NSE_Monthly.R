# A simplified and robust monthly NSE function
ErrorCrit_NSE_Monthly <- function(InputsCrit, OutputsModel, warnings = FALSE, verbose = FALSE, ...) {
  # Handle outputs directly without relying on external variables
  tryCatch({
    # Extract simulated values
    Qsim_daily <- OutputsModel$Qsim
    
    # Debugging
    if (verbose) {
      cat("Length of Qsim_daily:", length(Qsim_daily), "\n")
      if (length(Qsim_daily) > 0) {
        cat("Sample of Qsim_daily:", head(Qsim_daily), "\n")
      } else {
        cat("Qsim_daily is empty\n")
      }
    }
    
    # Safely get dates
    period_indices <- InputsCrit$RunOptions$IndPeriod_Run
    if (length(period_indices) == 0) {
      if (verbose) cat("period_indices is empty\n")
      # Return NA if we don't have any indices
      OutputsCrit <- list(CritValue = NA, CritName = "NSE_Monthly", CritBestValue = -1)
      return(OutputsCrit)
    }
    
    # Get a sequence of dates - create synthetic dates if needed
    # Don't rely on external variables - generate dates based on length
    start_date <- as.Date("2000-01-01")  # Default start date
    dates <- seq(from = start_date, by = "day", length.out = length(Qsim_daily))
    
    # Create dataframe and aggregate to monthly
    df_sim <- data.frame(
      date = dates,
      Qsim = Qsim_daily
    )
    
    # Add month column
    df_sim$month <- floor_date(df_sim$date, "month")
    
    # Aggregate to monthly
    df_sim_monthly <- aggregate(
      Qsim ~ month, 
      data = df_sim, 
      FUN = mean, 
      na.rm = TRUE
    )
    
    # Get monthly observations
    Qobs_monthly <- InputsCrit$Obs
    
    # Debugging
    if (verbose) {
      cat("Length of Qobs_monthly:", length(Qobs_monthly), "\n")
      if (length(Qobs_monthly) > 0) {
        cat("Sample of Qobs_monthly:", head(Qobs_monthly), "\n")
      } else {
        cat("Qobs_monthly is empty\n")
      }
    }
    
    # Create observation data frame - use first n months from simulation data
    n_obs <- length(Qobs_monthly)
    if (n_obs == 0) {
      if (verbose) cat("No observations available\n")
      # Return NA if no observations
      OutputsCrit <- list(CritValue = NA, CritName = "NSE_Monthly", CritBestValue = -1)
      return(OutputsCrit)
    }
    
    # Limit to available months
    n_months <- min(n_obs, nrow(df_sim_monthly))
    if (n_months == 0) {
      if (verbose) cat("No overlapping months between sim and obs\n")
      # Return NA if no overlapping months
      OutputsCrit <- list(CritValue = NA, CritName = "NSE_Monthly", CritBestValue = -1)
      return(OutputsCrit)
    }
    
    # Subset data to matching length
    df_sim_subset <- df_sim_monthly[1:n_months, ]
    Qobs_subset <- Qobs_monthly[1:n_months]
    
    # Calculate NSE
    Qsim_values <- df_sim_subset$Qsim
    Qobs_values <- Qobs_subset
    
    # Apply transformation if specified
    if (!is.null(InputsCrit$transfo)) {
      if (InputsCrit$transfo == "sqrt") {
        Qsim_values <- sqrt(Qsim_values)
        Qobs_values <- sqrt(Qobs_values)
      } else if (InputsCrit$transfo == "log") {
        # Safe handling of zero values
        min_pos <- min(c(Qsim_values[Qsim_values > 0], Qobs_values[Qobs_values > 0]), na.rm = TRUE)
        epsilon <- ifelse(is.finite(min_pos), min_pos / 100, 0.0001)
        Qsim_values <- log(Qsim_values + epsilon)
        Qobs_values <- log(Qobs_values + epsilon)
      }
    }
    
    # Calculate NSE
    moy_Qobs <- mean(Qobs_values, na.rm = TRUE)
    denominator <- sum((Qobs_values - moy_Qobs)^2, na.rm = TRUE)
    
    # Avoid division by zero
    if (denominator > 0) {
      NSE <- 1 - sum((Qsim_values - Qobs_values)^2, na.rm = TRUE) / denominator
    } else {
      NSE <- NA
    }
    
    # Handle NA values
    if (is.na(NSE) || !is.finite(NSE)) {
      NSE <- -999  # Use a very poor score instead of NA
    }
    
    # Apply weights if provided
    if (!is.null(InputsCrit$Weights)) {
      NSE <- NSE * InputsCrit$Weights
    }
    
    # Return results
    OutputsCrit <- list(CritValue = -NSE, CritName = "NSE_Monthly", CritBestValue = -1)
    return(OutputsCrit)
  }, error = function(e) {
    # Return a default value on error
    if (verbose || warnings) {
      cat("Error in ErrorCrit_NSE_Monthly:", e$message, "\n")
    }
    OutputsCrit <- list(CritValue = -999, CritName = "NSE_Monthly", CritBestValue = -1)
    return(OutputsCrit)
  })
}