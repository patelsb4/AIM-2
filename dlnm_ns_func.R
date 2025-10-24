library(lubridate)
library(dplyr)
library(ggplot2)
library(dlnm)
library(splines)
library(mvmeta)
library(survival)
library(data.table)

#-----------------------------------------------------------
# FUNCTION: run_dlnm_casecross
#-----------------------------------------------------------
run_dlnm_casecross_ns <- function(data,
                               temp_var = "temp.mean",
                               lag_days = 7,
                               exp_probs,
                               centering = "median",
                               plot_name) {
  
  #-------------------------------
  # Prepare exposure variable
  #-------------------------------
  temp <- data[[temp_var]]
  exp_knots <- quantile(temp, probs = exp_probs, na.rm = TRUE)
  perc <- quantile(temp, probs = c(0.90, 0.95, 0.975, 0.99), na.rm = TRUE)
  
  #-------------------------------
  # Define crossbasis
  #-------------------------------
  cb <- crossbasis(temp,
                   lag = lag_days,
                   argvar = list(fun = "ns", 
                                 knots = exp_knots),
                   arglag = list(fun = "ns", 
                                 knots = logknots(lag_days, 2)))
  
  #-------------------------------
  # Fit conditional logistic regression
  #-------------------------------
  
  model <- clogit(case ~ cb + holiday + strata(claim_id), data = data)
  
  #-------------------------------
  # Identify centering value (MMT)
  #-------------------------------
  if (centering == "median") {
    cen_temp <- median(temp, na.rm = TRUE)
  } else {
    cen_temp <- centering
  }
  
  #-------------------------------
  # Predict overall exposure-response and lag effects
  #-------------------------------
  cp_overall <- crosspred(cb, model,
                          cen = cen_temp,
                          at = seq(min(temp, na.rm = TRUE),
                                   max(temp, na.rm = TRUE),
                                   length = 100))
  
  cp_perc <- crosspred(cb, model,
                       cen = cen_temp,
                       at = perc,
                       bylag = 0.2)
  
  #-------------------------------
  # Combine RR and 95% CI for summary
  #-------------------------------
  cumulativeRR <- data.frame(
    Percentile = names(perc),
    Temperature = as.numeric(perc),
    RR = sprintf(cp_perc$allRRfit, fmt = '%#.2f'),
    CI = paste0("(", sprintf(cp_perc$allRRlow, fmt = '%#.2f'), ", ",
                sprintf(cp_perc$allRRhigh, fmt = '%#.2f'), ")")
  )
  
  #-------------------------------
  # AIC and MMT
  #-------------------------------
  aic_val <- AIC(model)
  mmt_index <- which.min(cp_overall$allRRfit)
  mmt <- cp_overall$predvar[mmt_index]
  
  #-------------------------------
  # Plots
  #-------------------------------
  png(plot_name, width = 1200, height = 800, res = 150) 
  par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)
  
  ## Exposure-response curve
  plot(cp_overall, "overall",
       col = 1,
       ylab = "OR",
       xlab = "Average Temperature (°F)",
       axes = TRUE,
       lwd = 1.5,
       log = "y",
       main = "Exposure-Response: Extreme Heat and ED Visits", cex.main = 0.8)
  abline(v = perc, col = "red", lty = 2)
  mtext(paste0("Ref = ", round(cen_temp, 1), "°F"), cex = 0.6)
  
  ## Lag-response at 90th percentile
  plot(cp_perc, "slices",
       var = as.numeric(perc["99%"]),
       col = 1,
       ylab = "OR",
       xlab = "Lag (days)",
       lwd = 1.5,
       main = paste0("Lag-Response (", round(perc["99%"], 1), "°F)"), cex.main = 0.8)
  mtext(paste0("Ref = ", round(cen_temp, 1), "°F"), cex = 0.6)
  
  dev.off()
  
  #-------------------------------
  # Return summary
  #-------------------------------
  list(
    model = model,
    AIC = aic_val,
    MMT = mmt,
    Knots = exp_knots,
    Cumulative_RR = cumulativeRR
  )
}



