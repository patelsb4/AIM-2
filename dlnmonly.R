library(lubridate)
library(tidyverse)
library(dlnm)
library(splines)
library(mvmeta)
library(survival)

source('Code/edvisits2.R')
setwd("Y:/patels")

edpregpp$case=1
edpregpp$dateonly=edpregpp$claim_date
edpregpp.temp = edpregpp  %>%
 left_join(daily.temp.all, by='dateonly')


counts <- edpregpp.temp %>%
  #filter(preg==1) %>%
  group_by(dateonly) %>%
  summarise(
    case = sum(case, na.rm = TRUE),
    tmax   = mean(temp.max, na.rm = TRUE),
    tmean   = mean(temp.mean, na.rm = TRUE),   
    tmin   = mean(temp.min, na.rm = TRUE),   
    .groups = "drop"
  )

holiday_dates <- as.Date(c(
  # Memorial Day (last Monday of May)
  "2016-05-30", "2017-05-29", "2018-05-28", "2019-05-27",
  # Independence Day
  "2016-07-04", "2017-07-04", "2018-07-04", "2019-07-04",
  # Labor Day (first Monday of September)
  "2016-09-05", "2017-09-04", "2018-09-03", "2019-09-02"
))

counts <- counts %>% 
  mutate(holiday = if_else(dateonly %in% holiday_dates, 1, 0))


counts$month=month(counts$dateonly);counts$year=year(counts$dateonly)
counts$dow=wday(counts$dateonly)

# FUNCTION TO COMPUTE THE Q-AIC IN QUASI-POISSON MODELS
fqaic <- function(model) {
  loglik <- sum(dpois(model$y,model$fitted.values,log=TRUE))
  phi <- summary(model)$dispersion
  qaic <- -2*loglik + 2*summary(model)$df[3]*phi
  return(qaic)
}

run_dlnm <- function(data=counts,
                     temp=counts$tmean,
                     knots=c(0.33,0.66),
                     plot_name) {
                     
(perc <- quantile(temp,probs=c(0.90,0.95, 0.975, 0.99)))
exp_knots <- quantile(temp,probs=knots, na.rm = TRUE)

#cross basis 
cb <- crossbasis(temp,
                 lag=7,
                 argvar=list(fun = "ns", 
                             knots = exp_knots),
                 #  argvar=list(fun="ns", knots=exp_knots),
                 arglag = list(fun = "ns", 
                               knots = logknots(7, 2)))


# RUN THE MODEL AND OBTAIN PREDICTIONS
model <- glm(case~cb +factor(year) + factor(month) + factor(dow) + holiday, data,family=quasipoisson,na.action="na.exclude")

#summary(model)

#cross prediction is making predictions based on the output from the model 
#RR at percentiles in ref to centering value (20C)
cen=median(temp)
cp_overall <- crosspred(cb, model,
                        cen = cen,
                        at = seq(min(temp, na.rm = TRUE),
                                 max(temp, na.rm = TRUE),
                                 length = 100))

cp_perc <- crosspred(cb, model,
                     cen = cen,
                     at = perc,
                     bylag = 0.2)


#pasting RR
cumulativeRR <- data.frame(
  Percentile = names(perc),
  Temperature = as.numeric(perc),
  AIC = fqaic(model),
  RR = sprintf(cp_perc$allRRfit, fmt = '%#.2f'),
  CI = paste0("(", sprintf(cp_perc$allRRlow, fmt = '%#.2f'), ", ",
              sprintf(cp_perc$allRRhigh, fmt = '%#.2f'), ")")
)
print(cumulativeRR) 


#-------------------------------


png(filename=plot_name, width = 1200, height = 800, res = 150) 
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)

## Exposure-response curve
plot(cp_overall, "overall",
     col = 1,
     ylab = "RR",
     xlab = "Average Temperature (°F)",
     axes = TRUE,
     lwd = 1.5,
     log = "y",
     main = "Exposure-Response: Extreme Heat and ED Visits", cex.main = 0.8)
abline(v = perc, col = "red", lty = 2)
mtext(paste0("Ref = ", round(cen, 1), "°F"), cex = 0.6)

## Lag-response at 90th percentile
plot(cp_perc, "slices",
     var = as.numeric(perc["99%"]),
     col = 1,
     ylab = "OR",
     xlab = "Lag (days)",
     lwd = 1.5,
     main = paste0("Lag-Response (", round(perc["99%"], 1), "°F)"), cex.main = 0.8)
mtext(paste0("Ref = ", round(cen, 1), "°F"), cex = 0.6)

dev.off()
}





run_dlnm(data=counts,
         temp=counts$tmean,
         knots=c(0.33,0.66),
         plot_name='Plots/dlnm_all_3366' ) 



run_dlnm(data=counts,
         temp=counts$tmean,
         knots=c(0.75),
         plot_name='Plots/dlnm_all_75' ) 


run_dlnm(data=counts,
         temp=counts$tmean,
         knots=c(0.25, 0.50, 0.75),
         plot_name='Plots/dlnm_all_255075' ) 

run_dlnm(data=counts,
         temp=counts$tmean,
         knots=c(0.50,0.75),
         plot_name='Plots/dlnm_all_2575' ) 


run_dlnm(data=counts,
         temp=counts$tmean,
         knots=c(0.90),
         plot_name='Plots/dlnm_all_90' ) 

#################################
################################
################################
###############################


counts_preg <- edpregpp.temp %>%
  filter(preg==1) %>%
  group_by(dateonly) %>%
  summarise(
    case = sum(case, na.rm = TRUE),
    tmax   = mean(temp.max, na.rm = TRUE),
    tmean   = mean(temp.mean, na.rm = TRUE),   
    tmin   = mean(temp.min, na.rm = TRUE),   
    .groups = "drop"
  )

holiday_dates <- as.Date(c(
  # Memorial Day (last Monday of May)
  "2016-05-30", "2017-05-29", "2018-05-28", "2019-05-27",
  # Independence Day
  "2016-07-04", "2017-07-04", "2018-07-04", "2019-07-04",
  # Labor Day (first Monday of September)
  "2016-09-05", "2017-09-04", "2018-09-03", "2019-09-02"
))

counts_preg <- counts_preg %>% 
  mutate(holiday = if_else(dateonly %in% holiday_dates, 1, 0))


counts_preg$month=month(counts_preg$dateonly);counts_preg$year=year(counts_preg$dateonly)
counts_preg$dow=wday(counts_preg$dateonly)


run_dlnm(data=counts_preg,
         temp=counts_preg$tmean,
         knots=c(0.33,0.66),
         plot_name='Plots/dlnm_preg_3366' ) 



run_dlnm(data=counts_preg,
         temp=counts_preg$tmean,
         knots=c(0.75),
         plot_name='Plots/dlnm_pregl_75' ) 


run_dlnm(data=counts_preg,
         temp=counts_preg$tmean,
         knots=c(0.25, 0.50, 0.75),
         plot_name='Plots/dlnm_preg_255075' ) 

run_dlnm(data=counts_preg,
         temp=counts_preg$tmean,
         knots=c(0.50,0.75),
         plot_name='Plots/dlnm_preg_2575' ) 


run_dlnm(data=counts_preg,
         temp=counts_preg$tmean,
         knots=c(0.90),
         plot_name='Plots/dlnm_preg_90' ) 



#################################
################################
################################
###############################


counts_pp <- edpregpp.temp %>%
  filter(postpartum==1) %>%
  group_by(dateonly) %>%
  summarise(
    case = sum(case, na.rm = TRUE),
    tmax   = mean(temp.max, na.rm = TRUE),
    tmean   = mean(temp.mean, na.rm = TRUE),   
    tmin   = mean(temp.min, na.rm = TRUE),   
    .groups = "drop"
  )

holiday_dates <- as.Date(c(
  # Memorial Day (last Monday of May)
  "2016-05-30", "2017-05-29", "2018-05-28", "2019-05-27",
  # Independence Day
  "2016-07-04", "2017-07-04", "2018-07-04", "2019-07-04",
  # Labor Day (first Monday of September)
  "2016-09-05", "2017-09-04", "2018-09-03", "2019-09-02"
))

counts_pp <- counts_pp %>% 
  mutate(holiday = if_else(dateonly %in% holiday_dates, 1, 0))


counts_pp$month=month(counts_pp$dateonly);counts_pp$year=year(counts_pp$dateonly)
counts_pp$dow=wday(counts_pp$dateonly)


run_dlnm(data=counts_pp,
         temp=counts_pp$tmean,
         knots=c(0.33,0.66),
         plot_name='Plots/dlnm_pp_3366' ) 



run_dlnm(data=counts_pp,
         temp=counts_pp$tmean,
         knots=c(0.75),
         plot_name='Plots/dlnm_pp_75' ) 


run_dlnm(data=counts_pp,
         temp=counts_pp$tmean,
         knots=c(0.25, 0.50, 0.75),
         plot_name='Plots/dlnm_pp_255075' ) 

run_dlnm(data=counts_pp,
         temp=counts_pp$tmean,
         knots=c(0.50,0.75),
         plot_name='Plots/dlnm_pp_2575' ) 


run_dlnm(data=counts_pp,
         temp=counts_pp$tmean,
         knots=c(0.90),
         plot_name='Plots/dlnm_pp_90' ) 


