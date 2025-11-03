
library(haven)
library(dplyr)
library(lubridate)
library(survival)

setwd("Y:/patels")

#load('Data/edpregpp.Rdata')
#load('Data/edpregpp_all.Rdata')
source('Code/edvisits2.R')

casecross <- function(data) {
  
  
  dates<-c(unique(data$claim_date)) # date list
  
  datalist<- list()
  
  #loop through each case date to find matching control dates
  for (i in seq_along(dates)) {
    case_date <- as.Date(dates[i])
    month_start <- as.Date(paste(year(case_date), month(case_date), "01", sep = "-"))
    month_end <- as.Date(paste(year(case_date), month(case_date), days_in_month(case_date), sep = "-"))
    
    # Find all same-day-of-week (DOW) dates in that month
    all_days <- seq(month_start, month_end, by = "day")
    same_dow <- all_days[wday(all_days) == wday(case_date)]
    
    #matched control dates to case dates
    datalist[[i]]<-data.frame(
      caseedt = case_date,
      cntrldt = same_dow
    )
  }
  
  #case-control date pairs  
  controldates <- bind_rows(datalist) %>%
    # Keep only unique pairs (avoid duplicate case-control combinations)
    distinct() %>%
    mutate(
      case = ifelse(caseedt == cntrldt, 1, 0), #binary variable for case
      newdate = cntrldt                        # updated date variable based on case vs contrl day
    )
  
  #merge case/control dates with the claims dataset
  
  casecross <- controldates %>%
    left_join(data, by = c("caseedt" = "claim_date"), relationship = "many-to-many")
  
  load('Data/temp1619.Rdata') #temp data
  
  #merge with temp data
  daily.temp.all <- daily.temp.all %>%
    mutate(newdate = as.Date(dateonly))
  
  casecross <- casecross %>%
    left_join(daily.temp.all, by ="newdate")
  
  holiday_dates <- as.Date(c(
    # Memorial Day (last Monday of May)
    "2016-05-30", "2017-05-29", "2018-05-28", "2019-05-27",
    # Independence Day
    "2016-07-04", "2017-07-04", "2018-07-04", "2019-07-04",
    # Labor Day (first Monday of September)
    "2016-09-05", "2017-09-04", "2018-09-03", "2019-09-02"
  ))
  
  casecross <- casecross %>%
    mutate(holiday = if_else(newdate %in% holiday_dates, 1, 0))
  return(casecross)
}

pregpp_cc=casecross(edpregpp)
preg_cc=casecross(edpregpp %>% filter(preg==1))
pp_cc=casecross(edpregpp %>% filter(postpartum==1))

#bycause
pregrel_cc=casecross(preg)
mental_cc=casecross(mental)
musc_cc=casecross(musc)
resp_cc=casecross(resp)
infection_cc=casecross(infection)
digestive_cc=casecross(digestive)
genit_cc=casecross(genit)
injury_cc=casecross(injury)





