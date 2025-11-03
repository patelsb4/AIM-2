

library(lubridate)
library(weathermetrics)
library(dplyr)

setwd("Y:/patels")
# function to restrict data on summer months and hourly data
process.data <- function(data, year) {
  data$DATE <- ymd_hms(data$DATE)

  # Filter the data
  data <- data[data$`REPORT_TYPE` == "FM-15", ]
  
  # Select specific columns and add year column
  data <- data[, c("DATE", "HourlyRelativeHumidity", "HourlyDryBulbTemperature")]
  data$Year <- year

  return(data)
}

temp.data <- list(
  temp16 = read.csv("Data/LCD_USW00013739_2016.csv"),
  temp17 = read.csv("Data/LCD_USW00013739_2017.csv"),
  temp18 = read.csv("Data/LCD_USW00013739_2018.csv"),
  temp19 = read.csv("Data/LCD_USW00013739_2019.csv")
)

# Process and combine all data frames
temp.all <- do.call(rbind, lapply(names(temp.data), function(year) {
  process.data(temp.data[[year]], year)
}))
years <- 2016:2019

# Process and combine all data frames with year information
temp.all <- do.call(rbind, mapply(function(df, year) process.data(df, year), temp.data, years, SIMPLIFY = FALSE))
# missingness
sum(is.na(temp.all$HourlyDryBulbTemperature))
sum(is.na(temp.all$HourlyRelativeHumidity))

## convert Celsius to Fahrenheit
temp.all <- temp.all %>%
  mutate(HourlyTempF = case_when(
    Year == 2004 ~ HourlyDryBulbTemperature, # Data already in Fahrenheit
    TRUE ~ convert_temperature(HourlyDryBulbTemperature, old_metric = "c", new_metric = "f") # Convert if not 2004
  ))

temp.all$HI <- heat.index(
  t = temp.all$HourlyTempF,
  rh = temp.all$HourlyRelativeHumidity,
  temperature.metric = "fahrenheit",
  output.metric = "fahrenheit"
)
temp.all$HI2 <- ifelse(temp.all$HourlyRelativeHumidity <= 40, temp.all$HourlyTempF, temp.all$HI)
temp.all$dateonly <- as.Date(temp.all$DATE)

daily.temp.all <- temp.all %>%
  filter(month(dateonly) %in% 5:9)

daily.temp.all <- daily.temp.all %>%
  group_by(dateonly) %>%
  summarize(
    temp.min = mean(HourlyTempF),
    temp.mean = mean(HourlyTempF),
    temp.max = max(HourlyTempF),
    hi.min = min(HI),
    hi.mean = mean(HI),
    hi.max = max(HI),
  )
