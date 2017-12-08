#################################
# Build base dataset for 651
#################################

# Preliminary settings
rm(list=ls())
Sys.setenv(TZ = "America/New_York")
# options(scipen = 999) # remove annoying scientific notation

setwd("/Users/philipp/Google Drive/Courses/Math 651/final project/")
library(dplyr)
library(lubridate)
library(ggplot2)

# Load base data (OBJECTID is unique key) &
# add response variablesl drop empty/unused variables 
# (**careful: ensure mutation units are correct)
# (SERVICECODE, SERVICECODEDESCRIPTION) identical
# 
data <- read.csv("data/City_Service_Requests_in_2017.csv") %>% 
	tbl_df() %>%
	mutate(
		wait = as.numeric(ymd_hms(RESOLUTIONDATE) - ymd_hms(ADDDATE))/(60*60*24),
		eta  = as.numeric(ymd_hms(SERVICEDUEDATE) - ymd_hms(ADDDATE))/(24),
		late = ymd_hms(RESOLUTIONDATE) > ymd_hms(SERVICEDUEDATE),
		ADDDATE = ymd_hms(ADDDATE),
		ADDDATE_simple = as_date(ADDDATE),
		RESOLUTIONDATE = ymd_hms(RESOLUTIONDATE),
		RESOLUTIONDATE_simple = as_date(RESOLUTIONDATE),
		SERVICEDUEDATE = ymd_hms(SERVICEDUEDATE)) %>%
	select(-SERVICECALLCOUNT, 
				 -SERVICEORDERDATE,
				 -INSPECTORNAME)

# Load and mutate weather data
weather <- read.csv("wunderground_hourlyWeather20005.csv") %>% tbl_df()

category_rain <- names(table(weather$cond))[c(5, 7, 10, 12, 14, 19, 23)]
category_heavyrain <- names(table(weather$cond))[c(5, 7, 14, 23)]
category_snow <- names(table(weather$cond))[c(21, 13, 11, 6, 4, 8)]

weather <- weather %>%	
	mutate(date_simple = as_date(date)) %>%
	group_by(date_simple) %>%
	summarize(
		precip_daily = sum(precip, na.rm = TRUE),
		temp_daily = mean(temp, na.rm = TRUE),
		rain_daily = sum(cond %in% category_rain) > 0,
		rain_daily_heavy = sum(cond %in% category_heavyrain) > 0,
		snow_daily = sum(cond %in% category_snow) > 0)

# Merge in weather data
data <- data %>%
	left_join(weather, by=c("ADDDATE_simple" = "date_simple"))

# Build weather intervals

weather_interval_pct <- function(date_begin, date_end, var) {
	# date_begin <- data$ADDDATE_simple[1]; date_end <- data$RESOLUTIONDATE_simple[1]; var <- "rain_daily"
	bool_vector <- weather[[var]][( weather$date_simple > (date_begin-1) ) & ( weather$date_simple < (date_end+1) )]
	sum(bool_vector)/length(bool_vector)
}

begin <- data$ADDDATE_simple
end <- data$RESOLUTIONDATE_simple

rain_interval <- rep(NA, nrow(data))
for (i in 1:nrow(data)) {
	rain_interval[i] <- weather_interval_pct(begin[i], end[i], "rain_daily")
}

rain_heavy_interval <- rep(NA, nrow(data))
for (i in 1:nrow(data)) {
	rain_heavy_interval[i] <- weather_interval_pct(begin[i], end[i], "rain_daily_heavy")
}

snow_interval <- rep(NA, nrow(data))
for (i in 1:nrow(data)) {
	snow_interval[i] <- weather_interval_pct(begin[i], end[i], "snow_daily")
}

data$rain_interval <- rain_interval
data$rain_heavy_interval <- rain_heavy_interval
data$snow_interval <- snow_interval

# Export temporary dataset for quick later re-use
write.csv(data, "data_appended.csv", row.names = FALSE)
write.csv(data %>% select(OBJECTID, wait:snow_interval), "data_split.csv", row.names=FALSE)
