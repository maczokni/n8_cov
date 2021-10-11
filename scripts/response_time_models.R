# forecast proportion of calls attended in total and by crime type

# Load packages
library(fable)
library(lubridate)
library(tsibble)
library(tidyverse)

# Read and clean data

# calls <- read_csv("Z:\\n8_data_v2.csv.gz") %>%  janitor::clean_names()
calls <- read_csv("/Volumes/n8_covid/n8_data_v2.csv.gz") %>%  janitor::clean_names()

# Find categories with fewer than a certain number of calls so we can exclude
# these, since these call types are not amenable to these types of incidents
minor_categories <- calls %>% 
  count(incident_type) %>% 
  filter(n < 1000) %>% 
  pull(incident_type)

# Categorise calls
calls <- mutate(
  calls,
  incident_type_new = case_when(
    incident_type %in% c("Distraction Burglary") ~
      "Burglary - Residential",
    incident_type %in% c(
      "Assistance to Other Agencies",
      "CRB Use Only",
      "CSI To Be Informed",
      "External System",
      "Found Stolen Vehicle",
      "Message to Pass/OOF Enquiries",
      "PNC Markers",
      "Police Vehicle Recovery",
      "Pre-Planned Events",
      "Stop Search",
      "Training"
    ) ~ "other - admin",
    incident_type %in% c(
      "Alarm - Activation", 
      "Alarm - No Response", 
      "Audible Only Alarm",
      "Insecure Premises",
      "Police Installed Alarm",
      "Unauthorised Encampment"
    ) ~ "alarms",
    incident_type %in% c(
      "Bail Breaches/Wanted Person",
      "Bail/Curfew/Wanted",
      "Prison Licence Recall",
      "Warrant Crown Court"
    ) ~ "warrant/bail issue",
    incident_type %in% c("Domestic Animal Concern", "Wildlife Matters") ~ 
      "animals",
    incident_type %in% c(
      "Firearms - Non Notifiable Crime",
      "Firearms Involved Crime"
    ) ~ "firearms",
    incident_type %in% c("RTC", "RTC - Damage Only") ~ "traffic collision",
    incident_type %in% minor_categories ~ "other - minor",
    TRUE ~ incident_type
  )
)

# create attended variable


calls$attended <- case_when(calls$attended_flag == 1 ~ "Yes", 
                            calls$attended_flag == 0 ~ "No", 
                            TRUE ~ NA_character_)

# filter attended only 

attended_calls <- calls %>% filter(attended == "Yes")

# create response time variable

attended_calls$response_time <- as.period(ymd_hms(attended_calls$earliest_arrived_date_time) -  
                                            ymd_hms(attended_calls$earliest_deployed_date_time))

attended_calls$response_time_mins <- time_length(attended_calls$response_time, unit = "minute")

# add 12 hours to the negative time ones
attended_calls$response_time_mins <- ifelse(attended_calls$response_time_mins <0, 
                                            attended_calls$response_time_mins + 720, 
                                            attended_calls$response_time_mins)

# count the median response time for each week in total first

median_response_time <- attended_calls %>% 
  mutate(incident_week = yearweek(incident_date_time)) %>% 
  group_by(incident_week) %>% 
  summarise(median_resp_time = median(response_time_mins, na.rm = TRUE))



#  convert to a tsibble object

median_response_time <- median_response_time %>% 
  # slice(2:(n() - 1)) %>% # remove first and last row
  as_tsibble(index = incident_week) %>% 
  fill_gaps(median_resp_time = NA) %>% 
  # Add dummy variables
  mutate(
    # Dummy for change from old to new call-handling system
    new_system = incident_week > yearweek(ymd("2017-06-03")),
    # Dummy for changes in practice after adverse HMIC call-handling report
    hmic_changes = incident_week > yearweek(ymd("2017-05-15")), 
    # Dummy for bank holiday 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))


# The model is based only on calls from before the first UK COVID case on 31
# January 2021
model_all_resp_time <- median_response_time %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(median_resp_time ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )


# Create data for forecasting
fdata_resp_time <- expand_grid(
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  new_system = TRUE,
  hmic_changes = TRUE
) %>% 
  as_tsibble(index = incident_week) 

fdata_resp_time$bank_holiday <- fdata_resp_time$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))


# Create forecasts and extract confidence intervals
forecast_resp_time <-model_all_resp_time %>% 
  forecast(new_data = fdata_resp_time) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_all_resp_time <- median_response_time %>% 
  filter(
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_rt = median_resp_time) %>% 
  full_join(
    forecast_resp_time, 
    by = c("incident_week")
  ) %>% 
  select(
    incident_week, 
    actual_rt, 
    forecast_mean = mean, 
    forecast_lower = x95_percent_lower, 
    forecast_upper = x95_percent_upper
  ) %>% 
  mutate(
    # When plotting, it is more convenient to store the week as a date rather 
    # than a `yearweek` column
    incident_week = as_date(incident_week),
    # Calls are significantly different from the forecast if the call count is
    # less than the lower 95% CI or higher than the upper 95% CI
    sig = actual_rt < forecast_lower | actual_rt > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_all_resp_time, here::here("output/all_resp_time_forecast.Rds"))



######

# now break it down by call type

# Count median response time for calls which were attended, add dummy variables and convert to a tsibble object


median_response_time <- attended_calls %>% 
  mutate(incident_week = yearweek(incident_date_time)) %>% 
  group_by(incident_week, incident_type_new) %>% 
  summarise(median_resp_time = median(response_time_mins, na.rm = TRUE)) %>% 
  # slice(2:(n() - 1)) %>% 
  as_tsibble(index = incident_week, key = incident_type_new) %>% 
  fill_gaps(median_resp_time = NA) %>% 
  # Add dummy variables
  mutate(
    # Dummy for change from old to new call-handling system
    new_system = incident_week > yearweek(ymd("2017-06-03")),
    # Dummy for changes in practice after adverse HMIC call-handling report
    hmic_changes = incident_week > yearweek(ymd("2017-05-15")), 
    # Dummy for bank holiday 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))



model_rt <- median_response_time %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(median_resp_time ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )



# Create data for forecasting

fdata_median_rt <- expand_grid(
  incident_type_new  = unique(model_rt$incident_type_new),
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  new_system = TRUE,
  hmic_changes = TRUE
) %>% 
  as_tsibble(index = incident_week, key = incident_type_new) 

#key = call_origin
fdata_median_rt$bank_holiday <- fdata_median_rt$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))
# Create forecasts and extract confidence intervals
forecast_all_calls <- model_rt %>% 
  forecast(new_data = fdata_median_rt) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_median_rt <- median_response_time %>% 
  filter(
    incident_type_new %in% unique(model_rt$incident_type_new),
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_rt = median_resp_time) %>% 
  full_join(
    forecast_all_calls, 
    by = c("incident_type_new", "incident_week")
  ) %>% 
  select(
    incident_type_new,
    incident_week, 
    actual_rt, 
    forecast_mean = mean, 
    forecast_lower = x95_percent_lower, 
    forecast_upper = x95_percent_upper
  ) %>% 
  mutate(
    # When plotting, it is more convenient to store the week as a date rather 
    # than a `yearweek` column
    incident_week = as_date(incident_week),
    # Calls are significantly different from the forecast if the call count is
    # less than the lower 95% CI or higher than the upper 95% CI
    sig = actual_rt < forecast_lower | actual_rt > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_median_rt, here::here("output/by_type_rt_forecasts.Rds"))


# now break it down by grade

# Count median response time for calls which were attended, add dummy variables and convert to a tsibble object


median_response_time <- attended_calls %>% 
  mutate(incident_week = yearweek(incident_date_time)) %>% 
  group_by(incident_week, initial_grade_of_response) %>% 
  summarise(median_resp_time = median(response_time_mins, na.rm = TRUE)) %>% 
  # slice(2:(n() - 1)) %>% 
  as_tsibble(index = incident_week, key = initial_grade_of_response) %>% 
  fill_gaps(median_resp_time = NA) %>% 
  # Add dummy variables
  mutate(
    # Dummy for change from old to new call-handling system
    new_system = incident_week > yearweek(ymd("2017-06-03")),
    # Dummy for changes in practice after adverse HMIC call-handling report
    hmic_changes = incident_week > yearweek(ymd("2017-05-15")), 
    # Dummy for bank holiday 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))



model_rt <- median_response_time %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(median_resp_time ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )



# Create data for forecasting

fdata_median_rt <- expand_grid(
  initial_grade_of_response  = unique(model_rt$initial_grade_of_response),
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  new_system = TRUE,
  hmic_changes = TRUE
) %>% 
  as_tsibble(index = incident_week, key = initial_grade_of_response) 

#key = call_origin
fdata_median_rt$bank_holiday <- fdata_median_rt$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))
# Create forecasts and extract confidence intervals
forecast_all_calls <- model_rt %>% 
  forecast(new_data = fdata_median_rt) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_median_rt <- median_response_time %>% 
  filter(
    initial_grade_of_response %in% unique(model_rt$initial_grade_of_response),
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_rt = median_resp_time) %>% 
  full_join(
    forecast_all_calls, 
    by = c("initial_grade_of_response", "incident_week")
  ) %>% 
  select(
    initial_grade_of_response,
    incident_week, 
    actual_rt, 
    forecast_mean = mean, 
    forecast_lower = x95_percent_lower, 
    forecast_upper = x95_percent_upper
  ) %>% 
  mutate(
    # When plotting, it is more convenient to store the week as a date rather 
    # than a `yearweek` column
    incident_week = as_date(incident_week),
    # Calls are significantly different from the forecast if the call count is
    # less than the lower 95% CI or higher than the upper 95% CI
    sig = actual_rt < forecast_lower | actual_rt > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_median_rt, here::here("output/by_grade_rt_forecasts.Rds"))


