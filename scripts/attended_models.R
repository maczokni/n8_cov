# forecast proportion of calls attended in total and by crime type

# Load packages
library(fable)
library(lubridate)
library(tsibble)
library(tidyverse)

# Read and clean data

calls <- read_csv("Z:\\n8_data_v2.csv.gz") %>%  janitor::clean_names()

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


# count the proportion which were attened each week in total first

count_attended_calls <- calls %>% 
  mutate(incident_week = yearweek(incident_date_time)) %>% 
  group_by(incident_week) %>% 
  summarise(num_calls = n(), 
            num_attended = sum(attended_flag, na.rm=TRUE), 
            percent_attended = round(num_attended/num_calls*100,1))

#  convert to a tsibble object

count_attended_calls <- count_attended_calls %>% 
  slice(2:(n() - 1)) %>% # remove first and last row
  select(-num_calls, -num_attended) %>%  # remove unnecessary cols
  as_tsibble(index = incident_week) %>% 
  fill_gaps(call_count = 0) %>% 
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
model_attended_calls <- count_attended_calls %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(percent_attended ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )


# Create data for forecasting
# This step is necessary because the model contains dummy variables, so we have
# to have some way of specifying the value of each dummy for each time point.
# The dummy variables are all `TRUE` for all future periods, so this is easy to 
# do. The variable names must match those in the original data and therefore the
# names of the model terms, excluding `trend()` and `season()`, which are
# handled automatically.
fdata_attended_calls <- expand_grid(
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  new_system = TRUE,
  hmic_changes = TRUE
) %>% 
  as_tsibble(index = incident_week) 

fdata_attended_calls$bank_holiday <- fdata_attended_calls$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))


# Create forecasts and extract confidence intervals
forecast_attended_calls <- model_attended_calls %>% 
  forecast(new_data = fdata_attended_calls) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_attended_calls <- count_attended_calls %>% 
  filter(
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_attended = percent_attended) %>% 
  full_join(
    forecast_attended_calls, 
    by = c("incident_week")
  ) %>% 
  select(
    incident_week, 
    actual_attended, 
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
    sig = actual_attended < forecast_lower | actual_attended > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_attended_calls, here::here("output/all_attended_forecasts.Rds"))



######

# now break it down by call type

# Count proporation of weekly calls which were attended, add dummy variables and convert to a tsibble object

# count the proportion which were attened each week in total first

count_attended_calls <- calls %>% 
  mutate(incident_week = yearweek(incident_date_time)) %>% 
  group_by(incident_week, incident_type_new) %>% 
  summarise(num_calls = n(), 
            num_attended = sum(attended_flag, na.rm=TRUE), 
            percent_attended = round(num_attended/num_calls*100,1)) %>% 
  slice(2:(n() - 1)) %>% 
  select(-num_calls, -num_attended) %>%  # remove unnecessary cols
  as_tsibble(index = incident_week, key = incident_type_new) %>% 
  fill_gaps(percent_attended = 0) %>% 
  # Add dummy variables
  mutate(
    # Dummy for change from old to new call-handling system
    new_system = incident_week > yearweek(ymd("2017-06-03")),
    # Dummy for changes in practice after adverse HMIC call-handling report
    hmic_changes = incident_week > yearweek(ymd("2017-05-15")), 
    # Dummy for bank holiday 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))



model_attended_calls <- count_attended_calls %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(percent_attended ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )



# Create data for forecasting
# This step is necessary because the model contains dummy variables, so we have
# to have some way of specifying the value of each dummy for each time point.
# The dummy variables are all `TRUE` for all future periods, so this is easy to 
# do. The variable names must match those in the original data and therefore the
# names of the model terms, excluding `trend()` and `season()`, which are
# handled automatically.
fdata_attended_calls <- expand_grid(
  incident_type_new  = unique(model_attended_calls$incident_type_new),
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
fdata_attended_calls$bank_holiday <- fdata_attended_calls$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))
# Create forecasts and extract confidence intervals
forecast_all_calls <- model_attended_calls %>% 
  forecast(new_data = fdata_attended_calls) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_attended_calls <- count_attended_calls %>% 
  filter(
    incident_type_new %in% unique(model_attended_calls$incident_type_new),
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_attended = percent_attended) %>% 
  full_join(
    forecast_all_calls, 
    by = c("incident_type_new", "incident_week")
  ) %>% 
  select(
    incident_type_new,
    incident_week, 
    actual_attended, 
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
    sig = actual_attended < forecast_lower | actual_attended > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_attended_calls, here::here("output/by_type_attended_forecasts.Rds"))


# attended for domestic incidents

# count the proportion which were attened each week in total first

count_attended_calls <- calls %>% 
  filter(incident_type_new == "Domestic Incident", 
         initial_grade_of_response %in% c("Grade 1", "Grade 2", "Grade 3", "Grade 4")) %>% 
  mutate(incident_week = yearweek(incident_date_time)) %>% 
  group_by(incident_week, initial_grade_of_response) %>% 
  summarise(num_calls = n(), 
            num_attended = sum(attended_flag, na.rm=TRUE), 
            percent_attended = round(num_attended/num_calls*100,1)) %>% 
  select(-num_calls, -num_attended) %>%  # remove unnecessary cols
  as_tsibble(index = incident_week, key = initial_grade_of_response) %>% 
  fill_gaps(percent_attended = 0) %>% 
  # Add dummy variables
  mutate(
    # Dummy for change from old to new call-handling system
    new_system = incident_week > yearweek(ymd("2017-06-03")),
    # Dummy for changes in practice after adverse HMIC call-handling report
    hmic_changes = incident_week > yearweek(ymd("2017-05-15")), 
    # Dummy for bank holiday 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))



model_attended_calls <- count_attended_calls %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(percent_attended ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )



# Create data for forecasting
fdata_attended_calls <- expand_grid(
  initial_grade_of_response  = unique(model_attended_calls$initial_grade_of_response),
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
fdata_attended_calls$bank_holiday <- fdata_attended_calls$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))
# Create forecasts and extract confidence intervals
forecast_all_calls <- model_attended_calls %>% 
  forecast(new_data = fdata_attended_calls) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_attended_calls <- count_attended_calls %>% 
  filter(
    initial_grade_of_response %in% unique(model_attended_calls$initial_grade_of_response),
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_attended = percent_attended) %>% 
  full_join(
    forecast_all_calls, 
    by = c("initial_grade_of_response", "incident_week")
  ) %>% 
  select(
    initial_grade_of_response,
    incident_week, 
    actual_attended, 
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
    sig = actual_attended < forecast_lower | actual_attended > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_attended_calls, here::here("output/domestic_by_grade_attended_forecasts.Rds"))




