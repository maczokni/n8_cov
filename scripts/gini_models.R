# forecast proportion of calls attended in total and by crime type

# Load packages
library(fable)
library(lubridate)
library(tsibble)
library(tidyverse)
library(ineq)

# Read and clean data
cheshire_calls <- read_csv("/Volumes/n8_covid/cheshire_calls.csv") %>% janitor::clean_names()
weekly_count_by_lsoa <- read_csv("/Volumes/n8_covid/weekly_count_by_lsoa.csv") %>% janitor::clean_names()
weekly_asb_by_lsoa <- read_csv("/Volumes/n8_covid/weekly_asb_by_lsoa.csv")
weekly_drugs_by_lsoa <- read_csv("/Volumes/n8_covid/weekly_drugs_by_lsoa.csv")
weekly_violence_by_lsoa <- read_csv("/Volumes/n8_covid/weekly_violence_by_lsoa.csv")
weekly_dv_by_lsoa <- read_csv("/Volumes/n8_covid/weekly_dv_by_lsoa.csv")
weekly_misper_by_lsoa <- read_csv("/Volumes/n8_covid/weekly_misper_by_lsoa.csv")

# get gini for each week

weekly_gini <- weekly_count_by_lsoa %>% 
  mutate(incident_week = yearweek(inc_wk)) %>% 
  group_by(incident_week) %>% 
  summarise(gini = ineq(n), type = "Gini") 

#  convert to a tsibble object

weekly_gini <- weekly_gini %>% 
  slice(2:(n() - 1)) %>% # remove first and last row
  select(-type) %>%  # remove unnecessary cols
  as_tsibble(index = incident_week) %>% 
  # fill_gaps(call_count = 0) %>% 
  # Add dummy variables
  mutate(
    # Dummy for change from old to new call-handling system
    new_system = incident_week > yearweek(ymd("2017-06-03")),
    # Dummy for changes in practice after adverse HMIC call-handling report
    hmic_changes = incident_week >  yearweek(ymd("2017-05-15")), 
    # Dummy for bank holiday 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))


# The model is based only on calls from before the first UK COVID case on 31
# January 2021
model_attended_calls <- weekly_gini %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(gini ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )


# Create data for forecasting
# This step is necessary because the model contains dummy variables, so we have
# to have some way of specifying the value of each dummy for each time point.
# The dummy variables are all `TRUE` for all future periods, so this is easy to 
# do. The variable names must match those in the original data and therefore the
# names of the model terms, excluding `trend()` and `season()`, which are
# handled automatically.
fdata_gini <- expand_grid(
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  new_system = TRUE,
  hmic_changes = TRUE
) %>% 
  as_tsibble(index = incident_week) 

fdata_gini$bank_holiday <- fdata_gini$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))


# Create forecasts and extract confidence intervals
forecast_gini <- model_attended_calls %>% 
  forecast(new_data = fdata_gini) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_gini <- weekly_gini %>% 
  filter(
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_gini = gini) %>% 
  full_join(
    forecast_gini, 
    by = c("incident_week")
  ) %>% 
  select(
    incident_week, 
    actual_gini, 
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
    sig = actual_gini < forecast_lower | actual_gini > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_gini, "/Volumes/n8_covid/gini_forecasts.Rds")



######

# now break it down by call type - but have to do separate for each type of interest. 

# ASB

weekly_gini <- weekly_asb_by_lsoa %>% 
  mutate(incident_week = yearweek(inc_wk)) %>% 
  group_by(incident_week) %>% 
  summarise(gini = ineq(n), type = "Gini") 

#  convert to a tsibble object

weekly_gini <- weekly_gini %>% 
  slice(2:(n() - 1)) %>% # remove first and last row
  select(-type) %>%  # remove unnecessary cols
  as_tsibble(index = incident_week) %>% 
  # fill_gaps(call_count = 0) %>% 
  # Add dummy variables
  mutate(
    # Dummy for change from old to new call-handling system
    new_system = incident_week > yearweek(ymd("2017-06-03")),
    # Dummy for changes in practice after adverse HMIC call-handling report
    hmic_changes = incident_week >  yearweek(ymd("2017-05-15")), 
    # Dummy for bank holiday 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))


# The model is based only on calls from before the first UK COVID case on 31
# January 2021
model_attended_calls <- weekly_gini %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(gini ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )


# Create data for forecasting
# This step is necessary because the model contains dummy variables, so we have
# to have some way of specifying the value of each dummy for each time point.
# The dummy variables are all `TRUE` for all future periods, so this is easy to 
# do. The variable names must match those in the original data and therefore the
# names of the model terms, excluding `trend()` and `season()`, which are
# handled automatically.
fdata_gini <- expand_grid(
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  new_system = TRUE,
  hmic_changes = TRUE
) %>% 
  as_tsibble(index = incident_week) 

fdata_gini$bank_holiday <- fdata_gini$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))


# Create forecasts and extract confidence intervals
forecast_gini <- model_attended_calls %>% 
  forecast(new_data = fdata_gini) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_gini <- weekly_gini %>% 
  filter(
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_gini = gini) %>% 
  full_join(
    forecast_gini, 
    by = c("incident_week")
  ) %>% 
  select(
    incident_week, 
    actual_gini, 
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
    sig = actual_gini < forecast_lower | actual_gini > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_gini, "/Volumes/n8_covid/gini_asb_forecast.Rds")



# Drugs

weekly_gini <- weekly_drugs_by_lsoa %>% 
  mutate(incident_week = yearweek(inc_wk)) %>% 
  group_by(incident_week) %>% 
  summarise(gini = ineq(n), type = "Gini") 

#  convert to a tsibble object

weekly_gini <- weekly_gini %>% 
  slice(2:(n() - 1)) %>% # remove first and last row
  select(-type) %>%  # remove unnecessary cols
  as_tsibble(index = incident_week) %>% 
  # fill_gaps(call_count = 0) %>% 
  # Add dummy variables
  mutate(
    # Dummy for change from old to new call-handling system
    new_system = incident_week > yearweek(ymd("2017-06-03")),
    # Dummy for changes in practice after adverse HMIC call-handling report
    hmic_changes = incident_week >  yearweek(ymd("2017-05-15")), 
    # Dummy for bank holiday 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))


# The model is based only on calls from before the first UK COVID case on 31
# January 2021
model_attended_calls <- weekly_gini %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(gini ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )


# Create data for forecasting
# This step is necessary because the model contains dummy variables, so we have
# to have some way of specifying the value of each dummy for each time point.
# The dummy variables are all `TRUE` for all future periods, so this is easy to 
# do. The variable names must match those in the original data and therefore the
# names of the model terms, excluding `trend()` and `season()`, which are
# handled automatically.
fdata_gini <- expand_grid(
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  new_system = TRUE,
  hmic_changes = TRUE
) %>% 
  as_tsibble(index = incident_week) 

fdata_gini$bank_holiday <- fdata_gini$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))


# Create forecasts and extract confidence intervals
forecast_gini <- model_attended_calls %>% 
  forecast(new_data = fdata_gini) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_gini <- weekly_gini %>% 
  filter(
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_gini = gini) %>% 
  full_join(
    forecast_gini, 
    by = c("incident_week")
  ) %>% 
  select(
    incident_week, 
    actual_gini, 
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
    sig = actual_gini < forecast_lower | actual_gini > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_gini, "/Volumes/n8_covid/gini_drugs_forecast.Rds")



# Violence

weekly_gini <- weekly_violence_by_lsoa %>% 
  mutate(incident_week = yearweek(inc_wk)) %>% 
  group_by(incident_week) %>% 
  summarise(gini = ineq(n), type = "Gini") 

#  convert to a tsibble object

weekly_gini <- weekly_gini %>% 
  slice(2:(n() - 1)) %>% # remove first and last row
  select(-type) %>%  # remove unnecessary cols
  as_tsibble(index = incident_week) %>% 
  # fill_gaps(call_count = 0) %>% 
  # Add dummy variables
  mutate(
    # Dummy for change from old to new call-handling system
    new_system = incident_week > yearweek(ymd("2017-06-03")),
    # Dummy for changes in practice after adverse HMIC call-handling report
    hmic_changes = incident_week >  yearweek(ymd("2017-05-15")), 
    # Dummy for bank holiday 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))


# The model is based only on calls from before the first UK COVID case on 31
# January 2021
model_attended_calls <- weekly_gini %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(gini ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )


# Create data for forecasting
# This step is necessary because the model contains dummy variables, so we have
# to have some way of specifying the value of each dummy for each time point.
# The dummy variables are all `TRUE` for all future periods, so this is easy to 
# do. The variable names must match those in the original data and therefore the
# names of the model terms, excluding `trend()` and `season()`, which are
# handled automatically.
fdata_gini <- expand_grid(
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  new_system = TRUE,
  hmic_changes = TRUE
) %>% 
  as_tsibble(index = incident_week) 

fdata_gini$bank_holiday <- fdata_gini$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))


# Create forecasts and extract confidence intervals
forecast_gini <- model_attended_calls %>% 
  forecast(new_data = fdata_gini) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_gini <- weekly_gini %>% 
  filter(
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_gini = gini) %>% 
  full_join(
    forecast_gini, 
    by = c("incident_week")
  ) %>% 
  select(
    incident_week, 
    actual_gini, 
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
    sig = actual_gini < forecast_lower | actual_gini > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_gini, "/Volumes/n8_covid/gini_violence_forecast.Rds")


# DOmestic incidents

weekly_gini <- weekly_dv_by_lsoa %>% 
  mutate(incident_week = yearweek(inc_wk)) %>% 
  group_by(incident_week) %>% 
  summarise(gini = ineq(n), type = "Gini") 

#  convert to a tsibble object

weekly_gini <- weekly_gini %>% 
  slice(2:(n() - 1)) %>% # remove first and last row
  select(-type) %>%  # remove unnecessary cols
  as_tsibble(index = incident_week) %>% 
  # fill_gaps(call_count = 0) %>% 
  # Add dummy variables
  mutate(
    # Dummy for change from old to new call-handling system
    new_system = incident_week > yearweek(ymd("2017-06-03")),
    # Dummy for changes in practice after adverse HMIC call-handling report
    hmic_changes = incident_week >  yearweek(ymd("2017-05-15")), 
    # Dummy for bank holiday 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))


# The model is based only on calls from before the first UK COVID case on 31
# January 2021
model_attended_calls <- weekly_gini %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(gini ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )


# Create data for forecasting
# This step is necessary because the model contains dummy variables, so we have
# to have some way of specifying the value of each dummy for each time point.
# The dummy variables are all `TRUE` for all future periods, so this is easy to 
# do. The variable names must match those in the original data and therefore the
# names of the model terms, excluding `trend()` and `season()`, which are
# handled automatically.
fdata_gini <- expand_grid(
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  new_system = TRUE,
  hmic_changes = TRUE
) %>% 
  as_tsibble(index = incident_week) 

fdata_gini$bank_holiday <- fdata_gini$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))


# Create forecasts and extract confidence intervals
forecast_gini <- model_attended_calls %>% 
  forecast(new_data = fdata_gini) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_gini <- weekly_gini %>% 
  filter(
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_gini = gini) %>% 
  full_join(
    forecast_gini, 
    by = c("incident_week")
  ) %>% 
  select(
    incident_week, 
    actual_gini, 
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
    sig = actual_gini < forecast_lower | actual_gini > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_gini, "/Volumes/n8_covid/gini_dv_forecast.Rds")

# missing persons

weekly_gini <- weekly_misper_by_lsoa %>% 
  mutate(incident_week = yearweek(inc_wk)) %>% 
  group_by(incident_week) %>% 
  summarise(gini = ineq(n), type = "Gini") 

#  convert to a tsibble object

weekly_gini <- weekly_gini %>% 
  slice(2:(n() - 1)) %>% # remove first and last row
  select(-type) %>%  # remove unnecessary cols
  as_tsibble(index = incident_week) %>% 
  # fill_gaps(call_count = 0) %>% 
  # Add dummy variables
  mutate(
    # Dummy for change from old to new call-handling system
    new_system = incident_week > yearweek(ymd("2017-06-03")),
    # Dummy for changes in practice after adverse HMIC call-handling report
    hmic_changes = incident_week >  yearweek(ymd("2017-05-15")), 
    # Dummy for bank holiday 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))


# The model is based only on calls from before the first UK COVID case on 31
# January 2021
model_attended_calls <- weekly_gini %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(gini ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )


# Create data for forecasting
# This step is necessary because the model contains dummy variables, so we have
# to have some way of specifying the value of each dummy for each time point.
# The dummy variables are all `TRUE` for all future periods, so this is easy to 
# do. The variable names must match those in the original data and therefore the
# names of the model terms, excluding `trend()` and `season()`, which are
# handled automatically.
fdata_gini <- expand_grid(
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  new_system = TRUE,
  hmic_changes = TRUE
) %>% 
  as_tsibble(index = incident_week) 

fdata_gini$bank_holiday <- fdata_gini$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))


# Create forecasts and extract confidence intervals
forecast_gini <- model_attended_calls %>% 
  forecast(new_data = fdata_gini) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_gini <- weekly_gini %>% 
  filter(
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_gini = gini) %>% 
  full_join(
    forecast_gini, 
    by = c("incident_week")
  ) %>% 
  select(
    incident_week, 
    actual_gini, 
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
    sig = actual_gini < forecast_lower | actual_gini > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_gini, "/Volumes/n8_covid/gini_misper_forecast.Rds")







