# Load packages
library(fable)
library(lubridate)
library(tsibble)
library(tidyverse)

# Read and clean data
calls <- read_csv("../cheshire-calls/cheshire_raw_data.csv.gz") %>% # USE CORRECT PATH FOR CLEAN DATA FILE ON YOUR COMPUTER
  janitor::clean_names()

# Find categories with fewer than a certain number of calls so we can exclude
# these, since these call types are not amenable to these types of incidents
minor_categories <- calls %>% 
  count(incident_type) %>% 
  filter(n < 1000) %>% 
  pull(incident_type)

# Categorise calls
    calls <- calls %>% 
      mutate(
        call_origin = case_when(
          call_origin %in% c(
            "Alarm Company", 
            "Email (to Public Contact Mailbox)",
            "Helpdesk",
            "Public Non Emergency (inc. Door Phones and PCPs)",
            "Single Online Home",
            "Social Media"
          ) ~ "public non-emergency",
          call_origin == "Other Emergency Services (inc. Other Forces)" ~
            "other emergency services",
          call_origin == "Police Generated (inc. Call Sign / Collar Numbers)" ~
            "police generated",
          call_origin == "Unknown Choice List Value" | is.na(call_origin) ~ 
            NA_character_,
          TRUE ~ call_origin
        ),
        incident_type = case_when(
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
    
    
    

# Count weekly calls, add dummy variables and convert to a tsibble object
count_all_calls <- calls %>% 
  mutate(incident_week = yearweek(incident_date_time)) %>% 
  count(call_origin, incident_week, name = "call_count") %>%   
  # Remove the first and last weeks from the data because weeks are defined as
  # being seven days starting on a Monday so weeks are sometimes split across
  # years. This means that at the start and end of the data there might be (and
  # in-fact are) partial weeks containing fewer than seven days, meaning those
  # 'weekly' call counts are artificially low.
  slice(2:(n() - 1)) %>% 
  as_tsibble(index = incident_week, key = call_origin) %>% 
  fill_gaps(call_count = 0) %>% 
  # Add dummy variables
  mutate(
    # Dummy for change from old to new call-handling system
    new_system = incident_week > yearweek(ymd("2017-06-03")),
    # Dummy for changes in practice after adverse HMIC call-handling report
    hmic_changes = incident_week > yearweek(ymd("2017-05-15")), 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))
    
  

# Model weekly call counts
# The model is based only on calls from before the first UK COVID case on 31
# January 2021
model_all_calls <- count_all_calls %>% 
  # If you only want to model certain types of call, add a `filter()` here
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(call_count ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )

# Create data for forecasting
# This step is necessary because the model contains dummy variables, so we have
# to have some way of specifying the value of each dummy for each time point.
# The dummy variables are all `TRUE` for all future periods, so this is easy to 
# do. The variable names must match those in the original data and therefore the
# names of the model terms, excluding `trend()` and `season()`, which are
# handled automatically.
fdata_all_calls <- expand_grid(
 call_origin  = unique(model_all_calls$call_origin),
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  new_system = TRUE,
  hmic_changes = TRUE
) %>% 
  as_tsibble(index = incident_week, key = call_origin) 

fdata_all_calls$bank_holiday <- fdata_all_calls$incident_week %in% 
  yearweek(as_date(timeDate::holidayLONDON(year = 2020)))

# Create forecasts and extract confidence intervals
forecast_all_calls <- model_all_calls %>% 
  forecast(new_data = fdata_all_calls) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent)

# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_all_calls_2 <- count_all_calls %>% 
  filter(
    call_origin %in% unique(model_all_calls$call_origin),
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_calls = call_count) %>% 
  full_join(
    forecast_all_calls, 
    by = c("call_origin", "incident_week")
  ) %>% 
  select(
    call_origin,
    incident_week, 
    actual_calls, 
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
    sig = actual_calls < forecast_lower | actual_calls > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_all_calls_2, here::here("output/call_origin_forecasts.Rds"))













