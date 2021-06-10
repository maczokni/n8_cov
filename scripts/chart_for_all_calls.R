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

# Count weekly calls, add dummy variables and convert to a tsibble object
count_all_calls <- calls %>% 
  mutate(incident_week = yearweek(incident_date_time)) %>% 
  count(incident_week, name = "call_count") %>% 
  #count(call_origin, incident_weekm name = "call_count")   - for call origin forecasts!
  # Remove the first and last weeks from the data because weeks are defined as
  # being seven days starting on a Monday so weeks are sometimes split across
  # years. This means that at the start and end of the data there might be (and
  # in-fact are) partial weeks containing fewer than seven days, meaning those
  # 'weekly' call counts are artificially low.
  slice(2:(n() - 1)) %>% 
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
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  new_system = TRUE,
  hmic_changes = TRUE
) %>% 
  as_tsibble(index = incident_week) 
#key = call_origin
fdata_all_calls$bank_holiday <- fdata_all_calls$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))
# Create forecasts and extract confidence intervals
forecast_all_calls <- model_all_calls %>% 
  forecast(new_data = fdata_all_calls) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_all_calls <- count_all_calls %>% 
  filter(
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_calls = call_count) %>% 
  full_join(
    forecast_all_calls, 
    by = c( "incident_week")
  ) %>% 
  select(
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

dates <- tribble(
  ~date, ~event,
  "2020-01-31", "first UK COVID case",
  "2020-03-23", "first lockdown begins",
  "2020-06-15", "first lockdown ends",
  "2020-11-05", "second lockdown begins",
  "2020-12-02", "second lockdown ends"
) %>% 
  mutate(
    date = as_date(yearweek(ymd(date))), 
    row = row_number(),
    label = str_glue("{row}. {event}")
  )


final_all_calls %>% 
 mutate(forecast_lower = ifelse(forecast_lower < 0, 0, forecast_lower)) %>% 
  ggplot() +
  # Forecast
  geom_ribbon(
    aes(incident_week, ymin = forecast_lower, ymax = forecast_upper), 
    na.rm = TRUE,
    alpha = 0.5, 
    fill = "grey80"
  ) +
  geom_line(aes(incident_week, forecast_mean), na.rm = TRUE, linetype = "22") +
  # Dates of interest
  geom_vline(aes(xintercept = date), data = dates, linetype = "12") +
  geom_label(aes(date, 0, label = row), data = dates, colour = "grey20") +
  # Actual calls
  geom_line(aes(incident_week, actual_calls)) +
  geom_point(aes(incident_week, actual_calls, fill = sig), shape = 21) +
  scale_x_date(date_labels = "%e %b\n%Y", 
               limits = as.Date(c("2020-01-06", "2020-12-21"))) +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma_format()) +
  scale_fill_manual(values = c(`TRUE` = "black", `FALSE` = "grey80")) +
  labs(
    title = "Change in all calls for service during 2021 compared to pre-pandemic forecast",
    subtitle = str_wrap(
      str_glue("Events by week: ", str_c(pull(dates, label), collapse = "; ")), 
      80
    ),
    caption = "Forecast calculated using data up to 31 January 2020",
    x = NULL,
    y = "Weekly number of calls",
    fill = "actual calls significantly different from forecast"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold")) + 
    xlim(c(min(final_all_calls$incident_week), ymd("2020-12-14")))




