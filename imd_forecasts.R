library(lubridate)
library(tidyverse)
library(tsibble)
library(sf)
library(fable)




cheshire_lsoas <- st_read("cheshire_lsoas.geojson")

cheshire_calls <- read_csv("/Volumes/n8_covid/cheshire_calls.csv") 

x <- sort(cheshire_lsoas$index_of_multiple_deprivation_imd_rank)
n = 3
thing <- split(x, sort(x%%n))
min(thing$`0`)
max(thing$`0`)
min(thing$`1`)
max(thing$`1`)
min(thing$`2`)
max(thing$`2`)

cheshire_lsoas <- cheshire_lsoas %>% 
  mutate(dep_level = case_when(index_of_multiple_deprivation_imd_rank %in% thing$`0` ~ "high",
                               index_of_multiple_deprivation_imd_rank %in% thing$`1` ~ "medium",
                               index_of_multiple_deprivation_imd_rank %in% thing$`2` ~ "low"))

calls <- left_join(cheshire_calls, cheshire_lsoas, by = c("lsoa" = "LSOA11NM"))

# cheshire_lsoas %>% st_drop_geometry() %>% group_by(dep_level, index_of_multiple_deprivation_imd_decile) %>% count()


# Count weekly calls, calculate percent each week from high/med/low, 
#  add dummy variables and convert to a tsibble object
count_all_calls <- calls %>% 
  filter(incident_type_new == "ASB") %>% 
  mutate(incident_week = yearweek(incident_date_time)) %>% 
  count(dep_level, incident_week, name = "call_count") %>%   
  group_by(incident_week) %>% 
  mutate(per =  call_count/sum(call_count)*100) %>% 
  ungroup %>% 
  # Remove the first and last weeks from the data because weeks are defined as
  # being seven days starting on a Monday so weeks are sometimes split across
  # years. This means that at the start and end of the data there might be (and
  # in-fact are) partial weeks containing fewer than seven days, meaning those
  # 'weekly' call counts are artificially low.
  slice(2:(n() - 1)) %>% 
  as_tsibble(index = incident_week, key = dep_level) %>% 
  fill_gaps(per = 0) %>% 
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
    arima = ARIMA(per ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )

# Create data for forecasting
# This step is necessary because the model contains dummy variables, so we have
# to have some way of specifying the value of each dummy for each time point.
# The dummy variables are all `TRUE` for all future periods, so this is easy to 
# do. The variable names must match those in the original data and therefore the
# names of the model terms, excluding `trend()` and `season()`, which are
# handled automatically.
fdata_all_calls <- expand_grid(
  dep_level  = unique(model_all_calls$dep_level),
  incident_week = yearweek(seq.Date(
    from = ymd("2020-01-31"), 
    to = ymd("2020-12-31"),
    by = "week"
  )),
  new_system = TRUE,
  hmic_changes = TRUE
) %>% 
  as_tsibble(index = incident_week, key = dep_level) 

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
    dep_level %in% unique(model_all_calls$dep_level),
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_calls = per) %>% 
  full_join(
    forecast_all_calls, 
    by = c("dep_level", "incident_week")
  ) %>% 
  select(
    dep_level,
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
write_rds(final_all_calls_2, here::here("output/dep_level_asb_forecasts.Rds"))






