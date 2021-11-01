library(lubridate)
library(tsibble)
library(tidyverse)

# reka path
calls <- read_csv("/Volumes/n8_covid/n8_data.csv.gz") %>% # USE CORRECT PATH FOR CLEAN DATA FILE ON YOUR COMPUTER
  janitor::clean_names()

# Clean calls
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

# Note important dates
# Source: https://www.instituteforgovernment.org.uk/sites/default/files/timeline-lockdown-web.pdf
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



# look at attended yes/no

calls$attended <- case_when(calls$attended_flag == 1 ~ "Yes", 
                            calls$attended_flag == 0 ~ "No", 
                            TRUE ~ NA_character_)

attended_calls <- calls %>% filter(attended == "Yes")


# Create time on scene variable

attended_calls$tos <- as.period(ymd_hms(attended_calls$released_date_time) -  
                                            ymd_hms(attended_calls$earliest_arrived_date_time))

attended_calls$tos_mins <- time_length(attended_calls$tos, unit = "minute")

# Okay there is something spooky, as we have 64 thousand cases where they were released *after* they arrived on scene.... 
negative_tos <- attended_calls %>% filter(tos_mins < 0)

# Excluding negative tos for now... 

positive_tos <- attended_calls %>% filter(tos_mins >= 0)

options(scipen = 999) # remove scientific notation

ggplot(attended_calls, aes(x = tos_mins)) + 
  geom_density(fill = "grey", alpha = 0.7) + 
  geom_vline(xintercept = 0, linetype = 13) + 
  geom_vline(xintercept = 1, linetype = 10) + 
  annotate("label", x = 1, y = 0.6, label = "1 minute") + 
  geom_vline(xintercept = 60, linetype = 11) + 
  annotate("label", x = 60, y = 0.6, label = "1 hour") + 
  geom_vline(xintercept = 24*60, linetype = 12) + 
  annotate("label", x = 24*60, y = 0.6, label = "1 day") + 
  geom_vline(xintercept = 24*60*7, linetype = 14) + 
  annotate("label", x = 24*60*7, y = 0.6, label = "1 week") + 
  theme_bw() +
  scale_x_continuous(trans = scales::pseudo_log_trans()) + 
  labs(title = "Distribution of time on scene across all calls")+ 
  xlab("Response time in minutes") + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())


# cool now compare everything up to 31 Jan 2020

attended_calls$pre_pandemic <- ifelse(attended_calls$incident_date_time <= dmy("31-01-2020"), "pre-pandemic", "pandemic")

attended_calls %>% 
  filter(initial_grade_of_response %in% c("Grade 1", "Grade 2", "Grade 3", "Grade 4")) %>% 
  ggplot(., aes(x = tos_mins, fill = pre_pandemic)) + 
  geom_density(alpha = 0.7) + 
  geom_vline(xintercept = 0, linetype = 13) + 
  geom_vline(xintercept = 1, linetype = 10) + 
  annotate("label", x = 1, y = 0.6, label = "1 minute") + 
  geom_vline(xintercept = 60, linetype = 11) + 
  annotate("label", x = 60, y = 0.6, label = "1 hour") + 
  geom_vline(xintercept = 24*60, linetype = 12) + 
  annotate("label", x = 24*60, y = 0.6, label = "1 day") + 
  geom_vline(xintercept = 24*60*7, linetype = 14) + 
  annotate("label", x = 24*60*7, y = 0.6, label = "1 week") + 
  theme_bw() +
  scale_x_continuous(trans = scales::pseudo_log_trans()) + 
  labs(title = "Distribution of time on scene across all calls")+ 
  xlab("Time on scene in minutes") + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank()) + 
  facet_wrap(~initial_grade_of_response, ncol = 1)


# make model anyway

# Count median response time for calls which were attended, add dummy variables and convert to a tsibble object


median_tos <- attended_calls %>% 
  filter(initial_grade_of_response %in% c("Grade 1", "Grade 2", "Grade 3", "Grade 4")) %>% 
  mutate(incident_week = yearweek(incident_date_time)) %>% 
  group_by(incident_week, initial_grade_of_response) %>% 
  summarise(median_tos = median(tos_mins, na.rm = TRUE)) %>% 
  # slice(2:(n() - 1)) %>% 
  as_tsibble(index = incident_week, key = initial_grade_of_response) %>% 
  fill_gaps(median_tos = NA) %>% 
  # Add dummy variables
  mutate(
    # Dummy for change from old to new call-handling system
    new_system = incident_week > yearweek(ymd("2017-06-03")),
    # Dummy for changes in practice after adverse HMIC call-handling report
    hmic_changes = incident_week > yearweek(ymd("2017-05-15")), 
    # Dummy for bank holiday 
    bank_holiday = incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2015:2020))))



model_tos <- median_tos %>% 
  filter(incident_week < yearweek(ymd("2020-01-31"))) %>% 
  model(
    arima = ARIMA(median_tos ~ trend() + season() + new_system + hmic_changes + bank_holiday)
  )



# Create data for forecasting

fdata_median_tos <- expand_grid(
  initial_grade_of_response  = unique(model_tos$initial_grade_of_response),
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
fdata_median_tos$bank_holiday <- fdata_median_tos$incident_week %in% yearweek(as_date(timeDate::holidayLONDON(year = 2020)))
# Create forecasts and extract confidence intervals
forecast_all_calls <- model_tos %>% 
  forecast(new_data = fdata_median_tos) %>% 
  hilo(level = 95) %>% 
  janitor::clean_names() %>% 
  unpack_hilo(cols = x95_percent) 


# Join actual counts to the forecast object and check if actual calls were 
# outside the forecast range
final_median_tos <- median_tos %>% 
  filter(
    initial_grade_of_response %in% unique(model_tos$initial_grade_of_response),
    incident_week > yearweek(ymd("2019-12-31"))
  ) %>% 
  select(incident_week, actual_tos = median_tos) %>% 
  full_join(
    forecast_all_calls, 
    by = c("initial_grade_of_response", "incident_week")
  ) %>% 
  select(
    initial_grade_of_response,
    incident_week, 
    actual_tos, 
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
    sig = actual_tos < forecast_lower | actual_tos > forecast_upper
  ) %>% 
  replace_na(list(sig = FALSE))

# Save result for use elsewhere (e.g. in an Rmarkdown document)
write_rds(final_median_tos, here::here("output/by_grade_tos_forecasts.Rds"))


