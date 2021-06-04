library(lubridate)
library(tsibble)
library(tidyverse)

# reka path
calls <- read_csv("/Volumes/n8_covid/n8_data.csv.gz") %>% # USE CORRECT PATH FOR CLEAN DATA FILE ON YOUR COMPUTER
  janitor::clean_names()

# create a "time of day" variable
calls$hour <- hour(ymd_hms(calls$incident_date_time))
calls$time <- case_when(calls$hour >= 5 & calls$hour < 11 ~ "Early", 
                        calls$hour >= 11 & calls$hour < 17 ~ "Daytime", 
                        calls$hour >= 17 & calls$hour < 23 ~ "Evening", 
                        calls$hour >= 23 | calls$hour < 5 ~ "Late", 
                        TRUE ~ NA_character_)
calls$time <- factor(calls$time, levels = c("Early","Daytime","Evening","Late"))

# plot
calls %>% 
  mutate(week = as_date(yearweek(incident_date_time))) %>% 
  count(time, week) %>% 
  ggplot(aes(x = week, y = n, fill = time)) +
  geom_area(position = "fill") +
  # First UK case
  geom_vline(xintercept = as.Date("2020-01-23"), linetype = "33") +
  # First UK lockdown begins
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "12") +
  # First UK lockdown ends
  geom_vline(xintercept = as.Date("2020-06-01"), linetype = "42") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y", expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(type = "qual") + 
  labs(
    title = "Proportion of Concern for Safety Incidents by Time of Day",
    x = NULL,
    y = "Proportion",
    fill = "Time of Day"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +    guides(fill=guide_legend(nrow=1,byrow=TRUE))


# look at attended yes/no

calls$attended <- case_when(calls$attended_flag == 1 ~ "Yes", 
                            calls$attended_flag == 0 ~ "No", 
                            TRUE ~ NA_character_)

calls %>% 
  mutate(week = as_date(yearweek(incident_date_time))) %>% 
  count(attended, week) %>% 
  ggplot(aes(x = week, y = n, fill = attended)) +
  geom_area(position = "fill") +
  # First UK case
  geom_vline(xintercept = as.Date("2020-01-23"), linetype = "33") +
  # First UK lockdown begins
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "12") +
  # First UK lockdown ends
  geom_vline(xintercept = as.Date("2020-06-01"), linetype = "42") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y", expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(type = "qual") + 
  labs(
    title = "Proportion of Concern for Safety Incidents by Attended",
    x = NULL,
    y = "Proportion",
    fill = "Attended (yes or no)"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +    guides(fill=guide_legend(nrow=1,byrow=TRUE))


# filter attended only 

attended_calls <- calls %>% filter(attended == "Yes")

# create response time variable

attended_calls$response_time <- as.period(ymd_hms(attended_calls$earliest_arrived_date_time) -  
                                     ymd_hms(attended_calls$earliest_deployed_date_time))

mean(time_length(attended_calls$response_time, unit = "minute"), na.rm = TRUE)



