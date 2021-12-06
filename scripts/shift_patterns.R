library(lubridate)
library(tsibble)
library(tidyverse)
# devtools::install_github('Ather-Energy/ggTimeSeries')
library(ggTimeSeries)

# reka path
# calls <- read_csv("/Volumes/n8_covid/n8_data.csv.gz") %>% # USE CORRECT PATH FOR CLEAN DATA FILE ON YOUR COMPUTER
#   janitor::clean_names()

calls <- read_csv("Z:\\n8_data_v2.csv.gz") %>%  janitor::clean_names()

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
    title = "Proportion of all Incidents by Time of Day",
    x = NULL,
    y = "Proportion",
    fill = "Time of Day"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +    guides(fill=guide_legend(nrow=1,byrow=TRUE))

calls <- calls %>% 
  mutate(inc_date = floor_date(ymd_hms(incident_date_time), unit = "day"))



# Calendar plot for all calls
calls %>% 
  group_by(inc_date) %>% 
  count() %>% 
  ggplot_calendar_heatmap(
  .,  
  cDateColumnName = 'inc_date', # column name of the dates
  cValueColumnName = 'n') + # column name of the data
  xlab(NULL) + # x axis lable
  ylab(NULL) + # y axis lable
  scale_fill_continuous(low = '#f7fcfd',  # set colour for low count
                        high = '#6e016b') +   # set colour for high count
  facet_wrap(~Year, ncol = 1) +  # separate out by each year
  theme_minimal()


