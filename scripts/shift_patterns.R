library(lubridate)
library(tsibble)
library(tidyverse)
# devtools::install_github('Ather-Energy/ggTimeSeries')
library(ggTimeSeries)
library(fmsb)

# reka path
calls <- read_csv("/Volumes/n8_covid/n8_data.csv.gz") %>% # USE CORRECT PATH FOR CLEAN DATA FILE ON YOUR COMPUTER
  janitor::clean_names()

# calls <- read_csv("Z:\\n8_data_v2.csv.gz") %>%  janitor::clean_names()

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
daily_count <- calls %>% 
  group_by(inc_date) %>% 
  count() 

  ggplot_calendar_heatmap(daily_count,  
  cDateColumnName = 'inc_date', # column name of the dates
  cValueColumnName = 'n') + # column name of the data
  xlab(NULL) + # x axis lable
  ylab(NULL) + # y axis lable
  scale_fill_continuous(low = '#f7fcfd',  # set colour for low count
                        high = '#6e016b') +   # set colour for high count
  facet_wrap(~Year, ncol = 1) +  # separate out by each year
  theme_minimal()
  
# without that one Thursday

  ggplot_calendar_heatmap(daily_count %>% filter(inc_date != ymd('2017-02-23')),  
                          cDateColumnName = 'inc_date', # column name of the dates
                          cValueColumnName = 'n') + # column name of the data
    xlab(NULL) + # x axis lable
    ylab(NULL) + # y axis lable
    scale_fill_continuous(low = '#f7fcfd',  # set colour for low count
                          high = '#6e016b') +   # set colour for high count
    facet_wrap(~Year, ncol = 1) +  # separate out by each year
    theme_minimal()
  
  

# try for radar chart for pre 2020
  
  dow_count_pre <- calls %>% 
    filter(inc_date < dmy('31-01-2020')) %>% 
    mutate(dow = wday(inc_date, label = TRUE), 
           yr_wk = yearweek(inc_date)) %>% 
    group_by(dow, yr_wk) %>% 
    count() %>% 
    group_by(dow) %>% 
    summarise(avg_calls = round(mean(n, na.rm = TRUE),0))
  
wide_dow_pre <- dow_count_pre %>% 
  pivot_wider(names_from = dow, values_from = avg_calls)

rownames(wide_dow_pre) <- "pre-pandemic"

# now for 2020

dow_count_2020 <- calls %>% 
  filter(inc_date >= dmy('31-01-2020')) %>% 
  mutate(dow = wday(inc_date, label = TRUE), 
         yr_wk = yearweek(inc_date)) %>% 
  group_by(dow, yr_wk) %>% 
  count() %>% 
  group_by(dow) %>% 
  summarise(avg_calls = round(mean(n, na.rm = TRUE),0))

wide_dow_2020 <- dow_count_2020 %>% 
  pivot_wider(names_from = dow, values_from = avg_calls)

rownames(wide_dow_2020) <- "2020"

# bind together
radar_df <- rbind(rep(max(dow_count_pre$avg_calls)+10, 7),
                      rep(min(dow_count_2020$avg_calls)-10, 7), 
                      wide_dow_pre, 
                      wide_dow_2020)

# add colours
line_cols <- c('#7570b3', '#1b9e77')

# make chart
radarchart(radar_df, pcol = line_cols, plwd = 4, plty = 1)
legend(x=0.8, y=1.4, legend = c("Pre-pandemic", "2020"), bty = "n", pch=20 , col=line_cols , text.col = "black", cex=1.2)

# Do the same for shift patterns

calls$time <- factor(calls$time, levels = c("Early", "Late", "Evening", "Daytime"))


# try for radar chart for pre 2020

time_count_pre <- calls %>% 
  filter(inc_date < dmy('31-01-2020')) %>% 
  mutate(yr_wk = yearweek(inc_date)) %>% 
  group_by(time, yr_wk) %>% 
  count() %>% 
  group_by(time) %>% 
  summarise(avg_calls = round(mean(n, na.rm = TRUE),0))

wide_time_pre <- time_count_pre %>% 
  pivot_wider(names_from = time, values_from = avg_calls)

rownames(wide_time_pre) <- "pre-pandemic"

# now for 2020

time_count_2020 <- calls %>% 
  filter(inc_date >= dmy('31-01-2020')) %>% 
  mutate(yr_wk = yearweek(inc_date)) %>% 
  group_by(time, yr_wk) %>% 
  count() %>% 
  group_by(time) %>% 
  summarise(avg_calls = round(mean(n, na.rm = TRUE),0))

wide_time_2020 <- time_count_2020 %>% 
  pivot_wider(names_from = time, values_from = avg_calls)

rownames(wide_time_2020) <- "2020"

# bind together
radar_df <- rbind(rep(max(time_count_pre$avg_calls)+10, 7),
                  rep(min(time_count_2020$avg_calls)-10, 7), 
                  wide_time_pre, 
                  wide_time_2020)

# add colours
line_cols <- c('#7570b3', '#1b9e77')

# make chart
radarchart(radar_df, pcol = line_cols, plwd = 4, plty = 1)
legend(x=0.8, y=1.4, legend = c("Pre-pandemic", "2020"), bty = "n", pch=20 , col=line_cols , text.col = "black", cex=1.2)


# Can break down by hour if we want...  



# try for radar chart for pre 2020

hour_count_pre <- calls %>% 
  filter(inc_date < dmy('31-01-2020')) %>% 
  mutate(yr_wk = yearweek(inc_date)) %>% 
  group_by(hour, yr_wk) %>% 
  count() %>% 
  group_by(hour) %>% 
  summarise(avg_calls = round(mean(n, na.rm = TRUE),0))

wide_hour_pre <- hour_count_pre %>% 
  pivot_wider(names_from = hour, values_from = avg_calls)

rownames(wide_hour_pre) <- "pre-pandemic"

# now for 2020

hour_count_2020 <- calls %>% 
  filter(inc_date >= dmy('31-01-2020')) %>% 
  mutate(yr_wk = yearweek(inc_date)) %>% 
  group_by(hour, yr_wk) %>% 
  count() %>% 
  group_by(hour) %>% 
  summarise(avg_calls = round(mean(n, na.rm = TRUE),0))

wide_hour_2020 <- hour_count_2020 %>% 
  pivot_wider(names_from = hour, values_from = avg_calls)

rownames(wide_hour_2020) <- "2020"

# bind together
radar_df <- rbind(rep(max(hour_count_pre$avg_calls)+10, 7),
                  rep(min(hour_count_2020$avg_calls)-10, 7), 
                  wide_hour_pre, 
                  wide_hour_2020)

# add colours
line_cols <- c('#7570b3', '#1b9e77')

# make chart
radarchart(radar_df[,c(1, 24:2)], pcol = line_cols, plwd = 4, plty = 1)
legend(x=0.8, y=1.4, legend = c("Pre-pandemic", "2020"), bty = "n", pch=20 , col=line_cols , text.col = "black", cex=1.2)


