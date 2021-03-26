
# NEW PREDICTORS -------------

#create two new predictors (change in system/inspection)
  #0 = before, 1= after
calls <- calls %>%
  mutate(datetime = ymd_hms(incident_date_time), 
         date = date(datetime), 
         system_change = if_else(date < ymd("2017-06-03"), 0,1))

calls <- calls %>%
  mutate(datetime = ymd_hms(incident_date_time), 
         date = date(datetime), 
         inspection = if_else(date < ymd("2017-05-15"), 0,1))


# COUNT CRIME ---------------
call_count <- calls %>% 
  filter(!is.na(incident_type)) %>% 
  mutate(week = as_date(yearweek(incident_date_time))) %>% 
  count(incident_type, week, name = "calls") %>%
  as_tsibble(key = c(incident_type), index = week) %>%
  fill_gaps(calls = 0)



#ESTIMATE MODEL ------------
#check for implicit gaps in time series - how?

call_model <- call_count %>%
  #use date before first U.K case 
  filter(week < ymd("2020-01-23")) %>%
  model(arima = ARIMA(calls ~ trend() + season()))




#FORECAST MODEL -------------

