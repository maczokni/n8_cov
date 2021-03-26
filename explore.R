library(readr)
library(dplyr)
library(lubridate)
library(forecast)
library(fable)
library(tsibble)


calls <- read_csv("/Volumes/n8_covid/n8_data.csv.gz")
calls <- read_csv("Z:\\n8_data_v2.csv.gz")

calls %>% group_by(call_origin) %>% count() %>% View()

calls <- calls %>% mutate(year = year(ymd_hms(incident_date_time)))

# 
gitignore::gi_write_gitignore("r")
