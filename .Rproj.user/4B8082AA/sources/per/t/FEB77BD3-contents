library(readr)
library(dplyr)
library(lubridate)


calls <- read_csv("/Volumes/n8_covid/n8_data.csv.gz")

calls %>% group_by(call_origin) %>% count() %>% View()

calls <- calls %>% mutate(year = year(ymd_hms(incident_date_time)))

# 
gitignore::gi_write_gitignore("r")
