library(tidyverse)
library(tsibble)
library(lubridate)
library(sf)

cheshire_calls <- read_csv("/Volumes/n8_covid/cheshire_calls.csv.csv")
cheshire_lsoas <- st_read("cheshire_lsoas.geojson")

week_filler <- data.frame(daysforfiller = seq(min(cheshire_calls$incident_date_time), 
                                              max(cheshire_calls$incident_date_time), 
                                              by="days"))
week_filler <- week_filler %>% 
  mutate(inc_wk = as_date(yearweek(daysforfiller))) %>%   
  group_by(inc_wk) %>% count() %>% select(inc_wk)
lsoa_filler <- bind_rows(data.frame(lsoa = unique(cheshire_lsoas$LSOA11NM)))


datalist <- list()
i <- 1
for (lsoaname in lsoa_filler$lsoa) {
  
  datalist[[i]] <- data.frame(lsoa = rep(lsoaname, nrow(week_filler)), 
                                       inc_wk = week_filler$inc_wk)
  
  i <- i + 1
  
}

week_and_lsoa_filler <- do.call(rbind, datalist)

weekly_count_by_lsoa <- cheshire_calls %>% 
  mutate(inc_wk = as_date(yearweek(incident_date_time))) %>%   
  group_by(lsoa, inc_wk) %>% count() %>% 
  left_join(week_and_lsoa_filler, .) %>%
  mutate(n = replace_na(n, 0)) %>% 
  left_join(., cheshire_lsoas %>% st_drop_geometry(), by = c("lsoa" = "LSOA11NM"))


write.csv(weekly_count_by_lsoa, "/Volumes/n8_covid/weekly_count_by_lsoa.csv")


weekly_asb_by_lsoa <- cheshire_calls %>% 
  filter(incident_type_new == "ASB") %>% 
  mutate(inc_wk = as_date(yearweek(incident_date_time))) %>%   
  group_by(lsoa, inc_wk) %>% count() %>% 
  left_join(week_and_lsoa_filler, .) %>%
  mutate(n = replace_na(n, 0)) %>% 
  left_join(., cheshire_lsoas %>% st_drop_geometry(), by = c("lsoa" = "LSOA11NM"))


write.csv(weekly_asb_by_lsoa, "/Volumes/n8_covid/weekly_asb_by_lsoa.csv")

weekly_misper_by_lsoa <- cheshire_calls %>% 
  filter(incident_type_new == "Missing Person") %>% 
  mutate(inc_wk = as_date(yearweek(incident_date_time))) %>%   
  group_by(lsoa, inc_wk) %>% count() %>% 
  left_join(week_and_lsoa_filler, .) %>%
  mutate(n = replace_na(n, 0)) %>% 
  left_join(., cheshire_lsoas %>% st_drop_geometry(), by = c("lsoa" = "LSOA11NM"))


write.csv(weekly_misper_by_lsoa, "/Volumes/n8_covid/weekly_misper_by_lsoa.csv")

weekly_dv_by_lsoa <- cheshire_calls %>% 
  filter(incident_type_new == "Domestic Incident") %>% 
  mutate(inc_wk = as_date(yearweek(incident_date_time))) %>%   
  group_by(lsoa, inc_wk) %>% count() %>% 
  left_join(week_and_lsoa_filler, .) %>%
  mutate(n = replace_na(n, 0)) %>% 
  left_join(., cheshire_lsoas %>% st_drop_geometry(), by = c("lsoa" = "LSOA11NM"))


write.csv(weekly_dv_by_lsoa, "/Volumes/n8_covid/weekly_dv_by_lsoa.csv")

weekly_violence_by_lsoa <- cheshire_calls %>% 
  filter(incident_type_new == "Violence/Harassment") %>% 
  mutate(inc_wk = as_date(yearweek(incident_date_time))) %>%   
  group_by(lsoa, inc_wk) %>% count() %>% 
  left_join(week_and_lsoa_filler, .) %>%
  mutate(n = replace_na(n, 0)) %>% 
  left_join(., cheshire_lsoas %>% st_drop_geometry(), by = c("lsoa" = "LSOA11NM"))


write.csv(weekly_violence_by_lsoa, "/Volumes/n8_covid/weekly_violence_by_lsoa.csv")




