library(lubridate)
library(readxl)
library(tsibble)
library(tidyverse)

excel_sheets("../cheshire-calls/N8Data - Submitted Version.xlsx")

calls <- bind_rows(
  read_excel("../cheshire-calls/N8Data - Submitted Version.xlsx", sheet = 1),
  read_excel("../cheshire-calls/N8Data - Submitted Version.xlsx", sheet = 2)
) %>% 
  janitor::clean_names()

calls <- janitor::clean_names(calls)

minor_categories <- calls %>% 
  count(incident_type) %>% 
  filter(n < 1000) %>% 
  pull(incident_type)

calls %>% 
  mutate(incident_type_new = case_when(
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
  )) %>% 
  count(incident_type_new) %>% 
  View(title = "incident_type_new")


calls %>% 
  filter(incident_type %in% c("Drugs")) %>% 
  mutate(week = as_date(yearweek(incident_date_time))) %>% 
  count(week) %>% 
  ggplot(aes(x = week, y = n)) +
  geom_point() +
  geom_smooth(formula = y ~ x, method = "loess", span = 0.2) +
  # First UK case
  geom_vline(xintercept = as.Date("2020-01-23"), linetype = "33") +
  # First UK lockdown begins
  geom_vline(xintercept = as.Date("2020-03-23"), linetype = "12") +
  # First UK lockdown ends
  geom_vline(xintercept = as.Date("2020-06-01"), linetype = "42") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b\n%Y") +
  scale_y_continuous(limits = c(0, NA), labels = scales::comma_format()) +
  theme_minimal()
