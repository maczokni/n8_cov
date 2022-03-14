library(lubridate)
library(tsibble)
library(stringr)
library(readr)
library(sf)
library(dplyr)
library(ineq)
library(ggplot2)

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

# Read in data which has weekly count of calls (all calls) for each LSOA
weekly_count_by_lsoa <- read_csv("/Volumes/n8_covid/weekly_count_by_lsoa.csv")
weekly_asb_by_lsoa <- read_csv("/Volumes/n8_covid/weekly_asb_by_lsoa.csv")
weekly_admin_by_lsoa <- read_csv("/Volumes/n8_covid/weekly_admin_by_lsoa.csv")

  
  
# Shapefile for each LSOA if needed
cheshire_lsoas <- st_read("cheshire_lsoas.geojson")


# Read in all the calls so can revisit by type
cheshire_calls <- read_csv("/Volumes/n8_covid/cheshire_calls.csv")

weekly_gini <- weekly_count_by_lsoa %>% 
  group_by(inc_wk) %>% 
  summarise(gini = ineq(n), type = "Gini")

ggplot(weekly_gini, aes(x = inc_wk, y = gini)) + 
  geom_line() + 
  # Dates of interest
  geom_vline(aes(xintercept = date), data = dates, linetype = "12") +
  geom_label(aes(date, 0.35, label = row), data = dates, colour = "grey20") +
  scale_x_date(date_labels = "%e %b\n%Y", 
               limits = as.Date(c("2015-01-01", "2020-12-21")))+
  labs(
    title = "Gini coefficient* over time",
    subtitle = "*measure of inequality with 0 = perfect equality and 1 = maximal inequality",
    x = NULL,
    y = "Gini coefficient") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))



make_gini_plot <- function(x){
  
  weekly_gini <- x %>% 
    group_by(inc_wk) %>% 
    summarise(gini = ineq(n), type = "Gini")
  
  ggplot(weekly_gini, aes(x = inc_wk, y = gini)) + 
    geom_line() + 
    # Dates of interest
    geom_vline(aes(xintercept = date), data = dates, linetype = "12") +
    geom_label(aes(date, 0.35, label = row), data = dates, colour = "grey20") +
    scale_x_date(date_labels = "%e %b\n%Y", 
                 limits = as.Date(c("2015-01-01", "2020-12-21")))+
    labs(
      title = "Gini coefficient* over time",
      subtitle = "*measure of inequality with 0 = perfect equality and 1 = maximal inequality",
      x = NULL,
      y = "Gini coefficient") +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
}

make_gini_plot(weekly_asb_by_lsoa)


#############################################
#############################################

# Make df for pre- and post-cov ASB calls per LSOA

pre_cov_asb_lsoa <- weekly_asb_by_lsoa %>% 
  filter(yearweek(inc_wk) < yearweek(ymd("2020-01-31"))) %>% 
  group_by(lsoa) %>% 
  summarise(pre_num_asb_calls = sum(n))

post_cov_asb_lsoa <- weekly_asb_by_lsoa %>% 
  filter(yearweek(inc_wk) >= yearweek(ymd("2020-01-31"))) %>% 
  group_by(lsoa) %>% 
  summarise(post_num_asb_calls = sum(n))

map_df <- left_join(cheshire_lsoas, pre_cov_asb_lsoa, by = c("LSOA11NM" = "lsoa"))
map_df <- left_join(map_df, post_cov_asb_lsoa, by = c("LSOA11NM" = "lsoa"))

# # create neighbouts list (queen contiguity)
# map_df <- map_df %>% mutate(INTERSECT = st_intersects(.))
# 
# # converst to nb object
# as.nb.sgbp <- function(x, ...) {   
#   attrs <- attributes(x)   
#   x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )   
#   attributes(x) <- attrs   
#   class(x) <- "nb"   
#   x 
#   } 
# 
# w_sf <- as.nb.sgbp(map_df$INTERSECT)
# 
# class(w_sf)
# summary(w_sf)
# 
# plot(st_geometry(map_df))
# xy <- sp::coordinates(map_df)
# plot()



map_sp <- as(map_df, "Spatial")
library(sp)
library(spdep)

w <- poly2nb(map_sp, row.names=map_sp$LSOA11NM)
class(w)
summary(w)
ww <-  nb2listw(w, style='W')

moran(map_sp$pre_num_asb_calls, ww, n=length(ww$neighbours), S0=Szero(ww))
moran(map_sp$post_num_asb_calls, ww, n=length(ww$neighbours), S0=Szero(ww))


locm_pre <- localmoran(map_sp$pre_num_asb_calls, ww)
locm_post <- localmoran(map_sp$post_num_asb_calls, ww)

map_sp$pre_scale <- scale(map_sp$pre_num_asb_calls) %>% as.vector()
map_sp$post_scale <- scale(map_sp$post_num_asb_calls) %>% as.vector()

map_sp$pre_scale_lag <- lag.listw(ww, map_sp$pre_scale)
map_sp$post_scale_lag <- lag.listw(ww, map_sp$post_scale)


map_sf <- st_as_sf(map_sp) %>% 
  mutate(pre_quad_sig = ifelse(map_sp$pre_scale > 0 & 
                             map_sp$pre_scale_lag > 0 & 
                               locm_pre[,5] <= 0.05, 
                           "high-high",
                           ifelse(map_sp$pre_scale <= 0 & 
                                    map_sp$pre_scale_lag <= 0 & 
                                    locm_pre[,5] <= 0.05, 
                                  "low-low", 
                                  ifelse(map_sp$pre_scale > 0 & 
                                           map_sp$pre_scale_lag <= 0 & 
                                           locm_pre[,5] <= 0.05, 
                                         "high-low",
                                         ifelse(map_sp$pre_scale <= 0 & 
                                                  map_sp$pre_scale_lag > 0 & 
                                                  locm_pre[,5] <= 0.05,
                                                "low-high", 
                                                "non-significant")))),
         post_quad_sig = ifelse(map_sp$post_scale > 0 & 
                                 map_sp$post_scale_lag > 0 & 
                                 locm_post[,5] <= 0.05, 
                               "high-high",
                               ifelse(map_sp$post_scale <= 0 & 
                                        map_sp$post_scale_lag <= 0 & 
                                        locm_post[,5] <= 0.05, 
                                      "low-low", 
                                      ifelse(map_sp$post_scale > 0 & 
                                               map_sp$post_scale_lag <= 0 & 
                                               locm_post[,5] <= 0.05, 
                                             "high-low",
                                             ifelse(map_sp$post_scale <= 0 & 
                                                      map_sp$post_scale_lag > 0 & 
                                                      locm_post[,5] <= 0.05,
                                                    "low-high", 
                                                    "non-significant")))))

ggplot() + 
  geom_sf(data = map_sf, aes(fill = post_quad_sig), 
          lwd = 0.05, col = "white") + 
  theme_void()



ggplot() + 
  geom_sf(data = map_df, aes(fill = pre_num_asb_calls), 
          lwd = 0.05, col = "white") + 
  theme_void()

ggplot() + 
  geom_sf(data = map_df, aes(fill = post_num_asb_calls), 
          lwd = 0.05, col = "white") + 
  theme_void()


# Make df for pre- and post-cov admin calls per LSOA

pre_cov_admin_lsoa <- weekly_admin_by_lsoa %>% 
  filter(yearweek(inc_wk) < yearweek(ymd("2020-01-31"))) %>% 
  group_by(lsoa) %>% 
  summarise(pre_num = sum(n))

post_cov_admin_lsoa <- weekly_admin_by_lsoa %>% 
  filter(yearweek(inc_wk) >= yearweek(ymd("2020-01-31"))) %>% 
  group_by(lsoa) %>% 
  summarise(post_num = sum(n))

map_df <- left_join(cheshire_lsoas, pre_cov_admin_lsoa, by = c("LSOA11NM" = "lsoa"))
map_df <- left_join(map_df, post_cov_admin_lsoa, by = c("LSOA11NM" = "lsoa"))

# # create neighbouts list (queen contiguity)
# map_df <- map_df %>% mutate(INTERSECT = st_intersects(.))
# 
# # converst to nb object
# as.nb.sgbp <- function(x, ...) {   
#   attrs <- attributes(x)   
#   x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )   
#   attributes(x) <- attrs   
#   class(x) <- "nb"   
#   x 
#   } 
# 
# w_sf <- as.nb.sgbp(map_df$INTERSECT)
# 
# class(w_sf)
# summary(w_sf)
# 
# plot(st_geometry(map_df))
# xy <- sp::coordinates(map_df)
# plot()



map_sp <- as(map_df, "Spatial")
library(sp)
library(spdep)

w <- poly2nb(map_sp, row.names=map_sp$LSOA11NM)
class(w)
summary(w)
ww <-  nb2listw(w, style='W')

moran(map_sp$pre_num, ww, n=length(ww$neighbours), S0=Szero(ww))
moran(map_sp$post_num, ww, n=length(ww$neighbours), S0=Szero(ww))


locm_pre <- localmoran(map_sp$pre_num, ww)
locm_post <- localmoran(map_sp$post_num, ww)

map_sp$pre_scale <- scale(map_sp$pre_num) %>% as.vector()
map_sp$post_scale <- scale(map_sp$post_num) %>% as.vector()

map_sp$pre_scale_lag <- lag.listw(ww, map_sp$pre_scale)
map_sp$post_scale_lag <- lag.listw(ww, map_sp$post_scale)


map_sf <- st_as_sf(map_sp) %>% 
  mutate(pre_quad_sig = ifelse(.$pre_scale > 0 & 
                                 .$pre_scale_lag > 0 & 
                                 locm_pre[,5] <= 0.05, 
                               "high-high",
                               ifelse(.$pre_scale <= 0 & 
                                        .$pre_scale_lag <= 0 & 
                                        locm_pre[,5] <= 0.05, 
                                      "low-low", 
                                      ifelse(.$pre_scale > 0 & 
                                               .$pre_scale_lag <= 0 & 
                                               locm_pre[,5] <= 0.05, 
                                             "high-low",
                                             ifelse(.$pre_scale <= 0 & 
                                                      .$pre_scale_lag > 0 & 
                                                      locm_pre[,5] <= 0.05,
                                                    "low-high", 
                                                    "non-significant")))),
         post_quad_sig = ifelse(.$post_scale > 0 & 
                                  .$post_scale_lag > 0 & 
                                  locm_post[,5] <= 0.05, 
                                "high-high",
                                ifelse(.$post_scale <= 0 & 
                                         .$post_scale_lag <= 0 & 
                                         locm_post[,5] <= 0.05, 
                                       "low-low", 
                                       ifelse(.$post_scale > 0 & 
                                                .$post_scale_lag <= 0 & 
                                                locm_post[,5] <= 0.05, 
                                              "high-low",
                                              ifelse(.$post_scale <= 0 & 
                                                       .$post_scale_lag > 0 & 
                                                       locm_post[,5] <= 0.05,
                                                     "low-high", 
                                                     "non-significant")))))



my_fill_cols <- c("high-high" = "#33a02c", 
                  "low-low" = "#1f78b4",
                  "high-low" = "#b2df8a",
                  "low-high" = "#a6cee3", 
                  "non-significant" = "#D3D3D3")
p1 <- ggplot() + 
  geom_sf(data = map_sf, aes(fill = pre_quad_sig), 
          lwd = 0.05, col = "white") + 
  scale_fill_manual(values = my_fill_cols) +
  theme_void() + 
  guides(fill="none") + 
  labs(title = "Admin calls pre-COVID")

p2 <- ggplot() + 
  geom_sf(data = map_sf, aes(fill = post_quad_sig), 
          lwd = 0.05, col = "white") + 
  scale_fill_manual(name = "", values = my_fill_cols) +
  theme_void() +
  theme(legend.position = "bottom") + 
  labs(title = "Admin calls in 2020")


gridExtra::grid.arrange(p1, p2, ncol = 1)


ggplot() + 
  geom_sf(data = map_df, aes(fill = pre_num), 
          lwd = 0.05, col = "white") + 
  theme_void()

ggplot() + 
  geom_sf(data = map_df, aes(fill = post_num), 
          lwd = 0.05, col = "white") + 
  theme_void()


