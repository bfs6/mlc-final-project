rm(list = ls())

####Read in Libraries####
library(data.table)
library(tidyverse)
library(janitor)
library(tidycensus)
library(ggmap)
library(geosphere)
library(RSocrata)
library(httr)
library(flextable)
library(nycgeo)
library(sf)
library(scales)
library(ggnewscale)


####Read in Data####
##Hyper Local Temperature Data
#hyperlocal_temp_url <- "https://data.cityofnewyork.us/resource/qdq3-9eqn.csv"

hyper_temp_data_raw <- 
  read.socrata(
    "https://data.cityofnewyork.us/resource/qdq3-9eqn.json",
    app_token = "ZJtMSj0yoibrMVRWUtddgbgBx",
    email     = "basilfseif@gmail.com",
    password  = "NYC1234!"
  ) %>% 
  clean_names()

hyper_temp_data <-
  hyper_temp_data_raw %>%
  dplyr::select(sensor_id, longitude, latitude) %>% 
  group_by(sensor_id) %>% 
  slice(1) %>%
  ungroup() %>% 
  right_join(hyper_temp_data_raw %>% 
               dplyr::select(-c(longitude, latitude)), by = "sensor_id")

rm(hyper_temp_data_raw)

##Cooling Sites Data
cooling_sites_url <- "https://data.cityofnewyork.us/resource/h2bn-gu9k.csv"
cooling_sites_data <- 
  cooling_sites_url %>% 
  fread(sep = ",", header = TRUE, stringsAsFactors = FALSE) %>% 
  clean_names()  %>% 
  mutate(propertyname_full = paste0(propertyname, ", ", borough)) %>% 
  filter(status == "Activated")

# register_google(key = "AIzaSyCKqscWU_VbFRaFZ3NQgNT8G_dV42mE7DY")
# locations <- cooling_sites_data$propertyname_full
# results_list <- lapply(locations, geocode)
# results <- do.call(rbind, results_list)
# results$propertyname_full <- locations
# geocode_results <- 
#   results %>% 
#   unique()

my_sf_df <- st_as_sf(data.frame(x = cooling_sites_data$x, y = cooling_sites_data$y), coords = c("x", "y"), crs = 2263) 
my_latlon_df <- st_transform(my_sf_df, crs = 4326 ) 
my_latlon_df <- my_latlon_df%>%
  mutate( lon= st_coordinates(my_latlon_df)[,1],
          lat = st_coordinates(my_latlon_df)[,2])
my_latlon_df


cooling_sites_data_full <- 
  cooling_sites_data %>% 
  mutate(longitude = my_latlon_df$lon, latitude = my_latlon_df$lat)

##ACS Census Data
options(tigris_use_cache = TRUE)
nyc_census_list <- list()
years <- 2016:2018
counties <- c("New York", "Kings", "Bronx", "Queens", "Richmond")
api_key <- "84be34aceeb52f73ac43fb13c4a84d49b073c0c9"
census_api_key(api_key, install = TRUE, overwrite = TRUE)
desired_vars = c(
  all_pop = "DP05_0028",
  white_pop = "DP05_0059",
  baa_pop = "DP05_0060",
  hisp_pop = "DP05_0066",
  native_american_pop = "DP05_0061",
  asian_pop = "DP05_0062",
  hawaiian_pacific_islander_pop = "DP05_0063",
  other_pop = "DP05_0064",
  avg_family_size = "DP02_0016",
  avg_household_size = "DP02_0015",
  homeowner_vacancy_rate = "DP04_0004",
  median_age = "DP05_0017",
  #percent_below_poverty_level = "DP03_0128"
  per_capita_income = "DP03_0088"
)

for(i in seq_along(years)){
  for(j in seq_along(counties)){
    dat <- 
      get_acs(
        state = "NY",
        county = counties[j],
        geography = "tract",
        variables = desired_vars,
        #summary_var = "P2_001N",
        geometry = TRUE,
        year = years[i]
      ) %>% 
      mutate(year = years[i])
    counter = 5 * (i - 1) + j
    nyc_census_list[[counter]] <- dat 
  }
}

nyc_census <- 
  nyc_census_list %>% 
  bind_rows() %>% 
  separate(col = "NAME",
           into = c("census_tract", "county", "state"),
           sep = ",") %>% 
  mutate(county = str_trim(county, "both")) %>% 
  dplyr::select(-c(moe, state)) %>%
  mutate(borough = case_when(county == "New York County" ~ "Manhattan",
                             county == "Kings County" ~ "Brooklyn",
                             county == "Bronx County" ~ "Bronx",
                             county == "Queens County" ~ "Queens",
                             county == "Richmond County" ~ "Staten Island")) %>% 
  pivot_wider(names_from = "variable",
              values_from = "estimate")


####Merge Cooling Sites and Hyper Local Temperature Datasets####
##Find Closest Cooling Stations to Each Sensor
hyper_temp_sensors <- 
  hyper_temp_data %>% 
  dplyr::select(sensor_id, latitude, longitude) %>% 
  distinct()

dist_cooling_stations_list <- list()
for(i in seq_len(nrow(hyper_temp_sensors))){
  cooling_site_types <- data.frame(featuretype = unique(cooling_sites_data$featuretype))
  hyper_temp_sensor_ind <- as.numeric(c(hyper_temp_sensors$longitude[i], hyper_temp_sensors$latitude[i]))
  coords <- cbind(cooling_sites_data_full$lon, cooling_sites_data_full$lat)
  distances_km <- distm(hyper_temp_sensor_ind, coords, fun = distHaversine) / 1000
  distances_mi <- distances_km * 0.621371
  min_dist <- distances_mi[which.min(distances_mi)]
  closest_cooling_site_type <- cooling_sites_data$featuretype[which.min(distances_mi)]
  num_cooling_stations_w_in_1_mi <- length(which(distances_mi <= 1))
  
  stations_w_in_1_mi <- 
    cooling_sites_data[which(distances_mi <= 1)] %>% 
    group_by(featuretype) %>% 
    dplyr::summarize(count = n()) %>% 
    right_join(cooling_site_types, by = "featuretype") %>% 
    mutate(count = replace_na(count, 0),
           featuretype = 
             featuretype %>% 
             str_to_lower() %>% 
             str_replace_all(" ", "_"),
           featuretype = paste0(paste0("cooling_station_w_in_1_mi", featuretype))) %>% 
    pivot_wider(names_from = "featuretype",
                values_from = "count")
  
  dist_cooling_stations <- 
    data.frame(cooling_station_min_dist = min_dist, 
               closest_cooling_site_type = closest_cooling_site_type, 
               num_cooling_stations_w_in_1_mi = num_cooling_stations_w_in_1_mi) %>% 
    cbind(stations_w_in_1_mi)
  
  dist_cooling_stations_list[[i]] <- dist_cooling_stations
}

dist_cooling_stations_full <- 
  dist_cooling_stations_list %>% 
  bind_rows()

hyper_temp_sensors_full <- 
  hyper_temp_sensors %>% 
  bind_cols(dist_cooling_stations_full)


####Merge Census Data####
# Geocode the points to get census tracts
coord_tracts <- 
  hyper_temp_sensors_full %>% 
  dplyr::select(latitude, longitude) 

get_census_tract <- function(latitude, longitude) {
  
  # Get the county and state FIPS codes
  url <- paste0("https://geo.fcc.gov/api/census/block/find?latitude=", latitude, "&longitude=", longitude, "&format=json")
  response <- GET(url)
  content <- content(response, "text")
  json_content <- jsonlite::fromJSON(content, simplifyDataFrame = TRUE)
  
  # Get the census tract using the tidycensus package
  tract <- substr(json_content$Block$FIPS, 1, 11)
  
  # Return the census tract name
  return(tract)
}

tracts <- 
  coord_tracts %>% 
  pmap(get_census_tract) %>% 
  unlist()

hyper_temp_sensors_full$tract_id <- tracts

hyper_temp_sensors_total <-
  hyper_temp_data %>%
  left_join(hyper_temp_sensors_full %>% 
              dplyr::select(-c(latitude, longitude)), by = c("sensor_id")) %>% 
  left_join(nyc_census %>% 
              filter(year == 2018) %>% 
              dplyr::select(-c(year, borough)), by = c("tract_id" = "GEOID")) %>% 
  dplyr::select(-geometry)

rm(hyper_temp_data)


####Read in Data####
fwrite(hyper_temp_sensors_total, "data/hyper_temp_full_data_mlc.csv", row.names = FALSE)


####Create Table for # Cooling Centers/Borough####
##Create Table
all_pop_data <- 
  nyc_census %>% 
  as.data.frame() %>%
  dplyr::select(borough, all_pop) %>%
  group_by(borough) %>% 
  dplyr::summarize(pop = sum(all_pop, na.rm = TRUE))

cooling_sites_table <- 
  cooling_sites_data %>% 
  group_by(featuretype, borough) %>% 
  dplyr::summarize(count = n()) %>% 
  ungroup() %>% 
  group_by(borough) %>%
  mutate(Total = sum(count)) %>% 
  ungroup() %>% 
  left_join(all_pop_data, by = "borough") %>%
  mutate(Total_per_capita = 100000 * Total/pop) %>% 
  dplyr::select(-pop) %>% 
  pivot_wider(names_from = "featuretype", values_from = "count") %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  column_to_rownames("borough") %>% 
  relocate("Other", .after = last_col()) %>% 
  relocate("Total", .after = last_col()) %>% 
  relocate("Total_per_capita", .after = last_col()) %>% 
  t() %>% 
  as.data.frame() %>% 
  rownames_to_column("Cooling Station Type") %>% 
  mutate_if(is.character, ~str_replace_all(.x, "Total_per_capita", "Total per 100,000")) 

cooling_sites_table_final <- 
  cooling_sites_table[-nrow(cooling_sites_table),] %>% 
  mutate(across(is.numeric, ~as.character(as.integer(.)))) %>%
  bind_rows(
    cooling_sites_table[nrow(cooling_sites_table),] %>% 
      mutate(across(is.numeric, 
                    ~round(., 3) %>% 
                      as.character()))
  )
  
table1 <- 
  flextable(cooling_sites_table_final) %>% 
  autofit()

#png_file <- tempfile(tmpdir = paste0(getwd(), "/outputs"), fileext = "cooling_sites_table.png")
save_as_image(table1, path = paste0(getwd(), "/outputs/cooling_sites_table.png"))


####Map of Cooling Centers####
x_abhi <- 
  "data-raw/X.csv" %>% 
  fread(sep = ",", header = FALSE, stringsAsFactors = FALSE) %>% 
  dplyr::select(V1, V2) %>% 
  rename("longitude" = V1,
         "latitude" = V2)   

y_abhi <- 
  "data-raw/y.csv" %>% 
  fread(sep = ",", header = FALSE, stringsAsFactors = FALSE) %>% 
  rename("y_True" = "V1") 

y_pred_abhi <- 
  "data-raw/y_pred.csv" %>% 
  fread(sep = ",", header = FALSE, stringsAsFactors = FALSE) %>% 
  rename("y_Predicted" = "V1")

abhi_dat <- 
  x_abhi %>% 
  bind_cols(y_abhi) %>% 
  bind_cols(y_pred_abhi) %>% 
  pivot_longer(cols = c(y_True, y_Predicted), names_to = "type", values_to = "y") %>% 
  mutate(type = 
           type %>% 
           str_replace_all("y_", ""))
  


##Map
nyc_tract <- 
  nyc_boundaries(
  geography = 'tract',
) %>% 
st_transform(2263)

cooling_stations_sf <- 
  cooling_sites_data_full %>% 
  st_as_sf(coords = c("longitude","latitude"),
           crs = 4326) %>% 
  st_transform(crs = 2263)

abhi_dat_sf <- 
  abhi_dat %>% 
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(crs = 2263)
 



# plot community districts
nyc_tract_plot <- 
  ggplot() +
  geom_sf(
    data = nyc_tract,
    color = "white",
    lwd = 0.2,
    aes(fill = borough_name),
    alpha = 0.8) +
  geom_sf(
    data = cooling_stations_sf,
    aes(color = "Cooling Sites"),
    alpha = 0.8,
    show.legend = "point",
    pch = 21,
    color = "white",
    fill = "red") +
  scale_fill_manual(values = c("lightblue", "deepskyblue4",
                               muted("blue"),
                               muted("purple"), "darkblue"),
                    guide = guide_legend(override.aes = list(
                      linetype = "blank",
                      shape = NA
                    ))) +
  scale_color_manual(values = c("Cooling Sites" = "white"),
                     guide = guide_legend(
                       override.aes = list(
                         pch = 21,
                         color = "white",
                         fill = "red",
                         linetype = "blank"))) +
  labs(fill = "Borough", 
       title = "NYC Cooling Stations by Borough") +
  theme_void() +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid = element_line(color = "transparent"))

first_model_results <- 
  ggplot() +
  geom_sf(
    data = nyc_tract,
    color = "white",
    lwd = 0.2,
    aes(fill = borough_name),
    alpha = 0.8) +
  scale_fill_manual(values = c("lightblue", "deepskyblue4",
                               muted("blue"),
                               muted("purple"), "darkblue"),
                    guide = guide_legend(override.aes = list(
                      linetype = "blank",
                      shape = NA
                    ))) +
  labs(fill = "Borough", 
       title = "Risk of Heat-Related Illness by Borough") +
  new_scale_fill() +
  geom_sf(
    data = abhi_dat_sf,
    aes(fill = type,
        alpha = y,
        size = y),
    show.legend = "point",
    pch = 21) +
  labs(size = "Risk of Heat-Related Illness",
       alpha = "Risk of Heat-Related Illness") +
  scale_fill_manual(values = c("red", "yellow")) +
  theme_void() +
  theme(plot.title.position = 'plot', 
        plot.title = element_text(hjust = 0.5)) +
  theme(panel.grid = element_line(color = "transparent"))

# save plot
ggsave("plots/nyc_tract_plot.png", plot = nyc_tract_plot)







