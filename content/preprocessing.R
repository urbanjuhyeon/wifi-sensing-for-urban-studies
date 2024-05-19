# Load necessary packages
if (!require(pacman)) install.packages("pacman")
pacman::p_load(data.table, lubridate, DBI, RSQLite, stringr, purrr, tidyverse, sf)

#### 1. Data aggregation ####
##### 1.1 List and filter files ####
files <- list.files(path = "D:/Experiments/uou20/WiFi", pattern = "\\.sqlite3$", full.names = TRUE, recursive = TRUE)
filtered_files <- files[grepl("wifi_2020-07-12", files)]
print(filtered_files)

#####  1.2 Create directory if it doesn't exist ##### 
output_dir <- "D:/Experiments/uou20/WiFi/240518"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

#####  1.3 Define the aggregation function ##### 
aggregate_data <- function(db_path, start_date, end_date, interval = "second", output_suffix = "_1second.csv") {
  conn <- dbConnect(SQLite(), db_path)
  
  wifi_data <- dbGetQuery(conn, "SELECT timestamp, type, subtype, strength as rssi, source_address FROM packets")
  setDT(wifi_data)
  
  path_components <- strsplit(db_path, "/|\\\\")[[1]]
  sensor_id <- path_components[length(path_components) - 1]
  wifi_data[, sensor_name := sensor_id]
  
  wifi_data <- wifi_data[between(ymd_hms(timestamp), start_date, end_date)]
  
  wifi_data <- wifi_data[!grepl("response", subtype)]
  wifi_data <- wifi_data[between(rssi, -80, -30)]
  
  wifi_data[, timestamp := floor_date(ymd_hms(timestamp), unit = interval)]
  aggregated_data <- wifi_data[, .(median_rssi = median(rssi), count = .N), by = .(sensor_name, source_address, timestamp)]
  
  sensor_name <- if (nrow(aggregated_data) > 0) aggregated_data[1]$sensor_name else "unknown_sensor"
  output_filename <- paste0(sensor_name, "_", basename(sub("\\.sqlite3$", output_suffix, db_path)))
  output_path <- file.path(output_dir, output_filename)
  
  fwrite(aggregated_data, output_path)
  
  dbDisconnect(conn)
  gc()
}

##### 1.4. Conduct data aggregation ####
start_date <- ymd_hms("2020-07-12 10:00:00")
end_date <- ymd_hms("2020-07-12 14:00:00")
map(filtered_files, ~aggregate_data(.x, start_date, end_date, interval = "second"))

#### 2. Data Cleaning ####
##### 2.1 Load and combine the aggregated data ####
files <- list.files("D:/Experiments/uou20/WiFi/240518", full.names = TRUE)
# files containing "wifi" and ".csv" with size greater than 10KB
files <- files[grepl("wifi", files) & grepl("\\.csv$", files) & file.size(files) > 10000]
wf_1a <- rbindlist(lapply(files, fread))

fwrite(wf_1a, "D:/Experiments/uou20/WiFi/240518/wf_1a.csv")
wf_1a <- fread("D:/Experiments/uou20/WiFi/240518/wf_1a.csv")

wf_1a %>% 
  group_by(sensor_name) %>% 
  summarise(n = n()) %>% 
  arrange((sensor_name)) %>% 
  print(n = Inf)

##### 2.2 Select sensors interested in ####
sensors_selected <- c("U-01", "U-02", "U-04", "U-05", "U-06",
                      "U-07", "U-09", "U-10", "U-12", "U-13",
                      "U-15", "U-16", "U-17", "U-18", "U-19", 
                      "U-20")

wf_1b <- wf_1a[sensor_name %in% sensors_selected]

##### 2.3 function to clean the data ####
clean_wifi_data <- function(
    db_wf_combined, sensor_threshold_percentile, continuous_detection_threshold, total_detection_time_threshold) {
  # Load and combine the aggregated data
  #wf_combined <- rbindlist(lapply(csv_files, fread))
  RANDOMIZED_SECOND_ORDER <- c("2", "6", "a", "e")
  wf_filtered_1 <- db_wf_combined[!(as.character(str_sub(source_address, 2, 2)) %in% RANDOMIZED_SECOND_ORDER)]
  
  # Step 2: Identify and remove non-mobile devices
  device_sensor_counts <- wf_filtered_1[, .(sensor_count = uniqueN(sensor_name)), by = source_address]
  low_detection_threshold <- quantile(device_sensor_counts$sensor_count, sensor_threshold_percentile)
  low_detection_devices <- device_sensor_counts[sensor_count <= low_detection_threshold, source_address]
  
  wf_filtered_2 <- wf_filtered_1[source_address %in% low_detection_devices]
  wf_filtered_2[, time_diff := as.numeric(difftime(timestamp, shift(timestamp, type = "lag"), units = "secs")), by = .(source_address)]
  wf_filtered_2[, continuous_detection := cumsum(time_diff > continuous_detection_threshold | is.na(time_diff)), by = .(source_address)]
  wf_filtered_2[, total_continuous_time := sum(time_diff[time_diff <= continuous_detection_threshold], na.rm = TRUE), by = .(sensor_name, source_address, continuous_detection)]
  non_mobile_devices <- wf_filtered_2[total_continuous_time >= total_detection_time_threshold, .(source_address)]
  non_mobile_devices <- unique(non_mobile_devices$source_address)
  
  wf_final <- wf_filtered_1[!source_address %in% non_mobile_devices]
  
  return(wf_final)
}

# Define parameters for data cleaning
sensor_threshold_percentile <- 0.3  # Devices detected by fewer sensors: the 30th percentile
continuous_detection_threshold <- 300  # 5 minutes (300 seconds)
total_detection_time_threshold <- 3600 * 2  # Total 2 hours (7200 seconds)

# Clean the WiFi data
wf_1c <- clean_wifi_data(wf_1b, sensor_threshold_percentile, continuous_detection_threshold, total_detection_time_threshold)
print(head(wf_1c))

# Save the cleaned WiFi data to a CSV file
fwrite(wf_1c, "D:/Experiments/uou20/WiFi/240518/wf_cleaned.csv")

#### 3. Data Localization ####

##### 3.1 Load the cleaned data ####
wf_2a <- wf_1c[, timestamp := as.POSIXct(timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")]
wf_2a[, timestamp_mid := floor_date(timestamp, "30 seconds") + seconds(15)]

wifi_data_30s <- wf_2a[, .(
  median_rssi  = round(median(median_rssi)), count = .N),
  by = .(source_address, sensor_name, timestamp = timestamp_mid)]

head(wifi_data_30s)

##### 3.2 Load the sensor shapefile ####
shp_sensor <- st_read("D:/Experiments/2019_UNIST/data/shp/WiFi_sensor.shp") %>%
  filter(id_last %in% sensors_selected)

shp_sensor <- setDT(shp_sensor)

shp_sensor[, `:=` (x_sensor = st_coordinates(geometry)[,1],
                   y_sensor = st_coordinates(geometry)[,2])]

sensor_coords <- shp_sensor[, .(sensor_name = id_last, x_sensor, y_sensor)]

head(sensor_coords)

##### 3.3 Define the wcentroid function ####
wifi_data_30s <- merge(wifi_data_30s, sensor_coords, by = "sensor_name")

wcentroid_dt <- function(data) {
  weighted_x <- sum(data$x_sensor * data$median_rssi) / sum(data$median_rssi)
  weighted_y <- sum(data$y_sensor * data$median_rssi) / sum(data$median_rssi)
  list(X = weighted_x, Y = weighted_y)
}


localized_data <- wifi_data_30s[, wcentroid_dt(.SD), by = .(source_address, timestamp)]

##### 3.4 Plot the localized data ####
ggplot(localized_data, aes(x = X, y = Y)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Localized WiFi Device Positions",
       x = "X Coordinate",
       y = "Y Coordinate")


## 한 기기당 감지한 센서의 개수 data.table
device_sensor_counts <- wf_1c[, .(sensor_count = uniqueN(sensor_name)), by = source_address][
  , .N, by = sensor_count][order(sensor_count)]

