# library(jsonlite)
# library(tidyverse)
# library(httr)
# library(sf)
#
#
# poly <- c(25.44413, 64.99956, 25.49798, 65.02327) %>%
#   set_names("xmin", "ymin", "xmax", "ymax") %>%
#   st_bbox() %>%
#   st_as_sfc() %>%
#   st_as_sf(crs = 4326)
#
# url <- "https://api.oulunliikenne.fi/proxy/graphql"
#
# q <-
#   "query GetAllCarParks {
#     carParks {
#       carParkId
#       name
#       lat
#       lon
#       maxCapacity
#       spacesAvailable
#       realtime
#     }
#   }"
#
# b <- toJSON(list(query = q), auto_unbox = TRUE)
# r <- POST(url, body = b)
# data <- parse_json(content(r, as = "text"), simplifyVector = TRUE)
#
# ptalo_data <- data$data$carParks %>% as_tibble() %>%
#   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
#   st_filter(poly) %>%
#   drop_na() %>%
#   distinct(name, .keep_all = TRUE) %>%
#   arrange(-spacesAvailable)
#
#
#   mutate(timestamp = if_else(realtime, lubridate::now(), lubridate::as_datetime(NA))) %>%
#   rename(freespace = spacesAvailable,
#          totalspace = maxCapacity) %>%
#   mutate_at(vars(freespace, totalspace), as.integer)
#
# kivis <- ptalo_data %>%
#   filter(name == "Kivisydän")
#
# if (file.exists("dataa.rds")) {
#   vanha_data <- readRDS("dataa.rds")
#   vanha_data %>%
#     bind_rows(ptalo_data) %>%
#     saveRDS("dataa.rds")
# } else {
#   saveRDS(ptalo_data, "dataa.rds")
# }
#
# saveRDS(kivis, file = "kivis.rds")
#
# quit(save = "no")
