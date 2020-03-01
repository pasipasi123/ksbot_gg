library(jsonlite)
library(tidyverse)
library(httr)

url <- "https://api.oulunliikenne.fi/proxy/graphql"

q <- 
  "query GetAllCarParks {
    carParks {
      carParkId
      name
      maxCapacity
      spacesAvailable
      realtime
    }
  }"

b <- toJSON(list(query = q), auto_unbox = TRUE)
r <- POST(url, body = b)
data <- parse_json(content(r, as = "text"), simplifyVector = TRUE)

ptalo_data <- data$data$carParks %>% as_tibble() %>% 
  mutate(timestamp = if_else(realtime, lubridate::now(), lubridate::as_datetime(NA))) %>% 
  rename(freespace = spacesAvailable, 
         totalspace = maxCapacity) %>% 
  mutate_at(vars(freespace, totalspace), as.integer)

kivis <- ptalo_data %>% 
  filter(name == "Kivisydän")

if (file.exists("dataa.rds")) {
  vanha_data <- readRDS("dataa.rds")
  vanha_data %>% 
    bind_rows(ptalo_data) %>% 
    saveRDS("dataa.rds")
} else {
  saveRDS(ptalo_data, "dataa.rds")
}

saveRDS(kivis, file = "kivis.rds")

quit(save = "no")
