library(bskyr)
library(jsonlite)
# library(tidyverse)
library(tidyr)
library(rlang)
library(dplyr)
library(stringr)
library(httr)
library(sf)
library(DBI)
library(RSQLite)
library(lubridate)

db <- dbConnect(RSQLite::SQLite(), "ptalot.sqlite", extended_types = TRUE)

poly <- c(25.44413, 64.99956, 25.49798, 65.02327) %>%
  set_names("xmin", "ymin", "xmax", "ymax") %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf(crs = 4326)

url <- "https://api.oulunliikenne.fi/proxy/graphql"

q <-
  "query GetAllCarParks {
    carParks {
      carParkId
      name
      lat
      lon
      maxCapacity
      spacesAvailable
      realtime
    }
  }"

b <- toJSON(list(query = q), auto_unbox = TRUE)
r <- POST(url, body = b)
data <- parse_json(content(r, as = "text"), simplifyVector = TRUE)

ptalo_data <- data$data$carParks %>% as_tibble() %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  drop_na() %>%
  distinct(name, .keep_all = TRUE) %>%
  arrange(-spacesAvailable) %>%
  mutate(time = round_date(now(), "hour"))

post <- ptalo_data %>%
  st_filter(poly) %>%
  select(name, spacesAvailable) %>%
  st_set_geometry(NULL) %>%
  with(paste0(name, ": ", spacesAvailable, "\n")) %>%
  str_c(collapse = "")

kello <- hour(now(tzone = "Europe/Helsinki"))

post <- str_c("Vapaita parkkipaikkoja Oulun keskustassa kello ", kello, ":\n\n", post)

set_bluesky_user('oulun-parkkitalot.bsky.social')
set_bluesky_pass(readr::read_rds("bsky_token.rds"))

bs_post(text = post)

ptalo_data %>%
  st_set_geometry(NULL) %>%
  dbWriteTable(db, "ptalot", ., append = TRUE)
