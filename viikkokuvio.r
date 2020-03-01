#viikko

library(dplyr)
library(tibble)
library(purrr)
library(lubridate)
library(ggplot2)
library(jsonlite)
library(stringr)

Sys.setlocale("LC_TIME", "fi_FI.utf8")

onko_vkl <- function(dttm) {
  v_paiva <- wday(dttm, week_start = 1)
  
  if (v_paiva %in% 2:4) return(FALSE)
  if (v_paiva %in% 6:7) return(TRUE)
  if (v_paiva == 5 && hms::as_hms(dttm) > hms::hms(0, 1, 16)) return(TRUE)
  if (v_paiva == 1 && hms::as_hms(dttm) < hms::hms(0, 1, 6)) return(TRUE)
  
  return(FALSE)
}

datelab <- function(x) {
  d <- day(x)
  m <- month(x)
  y <- year(x)
  
  paste0(
    wday(x, label = TRUE) %>% str_to_title(), "\n", 
    d, ".", m, ".", y)
}

reset_test <- function(df) {
  df2 <- df %>% filter(hour(timestamp) == 6) %>% 
    arrange(timestamp) %>% 
    mutate(pv = day(timestamp)) %>% 
    select(date, osuus, pv) %>% 
    group_by(pv) %>% 
    mutate(erotus = osuus - lag(osuus)) %>% 
    ungroup() %>% 
    filter(!is.na(erotus)) %>% 
    arrange(erotus) %>% 
    filter(row_number() == 1) %>% 
    mutate(date = ymd_hm(paste(date, "06:30")),
           y = osuus + abs(erotus) + 5)
  
  assign("erotus", df2, .GlobalEnv)
  
  reset_nudge <- df %>%
    filter(as_date(timestamp) == as_date(df2$date) - days(1)) %>%
    summarise(osuus = max(osuus)) %>%
    mutate(osuus = pmax(0, osuus, na.rm = TRUE)) %>%
    pull(osuus)

  assign("reset_nudge", reset_nudge, .GlobalEnv)
  
  if (df2$erotus < -3) {
    list(ggrepel::geom_text_repel(data = erotus, aes(x = date, 
                                                     y = y,
                                                     label = "Resetointi?"),
                                  nudge_y = 10 + pmax(10, reset_nudge), 
                                  nudge_x = -6 * 60 * 60,
                                  size = 3))
  }
}

alku <- floor_date(today() - weeks(1)) %>% as_date 
paivat <- alku + days(0:6)

fi <- list.files("~/ptalot/data", full.names = TRUE) %>% 
  enframe() %>% 
  arrange(name) %>% 
  tail(2) %>% 
  pull(value)

da <- map(fi, fromJSON) %>% 
  bind_rows %>% 
  as_tibble

da <- da %>% 
  filter(name == "Kivisydän") %>% 
  mutate(timestamp = dmy_hms(timestamp, tz = "Europe/Helsinki"),
         date = as_date(timestamp)) %>% 
  filter(date %in% paivat) %>%
  mutate_at(vars(totalspace, freespace), as.integer) %>% 
  mutate(totalspace = if_else(onko_vkl(timestamp), totalspace + 100L, totalspace)) %>% 
  mutate(osuus = round(100 - (100 * freespace / totalspace), 1))

reset_list <- reset_test(da)

vk1 <- ggplot(da, aes(timestamp, osuus)) +
  geom_line(color = "red", size = 1) +
  labs(x = NULL,
       y = NULL,
       title = paste0("Kivisydämen asiointipaikkojen käyttöaste viikolla ", week(today()) - 1),
       caption = "Analyysi: Pasi Haapakorva\n@pasi_pasi, @kivisydanbot") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(labels = function(x) paste(x, "%")) +
  scale_x_datetime(labels = datelab,
                   date_breaks = "1 day") +
  theme_minimal() +
  theme(text = element_text(size = 8)) +
  reset_list

ggsave("viikkokuvio.png", h = 4, w = 6)


