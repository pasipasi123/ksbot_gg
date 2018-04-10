#viikko

library(dplyr)
library(tibble)
library(purrr)
library(lubridate)
library(ggplot2)
library(jsonlite)

alku <- floor_date(today() - weeks(1)) %>% as_date
paivat <- alku + days(0:6)

fi <- file.info(list.files("~/ptalot/data"))
fi <- as_tibble(fi) %>% 
  rownames_to_column("name") %>% 
  mutate(name = paste0("data/", name)) %>%
  arrange(name) %>% 
  tail(2) %>% 
  pull(name)

da <- map(fi, fromJSON) %>% 
  bind_rows %>% 
  as_tibble

da <- da %>% 
  filter(name == "Kivisydän") %>% 
  mutate(timestamp = dmy_hms(timestamp),
         date = as_date(timestamp)) %>% 
  filter(date %in% paivat) %>%
  mutate_at(vars(totalspace, freespace), as.integer) %>% 
  mutate(osuus = round(100 - (100 * freespace / totalspace), 1))

png("viikkokuvio.png", height = 4, width = 6, units = "in", res = 300)
ggplot(da, aes(timestamp, osuus)) +
  geom_line(color = "red", size = 1) +
  labs(x = "aika",
                y = "käyttöaste %",
                title = paste0("Kivisydämen asiointipaikkojen käyttöaste viikolla ", week(today()) - 1),
                caption = "Analyysi: Pasi Haapakorva\n@pasi_pasi, @kivisydanbot") +
  coord_cartesian(ylim = c(0, 100)) +
  #theme_economist() +
  theme(text = element_text(size = 8),
                 axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5),
                 axis.title.x = ggplot2::element_blank()) +
  scale_x_datetime(date_breaks = "1 day")
dev.off()
