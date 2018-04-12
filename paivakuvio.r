library(dplyr)
library(magrittr)
library(lubridate)
library(jsonlite)
library(ggplot2)

wdata <- as_tibble(jsonlite::fromJSON("dataa.txt"))

kivi <- wdata %>%
  filter(name == "Kivisydän") %>%
  mutate(timestamp = dmy_hms(timestamp),
         date = as_date(timestamp)) %>%
  mutate_at(vars(ends_with("space")), as.integer) %>%
  filter(date == today() - days(1)) %>%
  mutate(osuus = round(100 - (100 * freespace / totalspace), 1))

keski <- kivi %>% 
  filter(hour(timestamp) %in% 8:20) %>% 
  summarise(mean = round(mean(osuus, na.rm = TRUE), 2)) %>% 
  pull(mean)

keskipv <- ymd_hm(paste(today() - days(1), "05:00"))

autoja <- kivi %>% filter(hour(timestamp) == 2) %>% 
  filter(row_number() == 1) %>% 
  transmute(timestamp, osuus, autoja = totalspace - freespace) 

pv <- ggplot(kivi, aes(timestamp, osuus)) +
  geom_hline(yintercept = keski) +
  geom_line(size = 1, color = "red") +
  labs(x = NULL,
       y = NULL,
       title = paste0("Kivisydämen asiointipaikkojen käyttöaste ", format(today() - days(1), format = "%d.%m.%Y")),
       caption = "Analyysi: Pasi Haapakorva\n@pasi_pasi, @kivisydanbot") +
  coord_cartesian(ylim = c(0, 100)) +
  geom_text(aes(x = keskipv, y = keski + 5, label = paste0("Keskiarvo klo 8–20 ", keski, " %")), size = 3) +
  scale_x_datetime(date_breaks = "1 hour", date_labels= "%H:%M") +
  scale_y_continuous(labels = function(x) paste(x, "%")) +
  theme_minimal() +
  theme(text = element_text(size = 8),
                 axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_point(data = autoja, size = 2, color = "blue") +
  ggrepel::geom_text_repel(data = autoja, aes(label = paste("Aamuyöllä hallissa\n", autoja, "autoa.")), 
                           direction = "y", nudge_y = 10, size = 3)

ggsave("kuvio.png", h = 4, w = 5)


