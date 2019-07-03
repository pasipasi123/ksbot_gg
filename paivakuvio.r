library(tidyverse)
library(lubridate)

# wdata <- as_tibble(jsonlite::fromJSON("dataa.txt"))
wdata <- readRDS("dataa.rds")

kivi <- wdata %>%
  filter(name == "Kivisydän") %>%
  mutate(#timestamp = dmy_hms(timestamp),
         date = as_date(timestamp)) %>%
  # mutate_at(vars(ends_with("space")), as.integer) %>%
  filter(date == today() - days(1)) %>%
  mutate(osuus = round(100 - (100 * freespace / totalspace), 1))

keskiarvo <- kivi %>% 
  filter(hour(timestamp) %in% 8:20) %>% 
  summarise(y = round(mean(osuus, na.rm = TRUE), 2)) %>% 
  mutate(aika = ymd_hm(paste(today() - days(1), "05:00"))) %>% 
  mutate(aika = if_else(y < 20, aika + hours(7), aika))

# keskipv <- ymd_hm(paste(today() - days(1), "05:00"))

autoja <- kivi %>% filter(hour(timestamp) == 2) %>% 
  filter(row_number() == 1) %>% 
  transmute(aika = timestamp, y = osuus, autoja = totalspace - freespace) 

repel_pisteet <- bind_rows(keskiarvo, autoja) %>% 
  mutate(label = c(paste0("Keskiarvo klo 8–20\n", format(y[1], decimal.mark = ",", digits = 3), " %"), 
                   paste("Aamuyöllä hallissa\n", autoja[2], "autoa."))) %>% 
  mutate(nudge_y = case_when(
    str_detect(label, "Keski") ~ 10,
    abs(y + 30 - lag(y)) < 10 ~ 10,
    TRUE ~ 30))

kausiautot <- kivi %>% 
  filter(hour(timestamp) == 5) %>% 
  distinct(totalspace) %>% 
  mutate(kausi = 900 - totalspace)

pv <- ggplot(kivi, aes(timestamp, osuus)) +
  geom_hline(yintercept = keskiarvo$y) +
  geom_line(size = 1, color = "red") +
  labs(x = NULL,
       y = NULL,
       title = paste0("Kivisydämen asiointipaikkojen käyttöaste ", format(today() - days(1), format = "%d.%m.%Y")),
       caption = "Analyysi: Pasi Haapakorva\n@pasi_pasi, @kivisydanbot") +
  coord_cartesian(ylim = c(0, 100)) +
  # geom_text(aes(x = keskipv, y = keski + 5, label = paste0("Keskiarvo klo 8–20 ", keski, " %")), size = 3) +
  ggrepel::geom_text_repel(data = repel_pisteet, aes(x = aika, y = y, label = label),
                           direction = "y", nudge_y = repel_pisteet$nudge_y, size = 3) +
  geom_point(data = autoja, aes(aika, y), size = 2, color = "blue") +
  scale_x_datetime(date_breaks = "1 hour", date_labels= "%H:%M") +
  scale_y_continuous(labels = function(x) paste(x, "%")) +
  theme_minimal() +
  theme(text = element_text(size = 8),
                 axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  annotate("text", x = ymd_hm(paste(today() - days(1), "20:00")), y = 90, 
           label = paste("Kausipaikkoja varattu\n", kausiautot$kausi, "kappaletta."), hjust = 1, size = 3)
  # ggrepel::geom_text_repel(data = autoja, aes(label = paste("Aamuyöllä hallissa\n", autoja, "autoa.")), 
  #                          direction = "y", nudge_y = 10, size = 3) +
  # geom_point(data = autoja, size = 2, color = "blue")

ggsave("kuvio.png", h = 4, w = 5)


