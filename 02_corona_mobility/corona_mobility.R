library(tidyverse)
library(covdata)
library(extrafont)

# The Google and Apple mobility data is importet from the covdata package by Kieran Healy
# remotes::install_github("kjhealy/covdata")

# The plots use the font Roboto, download the font from https://fonts.google.com/
# After installation of the font execute this command to import the font: font_import()

# Google Community Mobility Reports, Denmark ------------------------------

ann1 <- data.frame(date = as.Date("2020-03-10"), pct_diff = 1.6, type = "Grocery")
ann2 <- data.frame(date = as.Date("2020-03-18"), pct_diff = 1.6, type = "Grocery")
ann3 <- data.frame(date = as.Date("2020-04-05"), pct_diff = 0.6, type = "Grocery")

google_mobility %>% 
  filter(country_region == "Denmark", is.na(sub_region_1)) %>% 
  mutate(type = tools::toTitleCase(type),
         date = as.Date(date),
         pct_diff = pct_diff/100) %>%
  ggplot(aes(x = date, y = pct_diff)) +
  geom_ribbon(aes(ymin=pmin(pct_diff,0), ymax=0), fill="#CC1F1A", alpha=0.8) +
  geom_ribbon(aes(ymin=0, ymax=pmax(pct_diff,0)), fill="#2779BD", alpha=0.8) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date("2020-03-11"), size = 0.3, linetype = "dashed", color = "#3D4852") +
  geom_vline(xintercept = as.Date("2020-03-17"), size = 0.3, linetype = "dashed", color = "#3D4852") +
  geom_vline(xintercept = as.Date("2020-04-06"), size = 0.3, linetype = "dashed", color = "#3D4852") +
  geom_text(data = ann1, family = "Roboto", size = 2.5, hjust = "right",
            label = "Pressemøde\n11. marts") +
  geom_text(data = ann2, family = "Roboto", size = 2.5, hjust = "left",
            label = "Pressemøde\n17. marts") +
  geom_text(data = ann3, family = "Roboto", size = 2.5, hjust = "right",
            label = "Pressemøde\n6. april") +
  scale_x_date(date_labels = "%d. %b") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ type) +
  theme_minimal() +
  labs(title = "Mobilitet i Danmark under COVID-19",
       subtitle = "Procentuel difference ift. normal mobilitet i Danmark som opgjort af Google",
       caption = "@StraubingerDK | Data: Google Community Mobility Reports") +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none",
        plot.caption = element_text(color = "#606F7B", margin = margin(t = 10)),
        axis.title = element_blank(),
        text = element_text(family = "Roboto"),
        strip.text=element_text(face = "bold", hjust = 0),
        panel.spacing.x = unit(2, "lines"),
        panel.spacing.y = unit(1, "lines"))

ggsave(width = 8, height = 4, "plot_google_mobility.png")


# Apple Mobility Trends Reports, Denmark ----------------------------------

ann4 <- data.frame(date = as.Date("2020-03-10"), index = 0.4, transportation_type = "Driving")
ann5 <- data.frame(date = as.Date("2020-03-18"), index = 0.4, transportation_type = "Driving")
ann6 <- data.frame(date = as.Date("2020-04-05"), index = 0.2, transportation_type = "Driving")

apple_mobility %>% 
  filter(region == "Denmark") %>% 
  mutate(transportation_type = tools::toTitleCase(transportation_type),
         date = as.Date(date),
         index = (index - 100)/100) %>%
  ggplot(aes(x = date, y = index)) +
  geom_ribbon(aes(ymin=pmin(index,0), ymax=0), fill="#CC1F1A", alpha=0.8) +
  geom_ribbon(aes(ymin=0, ymax=pmax(index,0)), fill="#2779BD", alpha=0.8) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date("2020-03-11"), size = 0.3, linetype = "dashed", color = "#3D4852") +
  geom_vline(xintercept = as.Date("2020-03-17"), size = 0.3, linetype = "dashed", color = "#3D4852") +
  geom_vline(xintercept = as.Date("2020-04-06"), size = 0.3, linetype = "dashed", color = "#3D4852") +
  geom_text(data = ann4, family = "Roboto", size = 2.5, hjust = "right",
            label = "Pressemøde\n11. marts") +
  geom_text(data = ann5, family = "Roboto", size = 2.5, hjust = "left",
            label = "Pressemøde\n17. marts") +
  geom_text(data = ann6, family = "Roboto", size = 2.5, hjust = "right",
            label = "Pressemøde\n6. april") +
  scale_x_date(date_labels = "%d. %b") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ transportation_type) +
  labs(title = "Mobilitet i Danmark under COVID-19",
       subtitle = "Procentuel difference ift. normal mobilitet i Danmark som opgjort af Apple",
       caption = "@StraubingerDK | Data: Apple Mobility Trends Reports") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "none",
        plot.caption = element_text(color = "#606F7B", margin = margin(t = 10)),
        axis.title = element_blank(),
        text = element_text(family = "Roboto"),
        strip.text=element_text(face = "bold", hjust = 0),
        panel.spacing.x = unit(2, "lines"))

ggsave(width = 8, height = 4, "plot_apple_mobility.png")


# Google Community Mobility Reports, Danish regions -----------------------

google_mobility %>% 
  filter(country_region == "Denmark", !is.na(sub_region_1)) %>% 
  mutate(type = tools::toTitleCase(type),
         date = as.Date(date),
         pct_diff = pct_diff/100,
         sub_region_1 = case_when(sub_region_1 == "Capital Region of Denmark" ~ "Hovedstaden",
                                  sub_region_1 == "Central Denmark Region" ~ "Midtjylland",
                                  sub_region_1 == "North Denmark Region" ~ "Nordjylland",
                                  sub_region_1 == "Region of Southern Denmark" ~ "Syddanmark",
                                  sub_region_1 == "Region Zealand" ~ "Sjælland")
  ) %>% 
  ggplot(aes(x = date, y = sub_region_1, fill = pct_diff)) +
  geom_tile() +
  geom_vline(xintercept = as.Date("2020-03-11"), size = 0.4, linetype = "dashed", color = "#3D4852") +
  geom_vline(xintercept = as.Date("2020-03-17"), size = 0.4, linetype = "dashed", color = "#3D4852") +
  geom_vline(xintercept = as.Date("2020-04-06"), size = 0.4, linetype = "dashed", color = "#3D4852") +
  scale_fill_gradient2(low = "#CC1F1A", high = "#2779BD",labels = scales::percent) +
  scale_x_date(date_labels = "%d. %b") +
  facet_wrap(~ type, ncol = 1) +
  labs(title = "Mobilitet i danske regioner under COVID-19",
       subtitle = "Procentuel difference ift. normal mobilitet i danske regioner\nsom opgjort af Google",
       caption = "@StraubingerDK | Data: Google Community Mobility Reports") +
  guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5, ticks = FALSE)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank(),
        plot.caption = element_text(color = "#606F7B", margin = margin(t = 10)),
        text = element_text(family = "Roboto"),
        strip.text=element_text(hjust = 0, face = "bold"),
        panel.spacing.x = unit(1, "lines"))
  
ggsave(width = 6, height = 9, "plot_google_mobility_regions.png")




