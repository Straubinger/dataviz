# The Google and Apple mobility data is importet from the covmobility package by Kieran Healy
# Vignette: https://kjhealy.github.io/covmobility/

# The plots use the font Roboto, download the font from https://fonts.google.com/
# After installation of the font execute this command to import the font: font_import()

library(tidyverse)
library(covmobility)
library(extrafont)
library(ggtext)

theme_set(theme_minimal())

# Plot size
aspect_ratio = 16/9
height = 5
width  = round(aspect_ratio*height)


# Google Community Mobility Reports, Denmark ------------------------------

data(google_mobility)

google <- google_mobility %>% 
  filter(country_region == "Denmark",is.na(sub_region_1), type != "parks") %>% 
  mutate(type = tools::toTitleCase(type),
         date = as.Date(date),
         pct_diff = pct_diff/100) %>% 
  filter(date <= "2020-07-01")

ggplot(data=google,aes(x = date, y = pct_diff)) +
  geom_ribbon(aes(ymin=pmin(pct_diff,0), ymax=0), fill="#CC1F1A", alpha=0.7) +
  geom_ribbon(aes(ymin=0, ymax=pmax(pct_diff,0)), fill="#2779BD", alpha=0.7) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date("2020-03-11"), size = 0.4, linetype = "dashed", color = "#CC1F1A") +
  geom_vline(xintercept = as.Date("2020-03-17"), size = 0.4, linetype = "dashed", color = "#CC1F1A") +
  geom_vline(xintercept = as.Date("2020-04-06"), size = 0.4, linetype = "dashed", color = "#1F9D55") +
  geom_vline(xintercept = as.Date("2020-05-07"), size = 0.4, linetype = "dashed", color = "#1F9D55") +
  geom_vline(xintercept = as.Date("2020-05-20"), size = 0.4, linetype = "dashed", color = "#1F9D55") +
  scale_x_date(date_labels = "%b") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ type) +
  labs(title = "Mobilitet i Danmark under COVID-19",
       subtitle = paste("Procentuel difference i mobilitet ift. median opgjort for perioden 3. januar til 6. februar 2020.<br/>
       Stiplede linjer markerer pressemøder om <span style='color:#CC1F1A'>nedlukning</span> og 
       <span style='color:#1F9D55'>genåbning</span> af Danmark. Data opdateret", format(max(google$date), "%d/%m-%Y")),
       caption = "Simon Straubinger (@StraubingerDK) | Kilde: Google Community Mobility Reports") +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(lineheight = 1.2),
        legend.position = "none",
        plot.caption = element_text(color = "#606F7B", margin = margin(t = 10)),
        axis.title = element_blank(),
        text = element_text(family = "Roboto"),
        strip.text=element_text(face = "bold", hjust = 0),
        panel.spacing.x = unit(1, "lines"),
        panel.spacing.y = unit(1, "lines"))

ggsave(width = width, height = height, "plot_google_mobility.png", type = "cairo-png")


# Apple Mobility Trends Reports, Denmark ----------------------------------

data(apple_mobility)

apple <- apple_mobility %>% 
  filter(region == "Denmark") %>% 
  mutate(transportation_type = tools::toTitleCase(transportation_type),
         date = as.Date(date),
         score = (score - 100)/100) %>% 
  filter(date <= "2020-07-01")

ggplot(data=apple, aes(x = date, y = score)) +
  geom_ribbon(aes(ymin=pmin(score,0), ymax=0), fill="#CC1F1A", alpha=0.8) +
  geom_ribbon(aes(ymin=0, ymax=pmax(score,0)), fill="#2779BD", alpha=0.8) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = as.Date("2020-03-11"), size = 0.4, linetype = "dashed", color = "#CC1F1A") +
  geom_vline(xintercept = as.Date("2020-03-17"), size = 0.4, linetype = "dashed", color = "#CC1F1A") +
  geom_vline(xintercept = as.Date("2020-04-06"), size = 0.4, linetype = "dashed", color = "#1F9D55") +
  geom_vline(xintercept = as.Date("2020-05-07"), size = 0.4, linetype = "dashed", color = "#1F9D55") +
  geom_vline(xintercept = as.Date("2020-05-20"), size = 0.4, linetype = "dashed", color = "#1F9D55") +
  scale_x_date(date_labels = "%b") +
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(~ transportation_type) +
  labs(title = "Mobilitet i Danmark under COVID-19",
       subtitle = paste("Procentuel difference i mobilitet ift. den 13. januar 2020.<br/>
       Stiplede linjer markerer pressemøder om <span style='color:#CC1F1A'>nedlukning</span> og 
       <span style='color:#1F9D55'>genåbning</span> af Danmark. Data opdateret", format(max(apple$date), "%d/%m-%Y")),
       caption = "Simon Straubinger (@StraubingerDK) | Kilde: Apple Mobility Trends Reports") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(lineheight = 1.2),
        legend.position = "none",
        plot.caption = element_text(color = "#606F7B", margin = margin(t = 10)),
        axis.title = element_blank(),
        text = element_text(family = "Roboto"),
        strip.text=element_text(face = "bold", hjust = 0),
        panel.spacing.x = unit(1, "lines"))

ggsave(width = width, height = height, "plot_apple_mobility.png", type = "cairo-png")


# Google Community Mobility Reports, Danish regions -----------------------

google_regions <- google_mobility %>% 
  filter(country_region == "Denmark", is.na(sub_region_2), type != "parks") %>% 
  mutate(type = tools::toTitleCase(type),
         date = as.Date(date),
         pct_diff = pct_diff/100,
         sub_region_1 = recode_factor(sub_region_1,
                                      "North Denmark Region" = "Nordjylland",
                                      "Central Denmark Region" = "Midtjylland",
                                      "Region of Southern Denmark" = "Syddanmark",
                                      "Region Zealand" = "Sjælland",
                                      "Capital Region of Denmark" = "Hovedstaden")
  ) %>% 
  filter(date <= "2020-07-01")

ggplot(data=google_regions, aes(x = date, y = sub_region_1, fill = pct_diff)) +
  geom_tile() +
  geom_vline(xintercept = as.Date("2020-03-11"), size = 0.4, linetype = "dashed", color = "#CC1F1A") +
  geom_vline(xintercept = as.Date("2020-03-17"), size = 0.4, linetype = "dashed", color = "#CC1F1A") +
  geom_vline(xintercept = as.Date("2020-04-06"), size = 0.4, linetype = "dashed", color = "#1F9D55") +
  geom_vline(xintercept = as.Date("2020-05-07"), size = 0.4, linetype = "dashed", color = "#1F9D55") +
  geom_vline(xintercept = as.Date("2020-05-20"), size = 0.4, linetype = "dashed", color = "#1F9D55") +
  scale_fill_gradient2(low = "#CC1F1A", high = "#2779BD",labels = scales::percent) +
  scale_x_date(date_labels = "%b") +
  facet_wrap(~ type, ncol = 1) +
  labs(title = "Mobilitet i danske regioner under COVID-19",
       subtitle = paste("Procentuel difference i mobilitet ift. median opgjort for perioden 3. januar<br/>til 6. februar 2020.
       Stiplede linjer markerer pressemøder om <span style='color:#CC1F1A'>nedlukning</span> og<br/>
       <span style='color:#1F9D55'>genåbning</span> af Danmark. Data opdateret",format(max(google_regions$date), "%d/%m-%Y")),
       caption = "Simon Straubinger (@StraubingerDK) | Kilde: Google Community Mobility Reports") +
  guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5, ticks = FALSE)) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_markdown(lineheight = 1.2),
        panel.grid = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank(),
        plot.caption = element_text(color = "#606F7B", margin = margin(t = 10)),
        text = element_text(family = "Roboto"),
        strip.text=element_text(hjust = 0, face = "bold"),
        panel.spacing.x = unit(1, "lines"))

ggsave(width = 6, height = 9, "plot_google_mobility_regions.png", type = "cairo-png")
