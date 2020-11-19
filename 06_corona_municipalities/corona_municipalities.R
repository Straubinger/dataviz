
# Load libraries and set theme to theme_minimal() -------------------------

library(tidyverse)
library(here)
library(zoo)
library(extrafont)

theme_set(theme_minimal())

# The plots use the font Roboto, download the font from https://fonts.google.com/
# After installation of the font execute this command to import the font: font_import()


# Import data from SSI ----------------------------------------------------

data_date <- as.Date('2020-11-19')

url <- "https://covid19.ssi.dk/overvagningsdata/download-fil-med-overvaagningdata"

link <- do.call(rbind.data.frame, str_match_all(paste(readLines(url), collapse="\n"), "<a href=\"(.*?)\"")) %>%
  select(V2) %>%
  filter(str_detect(tolower(V2), 'data-epi'),
         str_detect(V2,  format(data_date, "%d%m%Y"))) %>%
  pull(V2)

temp <- tempfile()
download.file(link, temp, mode="wb")
unzip(temp, c("Municipality_test_pos.csv", "Municipality_cases_time_series.csv",
              "Municipality_tested_persons_time_series.csv"), exdir = here())

bef <- read.csv2(here("Municipality_test_pos.csv"), header=T, encoding = "UTF-8") %>% 
  select(id = X.U.FEFF.Kommune_.id.,
         municipality = Kommune_.navn.,
         bef = Befolkningstal) %>% 
  mutate(bef = as.numeric(str_remove(bef, "\\."))) %>% 
  filter(municipality != "Christiansø")

cases <- read.csv2(here("Municipality_cases_time_series.csv"), header=T, encoding = "UTF-8") %>% 
  pivot_longer(!date_sample, names_to = "municipality", values_to = "cases") %>% 
  mutate(date_sample = as.Date(date_sample),
         municipality = case_when(municipality == "Copenhagen" ~ "København",
                                  municipality == "Faaborg.Midtfyn" ~ "Faaborg-Midtfyn",
                                  municipality == "Lyngby.Taarbæk" ~ "Lyngby-Taarbæk",
                                  municipality == "Ringkøbing.Skjern" ~ "Ringkøbing-Skjern",
                                  municipality == "Høje.Taastrup" ~ "Høje-Taastrup",
                                  municipality == "Ikast.Brande" ~ "Ikast-Brande",
                                  TRUE ~ municipality)) %>% 
  group_by(municipality) %>% 
  mutate(cases7days = rollapplyr(cases, 7, sum, partial=TRUE)) %>% 
  ungroup() %>% 
  mutate(date_incidens = date_sample+1) %>% 
  full_join(bef, by = "municipality") %>% 
  mutate(incidens = (100000/bef*cases7days)) %>% 
  left_join(read.csv2(here("Municipality_tested_persons_time_series.csv"), header=T, encoding = "UTF-8") %>% 
              pivot_longer(!PrDate_adjusted, names_to = "municipality", values_to = "tests") %>% 
              mutate(date = as.Date(PrDate_adjusted),
                     municipality = case_when(municipality == "Copenhagen" ~ "København",
                                              municipality == "Faaborg.Midtfyn" ~ "Faaborg-Midtfyn",
                                              municipality == "Lyngby.Taarbæk" ~ "Lyngby-Taarbæk",
                                              municipality == "Ringkøbing.Skjern" ~ "Ringkøbing-Skjern",
                                              municipality == "Høje.Taastrup" ~ "Høje-Taastrup",
                                              municipality == "Ikast.Brande" ~ "Ikast-Brande",
                                              TRUE ~ municipality)) %>% 
              select(date, municipality, tests),
            by = c("date_sample" = "date", "municipality")) %>% 
  mutate(positive_rate = cases/tests,
         positive_rate = replace_na(positive_rate, 0)) %>% 
  filter(date_incidens >= "2020-09-01" & date_sample < as.Date(data_date)-2)

cases_plot <- cases %>% 
  full_join(cases %>% 
              group_by(date_sample) %>% 
              summarise(cases = sum(cases),
                        tests = sum(tests)) %>% 
              ungroup() %>% 
              mutate(positive_rate_dk = cases/tests) %>% 
              select(date_sample, positive_rate_dk),
            by = "date_sample") %>% 
  full_join(cases %>% 
              group_by(date_incidens) %>% 
              summarise(cases7days = sum(cases7days),
                        bef = sum(bef)) %>% 
              ungroup() %>% 
              mutate(incidens_dk = (100000/bef*cases7days)) %>% 
              select(date_incidens, incidens_dk),
            by = "date_incidens") %>%
  full_join(cases %>% 
              filter(date_incidens == max(date_incidens)) %>% 
              select(municipality,
                     incidens_order = incidens),
            by = "municipality") %>%
  mutate(municipality = fct_rev(fct_reorder(as_factor(municipality), incidens_order)))


# Incidens line chart -----------------------------------------------------

ggplot(cases_plot, aes(x = date_incidens, y = incidens)) +
  geom_line(aes(linetype = "Kommune"), size = 0.4) +
  geom_line(aes(x = date_incidens, y = incidens_dk, linetype = "Hele DK"), size = 0.4) +
  geom_ribbon(aes(ymin = incidens, ymax = pmin(incidens, incidens_dk), fill = "Over DK-niveau", alpha = 0.05)) +
  geom_ribbon(aes(ymin = incidens_dk, ymax = pmin(incidens_dk, incidens), fill = "Under DK-niveau", alpha = 0.05)) +
  facet_wrap(~ municipality, ncol = 6) +
  scale_x_date(date_labels = "%d/%m", date_breaks = "months") +
  scale_y_continuous(breaks = seq(0, 400, by = 200)) +
  scale_fill_manual(values=c("Over DK-niveau" = "#ff7a69", "Under DK-niveau" = "#559fff")) +
  scale_linetype_manual(breaks=c("Kommune","Hele DK"), values=c(1,2)) +
  guides(alpha = FALSE) +
  labs(title = "Udvikling i COVID-19 incidens i efteråret 2020",
       subtitle = "Incidens defineres som tilfælde COVID-19 de seneste 7 dage pr. 100.000 indbyggere",
       caption = "@StraubingerDK | Data: SSI") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(color = "#606F7B", margin = margin(t = 10)),
        axis.title = element_blank(),
        axis.ticks.x = element_line(color = "#606F7B"),
        axis.text = element_text(size = 8),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        strip.text=element_text(hjust = 0),
        legend.title = element_blank(),
        legend.position = "bottom",
        text=element_text(family="Roboto"))

ggsave(width = 8, height = 14, here("plot_corona_line.png"), type = "cairo-png", dpi = 600)

# Incidens heatmap --------------------------------------------------------

ggplot(cases_plot, aes(x = date_incidens, y = fct_rev(municipality), fill = incidens)) +
  geom_tile() +
  scale_fill_gradient2(low = "white", high = "#CC1F1A") +
  scale_x_date(date_labels = "%d %b") +
  guides(fill = guide_colourbar(barwidth = 15, barheight = 0.5, ticks = FALSE)) +
  labs(title = "Udvikling i COVID-19 incidens i efteråret 2020",
       subtitle = "Incidens defineres som tilfælde af COVID-19 seneste 7 dage pr. 100.000 indbyggere",
       caption = "@StraubingerDK | Data: SSI") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(color = "#606F7B", margin = margin(t = 10)),
        panel.grid = element_blank(),
        axis.ticks.x = element_line(color = "#606F7B"),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank(),
        text = element_text(family = "Roboto"))

ggsave(width = 8, height = 14, here("plot_corona_heat.png"), type = "cairo-png", dpi = 600)



