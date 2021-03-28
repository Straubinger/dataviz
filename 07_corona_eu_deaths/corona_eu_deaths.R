# Inspired by a graphic by the NY Times: https://twitter.com/nytimes/status/1363498989652426754?s=19

Sys.setlocale("LC_TIME", "English")

library(tidyverse)
library(extrafont)
library(zoo)
library(glue)

plot_col <- "#e3120b"

# The plot use the font Roboto, download the font from https://fonts.google.com/
# After installation of the font execute this command to import the font: font_import()

# Import data from Our World in Data GitHub repository --------------------

eu_deaths <- read.csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv")) %>% 
  filter(location == "European Union") %>% 
  select(location, date, new_deaths, total_deaths) %>% 
  mutate(date = as.Date(date),
         date_numeric = as.numeric(date)) %>% 
  drop_na() %>% 
  filter(date <= '2021-03-01')


# Clean and wrangle plot data ---------------------------------------------

eu_deaths_plot <- eu_deaths %>%
  select(-total_deaths) %>%
  group_by(location) %>%
  mutate(ma_deaths = round(rollapplyr(new_deaths, 7, mean, partial=TRUE), 0)) %>% 
  ungroup() %>% 
  select(location, date, date_numeric, ma_deaths) %>% 
  uncount(ma_deaths)


# Clean and wrangle data for annotations ----------------------------------

eu_deaths_text = data.frame()

for (i in seq(100000, 500000, by=100000)) {
  df <- eu_deaths %>% 
    select(-new_deaths) %>% 
    filter(total_deaths > i) %>% 
    slice(1L) %>% 
    mutate(xvalue = 0.5,
           date_line = date_numeric,
           date_text = date_numeric-8,
           label = glue("{format(date, '%b %d')}\n{format(total_deaths, big.mark = ',')}")) %>% 
    select(-location, -total_deaths)
  eu_deaths_text <- bind_rows(eu_deaths_text, df)
}


# Plot data ---------------------------------------------------------------

ggplot(eu_deaths_plot, aes(x = location, y = date_numeric)) +
  geom_jitter(size = 0.5, alpha = 0.2, shape = 16) +
  geom_point(data = subset(eu_deaths_plot, date == min(date_numeric)), color = plot_col, shape = 21, size = 3) +
  geom_hline(data = eu_deaths_text, aes(yintercept = date_line), color = plot_col) +
  geom_text(data = eu_deaths_text, aes(x = xvalue, y = date_text, label = label, family = "Roboto"), 
            color = plot_col, hjust=0) +
  geom_text(aes(x = 0.5, y = min(eu_deaths_text$date_line)+4, label = "deaths", family = "Roboto"), 
            color = plot_col, hjust=0) +
  geom_text(aes(x = 1.42, y = max(eu_deaths_text$date_line)+10,
                label = paste0("As of ", format(max(eu_deaths$date),'%b %d'),"\n",
                               format(max(eu_deaths$total_deaths), big.mark = ","), "\ndeaths"),
                family = "Roboto"), color = plot_col, hjust=0, vjust = 1) +
  geom_text(aes(x = 1.02, y = min(date_numeric), label = paste0(format(as.Date(min(date)), '%b %d %Y'), 
                                                        "\nfirst report of an EU death, in France"), 
                family = "Roboto"), color = plot_col, hjust=0) +
  scale_y_continuous(trans = "reverse") +
  labs(title = "The EU surpasses Half a Million Covid Deaths",
       subtitle = "Each dot represents one death from Covid-19 in the EU",
       caption = "@StraubingerDK | Data: Our World in Data") +
  theme_void() +
  theme(axis.title = element_blank(),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        plot.caption = element_text(size = 12, color = "#606F7B", hjust = 0.5),
        plot.margin = margin(t = 1, b = 1, unit = "cm"),
        text = element_text(family = "Roboto"))

ggsave(width = 8, height = 14, "plot_corona_eu_deaths.png", type = "cairo-png", dpi = 600)

