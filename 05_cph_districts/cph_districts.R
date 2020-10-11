
# Load libraries, set theme to theme_minimal() and adjust plot size -------

library(tidyverse)
library(geofacet)
library(readxl)
library(extrafont)
library(scales)
library(here)

theme_set(theme_minimal())

# The plots use the font Roboto, download the font from https://fonts.google.com/
# After installation of the font execute this command to import the font: font_import()

# Plot size
aspect_ratio = 16/9
height = 7
width  = round(aspect_ratio*height)


# Grid of districts of Copehagen ------------------------------------------

# The grid of the district of Copenhagen has been submitted to the R package 'geofacet'
# Until it is added to the package run the code below to use the grid

dk_cph_grid1 <- data.frame(
  row = c(1, 1, 1, 1, 2, 2, 3, 3, 3, 3),
  col = c(1, 2, 4, 3, 1, 3, 4, 3, 1, 2),
  code = c("7", "8", "2", "3", "6", "1", "9", "10", "5", "4"),
  name = c("Brønshøj-Husum", "Bispebjerg", "Østerbro", "Nørrebro", "Vanløse", "Indre By", 
           "Amager Øst", "Amager Vest", "Valby", "Vesterbro-Kgs. Enghave"),
  stringsAsFactors = FALSE
)

grid_preview(dk_cph_grid1)

# Import population data from xlsx files ----------------------------------

# Raw data from https://statistikbanken.kk.dk/
# Population data from table Be4.3
# Population forecast data from table Be62

cph_pop <- read_excel(here("Be4.3.xlsx"), skip = 2) %>% 
  rename(year = Generation_tekst,
         district = `Bydel:`,
         people = `Sum af Antal personer`) %>% 
  filter(district != 'Uden for inddeling',
         str_detect(year, "1231") == TRUE) %>% 
  mutate(code = parse_number(district),
         year = as.numeric(str_sub(year, 1, 4)),
         type = "actual") %>% 
  rbind(read_excel(here("Be62.xlsx"), skip = 2) %>% 
          filter(Bydel != 'Bydel - Uden for inddeling') %>% 
          mutate(year = as.numeric(str_remove(aar, "År")),
                 district = str_remove(Bydel, "Bydel - "),
                 code = parse_number(district)) %>% 
          group_by(year, district, code) %>% 
          summarise(people = sum(`Antal - Personer 1år`)) %>% 
          ungroup() %>% 
          mutate(type = "forecast")) %>% 
  rbind(read_excel(here("Be62.xlsx"), skip = 2) %>% 
          filter(Bydel != 'Bydel - Uden for inddeling') %>% 
          mutate(year = as.numeric(str_remove(aar, "År")),
                 district = str_remove(Bydel, "Bydel - "),
                 code = parse_number(district)) %>% 
          group_by(year, district, code) %>% 
          summarise(people = sum(`Antal - Personer 1år`)) %>% 
          ungroup() %>% 
          mutate(type = "actual") %>% 
          filter(year == 2020))


# Import income data from xlsx file ---------------------------------------

# Raw data from https://statistikbanken.kk.dk/
# Disposable income data from table In74

cph_income <- read_excel(here("In74.xlsx"), skip = 2) %>% 
  rename(year = År,
         district = Bydele,
         people = `Sum af Antal personer`,
         income = `Sum af Disponibel indkomst (1000 kr.)`) %>% 
  group_by(year, district) %>% 
  summarise(people = sum(people),
            income = sum(income)*1000) %>% 
  ungroup() %>% 
  filter(district != 'Bydel - Uden for inddeling') %>% 
  mutate(district = str_remove(district, "Bydel - "),
         code = parse_number(district)) %>% 
  mutate(income_avg = income/people) %>% 
  full_join(read_excel(here("In74.xlsx"), skip = 2) %>% 
              rename(year = År,
                     people = `Sum af Antal personer`,
                     income = `Sum af Disponibel indkomst (1000 kr.)`) %>% 
              group_by(year) %>% 
              summarise(people = sum(people),
                        income = sum(income)*1000) %>% 
              ungroup() %>% 
              mutate(income_avg_cph = income/people) %>% 
              select(year, income_avg_cph),
            by = "year")


# Annotations for population chart ----------------------------------------

ann_pop_forecast <- cph_pop %>% 
  filter(type == "actual") %>% 
  filter(year == max(year)-1) %>% 
  full_join(cph_pop %>% 
              filter(type == "forecast") %>% 
              filter(year == max(year)) %>% 
              select(people_f = people,
                     code),
            by = "code") %>% 
  full_join(cph_pop %>% 
              filter(type == "forecast") %>% 
              mutate(x = max(year)-1) %>% 
              filter(year == max(year)-1) %>%
              select(people_l = people,
                     code,
                     x),
            by = "code") %>% 
  mutate(label = ifelse((people_f-people)/people<0, percent((people_f-people)/people, accuracy = 1),
                        str_c('+',percent((people_f-people)/people, accuracy = 1))),
         y = people_l+11000) %>% 
  select(label, code, x, y)

ann_pop_actual <- cph_pop %>% 
  filter(type == "actual") %>% 
  filter(year == max(year)) %>% 
  full_join(cph_pop %>% 
              filter(type == "actual") %>% 
              filter(year == min(year)) %>% 
              select(people_b = people,
                     code),
            by = "code") %>% 
  full_join(cph_pop %>% 
              filter(type == "actual") %>% 
              mutate(x = max(year)-1) %>% 
              filter(year == max(year)-1) %>%
              select(people_l = people,
                     code,
                     x),
            by = "code") %>% 
  mutate(label = ifelse((people-people_b)/people_b<0, percent((people-people_b)/people_b, accuracy = 1),
                        str_c('+',percent((people-people_b)/people_b, accuracy = 1))),
         y = people_l+11000) %>% 
  select(label, code, x, y)
  
ann_forecast <- data.frame(x = 2027.5, y = 18000, code = c("7"), label = c("Prognose"))
ann_pct <- data.frame(x = 1996, y = 105000, code = c("7"), label = c("Befolkningsudvikling for hhv.\n1991-2019 og 2020-2035"))
segm_pct1 <- data.frame(x = 2001, xend = 2012, y = 83000, yend = 60000, code = c("7"))
segm_pct2 <- data.frame(x = 2021, xend = 2030, y = 95000, yend = 73000, code = c("7"))

# Population chart --------------------------------------------------------
  
ggplot(cph_pop, aes(x = year, y = people)) +
  geom_line(colour = "#E9770C", size = 1) + 
  geom_area(data = subset(cph_pop, type == "actual"), fill="#E9770C", alpha = 0.4) +
  geom_area(data = subset(cph_pop, type == "forecast"), fill="#E9770C", alpha = 0.2) +
  geom_text(aes(x = x, y = y, label = label), data = ann_forecast, family = "Roboto", size = 3) +
  geom_text(aes(x = x, y = y, label = label), data = ann_pct, family = "Roboto", hjust = "left", size = 3) +
  geom_text(aes(x = x, y = y, label = label), data = ann_pop_actual, 
            family = "Roboto", hjust = "right", size = 3) +
  geom_text(aes(x = x, y = y, label = label), data = ann_pop_forecast, 
            family = "Roboto", hjust = "right", size = 3) +
  geom_curve(aes(x = x, xend = xend, y = y, yend = yend), data = segm_pct1,
             arrow = arrow(length = unit(0.08, "in")), curvature = 0.3, size = 0.3) +
  geom_curve(aes(x = x, xend = xend, y = y, yend = yend), data = segm_pct2,
             arrow = arrow(length = unit(0.08, "in")), curvature = -0.3, size = 0.3) +
  facet_geo(~ code, grid = dk_cph_grid1, label = "name") +
  scale_x_continuous(n.breaks = 5) +
  scale_y_continuous(limits = c(0,NA), labels = unit_format(unit = '', scale = 1e-3)) +
  labs(title = "Hvor bor folk i København?",
       subtitle = "Befolkningsudvikling i Københavns Kommunes bydele, 1991-2035 ('000 personer)",
       caption = "@StraubingerDK | Data: Københavns Kommunes Statistikbank, tabel Be4.3 og Be62") +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(color = "#606F7B", margin = margin(t = 10)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.title = element_blank(),
        axis.ticks.x = element_line(),
        text = element_text(family = "Roboto"),
        strip.text=element_text(face = "bold", hjust = 0),
        panel.spacing = unit(1, "lines"))

ggsave(width = width, height = height, here("plot_cph_pop.png"), type = "cairo-png", dpi = 600)

# Disposable income chart -------------------------------------------------

ann_income1 <- data.frame(x = 2011, y = 65000, code = c("8"), label = "Disponibel indkomst\nlavere ift. gns. for hele\nKøbenhavns Kommune")
segm_income1 <- data.frame(x = 2012, xend = 2013, y = 120000, yend = 185000, code = c("8"))
ann_income2 <- data.frame(x = 2013, y = 75000, code = c("1"), label = "Disponibel indkomst\nhøjere ift. gns. for hele\nKøbenhavns Kommune")
segm_income2 <- data.frame(x = 2014, xend = 2013, y = 130000, yend = 215000, code = c("1"))
ann_crisis <- data.frame(x = 2011.5, y = 60000, code = c("7"), label = "Finanskrisen\n2007-2009")
segm_crisis <- data.frame(x = 2011, xend = 2008, y = 60000, yend = 60000, code = c("7"))

ggplot(cph_income, aes(x = year, y = income_avg)) +
  geom_line(aes(linetype = "Bydel"), size = 0.8, color = "gray30") +
  geom_line(aes(x = year, y = income_avg_cph, linetype = "Københavns\nKommune"), size = 0.8, color = "gray30") +
  geom_ribbon(aes(ymin = income_avg_cph, ymax = pmin(income_avg_cph, income_avg), fill = "Under", alpha = 0.05)) +
  geom_ribbon(aes(ymin = income_avg, ymax = pmin(income_avg, income_avg_cph), fill = "Over", alpha = 0.05)) +
  annotate(xmin = 2007, xmax = 2009, 
           ymin = -Inf, ymax = Inf, geom = 'rect', alpha = 0.2) +
  geom_text(aes(x = x, y = y, label = label), data = ann_crisis, family = "Roboto", size = 3, hjust = "left") +
  geom_text(aes(x = x, y = y, label = label), data = ann_income1, family = "Roboto", size = 3) +
  geom_text(aes(x = x, y = y, label = label), data = ann_income2, family = "Roboto", size = 3) +
  geom_curve(aes(x = x, xend = xend, y = y, yend = yend), data = segm_crisis,
             arrow = arrow(length = unit(0.08, "in")), curvature = -0.3, size = 0.3) +
  geom_curve(aes(x = x, xend = xend, y = y, yend = yend), data = segm_income1,
               arrow = arrow(length = unit(0.08, "in")), curvature = -0.3, size = 0.3) +
  geom_curve(aes(x = x, xend = xend, y = y, yend = yend), data = segm_income2,
             arrow = arrow(length = unit(0.08, "in")), curvature = 0.3, size = 0.3) +
  facet_geo(~ code, grid = dk_cph_grid1, label = "name") +
  scale_y_continuous(limits = c(0,NA), labels = unit_format(unit = '', scale = 1e-3)) + 
  guides(fill = FALSE, alpha = FALSE) +
  scale_fill_manual(values = c("Under" = "#ff0000", "Over" = "#559fff")) +
  labs(title = "Hvor findes pengene i København?",
       subtitle = "Gnstl. disponibel indkomst i Københavns Kommunes bydele, 2000-2017 ('000 kr.)",
       caption = "@StraubingerDK | Data: Københavns Kommunes Statistikbank, tabel In74") +
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(color = "#606F7B", margin = margin(t = 10)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.85, 0.55),
        legend.text = element_text(size = 10),
        axis.title = element_blank(),
        axis.ticks.x = element_line(),
        text = element_text(family = "Roboto"),
        strip.text=element_text(face = "bold", hjust = 0),
        panel.spacing = unit(1, "lines"))

ggsave(width = width, height = height, here("plot_cph_income.png"), type = "cairo-png", dpi = 600)

