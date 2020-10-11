library(tidyverse)
library(RCurl)
library(extrafont)

# The plots use the font Roboto, download the font from https://fonts.google.com/
# After installation of the font execute this command to import the font: font_import()

# Theme elements for plots
plot_font <- "Roboto"
annotate_color <- "#606F7B"
annotate_size <- 2.8
pm_size <- 2.8
theme_set(theme_minimal())

# Plot size
aspect_ratio = 16/9
height = 6
width  = round(aspect_ratio*height)


# LIX of New Year Addresses by DK and NO PM -------------------------------

# Arrows used to annotate LIX scores
segment_arrows <- tibble(
  x1 = c(as.Date("1985-01-01"), as.Date("1985-01-01"), as.Date("2020-01-01")),
  x2 = c(as.Date("1985-01-01"), as.Date("1985-01-01"), as.Date("2020-01-01")),
  y1 = c(24, 26, 36),
  y2 = c(20, 34, 44)
)

# Curved arrows used to annotate specific speeches
curve_arrows <- tibble(
  x1 = c(as.Date("1998-10-01"), as.Date("2005-07-01"), as.Date("2012-06-01")),
  x2 = c(as.Date("2001-05-01"), as.Date("2010-08-01"), as.Date("2014-10-01")),
  y1 = c(39.5, 27, 21),
  y2 = c(36.5, 29, 23)
)

# Segments used to annotate Danish PM in office
segment_pm <- tibble(
  x1 = c(as.Date("1985-01-01"), as.Date("1993-01-25"), as.Date("2001-11-27"), as.Date("2009-04-05"), 
         as.Date("2011-10-03"), as.Date("2015-06-28"), as.Date("2019-06-27")),
  x2 = c(as.Date("1993-01-25")-30, as.Date("2001-11-27")-30, as.Date("2009-04-05")-30, as.Date("2011-10-03")-30,
         as.Date("2015-06-28")-30, as.Date("2019-06-27")-30, as.Date("2020-01-01")),
  y1 = c(17, 17, 17, 17, 17, 17, 17),
  y2 = c(17, 17, 17, 17, 17, 17, 17),
  col = c("#00583c", "#f04d46", "#002883", "#002883", "#f04d46", "#002883", "#f04d46")
)

read.csv(text=getURL("https://raw.githubusercontent.com/Straubinger/lix/master/lix.csv")) %>% 
  mutate(date = as.Date(date)) %>% 
  filter(speaker == "Prime Minister" & occasion == "New Year") %>% 
  ggplot(aes(x = date, y = lix, color = factor(country))) +
  geom_line(size = 0.5) +
  geom_point(color = "white", stroke = 2) +
  geom_point(size = 1.3) +
  scale_y_continuous(limits = c(15, 45), breaks=seq(15, 45, 10)) +
  scale_x_date(limits = as.Date(c('1985-01-01',NA)), date_breaks = "5 years", date_labels = "%Y") +
  scale_color_manual(values = c("#002883", "#00583c", "#f04d46", "#e3120b", "#acc8d4")) +
  # Annotation of country
  annotate("text",
           x = as.Date("2012-01-01"), 
           y = 32, 
           hjust = "left",
           color = "#acc8d4",
           family = plot_font,
           label = "Norge") +
  annotate("text",
           x = as.Date("2008-01-01"),
           y = 37, 
           hjust = "left", 
           color = "#e3120b", 
           family = plot_font, 
           label = "Danmark") +
  # Danish PM in office
  geom_segment(data = segment_pm, aes(x = x1, y = y1, xend = x2, yend = y2, color = col)) +
  annotate("text", 
           x = as.Date("1985-01-01"), 
           y = 16.2, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Schlüter") +
  annotate("text",
           x = as.Date("1993-01-25"), 
           y = 16.2, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label="Nyrup") +
  annotate("text", 
           x = as.Date("2001-11-27"), 
           y = 16.2, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Fogh") +
  annotate("text", 
           x = as.Date("2009-04-05"), 
           y = 16.2, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Løkke") +
  annotate("text", 
           x = as.Date("2011-10-03"), 
           y = 16.2, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Thorning") +
  annotate("text", 
           x = as.Date("2015-06-28"),
           y = 16.2, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Løkke") +
  annotate("text", 
           x = as.Date("2020-01-01"),
           y = 16.2, 
           hjust = "right",
           color = annotate_color,
           size = pm_size,
           family = plot_font,
           label = "Frederiksen") +
  # Intepretation of LIX scores
  geom_segment(data = segment_arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.08, "in")), color = annotate_color, size = 0.3
  ) +
  annotate("text", 
           x = as.Date("1985-07-01"),
           y = 24, 
           vjust = "top",
           hjust = "left",
           color = annotate_color, 
           size = annotate_size,
           family = plot_font, 
           label = "LIX under 25\nMeget let niveau\nF.eks. børnelitteratur") +
  annotate("text",
           x = as.Date("1985-07-01"),
           y = 26,
           vjust = "bottom",
           hjust = "left",
           color = annotate_color,
           size = annotate_size,
           family = plot_font, 
           label = "LIX 25-34\nLet niveau\nF.eks. ugeblade") +
  annotate("text",
           x = as.Date("2019-07-01"),
           y = 36, 
           vjust = "bottom", 
           hjust = "right",
           color = annotate_color,
           size = annotate_size,
           family = plot_font, 
           label = "LIX 35-44\nMiddel niveau\nF.eks. aviser") +
  # Annotation of specific speeches
  geom_curve(data = curve_arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
             arrow = arrow(length = unit(0.08, "in")), curvature = 0.3, color = annotate_color, size = 0.3) +
  annotate("text",
           x = as.Date("2010-01-01"),
           y = 21,
           family = plot_font,
           size = annotate_size,
           label = "Thorning 2015\nLaveste LIX på 24") +
  annotate("text", 
           x = as.Date("1999-01-01"), 
           y = 42, 
           family = plot_font,
           size = annotate_size,
           label = "Foghs opgør med\nsmagsdommerne i 2002\nLIX på 36") +
  annotate("text",
           x = as.Date("2003-01-01"),
           y = 27, family = plot_font, 
           size = annotate_size,
           label = "Løkkes opgør med\nefterlønnen i 2011\nLIX på 30") +
  labs(title ="Hvor nem er statsministeren at forstå til nytår?",
       subtitle = "LIX (LæsbarhedsIndeX) over den danske og norske statsministers nytårstaler",
       caption = "\n@StraubingerDK | Data: github.com/straubinger/lix") +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = annotate_color, margin = margin(t = 10)),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_line(),
        text = element_text(family = plot_font))

ggsave("plot_lix_pm_newyear.png", width = width, height = height, type = "cairo-png")


# LIX of Opening of Parliament speeches -----------------------------------

# Arrows used to annotate LIX scores
segment_arrows <- tibble(
  x1 = c(as.Date("1946-01-01"), as.Date("1946-01-01")),
  x2 = c(as.Date("1946-01-01"), as.Date("1946-01-01")),
  y1 = c(45, 35),
  y2 = c(49, 31)
)

# Segments used to annotate Danish PM in office
segment_pm <- tibble(
  x1 = c(as.Date("1946-01-01"), as.Date("1947-11-13"), as.Date("1950-10-30"), as.Date("1953-09-30"), as.Date("1955-02-01"), 
         as.Date("1960-02-21"), as.Date("1962-09-03"), as.Date("1968-02-02"), as.Date("1971-10-11"), as.Date("1972-10-05"),
         as.Date("1973-12-19"), as.Date("1975-02-13"), as.Date("1982-09-10"), as.Date("1993-01-25"), as.Date("2001-11-27"),
         as.Date("2009-04-05"), as.Date("2011-10-03"), as.Date("2015-06-28"), as.Date("2019-06-27")),
   x2 = c(as.Date("1947-11-13")-30, as.Date("1950-10-30")-30, as.Date("1953-09-30")-30, as.Date("1955-02-01")-30, as.Date("1960-02-21")-30, 
          as.Date("1962-09-03")-30, as.Date("1968-02-02")-30, as.Date("1971-10-11")-30, as.Date("1972-10-05")-30, as.Date("1973-12-19")-30, 
          as.Date("1975-02-13")-30, as.Date("1982-09-10")-30, as.Date("1993-01-25")-30, as.Date("2001-11-27")-30, as.Date("2009-04-05")-30, 
          as.Date("2011-10-03")-30, as.Date("2015-06-28")-30, as.Date("2019-06-27")-30, as.Date("2021-01-01")),
  y1 = c(18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18),
  y2 = c(18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18),
  col = c("#002883", "#f04d46", "#002883", "#f04d46", "#f04d46", "#f04d46", "#f04d46", "#ed008c", "#f04d46", "#f04d46", "#002883", 
          "#f04d46", "#00583c", "#f04d46", "#002883", "#002883", "#f04d46", "#002883", "#f04d46")
)

read.csv(text=getURL("https://raw.githubusercontent.com/Straubinger/lix/master/lix.csv")) %>% 
  mutate(date = as.Date(date)) %>% 
  filter(speaker == "Prime Minister" & occasion == "Opening of Parliament" &
           country %in% c("Denmark", "Sweden")) %>% 
ggplot(aes(x = date, y = lix, color = factor(country))) +
  geom_line(size = 0.5) +
  geom_point(color = "white", stroke = 1.5) +
  geom_point(size = 1.3) +
  scale_y_continuous(limits = c(15, 65), breaks=seq(15, 65, 10)) +
  scale_color_manual(values = c("#002883", "#00583c", "#ed008c", "#f04d46", "#e3120b", "#acc8d4")) +
  # Annotation of country
  annotate("text",
           x = as.Date("1988-01-01"), 
           y = 40, 
           hjust = "left",
           color = "#e3120b",
           family = plot_font,
           label = "Danmark") +
  annotate("text",
           x = as.Date("2012-01-01"),
           y = 50, 
           hjust = "left", 
           color = "#acc8d4",
           family = plot_font, 
           label = "Sverige") +
  # Change of speech format in Denmark
  geom_segment(aes(x = as.Date("1966-05-01"), y = 34, xend = as.Date("1966-05-01"), yend = 63), 
               color = annotate_color, size = 0.3, linetype = "dashed") +
  annotate("text",
           x = as.Date("1951-03-01"),
           y = 44,
           hjust = "left",
           vjust = "top",
           family = plot_font,
           size = annotate_size,
           label = "I 1966 gik den danske åbningstale\nfra at være en saglig redegørelse\nfor det kommende folketingsårs\nlovinitiativer...") +
  annotate("text",
           x = as.Date("1967-01-01"),
           y = 44,
           hjust = "left",
           vjust = "top",
           family = plot_font,
           size = annotate_size,
           label = "... til i højere grad at være\nstatsministerens personlige tale.") +
  # Danish PM in office
  geom_segment(data = segment_pm, aes(x = x1, y = y1, xend = x2, yend = y2, color = col)) +
  geom_curve(aes(x = as.Date("1967-01-01"), y = 18.5, xend = as.Date("1972-03-01"), yend = 18.5),
             curvature = -0.3, color = annotate_color, size = 0.3) +
  geom_curve(aes(x = as.Date("1973-08-01"), y = 17.5, xend = as.Date("1976-01-01"), yend = 17.5),
             curvature = 0.4, color = annotate_color, size = 0.3) +
  geom_curve(aes(x = as.Date("2017-09-01"), y = 22, xend = as.Date("2020-06-01"), yend = 19),
             arrow = arrow(length = unit(0.05, "in")), curvature = -0.4, color = annotate_color, size = 0.3) +
  annotate("text", 
           x = as.Date("1946-01-01"), 
           y = 19.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Kristensen") +
  annotate("text", 
           x = as.Date("1947-11-13"), 
           y = 16.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Hedtoft") +
  annotate("text", 
           x = as.Date("1950-10-30"), 
           y = 19.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Eriksen") +
  annotate("text", 
           x = as.Date("1953-09-30"), 
           y = 16.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Hedtoft") +
  annotate("text", 
           x = as.Date("1955-02-01"), 
           y = 19.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "H.C. Hansen") +
  annotate("text", 
           x = as.Date("1960-02-21"), 
           y = 16.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Kampmann") +
  annotate("text", 
           x = as.Date("1963-06-01"), 
           y = 19.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Krag") +
  annotate("text", 
           x = as.Date("1968-02-02"), 
           y = 16.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Baunsgaard") +
  annotate("text", 
           x = as.Date("1973-12-19"), 
           y = 19.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Hartling") +
  annotate("text", 
           x = as.Date("1976-09-01"), 
           y = 16.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Jørgensen") +
  annotate("text", 
           x = as.Date("1982-09-10"), 
           y = 16.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Schlüter") +
  annotate("text",
           x = as.Date("1993-01-25"), 
           y = 16.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label="Nyrup") +
  annotate("text", 
           x = as.Date("2001-11-27"), 
           y = 16.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Fogh") +
  annotate("text", 
           x = as.Date("2009-04-05"), 
           y = 16.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Løkke") +
  annotate("text", 
           x = as.Date("2011-10-03"), 
           y = 19.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Thorning") +
  annotate("text", 
           x = as.Date("2015-06-28"),
           y = 16.5, 
           hjust = "left", 
           color = annotate_color, 
           size = pm_size,
           family = plot_font,
           label = "Løkke") +
  annotate("text", 
           x = as.Date("2012-06-01"),
           y = 22, 
           hjust = "left",
           color = annotate_color,
           size = pm_size,
           family = plot_font,
           label = "Frederiksen") +
  # Intepretation of LIX scores
  geom_segment(data = segment_arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.08, "in")), color = annotate_color, size = 0.3) +
  annotate("text",
           x = as.Date("1946-01-01"),
           y = 29,
           color = annotate_color,
           size = annotate_size,
           family = plot_font,
           label = "Nemmere") +
  annotate("text",
           x = as.Date("1946-01-01"),
           y = 40,
           color = annotate_color,
           size = annotate_size,
           family = plot_font,
           label = "LIX 35-45\nMiddel niveau\nF.eks. aviser") +
  annotate("text",
           x = as.Date("1946-01-01"),
           y = 51,
           color = annotate_color,
           size = annotate_size,
           family = plot_font,
           label = "Sværere") +
  # Annotation of specific speeches
  geom_curve(aes(x = as.Date("2007-12-01"), y = 30, xend = as.Date("2018-08-01"), yend = 27),
             arrow = arrow(length = unit(0.08, "in")), curvature = 0.2, color = annotate_color, size = 0.3) +
  annotate("text",
           x = as.Date("2003-01-01"),
           y = 30,
           family = plot_font,
           size = annotate_size,
           label = "Frederiksen 2019\nLaveste LIX på 28") +
  labs(title ="Hvor nem er statsministeren at forstå ved parlamentets åbning?",
       subtitle = "LIX (LæsbarhedsIndeX) over den danske og svenske statsministers tale ved parlamentets åbning",
       caption = "\n@StraubingerDK | Data: github.com/straubinger/lix") +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = annotate_color, margin = margin(t = 10)),
        axis.title = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_line(),
        text = element_text(family = plot_font))

ggsave("plot_lix_pm_openparl.png", width = width, height = height, type = "cairo-png")

