library(tidyverse)
library(RCurl)
library(extrafont)

# Download the fonts Lato and Roboto Mono from https://fonts.google.com/
# And execute the command below
# font_import()

# Load data from GitHub
lix <- read.csv(text=getURL("https://raw.githubusercontent.com/Straubinger/lix/master/lix.csv")) %>% 
  mutate(date = as.Date(date))

# Theme elements
plot_font <- "Lato"
annotate_color <- "gray35"
annotate_size <- 2.2

# Arrows used to annotate LIX scores
segment_arrows <- tibble(
  x1 = c(as.Date("1947-01-01"), as.Date("1947-01-01")),
  x2 = c(as.Date("1947-01-01"), as.Date("1947-01-01")),
  y1 = c(44, 36),
  y2 = c(48, 32)
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
          as.Date("2011-10-03")-30, as.Date("2015-06-28")-30, as.Date("2019-06-27")-30, as.Date("2020-01-01")),
  y1 = c(18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18),
  y2 = c(18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18, 18),
  col = c("#002883", "#f04d46", "#002883", "#f04d46", "#f04d46", "#f04d46", "#f04d46", "#ed008c", "#f04d46", "#f04d46", "#002883", 
          "#f04d46", "#00583c", "#f04d46", "#002883", "#002883", "#f04d46", "#002883", "#f04d46")
)

ggplot(subset(lix, speaker == "Prime Minister" & occasion == "Opening of Parliament" & country %in% c("Denmark", "Sweden")), 
       aes(x = date, y = lix, color = factor(country))) +
  geom_point(size = 1.5) +
  geom_line(size = 0.5) +
  scale_y_continuous(limits = c(15, 65), breaks=seq(15, 65, 10)) +
  scale_color_manual(values = c("#002883", "#00583c", "#ed008c", "#f04d46", "#e3120b", "#acc8d4")) +
  # Annotations instead of legend
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
  # Changing format of speech in Denmark
  geom_segment(aes(x = as.Date("1966-05-01"), y = 34, xend = as.Date("1966-05-01"), yend = 63), 
               color = annotate_color, size = 0.3, linetype = "dashed") +
  annotate("text",
           x = as.Date("1953-01-01"),
           y = 44,
           hjust = "left",
           vjust = "top",
           color = annotate_color,
           family = plot_font,
           size = annotate_size,
           label = "I 1966 gik den danske åbningstale\nfra at være en saglig redegørelse\nfor det kommende folketingsårs\nlovinitiativer...") +
  annotate("text",
           x = as.Date("1967-01-01"),
           y = 44,
           hjust = "left",
           vjust = "top",
           color = annotate_color,
           family = plot_font,
           size = annotate_size,
           label = "... til i højere grad at være\nstatsministerens personlige tale.") +
  # Danish PM in office
  geom_segment(data = segment_pm, aes(x = x1, y = y1, xend = x2, yend = y2, color = col)) +
  geom_curve(aes(x = as.Date("1975-01-13"), y = 16.8, xend = as.Date("1973-07-05"), yend = 17.5),
             arrow = arrow(length = unit(0.05, "in")), curvature = -0.4, color = annotate_color, size = 0.3) +
  geom_curve(aes(x = as.Date("2017-06-01"), y = 22, xend = as.Date("2019-08-01"), yend = 19),
             arrow = arrow(length = unit(0.05, "in")), curvature = -0.4, color = annotate_color, size = 0.3) +
  annotate("text", 
           x = as.Date("1946-01-01"), 
           y = 16.8, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Kristensen") +
  annotate("text", 
           x = as.Date("1947-11-13"), 
           y = 19.2, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Hedtoft") +
  annotate("text", 
           x = as.Date("1950-10-30"), 
           y = 16.8, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Eriksen") +
  annotate("text", 
           x = as.Date("1953-09-30"), 
           y = 19.2, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Hedtoft") +
  annotate("text", 
           x = as.Date("1955-02-01"), 
           y = 16.8, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "H.C. Hansen") +
  annotate("text", 
           x = as.Date("1960-02-21"), 
           y = 19.2, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Kampmann") +
  annotate("text", 
           x = as.Date("1962-09-03"), 
           y = 16.8, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Krag") +
  annotate("text", 
           x = as.Date("1968-02-02"), 
           y = 16.8, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Baunsgaard") +
  annotate("text", 
           x = as.Date("1971-10-11"), 
           y = 19.2, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Krag") +
  annotate("text", 
           x = as.Date("1973-12-19"), 
           y = 19.2, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Hartling") +
  annotate("text", 
           x = as.Date("1975-02-13"), 
           y = 16.8, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Jørgensen") +
  annotate("text", 
           x = as.Date("1982-09-10"), 
           y = 16.8, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Schlüter") +
  annotate("text",
           x = as.Date("1993-01-25"), 
           y = 16.8, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label="Nyrup") +
  annotate("text", 
           x = as.Date("2001-11-27"), 
           y = 16.8, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Fogh") +
  annotate("text", 
           x = as.Date("2009-04-05"), 
           y = 16.8, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Løkke") +
  annotate("text", 
           x = as.Date("2011-10-03"), 
           y = 19.2, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Thorning") +
  annotate("text", 
           x = as.Date("2015-06-28"),
           y = 16.8, 
           hjust = "left", 
           color = annotate_color, 
           size = annotate_size,
           family = plot_font,
           label = "Løkke") +
  annotate("text", 
           x = as.Date("2012-01-01"),
           y = 22, 
           hjust = "left",
           color = annotate_color,
           size = annotate_size,
           family = plot_font,
           label = "Frederiksen") +
  # Intepretation of LIX scores
  geom_segment(data = segment_arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.08, "in")), color = annotate_color, size = 0.3) +
  annotate("text",
           x = as.Date("1947-01-01"),
           y = 30,
           color = annotate_color,
           size = annotate_size,
           family = plot_font,
           label = "Nemmere") +
  annotate("text",
           x = as.Date("1947-01-01"),
           y = 40,
           color = annotate_color,
           size = annotate_size,
           family = plot_font,
           label = "LIX 35-45\nMiddel niveau\nF.eks. aviser") +
  annotate("text",
           x = as.Date("1947-01-01"),
           y = 50,
           color = annotate_color,
           size = annotate_size,
           family = plot_font,
           label = "Sværere") +
  # Annotation of specific speeches
  geom_curve(aes(x = as.Date("2007-01-01"), y = 30, xend = as.Date("2018-08-01"), yend = 27),
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
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(size = 8, colour = annotate_color),
        axis.title = element_blank(),
        axis.text = element_text(size = 8, colour = annotate_color, family = "Roboto Mono"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_line(),
        text = element_text(family = plot_font))

ggsave("plot_lix_pm_openparl.png", width = 9, height = 5)

