library(tidyverse)
library(RCurl)
library(extrafont)

# Download the fonts Lato and Roboto Mono from https://fonts.google.com/

# Load data from GitHub
lix <- read.csv(text=getURL("https://raw.githubusercontent.com/Straubinger/lix/master/lix.csv"))

# Convert data from wide to long
lix <- gather(lix, type, lix, openparl_dk:xmas_royal_se, factor_key = TRUE) %>% 
  mutate(year = as.Date(ISOdate(year, 1, 1)))

# Theme elements
plot_font <- "Lato"
annotate_color <- "gray20"

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
  y1 = c(40, 27, 21),
  y2 = c(36.5, 29, 23)
)

# Segments used to annotate PM in office
segment_pm <- tibble(
  x1 = c(as.Date("1985-01-01"), as.Date("1993-01-25"), as.Date("2001-11-27"), as.Date("2009-04-05"), 
         as.Date("2011-10-03"), as.Date("2015-06-28"), as.Date("2019-06-27")),
  x2 = c(as.Date("1993-01-25")-30, as.Date("2001-11-27")-30, as.Date("2009-04-05")-30, as.Date("2011-10-03")-30,
         as.Date("2015-06-28")-30, as.Date("2019-06-27")-30, as.Date("2020-01-01")),
  y1 = c(17, 17, 17, 17, 17, 17, 17),
  y2 = c(17, 17, 17, 17, 17, 17, 17),
  col = c("#00583c", "#f04d46", "#002883", "#002883", "#f04d46", "#002883", "#f04d46")
)

ggplot(subset(lix, type %in% c("newyear_pm_dk", "newyear_pm_no")), 
       aes(x = year, y = lix, color = factor(type))) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  scale_y_continuous(limits = c(15, 45), breaks=seq(15, 45, 10)) +
  scale_x_date(limits = as.Date(c('1985-01-01',NA)), date_breaks = "5 years", date_labels = "%Y") +
  scale_color_manual(values = c("#002883", "#00583c", "#f04d46", "#e3120b", "#acc8d4")) +
  # Annotations instead of legend
  annotate("text", x = as.Date("2012-01-01"), y = 32, hjust = "left", color = "#acc8d4", family = plot_font, size = 7,
           label = "Norge") +
  annotate("text", x = as.Date("2008-01-01"), y = 37, hjust = "left", color = "#e3120b", family = plot_font, size = 7,
           label = "Danmark") +
  # PM in office
  geom_segment(data = segment_pm, aes(x = x1, y = y1, xend = x2, yend = y2, color = col),
               size = 1.5) +
  annotate("text", x = as.Date("1985-01-01"), y = 16.2, hjust = "left", color = annotate_color, family = plot_font,
           label = "Schlüter") +
  annotate("text",x = as.Date("1993-01-25"), y = 16.2, hjust = "left", color = annotate_color, family = plot_font,
           label="Nyrup") +
  annotate("text", x = as.Date("2001-11-27"), y = 16.2, hjust = "left", color = annotate_color, family = plot_font,
           label = "Fogh") +
  annotate("text", x = as.Date("2009-04-05"), y = 16.2, hjust = "left", color = annotate_color, family = plot_font,
           label = "Løkke") +
  annotate("text", x = as.Date("2011-10-03"), y = 16.2, hjust = "left", color = annotate_color, family = plot_font,
           label = "Thorning") +
  annotate("text", x = as.Date("2015-06-28"), y = 16.2, hjust = "left", color = annotate_color, family = plot_font,
           label = "Løkke") +
  annotate("text", x = as.Date("2020-01-01"), y = 17.8, hjust = "right", color = annotate_color, family = plot_font,
           label = "Frederikesen") +
  # Intepretation of LIX scores
  annotate("text", x = as.Date("1985-07-01"), y = 24, vjust = "top", hjust = "left",
           color = annotate_color, family = plot_font, size = 5, label = "LIX under 25\nMeget let niveau\nF.eks. børnelitteratur") +
  annotate("text", x = as.Date("1985-07-01"), y = 26, vjust = "bottom", hjust = "left",
           color = annotate_color, family = plot_font,  size = 5, label = "LIX 25-34\nLet niveau\nF.eks. ugeblade") +
  annotate("text", x = as.Date("2019-07-01"), y = 36, vjust = "bottom", hjust = "right",
           color = annotate_color, family = plot_font, size = 5, label = "LIX 35-44\nMiddel niveau\nF.eks. aviser") +
  geom_segment(data = segment_arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
               arrow = arrow(length = unit(0.1, "in")), color = annotate_color) +
  # Annotation of specific speeches
  annotate("text", x = as.Date("2010-01-01"), y = 21, family = plot_font, size = 5, color = annotate_color,
           label = "Thorning 2015\nLaveste LIX på 24") +
  annotate("text", x = as.Date("1999-01-01"), y = 42, family = plot_font, size = 5, color = annotate_color,
           label = "Foghs opgør med\nsmagsdommerne i 2002\nLIX på 36") +
  annotate("text", x = as.Date("2003-01-01"), y = 27, family = plot_font, size = 5, color = annotate_color,
           label = "Løkkes opgør med\nefterlønnen i 2011\nLIX på 30") +
  geom_curve(data = curve_arrows, aes(x = x1, y = y1, xend = x2, yend = y2),
             arrow = arrow(length = unit(0.1, "in")), curvature = 0.3, color = annotate_color) +
  labs(y = "LIX",
       title ="Hvor nem er statsministeren at forstå?",
       subtitle = "LIX (LæsbarhedsIndeX) over den danske (og norske) statsministers nytårstaler, 1985-2020",
       caption = "\n@StraubingerDK | Data: github.com/straubinger/lix") +
  theme_minimal() +
  theme(plot.title = element_text(size = 25, face = "bold"),
        plot.subtitle = element_text(size = 18),
        plot.caption = element_text(size = 14, colour = annotate_color),
        axis.title = element_blank(),
        axis.text = element_text(size = 14, family = "Roboto Mono"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_line(),
        text = element_text(family = plot_font))
  
ggsave("lix_pm_plot.png", width = 14, height = 9)
