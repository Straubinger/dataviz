library(tidyverse)
library(extrafont)
library(statsDK)

# Inspired by this chart by Nathan Yau
# https://flowingdata.com/2020/06/22/age-generation-populations/

# The plots use the font Roboto, download the font from https://fonts.google.com/
# After installation of the font execute this command to import the font: font_import()

# Theme elements for plots
plot_font <- "Roboto"
annotate_color <- "#606F7B"
annotate_size <- 2.5


# Retrives data from Statistics Denmark -----------------------------------

# Using the statsDK package by Mikkel Krogsholm:
# https://github.com/mikkelkrogsholm/statsDK

FOLK3 <- sdk_retrieve_data("FOLK3", FDAG = "TOT", FMAANED = "TOT", FODAAR ="*", Tid = "2020") %>% 
  filter(FODAAR != "Total") %>%
  mutate(FODAAR = case_when(as.numeric(FODAAR) <= 1920 ~ 1920,
                            TRUE ~ as.numeric(FODAAR)),
         generation = case_when(FODAAR <= 1927 ~ "Greatest Generation",
                                between(FODAAR, 1928, 1945) ~ "Silent Generation",
                                between(FODAAR, 1946, 1964) ~ "Boomers",
                                between(FODAAR, 1965, 1980) ~ "Generation X",
                                between(FODAAR, 1981, 1996) ~ "Millennials",
                                between(FODAAR, 1997, 2012) ~ "Generation Z",
                                FODAAR >= 2013 ~ "Generation Alpha"))


# Making chart ------------------------------------------------------------

p <- FOLK3 %>%
  ggplot(aes(x = FODAAR, y = INDHOLD, fill = generation)) +
  geom_col() +
  scale_fill_manual(breaks = c("Greatest Generation", "Silent Generation", "Boomers", "Generation X",
                               "Millennials", "Generation Z", "Generation Alpha"), 
                    values=c("#6CB2EB", "#3490DC", "#6CB2EB", "#3490DC", "#6CB2EB", "#3490DC", "#6CB2EB")) +
  scale_y_continuous(limit =c(0, 100000),
                     labels = scales::comma_format(big.mark = ".", decimal.mark = ",")) +
  geom_curve(x = 1924, y = 13000, xend = 1921, yend = 4000,
             arrow = arrow(length = unit(0.08, "in")), curvature = -0.3, color = annotate_color, size = 0.3) +
  labs(title = "De syv generationer",
       subtitle = "Antal personer i Danmark fordelt på fødselsår pr. 1. januar 2020",
       caption = "@StraubingerDK | Data: Danmarks Statistik, FOLK3 tabel") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(colour = annotate_color, margin = margin(t = 10)),
        axis.title = element_blank(),
        axis.text = element_text(color = annotate_color),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        axis.ticks.x = element_line(),
        text = element_text(family = plot_font))


# Data to annotations -----------------------------------------------------

y_ann_gen <- 95000
y_ann_pct <- y_ann_gen-4000

ann_data <- FOLK3 %>% 
  group_by(generation) %>% 
  summarise(people = sum(INDHOLD)) %>% 
  ungroup() %>% 
  mutate(pct = scales::percent(people/sum(people), accuracy = 0.5, big.mark = ".", decimal.mark = ","))

for (i in unique(ann_data$generation)) {
  ann <- ann_data %>% 
    filter(generation == i)
  assign(paste0("gen_",i), ann$generation)
  assign(paste0("pct_",i), ann$pct)
}


# Adding annotations ------------------------------------------------------

p +
  annotate("text", x = 1923, y = y_ann_gen,
           color = annotate_color, family = plot_font, size = annotate_size, fontface = "bold",
           label = `gen_Greatest Generation`) +
  annotate("text", x = 1923, y = y_ann_pct,
           color = annotate_color, family = plot_font, size = annotate_size, vjust = "top",
           label = paste0(`pct_Greatest Generation`, "\naf befolkningen")) +
  annotate("text", x = 1937, y = y_ann_gen,
           color = annotate_color, family = plot_font, size = annotate_size, fontface = "bold",
           label = `gen_Silent Generation`) +
  annotate("text", x = 1937, y = y_ann_pct,
           color = annotate_color, family = plot_font, size = annotate_size, vjust = "top",
           label = `pct_Silent Generation`) +
  annotate("text", x = 1955, y = y_ann_gen,
           color = annotate_color, family = plot_font, size = annotate_size, fontface = "bold",
           label = gen_Boomers) +
  annotate("text", x = 1955, y = y_ann_pct,
           color = annotate_color, family = plot_font, size = annotate_size, vjust = "top",
           label = pct_Boomers) +
  annotate("text", x = 1973, y = y_ann_gen,
           color = annotate_color, family = plot_font, size = annotate_size, fontface = "bold",
           label = `gen_Generation X`) +
  annotate("text", x = 1973, y = y_ann_pct,
           color = annotate_color, family = plot_font, size = annotate_size, vjust = "top",
           label = `pct_Generation X`) +
  annotate("text", x = 1988, y = y_ann_gen,
           color = annotate_color, family = plot_font, size = annotate_size, fontface = "bold",
           label = gen_Millennials) +
  annotate("text", x = 1988, y = y_ann_pct,
           color = annotate_color, family = plot_font, size = annotate_size, vjust = "top",
           label = pct_Millennials) +
  annotate("text", x = 2003, y = y_ann_gen,
           color = annotate_color, family = plot_font, size = annotate_size, fontface = "bold",
           label = `gen_Generation Z`) +
  annotate("text", x = 2003, y = y_ann_pct,
           color = annotate_color, family = plot_font, size = annotate_size, vjust = "top",
           label = `pct_Generation Z`) +
  annotate("text", x = 2017, y = y_ann_gen,
           color = annotate_color, family = plot_font, size = annotate_size, fontface = "bold",
           label = `gen_Generation Alpha`) +
  annotate("text", x = 2017, y = y_ann_pct,
           color = annotate_color, family = plot_font, size = annotate_size, vjust = "top",
           label = `pct_Generation Alpha`) +
  annotate("text", x = 1924, y = 18000,
           color = annotate_color, family = plot_font, size = annotate_size,
           label = "Alle født før og i 1920\ner grupperet sammen")

ggsave("plot_generations.png", width = 10, height = 5)

