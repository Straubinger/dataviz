# Load libraries and set theme to theme_minimal() -------------------------
library(tidyverse)
library(imfr)
library(lubridate)
library(extrafont)

theme_set(theme_minimal())

# Import data from IMF Primary Commodity Price System ---------------------
ldf <- list()

for (i in c("PTIN", "PCOPP", "PALUM", "PNICK", "PZINC")){
df <- imf_data(
  database_id = "PCPS",
  indicator = i,
  country =  "W00",
  start = 1990,
  end = current_year(),
  freq = "M",
  return_raw = TRUE
)

ldf[[i]] <- df$CompactData$DataSet$Series %>% 
  filter(`@FREQ` == "M", `@COMMODITY` ==  i, `@UNIT_MEASURE` == "USD") %>% 
  unnest(Obs)
}

PCPS <- as.data.frame(do.call("rbind", ldf))

# Import data from IMF International Financial Statistics -----------------
IFS <- imf_data(database_id = 'IFS', indicator = 'ENDA_XDC_USD_RATE',
                    country = c('DK'), freq = 'M',
                    start = 1990, end = current_year())

# Data cleaning and wrangling ---------------------------------------------
coin_data <- PCPS %>% 
  inner_join(IFS, by = c("@TIME_PERIOD" = "year_month")) %>% 
  mutate(USD_ton = as.numeric(`@OBS_VALUE`),
         year_month = ymd(`@TIME_PERIOD`, truncated = 1),
         DKK_gram = USD_ton*ENDA_XDC_USD_RATE/1000000) %>% 
  select(commodity = `@COMMODITY`,
         year_month,
         DKK_gram) %>% 
  pivot_wider(names_from = commodity, values_from = DKK_gram) %>% 
  mutate(price_25ore = (2.8*0.97*PCOPP+2.8*0.005*PTIN+2.8*0.025*PZINC)*100, # 2.8g, copper 97%, tin 0.5%, zinc 2,5%
         price_50ore = (4.3*0.97*PCOPP+4.3*0.005*PTIN+4.3*0.025*PZINC)*100, # 4.3g, copper 97%, tin 0.5%, zinc 2,5%
         price_1kr = (3.6*0.75*PCOPP+3.6*0.25*PNICK)*100, # 3.6g, copper 75%, nickel 25%
         price_2kr = (5.9*0.75*PCOPP+5.9*0.25*PNICK)*100, # 5.9g, copper 75%, nickel 25%
         price_5kr = (9.2*0.75*PCOPP+9.2*0.25*PNICK)*100, # 9.2g, copper 75%, nickel 25%
         price_10kr = (7*0.92*PCOPP+7*0.02*PNICK+7*0.06*PALUM)*100, # 7g, copper 92%, nickel 2%, aluminium 6%
         price_20kr = (9.3*0.92*PCOPP+9.3*0.02*PNICK+9.3*0.06*PALUM)*100, # 9.3g, copper 92%, nickel 2%, aluminium 6%
         pct_25ore = price_25ore/25,
         pct_50ore = price_50ore/50,
         pct_1kr = price_1kr/100,
         pct_2kr = price_2kr/200,
         pct_5kr = price_5kr/500,
         pct_10kr = price_10kr/1000,
         pct_20kr = price_20kr/2000) %>% 
  select(-c(PTIN:PZINC)) %>% 
  pivot_longer(-year_month) %>% 
  separate(name, c("measure", "coin")) %>%
  mutate(coin = recode_factor(coin, 
                              "25ore" = "25-øre",
                              "50ore" = "50-øre",
                              "1kr" = "1-krone",
                              "2kr" = "2-krone",
                              "5kr" = "5-krone",
                              "10kr" = "10-krone",
                              "20kr" = "20-krone")
  )

# Melt value of coins -----------------------------------------------------
ann1 <- data.frame(year_month = as.Date("1994-01-01"), value = 45, coin = "25-øre")
segm1 <- data.frame(x=as.Date("2002-01-01"), y=45, xend=as.Date("2008-06-01"), yend=18, coin = "25-øre")
ann2 <- data.frame(year_month = as.Date("1997-01-01"), value = 63, coin = "1-krone")
segm2 <- data.frame(x=as.Date("2005-01-01"), y=70, xend=as.Date("2008-03-01"), yend=70, coin = "1-krone")

coin_data %>% 
  filter((measure == "price" & year_month <= '2008-09-01') | 
           (measure == "price" & year_month > '2008-09-01' & coin != "25-øre")) %>% 
  ggplot(aes(x = year_month, y = value)) +
  geom_line(colour = "#5661B3") +
  geom_text(data = ann1, family = "Roboto", size = 3, hjust = "left",
            label = "25-øren ud af\ncirkulation") +
  geom_curve(data = segm1, aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.08, "in")), curvature = -0.3, size = 0.4) +
  geom_text(data = ann2, family = "Roboto", size = 3, hjust = "left",
            label = "Finanskrisen\n2007-2008") +
  geom_segment(data = segm2, aes(x = x, y = y, xend = xend, yend = yend), size = 0.4) +
  annotate(xmin = as.Date("2007-06-01"), xmax = as.Date("2009-01-01"), 
           ymin = -Inf, ymax = Inf, geom = 'rect', alpha = 0.2) +
  facet_wrap(~coin, ncol = 2) +
  labs(title = "Hvad koster de danske mønter?",
       subtitle = "Danske mønters metalværdi pr. måned i danske øre",
       caption = "@StraubingerDK | Data: egne beregninger, IMF, Danmarks Nationalbank") +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(color = "#606F7B", margin = margin(t = 10)),
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_line(),
        text = element_text(family = "Roboto"),
        strip.text=element_text(face = "bold", hjust = 0),
        panel.spacing = unit(1, "lines"))

ggsave(width = 7, height = 8, "plot_coins_value.png")

# Melt value vs. nominal value of coins -----------------------------------
ann3 <- data.frame(year_month = as.Date("2012-01-01"), value = 0.2, coin = "25-øre")
segm3 <- data.frame(x=as.Date("2015-01-01"), y=0.28, xend=as.Date("2010-01-01"), yend=0.4, coin = "25-øre")
ann4 <- data.frame(year_month = as.Date("1997-01-01"), value = 0.42, coin = "1-krone")
segm4 <- data.frame(x=as.Date("2005-01-01"), y=0.45, xend=as.Date("2008-03-01"), yend=0.45, coin = "1-krone")

coin_data %>% 
  filter((measure == "pct" & year_month <= '2008-09-01') | 
           (measure == "pct" & year_month > '2008-09-01' & coin != "25-øre")) %>% 
  ggplot(aes(x = year_month, y = value)) +
  geom_line(colour = "#1F9D55") +
  geom_text(data = ann3, family = "Roboto", size = 3, hjust = "left",
            label = "25-øren ud af\ncirkulation") +
  geom_curve(data = segm3, aes(x = x, y = y, xend = xend, yend = yend),
             arrow = arrow(length = unit(0.08, "in")), curvature = 0.3, size = 0.4) +
  geom_text(data = ann4, family = "Roboto", size = 3, hjust = "left",
            label = "Finanskrisen\n2007-2008") +
  geom_segment(data = segm4, aes(x = x, y = y, xend = xend, yend = yend), size = 0.4) +
  annotate(xmin = as.Date("2007-06-01"), xmax = as.Date("2009-01-01"), 
           ymin = -Inf, ymax = Inf, geom = 'rect', alpha = 0.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
  facet_wrap(~coin, ncol = 2) +
  labs(title = "Metalværdi vs. nominel værdi",
       subtitle = "Danske mønters metalværdi ift. deres nominelle værdi pr. måned",
       caption = "@StraubingerDK | Data: egne beregninger, IMF, Danmarks Nationalbank") +
  theme(plot.title = element_text(face = "bold"),
        plot.caption = element_text(color = "#606F7B", margin = margin(t = 10)),
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.ticks.x = element_line(),
        text = element_text(family = "Roboto"),
        strip.text=element_text(face = "bold", hjust = 0),
        panel.spacing = unit(1, "lines"))

ggsave(width = 7, height = 8, "plot_coins_pct.png")
