# Made by John Michael LaSalle
# jmlasalle.com
# MIT License

# ---- Set up Environment ----
library(tidyverse)
library(lubridate)
library(here)

options(scipen = 999)

# ---- Load and Wrangle Data ----
# download case data and format timeseries
# Source: https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv
confirmed_cases <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
  pivot_longer(starts_with("X"), names_to = "date", values_to = "confirmed_cases") %>% 
  group_by(Country.Region, date) %>% 
  summarise(confirmed_cases = sum(confirmed_cases)) %>% 
  mutate(date = mdy(str_remove(date, 'X')))

# Get the first date with confirmed cases for each country
start_dates <- confirmed_cases %>% 
  filter(confirmed_cases > 0) %>% 
  group_by(Country.Region) %>%
  summarise(start_date = min(date))

# Prepare data for graph
dat <- confirmed_cases %>%  
  filter(confirmed_cases != 0) %>% 
  left_join(., start_dates) %>% 
  mutate(day = as.integer(date-start_date),
         Country = as.character(Country.Region)) 

# create end points
points <- dat %>% 
  group_by(Country) %>% 
  filter(day == max(day))

# Create name labels for the 10 countries with the most cases
lab <- dat %>% 
  group_by(Country) %>% 
  summarise(day = max(day), y = max(confirmed_cases)+6000) %>% 
  top_n(., 10, y) %>% 
  mutate(Country = ifelse(Country == "Korea, South", "South Korea", Country))

# Get latest date in dataset
latest_date <- paste(as.character(day(max(confirmed_cases$date))), 
                     as.character(month(max(confirmed_cases$date), label=T, abbr=F)),
                     as.character(year(max(confirmed_cases$date))))

# ---- Create and Export Graph ----
ggplot() +
  geom_line(data = anti_join(dat, lab, by="Country"), mapping = aes(x=day, y=confirmed_cases, group=Country), col="grey22") +
  geom_point(data=anti_join(points, lab), mapping = aes(x=day, y=confirmed_cases), col="grey22", size=0.75) +
  geom_line(data = semi_join(dat, lab, by="Country"), mapping = aes(x=day, y=confirmed_cases, col=Country)) +
  geom_point(data=inner_join(points, lab), mapping = aes(x=day, y=confirmed_cases, col=Country), size=0.75) +
  geom_text(data=lab, mapping = aes(x=day, y=y, label=Country, col=Country)) +
  scale_colour_viridis_d() +
  labs(title = "Growth of Confirmed COVID-19 Cases by Country",
       subtitle = paste("Data through", latest_date),
       y= "Confirmed Cases",
       x = "Days Since First Confirmed Case",
       caption = "Made by jmlasalle.com\nData: Johns Hopkins University") +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  theme_minimal() +
  guides(col=F)

#Save A4 sized JPG image
ggsave(paste0(date(max(confirmed_cases$date)), "-covid-cases-growth-by-country.jpg"), path=paste0(here(), '/images'), width = 297, height = 210, dpi = 150, units = "mm")
