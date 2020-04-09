# Made by John Michael LaSalle
# jmlasalle.com
# MIT License

# ---- Set up Environment ----
library(tidyverse)
library(lubridate)
library(here)
library(wbstats)

options(scipen = 999)

# ---- Load and Wrangle Data ----
# download case data and format timeseries
# Source: https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv
confirmed_cases <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
  pivot_longer(starts_with("X"), names_to = "date", values_to = "confirmed_cases") %>% 
  group_by(Country.Region, date) %>% 
  summarise(confirmed_cases = sum(confirmed_cases)) %>% 
  ungroup() %>% 
  mutate(date = mdy(str_remove(date, 'X')),
         country = ifelse(Country.Region == "Korea, South", "South Korea", as.character(Country.Region))) %>% 
  filter(!(country %in% c("Taiwan*", "Holy See", "Eritrea", "Diamond Princess"))) %>% 
  select(-Country.Region)

# Deaths
deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>% 
  pivot_longer(starts_with("X"), names_to = "date", values_to = "deaths") %>% 
  group_by(Country.Region, date) %>% 
  summarise(deaths = sum(deaths)) %>% 
  ungroup() %>% 
  mutate(date = mdy(str_remove(date, 'X')),
         country = ifelse(Country.Region == "Korea, South", "South Korea", as.character(Country.Region))) %>% 
  filter(!(country %in% c("Taiwan*", "Holy See", "Eritrea", "Diamond Princess"))) %>% 
  select(-Country.Region)

# Recoveries
recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") %>% 
  pivot_longer(starts_with("X"), names_to = "date", values_to = "recovered") %>% 
  group_by(Country.Region, date) %>% 
  summarise(recovered = sum(recovered)) %>% 
  ungroup() %>% 
  mutate(date = mdy(str_remove(date, 'X')),
         country = ifelse(Country.Region == "Korea, South", "South Korea", as.character(Country.Region))) %>% 
  filter(!(country %in% c("Taiwan*", "Holy See", "Eritrea", "Diamond Princess"))) %>% 
  select(-Country.Region)

# population data
# https://data-worldbank-org.proxy.library.upenn.edu/indicator/SP.POP.TOTL
population <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2020) %>% 
  select(country, value) %>% 
  rename(pop = value) %>% 
  mutate(country = ifelse(country == 'Venezuela, RB', 'Venezuela', country),
         country = ifelse(country == 'United States', 'US', country),
         country = ifelse(country == 'Syrian Arab Republic', 'Syria', country),
         country = ifelse(country == 'Slovak Republic', 'Slovakia', country),
         country = ifelse(country == 'St. Vincent and the Grenadines', 'Saint Vincent and the Grenadines', country),
         country = ifelse(country == 'St. Lucia', 'Saint Lucia', country),
         country = ifelse(country == 'St. Kitts and Nevis', 'Saint Kitts and Nevis', country),
         country = ifelse(country == 'Russian Federation', 'Russia', country),
         country = ifelse(country == 'Lao PDR', 'Laos', country),
         country = ifelse(country == 'Kyrgyz Republic', 'Kyrgyzstan', country),
         country = ifelse(country == 'Korea, Rep.', 'South Korea', country),
         country = ifelse(country == 'Iran, Islamic Rep.', 'Iran', country),
         country = ifelse(country == 'Gambia, The', 'Gambia', country),
         country = ifelse(country == 'Egypt, Arab Rep.', 'Egypt', country),
         country = ifelse(country == 'Czech Republic', 'Czechia', country),
         country = ifelse(country == 'Congo, Dem. Rep.', 'Congo (Kinshasa)', country),
         country = ifelse(country == 'Congo, Rep.', 'Congo (Brazzaville)', country),
         country = ifelse(country == 'Brunei Darussalam', 'Brunei', country),
         country = ifelse(country == 'Bahamas, The', 'Bahamas', country))

# Join data and calculate active cases
dat <- left_join(confirmed_cases, deaths) %>% 
  left_join(., recovered) %>% 
  left_join(., population) %>% 
  mutate(active = confirmed_cases - deaths - recovered)

# Create end points
points <- dat %>% filter(date == max(date))

# create labels for top 10 countries
lab <- points %>% 
  top_n(., 10, active) %>% 
  mutate(y = active + 6000) %>% 
  select(country, date, y)

# Get latest date in dataset
latest_date <- paste(as.character(day(max(confirmed_cases$date))), 
                     as.character(month(max(confirmed_cases$date), label=T, abbr=F)),
                     as.character(year(max(confirmed_cases$date))))

# ---- Create and Export Graph ----

ggplot() +
  geom_line(data = anti_join(dat, lab, by = "country"), mapping = aes(x=date, y=active, group=country), col="grey22") +
  geom_line(data = semi_join(dat, lab, by="country"), mapping = aes(x=date, y=active, group=country, col=country)) +
  geom_point(data=anti_join(points, lab, by = "country"), mapping = aes(x=date, y=active), col="grey22", size=0.75) +
  geom_point(data = inner_join(dat, lab), mapping = aes(x=date, y=active, group=country, col=country), size=0.75) +
  geom_text(data=lab, mapping = aes(x=date, y=y, label=country, col=country)) +
  scale_colour_viridis_d() +
  labs(title = "Active COVID-19 Cases by Country",
       subtitle = paste("Data through", latest_date),
       y= "Active Cases",
       x = "Date",
       caption = "Made by jmlasalle.com\nData: Johns Hopkins University") +
  scale_y_continuous(labels = function(x) format(x, big.mark = " ", scientific = FALSE)) +
  theme_minimal() +
  guides(col=F)

#Save A4 sized JPG image
ggsave(paste0(date(max(confirmed_cases$date)), "-covid-active-cases-by-country.jpg"), path=paste0(here(), '/images'), width = 297, height = 210, dpi = 150, units = "mm")
