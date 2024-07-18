# load packages
library(tidyverse)

#remove scientific notation
options(scipen = 1000)

# read & wrangle data
malaria_incidence <- read.csv("data/MalariaIncidence.csv", skip = 4, header = T) %>%
  setNames(sub("X","",colnames(.))) %>%
  select(Country.Name, Country.Code,`2014`:`2022`) %>%
  pivot_longer(cols = `2014`:`2022`, names_to = "year", values_to = "malaria_incidence") %>%
  janitor::clean_names()

gdp_per_capita <- read.csv("data/GDPPerCapita.csv", skip = 4, header = T) %>%
  setNames(sub("X","",colnames(.))) %>%
  select(Country.Name, Country.Code,`2014`:`2022`) %>%
  pivot_longer(cols = `2014`:`2022`, names_to = "year", values_to = "gdp_per_capita") %>%
  janitor::clean_names()

burden_data <- malaria_incidence %>%
  left_join(gdp_per_capita, by = c("country_name", "country_code", "year"))
 
# filter for specific regions
filtered_burden_data <- burden_data %>% 
  filter(country_code %in% c("NGA", "WLD", "AFW")) %>%
  arrange (country_name, year)

# plot malaria incidence and gdp
ggplot(data = filtered_burden_data, aes(x = year)) +
  geom_line(aes(y = malaria_incidence, group = country_name, colour = "Malaria Incidence"), size = 1) +
  geom_point(aes(y = malaria_incidence, colour = "Malaria Incidence"), size = 3, shape = 21, fill = "white") +
  geom_text(aes(y = malaria_incidence, label = country_name), vjust = -1, hjust = 1.1, check_overlap = TRUE, 
            colour = "#d73027", data = subset(filtered_burden_data, year == max(year))) +
  geom_line(aes(y = gdp_per_capita/100, group = country_name, colour = "GDP per Capita (thousands)"), size = 1) +
  geom_point(aes(y = gdp_per_capita/100, colour = "GDP per Capita (thousands)"), size = 3, shape = 21, fill = "white") +
  geom_text(aes(y = gdp_per_capita/100, label = country_name), vjust = 1, hjust = 1.1, check_overlap = TRUE, 
            colour = "#4575b4", data = subset(filtered_burden_data, year == max(year))) +
  scale_y_continuous(sec.axis = sec_axis(~.*0.1, name = "GDP per Capita (USD)")) +
  labs(title = "Trend of Malaria Incidence and GDP per Capita (2014-2022)",
       x = "Year",
       y = "Malaria Incidence (per 1,000 population at risk)") +
  theme_bw() +
  scale_colour_manual(values = c("Malaria Incidence" = "#d73027", "GDP per Capita (thousands)" = "#4575b4")) +
  theme(legend.title = element_blank(), legend.position = "bottom")


  
