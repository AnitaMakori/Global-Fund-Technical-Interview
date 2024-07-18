# load packages
library(tidyverse)
library(WDI)
library(jsonlite)
library(httr)
library(treemap)

# define API endpoint
globalfund_grants_url <- "https://fetch.theglobalfund.org/v4/odata/Grants?count=false"

# make API request
response <- GET(globalfund_grants_url)
status_code(response)  # Check if request was successful

# parse response to a data frame
grants_data <- as.data.frame(fromJSON(content(response, "text"), flatten = TRUE))

# wrangle data
grants_data_clean <- grants_data %>%
  setNames(sub("value.","",colnames(.))) %>%
  janitor::clean_names() %>%
  select(total_disbursed_amount_reference_rate, code, period_start_date) %>%
  mutate(country_code = str_sub(code, 1, 3)) %>%
  mutate(start_year = str_sub(period_start_date, 1, 4))

# use gdp data to match country codes
country_codes <- gdp_per_capita %>%
  select(country_code, country_name) 

grants_data_clean1 <- grants_data_clean %>%
  left_join(country_codes, by = "country_code") %>%
  filter(start_year>2020) 

# treemap plot
treemap(grants_data_clean1,
        index = "country_name",  
        vSize = "total_disbursed_amount_reference_rate",  
        title = "Global Fund Investments by Country (2021-2024)",
        fontsize.title = 14,
        palette = "RdYlBu",
)
