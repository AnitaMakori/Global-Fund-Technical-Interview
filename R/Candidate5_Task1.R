
# load packages
library(tidyverse)

# read & wrangle data
hiv_data <- read_csv("data/HIVDiseaseBurden.csv") %>%
  janitor::clean_names()
hiv_data <- as.data.frame(hiv_data)

# Define the function for allocating funds
fund_allocation <- function(data, total_funds) {
  # Calculate initial allocation based on disease burden
  data$allocated_amount <- (data$people_living_with_hiv / sum(data$people_living_with_hiv)) * total_funds
  
  # Correct for minimum funding guarantee
  while(any(data$allocated_amount < 500000) || any(data$allocated_amount > total_funds * 0.10)) {
    underfunded <- data$allocated_amount < 500000
    overfunded <- data$allocated_amount > total_funds * 0.10
    normal <- !underfunded & !overfunded
    
    # Calculate the amount needed to bring underfunded countries to $500,000
    needed <- sum(500000 - data$allocated_amount[underfunded])
    
    # Cap the overfunded countries to 10% of the total funds
    excess <- sum(data$allocated_amount[overfunded] - total_funds * 0.10)
    
    # Redistribute the excess to underfunded and normally funded countries
    data$allocated_amount[overfunded] <- total_funds * 0.10
    
    # Use excess to fund underfunded, rest distribute to normal
    remaining_excess <- excess - needed
    data$allocated_amount[underfunded] <- 500000
    if (remaining_excess > 0) {
      data$allocated_amount[normal] <- data$allocated_amount[normal] + remaining_excess * (data$people_living_with_hiv[normal] / sum(data$people_living_with_hiv[normal]))
    } else {
      data$allocated_amount[normal] <- data$allocated_amount[normal] - needed * (data$people_living_with_hiv[normal] / sum(data$people_living_with_hiv[normal]))
    }
  }
  
  result <- data %>% select(iso3, people_living_with_hiv, allocated_amount)
  return(result)
}

allocation_data <- fund_allocation (hiv_data, 100000000)

