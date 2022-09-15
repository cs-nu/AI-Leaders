library(dplyr)
data <- read.csv('\\Users\\timot\\Downloads\\cbd_data.csv')
small <- data %>% slice(1:1000)

summary <- data %>% group_by(Gender, Zip.Code) %>% mutate(total_sales = sum(Price)) %>% 
  ungroup() %>% select(Gender, Zip.Code, total_sales) %>% distinct()

## Revenue
## Lifetime Customer Value
  # What does "lifetime" mean e.g. do we want to invest in customers now who are 20 because they might spend a lot when they're 80
  # Do we care more revenue or profit at this stage
  # Do we have data over a longer time horizon that allows us to track behavior over time
## Demand by Plant Type
## Quickly identify model breakdowns (and why)
## Correlate customer value with ad targetable metrics

## What links have been seen in the past between medical data and recreational data