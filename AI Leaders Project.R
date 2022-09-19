library(dplyr)
data <- read.csv('cbd_data.csv')
small <- data %>% slice(1:1000)

summary <- data %>% group_by(Gender, Zip.Code) %>% mutate(total_sales = sum(Price)) %>% 
  ungroup() %>% select(Gender, Zip.Code, total_sales) %>% distinct()

## Revenue
## Lifetime Customer Value
  # What does "lifetime" mean e.g. do we want to invest in customers now who are 20 because they might spend a lot when they're 80
  # Do we care more revenue or profit at this stage
  # Do we have data over a longer time horizon that allows us to track behavior over time
## Demand by Plant Type
  # Based on the products in the data, what is the best way to identify which ones use consistent plants across product types?
## Quickly identify model breakdowns (and why)
  # What constitutes an "Anomaly"
## Correlate customer value with ad target-able metrics
  # What metrics are useful when targeting ads?

## What links have been seen in the past between medical data and recreational data?

## Business Questions:
  # Which KPI is most important?
    # Are there metrics other than the KPI which we must ensure are not negatively impacted?
  # How receptive to change are those at the client and what institutional barriers exist?
  # What are the key stakeholders in the business segments that may be impacted by AI driven changes?
  # How well positioned is the company to change policy/procedure if optimizations are identified?
    # Which process are flexible, and which are fully immutable?



