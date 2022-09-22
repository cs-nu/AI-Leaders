library(dplyr)
data <- read.csv('cbd_data.csv')
small <- data %>% slice(1:1000)

cust_summary <- data %>% group_by(Patient.ID) %>% 
  mutate(trans_id = row_number(), sales = sum(Price), num_trans = n(), profit = sum(Price - Cost)) %>%
  ungroup() %>% select(Patient.ID, Gender, Zip.Code, Age, sales, num_trans, profit) %>% distinct() 

summary <- cust_summary %>% group_by(Zip.Code) %>%
  mutate(avg_sales = mean(sales), num_customers = n(), total_trans = sum(num_trans)) %>%
  ungroup() %>% select(Zip.Code, avg_sales, num_customers, total_trans) %>% distinct()

broad_summary <- cust_summary %>% filter(Zip.Code == 90210) %>%
  mutate(age_group = case_when(Age < 21 ~ '1. infant',
                                                           Age < 30 ~ '2. youngish',
                                                           Age < 40 ~ '3. Us soon',
                                                           Age < 50 ~ '4. parents',
                                                           Age < 60 ~ '5. old professionals',
                                                           Age < 70 ~ '6. retirees',
                                                           TRUE ~ '7. Skeletons')) %>% group_by(age_group) %>% 
  mutate(avg_sales = mean(sales), num_customers = n(), total_trans = sum(num_trans), total_profit = sum(profit)) %>% 
  ungroup() %>% select(age_group, avg_sales, num_customers, total_trans, total_profit) %>% distinct()

prods <- data %>% select(Product) %>% distinct()

prod_summary <- data %>% mutate(product_category = case_when(grepl('Blue Dream', Product)        == TRUE ~'Blue Dream',
                                                             grepl('Cats Meow', Product)         == TRUE ~'Cats Meow',
                                                             grepl('Desert Trinity', Product)    == TRUE ~'Desert Trinity',
                                                             grepl('Gorilla Panic', Product)     == TRUE ~'Gorilla Panic',
                                                             grepl('Lucky Stars', Product)       == TRUE ~'Lucky Stars',
                                                             grepl('Midnight Marauder', Product) == TRUE ~'Midnight Marauder',
                                                             grepl('New Mexico', Product)        == TRUE ~'New Mexico Sunset',
                                                             grepl('Night Terror', Product)      == TRUE ~'Night Terror',
                                                             grepl('Orange Kush', Product)       == TRUE ~'Orange Kush',
                                                             grepl('Pippins Palace', Product)    == TRUE ~'Pippins Palace',
                                                             grepl('Popcorn Nugs', Product)      == TRUE ~'Popcorn Nugs',
                                                             grepl('Purple Kush', Product)       == TRUE ~'Purple Kush',
                                                             grepl('Raw Deisel', Product)        == TRUE ~'Raw Deisel',
                                                             grepl('Skunk Hero', Product)        == TRUE ~'Skunk Hero',
                                                             grepl('Sleepy Clouds', Product)     == TRUE ~'Sleepy Clouds',
                                                             grepl('Sweet Kush', Product)        == TRUE ~'Sweet Kush',
                                                             grepl('Vegas Homecoming', Product)  == TRUE ~'Vegas Homecoming',
                                                             grepl('Wedding Cake', Product)      == TRUE ~'Wedding Cake',
                                                             grepl('Wicked Flow', Product)       == TRUE ~'Wicked Flow',
                                                             grepl('Yellow Snow', Product)       == TRUE ~'Yellow Snow',
                                                             TRUE ~ 'Edible'),
                                size = case_when(grepl('1gm', Product) == TRUE ~ '1',
                                                 grepl('2gm', Product) == TRUE ~ '2',
                                                 grepl('3gm', Product) == TRUE ~ '3',
                                                 grepl('4gm', Product) == TRUE ~ '4',
                                                 grepl('5gm', Product) == TRUE ~ '5',
                                                 TRUE ~ 'Edible' )) %>% 
    group_by(product_category, size) %>% 
    mutate(avg_price = mean(Price), 
           avg_cost = mean(Cost),
           avg_profit = mean(Price - Cost),
           sales = sum(Price),
           total_profit = sum(Price - Cost),
           num_trans = n()) %>%
    ungroup() %>% select(product_category, size, sales, total_profit, num_trans, avg_cost, avg_price, avg_profit) %>% distinct() 

## Calc total profit per product
## Calc profit per gram for each product
## Look at lowest cost per gram (best for edibles)
