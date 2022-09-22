library(dplyr)
data <- read.csv('cbd_data.csv')
small <- data %>% slice(1:1000)

cust_summary <- data %>% group_by(Patient.ID) %>% 
  mutate(trans_id = row_number(), sales = sum(Price), num_trans = n()) %>%
  ungroup() %>% select(Patient.ID, Gender, Zip.Code, Age, sales, num_trans) %>% distinct() 

summary <- cust_summary %>% group_by(Age) %>% 
  mutate(avg_sales = mean(sales), num_customers = n(), total_trans = sum(num_trans)) %>% 
  ungroup() %>% select(Age, avg_sales, num_customers, total_trans) %>% distinct()

broad_summary <- cust_summary %>% mutate(age_group = case_when(Age < 21 ~ 'infant',
                                                           Age < 25 ~ 'college',
                                                           Age < 30 ~ 'youngish',
                                                           Age < 40 ~ 'middle-age',
                                                           Age < 50 ~ 'parents',
                                                           Age < 65 ~ 'old professionals',
                                                           Age < 80 ~ 'retirees',
                                                           TRUE ~ 'Skeletons')) %>% group_by(age_group) %>% 
  mutate(avg_sales = mean(sales), num_customers = n(), total_trans = sum(num_trans)) %>% 
  ungroup() %>% select(Age, avg_sales, num_customers, total_trans) %>% distinct()

