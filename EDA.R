

library(tidyverse)

options("tibble.width" = Inf) 
options("tibble.width" = NULL)

# import data from dropbox
homes_df <- read_csv('https://www.dropbox.com/scl/fi/zmvcst0q4s3p4bqi5ybqy/15_train.csv?rlkey=rb3r4i2uid0dsdg59otloissq&dl=1')

#clean the col names
homes_df <- homes_df %>% 
  janitor::clean_names()

#examine characteristics of the dependent variable (price)
homes_df %>% 
  ggplot(aes(y = price, fill = type)) +
  geom_boxplot() +
  facet_wrap(~type, scales = 'free_y')

homes_df %>% 
  ggplot(aes(x = propertysqft, y = price, color = type)) +
  geom_point() +
  facet_wrap(~type, scales = 'free_y')

homes_df %>% 
  select(price) %>% 
  arrange(price)

homes_df %>% 
  filter(price == 5800)

homes_df %>% 
  filter(is.na(price))


### other EDA
homes_df %>% 
  glimpse()

# look for NA values across all columns
# result is 0 NA 
homes_df %>% 
  summarize(across(everything(), ~sum(is.na(.x)))) %>% 
  pivot_longer(everything(), 
               names_to = 'column', 
               values_to = 'count_na')

# investigate administrative_area_level_2 
homes_df %>% 
  select(address, state, administrative_area_level_2) %>% 
  filter(administrative_area_level_2 != 'New York',
         administrative_area_level_2 != 'New York County',
         administrative_area_level_2 != 'United States',
         administrative_area_level_2 != 'Bronx County',
         administrative_area_level_2 != 'Queens County',
         administrative_area_level_2 != 'Kings County',
         administrative_area_level_2 != 'Brooklyn') %>% 
  print(n = 30)

# investigate the case where administrative_area_level_2 zip code <> state zip code
homes_df %>% 
  filter(address == '1808 Avenue Y') %>% 
  select(formatted_address)

# investigate locality and sublocality
homes_df %>% 
  select(formatted_address, locality, sublocality) %>% 
  slice_tail(n = 50)

homes_df %>% 
  select(formatted_address, street_name, long_name)


### Begin to clean data
homes_df %>% 
  select(brokertitle)

# clean brokertitle
homes_df %>% 
  mutate(brokertitle = str_replace(brokertitle, "Brokered by ", "")) %>% 
  separate_wider_delim(
    col = brokertitle, 
    names = c("broker_name", "broker_office"), 
    delim = "-",
    too_few = "align_start", 
    too_many = "debug"
  ) %>% 
  mutate(broker_name = trimws(broker_name),
         broker_office = trimws(broker_office)) %>% 
  filter(broker_name == "Douglas Elliman") %>% 
  select(brokertitle, broker_name, broker_office)
