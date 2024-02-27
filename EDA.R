

library(tidyverse)

options("tibble.width" = Inf) 
options("tibble.width" = NULL)

# import data from dropbox
homes_df <- read_csv('https://www.dropbox.com/scl/fi/zmvcst0q4s3p4bqi5ybqy/15_train.csv?rlkey=rb3r4i2uid0dsdg59otloissq&dl=1')

#clean the col names
homes_df <- homes_df %>% 
  janitor::clean_names()

#examine characteristics of the dependent variable
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
