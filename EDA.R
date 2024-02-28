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

unique_localities <- unique(homes_df$LOCALITY)

ggplot(average_price_by_county, aes(x = LOCALITY, y = PRICE)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  labs(x = "locality", y = "Price", title = "Average Price by County") +
  scale_y_continuous(labels = scales::comma)

 homes_df %>%
  ggplot(aes(x = propertysqft, y = price, color = locality)) +
  geom_point(alpha = 0.6) + 
  geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = FALSE) + 
  facet_wrap(~locality, scales = 'free') +
  labs(x = "Property Square Footage", y = "Price", color = "Locality") +  
  theme_minimal()

 ggplot(homes_df, aes(x = beds, y = price)) +
   geom_point(alpha = 0.6) +  # Adding transparency to the points
   labs(x = "Beds", y = "Price", title = "Price vs Beds") +  # Adjusting axis labels and title
   theme_minimal()
