# Make a map
library(readr)
food_df <- read_csv("2020-02-17/food_consumption.csv")

world_map <- maps::map(fill = TRUE, plot=FALSE)
world_sf <- sf::st_as_sf(world_map)

##### What's the highest consumed food per country? #####

high_consumption <- food_df %>% 
  group_by(country) %>% 
  summarize(most_consumed = food_category[which.max(consumption)])

high_consumption_sf <- world_sf %>% 
  left_join(high_consumption, by = c("ID" = "country")) %>% 
  filter(!is.na(most_consumed))

ggplot(high_consumption_sf, aes(fill = most_consumed)) +
  geom_sf() + 
  theme_void() +
  coord_sf() + 
  ggtitle("Top consumed food category per country")

# Only fish category
high_consumption_fish_sf <- high_consumption_sf %>% 
  filter(most_consumed == "Fish")

ggplot(high_consumption_fish_sf, aes(fill = most_consumed)) +
  geom_sf() + 
  theme_void() +
  coord_sf() + 
  ggtitle("Countries where fish is the top consumed")

##### What categories are the most polluting? #####

high_polluter <- food_df %>% 
  group_by(country) %>% 
  summarize(biggest_polluter = food_category[which.max(co2_emmission)]) 
high_polluter_sf <- world_sf %>% 
  left_join(high_polluter, by = c("ID" = "country")) #%>% 
  # filter(!is.na(biggest_polluter))

ggplot(high_polluter_sf, aes(fill = biggest_polluter)) +
  geom_sf(color = "white", size = 0.1) + 
  theme_void() +
  coord_sf() + 
  ggtitle("Largest polluting food category by country")

##### What are the worst foods by country (most consumed + highest polluter)? #####

ggplot(food_df, aes(x = co2_emmission, y = consumption, color = food_category)) +
  geom_point() 

# Was surprised that this was so linear. Guess one is modeled based on the other.
# Conclusions could be that things below a 1:1 are not so great, 
# and things above 1:1 are better. "Above 1:1" meaning, as consumption increases,
# CO2 emissions are not rising as rapidly. And "Below 1:1" would be
# as consumption increases CO2 emissions increase faster.

