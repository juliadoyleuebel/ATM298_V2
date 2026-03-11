# when we want to commit changes 
# git add capstone_US.R
# git commit -m "Work on capstone project"
# git push

# load relevant packages
library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(readxl)
library(devtools)

# read in the data
policy_list = read_excel('/Users/jdoyleue/ATM298_V2/data/US_policies_filtered.xlsx')
policy_list
policies_filtered = policy_list %>%
  select("Name of Policy/Program", "State")
policies_filtered

states_mentions <- policies_filtered %>%
  mutate(
    State = str_trim(State),
    State = recode(State,
                   "Washington, DC" = "District of Columbia")
  ) %>%
  count(State, name = "Mentions")

sum(states_mentions$Mentions)

# US map
us_states <- ne_states(
  country = "United States of America",
  returnclass = "sf"
)

# join mentions
us_policies <- us_states %>%
  left_join(states_mentions, by = c("name" = "State"))

# separate alaska and hawaii
alaska <- us_policies %>% filter(name == "Alaska")
hawaii <- us_policies %>% filter(name == "Hawaii")
lower48 <- us_policies %>% filter(!name %in% c("Alaska", "Hawaii"))

# transform to projected CRS
alaska <- alaska %>% st_transform(2163)
hawaii <- hawaii %>% st_transform(2163)
lower48 <- lower48 %>% st_transform(2163)

# shrink Alaska
alaska$geometry <- alaska$geometry * 0.4

# shift Alaska
alaska$geometry <- alaska$geometry + c(-1400000, -2700000) # (x,y)

# shift Hawaii
hawaii$geometry <- hawaii$geometry + c(4200000, -1200000)

# restore CRS (lost during geometry math)
st_crs(alaska) <- 2163
st_crs(hawaii) <- 2163
st_crs(lower48) <- 2163

# transform back to lon/lat
alaska <- alaska %>% st_transform(4326)
hawaii <- hawaii %>% st_transform(4326)
lower48 <- lower48 %>% st_transform(4326)

# combine
us_final <- dplyr::bind_rows(lower48, alaska, hawaii)

# debugging
unmatched <- anti_join(
  states_mentions,
  us_states,
  by = c("State" = "name")
)

print(unmatched)

# plot map
ggplot(us_final) +
  geom_sf(aes(fill = Mentions), color = "grey70", size = 0.1) +
  scale_fill_gradientn(
    colours = c("#feebe2","#fbb4b9","#f768a1", "#c51b8a"), #from colorbrewer
    na.value = "grey90",
    name = "Number of policies\nper state"
  ) +
  labs(
    title = "Embodied Carbon Policies",
    subtitle = "Aggregated by state",
    caption = "Sources: Carbon Leadership Forum, C40 Cities"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold")
  )



