# when we want to commit changes 
# git add capstone_project.R
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
policy_list = read_excel('/Users/jdoyleue/ATM298_V2/data/Globalpolicies_filtered.xlsx') # we still have some work to do to clean up Europe and European Union
policy_list
policies_filtered = policy_list %>%
  select("Name of Policy/Program", "Country")
policies_filtered

eu_countries <- c(
  "Austria","Belgium","Bulgaria","Croatia","Cyprus","Czech Republic",
  "Denmark","Estonia","Finland","France","Germany","Greece","Hungary",
  "Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta",
  "Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia",
  "Spain","Sweden"
)

europe_countries <- ne_countries(
  continent = "Europe",
  returnclass = "sf"
)$name_long

# standardize country names
normal <- policies_filtered %>%
  mutate(
    Country = str_trim(Country),
    Country_clean = recode(
      Country,
      "USA" = "United States",
      "United States of America" = "United States",
      "The Netherlands" = "Netherlands",
      "Czechia" = "Czech Republic",
      "Russia" = "Russian Federation",
      "Türkiye" = "Turkey",
      "South Korea" = "Republic of Korea",
      .default = Country
    )
  ) %>%
  filter(!Country_clean %in% c("Europe", "European Union"))

eu_rows <- policies_filtered %>%
  filter(Country == "European Union") %>%
  tidyr::expand_grid(Country_clean = eu_countries)
  
europe_rows <- policies_filtered %>%
  filter(Country == "Europe") %>%
  tidyr::expand_grid(Country_clean = europe_countries)

countries_mentions <- bind_rows(
  normal %>% select(Country_clean),
  eu_rows %>% select(Country_clean),
  europe_rows %>% select(Country_clean)
) %>%
  count(Country_clean, name = "Mentions")

sum(countries_mentions$Mentions)

# world map
world <- ne_countries(scale = "large", returnclass = "sf")

# join mentions to world map
world_policies <- world %>%
  left_join(
    countries_mentions,
    by = c("name_long" = "Country_clean")
  )

# check unmatched countries (debugging)
unmatched <- anti_join(
  countries_mentions,
  world,
  by = c("Country_clean" = "name_long")
)

print(unmatched)

# plot map
ggplot(world_policies) +
  geom_sf(aes(fill = Mentions), color = "grey70", size = 0.1) +
  scale_fill_gradientn(
    colours = c("#feebe2","#fbb4b9","#f768a1", "#c51b8a"), #from colorbrewer
    na.value = "grey90",
    name = "Number of policies\nper country"
  ) +
  labs(
    title = "Embodied Carbon Policies",
  subtitle = "Aggregated by country",
  caption = "Sources: Carbon Leadership Forum, C40 Cities"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold")
  )




