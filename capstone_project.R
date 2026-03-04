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

# standardize country names # i think this is where we should put Europe and EU
countries_mentions <- policies_filtered %>%
  mutate(
    Country = str_trim(Country),
    Country_clean = recode(
      Country,
      "USA" = "United States of America",
      "The Netherlands" = "Netherlands",
      "Brunei" = "Brunei Darussalam",
      "Congo" = "Republic of the Congo",
      "Czechia" = "Czech Republic",
      "Swaziland" = "Eswatini",
      "Gambia" = "The Gambia",
      "Laos" = "Lao People's Democratic Republic",
      "North Korea" = "Dem. People's Rep. Korea",
      "Russia" = "Russian Federation",
      "United States of America" = "United States",
      "São Tomé and Príncipe" = "Sao Tome and Principe",
      "South Korea" = "Republic of Korea",
      "Türkiye" = "Turkey",
      "Eswatini" = "Kingdom of eSwatini",
      "Côte d’Ivoire" = "Côte d'Ivoire",
      "European Union" = NA_character_,
      "Europe" = NA_character_,
      .default = Country
    )
  )

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
    colours = c("#fee6ce", "#f0733d", "#fd5a1a"), # darken the top value
    na.value = "grey90",
    name = "Number of Policies per Country"
  ) +
  labs(
    title = "Global Embodied Carbon Policies",
    #    subtitle = "Aggregated by Country",
    #    caption = "Sources: Carbon Leadership Forum, C40 Cities"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(face = "bold")
  )




