library(tidyverse)


games <- read.csv("data/athlete_events.csv")
noc <- read.csv("data/noc_regions_continents.csv")
games <- left_join(games, noc, by="NOC")

games <- games %>%
  mutate(Gender=case_when(Sex == "F" ~ "Female",
                          TRUE ~ "Male")) %>%
  group_by(Games) %>%
  mutate(N_sports_by_games=n_distinct(Sport),
         N_events_by_games=n_distinct(Event)) %>%
  group_by(Games, Gender) %>%
  mutate(N_athletes_by_games_gender=n_distinct(ID)) %>%
  ungroup()

countries <- games %>%
  group_by(Games, Season, Year, NOC_name, Continent) %>%
  summarise(N_athletes_by_games_NOC_name=n_distinct(ID),
            N_sports_by_games_NOC_name=n_distinct(Sport),
            N_events_by_games_NOC_name=n_distinct(Event)) %>%
  ungroup() %>%
  complete(nesting(Games, Season, Year), nesting(NOC_name, Continent),
           fill=list(N_athletes_by_games_NOC_name=0,
                     N_sports_by_games_NOC_name=0,
                     N_events_by_games_NOC_name=0))

cities_games <- unique(games[, c("Games", "City")]) %>%
  filter(!(Games == "1956 Summer" & City == "Stockholm"))

countries <- left_join(countries, cities_games, by="Games")

countriesToSeason <- function(season) {
  countries %>%
    filter(Season == season) %>%
    droplevels() %>%
    group_by(NOC_name) %>%
    filter(any(N_athletes_by_games_NOC_name > 0)) %>%
    ungroup()
}

summer_countries <- countriesToSeason("Summer")
winter_countries <- countriesToSeason("Winter")

summer <- games %>%
  filter(Season == "Summer") %>%
  droplevels() %>%
  distinct(City, Year, Games) %>%
  arrange(Games) %>%
  filter(!(City == "Stockholm" & Games == "1956 Summer")) %>%
  mutate(City_games=paste(City, Games))

winter <- games %>%
  filter(Season == "Winter") %>%
  droplevels() %>%
  distinct(City, Year, Games) %>%
  arrange(Games) %>%
  mutate(City_games=paste(City, Games))

summerYrs <- as.character(summer$Year)
winterYrs <- as.character(winter$Year)
summerCityGames <- summer$City_games
winterCityGames <- winter$City_games

games <- games %>%
  mutate(region=NULL,
         notes=NULL) %>%
  select(ID, Name, Sex, Gender, Age, Height, Weight,
         Team, NOC, NOC_name, Continent, everything())

save(games, summer_countries, winter_countries, cities_games,
     summerYrs, winterYrs, summerCityGames, winterCityGames,
     file="olympics_evolution/data_for_app/Olympics.RData")
