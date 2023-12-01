
library(tidyverse)
library(ggmap)

players = read_csv("rosters.csv")

readRenviron("./.Renviron")
register_google(Sys.getenv("MAPSAPIKEY"))


cities = data.frame(unique(players$Hometown))
cities$Hometown = cities$unique.players.Hometown.
cities = cities %>% select(Hometown) %>% drop_na()



geocoded_cities = mutate_geocode(cities, Hometown)

write_csv(distinct(geocoded_cities %>% select(Hometown, lon, lat)), "./geocodes.csv")
