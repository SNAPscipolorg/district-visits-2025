library(tidyverse)
library(ggplot2)
library(readxl)
library(sf) # for making maps
library(here) # for making maps (lets you set filepaths)
library(ggmap) # for making maps
library(tmaptools) # for geocoding
library(maps)
library(tigris)

# # Where ML geo came from, but file is included in repo, just so you can know 
# # how it was generated

# ML_geo <- read_excel("McClintock Letter Sign-Up (Responses) copy.xlsx", sheet = "ML_geo")
# 
# ML_geocoded <- geocode_OSM(ML_geo$Location,
#                            details = FALSE, as.data.frame = TRUE)
# ML_geocoded <- ML_geocoded %>%
#   select(query, lat, lon) %>%
#   rename(Location = query)
# 
# ML_geo <- right_join(ML_geocoded, ML_geo, by = "Location")
# write_csv(ML_geo, file = "ML_geo.csv")


# setwd to wherever you keep this file / want outputs to go 
ML_geo <- read_csv("ML_geo.csv")

# re-arranging so published dots appear on top of pledged dots and are visible
ML_geo <- ML_geo %>% arrange(!is.na(Notes), Notes)

# Creating map outlines as a gg object 
us_states <- states()
us_states <- us_states[, c("NAME", "geometry")]
us_states <- us_states[-c(35:37,42),]
plot(us_states)

all_usa <- st_as_sf(us_states)
all_usa


# Creating ML map
ML_map <- ggplot() + 
  geom_sf(data = all_usa, fill = "transparent") +
  geom_count(data = ML_geo %>% filter(is.na(Notes)), aes(x = lon, y = lat, color = Notes)) +
  geom_count(data = ML_geo %>% filter(Notes == "Published"), aes(x = lon, y = lat, color = Notes)) +
  scale_color_manual(values = c("#643f7c"), na.value = "#93b28d") + 
  theme_classic() + 
  xlim(-180, -60) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'))

ML_map
ggsave("ML_map.png", ML_map, width = 12, height = 10)

# Creating Visits map
all_usa_visits <- all_usa %>%
  arrange((NAME))
all_usa_visits$visit <- c(1,2,1,2,1,1,1,2,2,1,1,2,1,1,1,1,1,1,2,2,1,1,1,1,2,1,2,1,
                          2,1,2,1,1,1,2,1,2,2,1,2,2,2,2,1,1,1,2,1,1,1,1,2)
all_usa_visits$visit <- as.character(all_usa_visits$visit)
map_visits <- ggplot() + 
  geom_sf(data = all_usa_visits, aes(fill = visit)) +
  scale_fill_manual(values = c("#E89142", "#687DA2")) + 
  theme_classic() + 
  xlim(-180, -60) +
  theme(
    panel.background = element_rect(fill='transparent'), #transparent panel bg
    plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
    legend.background = element_rect(fill='transparent'), #transparent legend bg
    legend.box.background = element_rect(fill='transparent'))

map_visits

# Adding ML map on top of visits map
ML_map_visits <- map_visits + 
  geom_count(data = ML_geo %>% filter(is.na(Notes)), aes(x = lon, y = lat, color = Notes)) +
  geom_count(data = ML_geo %>% filter(Notes == "Published"), aes(x = lon, y = lat, color = Notes)) +
  scale_color_manual(values = c("#643f7c"), na.value = "#C2C354")
  
ML_map_visits
ggsave("ML_map_visits.png", ML_map_visits, width = 12, height = 10)


# cool but doesn't have alaska, HI, or PR



  # trying w scale_color_gradientn - THE plor
  # ggplot()+
  #   geom_point(data = endo_herb_AGHY, aes(x = lon, y = lat, color = trate), pch = 20, size=0.8)+
#   scale_color_gradient(low="darkmagenta", high = "plum1",
#                         breaks= seq(0,1, by = 0.2),
#                         limits = c(0,1),
#                         labels = as.character(seq(0,1,by = 0.2)),
#                        name=NULL )+
#   ggtitle("Transmission Rates of Endophyte to Offspring")+
#   xlab("Longitude")+ylab("Latitude")+
#   geom_sf(data = usa, fill = "transparent")+
#   theme_classic()