library(sf)
library(raster)
library(tidyverse)

setwd("~/Desktop/DATA\ 440/Deliverables")

#Bringing in the urban areas data and household data
load("for_desc_of_settle.RData") #urban_areas and swz_mk
load("shell_pred.RData") #shell_pred

#Bringing in adm2 data
swz_adm2 <- read_sf("Data/gadm36_SWZ_shp/gadm36_SWZ_2.shp")

#A spatial join to assign points to their adm2
located_individuals <- st_join(shell_pred, swz_adm2, join = st_within)

mk_individuals <- located_individuals %>% filter(NAME_2 == "Mkhiweni")

ggplot() + #checking out results... everything seems to have come together correctly
  geom_sf(data = swz_mk) +
  geom_sf(data = urban_areas, fill = "maroon") +
  geom_sf(data = mk_individuals, size = 0.2, alpha = 0.5) #definitely too many ppl in sparsely pop areas...

#Get centroids for the urban areas
urban_centroids <- st_centroid(urban_areas) %>% st_cast("MULTIPOINT")

ggplot() + #checking that centroids were computed correctly
  geom_sf(data = swz_mk, fill = "grey80") +
  geom_sf(data = urban_areas, fill = "grey70") +
  geom_sf(data = urban_centroids, color = "black")

#Putting together voronoi tessellations
mk_bb <- st_bbox(swz_mk) %>% st_as_sfc() #getting bounding box and turning into a poly

ggplot() + #seeing bbox
  geom_sf(data = mk_bb) +
  geom_sf(data = swz_mk)

mk_voronoi <- st_voronoi(st_union(urban_centroids), mk_bb) %>% #producing voronoi polys - SEEMS NOT TO BE USING BBOX!
  st_collection_extract("POLYGON") #making the collection into many polys

ggplot() + #plotting for visual confirmation
  geom_sf(data = swz_mk, fill = "grey80") +
  geom_sf(data = urban_areas, fill = "grey70") +
  geom_sf(data = mk_voronoi, fill = NA, color = "black") +
  geom_sf(data = urban_centroids) +
  theme_void()

mk_voronoi_adm2 <- mk_voronoi %>% st_intersection(swz_mk) #only keeping portion in Mkhiweni

ggplot() + #checking the cleaner polys
  theme_void() +
  geom_sf(data = mk_voronoi_adm2, fill = NA, color = "black") +
  geom_sf(data = urban_centroids)

save(mk_voronoi_adm2, swz_mk, urban_areas, urban_centroids, file = "for_gravity_model.RData")
