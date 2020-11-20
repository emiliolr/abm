library(sf)
library(tidyverse)
library(gganimate)
library(gravity)

#Changing the working directory
setwd("~/Desktop/DATA\ 440/Deliverables")

#Bringing in SWZ adm1 data
swz_adm1 <- read_sf("Data/gadm36_SWZ_shp/gadm36_SWZ_1.shp")

#Bringing in the data from WorldPop
flow_data <- read_csv("Data/SWZ_5yrs_InternalMigFlows_2010/SWZ_5yrs_InternalMigFlows_2010.csv")
centroid_data <- read_sf("Data/SWZ_5yrs_InternalMigFlows_2010/SWZ_AdminUnit_Centroids/SWZ_AdminUnit_Centroids.shp")

#  key: 1 - Hhohho, 2 - Lubombo, 3 - Manzini, 4 - Shiselweni
ggplot() + #plotting for visual confirmation
  geom_sf(data = swz_adm1) +
  geom_sf(data = centroid_data) +
  geom_sf_text(data = centroid_data, mapping = aes(label = IPUMSID), nudge_y = 0.05)

#Making the OD matrix
OD_matrix <- flow_data %>% 
  dplyr::select(NODEI, NODEJ, PrdMIG) %>%
  pivot_wider(names_from = NODEJ, values_from = PrdMIG) %>% 
  dplyr::select(`1`, everything(), -NODEI)

#Adding dists to flow data
pts_1 <- dplyr::select(flow_data, LONFR, LATFR) %>% st_as_sf(coords = c("LONFR", "LATFR"), crs = st_crs(swz_adm1))
pts_2 <- dplyr::select(flow_data, LONTO, LATTO) %>% st_as_sf(coords = c("LONTO", "LATTO"), crs = st_crs(swz_adm1))

final_flow_data <- flow_data %>% 
  add_column(dist = st_distance(pts_1, pts_2, by_element = TRUE)) %>% #compares dist pairwise between point collections
  dplyr::select(-LONFR, -LATFR, -LONTO, -LATTO)

#Making a dist matrix for fun
dist_matrix <- final_flow_data %>% 
  dplyr::select(NODEI, NODEJ, dist) %>%
  mutate(dist = as.numeric(dist)) %>% 
  pivot_wider(names_from = NODEJ, values_from = dist) %>% 
  dplyr::select(`1`, everything(), -NODEI)

#Some visualization
#  1. looking at aggregate out/in for each centroid
origin_flows <- flow_data %>% st_as_sf(coords = c("LONFR", "LATFR"), crs = st_crs(swz_adm1))
destination_flows <- flow_data %>% st_as_sf(coords = c("LONTO", "LATTO"), crs = st_crs(swz_adm1))

#  summarizing
origin_summarized <- origin_flows %>% #for leaving the centroid
  group_by(NODEI) %>% 
  summarize(migration = sum(PrdMIG))

destinations_summarized <- destination_flows %>%  #for coming to the centroid
  group_by(NODEJ) %>% 
  summarize(migration = sum(PrdMIG))

#  plotting aggregate in/out for each centroid
ggplot() + #leaving the centroid
  geom_sf(data = swz_adm1) +
  geom_sf(data = origin_summarized, mapping = aes(size = migration))

ggplot() + #coming to the centroid
  geom_sf(data = swz_adm1) +
  geom_sf(data = destinations_summarized, mapping = aes(size = migration))

#  2. working on a more descriptive graph (includes edges)
adm1_centroids <- st_centroid(swz_adm1) %>% st_geometry()

#  if you want to do just one centroid
# OD_combos <- expand.grid(adm1_centroids, adm1_centroids) #finding all possible combos of origins and dests

multi_pts <- st_union(st_geometry(adm1_centroids), st_geometry(adm1_centroids)) %>% #creating structure for lines between centroids
  st_as_sf()
multi_pts <- multi_pts %>% filter(st_geometry_type(multi_pts) != "POINT") #pulling out those that go to themselves
edge_lines <- multi_pts %>% st_cast("LINESTRING") %>% st_as_sf() 

ggplot() +
  geom_sf(data = swz_adm1) +
  geom_sf(data = edge_lines, color = "grey60") +
  geom_sf(data = adm1_centroids) 

#  for when we were just doing one district
# edge_lines_final <- edge_lines %>% add_column(filter(origin_flows, NODEI == 1) %>% 
#                             st_set_geometry(NULL) %>% 
#                             dplyr::select(PrdMIG))
# 
# ggplot() + #plotting amount of migration out of Hhhohho
#   theme_bw() +
#   geom_sf(data = swz_adm1) +
#   geom_sf(data = edge_lines_final, mapping = aes(size = PrdMIG), lineend = "round", show.legend = FALSE) +
#   geom_sf(data = origin_summarized, mapping = aes(color = as.factor(NODEI)), size = 6, show.legend = FALSE) 

#An animation of the movement
num_steps <- 20

pts_for_anim <- st_line_sample(st_transform(edge_lines, 32736), num_steps) %>% #sampling pts off of the edge lines
  st_cast("POINT") %>% 
  st_as_sf() %>%
  st_transform(4979)

#  adding things for animation
pts_for_anim <- pts_for_anim %>% 
  mutate(time = rep(1:num_steps, 12), #adding a time for animation purposes
         id = rep(1:12, each = num_steps), #adding an ID to diff agents
         lon = st_coordinates(pts_for_anim)[,1], #adding lon/lat for animation
         lat = st_coordinates(pts_for_anim)[,2]) 

back_plot <- ggplot() + #checking that everything worked correctly
  theme_void() +
  geom_sf(data = swz_adm1, fill = "lightskyblue1", color = "black") +
  geom_sf(data = edge_lines, color = "grey65") +
  geom_sf(data = adm1_centroids, size = 3) +
  geom_point(data = pts_for_anim, mapping = aes(x = lon, y = lat, color = as.factor(id)), #for gganimate to work, has to be points
             show.legend = FALSE)
back_plot

anim <- back_plot + #adding the animation-specific stuff
  transition_reveal(along = time) +
  ease_aes()

gganimate::animate(anim, nframes = 100, fps = 20) #actually animating the movement
anim_save("Figures/moving_people_all_districts.gif") #saving the animation

#Producing a national level gravity model
final_flow_data <- final_flow_data %>% 
  mutate(var = rep(1, nrow(final_flow_data)), #requires an additional var for estimation, so using a dummy var
         dist = as.numeric(dist))

#  an initial model - double demeaning
ddm_fit <- ddm(
  dependent_variable = "PrdMIG",
  distance = "dist",
  additional_regressors = c("var"), 
  code_origin = "NODEI",
  code_destination = "NODEJ",
  data = final_flow_data
)

#  checking the model fit
summary(ddm_fit)

#  trying another model - poisson pseudo maximum likelihood
ppml_fit <- ppml(
  dependent_variable = "PrdMIG",
  distance = "dist",
  additional_regressors = c("var"), #TODO: replace with NTL
  data = final_flow_data
)

#  looking at the fitted vals
fitted(ppml_fit) #produces fitted vals that are more approachable as compared to ddm... looks to be a good pred

#Trying to use the model to predict flows in Mkhiweni
load("for_gravity_model.RData") #bringing in swz_mk, urban_areas, urban_centroids, mk_voronoi_adm2

ggplot() + #checking everything
  geom_sf(data = swz_mk) +
  geom_sf(data = urban_areas, fill = "grey55") +
  geom_sf(data = urban_centroids) +
  geom_sf(data = mk_voronoi_adm2, alpha = 0, color = "black")

#  getting dist between urban centroids
dists <- st_distance(st_geometry(urban_centroids), st_geometry(urban_centroids)) %>% as.data.frame() #a matrix currently

mk_dist_data <- pivot_longer(dists, V1:V12, names_to = "centroid_to", values_to = "dist") %>%
  mutate(dist = as.numeric(dist),
         centroid_to = str_remove(centroid_to, "V"),
         centroid_from = rep(1:12, each = 12), 
         var = 1) %>% 
  dplyr::select(centroid_from, everything()) %>% 
  filter(dist != 0)

#  pred at the adm2 level
predict(ppml_fit, mk_dist_data$dist) #doesn't work currently

#TODO: bring in NTL data and integrate into gravity model analysis
#TODO: figure out how to predict movement w/the new adm2 data

#TODO: animation... either have more people moving (more pts) OR resize the pts to reflect magnitude of flow
#TODO: work on improving the in/out movement visualization (all in one plot...?)
