library(sf)
library(raster)
library(tidyverse)
library(gganimate)
library(gravity)
library(doParallel)
library(snow)

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
ggplot() + #leaving/coming into the centroid
  theme_void() +
  geom_sf(data = swz_adm1, fill = "lightskyblue1", color = "black") +
  geom_sf(data = origin_summarized, mapping = aes(fill = migration), size = 5, shape = 21) +
  geom_sf(data = destinations_summarized, mapping = aes(color = migration), size = 6, shape = 13, 
          fill = NA, show.legend = FALSE) +
  scale_color_viridis_c(option = "cividis") +
  scale_fill_viridis_c(option = "cividis") +
  labs(fill = "Migration Flows")

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
  geom_sf(data = adm1_centroids, size = 3) +
  geom_point(data = pts_for_anim, mapping = aes(x = lon, y = lat, color = as.factor(id)), #for gganimate to work, has to be points
             show.legend = FALSE)
back_plot

anim <- back_plot + #adding the animation-specific stuff
  transition_reveal(along = time) +
  ease_aes()

gganimate::animate(anim, nframes = 100, fps = 20) #actually animating the movement
anim_save("Figures/moving_people_all_districts.gif") #saving the animation

#Trying to add more points for a more visually interesting animation
test_pts <- tibble(lon = st_coordinates(adm1_centroids)[,1], 
                   lat = st_coordinates(adm1_centroids)[,2])

migration_magnitudes <- final_flow_data %>% #pulling out magnitude of flows... will change num of pts based on this
  filter(NODEI == 1) %>% 
  dplyr::select(PrdMIG) %>% 
  deframe()

resized_magnitudes <- floor(migration_magnitudes / 25) #getting the proportionate flows

#  very garbage hardcoded df for animation... PROOF OF CONCEPT!
more_people <- tibble(lon = c(rep(test_pts$lon[[1]], sum(resized_magnitudes)),
                              rep(test_pts$lon[[2]], resized_magnitudes[1]), 
                              rep(test_pts$lon[[3]], resized_magnitudes[2]), 
                              rep(test_pts$lon[[4]], resized_magnitudes[3])), 
                   lat = c(rep(test_pts$lat[[1]], sum(resized_magnitudes)), 
                           rep(test_pts$lat[[2]], resized_magnitudes[1]), 
                           rep(test_pts$lat[[3]], resized_magnitudes[2]), 
                           rep(test_pts$lat[[4]], resized_magnitudes[3])),
                   id = rep(1:sum(resized_magnitudes), 2) %>% as.factor(),
                   time = c(rep(1, sum(resized_magnitudes)), rep(2, sum(resized_magnitudes))))

anim_v2 <- ggplot() + #creating the animation plot
  geom_sf(data = swz_adm1, fill = "lightskyblue1", color = "black") +
  geom_sf_text(data = swz_adm1, mapping = aes(label = NAME_1), size = 6, alpha = 0.5, color = "grey40") +
  geom_jitter(data = more_people, mapping = aes(x = lon, y = lat, color = id), width = 0.15, 
              height = 0.15, show.legend = FALSE) +
  theme_void() +
  transition_reveal(along = time) +
  ease_aes()

gganimate::animate(anim_v2, nframes = 100, fps = 20) #animating
anim_save("Figures/moving_people_lots_of_people.gif") #saving the animation

#Producing a national level gravity model
final_flow_data <- final_flow_data %>% mutate(dist = as.numeric(dist))

#  bringing in NTL as an additional push/pull factor in the model
swz_ntl <- raster("~/Desktop/DATA 440/ABM_Summer_Stuff/section_2.1/data/landuse_cover/swz_viirs_100m_2015.tif")
swz_pop20 <- raster("~/Desktop/DATA 440/ABM_Summer_Stuff/section_2.1/data/pop_data/swz_ppp_2020_1km_Aggregated.tif")

beginCluster(n = detectCores() - 1) #parallelizing the process - still takes a hot sec...
swz_ntl_adm1 <- raster::extract(swz_ntl, swz_adm1, df = TRUE) #extracting to the adm1 level
swz_pop20_adm1 <- raster::extract(swz_pop20, swz_adm1, df = TRUE)
endCluster()

agg_ntl <- swz_ntl_adm1 %>% #aggregating ntl to the adm1 level
  group_by(ID) %>%
  summarize(ttl_ntl = sum(swz_viirs_100m_2015, na.rm = TRUE)) #ends up w/coding as flow data (1 - Hhohho, etc.)

agg_pop <- swz_pop20_adm1 %>% #same for pop
  group_by(ID) %>%
  summarize(ttl_pop = sum(swz_ppp_2020_1km_Aggregated, na.rm = TRUE))

ntl_od <- expand.grid(agg_ntl$ttl_ntl, agg_ntl$ttl_ntl) %>% #getting data into a nicer format
  rename(destination_ntl = Var1, origin_ntl = Var2) %>%
  dplyr::select(origin_ntl, destination_ntl) %>% 
  filter(origin_ntl != destination_ntl)

pop_od <- expand.grid(agg_pop$ttl_pop, agg_pop$ttl_pop) %>% #same procedure for pop data
  rename(destination_pop = Var1, origin_pop = Var2) %>%
  dplyr::select(origin_pop, destination_pop) %>% 
  filter(origin_pop != destination_pop)

final_flow_data <- final_flow_data %>% bind_cols(ntl_od, pop_od) #adding ntl/pop data to flow data

#  an initial model - double demeaning
ddm_fit <- ddm(
  dependent_variable = "PrdMIG",
  distance = "dist",
  additional_regressors = c("origin_ntl", "destination_ntl", "origin_pop", "destination_pop"), 
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
  additional_regressors = c("origin_ntl", "destination_ntl", "origin_pop", "destination_pop"), 
  data = final_flow_data
)

#  looking at the fitted vals + summary
summary(ppml_fit)
ppml_district_fit <- fitted(ppml_fit) #produces fitted vals that are more interpretable as compared to ddm... looks to be a good pred

resid_matrix <- final_flow_data %>% #making a matrix of resids for writeup
  add_column(resid = final_flow_data$PrdMIG - as.numeric(ppml_district_fit)) %>%
  dplyr::select(NODEI, NODEJ, resid) %>%
  pivot_wider(names_from = NODEJ, values_from = resid) %>%
  dplyr::select(NODEI, `1`, everything()) %>%
  rename(origin_node = NODEI)

#  fitting a ppml w/out additional regressors for comparison
ppml_worse_fit <- final_flow_data %>% 
  mutate(var = 1) %>%
  ppml(
    dependent_variable = "PrdMIG",
    distance = "dist",
    additional_regressors = c("var"), 
    data = .
  )

ppml_worse_fitted <- fitted(ppml_worse_fit)

resid_matrix_worse <- final_flow_data %>% #making a matrix of resids for comparison in writeup
  add_column(resid = final_flow_data$PrdMIG - as.numeric(ppml_worse_fitted)) %>%
  dplyr::select(NODEI, NODEJ, resid) %>%
  pivot_wider(names_from = NODEJ, values_from = resid) %>%
  dplyr::select(NODEI, `1`, everything()) %>%
  rename(origin_node = NODEI)

#  calculating model RMSEs
CalcRMSE <- function(observed, estimated){
  res <- (observed - estimated)^2
  RMSE <- sqrt(mean(res))
  RMSE
}

CalcRMSE(final_flow_data$PrdMIG, ppml_district_fit) #for better model
CalcRMSE(final_flow_data$PrdMIG, ppml_worse_fitted) #for worse model

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
         centroid_from = rep(1:12, each = 12)) %>% 
  dplyr::select(centroid_from, everything()) %>% 
  filter(dist != 0)

#  adding NTL/pop for adm2
beginCluster(n = detectCores() - 1) 
swz_ntl_urban_polys <- raster::extract(swz_ntl, st_as_sf(mk_voronoi_adm2), df = TRUE) #extracting ntl to the voronoi polys
swz_pop20_urban_polys <- raster::extract(swz_pop20, st_as_sf(mk_voronoi_adm2), df = TRUE)
endCluster()

agg_ntl_urban <- swz_ntl_urban_polys %>% #aggregating to the urban polys
  group_by(ID) %>%
  summarize(ttl_ntl = sum(swz_viirs_100m_2015, na.rm = TRUE))

agg_pop_urban <- swz_pop20_urban_polys %>% #same for pop
  group_by(ID) %>%
  summarize(ttl_pop = sum(swz_ppp_2020_1km_Aggregated, na.rm = TRUE))

ntl_od_urban <- expand.grid(agg_ntl_urban$ttl_ntl, agg_ntl_urban$ttl_ntl) %>% 
  rename(destination_ntl = Var1, origin_ntl = Var2) %>%
  dplyr::select(origin_ntl, destination_ntl) %>% 
  filter(origin_ntl != destination_ntl)

pop_od_urban <- expand.grid(agg_pop_urban$ttl_pop, agg_pop_urban$ttl_pop) %>% 
  rename(destination_pop = Var1, origin_pop = Var2) %>%
  dplyr::select(origin_pop, destination_pop) %>% 
  filter(origin_pop != destination_pop)

mk_dist_data <- mk_dist_data %>% 
  bind_cols(ntl_od_urban, pop_od_urban) %>%
  mutate(dist_log = log(dist))

#  predicting at the adm2 level
mk_pred_flows <- mk_dist_data %>% #seems to work correctly, but the estimated model doesn't seem to translate well to more granular flows...
  dplyr::select(centroid_from, centroid_to) %>%
  add_column(pred_flows = exp(predict(ppml_fit, mk_dist_data))) 

mk_dist_data[1, ] %>% add_row(mk_dist_data[1, ] %>% mutate(origin_ntl = 12000)) %>% predict(ppml_fit, .) %>% exp()

#Saving all the things that are needed for the writeup
save(OD_matrix, dist_matrix, origin_summarized, destinations_summarized, final_flow_data, resid_matrix, ppml_fit,
     mk_pred_flows, resid_matrix_worse, file = "for_P3_writeup.RData")
