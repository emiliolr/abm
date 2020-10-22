library(tidyverse)
library(sf)
library(raster) #raster may create issues...
library(haven) 

setwd("~/Desktop/DATA\ 440/Deliverables")

#Initial elements
swz_avg_pph <- 4.6 #found in the DHS survey report

swz_pop_2006 <- raster("Data/swz_ppp_2006.tif") #at 100m resolution...

dhs_spatial <- read_sf("Data/swz_DHS_spatial/SZGE53FL.shp") #reading in the DHS shape file
swz_adm1 <- read_sf("Data/gadm36_SWZ_shp/gadm36_SWZ_1.shp") #only adm1 are of interest here...

dhs_household <- read_dta("Data/swz_DHS_data/household/szhr51fl.dta") #household data

#Transforming projections - might be messing w/the buffers later on
swz_adm1 <- swz_adm1 %>% st_transform(22291) 
dhs_spatial <- dhs_spatial %>% st_transform(22291)

#Modifying the data
no_loc <- dhs_spatial %>% filter(LATNUM == 0 | LONGNUM == 0) %>% .$DHSCLUST %>% unique() #the clusters with no loc data
dhs_spatial <- dhs_spatial %>% filter(LATNUM != 0 | LONGNUM != 0) #removing clusters w/no location data from spatial
dhs_household_filtered <- dhs_household %>% filter(!(hv001 %in% no_loc)) #removing houses from these clusters
dhs_household_sub <- dhs_household_filtered %>% select(hv001, hv009) #grabbing cluster number and household size

combined_data <- left_join(dhs_spatial, dhs_household_sub, by = c("DHSCLUST" = "hv001")) #combining spatial w/households

dhs_rural_obs <- combined_data %>% filter(URBAN_RURA == "R") #nabbing rural
urban_2 <- combined_data %>% filter(URBAN_RURA == "U") #nabbing urban - will be displaced in 2 km radius

clusters <- dhs_rural_obs$DHSCLUST %>% unique() #getting all rural clusters
clusters_10 <- sample(clusters, (length(clusters) * 0.01) %>% round())
rural_10 <- dhs_rural_obs %>% filter((DHSCLUST %in% clusters_10)) #the 1% to be displaced 10 km
rural_5 <- dhs_rural_obs %>% filter(!(DHSCLUST %in% clusters_10)) #99% of rural will just be displaced in 5 km radius

#Displacing/sampling the data w/in prescribed dists
rural_10_buffs <- rural_10 %>% 
  distinct(.keep_all = TRUE) %>% #keeping only distinct elements
  st_buffer(10000) %>% #buffering by 10 km (10000 m)
  st_intersection(swz_adm1) %>% #splitting out the geometry portions that intersect adm1 - VERY TRICKY!
  filter(NAME_1 == ADM1FIPSNA) #only keeping the buffers that are in the right adm1
  
plot(st_geometry(rural_10_buffs)) #making sure buffering worked correctly
plot(st_geometry(swz_adm1), add = TRUE)

rural_10_pts <- st_sample(rural_10_buffs %>% arrange(DHSCLUST), table(rural_10$DHSCLUST)) #randomly distributing the households

plot(st_geometry(rural_10_pts)) #checking out result of sampling
plot(st_geometry(rural_10_buffs), add = TRUE)

rural_10_cluster_order <- table(rural_10$DHSCLUST) %>% names() %>% as.numeric() #adding the correct cluster num to the points
rural_10_num_houses <- table(rural_10$DHSCLUST) %>% as.numeric()

rural_10_cluster_col <- rep(rural_10_cluster_order, #producing a vector w/the correct reps of each cluster num
                           times = rural_10_num_houses)

rural_10_pts <- rural_10_pts %>% #smashing the cluster num onto the point geometry
  st_sf() %>% 
  add_column(cluster_num = rural_10_cluster_col)

ggplot() + #checking that everything ended up in the right place
  geom_sf(data = rural_10_pts, mapping = aes(color = as.factor(cluster_num))) +
  geom_sf(data = rural_10_buffs, mapping = aes(color = as.factor(DHSCLUST)), fill = NA)

rural_5_buffs <- rural_5 %>% 
  distinct(.keep_all = TRUE) %>% 
  st_buffer(5000) %>% #buffering by 5 km (5000 m)
  st_intersection(swz_adm1) %>% 
  filter(NAME_1 == ADM1FIPSNA) 
rural_5_pts <- st_sample(rural_5_buffs %>% arrange(DHSCLUST), table(rural_5$DHSCLUST))

rural_5_cluster_order <- table(rural_5$DHSCLUST) %>% names() %>% as.numeric() #adding the correct cluster num to the points
rural_5_num_houses <- table(rural_5$DHSCLUST) %>% as.numeric()

rural_5_cluster_col <- rep(rural_5_cluster_order, #producing a vector w/the correct reps of each cluster num
                           times = rural_5_num_houses)

rural_5_pts <- rural_5_pts %>% #smashing the cluster num onto the point geometry
  st_sf() %>% 
  add_column(cluster_num = rural_5_cluster_col) 

ggplot() + #checking that everything ended up in the right place
  geom_sf(data = rural_5_pts %>% filter(cluster_num %in% 50:70), mapping = aes(color = as.factor(cluster_num))) +
  geom_sf(data = rural_5_buffs %>% filter(DHSCLUST %in% 50:70), mapping = aes(color = as.factor(DHSCLUST)), fill = NA)

urban_2_buffs <- urban_2 %>% 
  distinct(.keep_all = TRUE) %>% 
  st_buffer(2000) %>% #buffering by 2 km (2000 m)
  st_intersection(swz_adm1) %>% 
  filter(NAME_1 == ADM1FIPSNA) 
urban_2_pts <- st_sample(urban_2_buffs %>% arrange(DHSCLUST), table(urban_2$DHSCLUST))

urban_2_cluster_order <- table(urban_2$DHSCLUST) %>% names() %>% as.numeric()
urban_2_num_houses <- table(urban_2$DHSCLUST) %>% as.numeric()

urban_2_cluster_col <- rep(urban_2_cluster_order, times = urban_2_num_houses)

urban_2_pts <- urban_2_pts %>%
  st_sf() %>% 
  add_column(cluster_num = urban_2_cluster_col) 

ggplot() + #checking that everything ended up in the right place again
  geom_sf(data = urban_2_pts %>% filter(cluster_num %in% 50:70), mapping = aes(color = as.factor(cluster_num))) +
  geom_sf(data = urban_2_buffs %>% filter(DHSCLUST %in% 50:70), mapping = aes(color = as.factor(DHSCLUST)), fill = NA)

#NOT NECESSARY: they're already sf objects
# rural_10_pts <- st_sf(rural_10_pts) %>% rename(points = "rural_10_pts") #converting to sf objects
# rural_5_pts <- st_sf(rural_5_pts) %>% rename(points = "rural_5_pts")
# urban_2_pts <- st_sf(urban_2_pts) %>% rename(points = "urban_2_pts")

all_pts <- rbind(rural_10_pts, rural_5_pts, urban_2_pts)

#Checking out the results w/all elements
png(filename = "Figures/distributed_points.png", width = 1500, height = 1500)
plot(st_geometry(all_pts), cex = 0.25) 
plot(st_geometry(swz_adm1), add = TRUE)

plot(st_geometry(rural_10_buffs), add = TRUE)
plot(st_geometry(rural_5_buffs), add = TRUE)
plot(st_geometry(urban_2_buffs), add = TRUE)
dev.off()

#Joining the distributed locations back to the data
#  could stop here - doesn't include clusters w/missing spatial data (~90 obs)
dhs_household_sf <- dhs_household_filtered %>% #adding the points to the data, making sure to give each house a point in the correct cluster
  arrange(hv001) %>% 
  bind_cols(arrange(all_pts, cluster_num)) %>% 
  select(-cluster_num)

st_geometry(dhs_household_sf) <- dhs_household_sf$geometry #making it an sf object

ggplot() + #checking things out again - seems like it worked out ok
  geom_sf(data = dhs_household_sf %>% filter(hv001 %in% 220:235), mapping = aes(color = as.factor(hv001))) +
  geom_sf(data = rural_5_buffs %>% filter(DHSCLUST %in% 220:235), mapping = aes(color = as.factor(DHSCLUST)), alpha = 0) +
  geom_sf(data = urban_2_buffs %>% filter(DHSCLUST %in% 220:235), mapping = aes(color = as.factor(DHSCLUST)), alpha = 0)

#Adding back in the missing clusters, distributed w/"rpoint()"
load("Data/swz_rpoint_by_region.RData") #comes in as "hh_houses", etc. - they're PPPs

no_loc_households <- dhs_household %>% filter(hv001 %in% no_loc)

num_each_region <- no_loc_households %>% #grabbing the num of households in each region for missing data
  select(hv024) %>%
  mutate(hv024 = as_factor(hv024)) %>% 
  count(hv024) %>% 
  deframe()

dist_hh_houses <- hh_houses %>% 
  as_tibble() %>% 
  slice_sample(n = num_each_region["hhohho"])

dist_lu_houses <- lu_houses %>% 
  as_tibble() %>% 
  slice_sample(n = num_each_region["lubombo"])

dist_mz_houses <- mz_houses %>% 
  as_tibble() %>% 
  slice_sample(n = num_each_region["manzini"])

dist_houses <- bind_rows(dist_hh_houses, dist_mz_houses, dist_lu_houses) #putting houses all together

no_loc_households <- no_loc_households %>% #smashing the houses onto the original df
  arrange(hv024) %>%
  bind_cols(dist_houses) %>% 
  st_as_sf(coords = c("x", "y"), crs = 4326)

ggplot() + #checking results - looks like things ended up in the right places
  geom_sf(data = swz_adm1) +
  geom_sf(data = no_loc_households, mapping = aes(color = as_factor(hv024)))

#Making final df w/missing and non-missing obs
dhs_final_data <- dhs_household_sf %>% bind_rows(no_loc_households)

ggplot() + #seeing final product
  geom_sf(data = swz_adm1) +
  geom_sf(data = no_loc_households, color = "maroon", size = 1, alpha = 0.8) +
  geom_sf(data = dhs_household_sf, color = "darkolivegreen4", size = 1, alpha = 0.4) +
  geom_sf(data = swz_adm1, color = "black", alpha = 0)

#Saving some of the stuff for use in the write up
save(no_loc_households, all_pts, rural_10_buffs, urban_2_buffs, rural_5_buffs, 
     file = "Data/gps_for_p2_deliverable.RData")
