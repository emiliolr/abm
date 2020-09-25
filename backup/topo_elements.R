library(tidyverse)
library(sf)
library(raster)
library(rayrender)
library(rayshader)
library(png)

#setup stuff...
setwd("~/Desktop/DATA\ 440/Deliverables")
path_to_add_data <- "~/Desktop/DATA\ 440/ABM_Summer_Stuff/section_2.1/data"

#bringing in all the data
load("adm2_urban_areas.RData")
load("for_roads_and_healthcare.RData")
swz_topo <- raster("Data/swz_srtm_topo_100m.tif")
swz_adm2 <- str_c(path_to_add_data, "/gadm36_SWZ_shp/gadm36_SWZ_2.shp") %>% read_sf()

#preparing the data
comb_topo <- crop(swz_topo, urban_areas_exp) #cropping down to the bouding box
comb_matrix <- raster_to_matrix(comb_topo) #a rayshader function (DIMENSIONS: 300x364)

#plotting the data in 2D and 3D using rayshader
comb_matrix %>% 
  sphere_shade() %>% #shading the topo raster
  add_water(detect_water(comb_matrix)) %>% #adding in any detected bodies of water
  plot_map() #plotting

ambient_shadows <- ambient_shade(comb_matrix) #adding ambient shadows

comb_matrix %>% #making a 3D plot --> displays in a new window
  sphere_shade(texture = "imhof3") %>%
  add_water(detect_water(comb_matrix), color = "azure") %>% 
  add_shadow(ray_shade(comb_matrix, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
  add_shadow(lamb_shade(comb_matrix, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
  add_shadow(ambient_shadows, max_darken = 0.1) %>%
  plot_3d(comb_matrix, zscale = 20, windowsize = c(1000, 1000), phi = 40, theta = 130, zoom = 0.75,
          background = "grey40", shadowcolor = "grey5", soliddepth = -50, shadowdepth = -100)
  
render_snapshot() #rendering a snapshot on the plot while the window is open

#elements to add to the 2D/3D plots
comb_elements <- ggplot() + 
  geom_sf(data = comb_adm2, fill = NA, color = "gold", linetype = "11", size = 1.5) + #making the outline
  geom_sf(data = urban_areas_exp, alpha = 0.5, fill = "gold3", color = NA) +
  geom_sf(data = primary_routes_comb, color = "orange", size = 1.2) +
  geom_sf(data = secondary_routes_comb, color = "orange", size = 0.4) +
  # geom_sf(data = tertiary_routes_comb, color = "range3", size = 0.2) +
  geom_sf(data = comb_schools, size = 2.5, color = "deepskyblue") +
  geom_sf(data = comb_health_care, color = "hotpink", shape = 3, size = 3.5, show.legend = FALSE) +
  theme_void() +
  scale_x_continuous(expand = expansion()) +
  scale_y_continuous(expand = expansion())
comb_elements

png("Figures/combined.png", width = 300, height = 364, units = "px", bg = "transparent") #saving the outline as a .png
comb_elements
dev.off()

#adding the adm2 outline to the 2D/3D plots
overlay_img <- readPNG("Figures/combined.png")

comb_matrix %>% #the 2D plot
  sphere_shade() %>% 
  add_water(detect_water(comb_matrix)) %>% 
  add_overlay(overlay_img, alphalayer = 0.95) %>% #adding the adm2 stuff
  plot_map() 

comb_matrix %>% #the 3D plot
  sphere_shade(texture = "imhof3") %>%
  add_water(detect_water(comb_matrix), color = "azure") %>% 
  add_shadow(ray_shade(comb_matrix, sunaltitude = 3, zscale = 33, lambert = FALSE), max_darken = 0.5) %>%
  add_shadow(lamb_shade(comb_matrix, sunaltitude = 3, zscale = 33), max_darken = 0.7) %>%
  add_shadow(ambient_shadows, max_darken = 0.1) %>%
  add_overlay(overlay_img, alphalayer = 0.95) %>% 
  plot_3d(comb_matrix, zscale = 20, windowsize = c(1000, 1000), phi = 50, theta = 350, zoom = 0.65,
          background = "grey40", shadowcolor = "grey5", soliddepth = -50, shadowdepth = -100)
  
render_snapshot("Figures/adm2_w_topo.png")
