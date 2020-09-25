library(tidyverse)
library(sf)
library(raster)
library(spatstat)
library(maptools)
library(ggrepel)
library(units)
library(snow)
library(doParallel)

#Setting things up...
setwd("~/Desktop/DATA\ 440/Deliverables")
path_to_data <- "~/Desktop/DATA\ 440/ABM_Summer_Stuff/section_2.1/data"

#Bringing in pop and sf data
swz_adm2 <- str_c(path_to_data, "/gadm36_SWZ_shp/gadm36_SWZ_2.shp") %>% read_sf()
# pop20 <- str_c(path_to_data, "/pop_data/swz_ppp_2020_1km_Aggregated.tif") %>% raster()
pop20 <- raster("Data/swz_ppp_2020.tif")

swz_mk <- swz_adm2 %>% filter(NAME_2 == "Mkhiweni") #subsetting to Mkhiweni, Mphalaleni, Ekukhanyeni
swz_mp <- swz_adm2 %>% filter(NAME_2 == "Mphalaleni")
swz_ek <- swz_adm2 %>% filter(NAME_2 == "Ekukhanyeni")

comb_adm2 <- st_union(swz_mk, swz_mp) %>% st_union(swz_ek) #putting the selected adm2s together

plot(st_geometry(comb_adm2)) #taking a look

pop20_adm2 <- crop(pop20, comb_adm2) #zooming on the bounding box
pop20_adm2 <- mask(pop20_adm2, comb_adm2) #cutting out the relevant gridcells

adm2_pop_ttl <- cellStats(pop20_adm2, "sum") #the estimated pop for the adm2s

plot(pop20_adm2, main = NULL) #looking at the pop and geometry together
plot(st_geometry(comb_adm2), add = TRUE)

#Writing our sf object back to a .shp file and reading in w/maptools
comb_adm2 <- comb_adm2 %>% dplyr::select(geometry) #getting rid of extraneous cols

st_write(comb_adm2, "Data/comb_adm2.shp", delete_dsn = TRUE)
adm2_mtools <- readShapeSpatial("Data/comb_adm2.shp")

win <- as(adm2_mtools, "owin") #transforming object type
plot(win)

#Using the pop distribution derived from the masked raster to distribute points in a 
#  windowed regions (a planar point pattern, PPP)
set.seed(1001)
adm2_ppp <- rpoint(adm2_pop_ttl, f = as.im(pop20_adm2), win = win)

plot(win, main = NULL) #checking out the ppp w/window
plot(adm2_ppp, cex = 0.03, add = TRUE) 

#Calculating the PDF for our PPP

#THIS NEXT LINE IS COMPUTATIONALLY EXPENSIVE --> do NOT run again!
# bandwidth <- bw.ppl(adm2_ppp)
# 
# save(bandwidth, file = "adm2_ppp_bandwidth.RData")
load("adm2_ppp_bandwidth.RData") #reading in the bandwidth value

adm2_density_img <- density.ppp(adm2_ppp, sigma = bandwidth) #the function describing the pop density across Mkhiweni

plot(adm2_density_img, main = NULL) #checking out the density img

#Adding in contour lines
SLDF <- as(adm2_density_img, "SpatialGridDataFrame") %>% 
  as.image.SpatialGridDataFrame() %>% 
  contourLines(levels = 1.2e6) %>% #the choice for contour level here is mostly by visual inspection
  ContourLines2SLDF(CRS("+proj=longlat +datum=WGS84 +no_defs")) #SLDF: Spatial Lines Data Frame

sf_multiline <- st_as_sf(SLDF) #converting back to an sf object

plot(adm2_density_img, main = NULL) #inspecting the contour level
plot(sf_multiline, add = TRUE)

#Extracting the polygons that enclose the de facto settlements
inside_polys <- st_polygonize(sf_multiline) #extracting all of the already valid (interior) polys
plot(st_geometry(inside_polys))

outside_lines <- st_difference(sf_multiline, inside_polys) #the polys that border on the int'l boundary
plot(st_geometry(outside_lines))

outside_polygons <- st_buffer(outside_lines, 0.0013) %>% #offsetting the outside lines
  st_difference(comb_adm2, .) %>% #creating artificial polys based on how the buffer intersects the border
  st_cast("POLYGON") #turning our object back into a polys
plot(st_geometry(outside_polygons)) 

# outside_polygons <- outside_polygons %>% #filtering out the largest poly, which shouldn't be included
#   mutate(area = st_area(outside_polygons) %>% as.numeric) %>%
#   filter(area < 2e8)
# 
# outside_polygons_pop <- raster::extract(pop20, outside_polygons, df = TRUE) #getting the pop vals for each "outside" poly
# 
# outside_polygons_pop_ttls <- outside_polygons_pop %>% #aggregating pop vals
#   dplyr::group_by(ID) %>%
#   summarize(pop20 = sum(swz_ppp_2020, na.rm = TRUE))
# 
# outside_polygons <- outside_polygons %>% #adding back in the pop vals
#   add_column(pop20 = outside_polygons_pop_ttls$pop20, ID = outside_polygons_pop_ttls$ID)
# 
# outside_polygons_filtered <- outside_polygons %>% 
#   filter((pop20 > 200 & !(ID %in% c(9, 4))) | ID == 5) #attempting to remove low pop polys (by inspection)
# 
# plot(mk_density_img, main = NULL) #making sure that we're doing ok w/our filtering
# plot(st_geometry(outside_polygons_filtered), border = "lightblue", add = TRUE) 

# inside_polys <- st_collection_extract(inside_polys, "POLYGON") #turning the inside polys into polys
# 
# inside_polys_pop <- raster::extract(pop20, inside_polys, df = TRUE) #extracting pop
# 
# inside_polys_pop_ttls <- inside_polys_pop %>% #aggregating to each poly
#   dplyr::group_by(ID) %>% 
#   summarize(pop20 = sum(swz_ppp_2020, na.rm = TRUE))
# 
# inside_polys <- inside_polys %>% #adding back to our object
#   add_column(pop20 = inside_polys_pop_ttls$pop20, ID = inside_polys_pop_ttls$ID) 
# 
# inside_polys_filtered <- inside_polys %>% filter(pop20 > 30 & ID != 1) #again, filtering out by inspection
# 
# png("Figures/mk_filter_polys.png", width = 700, height = 700) #showing the resultant polygons on the density plot
# plot(mk_density_img, main = NULL)
# plot(st_geometry(inside_polys_filtered), border = "lightblue", add = TRUE) 
# plot(st_geometry(outside_polygons_filtered), border = "lightgreen", add = TRUE) 
# dev.off()

#Putting all of the urban area polygons together
urban_areas <- st_union(outside_polygons, inside_polys) %>% #putting the inside + outside together
  st_cast("POLYGON") %>% 
  dplyr::select(geometry)
plot(pop20_adm2) #comparing to our original raster
plot(st_geometry(urban_areas), add = TRUE)

beginCluster(detectCores() - 1)
urban_areas_pop <- raster::extract(pop20, urban_areas, df = TRUE) #extracting pop
endCluster()

urban_areas_pop_ttls <- urban_areas_pop %>% #aggregating to each urban poly
  dplyr::group_by(ID) %>% 
  summarize(pop20 = sum(swz_ppp_2020, na.rm = TRUE))

urban_areas <- urban_areas %>% #adding aggregated pop back to the sf object
  add_column(pop20 = urban_areas_pop_ttls$pop20) %>% 
  unique()

urban_areas <- urban_areas %>% #adding in area and density to the urban areas
  mutate(area = st_area(urban_areas) %>% set_units("km^2"),
         density = (pop20 / area) %>% as.numeric())

urban_areas_exp <- urban_areas %>% filter(density > 100, #filtering down to true urban areas
                                          pop20 > 50, 
                                          !(floor(density) == 107)) #removing a specific poly

save(urban_areas_exp, comb_adm2, file = "adm2_urban_areas.RData")

#Some visualization w/ggplot
ua_centers <- urban_areas %>% #pulling out the centroids for each poly... may not use
  st_centroid() %>% 
  st_cast("MULTIPOINT")

ggplot() +
  theme_bw() +
  geom_sf(data = comb_adm2, fill = "grey80", color = "black", alpha = 0.7) +
  geom_sf(data = urban_areas_exp, mapping = aes(fill = log(pop20))) +
  geom_text_repel(data = urban_areas_exp, mapping = aes(label = round(density, 2), geometry = geometry), 
                  stat = "sf_coordinates", size = 3, min.segment.length = 0, fontface = "bold") + #adding repelled text for density
  labs(y = "Longitude", x = "Latitude", fill = "Log of Population") +
  scale_fill_gradient(low = "azure", high = "maroon") +
  theme(legend.position = "top")


