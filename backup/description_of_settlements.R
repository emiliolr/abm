library(tidyverse)
library(sf)
library(raster)
library(spatstat)
library(maptools)
library(ggrepel)
library(units)

#Setting things up...
setwd("~/Desktop/DATA\ 440/Deliverables")
path_to_data <- "~/Desktop/DATA\ 440/ABM_Summer_Stuff/section_2.1/data"

#Bringing in pop and sf data
swz_adm2 <- str_c(path_to_data, "/gadm36_SWZ_shp/gadm36_SWZ_2.shp") %>% read_sf()
# pop20 <- str_c(path_to_data, "/pop_data/swz_ppp_2020_1km_Aggregated.tif") %>% raster()
pop20 <- raster("Data/swz_ppp_2020.tif")

swz_mk <- swz_adm2 %>% filter(NAME_2 == "Mkhiweni") #subsetting to Mkhiweni
plot(st_geometry(swz_mk)) #taking a look

pop20_mk <- crop(pop20, swz_mk) #zooming on the bounding box
pop20_mk <- mask(pop20_mk, swz_mk) #cutting out the relevant gridcells

mk_pop_ttl <- cellStats(pop20_mk, "sum") #the estimated pop for Mkhiweni

png("Figures/mkhiweni_pop20.png", width = 800, height = 800) #saving the results thus far (using base R)
plot(pop20_mk, main = NULL) #looking at the pop and geometry together
plot(st_geometry(swz_mk), add = TRUE)
dev.off()

#Writing our sf object back to a .shp file and reading into maptools
st_write(swz_mk, "Data/swz_mk.shp", delete_dsn = TRUE)
mk_mtools <- readShapeSpatial("Data/swz_mk.shp")

win <- as(mk_mtools, "owin") #transforming object type
plot(win)

#Using the pop distribution derived from the masked raster to distribute points in a 
#  windowed regions (a planar point pattern, PPP)
set.seed(1001)
mk_ppp <- rpoint(mk_pop_ttl, f = as.im(pop20_mk), win = win)

png("Figures/mk_ppp.png", width = 700, height = 700) #saving the ppp plot
plot(win, main = NULL) 
plot(mk_ppp, cex = 0.25, add = TRUE) 
dev.off()

#Calculating the PDF for our PPP

#THIS NEXT LINE IS COMPUTATIONALLY EXPENSIVE --> do NOT run again!
# bandwidth <- bw.ppl(mk_ppp)

# save(bandwidth, file = "mk_ppp_bandwidth.RData")
load("mk_ppp_bandwidth.RData") #reading in the bandwidth calculation

mk_density_img <- density.ppp(mk_ppp, sigma = bandwidth) #the function describing the pop density across Mkhiweni

png("Figures/mk_density.png", width = 700, height = 700)
plot(mk_density_img, main = NULL)
dev.off()

#Adding in contour lines
SLDF <- as(mk_density_img, "SpatialGridDataFrame") %>% 
  as.image.SpatialGridDataFrame() %>% 
  contourLines(levels = 5.8e5) %>% #the choice for contour level here is mostly by visual inspection
  #worth noting: a higher val on the levels parameter would be nice, but doesn't work out later on
  ContourLines2SLDF(CRS("+proj=longlat +datum=WGS84 +no_defs")) #SLDF: Spatial Lines Data Frame

sf_multiline <- st_as_sf(SLDF) #converting back to an sf object

png("Figures/mk_density_w_contour.png", width = 700, height = 700)
plot(mk_density_img, main = NULL) 
plot(sf_multiline, add = TRUE)
dev.off()

#Extracting the polygons that enclose the de facto settlements
inside_polys <- st_polygonize(sf_multiline) #extracting all of the already valid (interior) polys
plot(st_geometry(inside_polys))

outside_lines <- st_difference(sf_multiline, inside_polys) #the polys that border on the int'l boundary
plot(st_geometry(outside_lines))

outside_polygons <- st_buffer(outside_lines, 0.001) %>% #offsetting the outside lines
  st_difference(swz_mk, .) %>% #creating artificial polys based on how the buffer intersects Mkhiweni's border
  st_cast("POLYGON") #turning our object back into a polys

png("Figures/mk_cut_up_polys.png", width = 700, height = 700)
plot(st_geometry(outside_polygons)) 
dev.off()

outside_polygons <- outside_polygons %>% #filtering out the largest poly, which shouldn't be included
  mutate(area = st_area(outside_polygons) %>% as.numeric) %>%
  filter(area < 2e8)

outside_polygons_pop <- raster::extract(pop20, outside_polygons, df = TRUE) #getting the pop vals for each "outside" poly

outside_polygons_pop_ttls <- outside_polygons_pop %>% #aggregating pop vals
  dplyr::group_by(ID) %>%
  summarize(pop20 = sum(swz_ppp_2020, na.rm = TRUE))

outside_polygons <- outside_polygons %>% #adding back in the pop vals
  add_column(pop20 = outside_polygons_pop_ttls$pop20, ID = outside_polygons_pop_ttls$ID)

outside_polygons_filtered <- outside_polygons %>% 
  filter((pop20 > 200 & !(ID %in% c(9, 4))) | ID == 5) #attempting to remove low pop polys (by inspection)
                                
plot(mk_density_img, main = NULL) #making sure that we're doing ok w/our filtering
plot(st_geometry(outside_polygons_filtered), border = "lightblue", add = TRUE) 

inside_polys <- st_collection_extract(inside_polys, "POLYGON") #turning the inside polys into polys

inside_polys_pop <- raster::extract(pop20, inside_polys, df = TRUE) #extracting pop

inside_polys_pop_ttls <- inside_polys_pop %>% #aggregating to each poly
  dplyr::group_by(ID) %>% 
  summarize(pop20 = sum(swz_ppp_2020, na.rm = TRUE))

inside_polys <- inside_polys %>% #adding back to our object
  add_column(pop20 = inside_polys_pop_ttls$pop20, ID = inside_polys_pop_ttls$ID) 

inside_polys_filtered <- inside_polys %>% filter(pop20 > 30 & ID != 1) #again, filtering out by inspection

png("Figures/mk_filter_polys.png", width = 700, height = 700) #showing the resultant polygons on the density plot
plot(mk_density_img, main = NULL)
plot(st_geometry(inside_polys_filtered), border = "lightblue", add = TRUE) 
plot(st_geometry(outside_polygons_filtered), border = "lightgreen", add = TRUE) 
dev.off()

#Putting all of the urban area polygons together
urban_areas <- st_union(outside_polygons_filtered, inside_polys_filtered) %>% #putting the inside + outside together
  st_cast("POLYGON") %>% 
  dplyr::select(geometry)
plot(pop20_mk) #comparing to our original raster
plot(st_geometry(urban_areas), add = TRUE)

urban_areas_pop <- raster::extract(pop20, urban_areas, df = TRUE) #extracting pop

urban_areas_pop_ttls <- urban_areas_pop %>% #aggregating to each urban poly
  dplyr::group_by(ID) %>% 
  summarize(pop20 = sum(swz_ppp_2020, na.rm = TRUE))

urban_areas <- urban_areas %>% 
  add_column(pop20 = urban_areas_pop_ttls$pop20) %>% 
  unique()

urban_areas <- urban_areas %>% #adding in area and density fo the urban areas
  mutate(area = st_area(urban_areas) %>% set_units("km^2"),
         density = (pop20 / area) %>% as.numeric())

save(urban_areas, swz_mk, file = "for_desc_of_settle.RData")

#Some visualization w/ggplot
ua_centers <- urban_areas %>% #pulling out the centroids for each poly... may not use
  st_centroid() %>% 
  st_cast("MULTIPOINT")

ggplot() +
  theme_bw() +
  geom_sf(data = swz_mk, fill = "grey80", color = "black", alpha = 0.7) +
  geom_sf(data = urban_areas, mapping = aes(fill = log(pop20))) +
  geom_text_repel(data = urban_areas, mapping = aes(label = round(density, 2), geometry = geometry), 
                  stat = "sf_coordinates", size = 3, min.segment.length = 0, fontface = "bold") + #adding repelled text for density
  labs(y = "Longitude", x = "Latitude", fill = "Log of Population") +
  scale_fill_gradient(low = "azure", high = "maroon") +
  theme(legend.position = "top")

#CHALLENGE: Zipf's law... count on the y-axis and rank on the x-axis
just_pops <- tibble(pop20 = urban_areas$pop20) #extracting the necessary data

just_pops <- just_pops %>% #manipulating the data to visualize Zipf's law
  arrange(desc(pop20)) %>% 
  add_column(rank = 1:nrow(just_pops))

ggplot(just_pops, mapping = aes(x = rank, y = pop20)) + #plotting the data... look's approximately Zipfian
  theme_bw() +
  geom_line(color = "maroon", size = 1.1) +
  geom_point(size = 2, color = "maroon") +
  scale_x_continuous(breaks = 1:nrow(just_pops)) +
  labs(x = "Rank", y = "Population") +
  theme(panel.grid.minor = element_blank())

