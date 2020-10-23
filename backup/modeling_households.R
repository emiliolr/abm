#Write up associated w/survey: https://dhsprogram.com/pubs/pdf/FR202/FR202.pdf
#Methodology for stat methods: https://dhsprogram.com/pubs/pdf/SAR11/SAR11.pdf

library(tidyverse)
library(haven)
library(raster)
library(sf)

library(spatstat)
library(maptools)

library(survey)

library(missForest)
library(doParallel)

library(caret)
library(nnet)

setwd("~/Desktop/DATA\ 440/Deliverables")

data_fp <- "Data/swz_DHS_data/"

#Reading in DHS household data
#  to see the Stata variable label, use 'attr(DATAFRAME$VARNAME, "label")'
dhs_household <- read_dta(str_c(data_fp, "household/szhr51fl.dta")) 

#A fun exploratory visualization: empirical dist of num of children (5 and under) in household
dhs_household %>% 
  count(hv014, sort = TRUE) %>% 
  ggplot() + 
  geom_linerange(aes(x = hv014, ymin = 0, ymax = n)) + 
  geom_point(aes(x = hv014, y = n), size = 2) + 
  theme_bw()

#Important: you can change the Stata variable coding (labels) back to a factor w/'haven::as_factor()'
dhs_household %>% 
  count(hv228) %>% 
  mutate(hv228 = as_factor(hv228)) %>% #key step!
  ggplot() + 
  theme_bw() +
  geom_col(aes(x = hv228, y = n, fill = hv228), width = 0.7, show.legend = FALSE)

#Reading in other data + setup
swz_pop_2006 <- raster("Data/swz_ppp_2006.tif") #at 100m resolution...

dhs_spatial <- read_sf("Data/swz_DHS_spatial/SZGE53FL.shp") #reading in the DHS shape file
swz_adm1 <- read_sf("Data/gadm36_SWZ_shp/gadm36_SWZ_1.shp")

#Looking at the dist of clusters
dhs_spatial %>% 
  filter(LATNUM != 0 & LONGNUM != 0) %>%
  mutate(URBAN_RURA = fct_recode(URBAN_RURA, urban = "U", rural = "R")) %>%
  ggplot() +
  theme_bw() +
  geom_sf(mapping = aes(color = URBAN_RURA), size = 0.2) +
  geom_sf(data = swz_adm2, fill = NA) +
  labs(color = "Location")

#Pulling out just our vars of interest from the household survey data --> for the log reg
dhs_desired_vars <- dhs_household %>% 
  dplyr::select(hhid, hv005, hv009, hv024, hv104_01:hv104_34, hv105_01:hv105_34, hv106_01:hv106_34) %>% 
  mutate(hv005 = hv005 / 10e5)
save(dhs_desired_vars, file = "DHS_desired_vars.RData")

#Distributing points across swz - full population
pop_2006 <- swz_pop_2006 %>% cellStats("sum")

num_houses <- (pop_2006 / swz_avg_pph) %>% floor()

swz_mt <- readShapeSpatial("Data/gadm36_SWZ_shp/gadm36_SWZ_1.shp")
win <- as(swz_mt, "owin")

swz_houses <- rpoint(num_houses, f = as.im(swz_pop_2006), win = win) #actually distributing the houses

png("Figures/houses_rpoint.png", width = 1500, height = 1500)
plot(win, main = NULL)
plot(swz_houses, cex = 0.1, add = TRUE)
dev.off()

#Distributing by region and putting back together with DHS household data
houses_by_region <- dhs_household %>% #grabbing the num of households in each region
  dplyr::select(hv024) %>%
  mutate(hv024 = as_factor(hv024)) %>% 
  count(hv024) %>% 
  deframe() #turning into a named vector

hhohho_region <- crop(swz_pop_2006, filter(swz_adm1, NAME_1 == "Hhohho")) %>% #grabbing each region
  mask(filter(swz_adm1, NAME_1 == "Hhohho"))
lubombo_region <- crop(swz_pop_2006, filter(swz_adm1, NAME_1 == "Lubombo")) %>% 
  mask(filter(swz_adm1, NAME_1 == "Lubombo"))
manzini_region <- crop(swz_pop_2006, filter(swz_adm1, NAME_1 == "Manzini")) %>% 
  mask(filter(swz_adm1, NAME_1 == "Manzini"))
shiselweni_region <- crop(swz_pop_2006, filter(swz_adm1, NAME_1 == "Shiselweni")) %>% 
  mask(filter(swz_adm1, NAME_1 == "Shiselweni"))

st_write(filter(swz_adm1, NAME_1 == "Hhohho"), "Data/rpoints/hh.shp", delete_dsn = TRUE)
st_write(filter(swz_adm1, NAME_1 == "Lubombo"), "Data/rpoints/lu.shp", delete_dsn = TRUE)
st_write(filter(swz_adm1, NAME_1 == "Manzini"), "Data/rpoints/mz.shp", delete_dsn = TRUE)
st_write(filter(swz_adm1, NAME_1 == "Shiselweni"), "Data/rpoints/sh.shp", delete_dsn = TRUE)

hh_mt <- readShapeSpatial("Data/rpoints/hh.shp")
lu_mt <- readShapeSpatial("Data/rpoints/lu.shp")
mz_mt <- readShapeSpatial("Data/rpoints/mz.shp")
sh_mt <- readShapeSpatial("Data/rpoints/sh.shp")

hh_win <- as(hh_mt, "owin")
lu_win <- as(lu_mt, "owin")
mz_win <- as(mz_mt, "owin")
sh_win <- as(sh_mt, "owin")

hh_houses <- rpoint(houses_by_region["hhohho"] %>% as.numeric(), f = as.im(hhohho_region), win = hh_win) 
lu_houses <- rpoint(houses_by_region["lubombo"] %>% as.numeric(), f = as.im(lubombo_region), win = lu_win)
mz_houses <- rpoint(houses_by_region["manzini"] %>% as.numeric(), f = as.im(manzini_region), win = mz_win)
sh_houses <- rpoint(houses_by_region["shiselweni"] %>% as.numeric(), f = as.im(shiselweni_region), win = sh_win)

all_houses <- rbind.data.frame(as.data.frame(hh_houses), #order is Hhohho, Manzini, Shiselweni, Lubombo
                               as.data.frame(mz_houses),
                               as.data.frame(sh_houses),
                               as.data.frame(lu_houses))
save(hh_houses, lu_houses, mz_houses, sh_houses, file = "Data/swz_rpoint_by_region.RData")

dhs_desired_vars <- dhs_desired_vars %>% #sticking the houses onto the original data frame
  arrange(hv024) %>% #ensures the household data is stacked in the right order
  bind_cols(all_houses)

houses_sf <- st_as_sf(dhs_desired_vars, coords = c("x", "y"), crs = 4326) #converting to sf object
save(houses_sf, file = "Data/DHS_simple_location.RData") #saving the data

ggplot() + #checking out the results of household dist
  theme_bw() +
  geom_sf(data = swz_adm1, color = "black") +
  geom_sf(data = houses_sf, mapping = aes(color = as_factor(hv024)), size = 0.2) + #add color to points to check
  scale_color_brewer(palette = "BrBG") +
  labs(color = "Region")

#WEIGHTS: weights are meant to be divided by 10e6 and then used as Stata importance weights... 
#  -it's possible that "survey::svydesign()" can handle this operation
DHS_design <- svydesign(id = dhs_household$hv001, strata = dhs_household$hv022, #making a survey design, which applies weights
                        weights = dhs_household$hv005 / 10e6, data = dhs_household)

svymean(~hv014, DHS_design) #comparing the weighted vs unweighted result
mean(dhs_household$hv014)

#Making household shells
swz_avg_pph <- 4.6 #found in the DHS survey report
num_households <- floor(cellStats(swz_pop_2006, "sum") / swz_avg_pph)

#  generating pts for entire pop
swz_mt <- readShapeSpatial("Data/gadm36_SWZ_shp/gadm36_SWZ_0.shp")
win <- as(swz_mt, "owin")

swz_adm0_houses <- rpoint(num_households, f = as.im(swz_pop_2006), win = win) #actually distributing the houses

plot(win, main = NULL)
plot(swz_adm0_houses, cex = 0.01, add = TRUE)

#  turning into an sf object
hhs_locs <- as.data.frame(swz_adm0_houses) %>% st_as_sf(coords = c("x", "y"), crs = st_crs(swz_adm1)) 

#  sample w/replacement from the survey to create households for all of Eswatini
households_population <- slice_sample(dhs_desired_vars, n = num_households, replace = TRUE)

#  checking error that arises due to not integrating the weights here
sum(households_population$hv005) #sum of weights... ideally, should equal nrow()
nrow(households_population) #diff between the two is pretty small --> low error

#  putting locations together w/households
swz_hhs_locs <- bind_cols(households_population, hhs_locs)
swz_hhs_locs_not_sf <- bind_cols(dplyr::select(households_population, -x, -y), as.data.frame(swz_adm0_houses))

#Pivoting out so that obs are individuals (gender, age, education)
sex_pivot <- swz_hhs_locs_not_sf %>% 
  dplyr::select(hhid, hv005, hv009, hv024, x, y, starts_with("hv104_")) %>% #selecting out only the necessary vars
  pivot_longer(starts_with("hv104_"), names_to = "household_member", values_to = "sex") %>% #pivoting to long format... individuals for each obs
  mutate(household_member = household_member %>% str_replace("hv104_", "") %>% as.numeric(), #making member simply a num
         hhid = hhid %>% str_trim()) #trimming trailing/leading whitespace
  
age_pivot <- swz_hhs_locs %>% #same process for age
  dplyr::select(hhid, hv005, hv009, hv024, x, y, starts_with("hv105_")) %>%
  pivot_longer(starts_with("hv105_"), names_to = "household_member", values_to = "age") %>%
  mutate(household_member = household_member %>% str_replace("hv105_", "") %>% as.numeric(),
         hhid = hhid %>% str_trim())

edu_pivot <- swz_hhs_locs %>% #same process for education
  dplyr::select(hhid, hv005, hv009, hv024, x, y, starts_with("hv106_")) %>%
  pivot_longer(starts_with("hv106_"), names_to = "household_member", values_to = "educ_lvl") %>%
  mutate(household_member = household_member %>% str_replace("hv106_", "") %>% as.numeric(),
         hhid = hhid %>% str_trim())

#  joining sex, age, and educ to create shell individual pop
ind_shell_data <- sex_pivot %>% #seems to have worked... all "hhid" and "household_number" match!
  select(-hv024) %>% #taking out the region identifier
  bind_cols(dplyr::select(age_pivot, age), dplyr::select(edu_pivot, educ_lvl)) %>%
  filter(!(is.na(sex) & is.na(age) & is.na(educ_lvl))) %>% #removing out the people that don't exist
  st_as_sf(coords = c("x", "y"), crs = st_crs(swz_adm1)) #mending the spatial data
  
#  checking error
sum(ind_shell_data$hv005) #what nrow() should ideally be
cellStats(swz_pop_2006, "sum") #what pop should actually be
nrow(ind_shell_data) #what we actually have... we've ignored weights

(((nrow(ind_shell_data) - cellStats(swz_pop_2006, "sum")) / cellStats(swz_pop_2006, "sum")) * 100) %>% abs() #percent diff from actual pop
(((nrow(ind_shell_data) - sum(ind_shell_data$hv005)) / sum(ind_shell_data$hv005)) * 100) %>% abs() #percent diff from weights

#Data imputation
#  imputing w/missForest
registerDoParallel(cores = 3)
shell_data_imputed <- ind_shell_data %>% #imputing missing data w/random forest method
  dplyr::select(sex, age, educ_lvl) %>%
  st_set_geometry(NULL) %>%
  as.matrix() %>%
  missForest(maxiter = 5, ntree = 20, parallelize = "forests") #making sure to parallelize the process and reduce the forest size

#  adding back to the original df
imputed_vars <- shell_data_imputed$ximp %>% as.data.frame()

ind_shell_data_final <- ind_shell_data %>% #binding back onto original
  dplyr::select(-c(age, sex, educ_lvl)) %>% 
  bind_cols(imputed_vars) %>% 
  mutate(educ_lvl = round(educ_lvl), age = round(age))
st_geometry(ind_shell_data_final) <- ind_shell_data_final$geometry
save(ind_shell_data_final, file = "swz_shell_data.RData") #saving the data

#Basic modeling w/multinomial log reg
#  train/test split
index <- createDataPartition(ind_shell_data_final$educ_lvl, p = 0.7, list = FALSE) %>% as.numeric()

train <- ind_shell_data_final[index, ] 
test <- ind_shell_data_final[-index, ]

#  training the multinom log reg
# multinomial_model <- multinom(educ_lvl ~ household_member + hv009 + sex + age, data = train)
# save(multinomial_model, file = "multinomial_model.RData")
load("multinomial_model.RData") #above takes a long time, loading is faster

#  making preds on test data set
test$educ_lvl.pred <- predict(multinomial_model, newdata = test, "class")

#  accuracy of the model
sum(test$educ_lvl.pred == test$educ_lvl) / nrow(test)

#Producing some quick plots for the write up
#  a uniform dist of houses - "st_sample()"?
swz_uniform <- st_sample(swz_adm1, 10e4)

png("Figures/houses_uniform_dist.png", width = 800, height = 800)
plot(swz_uniform, cex = 0.008)
plot(st_geometry(swz_adm1), add = TRUE)
dev.off()

#  our dist of houses
png("Figures/shell_data_dist.png", width = 800, height = 800)
plot(st_geometry(ind_shell_data_final), cex = 0.008)
plot(st_geometry(swz_adm1), add = TRUE)
dev.off()

