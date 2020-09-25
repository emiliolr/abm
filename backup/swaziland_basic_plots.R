library(sf)
library(tidyverse)
library(ggpubr)

setwd("~/Desktop/DATA\ 440/ABM_Summer_Stuff/section_2.1/data/gadm36_SWZ_shp")

#bringing in the geometry
swz_int <- read_sf("gadm36_SWZ_0.shp") #basic int'l boundary
swz_adm1 <- read_sf("gadm36_SWZ_1.shp") #first level adm boundaries
swz_adm2 <- read_sf("gadm36_SWZ_2.shp") #second level adm boundaries

#plotting the shp data
p_intl_adm1 <- ggplot() +
  theme_bw() +
  geom_sf(data = swz_adm1, fill = "wheat2", color = "grey65") + #adm1 boundaries
  geom_sf(data = swz_int, color = "black", size = 1.1, alpha = 0) + #int'l boundary
  geom_sf_label(data = swz_int, aes(label = NAME_0), size = 5.5, nudge_y = -0.17, 
               fontface = "bold") + #country name
  geom_sf_text(data = swz_adm1, aes(label = NAME_1), size = 4, color = "grey30") + #adm1 names
  scale_x_continuous(limits = c(30.5, 32.5))
p_intl_adm1

#for a plot like this, it might be better to just paste the code into RMD... maybe not though!
p_adm1_adm2 <- ggplot() +
  theme_bw() +
  geom_sf(data = swz_adm2, fill = "wheat2", color = "grey65", size = 0.3) + #adm1 boundaries
  geom_sf(data = swz_adm1, color = "grey45", alpha = 0, size = 0.5) + #adm2 boundary
  geom_sf(data = swz_int, color = "black", size = 1, alpha = 0) + #adm2 boundary
  geom_sf_text(data = swz_adm2, aes(label = NAME_2), size = 1, color = "grey50") + #county name
  geom_sf_text(data = swz_adm1, aes(label = NAME_1), size = 4, color = "grey15") + #adm1 names
  scale_x_continuous(limits = c(30.5, 32.5))
p_adm1_adm2

#CHALLENGE 2: most populous county of swz
just_manzini_adm1 <- swz_adm1 %>% filter(NAME_1 == "Manzini")
just_manzini_adm2 <- swz_adm2 %>% filter(NAME_1 == "Manzini")

p_just_manzini <- ggplot() +
  theme_bw() +
  geom_sf(data = just_manzini_adm2, fill = "wheat2", color = "grey45", size = 0.2) +
  geom_sf(data = just_manzini_adm1, alpha = 0) +
  geom_sf_text(data = just_manzini_adm2, aes(label = NAME_2), size = 1.5) +
  labs(title = "Manzini", subtitle = "Swaziland's most populous region") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"), plot.subtitle = element_text(hjust = 0.5)) 
p_just_manzini

#CHALLENGE 3: arranging multiple plots
some_adm2 <- swz_adm2 %>% filter(NAME_2 == "Lubuli" | NAME_2 == "Nkilongo") #grabbing one more plot for our arrangement

p_some_adm2 <- ggplot(some_adm2) +
  theme_bw() +
  geom_sf(fill = "wheat2") +
  geom_sf_text(aes(label = NAME_2)) +
  scale_x_continuous(limits = c(31.5, 32.23))
p_some_adm2

#changing some things so that they'll fit together nicely
p_adm1_adm2_arrange <- p_adm1_adm2 +
  geom_rect(data = swz_int, xmin = 30.8, xmax = 31.8, ymin = -26.2, ymax = -26.8, fill = NA, color = "maroon") +
  geom_rect(data = swz_int, xmin = 31.65, xmax = 32.05, ymin = -26.7, ymax = -27.15, fill = NA, color = "olivedrab4") +
  labs(x = "Longitude", y = "Latitude", title = "Swaziland", subtitle = "Details A & B") +
  theme(panel.background = element_rect(fill = "azure"), panel.grid = element_line(color = "grey85"),
        plot.title = element_text(face = "bold"))

p_some_adm2_arrange <- p_some_adm2 +
  labs(title = "Detail B: Some Tinkhundla", subtitle = "These are some tinkhundla that I chose", x = NULL, y = NULL) +
  theme(axis.text = element_blank(), panel.background = element_rect(fill = "azure"), 
        panel.grid = element_blank(), axis.ticks = element_blank(), 
        plot.title = element_text(hjust = 0.5, face = "bold", size = 10), plot.subtitle = element_text(hjust = 0.5, size = 8), 
        panel.border = element_rect(color = "olivedrab4", size = 1.2))

p_just_manzini_arrange <- p_just_manzini +
  labs(title = "Detail A: Manzini", x = NULL, y = NULL) +
  theme(axis.text = element_blank(), panel.background = element_rect(fill = "azure"), 
        panel.grid = element_blank(), axis.ticks = element_blank(), 
        panel.border = element_rect(color = "maroon", size = 1.2), plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 10))
  
#putting them all together
p_arranged <- ggplot() +
  coord_equal(xlim = c(0, 6), ylim = c(0, 4), expand = FALSE) +
  annotation_custom(ggplotGrob(p_adm1_adm2_arrange), xmin = 0, xmax = 4, ymin = 0, ymax = 4) +
  annotation_custom(ggplotGrob(p_some_adm2_arrange), xmin = 4, xmax = 6, ymin = 0, ymax = 2) +
  annotation_custom(ggplotGrob(p_just_manzini_arrange), xmin = 4, xmax = 6, ymin = 2, ymax = 4) +
  theme_void()
p_arranged



