library(sf)
library(tidyverse)
library(units)
library(scales)
library(ggpubr)

setwd("~/Desktop/DATA\ 440/Deliverables")

#bringing in the sf data w/pop data attached
load("population_with_sf.Rdata")

#adding in a area and density variables
swz_adm1 <- swz_adm1 %>% 
  mutate(area = st_area(swz_adm1) %>% set_units("km^2"),
         density = pop20 / area)

#creating a bar plot to compare pop and pop props
plot_pop_bar <- swz_adm1 %>% 
  mutate(NAME_1 = fct_reorder(NAME_1, pop20)) %>% 
  ggplot(mapping = aes(x = NAME_1, y = pop20, fill = pop20)) +
  theme_bw() +
  geom_bar(stat = "identity", color = "black", width = 0.65, show.legend = FALSE) +
  geom_text(mapping = aes(label = percent(pop20 / sum(pop20))), position = position_stack(vjust = 0.5), fontface = "bold") + 
  labs(y = "Population (2020)", x = NULL) +
  scale_y_continuous(labels = comma, expand = expansion(mult = c(0.02, 0.05))) +
  scale_fill_gradient(low = "azure", high = "maroon") +
  coord_flip() +
  theme(panel.grid.minor = element_blank())
plot_pop_bar

#bringing back in the sf plot
plot_adm1_pop <- ggplot(swz_adm1) +
  theme_bw() +
  geom_sf(mapping = aes(fill = pop20), show.legend = FALSE) +
  geom_sf_text(mapping = aes(label = NAME_1), fontface = "bold") +
  geom_sf_text(mapping = aes(label = round(density, 2)), nudge_y = -0.075, size = 3) +
  scale_fill_gradient(low = "azure", high = "maroon") + 
  scale_x_continuous(breaks = seq(30.8, 32.2, by = 0.3)) +
  labs(title = "", fill = "Population (2020)", x = "Latitude", y = "Longitude")
plot_adm1_pop

#arranging the plots
ggarrange(plot_adm1_pop, plot_pop_bar, nrow = 1, widths = c(2.3, 2))

#CHALLENGE 1: making a nice pop breakdown that includes adm1 and adm2
swz_adm2 %>% 
  mutate(NAME_2 = fct_reorder(NAME_2, pop20)) %>%
  ggplot(mapping = aes(x = NAME_2, y = pop20, color = NAME_1)) +
  theme_bw() +
  geom_point(show.legend = FALSE, size = 2) +
  facet_wrap(~NAME_1, scales = "free_y", ncol = 2) +
  coord_flip() +
  labs(y = "Population (2020)", x = NULL) +
  theme(strip.text = element_text(face  = "bold"), panel.grid.minor = element_blank(), aspect.ratio = 1)
  

  
  


