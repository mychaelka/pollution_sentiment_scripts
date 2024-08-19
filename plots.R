library(tidyverse)
library(sf)
library(sp)
library(ggspatial)
library(rlang)
library(viridis)
library(stars)
library(gridExtra)
library(ggpubr)

# US shapefiles
usa_counties <- st_read("../data/USA_shapefiles/counties/USA_Counties.shp")
usa_states <- st_read("../data/USA_shapefiles/States_shapefile/")
usa_full <- st_read("../data/USA_shapefiles/gadm41_USA_shp/gadm41_USA_0.shp")

# remove Alaska and Hawaii
usa_states <- usa_states[-c(2,12),] %>% 
  select(State_Code, State_Name)

# ECMWF data
ecmwf <- read_stars("../data/ECMWF/july2015_new.nc")
pm25 <- st_as_sf(ecmwf[6]) %>% parse_nums() %>% 
  mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, ~.*1000000000)) %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`) %>% 
  dplyr::mutate(
    id = row_number()
  ) %>% 
  st_set_crs(st_crs(usa_states))
aod550 <- st_as_sf(ecmwf[8]) %>% parse_nums() %>%
  dplyr::select(`2015-07-05`:`2015-07-07 21:00:00`) %>%
  dplyr::mutate(
    id = row_number()
  ) %>% 
  st_set_crs(st_crs(usa_states))


# intersection of usa shapefile and pollution grid data
sf_use_s2(FALSE)
pm25_df <- st_intersection(pm25, usa_states) %>% 
  st_transform(5070) # USA Contiguous Albers Equal Area Conic projection
pm25_df <- st_intersection(pm25, usa_counties) 
aod550_df <- st_intersection(aod550, usa_states) %>% 
  st_transform(5070)

# select days for ggplots
cols <- names(df)[c(23,27,31,35)]

for (col in cols) {
  g <- df %>% ggplot(aes(fill=.data[[col]])) +
    geom_sf(lwd=0.01, col = "black") + 
    # geom_sf(data = ref_points, color = "red", size = 0.05) # in case I need reference points for cities
    annotation_scale(location = 'bl') +
    coord_sf(datum = NA) +
    scale_fill_viridis_c(
      option="magma",
      direction = -1,
      limits = c(0, 120),
      breaks = seq(from=0, to=120, by = 30),
      na.value = "black",
      guide = guide_colourbar(order = 1,
                              title = "Pm2.5 concentration (ug/m3)",
                              title.position = 'top')
    )  +
    #labs(title = substr(col, 2, 20)) +
    theme_void(base_family = "Times") +
    theme( 
      legend.position = c(0.25, 0.07),
      legend.direction = "horizontal",
      legend.justification = c("right","bottom"),
      legend.box = "horizontal",
      legend.box.just = "bottom",
      legend.title = element_text(size = 16),
     legend.key.size = unit(1.2, 'cm')
    )
  
  ggsave(filename = str_c(substr(col, 2, 20), ".png"), width = 11, 
         height = 7, device='png', dpi=700, bg = "white")
}

# calculate average US air pollution levels in July
july2015avg <- pm25_df %>% 
  rowwise() %>% mutate(avg=mean(`2015-07-01`:`2015-07-31 21:00:00`))


##################### PM2.5 and Aerosol Optical Depth plots ####################
pm25_df %>% ggplot(aes(fill=`X2015.07.06.12.00.00`)) +
  geom_sf(lwd=0.01) + 
  theme_bw() +
  annotation_scale(location = 'tr') +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="magma",
    direction = -1,
    limits = c(0, 120),
    breaks = seq(from=0, to=120, by = 30),
    na.value = "black",
    guide = guide_colourbar(order = 1,
                            title = "Pm2.5 concentration (ug/m3)",
                            title.position = 'top')
  ) +
  labs(title = substr("2015.07.01", 2, 20)) +
  theme_void(base_family = "Times") +
  theme( 
    legend.position = c(0.99, 0.75),
    legend.direction = "horizontal",
    legend.justification = c("right","bottom"),
    legend.box = "horizontal",
    legend.box.just = "bottom",
    legend.title = element_text(size = 13),
    legend.key.size = unit(0.7, 'cm')
  )

aod550_df %>% ggplot(aes(fill=`X2015.07.06.12.00.00`)) +
  geom_sf(lwd=0.01) + 
  theme_bw() +
  annotation_scale(location = 'tr') +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    option="magma",
    direction = -1,
    na.value = "black",
    guide = guide_colourbar(order = 1,
                            title = "Aerosol Optical Depth",
                            title.position = 'top')
  ) +
  labs(title = substr("2015.07.01", 2, 20)) +
  theme_void(base_family = "Times") +
  theme( 
    legend.position = c(0.99, 0.75),
    legend.direction = "horizontal",
    legend.justification = c("right","bottom"),
    legend.box = "horizontal",
    legend.box.just = "bottom",
    legend.title = element_text(size = 13),
    legend.key.size = unit(0.7, 'cm')
  )

################################################################################

###########
#four plots of the cloud
cols <- names(df)[c(23,27,31,35)]
myplots <- vector('list', 4)
i = 1

for (col in cols) {
  myplots[[i]] <- df %>% ggplot(aes(fill=.data[[col]])) +
    geom_sf(lwd=0.01, col = "black") + 
    annotation_scale(location = 'bl') +
    coord_sf(datum = NA) +
    scale_fill_viridis_c(
      option="magma",
      direction = -1,
      limits = c(0, 120),
      breaks = seq(from=0, to=120, by = 30),
      na.value = "black",
      guide = guide_colourbar(order = 1,
                              title = "Pm2.5 concentration (ug/m3)",
                              title.position = 'top')
    )  +
    theme_void(base_family = "Times") +
    theme( 
      legend.position = "none", #c(0.25, 0.07),
      legend.direction = "horizontal",
      legend.justification = c("bottom"),
      legend.box = "horizontal",
      legend.box.just = "bottom",
      legend.title = element_text(size = 16),
      legend.key.size = unit(1.2, 'cm')
    ) +
    labs(title = str_c(substr(col, 2, 12), " ", substr(col, 13, 17))) 
  i = i + 1
}

ggarrange(myplots[[1]], myplots[[2]], myplots[[3]], myplots[[4]], 
          common.legend = TRUE, legend = "bottom")

ggsave(filename = "july2015cloud.png", width = 11, 
       height = 7, device='png', dpi=700, bg = "white")
