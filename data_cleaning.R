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
usa_states <- st_read("../data/USA_shapefiles/gadm41_USA_shp/gadm41_USA_1.shp")  
usa_full <- st_read("../data/USA_shapefiles/gadm41_USA_shp/gadm41_USA_0.shp")

# remove Alaska from states
usa_states <- usa_states %>% filter(ISO_1 != "US-AK" & ISO_1 != "US-HI")

# ECMWF data
ecmwf <- read_stars("../data/ECMWF/july2015.nc")
# ecmwf <- read_stars("data/ECMWF/august2015.nc")
pm25 <- st_as_sf(ecmwf[5])

# whole month
pm25july <- pm25 %>%
  # parse numeric values from character vectors
  mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, as.character)) %>% 
  mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, parse_number)) %>% 
  # change units to ppm from kg/m^3
  # 1 Kilogram Per Cubic Meter = 1 000 000 000 ug per cubic meter
  mutate(across(`2015-07-01`:`2015-07-31 21:00:00`, ~.*1000000000)) %>% 
  st_set_crs(st_crs(usa_full))

# key days are 5.7.-7.7.2015
pollution <- pm25july %>%
  dplyr::select(`2015-07-03`:`2015-07-09 21:00:00`)

# intersection of usa shapefile and pollution grid data -- takes a long time
sf_use_s2(FALSE)
# df <- st_intersection(pollution, usa_full)
df <- st_intersection(pollution, usa_states) %>% 
  st_transform(5070) # USA Contiguous Albers Equal Area Conic projection

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

# cloud of smoke -- calculate maximum pollution for each pixel over July
maxpollution <- df 
maxpollution$max <- apply(df[1:120] %>% st_drop_geometry(), 1, max)

# fig with maximum pollution over 6-7 july  
maxpollution %>% ggplot(aes(fill=max)) +
  geom_sf(lwd=0.01) + 
  theme_bw() +
  annotation_scale(location = 'tr') +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    limits = c(0, 120),
    breaks = seq(from=0, to=120, by = 30),
    na.value = "orange",
    guide = guide_colourbar(order = 1,
                            title = "log PM2.5 concentration (ug/m3)",
                            title.position = 'top')
  ) +
  labs(title = "Maximum air pollution over July 2015") +
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

ggsave(filename = "july2015max_log.png", width = 7, 
       height = 5, device='png', dpi=700, bg = "white")



# calculate average US air pollution levels in July
july2015avg <- pm25july %>% 
  rowwise() %>% mutate(avg=mean(`2015-07-01`:`2015-07-31 21:00:00`)) %>% 
  select(avg) %>% st_intersection(usa_states) %>% 
  st_transform(5070)

# average pollution fig (maybe average rather over the whole summer?)
july2015avg %>% ggplot(aes(fill=avg)) +
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

ggsave(filename = "july2015avg.png", width = 11, 
       height = 7, device='png', dpi=700, bg = "white")

# whole image (not cropped to USA)
pollution %>% ggplot(aes(fill=`2015-07-01`)) +
  geom_sf(lwd=0.01) + 
  theme_bw() +
  annotation_scale(location = 'tr') +
  coord_sf(datum = NA) +
  scale_fill_viridis_c(
    limits = c(0, 120),
    breaks = seq(from=0, to=120, by = 30),
    na.value = "grey",
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
    )
  i = i + 1
}

ggarrange(myplots[[1]], myplots[[2]], myplots[[3]], myplots[[4]], 
          common.legend = TRUE, legend = "bottom")

ggsave(filename = "july2015cloud.png", width = 11, 
       height = 7, device='png', dpi=700, bg = "white")
