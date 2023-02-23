###########################################
#
# Robby Fonner - NMFS & Brittany King - OSU
# Visualizations of restoration project data and
# exploratory data
# 
#
########################################

'SAVED DATA'
dep_vars <- read_csv( here("Output/dep_vars.csv")) %>%
  mutate(HUC_12 = as.character(HUC_12))
eco_social_data <- read_csv(here("Output/eco_soc_data.csv")) %>%
  mutate(HUC_12 = as.character(HUC_12))

'DATA IMPORT'

#Data for ecological and social variable maps
eco_soc_maps <- read_sf(here("Data/NHDPlusPN/NHDPlus17/WBDSnapshot/WBD/WBD_Subwatershed.shp")) %>%
  filter( as.numeric(substr(HUC_12,1,4)) %in% study_area ) %>%
  st_transform(4326) %>%
  group_by(HUC_12, HU_12_NAME) %>%
  summarise(do_union = TRUE) %>%
  ungroup() %>%
  dplyr::select(HUC_12) %>%
  left_join(eco_social_data, by = "HUC_12")

## Get data in long format and add breaks for choropleth map of dep vars
#Breaks for project count categories
breaks_qt <- c(0,1,2,5,10,15,25,50,150)
#Get depvar data in long format
dep_vars_long <- dep_vars %>%
  mutate( all_act = n_worksites) %>%
  pivot_longer(
    cols = ends_with("act"),
    names_to = "restoration_type",
    values_to = "project_count"
  ) %>%
  mutate(
    restoration_type = case_when(
      restoration_type == "instream_act" ~ "Instream restoration",
      restoration_type == "riparian_act" ~ "Riparian restoration",
      restoration_type == "land_act" ~ "Land acquisition",
      restoration_type == "fishpass_act" ~ "Fish passage",
      restoration_type == "all_act" ~ "All action types"
    ),
    worksite_counts = cut(project_count, breaks_qt, right = F)
  ) %>%
  left_join(eco_social_data[c("HU_8_NAME", "HUC_12")], by = "HUC_12")

#Spatial data for dependent variable maps
dep_var_maps <- read_sf(here("Data/NHDPlusPN/NHDPlus17/WBDSnapshot/WBD/WBD_Subwatershed.shp")) %>%
  filter( as.numeric(substr(HUC_12,1,4)) %in% study_area ) %>%
  st_transform(4326) %>%
  group_by(HUC_12, HU_12_NAME) %>%
  summarise(do_union = TRUE) %>%
  ungroup() %>%
  dplyr::select(HUC_12) %>%
  left_join(dep_vars_long, by = "HUC_12")  %>%
  dplyr::select(HUC_12,restoration_type,worksite_counts,HU_8_NAME,geometry) %>%
  filter(!is.na(HU_8_NAME))


'CORRELATION TABLE'

#data to summarize
sum_data <- eco_social_data %>%
  mutate(area = area / 10000) %>% #convert from sq m to hectares
  filter(HU_8_NAME == "PUGET SOUND") %>%
  left_join(dep_vars, by = "HUC_12") %>%
  
  dplyr::select(instream = instream_act, riparian = riparian_act, land = land_act,  passage = fishpass_act, n_worksites,
                stream_length , max_elevation, mean_slope , stream_order , natural_cover, species_rich, 
                bull_trout, esu_count, impaired, impervious, pct_public, household_inc,  white_non_hisp,
                owner_occ , edu_degree , pop_density, area)

cor_table <- data.frame( cor(sum_data, method = "pearson", use = "complete.obs" ))

write_csv(cor_table, here("Output/corr_table.csv"))

'DESCRIPTIVE STATISTICS' # ---- need to make by action

sumtable(  sum_data,           
  summ=c(
  'mean(x)',
  'median(x)',
  'sd(x)',
  'min(x)',
  'max(x)',
  'notNA(x)'
  #,'propNA(x)'
),
summ.names= c('Mean', 'Median', 'Std.Dev.', 'Min', 'Max', 'N'),
digits = 3, #group.test = T,
out = 'csv',
file = here("Output/Dstats.csv")
)

'MAPS OF STUDY AREA AND RESTORATION ACTION DATA'


#Dep var maps in google crs
  DVM_3857 <- st_transform(dep_var_maps, 3857) %>%
    filter(HU_8_NAME == "PUGET SOUND")
  #Get google map
  map <- get_googlemap("Dabob, WA", zoom = 7, maptype = "hybrid", source = "google")
  
  #Work around to get ggmap and sf to play nice
  #https://stackoverflow.com/questions/47749078/how-to-put-a-geom-sf-produced-map-on-top-of-a-ggmap-produced-raster
  ggmap_bbox <- function(map) {
    if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
    # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
    # and set the names to what sf::st_bbox expects:
    map_bbox <- setNames(unlist(attr(map, "bb")), 
                         c("ymin", "xmin", "ymax", "xmax"))
    
    # Coonvert the bbox to an sf polygon, transform it to 3857, 
    # and convert back to a bbox (convoluted, but it works)
    bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = crs(dep_var_maps[dep_var_maps$HU_8_NAME == "PUGET SOUND",]))), 3857))
    
    # Overwrite the bbox of the ggmap object with the transformed coordinates 
    attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
    attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
    attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
    attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
    map
  }
  
  # Use the function
  map <- ggmap_bbox(map) 
  
#Color
colors <- c("HU_8_NAME" = "white")
##Study area map 
  ggmap(map) +
    coord_sf(crs = st_crs(3857)) +
    geom_sf(data = DVM_3857, fill = "yellow2", aes(color = "HU_8_NAME"), alpha = .05, size = .2, inherit.aes = FALSE) +
    labs(title = "", x="", y = "", fill="", color = "") + 
    scale_color_manual(values = colors) +
    theme(legend.position = "none")
  
  ggsave( "Study area subwatersheds.png", path = here("Output"), dpi = reso, width =12, height =12)

  

  
#Create maps, looping over study area basins
  
#Basins to loop over
basins <- c("PUGET SOUND", "WILLAMETTE", "YAKIMA")
basin_names <- c("Puget Sound", "Willamette", "Yakima")
basin_short <- c("PS", "WLMT","YKMA" )
  
  
  for(i in 1:length(basins)){

#bounding box
study_bbox <- eco_soc_maps %>%
  filter(HU_8_NAME == basins[i]) %>%
  st_bbox()

ggmap(get_stamenmap(bbox = as.numeric(study_bbox), zoom = 8, maptype = c("terrain"))) 
ggsave( paste0(basin_short[i],"_","study_area.png"), path = here("Output"), width =12, height =12, dpi = reso) 

#Map of worksite counts
ggplot(dep_var_maps[dep_var_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill=worksite_counts))  +
  scale_fill_brewer(palette = "YlGn", na.value = "grey50") +
  labs(title = paste0("Number of salmon habitat restoration worksites in ",
                      basin_names[i],
                      " \n subwatersheds from 2000 to 2015 by restoration action type")
       ) +
  facet_wrap(~ restoration_type)

ggsave( paste0(basin_short[i],"_","rest_proj.png"), path = here("Output"), width =12, height =11, dpi = reso) 

####Social variable maps

#Population density
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = pop_density)) +
  scale_fill_distiller(name = "Residents\nper sq km", palette = "YlOrRd", direction = 1) +
  labs(title = "Population density")  +
  theme(plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","pop_dens.png"), path = here("Output"), width =12, height =11, dpi = reso) 

#HH Income
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = household_inc)) +
  scale_fill_distiller(name = "2010 USD", palette = "YlOrRd", direction = 1) +
  labs(title = "Houshold income")  +
  theme(plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","hh_inc.png"), path = here("Output"), width =12, height =11, dpi = reso) 

#RACE WA
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = white_non_hisp)) +
  scale_fill_distiller(name = "Share of\npopulation", palette = "YlOrRd", direction = 1) +
  labs(title = "White and non-hispanic (i.e. white alone)")  +
  theme(plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","race_wa.png"), path = here("Output"), width =12, height =11, dpi = reso) 

#EDU_Degree
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = edu_degree)) +
  scale_fill_distiller(name = "Share of\npopulation", palette = "YlOrRd", direction = 1) +
  labs(title = "College degree")  +
  theme(plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","edu_deg.png"), path = here("Output"), width =12, height =11, dpi = reso) 

#OCC_UNIT_OWN
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = owner_occ)) +
  scale_fill_distiller(name = "Share of\npopulation", palette = "YlOrRd", direction = 1) +
  labs(title = "Home ownership")  +
  theme(plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","occ_own.png"), path = here("Output"), width =12, height =11, dpi = reso) 

###Catchment characteristics

#Stream length
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = stream_length)) +
  scale_fill_distiller(name = "Stream km", palette = "OrRd", direction = 1) +
  labs(title = "Total stream length")  +
  theme(plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","steam_len.png"), path = here("Output"), width =12, height =11, dpi = reso) 

#Max elevation
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = max_elevation)) +
  scale_fill_distiller(name = "Meters", palette = "OrRd", direction = 1) +
  labs(title = "Maximum elevation")  +
  theme(plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","max_elev.png"), path = here("Output"), width =12, height =11, dpi = reso)

#Mean slope
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = mean_slope)) +
  scale_fill_distiller(name = "Degrees", palette = "OrRd", direction = 1) +
  labs(title = "Mean slope")  +
  theme(plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","mean_slope.png"), path = here("Output"), width =12, height =11, dpi = reso) 

#Max order
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = stream_order)) +
  scale_fill_distiller(name = "Order", palette = "OrRd", direction = 1) +
  labs(title = "Maximum stream order")  +
  theme(plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","max_order.png"), path = here("Output"), width =12, height =11, dpi = reso)


###Ecological value characteristics
#Species richness
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = species_rich)) +
  scale_fill_distiller(name = "Number of\nfish species", palette = "BuGn", direction = 1) +
  labs(title = "Fish species richness")  +
  theme(plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","species.png"), path = here("Output"), width =12, height =11, dpi = reso)

#Bull trout habitat
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = bull_trout)) +
  scale_fill_distiller(name = "", palette = "BuGn", direction = 1) +
  labs(title = "Bull trout critical habitat (in green)")  +
  theme(
    legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","BT_CH.png"), path = here("Output"), width =12, height =11, dpi = reso) 

#Salmon ESUs
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = esu_count)) +
  scale_fill_distiller(name = "Number of\nESUs", palette = "BuGn", direction = 1) +
  labs(title = "Number of ESA-listed salmon & steelhead populations")  +
  theme(plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","ESU_count.png"), path = here("Output"), width =12, height =11, dpi = reso)

#Impaired waters
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = impaired)) +
  scale_fill_distiller(name = "", palette = "OrRd", direction = 1) +
  labs(title = "EPA 303d impaired waterways (red)")  +
  theme(
    legend.position="none",
    plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","impaired.png"), path = here("Output"), width =12, height =11, dpi = reso)

#Natural cover
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = natural_cover)) +
  scale_fill_distiller(name = "Share  of\nsubwatershed", palette = "BuGn", direction = 1) +
  labs(title = "Proportion natural cover")  +
  theme(plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","nat_cover.png"), path = here("Output"), width =12, height =11, dpi = reso)

#Impervious surface
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = impervious)) +
  scale_fill_distiller(name = "Share  of\ndeveloped cover", palette = "OrRd", direction = 1) +
  labs(title = "Proportion of developed cover that is impervious")  +
  theme(plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","imp_surface.png"), path = here("Output"), width =12, height =11, dpi = reso)

#Percent public
ggplot(eco_soc_maps[eco_soc_maps$HU_8_NAME == basins[i],]) +
  geom_sf(aes(fill = pct_public)) +
  scale_fill_distiller(name = "Share  of\nsubwatershed", palette = "BuGn", direction = 1) +
  labs(title = "Proportion of subwatershed that is public land")  +
  theme(plot.title = element_text(size=14, hjust = 0.5))

ggsave( paste0(basin_short[i],"_","pct_pub.png"), path = here("Output"), width =12, height =11, dpi = reso)


}
