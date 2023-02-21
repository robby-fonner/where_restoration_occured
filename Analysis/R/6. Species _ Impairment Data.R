###########################################
#
# Brittany King - OSU &
# Robby Fonner - NOAA Fisheries
#
# Fish and impairment Data
# 
#
########################################
### RECIPES FOR GEOCOMPUTATION
# Also see: https://geocompr.robinlovelace.net/
###PREPARE ENVIRONMENT ==== #four = in a row creates a chunk w dropdown


#Import ESU Critical Habitat Data (Unique HUC_12 = 411)
esu <- st_read(here("Data/HUC6/ESU/huc6_esu-withadds-ver5-alb27.shp")) %>%
  filter( as.numeric(substr(HUC_12,1,4)) %in% study_area
          & HUC_12 %in% huc12_wbd$HUC_12) %>%
  st_transform(4326)

#Intersection of USGS boundaries and esu shapefile
esu_wbd <- st_intersection(huc12_wbd,esu)

#ESU huc12 for matching -- only salmon-accessible habitat
esus <- esu %>%
  group_by(HUC_12) %>%
  summarise() %>%
  dplyr::select(HUC_12)
  
#Max number of ESU in HUC12
max_esu <- esu_wbd %>%
  group_by(HUC_12) %>%
  summarise (ESUCOUNT = max(ESUCOUNT, na.rm = T))


#Import SteamNet Data - All Fish distributions

streamnet <-st_read(here("Data/StreamNet_20190131.gdb/a0000004a.gdbtable")) %>%
  st_zm() %>%
  st_transform(4326)

  
#Clip Streamnet Data to WBD ( Object ID = 23905, Unique HUC 12 = 409)

fishdis_wbd <- st_intersection(huc12_wbd,streamnet)

fish_huc12 <- fishdis_wbd %>%
  group_by(HUC_12) %>%
  summarise(SpeciesRich = n_distinct(SpeciesID)) %>%
  ungroup()

#Import Bull Trout Critical Habitat Data (ObjectID = 8073, Unique HUC 12 = 262)
bt <- st_read(here("Data/BT/BT_FCH_Streams_2010.shp")) %>%
  # I changed this to include OR but don't have the data so cant run it
  filter(STATE == "OR" |  STATE == "WA") %>%
  st_transform(4326)

#Clip Critical Habitat Data HUC12
bt_wbd <-st_intersection(huc12_wbd,bt)

#Designated Final Critical Habitat in HUC12
bt_fch <- bt_wbd %>%
  filter(FCH == 1) %>%
  group_by(HUC_12) %>%
  summarise (BT_FCH = max(FCH)) %>%
  ungroup()

#Import EPA 303d Impairment data (ObjectID = 1098, Unique HUC 12 = 164)
impairment<- st_read(here("Data/rad_303d_20150501_shp/rad_303d_20150501/rad_303d_l.shp")) %>%
  filter( as.numeric(substr(HUC12,1,4)) %in% study_area ) %>%
  st_transform(4326)

impaired_wbd <-impairment %>%
  dplyr::select(HUC_12 = HUC12, EVENTTYPE)%>%
  mutate(impaired =ifelse(EVENTTYPE == 10001, 1, 0))%>%
  group_by(HUC_12)%>%
  summarise(impaired = n_distinct(impaired, na.rm = T)) %>%
  ungroup()
