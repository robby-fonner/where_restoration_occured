###########################################
#
# 
# Robby Fonner - NOAA Fisheries &
# Brittany King - OSU
#
# Get percentage public land by 
# HUC6
#
########################################


#HUC12 boundaries by state

huc12_OR <- read_sf(here("Data/NHDPlusPN/NHDPlus17/WBDSnapshot/WBD/WBD_Subwatershed.shp")) %>%
  filter( as.numeric(substr(HUC_12,1,4)) %in% c( 1709) ) %>% #Only Willamette in OR
  st_transform(4326) %>%
  group_by(HUC_12) %>%
  summarise() %>%
  ungroup()
huc12_OR$HUC12_AREA <- as.numeric(st_area(huc12_OR))

huc12_WA <- read_sf(here("Data/NHDPlusPN/NHDPlus17/WBDSnapshot/WBD/WBD_Subwatershed.shp")) %>%
  filter( as.numeric(substr(HUC_12,1,4)) %in% c( 1709, 1711, 1703) ) %>% #All others have at least some area in WA
  st_transform(4326) %>%
  group_by(HUC_12) %>%
  summarise() %>%
  ungroup()
huc12_WA$HUC12_AREA <- as.numeric(st_area(huc12_WA))

#Add Washington public land layer 
pub_lnd_wa <- st_read(here("Data/PLIP-RCO-PublicInfoShapefile/WA_PublicLandsInventory_2014_PublicInfo.shp")) %>%
  st_transform(4326) %>%
  ms_clip(huc12_WA, remove_slivers = T)
pub_lnd_wa <- st_make_valid(pub_lnd_wa)

#Add Oregon public land layer 
pub_lnd_or <- st_read(here("Data/kx-oregon-public-land-ownership-2003-SHP/oregon-public-land-ownership-2003.shp")) %>%
  st_transform(4326) %>%
  ms_clip(huc12_OR, remove_slivers = T) %>%
  filter( LAND_OWN != "Private")
pub_lnd_or <- st_make_valid(pub_lnd_or)

#Intersect HUC6 and public land layers
pub_poly_or <- st_intersection(huc12_OR,  pub_lnd_or)
pub_poly_or$POLY_AREA <- as.numeric(st_area(pub_poly_or))

pub_poly_wa <- st_intersection(huc12_WA,  pub_lnd_wa)
pub_poly_wa$POLY_AREA <- as.numeric(st_area(pub_poly_wa))

#7#Calculate area and proportion of public lands by huc12
huc12_pub_or <- as.data.frame(pub_poly_or) %>%
  group_by(HUC_12,HUC12_AREA) %>%
  summarise(
    HUC12_PUB = sum(POLY_AREA, na.rm = T)) %>%
  mutate(
    HUC12_PCTPUB = HUC12_PUB/HUC12_AREA,
    PUB_STATE = "OR",
    PUB_STATE = if_else(HUC_12 %in% intersect(pub_poly_wa$HUC_12, pub_poly_or$HUC_12), "OR/WA", PUB_STATE)
  ) %>%
  dplyr::select(HUC_12, HUC12_PUB, HUC12_PCTPUB, PUB_STATE) %>%
  rename_with(toupper)

huc12_pub_wa <- as.data.frame(pub_poly_wa) %>%
  group_by(HUC_12,HUC12_AREA) %>%
  summarise(
    HUC12_PUB = sum(POLY_AREA, na.rm = T)) %>%
  mutate(
    HUC12_PCTPUB = HUC12_PUB/HUC12_AREA,
    PUB_STATE = "WA",
    PUB_STATE = if_else(HUC_12 %in% intersect(pub_poly_wa$HUC_12, pub_poly_or$HUC_12), "OR/WA", PUB_STATE)
  ) %>%
  dplyr::select(HUC_12, HUC12_PUB, HUC12_PCTPUB, PUB_STATE) %>%
  rename_with(toupper)

#Combine public lands for HUC12 that span OR and WA 
huc12_pub <- rbind(huc12_pub_or,huc12_pub_wa) %>%
  group_by(HUC_12, PUB_STATE) %>%
  summarise(
    HUC12_PUB =  sum(HUC12_PCTPUB, na.rm = T),
    HUC12_PCTPUB = sum(HUC12_PCTPUB, na.rm = T)
    ) %>%
  ungroup()
