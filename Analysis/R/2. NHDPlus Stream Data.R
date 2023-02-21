###########################################
#
# Brittany King - OSU &
# Robby Fonner - NOAA Fisheries
#
# NHDPlus variables imported (Flowlines, Attributes, Stream Order, Slope, Max Elevation) and Clipped to study area WBD
# 
#
########################################

#Import NHDPlus V2 Flowline Data
flowlines <- st_read(here("Data/NHDPlusPN/NHDPlus17/NHDSnapshot/Hydrography/NHDFlowline.shp")) %>%
  st_transform(4326) %>%
  st_zm()

#Flowline data Clipped study area boundaries
flow_wbd <- st_intersection(huc12_wbd,flowlines)

#Import NHDPlusV2 Value Added Attributes
attributes_v2 <- read.dbf(here("Data/NHDPlusPN/NHDPlus17/NHDPlusAttributes/PlusFlowlineVAA.dbf"))

#Join county flowline and attributes data 

attrib_flow <- merge(flow_wbd, attributes_v2, by.x = "REACHCODE", by.y ="ReachCode", all.x =TRUE, all.y =FALSE) 

stream_order <- attrib_flow %>%
  filter(StreamOrde >= 1)

#Max Stream Order in Catchment 
max_order <- stream_order %>%
  group_by(HUC_12)%>%
  summarise (StreamOrde = max(StreamOrde, na.rm = TRUE ))
#Extract dataframe from sf object
length(max_order$HUC_12[unique(max_order$HUC_12)])

#Import NHDPlusVS Slope 
slope <- read.dbf(here("Data/NHDPlusPN/NHDPlus17/NHDPlusAttributes/elevslope.dbf"))

at_flow_slope <- merge(attrib_flow, slope, by.x = "COMID", by.y ="COMID", all.x =TRUE, all.y =FALSE) 

slope_value <- at_flow_slope %>%
  filter(SLOPE >=0)

mean_slope <-slope_value %>%
  group_by(HUC_12) %>%
  summarise(meanSLOPE = mean(SLOPE))

stream_length_km <- at_flow_slope %>%
  group_by(HUC_12) %>%
  summarise(streamlength = sum(LENGTHKM))



