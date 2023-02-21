###########################################
#
# Brittany King - OSU &
# Robby Fonner - NOAA Fisheries
#
#  Import elevation data
########################################

elevation <- get_elev_raster(huc12_wbd, z =8) #units in Meters
elevation

elev_crop = crop(elevation, huc12_wbd)

#plot (elev_crop, main = "Testing")
#plot (huc12_wbd["HUC_12"], add= T)

#Changed this code slightly from 1:row to 1:nrow ___PLEASE CHECK
elevate_huc12 <- huc12_wbd
elevate_huc12$POLYID <-1:nrow(elevate_huc12)

elevate_huc12 <- st_cast(elevate_huc12, "MULTIPOLYGON")


polymap <-fasterize(elevate_huc12, elev_crop, "POLYID")  
elev_crop[is.na(values(polymap))] <-NA
#plot(elev_crop, main = "HUC 12 Elevation")

elevate_huc12$mean_elevation <- exact_extract(elev_crop, elevate_huc12, fun=function(x,w) {mean(x,na.rm =T)})
elevate_huc12$max_elevation <- exact_extract(elev_crop, elevate_huc12, fun=function(x,w) {max(x,na.rm =T)})
elevate_huc12$min_elevation <- exact_extract(elev_crop, elevate_huc12, fun=function(x,w) {min(x,na.rm =T)})

elevation_huc12 <- elevate_huc12%>%
  dplyr::select (HUC_12, mean_elevation, max_elevation, min_elevation)

