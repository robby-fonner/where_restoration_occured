###########################################
#
# 
# Robby Fonner - NOAA Fisheries &
# Brittany King - OSU
#
# Convert nlcd raster to polygon then
# get % landcover by nlcd category
#
########################################

#Landcover data --- only run the command below once basin lancover csvs are created
huc_12_lndcvr_imp <- rbind( read_csv(here("Output/huc12_lndcvr_imp_Puget Sound.csv"))
                            ,read_csv(here("Output/huc12_lndcvr_imp_Willamette.csv"))
                            ,read_csv(here("Output/huc12_lndcvr_imp_Yakima.csv"))
                            ) %>%
  mutate( 
    WATER = WATER + ICE_SNOW,
    DEVELOPED = DEVELOPED_OPEN + DEVELOPED_LOW + DEVELOPED_MED + DEVELOPED_HIGH,
    FOREST = DECIDUOUS_FOR + EVERGREEN_FOR + MIXED_FOR, 
    SHRUBLAND =  SHRUB_SCRUB, #"Dwarf_Scrub"
    HERBACEOUS = GRASS_HERB, #"Sedge_Herb", "Lichens", "Moss"
    CULTIVATED = PASTURE + CROPS, 
    WETLANDS = WOOD_WETLANDS + HERB_WETLANDS
  ) %>%
  dplyr::select(
    HUC_12, WATER, DEVELOPED, FOREST, SHRUBLAND, HERBACEOUS, CULTIVATED, WETLANDS, HUC12_IMP 
  ) %>%
  mutate(HUC_12 = as.character(HUC_12))

#Run script below to create basin landcover csv files
#Loop over all study areas and export to .csv -- this takes long-time Johnson

for( i in 1:length(study_area)){
  
###DEFINE STUDY AREA###
study_area <- c( 1711, 1709, 1703)
st_area_names <- c("Puget Sound", "Willamette" , "Yakima")
  
  ##II##IMPORT DATA AND CONVERT NLCD RASTER TO POLYGONS 
  #1# Import HUC6 polygons and raw raster
  lnd_cvr <-  raster(here("Data/landcover/NLCD_2011_Land_Cover_L48_20190424_Sin9X0Wl9PvrYeF0oOUD.tiff") , RAT = TRUE)
  
  #NLCD imperviousness products represent urban impervious surfaces as a percentage of developed 
  #surface over every 30-meter pixel in the United States (https://www.mrlc.gov/node?page=11)
  imp_sur <- raster(here("Data/landcover/NLCD_2011_Impervious_L48_20190405_Sin9X0Wl9PvrYeF0oOUD.tiff"), RAT = T)
  
HUC12_COVER <-st_read(here("Data/NHDPlusPN/NHDPlus17/WBDSnapshot/WBD/WBD_Subwatershed.shp")) %>%
  filter( as.numeric(substr(HUC_8,1,4)) %in% study_area[i] ) %>%
  st_transform(projection(lnd_cvr)) %>% #Use same projection as lnd_cvr to implement cropping or raster below
  group_by(HUC_12) %>%
  summarize()

#Crop raster and examine raster object
lnd_cvr <- crop(lnd_cvr,HUC12_COVER)
#lnd_cvr #Note it's not picking up the attributes for some reason


imp_sur <- crop(imp_sur,HUC12_COVER)
#imp_sur #Note it's not picking up the attributes for some reason

#Check landcover download coverage
#plot (lnd_cvr, main = "Testing")
#plot (HUC12_COVER["HUC_12"], add= T)

#2# Set NLCD levels in raster (https://www.mrlc.gov/data/legends/national-land-cover-database-2011-nlcd2011-legend)
ID <- c(0,11,12,21,22,23,24,31,41,42,43,51,52,71,72,73,74,81,82,90,95)
class_cvr <- data.frame( ID,
                c(   "NA", 
               "Water", "Ice_Snow", #Water
               "Developed_Open", "Developed_Low", "Developed_Med", "Developed_High", #Developed
               "Barren", #Barren
               "Deciduous_For", "Evergreen_For", "Mixed_For", #Forest
               "Dwarf_Scrub", "Shrub_Scrub", #Shrubland
               "Grass_Herb", "Sedge_Herb", "Lichens", "Moss", #Herbaceous
               "Pasture", "Crops", #Planted/Cultivated
               "Wood_Wetlands", "Herb_Wetlands") #Wetlands
        )

#Ratify -- define RasterLayer as a categorical variable
names(lnd_cvr) <- c("nlcd_2011")
lnd_cvr <- ratify(lnd_cvr)

#names(imp_sur) <- c("imp_2011")
#imp_sur <- ratify(imp_sur)

#Get raster category codes and merge in classifications
rat_cvr <- levels(lnd_cvr)[[1]] %>%
  left_join(class_cvr, by = "ID")

#Set raster levels
levels(lnd_cvr) <- rat_cvr
levels(lnd_cvr)

#3# Write raster to file
writeRaster(lnd_cvr, filename= here("Data/landcover/nlcd.tif"), format="GTiff", overwrite=TRUE)
writeRaster(imp_sur, filename= here("Data/landcover/imp.tif"), format="GTiff", overwrite=TRUE)

#4# Read raster with stars
star_lndcvr <- read_stars(here("Data/landcover/nlcd.tif"))
star_lndcvr

star_imp <- read_stars(here("Data/landcover/imp.tif"))
star_imp

#5# Create polygons from stars raster
poly_lndcvr <- st_as_sf(star_lndcvr, as_points = FALSE, merge = TRUE) %>%
  st_transform(4326) %>% 
  filter(nlcd.tif != "NA")

poly_imp <- st_as_sf(star_imp, as_points = FALSE, merge = TRUE) %>%
  st_transform(4326) %>%
  mutate(
    imp.tif = as.numeric(imp.tif)
    )

#Get rid of problematic polygons
poly_lndcvr <- st_make_valid(poly_lndcvr)
poly_imp <- st_make_valid(poly_imp)

##III## GET PERCENTAGE LANDCOVER AT HUC6 LEVEL USING SIMILAR POLYGON MERGING TECHNIQUE USED TO GET CENSUS DATA AT HUC6 LEVEL###
#Import
HUC12_COVER <- HUC12_COVER %>%
  st_transform(projection(poly_lndcvr))
HUC12_COVER$HUC12_AREA <- as.numeric(st_area(HUC12_COVER))

#6# Intersect landcover polygons and HUC6 polygons
cvr_poly <- st_intersection(HUC12_COVER, poly_lndcvr)
cvr_poly$POLY_AREA <- as.numeric(st_area(cvr_poly))

imp_poly <- st_intersection(HUC12_COVER, poly_imp)
imp_poly$POLY_AREA <- as.numeric(st_area(imp_poly))

#7# Dataframe with HUC6 codes as rows and landcover/impervious surface as columns
huc12_imp <- as.data.frame(imp_poly) %>%
  mutate(
    HUC12_SHARE =  imp.tif * if_else(POLY_AREA/HUC12_AREA > 1,  1, POLY_AREA/HUC12_AREA)) %>%
  group_by(HUC_12) %>%
  summarise(
          HUC12_IMP = sum(HUC12_SHARE, na.rm = T)
  ) %>%
  ungroup()

huc12_lndcvr <- as.data.frame(cvr_poly) %>%
  mutate(
    HUC12_SHARE = if_else(POLY_AREA/HUC12_AREA > 1, 1 , POLY_AREA/HUC12_AREA),
    nlcd.tif = as.character(nlcd.tif)
  ) %>%
  group_by(HUC_12, nlcd.tif) %>%
  summarise(
    HUC12_LNDCVR = sum(HUC12_SHARE, na.rm = T)) %>%
  pivot_wider( names_from = nlcd.tif, values_from = HUC12_LNDCVR) %>%
  ungroup() 

#Join landcover & impervious surface, create landcover sub-categories, make names upper case, and write csv

huc_12_lndcvr_imp <- left_join(huc12_lndcvr, huc12_imp, by = "HUC_12") %>%
  rename_with(toupper) %>%
  mutate(HUC_12 = as.numeric(HUC_12)) %>%
 replace(is.na(.), 0) %>%
   write.csv(here(paste0("Output/huc12_lndcvr_imp_",st_area_names[i],".csv")), row.names = F, na = "") #use write.csv instead of write_csv to make NA blank\

rm(list = ls())

}



