###########################################
#
# Brittany King - OSU &
# Robby Fonner - NOAA Fisheries
#
# This code downloads 2010 Census and 2013 ACS data
# at the block and block group level and then estimates the value 
# of the downloaded social variables in HUC6 areas as the 
# average of intersecting block (or block groups) values weighted by population
#
#
########################################


social <- read_csv(here("Output/huc12_social.csv")) %>% 
  mutate(HUC_12 = as.character(HUC_12) )  #### this include Native Population

##Run script below to fetch census data by state and create population weighted averages of social variables by csv

#IMPORT CENSUS BLOCK GROUP POLYGONS AND DATA
###Define counties of interest

my_states <- c("WA", "OR")

fips <- fips_codes %>%
  filter( state %in% my_states )

#census_api_key("7fe5dda10ad35ef9e0361f92ae3a0b0144119c17", install = TRUE, overwrite = TRUE)


#2011 ACS
#Look at acs variables
acs_view <- load_variables(2013, "acs5") %>%
  filter(concept == "MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2013 INFLATION-ADJUSTED DOLLARS)","EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER", "HOUSEHOLDS BY PRESENCE OF PEOPLE UNDER 18 YEARS BY HOUSEHOLD TYPE", "HOUSEHOLDS BY PRESENCE OF PEOPLE 65 YEARS AND OVER, HOUSEHOLD SIZE AND HOUSEHOLD TYPE", "TENURE BY HOUSEHOLD SIZE BY AGE OF HOUSEHOLDER" )

#Variables to download
acs_vars <- c( HH_INC = "B19013_001", POP_25 = "B15003_001", EDU_HS = "B15003_017", EDU_HS_EQ = "B15003_018",
               EDU_AD = "B15003_021", EDU_BD = "B15003_022",EDU_MD= "B15003_023", EDU_PD = "B15003_024", EDU_DD = "B15003_025",
               POV_STAT = "B17021_001", POV_STAT_BELOW = "B17021_002") #Add additional variables here


#Import block group data
blockgrp_acs <- get_acs(geography = "block group",
                        state = my_states,
                        variables = acs_vars,
                        year = 2013, 
                        geometry = F) %>%
  pivot_wider( names_from = variable, values_from = estimate) %>%
  dplyr::select(blockgrp_ID = GEOID, HH_INC, POP_25, EDU_HS, EDU_HS_EQ, EDU_AD, EDU_BD, EDU_MD, EDU_PD, EDU_DD, POV_STAT, POV_STAT_BELOW )%>%
  group_by(blockgrp_ID) %>%
  summarize(HH_INC = sum(HH_INC, na.rm = T), POP_25 = sum(POP_25, na.rm = T), EDU_HS_EQ = sum(EDU_HS_EQ, na.rm = T), EDU_HS = sum(EDU_HS, na.rm = T),  
            EDU_AD = sum(EDU_AD, na.rm = T), EDU_BD = sum(EDU_BD, na.rm = T), EDU_MD = sum(EDU_MD, na.rm = T), EDU_PD = sum(EDU_PD, na.rm = T),
            EDU_DD = sum(EDU_DD, na.rm = T),
            POV_STAT = sum(POV_STAT, na.rm =T), POV_STAT_BELOW = sum(POV_STAT_BELOW, na.rm= T)) %>%
  mutate(EDU_HS_MAX = EDU_HS+EDU_HS_EQ,
         HS_MAX = EDU_HS_MAX/POP_25,
         COLLEGE = EDU_AD+EDU_BD+EDU_MD+EDU_PD+EDU_DD,
         EDU_DEGREE = COLLEGE/POP_25,
         POV_BELOW = POV_STAT_BELOW/POV_STAT)%>%
  dplyr::select(blockgrp_ID, HH_INC, POV_BELOW, HS_MAX, EDU_DEGREE)



#2010 Census data
census_vars <- c( TOT_POP = "P001001", RACE_WA_NO = "P009005",RACE_WA_TEST = "P003002",  RACE_NATIVE_NO = "P006004", NUM_HH = "P020001", NUM_HH_18 = "P020002", NUM_HH_65 = "P025002", 
                  O_UNIT = "H004001", O_OWN_M = "H004002", O_OWN_F = "H004003") #Add additional variables here

#Import census block data for oregon and washington using map2_dfr to get both
blocks_cen <- map2_dfr(  
  fips$state_code, fips$county_code,
  ~ get_decennial( geography = "block",
                              state = .x,
                              county = .y,
                              variables = census_vars,
                              year = 2010, 
                              geometry = F)) %>%
  pivot_wider( names_from = variable, values_from = value) %>%
  dplyr::select(block_ID = GEOID, TOT_POP, RACE_WA_NO,RACE_WA_TEST, RACE_NATIVE_NO, NUM_HH, NUM_HH_18, NUM_HH_65, O_UNIT, O_OWN_M, O_OWN_F)%>%
  mutate(RACE_WA = RACE_WA_NO/TOT_POP, RACE_NATIVE = RACE_NATIVE_NO/TOT_POP,
         OWN_OCC = O_OWN_F+ O_OWN_M,
         OCC_UNIT_OWN = OWN_OCC/O_UNIT,
         HH_under18 = NUM_HH_18/NUM_HH,
         HH_over65 = NUM_HH_65/NUM_HH) %>%
  dplyr::select(block_ID, TOT_POP, RACE_WA, RACE_WA_TEST, RACE_NATIVE, OCC_UNIT_OWN, HH_under18, HH_over65)

#Import census block data for oregon and washington using map2_dfr to get both
blocks <- map2(
  fips$state_code, fips$county_code,
  ~ get_decennial(        geography = "block",
                          state = .x,
                          county = .y,
                          variables = c( TOT_POP = "P001001"),
                          year = 2010, 
                          geometry = T)) %>%
  reduce(rbind) %>%
  dplyr::select(block_ID = GEOID, geometry) %>%
  mutate(
    blockgrp_ID = str_sub(block_ID,1,12)
  ) %>%
  distinct(block_ID, .keep_all = T) %>%
  left_join(blocks_cen, by = "block_ID") %>%
  left_join(blockgrp_acs, by = "blockgrp_ID") %>%
  st_transform(4326)
  blocks <- blocks %>%
  mutate(BLOCK_AREA = as.numeric(st_area(blocks)),
  BLOCK_AREA_KM = units::set_units(st_area(blocks), km^2),
  POP_DENSITY = TOT_POP/BLOCK_AREA_KM
  )

#INTERSECT CENSUS BLOCKS WITH HUC12
block_HUC12_poly <- st_intersection(huc12_wbd, blocks)
block_HUC12_poly$BLOCK_HUC12_AREA = as.numeric(st_area(block_HUC12_poly))

# Calculate social variables by HUC12 and export csv
huc12_soc <- block_HUC12_poly %>%
  dplyr::select(HUC_12, BLOCK_HUC12_AREA, BLOCK_AREA, TOT_POP, POP_DENSITY, HH_INC, POV_BELOW, HH_over65, 
                HH_under18, EDU_DEGREE, HS_MAX, OCC_UNIT_OWN, RACE_WA, RACE_NATIVE, geometry) %>%
  mutate(
    BLOCK_SHARE = BLOCK_HUC12_AREA/BLOCK_AREA, #area of intersected polygon divided by area of census block
    BLOCK_SHARE_POP = BLOCK_SHARE * TOT_POP) %>% #population of each polygon created by HUC12-Block intersection
  group_by(HUC_12) %>%
  mutate(
    HUC12_POP = sum(BLOCK_SHARE_POP, na.rm = T)) %>% #total HUC12 population
  ungroup() %>%
  mutate(
    HUC12_SHARE = BLOCK_SHARE_POP/HUC12_POP, #share of total huc12 population in each intersected polygon
    HH_INC = HH_INC * HUC12_SHARE,
    POP_DENSITY = POP_DENSITY * HUC12_SHARE,
    POV_BELOW = POV_BELOW * HUC12_SHARE, 
    HH_OVER65 = HH_over65 * HUC12_SHARE,
    HH_UNDER18 = HH_under18 * HUC12_SHARE, 
    EDU_DEGREE = EDU_DEGREE * HUC12_SHARE,
    EDU_HS_MAX = HS_MAX * HUC12_SHARE,
    OCC_UNIT_OWN = OCC_UNIT_OWN * HUC12_SHARE,
    RACE_WA = RACE_WA *HUC12_SHARE,
    RACE_NATIVE = RACE_NATIVE * HUC12_SHARE #Add additional variables here (first step)
  ) %>%
  group_by(HUC_12) %>%
  summarise(
    HUC12_POP = sum(BLOCK_SHARE_POP, na.rm = T), #Add additional variables here (second step)
    HH_INC = sum(HH_INC, na.rm = T), #Add additional variables here (second step)
    POP_DENSITY = sum(POP_DENSITY, na.rm = T),
    POV_BELOW = sum(POV_BELOW, na.rm = T),
    HH_OVER65 = sum(HH_OVER65, na.rm = T),
    HH_UNDER18 = sum(HH_UNDER18, na.rm = T),
    EDU_DEGREE = sum(EDU_DEGREE, na.rm = T),
    EDU_HS_MAX = sum(EDU_HS_MAX, na.rm = T),
    OCC_UNIT_OWN = sum(OCC_UNIT_OWN, na.rm = T),
    RACE_WA = sum(RACE_WA, na.rm = T),
    RACE_NATIVE = sum(RACE_NATIVE, na.rm = T)) %>%
  st_set_geometry(NULL) 

write.csv(huc12_soc, here("Output/huc12_social.csv"), row.names = F, na = "")






