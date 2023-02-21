###########################################
#
# Brittany King - OSU &
# Robby Fonner - NOAA Fisheries
#
# Use PNSHP data to summarize salmon habitat restoration in 
# Puget Sound during study period
# 
########################################


###I### IMPORT PNSHP DATA AND GET AT WORKSITE-TYPE LEVEL ====


#Which project types to consider
my_basins <- c("PUGET SOUND")
all_types <- c("est", "fishpass",  "flow", "instream", "land",  
               "riparian", "wetland" , "wqimp", "sedireduce", "uplandveg", "livestock", "projmaint", "projmaint", "nutrient", "screen", "agri")

#Import PNSHP data for puget sound years 2000 - present (last observation is 2015)
pnshp_full <- read_csv(here("Data/PNSHP_full_working.csv")) %>%
  mutate( type = word(action, 1, sep = "_")) %>%
  filter(basin %in% my_basins & project_year >= 2000 & type %in% all_types )%>%
  dplyr::select(project_id, worksite_id, project_year, completed_year, latitude, longitude, state, subwatershed,  watershed,  
                subbasin, basin, project_source,project_cost,  numeric_value, units,state_full, n_metric,n_action_wrk,  
                n_metric_wrk,  n_metric_wrk_dist, n_worksites, n_action_prj,  n_action_prj_dist, n_metric_prj,  n_metric_prj_dist, 
                same_action, missing_metric,adj_cost,cost_avail, metric_avail, both_avail, avail_class, n_action_class, type, action)


#number of worksites since 2000 ($2019 usd)
length(unique(pnshp_full$worksite_id))

#number of projects since 2000
length(unique(pnshp_full$project_id))

#Total number of pojects with costs
pnshp_cost <- pnshp_full %>%
  distinct(project_id,adj_cost, completed_year, cost_avail )%>%
  filter(cost_avail == 1)
nrow(pnshp_cost)

#Total reported costs
sum(pnshp_cost$adj_cost)

#Years covered
table(pnshp_full$project_year)
  

