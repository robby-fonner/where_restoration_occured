###########################################
#
# Brittany King - OSU &
# Robby Fonner - NOAA Fisheries
#
# Use PNSHP data to create dependent variables 
# for social drivers of habitat restoration effort
# 
########################################


###I### IMPORT PNSHP DATA AND GET AT WORKSITE-TYPE LEVEL ====

#Which project types to consider
main_types <- c("riparian", "instream", "land")
all_types <- c("est", "fishpass",  "flow", "instream", "land", "screen", 
               "riparian", "wetland" , "wqimp", "nutrient") #"sedireduce", "uplandveg", "livestock", "projmaint", "projmaint","agri",
my_basins <- c("PUGET SOUND", "WILLAMETTE", "YAKIMA")

#Import PNSHP data for puget sound years 2000 - present (last observation is 2015)
pnshp_full <- read_csv(here("Data/PNSHP_full_working.csv")) %>%
  mutate( type = word(action, 1, sep = "_") ,
          basin = if_else(basin =="YAKIMA RIVER" , "YAKIMA", basin)) %>% #assuming these are the same
  filter(basin %in% my_basins & project_year >= 2000 & type %in% all_types) %>%
  dplyr::select(project_id, worksite_id, action, type, project_year, completed_year, latitude, longitude, state, subwatershed,  watershed,  
         subbasin, basin, project_source,project_cost,  numeric_value, units,state_full, n_metric,n_action_wrk,  
         n_metric_wrk,  n_metric_wrk_dist, n_worksites, n_action_prj,  n_action_prj_dist, n_metric_prj,  n_metric_prj_dist, 
         same_action, missing_metric,adj_cost,cost_avail, metric_avail, both_avail, avail_class, n_action_class) %>%
  group_by(worksite_id, type) %>%
  #number of actions by workstite-type
  mutate( n_act_wrk = n() ) %>%
  ungroup() %>%
  group_by(project_id, type) %>%
  #number of projects by type
  mutate(n_type_prj = n() ) %>%
  distinct(worksite_id, type, .keep_all = TRUE)

###II### IMPORT SPATIAL DATA AND PERFORM SPATIAL ANALYSIS ====

#Import PHSHP as a spatial object
pnshp_spat <- pnshp_full %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    agr = "constant",
    crs = 4326,   #Same crs as above
    stringsAsFactors = FALSE,
    remove = TRUE) %>%
  dplyr::select( project_id, worksite_id, type, n_type_prj, n_act_wrk, adj_cost, geometry)

##JOIN pnshp to HUC12
pnshp_sp <- pnshp_spat %>%
  st_join(huc12_wbd, join = st_within)
 
#Project classifications
prj_class <- as.data.frame(pnshp_sp) %>%
  group_by(project_id) %>%
  mutate(
    instream_only = if_else(n_type_prj == 1 & type == "instream", 1, 0),
    riparian_only = if_else(n_type_prj == 1 & type == "riparian", 1, 0),
    land_only = if_else(n_type_prj == 1 & type == "land", 1, 0),
    pass_only = if_else(n_type_prj == 1 & type == "fishpass", 1, 0),
    main_types =  if_else( !(type %in% main_types) , 0 , 1) ,
    cost_na = if_else(is.na(adj_cost), 1, 0))  %>%
  ungroup() %>%
  dplyr::select(project_id, instream_only, riparian_only, land_only, pass_only, main_types, cost_na) %>%
  distinct(project_id, .keep_all = TRUE)

#Weight projects that span multiple HUC6 areas by number of actions per HUC6
costs <- as.data.frame(pnshp_sp) %>%
  group_by(project_id) %>%
  mutate(
    #number of HUC6 per project 
    n_huc_prj = length(unique(HUC_12)),
    #number of actions per project
    n_act_prj = sum(n_act_wrk, na.rm = T)
    )  %>%
  ungroup() %>%
  group_by(project_id, HUC_12) %>%
    mutate(
    #number of actions per HUC by project
    n_act_huc_prj = sum(n_act_wrk, na.rm = T) ) %>%
  ungroup() %>%
  mutate(
    cost_share = n_act_huc_prj/n_act_prj,
    #Costs allocated to HUC6 areas based on
    adj_cost_huc = adj_cost * cost_share) %>%
  distinct(project_id, .keep_all = TRUE) %>%
  left_join( prj_class, by = "project_id" ) %>%
  group_by(HUC_12) %>%
  summarise(
    instream_cst = sum(if_else(instream_only ==1, adj_cost_huc, 0), na.rm = T),
    n_instream_huc = sum(if_else(instream_only ==1, 1, 0), na.rm = T),
    n_instream_cst = sum(if_else(instream_only ==1 & instream_cst > 0, 1, 0), na.rm = T),
    
    riparian_cst= sum(if_else(riparian_only ==1, adj_cost_huc, 0), na.rm = T),
    n_riparian_huc = sum(if_else(riparian_only ==1, 1, 0), na.rm = T),
    n_riparian_cst = sum(if_else(riparian_only ==1 & riparian_cst > 0, 1, 0), na.rm = T),
    
    land_cst = sum(if_else(land_only ==1, adj_cost_huc, 0), na.rm = T),
    n_land_huc = sum(if_else(land_only ==1, 1, 0), na.rm = T),
    n_land_cst = sum(if_else(land_only ==1 & land_cst > 0, 1, 0), na.rm = T),
    
    main_cst = sum(if_else(main_types ==1, adj_cost_huc, 0), na.rm = T),
    n_main_huc = sum(if_else(main_types ==1, 1, 0), na.rm = T),
    n_main_cst = sum(if_else(main_types ==1 & main_cst > 0, 1, 0), na.rm = T),
    
    prj_cst =  sum(adj_cost_huc, na.rm = T),
    n_prj_cst = sum(if_else(!is.na(adj_cost_huc) , 1, 0)) ,
    n_prj_huc = n()
    
  ) %>%
  mutate(
    across( c(instream_cst, riparian_cst, land_cst, main_cst, n_prj_huc), ~na_if(.x, 0) ),
    
    instream_cst = if_else(n_instream_huc == 0 , 0 , instream_cst),
    riparian_cst = if_else(n_riparian_huc == 0 , 0 , riparian_cst),
    land_cst = if_else(n_land_huc == 0 , 0 , land_cst),
    prj_cst = if_else(n_prj_cst == 0 , 0 , prj_cst)
    
  )

#Create worksite_counts
actions_costs <- as.data.frame(pnshp_sp) %>%
  mutate(
    cats =  case_when( type == "instream" ~ "instream_act",
                       type == "riparian" ~ "riparian_act",
                       type == "land" ~ "land_act",
                       type == "fishpass" ~ "fishpass_act",
                       TRUE ~ "other_act")) %>%
  filter(cats != "other_act") %>%
  distinct(worksite_id, cats, .keep_all = TRUE) %>%
  pivot_wider(names_from = cats, values_from = n_act_wrk) %>%
  mutate(
    instream_act = ifelse(!is.na(instream_act) & instream_act > 0 , 1, NA),
    riparian_act = ifelse(!is.na(riparian_act)& riparian_act > 0 , 1, NA),
    land_act = ifelse(!is.na(land_act)& land_act > 0 , 1, NA),
    fishpass_act = ifelse(!is.na(fishpass_act)& fishpass_act > 0 , 1, NA)
  ) %>%
  group_by(HUC_12) %>%
  summarise(across(c(instream_act, riparian_act, land_act, fishpass_act), sum, na.rm=T))


#Create worksite counts
worksites <- as.data.frame(pnshp_sp) %>%
  distinct(worksite_id, .keep_all = T) %>%
  group_by(HUC_12) %>%
  summarise(
    n_worksites = n()
  )


#Combine dependent variables and add HUCs where no projects occurred. 
dep_vars <- huc12_wbd %>%
  dplyr::select(HUC_12) %>%
  left_join(actions_costs, by = "HUC_12" ) %>%
  mutate(HUC_12 = as.numeric(HUC_12)) %>%
  replace(is.na(.),0) %>%
  mutate(HUC_12 = as.character(HUC_12)) %>%
  left_join(costs, by = "HUC_12") %>%
  left_join(worksites, by = "HUC_12") %>%
  mutate(
    n_worksites = ifelse( !is.na(n_worksites), n_worksites, 0) 
  )


#Replace NA with zero for HUC costs and action counts where no projects occurred
dep_vars[ dep_vars$instream_act +  dep_vars$riparian_act +  dep_vars$land_act + dep_vars$fishpass_act == 0  ,
           (ncol(actions_costs)+1):(ncol(dep_vars)-1)] <- 0

dep_vars <- st_set_geometry(dep_vars, NULL)

write_csv(dep_vars, here("Output/dep_vars.csv"))

  
  
