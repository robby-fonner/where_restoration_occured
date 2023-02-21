###########################################
#
#Master Data Table
# 
#
########################################

#Load packages
library(pastecs)
library(MASS)
library(glm2)
library(corrplot)
library(pscl)
library(car)
library(fastDummies)
library(sandwich)
library(lmtest)


#Extract dataframes from sf objects
huc12_wbd <- st_set_geometry(huc12_wbd, NULL)
esus <- st_set_geometry(esus, NULL)
max_order <- st_set_geometry(max_order, NULL) 
mean_slope <- st_set_geometry(mean_slope, NULL)
stream_length_km <-st_set_geometry(stream_length_km, NULL)
elevation_huc12 <-st_set_geometry(elevation_huc12, NULL)
max_esu <- st_set_geometry(max_esu, NULL)
fish_huc12 <- st_set_geometry(fish_huc12, NULL)
bt_fch <-st_set_geometry(bt_fch, NULL)
impaired_wbd <-st_set_geometry(impaired_wbd, NULL)

### Create Data Table with Ecological and Social Data
eco_social_data <- esus %>% #Limit study area to salmon-accessible watersheds
  inner_join(huc12_wbd, by = "HUC_12" ) %>%
  inner_join( social, by = "HUC_12" ) %>%
  inner_join(max_order, by = "HUC_12") %>%
  inner_join(mean_slope, by = "HUC_12") %>%
  inner_join(stream_length_km, by = "HUC_12") %>%
  inner_join(elevation_huc12, by = "HUC_12") %>%
  inner_join(max_esu, by = "HUC_12") %>%
  inner_join(fish_huc12, by = "HUC_12") %>%
  left_join(bt_fch, by = "HUC_12") %>%
  left_join(impaired_wbd, by = "HUC_12") %>%
  left_join(huc12_pub, by = "HUC_12") %>%
  left_join(huc_12_lndcvr_imp, by = "HUC_12") %>%
  filter( HU_8_NAME != "UPPER COLUMBIA") %>%
  mutate(
    natural_cover = FOREST + SHRUBLAND + HERBACEOUS + WETLANDS,
    POP_DENSITY = as.numeric(POP_DENSITY), 
    SpeciesRich = as.numeric(SpeciesRich),
    impaired = ifelse(!is.na(impaired),impaired, 0),
    BT_FCH = ifelse(!is.na(BT_FCH),BT_FCH, 0),
    HUC12_PCTPUB = ifelse(!is.na(HUC12_PCTPUB), HUC12_PCTPUB, 0),
    PUGET = if_else( HU_8_NAME == "PUGET SOUND", 1, 0),
    YAKIMA = if_else(HU_8_NAME == "YAKIMA", 1, 0),
    WILLAM = if_else(HU_8_NAME == "WILLAMETTE", 1, 0)
    ) %>%
  dplyr::select(
    HUC_12, HU_12_NAME, HU_8_NAME, population = HUC12_POP, pop_density = POP_DENSITY, household_inc = HH_INC, pov_below = POV_BELOW, over_65 = HH_OVER65, under_18 = HH_UNDER18, 
    edu_hs_max = EDU_HS_MAX, edu_degree = EDU_DEGREE, owner_occ = OCC_UNIT_OWN, white_non_hisp = RACE_WA, native = RACE_NATIVE,
    mean_elevation ,  max_elevation, min_elevation, mean_slope = meanSLOPE, stream_order = StreamOrde, species_rich = SpeciesRich, 
    bull_trout = BT_FCH, 
    esu_count = ESUCOUNT, impaired, pct_public  = HUC12_PCTPUB, natural_cover, impervious = HUC12_IMP, stream_length = streamlength,
    PUGET, YAKIMA, WILLAM,
    area = huc_area) 
    # %>% drop_na() #Drop watersheds with incomplete explanatory data

write_csv(eco_social_data, here("Output/eco_soc_data.csv"))
#eco_social_data <- read_csv(here("Output/eco_soc_data.csv"))
#dep_vars <- read_csv(here("Output/dep_vars.csv"))

#Purge objects to free up memory
#rm(list=setdiff(ls(), c("eco_social_data", "dep_vars", "social", "study_area")) )
  
####Standardizing all variables by  subtracting the mean and dividing by 2 SD
standard_data_2 <- eco_social_data %>%
  group_by(HU_8_NAME) %>%
  mutate(stream_length = (stream_length -mean(stream_length))/(sd(stream_length)*2), 
         population = (population - mean(population))/(sd(population)*2),
         household_inc = (household_inc - mean(household_inc))/(sd(household_inc)*2),
         pop_density = (pop_density - mean(pop_density))/(sd(pop_density)*2),
         pov_below = (pov_below - mean(pov_below))/(sd(pov_below)*2),
         over_65 = (over_65 - mean(over_65))/(sd(over_65)*2),
         under_18 = (under_18 - mean(under_18))/(sd(under_18)*2),
         edu_degree = (edu_degree - mean(edu_degree))/(sd(edu_degree)*2),
         edu_hs_max = (edu_hs_max - mean(edu_hs_max))/(sd(edu_hs_max)*2),
         owner_occ = (owner_occ - mean(owner_occ))/(sd(owner_occ)*2),
         white_non_hisp = (white_non_hisp - mean(white_non_hisp))/(sd(white_non_hisp)*2), 
         native = (native - mean(native))/(sd(native)*2), 
         mean_elevation = (mean_elevation - mean(mean_elevation))/(sd(mean_elevation)*2),
         max_elevation = (max_elevation - mean(max_elevation))/(sd(max_elevation)*2),
         min_elevation= (min_elevation - mean(min_elevation))/(sd(min_elevation)*2),
         mean_slope = (mean_slope - mean(mean_slope))/(sd(mean_slope)*2),
         stream_order = (stream_order - mean(stream_order))/(sd(stream_order)*2),
         species_rich = (species_rich - mean(species_rich))/(sd(species_rich)*2),
         bull_trout = (bull_trout - mean(bull_trout))/(sd(bull_trout)*2),
         esu_count = (esu_count - mean(esu_count))/(sd(esu_count)*2),
         impaired = (impaired - mean(impaired))/(sd(impaired)*2),
         pct_public = (pct_public - mean(pct_public))/(sd(pct_public)*2),
         natural_cover = (natural_cover - mean(natural_cover))/(sd(natural_cover)*2),
         impervious = (impervious - mean(impervious))/(sd(impervious)*2),
         HUC_12 = as.character(HUC_12))  %>%
  ungroup()

 #Standardized - Model Run
  stan_model_data <- dep_vars %>%
    mutate(HUC_12 = as.character(HUC_12)) %>%
    left_join( standard_data_2, by= "HUC_12")
  

  #Define dependent variables
  Dep1 <- "instream_act"
  Dep2 <- "riparian_act"
  Dep3 <- "land_act"
  Dep4 <- "fishpass_act"
  Dep5 <- "n_worksites"
  Indep <- c( "max_elevation","stream_length","mean_slope", "stream_order", "natural_cover", "impervious","species_rich", "bull_trout", "esu_count", "impaired", 
             "pct_public", "household_inc",  "white_non_hisp","owner_occ", "edu_degree", "pop_density") 
  
 
  
  f1 <- as.formula(
    paste(Dep1,
          paste(Indep, collapse = "+"),
          sep = "~"))
  
  
  f2 <- as.formula(
    paste(Dep2,
          paste(Indep, collapse = "+"),
          sep = "~"))
  
  f3 <- as.formula(
    paste(Dep3,
          paste(Indep, collapse = "+"),
          sep = "~"))
  
  f4 <- as.formula(
    paste(Dep4,
          paste(Indep, collapse = "+"),
          sep = "~"))
  f5 <- as.formula(
    paste(Dep5,
          paste(Indep, collapse = "+"),
          sep = "~"))
  
  
  # Negative Binomial (Eco_Social_Puget Sound)
  stan_basin_data_ps <- stan_model_data %>%
    filter( HU_8_NAME == "PUGET SOUND")
  
  
instream_nb_ps <- glm.nb(f1, data = stan_basin_data_ps, family = binomial(link = log) , control = glm.control(maxit = 500))
  summary(instream_nb_ps)
  vif(instream_nb_ps)
  
riparian_nb_ps <- glm.nb(f2, data = stan_basin_data_ps, family =binomial(link = log), control = glm.control(maxit = 200))
  summary(riparian_nb_ps)
  vif(riparian_nb_ps)
  
land_nb_ps <- glm.nb(f3, data = stan_basin_data_ps, family =binomial(link = log), control = glm.control(maxit = 500))
  summary(land_nb_ps)
  vif(land_nb_ps)
  
fishpass_nb_ps <- glm.nb(f4, data = stan_basin_data_ps, family =binomial(link = log), control = glm.control(maxit = 200))
  summary(fishpass_nb_ps)
  vif(fishpass_nb_ps)
  
worksites_nb_ps <- glm.nb(f5, data = stan_basin_data_ps, family =binomial(link = log), control = glm.control(maxit = 200))
  summary(worksites_nb_ps)
  vif(worksites_nb_ps)
  

  # Negative Binomial (Eco_Social Yakima)
  
  stan_basin_data_y <- stan_model_data %>%
    filter( HU_8_NAME == "YAKIMA")
  
instream_nb_y <- glm.nb(f1, data = stan_basin_data_y, family =binomial(link = log), control = glm.control(maxit = 200))
  summary(instream_nb_y)
  vif(worksites_nb_y)
  
riparian_nb_y <- glm.nb(f2, data = stan_basin_data_y, family =binomial(link = log), control = glm.control(maxit = 200))
  summary(riparian_nb_y)
  vif(riparian_nb_y)
  
land_nb_y <- glm.nb(f3, data = stan_basin_data_y, family =binomial(link = log), control = glm.control(maxit = 200))
  summary(land_nb_y)
  vif(land_nb_y)
  
fishpass_nb_y <- glm.nb(f4, data = stan_basin_data_y, family =binomial(link = log), control = glm.control(maxit = 500))
  summary(fishpass_nb_y)
  vif(fishpass_nb_y)
  
worksites_nb_y <- glm.nb(f5, data = stan_basin_data_y, family =binomial(link = log), control = glm.control(maxit = 200))
  summary(worksites_nb_y)
  vif(worksites_nb_y)
  
#Negative Binomial (Eco_Social Willamette)
  
stan_basin_data_w <- stan_model_data %>%
  filter( HU_8_NAME == "WILLAMETTE")
  
instream_nb_w <- glm.nb(f1, data = stan_basin_data_w, family =binomial(link = log), control = glm.control(maxit = 200))
  summary(instream_nb_w)
  vif(worksites_nb_w)
  
riparian_nb_w <- glm.nb(f2, data = stan_basin_data_w, family =binomial(link = log), control = glm.control(maxit = 200))
  summary(riparian_nb_w)
  vif(riparian_nb_w)
  
land_nb_w <- glm.nb(f3, data = stan_basin_data_w, family =binomial(link = log), control = glm.control(maxit = 500))
  summary(land_nb_w)
  vif(land_nb_w)
  
fishpass_nb_w <- glm.nb(f4, data = stan_basin_data_w, family =binomial(link = log), control = glm.control(maxit = 200))
  summary(fishpass_nb_w)
  vif(fishpass_nb_w)
  
worksites_nb_w <- glm.nb(f5, data = stan_basin_data_w, family =binomial(link = log), control = glm.control(maxit = 200))
  summary(worksites_nb_w)
  vif(worksites_nb_w)
  
###########################DIDNT ESTIMATE MODELS PAST HERE ##############################
  
  #Linear model
  CostDep1 <- "prj_cst"
  CostDep2 <- "riparian_cst"
  CostDep3 <- "land_cst"
  CostDep4 <- "instream_cst"
  CostDep5 <- "main_cst"
  
  cost_f1 <- as.formula(
    paste(CostDep1,
          paste(Indep, collapse = "+"),
          sep = "~"))
  
  cost_f2 <- as.formula(
    paste(CostDep2,
          paste(Indep, collapse = "+"),
          sep = "~"))
  
  cost_f3 <- as.formula(
    paste(CostDep3,
          paste(Indep, collapse = "+"),
          sep = "~"))
  
  cost_f4 <- as.formula(
    paste(CostDep4,
          paste(Indep, collapse = "+"),
          sep = "~"))
  
  cost_f5 <- as.formula(
    paste(CostDep5,
          paste(Indep, collapse = "+"),
          sep = "~"))
  
  
  total_cost_ols <- lm(cost_f1, data = basin_data)
  summary(total_cost_ols)
  
  rip_cost_ols <- lm(cost_f2, data = basin_data)
  summary(rip_cost_ols)
  
  land_cost_ols <- lm(cost_f3, data = basin_data)
  summary(land_cost_ols)
  
  instream_cost_ols <- lm(cost_f4, data = basin_data)
  summary(instream_cost_ols)
  
main_cost_ols <- lm(cost_f5, data = basin_data)
  summary(main_cost_ols)



options(digits=2)

stat.desc(basin_data)


  