###########################################
#
# 
# Robby Fonner - NOAA Fisheries &
# Brittany King - OSU
# Visualize model estimates
# 
#
########################################

#Load packages
library(modelsummary)
library(flextable)
library(extrafont)
#font_import()
loadfonts(device = "win")

#figure resolution
reso = 750

###POST ESTIMATION### ====

#Create and name list of PS models
nb_ps <- list()
nb_ps[['Instream']] <- instream_nb_ps
nb_ps[['Riparian']] <- riparian_nb_ps
nb_ps[['Land']] <- land_nb_ps
nb_ps[['Fish passage']] <- fishpass_nb_ps
nb_ps[['Total worksites']] <- worksites_nb_ps

#Create and name list of Yak models
nb_y <- list()
nb_y[['Instream']] <- instream_nb_y
nb_y[['Riparian']] <- riparian_nb_y
nb_y[['Land']] <- land_nb_y
nb_y[['Fish passage']] <- fishpass_nb_y
nb_y[['Total worksites']] <- worksites_nb_y

#Create and name list of Will models
nb_w <- list()
nb_w[['Instream']] <- instream_nb_w
nb_w[['Riparian']] <- riparian_nb_w
nb_w[['Land']] <- land_nb_w
nb_w[['Fish passage']] <- fishpass_nb_w
nb_w[['Total worksites']] <- worksites_nb_w

#Make ms word table of results - PS
modelsummary(nb_ps, stars = T, output = here("Output/results table PS.docx"))

#Make ms word table of results - Yak
modelsummary(nb_y, stars = T, output = here("Output/results table Yakima.docx"))

#Make ms word table of results - Wil
modelsummary(nb_w, stars = T, output = here("Output/results table Willamette.docx"))


#make coefficient plots with error bars for ecological and social variables
#you need to specify the variables to omit to create the eco and social variables

inst <- tibble(
  names(confint.default(nb_ps[[1]])[,1]),
  confint.default(nb_ps[[1]])[,1],
  confint.default(nb_ps[[1]])[,2],
  names(nb_ps)[1],
  .name_repair = ~ c("term", "conf.low", "conf.high", "model"))

rip <- tibble(
  names(confint.default(nb_ps[[2]])[,1]),
  confint.default(nb_ps[[2]])[,1],
  confint.default(nb_ps[[2]])[,2],
  names(nb_ps)[2],
  .name_repair = ~ c("term", "conf.low", "conf.high", "model"))

lnd <- tibble(
  names(confint.default(nb_ps[[3]])[,1]),
  confint.default(nb_ps[[3]])[,1],
  confint.default(nb_ps[[3]])[,2],
  names(nb_ps)[3],
  .name_repair = ~ c("term", "conf.low", "conf.high", "model"))

pass <- tibble(
  names(confint.default(nb_ps[[4]])[,1]),
  confint.default(nb_ps[[4]])[,1],
  confint.default(nb_ps[[4]])[,2],
  names(nb_ps)[4],
  .name_repair = ~ c("term", "conf.low", "conf.high", "model"))

wrk_st <- tibble(
  names(confint.default(nb_ps[[5]])[,1]),
  confint.default(nb_ps[[5]])[,1],
  confint.default(nb_ps[[5]])[,2],
  names(nb_ps)[5],
  .name_repair = ~ c("term", "conf.low", "conf.high", "model"))

conf <- rbind(inst,rip,lnd,pass,wrk_st)


ps_res <-  modelplot(nb_ps, conf_level = 0.95, draw = F ) %>%
  dplyr::select(term, model, estimate, std.error) %>%
  left_join(conf, by = c("term", "model")) %>%
  filter(term != "(Intercept)")

#Create and reorder factors
ps_res$term <- ordered(ps_res$term, levels = c(
  "max_elevation", "mean_slope", "stream_length",  "stream_order", "natural_cover",
  "species_rich", "esu_count", "bull_trout", "impaired",
  "pct_public", "impervious", "pop_density", "household_inc", "white_non_hisp","owner_occ", "edu_degree"))
ps_res$term <- fct_rev(ps_res$term)


soc_var <- c("impaired","pct_public", "impervious", "pop_density", "household_inc",  "white_non_hisp","owner_occ", "edu_degree" )

font = "Arial"

ps_res %>% 
  filter( term %in% soc_var)  %>%
  ggplot2::ggplot() +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank()) + 
  geom_pointrange( aes(
    y = term, x = estimate, xmin = conf.low, xmax = conf.high,color = model, shape = model),
    position = ggplot2::position_dodge(width = .7)) +
  #labs( title = "Model results for human impact and social varibles",
  #      subtitle = "Coefficient estimates and 95% confidence intervals") +
  xlab("Estimate value") +
  ylab( "") + 
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "dark grey", size=.5) +
  theme(legend.position="bottom",
        axis.text.y = element_text(size = 12, family = font ),
        axis.text.x = element_text(size = 9, family = font ),
        legend.text = element_text(size = 12, family = font ),
        axis.title.x = element_text(size = 12, family = font )
        )

ggsave( "soc_coeff_plot.png", path = here("Output"), width =12, height =11, dpi = reso)

ps_res %>%
  filter( !(term %in% soc_var))  %>%
  ggplot2::ggplot() +
  ggplot2::theme_minimal() +
  ggplot2::theme(legend.title = ggplot2::element_blank()) + 
  geom_pointrange( aes(
    y = term, x = estimate, xmin = conf.low, xmax = conf.high,color = model, shape = model),
    position = ggplot2::position_dodge(width = .7)) +
  #labs( title = "Model results for atchment characteristics and ecological value varibles",
  #      subtitle = "Coefficient estimates and 95% confidence intervals") +
  xlab("Estimate value") +
  ylab( "") +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "dark grey", size=.5) +
  theme(legend.position="bottom",
        axis.text.y = element_text(size = 12, family = font ),
        axis.text.x = element_text(size = 9, family = font ),
        legend.text = element_text(size = 12, family = font ),
        axis.title.x = element_text(size = 12, family = font )
  ) #,face="bold"

ggsave( "eco_coeff_plot.png", path = here("Output"), width =12, height =11, dpi = reso)



###REGRESSION OUTPUT TO CSV###

#List of models for post-estimation
models <- list(worksites_nb_w, riparian_nb_w, land_nb_w, instream_nb_w, fishpass_nb_w,
               worksites_nb_y, riparian_nb_y, land_nb_y, instream_nb_y, fishpass_nb_y,
               worksites_nb_ps, riparian_nb_ps, land_nb_ps, instream_nb_ps, fishpass_nb_ps) 

###MODELS TO CSV FILES###
#Function to export results to a csv: x= list of model outputs

outreg <- function(x,y){
  
  names <- names(x[[length(x)]]$coefficients)
  results <- data.frame(names)
  model_names <- c("Willamette total worksites" , "Willamette riparian" , "Willamette land", "Willamette instream", "Willamette fish passage",
                   "Yakima total worksites", "Yakima riparian", "Yakima land", "Yakima instream", "Yakima fish passage",
                   "PS total worksites", "PS riparian","PS land", "PS instream", "PS fish passage" )
  
  for(i in 1:length(x)){
    x[[i]]$var <- vcov(x[[i]], complete = TRUE)
    Estimate <- x[[i]]$coefficients
    SE <- diag(as.matrix(x[[i]]$var))^.5
    tval <- Estimate/SE
    pval <-  round( 2*pt(abs(tval), df= length(x[[i]]$residuals)-length(x[[i]]$coefficients)-1 ,   lower=FALSE), digits = 10)
    sig <- if_else(pval<.1,".","")
    sig <- if_else(pval<.05,"*",sig)
    sig <- if_else(pval<.01,"**",sig)
    sig <- if_else(pval<.001,"***",sig)
    names<- names(x[[i]]$coefficients)
    mod_name <- rep(model_names[i], times = length(names))
    res <- data.frame( names,mod_name, Estimate, SE, tval, pval, sig)
    results <- full_join(res, results, by = "names")
  }
  colnames(results) <- c("names", rep(c("Model","Estimate","SE","tval","pval", "sig"), times=length(x)))
  write.csv(results, here(paste0("Output/", y,".csv")), row.names = T)
}

outreg(models, "SocDriversModels")

