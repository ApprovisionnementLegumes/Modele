########################################
# Load the libraries, functions & datas
########################################
rm(list=ls())                                                                   # Clean all variables
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))                     # Set working directory to the current file
pathway = "Database projet.xlsx"                                                # link the database

library(tidyverse)
library(readxl)
library(lubridate)
library(geosphere)
library(data.table)


source("Data definition.R")
source("Get_Production.R")
source("Get_Province.R")
#source("Get_Production_Monthly.R")
source("Inputs.R")
source("Get_Impacts.R")


Main = function(input_demand, input_legume, input_code, input_time, input_production_mode, input_transformation){
  #==========================
  # 1. GET THE IDs
  #==========================
  # yealds :
  yealds_id <- (impacts %>% filter(name == "Rendements"))$id_impact
  
  # legumes
  input_legume_id <- (legumes%>% filter(name == input_legume))$id_legume
  
  # production modes
  input_production_mode_id <- (production_modes %>% filter(name == input_production_mode))$id_mode
  
  # transformations
  input_transformation_id <- (transformation %>% filter(transformation == input_transformation))$id_transformation
  
  # product
  input_product_id <- (products %>% filter(
                                id_mode == input_production_mode_id,
                                id_transformation == input_transformation_id,
                                id_legume == input_legume_id))$id_product
  
  input_time_id <- which(months == input_time)
  #==========================
  # 2. GET THE TIMES
  #==========================
  # Some manipulation needed if for ex time start in november (11) and ends in february (2) -> length = (12-11) + 2
  
  # harvest
  harvest_start <- (legumes %>% filter(id_legume == input_legume_id))$"harvest start"
  harvest_stop <- (legumes %>% filter(id_legume == input_legume_id))$"harvest stop"
  if (harvest_stop < harvest_start){
    harvest_time <- seq(harvest_start,12,by=1)
    harvest_time <- c(harvest_stop,seq(1,harvest_stop,by=1))
  }else{harvest_time <- seq(harvest_start,harvest_stop,by=1)}
  
  # conservation
  conservation_start <- (legumes %>% filter(id_legume == input_legume_id))$"conservation start"
  conservation_stop <- (legumes %>% filter(id_legume == input_legume_id))$"conservation stop"
  if (conservation_stop < conservation_start){
    conservation_time <- seq(conservation_start,12,by=1)
    conservation_time <- c(conservation_time,seq(1,conservation_stop,by=1))
  }else{conservation_time <- seq(conservation_start,conservation_stop,by=1)}
  
  #==========================
  # 3. PREPARATION
  #==========================
  
  # Errors
  if(length(input_product_id) == 0){return("No data :(")}
  test = GetProvince(input_code)
  if(length(test) == 0){return("Enter a valid postal code")}
  
  province_order <- GetProvince(input_code)[[1]] # Get the order of the provinces
  Our_location = GetProvince(input_code)[[2]]    # Name of the localization (ex : "Louvain-la-Neuve")

  # various
  temp_province_order <- 1  # take the value of the province during the iteration
  end = FALSE               # if end is true, while loop ends
  total_offer <- 0          # total_offer initialization
  status <- NULL            # will save the status of the production
  
  #input_localisation <- province_order[temp_province_order,1]
  #input_localisation_name = (provinces %>% filter(id_province == input_localisation))$name
  
  # Request recap :
  #print(paste0("Resquest recap ; searching for ", input_demand, 
  #             " t of ", input_production_mode,
  #             " ", input_transformation,
  #             " ", input_legume, 
  #             " in ", province_order[1,3], 
  #             " in ", input_time))
    
  #===================
  # 4. GET THE PRODUCTION
  #===================
    
  # 1. Case of transformed product : get monthly production
  if (input_transformation_id == 1){ 
    offer <- get_production_transform(input_product_id,
                                      yealds_id, 
                                      input_demand, 
                                      province_order)
  }
    
  # 2. Case of fresh product : divide production by harvest and conservation time
  else if (input_transformation_id == 2){
    
    # 2.1 It is the season
    if(input_time_id %in% harvest_time){
      #print("It is still the season.")                                 
      status <- "It is still the season."
      offer <- get_production_fresh(input_product_id, 
                                    yealds_id, 
                                    input_demand, 
                                    province_order,
                                    harvest_time,
                                    conservation_time)
    
    # 2.2 Not hte season but stocks
    }else if (input_time_id %in% conservation_time){
      #print("It is not the season anymore but there is conservation stocks.")
      status <- "It is not the season anymore but there is conservation stocks."
      offer <- get_production_fresh(input_product_id, 
                                    yealds_id, 
                                    input_demand, 
                                    province_order,
                                    harvest_time,
                                    conservation_time)
    
    # 2.3 Not the season and no stocks
    }else{
      #print("It is not the season anymore and there is no more conserved stocks.")}
      status <- "It is not the season anymore and there is no more conserved stocks."
    }

  
  # 3. Outputs
    impact_final <- get_impacts(input_product_id,input_demand)
    output <- list(offer, impact_final)
  return(output)
  }
}

#test <- Main(input_demand, input_legume, input_code, input_time, input_production_mode, input_transformation)
# pour acceder à l'offre : test[[1]]
# pour acceder aux impacts : test[[2]]

