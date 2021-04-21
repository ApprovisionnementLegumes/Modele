########################################
# Load the libraries, functions & datas
########################################

library(tidyverse)
library(readxl)
library(lubridate)
library(geosphere)


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # Set working directory to the current file
source("Data definition.R")
source("Get_Production.R")
source("Get_Province.R")
source("Get_Production_Monthly.R")
source("Inputs.R")
source("Get_Impacts.R")


########################################
# Inputs transformation
########################################

# yealds :
yealds_id <- (impacts %>% 
                filter(name == "Rendements")
              )$id_impact

# legumes
input_legume_id <- (legumes%>% 
                      filter(name == input_legume)
                    )$id_legume

# production modes
input_production_mode_id <- (production_modes %>% 
                               filter(name == input_production_mode)
                             )$id_mode

# transformations
input_transformation_id <- (transformation %>% 
                              filter(transformation == input_transformation)
                            )$id_transformation

# product
input_product_id <- (repartition_modes %>% 
                       filter(id_mode == input_production_mode_id,
                              id_transformation == input_transformation_id,
                              id_legume == input_legume_id)
                     )$id_product

########################################
# Get Production
########################################
# 1. Variables definition

#Find nearest province
province_order <- GetProvince(input_lon, input_lat)
temp_province_order <- 1
# various
end = FALSE
total_offer <- 0

# 2. Loop
while (end == FALSE)
{ 
  input_localisation <- province_order[temp_province_order,1]
  input_localisation_name = (provinces %>% filter(id_province == input_localisation))$name
  print(paste0("Searching in ", input_localisation_name))
  
  offer <- get_production(input_product_id,
                          input_localisation)
  #print(offer)
  #offer <- get_production_monthly(input_legume_id,offer)
  
  total_offer <- total_offer + offer
  
  if(total_offer > input_demand)
  {
    
    print(paste0("There is enough in ", input_localisation_name, 
                 ". Current offer is ",total_offer,
                 " while the demand is ",input_demand))
    end = TRUE
  }
  
  else
  {
    if (temp_province_order >= length(province_order[,1]))
    {
      print("Sorry, there is still not enough production in Belgium for your request.")
      end = TRUE
    }
    else
    {
      print(paste0("There is not enough in ", input_localisation_name, 
                   ", let's search in neighboring provinces."))
    }
    
    temp_province_order <- temp_province_order + 1
  }
}

########################################
# Get Impacts
########################################
#impact_output <- get_impacts(input_product_id, total_offer, input_province_id)
