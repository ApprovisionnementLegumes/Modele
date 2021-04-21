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


Main = function(input_demand, input_legume, input_code, input_time, input_production_mode, input_transformation){
  
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
                                id_legume == input_legume_id))$id_product
  
  if(length(input_product_id) == 0){return("No data :(")}
  test = GetProvince(input_code)
  if(length(test) == 0){return("Enter a valid postal code fdp")}
  province_order <- GetProvince(input_code)[[1]]
  Our_location = GetProvince(input_code)[[2]]
  temp_province_order <- 1
  # various
  end = FALSE
  total_offer <- 0
  
  while (end == FALSE)
  { 
    input_localisation <- province_order[temp_province_order,1]
    input_localisation_name = (provinces %>% filter(id_province == input_localisation))$name
    #print(paste0("Searching in ", input_localisation_name))
    
    offer <- get_production(input_product_id,
                            input_localisation,
                            yealds_id
    )
    
    #offer <- get_production_monthly(input_legume_id,offer)
    total_offer <- total_offer + offer
    
    if(total_offer > input_demand)
    {
      end = TRUE
      output_list = list(input_localisation_name, total_offer, input_demand)
      return(paste0("There is enough in ", input_localisation_name, 
                    ". Current offer is ",total_offer,
                    "t while the demand is ",input_demand,"t."))
    }
    
    else
    {
      if (temp_province_order >= length(province_order[,1]))
      {
        end = TRUE
        return("There is not enough production in Wallonia for your request.")
      }
      else
      {
        #print(paste0("There is not enough in ", input_localisation_name, 
        #            ", let's search in neighboring provinces."))
      }
      
      temp_province_order <- temp_province_order + 1
    }
  }
  
}

Main(input_demand, input_legume, input_code, input_time, input_production_mode, input_transformation)

########################################
# Get Impacts
########################################
#impact_output <- get_impacts(input_product_id, total_offer, input_province_id)
