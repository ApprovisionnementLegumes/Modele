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


########################################
# Inputs
########################################

input_demand <- 100000
input_product <- "carrot"
input_lat <- 50
input_lon <- 5
input_time <- "december"
input_production_mode <- "organic"



########################################
# RUN THEN MODEL
########################################

#Find nearest province
province_order <- GetProvince(input_lat,input_lon)


#Find production : Make a loop ; while the production is not enough, we search in other provinces

print(paste0("Resquest recap ; searching for ", input_demand, 
             " of ", input_production_mode,
             " ", input_product, 
             " in ", province_order[1,1], 
             " in ", input_time))

# 1. Initial variables definition
end = FALSE
total_offer <- 0
temp_province_order <- 1

# 2. loop
while (end == FALSE)
{ 
  input_localisation <- province_order[temp_province_order,1]
  print(paste0("Searching in ", input_localisation))
  
  offer <- get_production(product = input_product, time = input_time, locality = input_localisation, mode = input_production_mode)
  total_offer <- total_offer + offer
  
  if(offer > input_demand)
    {
    
    print(paste0("There is enough in ", input_localisation, 
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
      print(paste0("There is not enough in ", input_localisation, 
                   ", let's search in neighboring provinces."))
      }
    
    temp_province_order <- temp_province_order + 1
    }
}




#TEST








