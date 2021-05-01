#==============================
# GET PRODUCTION BASIC FUNCTION
#==============================

get_production <- function(input_product_id, input_localisation, yealds_id){
  #=============================================================
  # Return the production of a given product in a given province
  #=============================================================
  
  # look to the purcentage of surface attributed to the product
  # ex : there is 75% of the carrot surface attributed to the reasoned conventionnal agric.
  specific_purcent <- (repartition_modes %>% filter(id_product == input_product_id))$"%"
  
  # look for the legume of the product
  legume <- (products %>% filter(id_product == input_product_id))$id_legume
  
  # calculate the surface 
  specific_surface <- specific_purcent*(surfaces %>% filter(id_legume == legume,
                                           id_province == input_localisation))$surface
  
  
  # look to the specific yield of the product (in T/ha)
  specific_yield <- (production_impacts %>% filter(id_product == input_product_id,
                                                  id_impact == yealds_id))$value
  
  # Calculate the production
  specific_production <- specific_surface*specific_yield

  return(specific_production)
}


#==============================
# GET PRODUCTION FRESH
#==============================

get_production_fresh <- function(input_product_id, yealds_id, input_demand, province_order, harvest_time, conservation_time){
  
  temp_province_order <- 1  # take the value of the province during the iteration
  end = FALSE               # if end is true, while loop ends
  total_offer <- 0          # total_offer initialization
  status <- NULL
  
  list_production <- data.frame(province = c(),
                                production = c(),
                                status = c())
  
  while(end == FALSE){
    input_localisation <- province_order[temp_province_order,1]
    input_localisation_name = (provinces %>% filter(id_province == input_localisation))$name
    #print(paste0("Searching in ", input_localisation_name))
    
    offer <- get_production(input_product_id,
                            input_localisation,
                            yealds_id)
    offer <- offer/(length(harvest_time)+length(conservation_time))
    total_offer <- total_offer + offer
    
    if(total_offer >= input_demand)
    {
    #output_list = list(input_localisation_name, total_offer, input_demand)
    #print(paste0("There is enough in ", input_localisation_name, ". Current offer is ",total_offer,"t while the demand is ",input_demand,"t."))
    status <- paste0("There is enough in ", input_localisation_name)
    
    end = TRUE
    
    }
    
    else
    {
      if (temp_province_order >= length(province_order[,1]))
      {
        end = TRUE
        #print("There is not enough production in Wallonia for your request.")
        status <- "There is not enough production in Wallonia for your request."
      }
      else
      {
        #print(paste0("There is not enough in ", input_localisation_name,", let's search in neighboring provinces."))
        #print(paste0("demand : ", input_demand, " | total offer : ",total_offer))
        status <- paste0("There is not enough in ", input_localisation_name)
      
      }
      
      temp_province_order <- temp_province_order + 1
    }
    
    #==========================
    # Encodage & sauvegarde
    #==========================
    
    temp <- data.frame(name = input_localisation_name,
                       production = offer,
                       status = status)
    
    list_production <- rbind(list_production, temp)
    
  }
  return(list_production)
}

get_production_fresh(15,9,100,province_order, harvest_time, conservation_time)

#==============================
# GET PRODUCTION TRANSFORMED
#==============================

get_production_transform <- function(input_product_id, yealds_id, input_demand, province_order){
  
  temp_province_order <- 1  # take the value of the province during the iteration
  temp_status <- NULL
  end = FALSE               # if end is true, while loop ends
  total_offer <- 0          # total_offer initialization
  
  list_production <- data.frame(province = c(),
                                production = c(),
                                status = c())
  
  while(end == FALSE){
    input_localisation <- province_order[temp_province_order,1]
    input_localisation_name = (provinces %>% filter(id_province == input_localisation))$name
    #print(paste0("Searching in ", input_localisation_name))
    
    offer <- get_production(input_product_id,
                            input_localisation,
                            yealds_id)
    offer <- offer/12
    total_offer <- total_offer + offer
    
    if(total_offer > input_demand)
    {
      end = TRUE
      output_list = list(input_localisation_name, total_offer, input_demand)
      
      
      #print(paste0("There is enough in ", input_localisation_name, ". Current offer is ",total_offer,"t while the demand is ",input_demand,"t."))
      
      status <- paste0("There is enough in ", input_localisation_name)
      
    }
    
    else
    {
      if (temp_province_order >= length(province_order[,1]))
      {
        end = TRUE
        #print("There is not enough production in Wallonia for your request.")
        status <- "There is not enough production in Wallonia for your request."
        
        
      }
      else
      {
        #print(paste0("There is not enough in ", input_localisation_name,", let's search in neighboring provinces."))
        status <- paste0("There is not enough in ", input_localisation_name)
        
      }
      
      
      
      temp_province_order <- temp_province_order + 1
    }
    
    #==========================
    # Encodage & sauvegarde
    #==========================
    
    temp <- data.frame(name = input_localisation_name,
                       production = offer,
                       status = status)
    
    list_production <- rbind(list_production, temp)
    
  }
  return(list_production)
}

#get_production_transform(15,9,100,province_order)
