get_production <- function(product, time, locality, mode){
  time_num <- which(months == time) # the month of interest, in number
  
  # get the specifi production for the given product
  specific_production <- production_data %>% 
    filter(name_product == product)
  
  if(nrow(specific_production) == 0){
    print("Sorry no data for this product")
    return(0) # get out of the function
  }
  
  # Get the specific production for a given production mode
  specific_production <- specific_production %>% 
    filter(name_mode == mode)
  
  if(nrow(specific_production) == 0){
    print("Sorry no production for that mode of production in the database")
    return(0) # get out of the function
  }
  
  # Get the specific production for a place
  specific_production <- specific_production %>% 
    filter(name_province == locality)
  
  if(nrow(specific_production) == 0){
    print("Sorry no production for that locality in the database")
    return(0) # get out of the function
  }
  
  
  # Get the specific production for a given time
  specific_production <- specific_production %>% 
    filter(start_harvest <= time_num & stop_harvest >= time_num)
  
  if(nrow(specific_production) == 0){
    print("Sorry no production for that date")
    return(0) # get out of the function
  }
  
  return(specific_production$monthly_quantity)
}



########################################
# TESTING ERRORS
########################################

#get_production(product = "potato", time = input_time, locality = "bw", mode = input_production_mode)
#get_production(product = input_product, time = "march", locality = "bw", mode = input_production_mode)
#get_production(product = input_product, time = input_time, locality = "bw", mode = "rationned")
#get_production(product = input_product, time = input_time, locality = "bxl", mode = input_production_mode)