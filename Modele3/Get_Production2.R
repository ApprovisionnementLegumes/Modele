source("Main.R")

get_production_province <- function(input_product_id, input_time, input_province_id){
  
  input_production_mode_id <- (products %>% filter(id_product == input_product_id))$id_mode
  input_production_mode <- (production_modes %>% filter(id_mode == input_production_mode_id))$name
  
  input_transformation_id <- (products %>% filter(id_product == input_product_id))$id_transformation
  input_transformation <- (transformation %>% filter(id_transformation == input_transformation_id))$transformation
  
  months <- c("january", "february", "march", "april", "may", "june", "july", "august", "september", "october", "november", "december")
  input_legume_id <- (products %>% filter(id_product == input_product_id))$id_legume
  input_time_id <- which(months == input_time)
  
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
  
  if(input_transformation_id == 1){
    
    legume_id <- (products %>% filter(id_product == input_product_id))$id_legume
    legume <- (legumes %>% filter(id_legume == legume_id))$name
    demand = 100000000000000000
    variable <- Main(demand, legume, input_code, input_time, input_production_mode, input_transformation)
    variable <- variable[[1]]
    
    province <- (provinces %>% filter(id_province == input_province_id))$name
    variable <- (variable %>% filter(name == province))$production
  }
    
  else if (input_transformation_id == 2){
  
    if(input_time_id %in% harvest_time){
      legume_id <- (products %>% filter(id_product == input_product_id))$id_legume
      legume <- (legumes %>% filter(id_legume == legume_id))$name
      demand = 100000000000000000
      variable <- Main(demand, legume, input_code, input_time, input_production_mode, input_transformation)
      variable <- variable[[1]]
      
      province <- (provinces %>% filter(id_province == input_province_id))$name
      variable <- (variable %>% filter(name == province))$production
    }
    
    else if(input_time_id %in% conservation_time){
      legume_id <- (products %>% filter(id_product == input_product_id))$id_legume
      legume <- (legumes %>% filter(id_legume == legume_id))$name
      demand = 100000000000000000
      variable <- Main(demand, legume, input_code, input_time, input_production_mode, input_transformation)
      variable <- variable[[1]]
      
      province <- (provinces %>% filter(id_province == input_province_id))$name
      variable <- (variable %>% filter(name == province))$production
    }
    
    else{
      variable <-0
    }
  }
    
  else{
    variable <- NULL
  }
  
  return(variable)
}

get_production_province(1,"december",2)

