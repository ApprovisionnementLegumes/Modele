get_impacts <- function(input_product_id,production, input_province_id){
  
  # look to the purcentage of surface attributed to the product
  # ex : there is 75% of the carrot surface attributed to the reasoned conventionnal agric.
  specific_purcent <- (repartition_modes %>% filter(id_product == input_product_id))$"%"
  
  # look for the legume of the product
  legume <- (repartition_modes %>% filter(id_product == input_product_id))$id_legume
  
  # calculate the surface 
  specific_surface <- specific_purcent*(surfaces %>% filter(id_legume == legume,
                                                            id_province == input_province_id))$surface
  
  list_impact = data.frame(name = c(), value = c(), units = c())
  
  for (impact_id in impacts[,1]){
    
    impact_name <- (impacts %>% filter(id_impact == impact_id))$name
    impact_units1 <- (impacts %>% filter(id_impact == impact_id))$units1
    impact_units2 <- (impacts %>% filter(id_impact == impact_id))$units2 
    impact_value <- (production_impacts %>% filter(id_impact == impact_id,
                                                   id_product == input_product_id))$value
    
    
    print(paste0("Impact on ", impact_name))
    
    if (impact_units2 == "kg")
      {
      impact_value_final <- impact_value*production/1000 #divide by 1000 because production is in t
      }
    else if (impact_units2 == "ha")
      {
      impact_value_final <- impact_value*specific_surface
      }
    else{
      print("Error, units not found")
    }
    
    temp <- data.frame(name = impact_name, value = impact_value_final, units = impact_units1)
    list_impact <- rbind(list_impact, temp)
    print(paste0("The impact on ", impact_name," is ", impact_value_final, " ", impact_units1))
    
  }
  
  
  return(list_impact)
}


production = 100
print(get_impacts(3, 100, 1))
