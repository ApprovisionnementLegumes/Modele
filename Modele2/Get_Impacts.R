get_impacts <- function(id_product,production,surface){
  
  list_impact = data.frame(name = c(), value = c(), units = c())
  
  for (impact_id in impacts[,1]){
    
    impact_name <- (impacts %>% filter(id_impact == impact_id))$name
    impact_units1 <- (impacts %>% filter(id_impact == impact_id))$units1
    impact_units2 <- (impacts %>% filter(id_impact == impact_id))$units2 
    impact_value <- (production_impacts %>% filter(id_impact == impact_id,
                                                   id_product == input_product_id))$value
    
    
    print(paste0("Impact on ", impact_name))
    
    #IMPLEMENTER LES POURCENTAGES???
    
    if (impact_units2 == "kg")
      {
      impact_value_final <- impact_value*production/1000 #divide by 1000 because production is in t
      }
    else if (impact_units2 == "ha")
      {
      impact_value_final <- impact_value*surface
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


impact_id = 1
production = 100
surface = 100
print(get_impacts(3, 100, 120))
