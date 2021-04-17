get_production <- function(input_product_id, input_province_id){
  
  # look to the purcentage of surface attributed to the product
  # ex : there is 75% of the carrot surface attributed to the reasoned conventionnal agric.
  specific_purcent <- (repartition_modes %>% filter(id_product == input_product_id))$"%"
  
  # look for the legume of the product
  legume <- (repartition_modes %>% filter(id_product == input_product_id))$id_legume
  
  # calculate the surface 
  specific_surface <- specific_purcent*(surfaces %>% filter(id_legume == legume,
                                           id_province == input_province_id))$surface
  
  
  # look to the specific yield of the product (in T/ha)
  specific_yield <- (production_impacts %>% filter(id_product == input_product_id,
                                                  id_impact == yealds_id))$value
  
  # Calculate the production
  specific_production <- specific_surface*specific_yield

  return(specific_production)
}
