get_production <- function(input_legume_id, input_province_id){
  
  # look to the specific surface of the product in the province
  specific_surface <- (surfaces %>% filter(id_legume == input_legume_id,
                                           id_province == input_province_id))$surface
  
  # look to the specific yield of the product (in T/ha)
  specific_yield <- (production_impacts %>% filter(id_product == input_product_id,
                                                  id_impact == yealds_id))$value
  
  # Calculate the production
  specific_production <- specific_surface*specific_yield

  return(specific_production)
}
