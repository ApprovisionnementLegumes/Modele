get_production_monthly <- function(input_legume_id, production){
  
  harvest_time = ((legumes %>% filter(id_legume == input_legume_id))$'harvest stop'
                  - (legumes %>% filter(id_legume == input_legume_id))$'harvest start')
  
  production_monthly = production/harvest_time
  
  return(production_monthly)
}