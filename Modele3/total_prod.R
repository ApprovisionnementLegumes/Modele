for(input_legume in legumes$name){
  input_legume_id <- (legumes%>% 
                        filter(name == input_legume))$id_legume
  id_product_list = (repartition_modes%>%filter(id_legume == input_legume_id))$id_product
  yealds_id <- (impacts %>% 
                  filter(name == "Rendements"))$id_impact
  prod_list = vector()
  for(input_product_id in id_product_list){
    prod = get_production(input_product_id, 2, yealds_id)
    prod_list = append(prod_list, prod)
    
  }
  
  total = sum(prod_list)
  print(prod_list)
  total_list = append(total_list, total)