get_impacts <-
  function(input_product_id,
           production) 
  {
    # TEST
    # input_product_id = 15
    # production = 200
    
    
    
    id_surface = (impacts %>% filter(name == "Surface de terre"))$id_impact
    
    # calculate the surface
    specific_surface <-
      production * ((
        production_impacts %>% filter(id_product == input_product_id,
                                      id_impact == id_surface)
      )$value) 
    # dans la DB, la surface par qtt est en ha*10-3 par kilo. 
    # En multipliant par la demande en tonnes, 
    # on obtient directement la surface spécifique en ha
    
    # print(paste0("Specific surface : ",specific_surface," ha"))
    
    list_impact = data.frame(name = c(),
                             value = c(),
                             units = c(),
                             incertitude = c(),
                             value_base = c()
    )
    
    for (impact_id in impacts[[1]]) {                              # !!!! si les id changent (pas très optimal mais j'ai pas réussi à transformer une colonne de df en liste)
      
      #===========================================
      # EXCEPTION SI IMPACT + RENDEMENT OU SURFACE
      #===========================================
      
      #if(impact_id == (impacts %>% filter(name == "Surface de terre"))$id_impact){
      # do nothing
      #}
      
      #print(impact_id)
      
      if(impact_id == (impacts %>% filter(name == "Rendements"))$id_impact){
        # do nothing
      }
      
      #===========================================
      # CALCUL DES IMPACTS
      #===========================================
      
      else{
        #print(impact_id)
        impact_value_final <- 0
        
        # NAME
        impact_name <- (impacts %>% filter(id_impact == impact_id))$name
        
        # UNITS
        impact_units1 <-
          (impacts %>% filter(id_impact == impact_id))$units1
        
        impact_units2 <-
          (impacts %>% filter(id_impact == impact_id))$units2
        
        # VALUE
        impact_value <-
          (
            production_impacts %>% filter(id_impact == impact_id,
                                          id_product == input_product_id)
          )$value
        
        #=============================================
        # 1. Cas ou l'impact est en unité par kilo
        
        if (impact_units2 == "kg")
        {
          impact_value_final <- impact_value * production * 1000 #multiply by 1000 because production is in t
          
          
        }
        
        #=============================================
        # 2. Cas ou l'impact est en unité par hectare
        
        else if (impact_units2 == "ha")
        {
          impact_value_final <- impact_value * specific_surface
        }
        
        #=============================================
        # 3. Cas ou l'impact est en unité par kilo par jour
        else if (impact_units2 == "kg.jour")  # cas du stockage
        {
          impact_value_final <- 0.00122 * production * 1000                       # Valeur rentrée mannuellement. Automatiser?
          impact_value <- 0.00122
          
        }
        
        
        #=============================================
        # EXCEPTIONS
        #=============================================
        else{
          if (impact_name == "DQR"){
            next
          }
          
          else{print(paste0("Error : impact not found. Impact ID is : ",impact_id))}
        }
        
        #=============================================
        # Encodage et sauvegarde
        #=============================================
        temp <- data.frame(name = impact_name,
                           value = impact_value_final,
                           units = impact_units1,
                           incertitude = (impacts %>% filter(name == impact_name))$incertitude,
                           value_base = impact_value)
        
        temp$value = as.integer(temp$value)
        
        if(impact_name == "Global score"){
          DQR_value = (impact_data %>% filter(name == "DQR", id_product == input_product_id))$value
          temp$incertitude = DQR_value
        }
        
        list_impact <- rbind(list_impact, temp)
        
        
        
        
        
        #print(paste0("The impact on ",impact_name," is ",impact_value_final," ",impact_units1))
        
      }
      
      
    }
    
    
    return(list_impact)
  }

Get_Means = function(){
  
  #============================
  # MODES
  #============================
  
  impact_means <- data.frame(id_impact = c(),
                             mean = c())
  
  for (i in impacts[[1]]){  # car dans impact il y a aussi le stockage
    #print(i)
    #print("==================")
    #print((impacts %>% filter(id_impact==i))$name)
    #print("==================")
    
    
    if(i == (impacts %>% filter(name == "DQR"))$id_impact){
      next
    }
    
    temp_means_vec = NULL
    
    
    temp_mean = mean((impact_data %>% filter(id_impact == i))[[3]])
    
    if(i == (impacts %>% filter(name == "Stockage"))$id_impact){
      temp_mean = 0.00122                                                       # car la value de Stockage ne change jamais
    }
    
    temp_vec = data.frame(id_impact = i,
                          mean = temp_mean)
    
    
    
    if(i == (impacts %>% filter(name == "DQR"))$id_impact){
      next
    }
    
    if(i == (impacts %>% filter(name == "Rendements"))$id_impact){
      next
    }
    
    impact_means = rbind(impact_means, temp_vec)
    
  }
  
  return(impact_means)
  
}