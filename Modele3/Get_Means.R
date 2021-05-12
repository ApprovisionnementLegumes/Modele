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

Get_Means()


