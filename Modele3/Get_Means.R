Get_Means = function(){

  #============================
  # MODES
  #============================
  
  impact_means <- data.frame(id_impact = c(), 
                             id_mode = c(), 
                             mean = c())
  
  for (j in impacts[1:14,]$id_impact){  # car dans impact il y a aussi le stockage
     
    # print("==================")
    # print((impacts %>% filter(id_impact==j))$name)
    # print("==================")
    
    temp_means_vec = NULL
    
    
    # pour chaque impact
    for (i in production_modes$id_mode){ 
      
      temp = NULL
      
      # print((production_modes %>% filter(id_mode == i))$name)
      
      # calcule la moyenne
      
      temp_means = mean((impact_data %>% filter(id_mode == i,
                                               id_impact == j))$value)
      
      temp = data.frame(id_impact = j, 
                        id_mode = i, 
                        mean = temp_means)
      
      temp_means_vec = rbind(temp_means_vec, temp)
      
    }
    
    impact_means = rbind(impact_means,temp_means_vec)
    
  }
  
  #print(impact_means)
  
  #============================
  # TRANSFORMATIONS
  #============================
  
  impact_means2 <- data.frame(id_impact = c(), 
                             id_transformation = c(), 
                             mean = c())
  
  for (j in impacts[1:14,]$id_impact){  # car dans impact il y a aussi le stockage
    
    # print("==================")
    # print((impacts %>% filter(id_impact==j))$name)
    # print("==================")
    
    temp_means_vec = NULL
    
    
    # pour chaque impact
    for (i in transformation$id_transformation){ 
      
      temp = NULL
      
      # print((production_modes %>% filter(id_mode == i))$name)
      
      # calcule la moyenne
      
      temp_means = mean((impact_data %>% filter(id_transformation == i,
                                                id_impact == j))$value)
      
      temp = data.frame(id_impact = j, 
                        id_transformation = i, 
                        mean = temp_means)
      
      temp_means_vec = rbind(temp_means_vec, temp)
      
    }
    
    impact_means2 = rbind(impact_means2,temp_means_vec)
    
  }
  
  #print(impact_means2)
  
  return(list(impact_means,impact_means2))
}

# Get_Means()
# -> retourne une liste dont [[1]] est la moyenne des valeurs des impacts par mode et
# [[2]] est la moyenne des valeurs des impacts par transformation
