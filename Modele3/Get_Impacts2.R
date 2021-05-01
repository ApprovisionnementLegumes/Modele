#get impact 2

#stockage = 0,00122


list_impact = data.frame(name = c(), value = c(), units = c())
for (impact_id in impacts[,1]){
  
  impact_name <- (impacts %>% filter(id_impact == impact_id))$name
}
