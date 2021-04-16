# need localisation as a list of coordinates
GetProvince = function(lon, lat){
  #file = 'base de donnees brouillon projet 1230.xlsx'
  #provinces = read_excel(file, sheet = 'provinces')
  province_vector = vector()
  distances = vector()
  
  for (i in seq(from = 1, to = nrow(provinces), by = 1)){
    prov = provinces[i,][['id_province']]
    lon1 = lon
    lat1 = lat
    lon2 = provinces[i,][['longitude']]
    lat2 = provinces[i,][['latitude']]
    dist = distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
    
    province_vector = append(province_vector, prov)
    distances = append(distances, dist)  
  }
  df = data.frame()
  df = cbind(data.frame(province_vector), data.frame(distances))
  df = df[order(df$distances),]
  return(df)
}
