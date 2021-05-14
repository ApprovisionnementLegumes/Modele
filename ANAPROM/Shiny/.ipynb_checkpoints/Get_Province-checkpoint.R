GetProvince = function(input_code){
  file = 'Database projet v2.xlsx'
  provinces = read_excel(file, sheet = 'provinces')
  file = 'code-postaux-belge.csv'
  df = read_csv2(file)
  province_vector = vector()
  distances = vector()
  names = vector()
  lon1 = (df%>%filter(Code == input_code))$Longitude
  lat1 = (df%>%filter(Code == input_code))$Latitude
  localite = (df%>%filter(Code == input_code))$Localite
  
  for (i in seq(from = 1, to = nrow(provinces), by = 1)){
    prov = provinces[i,][['id_province']]
    lon2 = provinces[i,][['longitude']]
    lat2 = provinces[i,][['latitude']]
    name = provinces[i,][['name']]
    dist = distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
    
    province_vector = append(province_vector, prov)
    distances = append(distances, dist)  
    names = append(names, name)
  }
  df = data.frame()
  df = cbind(data.frame(province_vector), data.frame(distances), data.frame(names))
  df = df[order(df$distances),]
  output_list = list(df, localite)
  return(output_list)
}