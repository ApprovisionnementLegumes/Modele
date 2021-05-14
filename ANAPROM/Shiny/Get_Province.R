GetProvince = function(input_code){
  file = '/srv/connect/apps/Modele2/Database projet.xlsx'
  #file = "/Users/charles/Documents/projet_disc/Modele/Modele2/Database projet.xlsx"
  provinces = read_excel(file, sheet = 'provinces')
  file = '/srv/connect/apps/Modele2/code-postaux-belge.csv'
  #file = "/Users/charles/Documents/projet_disc/Modele/Modele2/code-postaux-belge.csv"
  df = read_csv2(file)
  province_vector = vector()
  distances = vector()
  names = vector()
  test = df%>%filter(Code == input_code)
  if(length(test$Code) == 0){return(numeric(0))}
  
  lon1 = (df%>%filter(Code == input_code))$Longitude[[1]]
  lat1 = (df%>%filter(Code == input_code))$Latitude[[1]]
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
GetProvince(1348)