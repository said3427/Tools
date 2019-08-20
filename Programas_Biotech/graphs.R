#install.packages("devtools")
#devtools::install_github("diegovalle/mxmaps",host = "https://api.github.com")
#devtools::install_github("sjmgarnier/viridis")

library("mxmaps")
library("viridis")
library("scales")

data("df_mxmunicipio")

data(mxstate.map, package = "mxmaps")
data(mxmunicipio.map, package = "mxmaps")



data<-data.frame(read.csv("Data/ProgramasUniversitarios.csv"))
data$value<-data$Licenciatura+data$Posgrados

df_mxstate$value<-0

df_mxstate$value[match(data$Entidad,df_mxstate$state_name)]<-data$value

mxstate_choropleth(df_mxstate,title="Programas educativos relacionados con biotecnología en México, 2015")


df <- data.frame(matrix(unlist(out), nrow=27, byrow=F))

tartu_map_g_str <-    get_map(location = 'Mexico', zoom=4)      
                 
map <- get_map(location = 'Mexico', zoom=4)
mapPoints <- ggmap(map) + geom_point(aes(x = lon, y = lat), data = df , alpha = .5)



ggmap(tartu_map_g_str, extent = "device") + geom_density2d(data = tartu_housing_xy_wgs84_a, 
                                                           aes(x = lon, y = lat), size = .5) + stat_density2d(data = tartu_housing_xy_wgs84_a, 
                                                                                                               aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                               bins = 8, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.5), guide = FALSE)