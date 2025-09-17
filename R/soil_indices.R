library(geobr)
library(terra)
library(tidyverse)
library(sf)

cities <- read_municipality()
cities <- st_transform(cities,"EPSG:4326")
dataset <- st_drop_geometry(cities)[,c("code_muni","name_muni")]


ref <- rast(list.files(pattern = "soil")[1])
lapply(list.files(pattern = "soil"), function(file){
  r <- rast(file)
  r <- resample(r,ref)
  r
}) -> soil_data

v_cities <- vect(cities)

r <- rast(soil_data)
r[r==0] <- NA

elev <- rast("C:/Projetos/12_Insurance/DEM_GLO30_BRA_5km.tif")

extracted_values <- terra::extract(r, v_cities, fun = mean, na.rm = TRUE)
extracted_elev <- terra::extract(elev, v_cities, fun = mean, na.rm = TRUE)

dataset <- cbind(st_drop_geometry(cities)[,c('code_muni','name_muni')], extracted_values)
dataset <- merge(dataset,extracted_elev,by='ID')
colnames(dataset) <- c("ID","code_muni","name_muni","bdod",
                       "cec","cfvo","clay","nitrogen",
                       "ocd","phh20","sand","silt",
                       "soc","elev")
sf_use_s2(FALSE)
dataset$lat <- st_coordinates(st_centroid(cities$geom))[,2]

write.csv(dataset,"C:\\Projetos\\17_index_insurance\\soil_indices.csv",row.names=F)
