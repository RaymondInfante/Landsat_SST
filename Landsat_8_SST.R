library(tidyverse)
library(terra)
library(sf)
library(glue)

band_10_list_long <- list.files("10_band", full.names = T)
band_11_list_long <- list.files("11_band", full.names = T)

#Shapefile from Parguera
s <- st_read("Shape_File/Landsat_SST.shp")

#Process the Band 10
read_tiff_10 <- function(FileName){
  print(glue("printing item {FileName})"))
  r <- rast(FileName)
  Radiance_10 <- (0.0003342*r)+0.1
  Kelvin_10 <- (1321.08)/log(774.89/Radiance_10+1)
  Celcius_10 <- Kelvin_10 - 273.15
  p <- project(Celcius_10, "EPSG:4326")
  x <- crop(p,s)
  writeRaster(x, FileName, overwrite=TRUE)
}


#Process the Band 11
read_tiff_11 <- function(FileName){
  print(glue("printing item {FileName})"))
  r <- rast(FileName)
  Radiance_11 <- (0.0003342*r)+0.1
  Kelvin_11 <- (1201.14)/log(480.89/Radiance_11+1)
  Celcius_11 <- Kelvin_11 - 273.15
  p <- project(Celcius_11, "EPSG:4326")
  x <- crop(p,s)
  writeRaster(x, FileName, overwrite=TRUE)
}

#
map(band_10_list_long,read_tiff_10)
map(band_11_list_long,read_tiff_11)



#SST Formula from the combination of files 
for(i in seq_along(1:47)){
  t1 <- rast(band_10_list_long[i])
  t2 <- rast(band_11_list_long[i])
  SST <- (0.9767 * t1) + 1.8362*(t1-t2) + 0.0699
  writeRaster(SST, paste0("SST/",i,".tif"), overwrite = T)
}

SST <- list.files("SST", full.names = T)

for(i in SST){
  SST_I <- rast(i)
  df <- as.data.frame(SST_I, xy=TRUE)
  p <- ggplot(df)+
  geom_raster(aes(x = df[,1], y = df[,2], fill =df[,3]))+
  scale_fill_viridis_c(name ="Temperature (°C)", option = "turbo")+
  labs(x = "Longitude", y = "Latitude")+
  theme(plot.background = element_blank(),
        panel.background = element_blank())
  ggsave(paste0("Pictures/",i,".jpeg"))  
}


 
