######################################################
#Mapping of occurrences in Parana State
#####################################################

#Clean R Memory:
rm(list = ls())

#Packages:

library(sf)
library(sp)
library(raster)
library(dplyr)
library(beepr)
library(ggplot2)
library(tools)
library(ggspatial)

#Work Path:

setwd("D:/Parana-map")


##Raster Model (worldClim raster cut to South America - 5arcmnin):

raster_model <-raster("wc2.1_5m_tmin_01-SA.tif")
raster_model
plot(raster_model)

#Create a empty raster:  

empty_raster <- raster(raster_model)

empty_raster 

##Coordinate data:  

occ <- read.csv("lista-parana-splink.csv", sep = ";")

##Check point distribution: 
ggplot(occ, aes(x=longitude, y=latitude)) +
geom_point()

##Remove NA values: 
occ <- na.omit(occ)


##Make a raster for each species in table: 

species <- unique(occ$scientificname)

for (i in species){  #For each species: 
  
  #Filter species data:
  
  data <- filter(occ, scientificname == i)
  colnames(data) <- c("specie", "longitude", "latitude")
  
  #Create a dataframe with coordinate data:
  
  occ.points <-data.frame(data$longitude, data$latitude)
  
  #Make a spatialdata object: 
  
  spatial.point <- SpatialPointsDataFrame(occ.points, data, 
                                          proj4string = CRS(as.character(NA)), match.ID = TRUE)
  
  plot(spatial.point)
  
  ###Rasterize data:
  
  new_raster <-rasterize(spatial.point,
                         empty_raster,
                         field=1,
                         fun='count', 
                         background=0) 
  
  ##Change raster values for zero (ausence) or 1 (presence) in rater: 
  
  values(new_raster)[values(new_raster) > 0] <- 1  

  #Save raster in geotiff: 
  
  writeRaster(new_raster, filename = paste0('mapas_rasterizados/',i, ".tif"), overwrite=TRUE, format =  'GTiff')
  
}


#Make a unique raster with the sum of species occurrence in each pixel: 

setwd("D:/Parana-map/mapas_rasterizados")
tif <- dir(pattern = "tif$")
tif

ocorrencias <- stack(tif)


soma <- calc(ocorrencias, sum)

##Save geotiff:

writeRaster(soma, filename = 'D:/Parana-map/raster_ocorrencias-america-do-sul-5arcmin.tif', format =  'GTiff', overwrite = TRUE)

##Cut Raster to Parana state: 

parana <- st_read("D:/Parana-map/PR_UF_2022/PR_UF_2022.shp")
plot(parana)

ocorrencias_parana <- crop(x = soma, y = parana)
ocorrencias_parana <- mask(ocorrencias_parana, parana)

#Check:

ocorrencias_parana
plot(ocorrencias_parana)

#Save: 
writeRaster(ocorrencias_parana, filename = 'D:/Parana-map/raster_ocorrencias-Parana-5arcmin.tif', format =  'GTiff')

ocorrencias_parana <- raster("raster_ocorrencias-Parana-5arcmin.tif")

##Plot Map:  

##Federal protected areas in Brazil:  

UC <- st_read("D:/Parana-map/LimiteUCsFederais_26122023_a/LimiteUCsFederais_26122023_a.shp")

UC$UFAbrang

UC.PARANA <- UC[UC$UFAbrang == "PR",]

UC.PARANA$NomeUC

plot(UC.PARANA)

mapa_df <- as.data.frame(ocorrencias_parana, xy = TRUE)

write.csv(mapa_df, "df_mapa.csv")


mapa_df$layer <- gsub(0, NA, mapa_df$layer)
mapa_df <-na.omit(mapa_df)
mapa_df$layer <- as.integer(mapa_df$layer)
summary(mapa_df$layer)
unique(mapa_df$layer)

#Classifying the number of occurrences in each pixel: 

mapa_df$layer[mapa_df$layer %in% c(2,3,4,5,6)] <- "2-6"
mapa_df$layer[mapa_df$layer %in% c(7,8,9,10,11)] <- "7-11"
mapa_df$layer[mapa_df$layer %in% c(12,13,14,15,16)] <- "12-16"
mapa_df$layer[mapa_df$layer %in% c(17,18,19,20,21)] <- "17-21"
mapa_df$layer[mapa_df$layer %in% c(22,23,35)] <- ">21"

unique(mapa_df$layer)

##Define color map: 

mapa_df$layer <- factor(mapa_df$layer, levels=c("1", "2-6", "7-11","12-16","17-21",">21"))

colors = c(   "1" = "white",   
            "2-6" = "#00ffff", 
           "7-11" = "#00ff00", #ffff00
          "12-16" = "#ffff00", #ff8000
          "17-21" = "#ff8000", #ff0000
            ">21" = "#ff0000") #950101


##Plot: 
ggplot() +
  geom_sf(data = parana, fill = "gray", colour = "black") +
  geom_tile(data = mapa_df, aes(x, y, fill = layer), color = "black") +
  geom_sf(data = UC.PARANA, fill=NA, color="black") +
  scale_fill_manual(values = colors) +
  labs(x = "Longitude", y = "Latitude", fill = "
       Number of \n species per cell") +
  #coord_sf(ylim = c(-27,-22)) +
  annotation_scale(location = "br", width_hint = .3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0, "cm"), 
                         pad_y = unit(.8, "cm"),
                         style = north_arrow_fancy_orienteering) +
  
  theme_bw() +
  theme(axis.title = element_text(size = 15, face = "plain"),
        legend.background = element_rect(fill= NA, 
                                        linetype="solid"),
        legend.position = c(.90, .822),
        legend.justification = c("center"),
        legend.title.align=0)



ggsave(width = 20, height = 20, un = "cm", dpi = 300, filename = "D:/Parana-map/mapa-ocorrencia-splink-5arcmin2.jpeg")

#######################################################