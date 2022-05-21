## ---------------------------
## Script name: Population Mapping - Diversity Data Kids Maps
## Purpose of script: Getting Population Data Represented on Map
## Author: Karen Mai
## Date Updated: 02/25/2022 
## Issues to Still Fix: Points Too Crowded, Unsure if all Polygons Mapped
## --------------------------- 

library(httr)
library(jsonlite)
library(dplyr) 
library(stringr) 
library(tigris)
library(sp) 
library(leaflet.extras) 
library(mapview) 
library(tidyr)
library(leaflet)
library(sf)

### Create datatable that we will be extracting info from ### 
resource_id <- "44ee1ea6-a3df-4af3-93b7-c4695d5ab6a6"
rows <- 150000
url <- paste0("https://data.diversitydatakids.org/datastore/odata3.0/", resource_id, "?$top=", rows, "&$format=json")
database <- GET(url)
database <- rawToChar(database$content)
database <- fromJSON(database)
database <- database$value
database <- subset(database, year == '2015')
database <- database %>% select(geoid, msaname15, countyfips, statefips, aian, api, black, hisp, white)
database$geo_id <- ('1400000US')
database$geoid <- str_c(database$geo_id,'',database$geoid)


### Getting the place we want to map: REPLACE WITH MANNING's WORK ### 
map_pop <-database[database$msaname15=="Montgomery, AL Metro Area",]
map_pop$countyfips<-(as.character(map_pop$countyfips))
map_pop$fullfips<-(as.character(map_pop$countyfips))
map_pop$countyfips<-substr(map_pop$countyfips,3,6)
shapes <- unique(map_pop$fullfips)
shapes <- data.frame(shapes)
shapes<-na.omit(shapes)
shapes$state <- substr(shapes$shapes,1,2)
shapes$county <-substr(shapes$shapes,3,6)
shapes_map <- data.frame()
for (x in 1:nrow(shapes)){
  shape <- tracts(state = shapes[x,2],county =  shapes[x,3], year = 2010, cb=T)
  shapes_map <- rbind(shapes_map,shape)
}
shapes_map <- shapes_map %>% select(GEO_ID, geometry)
colnames(shapes_map) <- c('geoid', 'geometry')
full_map <- merge(shapes_map, map_pop, by = 'geoid')

### Getting on one county at a time to map?. ###
all_tracts <- data.frame()
for (i in 1:nrow(full_map)) { 
  One_Tract <- full_map[i,] # only taking one county
  One_Tract_Map <- as(One_Tract, Class = "Spatial") # change from SP to SF
  total <- spsample(One_Tract_Map,One_Tract_Map$aian+One_Tract_Map$api+One_Tract_Map$black+One_Tract_Map$hisp+One_Tract_Map$white,type='random')
  races <- c('aian','api','black','hisp','white')
  
  plot(One_Tract_Map)
  for(j in 1:length(races)) { 
    if (races[j] == 'aian') { 
      points(total[0:One_Tract_Map$aian], col='#E66100', pch=19, cex=0.5)
      row = One_Tract_Map$aian
    }
    else if (races[j] == 'api') {
      row_end = row+One_Tract_Map$api
      points(total[row:row_end], col='#FF00CC', pch=19, cex=0.5)
      row = row_end
    }
    else if (races[j] == 'black') { 
      row_end = row+One_Tract_Map$black
      points(total[row:row_end], col='#E6A900', pch=19, cex=0.5)
      row = row_end
    }
    else if (races[j] == 'hisp') { 
      row_end = row+One_Tract_Map$hisp
      points(total[row:row_end], col='#53017F', pch=19, cex=0.5)
      row = row_end
    }
    else if (races[j] == 'white') {
      row_end = row+One_Tract_Map$white
      points(total[row:row_end], col='#339900', pch=19, cex=0.5)
      row = row_end
    }
  }
  
  Coordinates <- data.frame(total@coords)
  Coordinates$Color <- NA
  for(k in 1:length(races)) { 
    if (races[k] == 'aian') { 
      Coordinates$Color[0:One_Tract_Map$aian] <- '#E66100'
      row = One_Tract_Map$aian
    }
    else if (races[k] == 'api') {
      row_end = row+One_Tract_Map$api
      Coordinates$Color[row:row_end] <- '#FF00CC'
      row = row_end
    }
    else if (races[k] == 'black') { 
      row_end = row+One_Tract_Map$black
      Coordinates$Color[row:row_end] <- '#E6A900'
      row = row_end
    }
    else if (races[k] == 'hisp') { 
      row_end = row+One_Tract_Map$hisp
      Coordinates$Color[row:row_end] <- '#53017F'
      row = row_end
    }
    else if (races[k] == 'white') {
      row_end = row+One_Tract_Map$white
      Coordinates$Color[row:row_end] <- '#339900'
      row = row_end
    }
  }
  all_tracts <- rbind(all_tracts,Coordinates)
}


# Adding back on the chooreleeopath 
resource_id <- "080cfe52-90aa-4925-beaa-90efb04ab7fb"
rows <- 150000 # rounding up
url <- paste0(
  "https://data.diversitydatakids.org/datastore/odata3.0/",
  resource_id,
  "?$top=",
  rows,
  "&$format=json"
)
df <- GET(url)
df <- rawToChar(df$content)
df <- fromJSON(df)
df <- df$value
df <- subset(df, year == '2015')
df <- df %>% select(geoid, c5_COI_nat) # COI is what we wanted to display. 
df$geo_id <- ('1400000US') # Adding for complete GEOID 
df$geoid <- str_c(df$geo_id,'',df$geoid)
df <- df %>% select(geoid, c5_COI_nat) # Getting back the only two columns we care about. 
colnames(df)<- c('GEO_ID','COI') # Giving appropriate name to properly do merge. 
# This is where we put in the FIP codes (state,county). You can have one or more places.
Places <- data.frame(c("01,001"))
colnames(Places) <- c('Places')
Places[c('state','county')] <- str_split_fixed(Places$Places,',',2)
Places <- Places %>% select(state, county)
Shapes <- data.frame()
for (x in 1:nrow(Places)){
  Shape <- tracts(state = Places[x,1],county =  Places[x,2], year = 2010, cb=T)
  Shapes <- rbind(Shapes,Shape)
}

# Merging Shapefile with COI data! 
Map_Data <- merge(Shapes, df, by = 'GEO_ID')
Map_Data$Level <- ifelse(Map_Data$COI == "Very High", "1",
                         ifelse(Map_Data$COI == "High", "2",
                                ifelse(Map_Data$COI == "Moderate", "3",
                                       ifelse(Map_Data$COI == "Low", "4",
                                              ifelse(Map_Data$COI == "Very Low","5",
                                                     NA)))))
Map_Data$Level <- factor(Map_Data$Level,levels=c(1,2,3,4,5), labels=c("Very High","High","Moderate","Low","Very Low"))
Symbology <- colorFactor(c("#B5E5FD","#52C1FA","#2E74B5","#045781","#013457"),c("Very Low","Low","Moderate","High","Very High"),ordered = TRUE)
Map_Data$Color <- Symbology(Map_Data$Level)


Map_Data<-data.frame(Map_Data$GEO_ID,Map_Data$COI, Map_Data$Color)
names(Map_Data)[1]<-"geoid"
full_map <- merge(Map_Data, full_map, by = 'geoid')


full_map <- sf::st_transform(full_map,'+proj=longlat +datum=WGS84')

Map <- leaflet(data = full_map) %>% 
  addProviderTiles(providers$CartoDB.PositronNoLabels, options = providerTileOptions(opacity=1)) %>% 
  addPolygons(data = full_map$geometry, color = full_map$Map_Data.Color, stroke = FALSE, fillOpacity=0.5, popup = full_map$Map_Data.COI.x) %>%
  addProviderTiles("CartoDB.PositronOnlyLabels",options = providerTileOptions(opacity=1)) %>% # Makes Label Brighter
  addCircles(lng = all_tracts[,1], lat = all_tracts[,2], color = all_tracts$Color, weight = 0.05) %>% 
  addProviderTiles("CartoDB.PositronOnlyLabels",options = providerTileOptions(opacity=1)) 
Map

