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

# Get Full Dataset (Population, Shapes, Counts, Etc.) From Diversity Data Kids 
RetrievingData <- function() { 
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
  return(database)
}

GetMap <- function(database) {
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
  return(full_map)
}


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

DataFromDDK <- RetrievingData()
MapToProcess <- GetMap(DataFromDDK)


