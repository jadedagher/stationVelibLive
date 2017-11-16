install.packages(c("httr", "jsonlite","magrittr","leaflet"))

library(leaflet)
library(httr)
library(jsonlite)
library(magrittr)

#begining of the refresh function 
refresh <- function(){

#Get Data from open data paris API (https://opendata.paris.fr)
velib_get  <- GET("https://opendata.paris.fr/api/records/1.0/search/?dataset=stations-velib-disponibilites-en-temps-reel&rows=300&facet=banking&facet=bonus&facet=status&facet=contract_name")

#test
#names(velib_get)
#velib_get$status_code
#velib_get$headers$'content-type'

#Get the content of the response
text_content <- content(velib_get, as="text", encoding = "UTF-8")
#parse with httr
parsed_content <- content(velib_get, as="parsed")
#names(parsed_content)
json_content <- text_content %>% fromJSON
velib <- json_content$records

#analysis

#Feature Engineering to get a lattitude and longitude column 
#------------lattitude & longitude

#velib$geometry
#column intersting = longitude, lattitude
#velib$geometry[,2]

#divide velib$geometry[,2] in 2 (longitude, lattitude) 
velib$longitude <- sapply(as.character(velib$geometry[,2]), function(x) strsplit(x, split = '[,]')[[1]][1])
velib$lattitude <- sapply(as.character(velib$geometry[,2]), function(x) strsplit(x, split = '[,]')[[1]][2])

#remove the 2 first character from longitude  
velib$longitude <- gsub('^.{2}', '', velib$longitude)

#remove the first and the last character from lattitude
velib$lattitude <- gsub('^.{1}', '', velib$lattitude)
velib$lattitude <- gsub('.{1}$', '', velib$lattitude)

#------------status
#names(velib)
#velib$fields
#column intersting = status
velib$fields[,1]

velib$status <- velib$fields[,1]

#Plot MAP
#convert longitude and lattitude columns in numeric for leaflet plot 
velib$longitude <- as.numeric(velib$longitude)
velib$lattitude <- as.numeric(velib$lattitude)

#plot MAP

#turn open station in "1" closed station in "2"
velib$st <- sapply(velib$status, function(x) {
  if(x == "OPEN") {
    1
  } else if(x == "CLOSED") {
    2
  } else {
    3
  } })

#getcolor have to compare integer
getColor <- function(velib) {
  sapply(velib$st, function(x) {
    if(x <= 1) {
      "green"
    } else if(x >= 2) {
      "red"
    } else {
      "orange"
    } })
}

#assign each color to icons 
icons <- awesomeIcons(
  icon = 'ion-android-bicycle',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(velib)
)

#display map 
leaflet(velib) %>% addTiles() %>%
  addAwesomeMarkers(~velib$longitude, ~velib$lattitude, popup= velib$status, icon=icons) %>% 
  addMiniMap();
} #end refresh function 

#refresh map data 
refresh()

