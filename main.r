install.packages(c("httr", "jsonlite","magrittr","leaflet", "stringi"))

library(leaflet)  #map plot 
library(httr)     #API request 
library(jsonlite) #parsing
library(magrittr) #pipe codding 
library(ggplot2)  #plot charts 
library(stringi)  #sting manipulation

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

#------------------------------------------------------------------------------------

#Interesting KPIs
#1) number of station open/closed 

#create a data frame for plotting 
global_station_status <- as.data.frame(table(velib$status))
#name the dataframe columns
colnames(global_station_status) <- c("status","number_of_station")
#plot using ggplot2
ggplot(global_station_status, aes(x=status, y = number_of_station, fill= status)) + 
  geom_bar(stat="identity") + 
  theme_bw()

#2) number of station open/closed by neighbourhood

velibfulladdress <- velib$fields[,11]
#cut the until the 5 integer 
velibfulladdress_cut <- sub('*(\\d{5}).*', '\\1', velibfulladdress)
#cut the last 5 characters in "velibfulladdress_cut" string 
velib$postcode <- stri_sub(velibfulladdress_cut,-5,-1)
#create a data frame for plotting 
velib_status_by_postcode <- as.data.frame(table(velib$postcode, velib$status))
#name the dataframe columns 
colnames(velib_status_by_postcode) <- c("postcode","status","velib_station_number")
#plot using ggplot2
ggplot(velib_status_by_postcode, aes(x=postcode, y = velib_station_number, fill= status)) + 
  geom_bar(stat="identity") + 
  coord_flip() + 
  theme_bw()


