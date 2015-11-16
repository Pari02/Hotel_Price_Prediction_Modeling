# installing required libraries
install.packages("rjson")
install.packages("jsonlite")
install.packages("hydroTSM")
install.packages("RCurl")
install.packages("XML")
install.packages("tm")
install.packages("qdap")
library(rjson)
library(jsonlite)
library(hydroTSM)
library(plyr) 
library(RCurl)
library(XML)
library(tm)
library(qdap)

# assign path to the variables
dirP <- file.path("C:", "Users", "Parikshita", "Desktop", "Data Science")
dirS <- file.path(dirP, "Fall 2015", "Applied Statistics") # cityRating for csv's
path <- file.path(dirP, "Summer 2015", "Practicum-CSP 572", "TripAdvisorJson", "json") 

# extract the data from the files
filename <- list.files(path, pattern = ".json", full.names = TRUE)

# extracting all the files
all_data <- lapply(filename, function(x) fromJSON(x))

# extracting cities and countries name
all_address <- lapply(1:length(all_data), function(i) all_data[[i]]$HotelInfo$Address)
plain.text <- lapply(1:length(all_doc), function(x) xpathSApply(all_doc[[x]], "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue))
pattern <- lapply(1:length(all_doc), function(x) which(plain.text[[x]] %in% c(" ", ", ")))
address <- lapply(1:length(plain.text), function(i) {
  if(length(pattern[[i]]) != 0)
  {
    plain.text[[i]][-pattern[[i]]]
  }
  else
  {
    plain.text[[i]] <- plain.text[[i]]
  }
})

all_doc <- lapply(1:length(all_address), function(x) htmlParse(all_address[x], asText = TRUE))

all_city <- lapply(1:length(all_doc), function(x) xpathSApply(all_doc[[x]], "//span[@property='v:locality']", xmlValue))
all_country <- lapply(1:length(all_doc), function(x) xpathSApply(all_doc[[x]], "//span[@property='v:country-name']", xmlValue))
all_country <- unique(subset(all_country, lapply(1:length(all_country), function(x) length(all_country[[x]])) > 0))

# extracting address of all the files
postal_codes <- lapply(1:length(all_doc), function(x) xpathSApply(all_doc[[x]], "//span[@property='v:postal-code']", xmlValue))

# creating directory where output will be saved
#dir.create(file.path(dirS, "Word"), showWarnings = FALSE)

# removing address of hotels which are not in United States
country_filter <- which(sapply(lapply(1:length(address), function(x) any(which(address[[x]] %in% all_country))), isTRUE))  
zips_country <- postal_codes[-country_filter]

# extracting rating & review date for all the hotels
all_rating <- lapply(1:length(all_data), function(i) all_data[[i]]$Reviews$Ratings)
rating_all <- all_rating[-country_filter]
all_hotel <- lapply(1:length(all_data), function(i) all_data[[i]]$HotelInfo$Name)
hotels <- all_hotel[-country_filter]
all_date <- lapply(1:length(all_data), function(i) all_data[[i]]$Reviews$Date)
date_all <- all_date[-country_filter] 
all_price <- lapply(1:length(all_data), function(i) all_data[[i]]$HotelInfo$Price)
price_all <- all_price[-country_filter]
address_all <- address[-country_filter]
#address_all <- lapply(1:length(address_all), function(x) tolower(address_all[[x]]))
  
# extarcting us_cities
us_cities <- all_city[-country_filter]
cities <- unique(subset(us_cities, lapply(1:length(us_cities), function(x) length(us_cities[[x]])) > 0))


# write city names in CSv
write.csv(as.matrix(cities), "us_cities.csv")

for(i in 1:length(cities))
{
  #dir.create(file.path(dirS), showWarnings = FALSE)
  city <- cities[[i]]
  #city <- "Chicago"
  
  # getting only those indexes which have city name in them
  city_filter <- which(sapply(lapply(1:length(address_all), function(x) any(which(address_all[[x]] %in% city)))
                              , isTRUE))  
  
  #if (length(city_filter) >= 30)
  #{
  # using the index field to get rating, review date and content information
  rating_city <- rating_all[city_filter]
  hotelName <- hotels[city_filter]
  review_date <- date_all[city_filter]
  zip_codes <- zips_country[city_filter]
  zip_codes <- lapply(1:length(zip_codes), function(l) ifelse(length(zip_codes[[l]]) == 0, 0
                                                              , zip_codes[[l]]))
  hotelPrice <- price_all[city_filter]
    
  # combining ratings, review date, hotel name and zip codes
  ratings <- lapply(1:length(city_filter), function(x) cbind(rating_city[[x]], review_date[[x]]
                                                             , hotelName[[x]], hotelPrice[[x]]
                                                             , zip_codes[[x]]))
    
  # command to create flatlist in R by row 
  ratings1 <- rbind.fill(lapply(ratings, function(f) { as.data.frame(Filter(Negate(is.null), f))}))
    
  # assigning names to columns
  #names(ratings1) <- c("Overall", "Service", "Cleansiness", "Value", "SleepQual", "Rooms", "Location"
   #                    , "Amenities", "Reception", "Date", "Hotel Name", "Hotel Price", "Zip Code")
  
  # as.character(f) requires a "primitive lookup" to find the function as.character.factor()
  #, which is defined as as.numeric(levels(f))[f]
  ratings1$`review_date[[x]]` <- as.yearmon(ratings1$`review_date[[x]]`, "%B%d, %Y")
    
  # write csv file for each city
  write.csv(ratings1, paste(city, "ratings.csv", sep = "_"))
  #}
}