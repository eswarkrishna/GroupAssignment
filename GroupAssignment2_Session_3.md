# *Group Name - Team Analytics*

### 71610025 - BASAVA ESWARA KRISHNA ALURI
### 71610076 - SHANTNU GUPTA
### 71610032 - KARTIK MAHESHWARI
### 71610038 - MOHIT KALSI

## City of choice
### DELHI
## Client
### City Hospital

# Decision problem[TBD]
 
## Should densely populated places be targeted for the billboards placement

# Research Objective
 
## Identify factors  which the hospital should  take into account while placing the billboards across the city.

##Rationale behind choosing the city
 
Delhi , being capital, is one of the most densely populated cities in the country. Air pollution index suggests that the inhabitants of the city is facing serious health issues [mostly cardiac illness]. Majority of the population is facing bad cardiac health issues. All this makes advertising in the city good for the business as:-
* Due to large population, the conversion rate is comparatively higher than any other cities in the country.
* Delhi had 153 micrograms of PM2.5 per cubic metre. Pollution at delhi is at alarming level.

## Proxies used
### Subway station
### Bus Station
### Liquor store
### Cemetery

#### Reasons for choosing

* Subway/metro  station - A large chunk of daily commuters involve office goers , offices located in and around places(cyber city, noida, Manesar) which are very densely polluted due to large amount of vehicles coming in daily. Therefore making these office goers prone to health disease due to physical inactivity which further amounts to stress , and other health issues . There are good chances of these set of people in availing these services.
 
* Bus station - Wider audience who look for affordable health solutions. Making them aware of these services could benefit in a way , as many of them may not be aware of these services.
 
* Funeral home/cementry - vulnerable state of mind. Putting hoardings around these locations will help generate awareness in people about the things which can be done in life threatening, devastating times.
 
* Liquor store - Placing billboards at these locations will hit the psychological mind set of the buyer regarding health awareness, as alcohol consumption lead to health problems.

# Code
<p><code>
library("RCurl")
library("jsonlite")
library("plotGoogleMaps")
library("geosphere")
 
key = "AIzaSyA7_dPinLRBEBfx4gVxP4v5VCMfKLDJ6eM"

# malls search
#url = paste0("https://maps.googleapis.com/maps/api/place/radarsearch/json?&query=malls+in+delhi&types=shopping_mall&location=28.6139,77.2090&radius=25000&key=",key)
#doc <- getURL(url)
#x <- jsonlite::fromJSON(doc)
#malls = x$results$geometry$location
#head(malls)

# hospitals
#url = paste0("https://maps.googleapis.com/maps/api/place/radarsearch/json?&query=hospitals+in+delhi&types=hospital&location=28.6139,77.2090&radius=25000&key=",key)
#doc <- getURL(url)
#x <- jsonlite::fromJSON(doc)
#hospitals = x$results$geometry$location
#head(hospitals)

# banks
#url = paste0("https://maps.googleapis.com/maps/api/place/radarsearch/json?&query=banks+in+bangalore&types=bank&location=28.6139,77.2090&radius=50000&key=",key)
#doc <- getURL(url)
#x <- jsonlite::fromJSON(doc)
#banks = x$results$geometry$location
#head(banks)


# health
#url = paste0("https://maps.googleapis.com/maps/api/place/radarsearch/json?&query=health+in+delhi&types=health&location=28.6139,77.2090&radius=25000&key=",key)
#doc <- getURL(url)
#x <- jsonlite::fromJSON(doc)
#health = x$results$geometry$location
#head(health)

# Bus station search
url = paste0("https://maps.googleapis.com/maps/api/place/radarsearch/json?&query=bus_station+in+delhi&types=bus_station&location=28.6139,77.2090&radius=25000&key=",key)
doc <- getURL(url)
x <- jsonlite::fromJSON(doc)
busstation = x$results$geometry$location
head(busstation)

#url = paste0("https://maps.googleapis.com/maps/api/place/radarsearch/json?&query=train_station+in+delhi&types=train_station&location=28.6139,77.2090&radius=25000&key=",key)
#doc <- getURL(url)
#x <- jsonlite::fromJSON(doc)
#trainstation = x$results$geometry$location
#head(trainstation)

# Subway station search
url = paste0("https://maps.googleapis.com/maps/api/place/radarsearch/json?&query=subway_station+in+delhi&types=subway_station&location=28.6139,77.2090&radius=25000&key=",key)
doc <- getURL(url)
x <- jsonlite::fromJSON(doc)
subwaystation = x$results$geometry$location
head(subwaystation)

# Cemetery search
url = paste0("https://maps.googleapis.com/maps/api/place/radarsearch/json?&query=cemetery+in+delhi&types=cemetery&location=28.6139,77.2090&radius=25000&key=",key)
doc <- getURL(url)
x <- jsonlite::fromJSON(doc)
cemetery = x$results$geometry$location
head(cemetery)

# Liquor store  search
url = paste0("https://maps.googleapis.com/maps/api/place/radarsearch/json?&type=liquor_store&location=28.6139,77.2090&radius=25000&key=",key)
doc <- getURL(url)
x <- jsonlite::fromJSON(doc)
liquorstore = x$results$geometry$location
head(liquorstore)




#malls$type = "Mall"
#banks$type = "Bank"
#health$type = "health"
#hospitals$type = "Hospital"
busstation$type="bus_station"
#trainstation$type = "train_station"
subwaystation$type= "subway_station"
cemetery$type= "cemetery"
liquorstore$type="liquor_store"
data = rbind(busstation,subwaystation,cemetery,liquorstore)
dim(data)

write.csv(data,"delhi_places.csv", row.names = F)

###################################################################
###################################################################
###################################################################

# let's plot the bus stations and do clustering based on distance matrix

sample = busstation
coordinates(sample) <-~ lng +lat # Create cordinates
proj4string(sample) = CRS('+proj=longlat +datum=WGS84') # Add Projections

m<-mcGoogleMaps(sample,zcol = "type", mapTypeId='ROADMAP') # Plot on Google maps


# let's plot the subway stations and do clustering based on distance matrix
sample = subwaystation
coordinates(sample) <-~ lng +lat # Create cordinates
proj4string(sample) = CRS('+proj=longlat +datum=WGS84') # Add Projections

m<-mcGoogleMaps(sample,zcol = "type", mapTypeId='ROADMAP') # Plot on Google maps

# let's plot the cemetery and do clustering based on distance matrix
sample = cemetery
coordinates(sample) <-~ lng +lat # Create cordinates
proj4string(sample) = CRS('+proj=longlat +datum=WGS84') # Add Projections

m<-mcGoogleMaps(sample,zcol = "type", mapTypeId='ROADMAP') # Plot on Google maps

# let's plot the liquor store and do clustering based on distance matrix
sample = liquorstore
coordinates(sample) <-~ lng +lat # Create cordinates
proj4string(sample) = CRS('+proj=longlat +datum=WGS84') # Add Projections

m<-mcGoogleMaps(sample,zcol = "type", mapTypeId='ROADMAP') # Plot on Google maps





# Get the coordinates
p2 = busstation[,1:2]

# calculate distances
dist_mat = matrix(0,nrow(p2),nrow(p2))

for (i in 1:nrow(p2)){
  for (j in 1:nrow(p2)){
    dist_mat[i,j] = distCosine(p2[i,],p2[j,], r=6378173)/1000    
  }
}

class(dist_mat)
dist_mat[1:10,1:10]

# Create clusters based in distances
fit <- hclust(as.dist(dist_mat), method="ward")
plot(fit) # display dendogram

groups <- cutree(fit, k=18) # cut tree into 18 clusters
# draw dendogram with red borders around the 18 clusters
rect.hclust(fit, k=18, border="red") 

sample$group = groups # Assign cluster groups

# Plot stores with clustor as label
m <- mcGoogleMaps(sample, mapTypeId='ROADMAP', zcol="group")


#########################################################
#########################################################
#########################################################

sample = data
coordinates(sample) <-~ lng +lat # Create cordinates
proj4string(sample) = CRS('+proj=longlat +datum=WGS84') # Add Projections

m<-mcGoogleMaps(sample,zcol = "type", mapTypeId='ROADMAP') # Plot on Google maps

</code></p>
 
## Results of analysis


