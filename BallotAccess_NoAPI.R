### setwd function to set your working directory

setwd("/Users/ahourazandiatashbar/Library/Mobile Documents/com~apple~CloudDocs/Documents/Job/SJSU/SAVI/Workshops/Summer 2024/My Workshop")

# 1. Geo-coding Santa Clara ballot box addresses ----

install.packages(c("tidyverse", "leaflet")) 
"The core packages included in tidyverse are 
ggplot2, tibble, tidyr, readr, dplyr, stringr, purr, and forcats."

library(tmap)

library(tidyverse)
"The tidyverse is a collection of R packages designed to 
make data manipulation and analysis easier and more efficient"

library(dplyr, warn.conflicts = FALSE)
"a core package within the tidyverse that provides a set of functions 
for data manipulation and transformation. It introduces a grammar of 
data manipulation, allowing you to express complex data operations 
in a concise and readable manner."

library(tidygeocoder)
"an R package that provides geocoding functionality, 
allowing you to convert addresses into geographic coordinates 
(latitude and longitude)."

library(leaflet)
"an R package that provides an interactive and flexible mapping environment. 
It allows you to create interactive maps with various layers, markers, pop-ups, 
and other visual elements, making it an excellent tool for data visualization and exploration."

data <- read_csv("Data/BB_Geo_0.csv")
View(data)

"To geocode the addresses, 
we’ll use the geocode function from the tidygeocoder package. 
This function takes the address field as input and returns 
the latitude and longitude values."

addr <- as.data.frame(data) 
"A dataframe is a data structure constructed 
with rows and columns, similar to a database or Excel spreadsheet"

lat_longs <- addr %>%
  geocode(CommAddress, method = "osm", lat = latitude, long = longitude, full_results = TRUE)
"%>% or pipe takes the output of the expression on its left and 
passes it as the first argument to the function on its right."

"It’s common to encounter null or missing values during the geocoding process. 
To remove these null values from the geocoded data, 
we can subset the dataframe using the subset function."

new_DF <- subset(lat_longs, lat_longs$latitude != "")
view(new_DF)

new_DF$boundingbox <- NULL
view(new_DF)
write.csv(new_DF, file = "Data/BB_Geo.csv")


m <- leaflet(new_DF) %>%
  addTiles() %>%
  addCircleMarkers(lng = new_DF$longitude, lat = new_DF$latitude, clusterOptions = markerClusterOptions())

m # Print the map







# 2. Ballot box availability per neighborhood in the city of San Jose ----

install.packages(c("matrixStats",
                   "SpatialAcc"))

library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)
library(rmapshaper)
library(matrixStats)
library(SpatialAcc)
library(leaflet)
library(tmap)


#Bringing Santa Clara voting pop per blockgroup using census API
options(tigris_use_cache = TRUE)
census_api_key("Add your API Key", overwrite = TRUE, install = TRUE)
readRenviron("~/.Renviron")

library(tidycensus)
vars <- load_variables(2022, "acs5")
View (vars)

sc.bgs <- get_acs(geography = "block group", 
                     year = 2022,
                     variables = "B29001_001",
                     state = "CA",
                     county = "Santa Clara",
                     output = "wide",
                     geometry = TRUE)

view(sc.bgs)

# Bring in city boundary data in CA
pl <- places(state = "CA", year = 2020, cb = TRUE)

# Keep San Jose city
sj.city <- filter(pl, NAME == "San Jose")

#Clip bgs using SJ boundary
sj.bgs <- ms_clip(target = sc.bgs, clip = sj.city, remove_slivers = TRUE)

# reading ballot box data
Ballot <- read.csv("Data/BB_Geo.csv")
sj.ballots <- subset (Ballot, Ballot$City =="San Jose")
summary (sj.ballots)
sj.ballots <- select(sj.ballots, -CommAddress, -Dates, -Hours, 
                     -place_id, -licence, -osm_type, 
                     -osm_id, -class, -type,
                     -place_rank, -importance, -addresstype,
                     -name, -display_name)
view(sj.ballots)


sj.ballots <- st_as_sf(sj.ballots, 
                       coords = c("longitude","latitude"), # note we put lon (or X) first!
                       crs=4269) # add projection (this is NAD83)
tmap_mode("view")
tm_shape(sj.ballots) +
  tm_dots(size = 0.1,
          col = "black")


glimpse(sj.ballots)

#Make sure the block groups and ballot 
#Coordinate Reference Systems are the same

st_crs(sj.ballots)
st_crs(sj.ballots)$proj4string
st_crs(sj.ballots)$units

st_crs(sj.bgs)
st_crs(sj.bgs)$proj4string
st_crs(sj.bgs)$units

sj.ballots.utm <- sj.ballots %>%
  st_transform(crs = "+proj=utm +zone=10 +datum=NAD83 +ellps=GRS80") 

sj.bgs.utm <- sj.bgs %>%
  st_transform(crs = "+proj=utm +zone=10 +datum=NAD83 +ellps=GRS80")

st_crs(sj.bgs.utm) == st_crs(sj.ballots.utm)

tm_shape(sj.bgs.utm) +
  tm_polygons() +
  tm_shape(sj.ballots.utm) +
  tm_dots(col = "red") +
  tmap_options(check.and.fix = TRUE)

# Supply vs Demand Analysis number of ballots per voting resident in a bg

#create a unique ID for each ballot
sj.ballots.utm <- sj.ballots.utm %>%
  mutate(ID = row_number())

#sum up number of ballots within each bg
sj.ballots_agg <- aggregate(sj.ballots.utm["ID"], sj.bgs.utm, FUN = "length")

#any bg with an NA has 0 ballots
sj.ballots_agg <- sj.ballots_agg %>%
  mutate(ID = replace_na(ID,0))

#save number of banks within a tract to main data object
sj.bgs.utm <- sj.bgs.utm %>%
  mutate(ballots = sj.ballots_agg$ID)

View(sj.bgs.utm)
 
# Ballot per 1000 voters
sj.bgs.utm<-sj.bgs.utm %>%
  mutate(ballratio = (ballots/B29001_001E)*1000)
summary(sj.bgs.utm$ballratio)

# mapping the Ballot per 1000 ratio

tm_shape(sj.bgs.utm, unit = "mi") +
  tm_polygons(col = "ballratio", style = "jenks",palette = "Reds", 
              border.alpha = 0, title = "Ballot per\n1k voters") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = "Ballot spatial accessibility in the city of San Jose",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")







# 3. Ballot box availability in nearest proximity of a San Jose neighborhood  ----
#Another approach: capture distance from the nearest amenities, 
# counting the number of ballots within r miles of the neighborhood/block group.

#Step 1: create buffers of radius r around each neighborhood centroid
bg.ctrs <- st_centroid(sj.bgs.utm)

tm_shape(sj.bgs.utm) +
  tm_polygons(col = "blue") +
  tm_shape(bg.ctrs) +
  tm_dots(col = "red")

bg.buff  <-st_buffer(bg.ctrs, dist = 1609) #1-mile or 1609-meter buffers
bg.buff

# lets see one case
ex1 <- filter(bg.buff, GEOID == "060855120341")
ex2 <- filter(sj.bgs.utm, GEOID == "060855120341")

tmap_mode("view")

tm_shape(sj.bgs.utm) +
  tm_polygons() +
  tm_shape(bg.ctrs) +
  tm_dots(size = 0.01) +
  tm_shape(ex1) +
  tm_borders(col="red") +  
  tm_shape(ex2) +
  tm_dots(col = "red")

# counting the number of ballots on the buffers
buff.ballot_agg <- aggregate(sj.ballots.utm["ID"], 
                             bg.buff, FUN = "length")

#replacing NAs with 0
buff.ballot_agg <- buff.ballot_agg %>%
  mutate(ID = replace_na(ID,0))

bg.buff <- bg.buff %>%
  mutate(ballot1m = buff.ballot_agg$ID)  %>%
  dplyr::select(GEOID, ballot1m)

view(bg.buff)
view(sj.bgs.utm)

# join bg.buff back to sj.bgs.utm 
# so can map ballot1m using the bg boundaries. 

bg.buff <- bg.buff %>%
  st_drop_geometry()

sj.bgs.utm <- sj.bgs.utm %>%
  left_join(bg.buff, by = "GEOID") %>%
  mutate(ball1mratio = (ballot1m/B29001_001E)*1000)

sj.bgs.utm %>%
  summarize(Mean = mean(ball1mratio, na.rm=TRUE)) %>%
  st_drop_geometry()

# creating a histogram of the data
sj.bgs.utm %>%
  ggplot() + 
  geom_histogram(mapping = aes(x=ball1mratio), na.rm = TRUE) +
  xlab("Ballots in 1-mile of a San Jose Block Groups")

# Choropleth map of ballot per capita within a 1-mile radius.
tmap_mode("view")

tm_shape(sj.bgs.utm, unit = "mi") +
  tm_polygons(col = "ball1mratio", style = "jenks",palette = "Reds", 
              border.alpha = 0, title = "Ballot box per capita") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = "Ballot box spatial accessibility in San Jose, CA",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")






# 4. Distance to nearest ballots in the city of San Jose ----

# A common spatial accessibility measure is the distance to the closest amenity

# Distance is typically measured in Euclidean distance referred to as straight-line distance or “as the crow flies,” 
# is the distance between two points connected by a straight line on a flat surface. 
# calculate the distance (in meters) from each bg’s centroid to each ballot box

ballot.dist <-st_distance(bg.ctrs, sj.ballots.utm)
dim(ballot.dist) #  rows bg.ctr - columns ballots

# Function rowMins() in the package matrixStats allows to calculate the shortest distance.

sj.bgs.utm <- sj.bgs.utm %>%
  mutate(ballotmin = rowMins(ballot.dist))

sj.bgs.utm <- sj.bgs.utm %>%
  mutate(ballotmile = ballotmin*0.000621371)

sj.bgs.utm %>%
  summarize("Mean Min" = mean(ballotmile, na.rm=TRUE),
            "Median Min" = median(ballotmile, na.rm=TRUE)) %>%
  st_drop_geometry()

#   Mean Min Median Min
# 1 1365.601   1159.251
# There is ballot box in ~1400 meters or almost 0.9 mile

sj.bgs.utm %>%
  ggplot() + 
  geom_histogram(mapping = aes(x=ballotmile), na.rm = TRUE) +
  xlab("Distance (mile) to nearest ballot box")

#Choropleth of dist to nearest ballot
tmap_mode("view") #interactive map mode

# To reverse a color palette in tm_shape in R, you can add a minus sign (-) at the beginning of the character
tm_shape(sj.bgs.utm, unit = "mi") +
  tm_polygons(col = "ballotmile", style = "jenks",palette = "-Reds", 
              border.alpha = 0, title = "Dist. to nearest ballot box") +
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(main.title = "Ballot box spatial accessibility in San Jose, CA",
            main.title.size = 0.95, frame = FALSE,
            legend.outside = TRUE, legend.outside.position = "right")




