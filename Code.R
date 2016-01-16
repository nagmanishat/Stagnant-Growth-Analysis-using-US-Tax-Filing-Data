#####################################################################################
# Making a map using R #
# Making a map essentially involves three steps --- getting a shape file for map, 
#		getting the data, and plotting. 
# Make a map to plot state level income share of top 1% of income earners in 2012
# Data downloaded from website http://www.shsu.edu/eco_mwf/inequality.html
#####################################################################################
library(raster) # to get map shape file
library(ggplot2) # for plotting and miscellaneous things
library(ggmap) # for plotting
library(plyr) # for merging datasets
library(scales) # to get nice looking legends
library(maps) # draw geographical maps

# Get a shape file of states of USA

usa.shape <- getData("GADM", country = "USA", level = 1)

# Plot to see how the map looks (may take a while)
# plot(usa.shape)

# Look at the names of states of USA
# usa.shape$NAME_1
#[1] "alabama"              "alaska"               "arizona"             
#[4] "arkansas"             "california"           "colorado"            
#[7] "connecticut"          "delaware"             "district of columbia"
#[10] "florida"              "georgia"              "hawaii"              
#[13] "idaho"                "illinois"             "indiana"             
#[16] "iowa"                 "kansas"               "kentucky"            
#[19] "louisiana"            "maine"                "maryland"            
#[22] "massachusetts"        "michigan"             "minnesota"           
#[25] "mississippi"          "missouri"             "montana"             
#[28] "nebraska"             "nevada"               "new hampshire"       
#[31] "new jersey"           "new mexico"           "new york"            
#[34] "north carolina"       "north dakota"         "ohio"                
#[37] "oklahoma"             "oregon"               "pennsylvania"        
#[40] "rhode island"         "south carolina"       "south dakota"        
#[43] "tennessee"            "texas"                "utah"                
#[46] "vermont"              "virginia"             "washington"          
#[49] "west virginia"        "wisconsin"            "wyoming"            
# > 

# To merge population data to the shape file, convert the shape file into a dataframe
usa.df <- map_data("state")

# Check the contents of the data frame
# str(usa.df)
#'data.frame':	15537 obs. of  6 variables:
#  $ long     : num  -87.5 -87.5 -87.5 -87.5 -87.6 ...
#$ lat      : num  30.4 30.4 30.4 30.3 30.3 ...
#$ group    : num  1 1 1 1 1 1 1 1 1 1 ...
#$ order    : int  1 2 3 4 5 6 7 8 9 10 ...
#$ region   : chr  "alabama" "alabama" "alabama" "alabama" ...
#$ subregion: chr  NA NA NA NA ...
#>

colnames(usa.df) [5] <- "state"
usa.df$state <- as.factor(usa.df$state)

#str(usa.df)
#'data.frame':	15537 obs. of  6 variables:
#  $ long     : num  -87.5 -87.5 -87.5 -87.5 -87.6 ...
#$ lat      : num  30.4 30.4 30.4 30.3 30.3 ...
#$ group    : num  1 1 1 1 1 1 1 1 1 1 ...
#$ order    : int  1 2 3 4 5 6 7 8 9 10 ...
#$ state    : Factor w/ 49 levels "alabama","arizona",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ subregion: chr  NA NA NA NA ...
# > 

# Get the state level income share of top 1% data based on 2012 census
usa.dat <- read.table("usa_top1_2012.csv", header = T, sep = ",")

# Merge the shape data with the population data by state name
usa.df <- join(usa.df, usa.dat, by = "state", type = "inner")

# str(usa.df)
# 'data.frame':	15537 obs. of  8 variables:
#   $ long     : num  -87.5 -87.5 -87.5 -87.5 -87.6 ...
# $ lat      : num  30.4 30.4 30.4 30.3 30.3 ...
# $ group    : num  1 1 1 1 1 1 1 1 1 1 ...
# $ order    : int  1 2 3 4 5 6 7 8 9 10 ...
# $ state    : Factor w/ 49 levels "alabama","arizona",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ subregion: chr  NA NA NA NA ...
# $ income   : num  16.7 16.7 16.7 16.7 16.7 ...
# $ abb      : Factor w/ 51 levels "AK","AL","AR",..: 2 2 2 2 2 2 2 2 2 2 ...
# > 


#range(usa.df$income)
# [1] 13.68477 33.00785
# > 

# Divide income into class intervals --- there will be one color for each interval
# provide only the upper limits of the intervals (the break points)

brks <- c(-10,-5,0,5,10,15,20,25,30,35)

#Create the binding of the abbreviation to the population data
cnames <- aggregate(cbind(long, lat) ~ abb, data= usa.df, 
                    FUN=function(x)mean(range(x)))

#Plot map with the abbreviations for  each state
p <- ggplot(usa.df,aes(long,lat)) +
  # with borders (slower)
  geom_polygon(data = usa.df, aes(x = long, y = lat, group = group, fill = income), 
               color = "black", size = 0.15) +
  scale_fill_distiller(palette = "Reds", breaks = brks, trans = "reverse") +
  theme_nothing(legend = TRUE) +
  labs(title = "Top 1% income earners in 2012 in USA", fill = "")	+
  geom_text(data=cnames, aes(long, lat, label = abb),size = 2)

# Note: we are using shades of red for plotting; trans = "reverse" option 
# Makes the shades go from dark to light as the income percent increases, thus 
# ensuring that darkest red = worst case scenario.

# Save the map to a file to viewing 

ggsave(p, file = "usa_income_map_2012.pdf")

##################################################################################

