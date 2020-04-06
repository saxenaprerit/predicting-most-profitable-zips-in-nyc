# Capital One data challenge

rm(list = ls(all=TRUE))

setwd("C:/Users/Prerit/Desktop/UCinn MSBA Course/Career Services/Capital One/airbnb-zillow-data-challenge-master")

library(data.table)

zillow <- fread('Zip_Zhvi_2bedroom.csv', header = TRUE, na.strings=c("", " ", NA), stringsAsFactors = T, strip.white = T)
listings <- fread('listings.csv', header = TRUE, na.strings=c("", " ", NA), stringsAsFactors = T, strip.white = T)

# Checking structure

str(zillow)
str(listings)

# checking summary

summary(zillow)
summary(listings)

# Data Quality check

# missing values

sort(colSums(is.na(zillow)), decreasing = T)
# Important columns have no missing values
# missing values in initial years for few zip codes which makes sense

sort(colSums(is.na(listings)), decreasing = T)
# 2 columns - 'has availibility' and 'listings' are completely empty
# few other columns have a lot of missing values

library(sqldf)

NYC_zillow <- sqldf('select * from zillow where City like "%New York%"')
NYC_zillow

# Zipcode check

zipcode_zillow <- unique(NYC_zillow$RegionName)
zipcode_zillow

# listings zipcode

listings2 <- listings[is.na(zipcode) == FALSE]
sort(table(listings2$zipcode))
listings2$zipcode <- substr(listings2$zipcode,1,5)
sort(table(listings2$zipcode))
listings2 <- listings2[zipcode!='1m']
sort(table(listings2$zipcode))

# Duplicate column names present in the data

listings2$city <- NULL
listings2$state <- NULL

# columns with maximum NAs

sort(colSums(is.na(listings2)), decreasing = T)

# Removing columns with large missing values

listings2$has_availability <- NULL
listings2$license <- NULL
listings2$jurisdiction_names <- NULL

# Setting format of price correctly

listings2$price <- as.numeric(listings2$price)

# Creating additional columns

NYC_zillow$yoy_10_inc <- (((NYC_zillow$`2017-06`-NYC_zillow$`2008-07`)/NYC_zillow$`2008-07`)*100)/10
summary(NYC_zillow$yoy_10_inc)
hist(NYC_zillow$yoy_10_inc, breaks = 26)

NYC_zillow$last_price <- NYC_zillow$`2017-06`
summary(NYC_zillow$last_price)
hist(NYC_zillow$last_price, breaks = 26)

# doing a inner join through zipcodes

library(plyr)

joining <- function(zillow, airbnb)
{
  zillow <- rename(zillow, c("RegionName" = "zipcode"))
  str(zillow)
  
  zillow_sub <- zillow[,-c(8:262)]
  str(zillow_sub)
  
  city_listings <- join(airbnb[bedrooms == 2], zillow_sub, by = "zipcode", type = "inner")
  
  return(city_listings)
}

NYC_listings <- joining(NYC_zillow, listings2)
str(NYC_listings)

# writing out the combined file

fwrite(NYC_listings, file = "NYC_listings.csv", row.names = FALSE)

# Going ahead we will use NYC_listings to further do our analysis

# Data Cleaning

# Column names

names(NYC_listings)

# Duplicate column names present in the data

NYC_listings$city <- NULL
NYC_listings$state <- NULL

# columns with maximum NAs

sort(colSums(is.na(NYC_listings)), decreasing = T)

# Removing columns with large missing values

NYC_listings$has_availability <- NULL
NYC_listings$license <- NULL
NYC_listings$jurisdiction_names <- NULL

# Setting format of price correctly

NYC_listings$price <- as.numeric(NYC_listings$price)

# EDA on NYC listings

# 1. Number of Unique zipcodes

zipcount <- length(unique(NYC_listings$zipcode))
zipcount

# 2. Number of properties in each zip code

zip_prop <- sqldf('select zipcode, count(distinct id) as props from NYC_listings group by zipcode order by count(distinct id) desc')
zip_prop

# 3. Average price in each zip code

zip_price <- sqldf('select zipcode, avg(price) as avg_price from NYC_listings group by zipcode order by avg(price) desc')
zip_price

# Property Prices graph

str(NYC_zillow)

NYC2 <- NYC_zillow[,c(2,8:262)]
NYC2 <- rename(NYC2, replace = c("RegionName", "zipcode"))

my_result <- melt(subset(NYC2, select = c(1,137:256)), id = c("zipcode"))
my_result[order(my.result$zipcode),]


ggplot(my_result, aes(x = variable, y = value, group = zipcode)) + 
  geom_line(aes(color = zipcode), size = 1) +
  scale_color_manual(values = c(1:26)) +
  theme_minimal() + labs(x = "years") + labs(y = "median value of property") + labs("Trend of property values by zip") 

# Additional Columns for NYC_listings

NYC_listings$occupancy <- 365*0.75
NYC_listings$revenue <- NYC_listings$price*NYC_listings$occupancy

# mean revenue per zipcode

zipwise_revenue <- sqldf('select zipcode, median(revenue) as median_revenue_zip from NYC_listings group by zipcode')

NYC_listings2 <- join(NYC_listings, zipwise_revenue, by = "zipcode", type = "inner")
str(NYC_listings2)

profitable <- function(df, years)
{
  profit_percent <- 0
  profit_percent <- ((((df$last_price*df$yoy_10_inc*years)/100 + df$median_revenue_zip *years)-df$last_price)/df$last_price)*100
  return(profit_percent)
}

NYC_listings2$profit_5 <- profitable(NYC_listings2, 5)
NYC_listings2$profit_10 <- profitable(NYC_listings2, 10)
NYC_listings2$profit_15 <- profitable(NYC_listings2, 15)

str(NYC_listings2)
NYC_listings2[,97:104]

# assuming 10 year period for evaluation of profit

most_profitable_medium <- unique(subset(NYC_listings2[order(profit_10, decreasing = T)], select = c("zipcode","latitude", "longitude", "profit_10")))
most_profitable_zips <- unique(subset(most_profitable_medium, select=c(1,4)))
most_profitable_zips$rank <- c(1:22)
most_profitable_zips

# Visual Narrative

# Property Prices graph

str(NYC_zillow)

NYC2 <- NYC_zillow[,c(2,8:262)]
NYC2 <- rename(NYC2, replace = c("RegionName"= "zipcode"))

my_result <- melt(subset(NYC2, select = c(1,137:256)), id = c("zipcode"))
my_result[order(my_result$zipcode),]

library(ggplot2)

ggplot(my_result, aes(x = variable, y = value, group = zipcode)) + 
  geom_line(aes(color = zipcode), size = 1) +
  scale_color_manual(values = c(1:26)) +
  theme_minimal() + labs(x = "years(2008 - 2017") + labs(y = "median value of property") + labs("Trend of property values by zip") + theme(axis.text.x = element_blank())

# Making maps

library(leaflet)

# Source: https://www.census.gov/geo/maps-data/data/gazetteer2017.html

zip_file <- fread('2017_Gaz_zcta_national.txt', header = TRUE, colClasses = c('character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))
str(zip_file)

zip_file$zipcode <- zip_file$GEOID

new_map <- join(most_profitable_medium[,-c(2,3)], zip_file[,c(6,7,8)], by = "zipcode", type = "left")
new_map <- unique(new_map)

profit <- paste0("Zipcode = ", new_map$zipcode, "<br>",
  "Profit % = ", as.character(format(new_map$profit_10, digits = 2)), "% over 10 years")

# Defining color palette

col_pal <- colorNumeric(c("red", "blue", "gold") , domain =NULL, reverse = F)
# Pass the palette function a data vector to get the corresponding colors
col_pal(c(1,6,9))

m <- leaflet(data = new_map) %>%
  addTiles() %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%  # Add default OpenStreetMap map tiles
  addCircles(lng=new_map$INTPTLONG, lat=new_map$INTPTLAT, radius=~ new_map$profit_10, color = ~col_pal(new_map$profit_10), popup = profit) %>%
  addLegend("bottomright", pal = col_pal, values = ~new_map$profit_10,
            title = "Profit over 10 years(in %)",
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)
m  # Print the map

# bar chart

library(ggplot2)

## set the levels in order we want
new_map2 <- transform(new_map, zipcode = reorder(zipcode, profit_10))

ggplot(new_map2, aes(zipcode, profit_10)) + geom_col(fill = 'dark green') + coord_flip() + ggtitle("Most profitable zip codes") + labs(y = "Profit over 10 years(in %)")

# Box plot of neighbourhood group wise price

library(RColorBrewer)
pal <- colorFactor(rainbow, domain = NULL)
  
p <- ggplot(NYC_listings, aes(neighbourhood_group_cleansed, price))
p + geom_boxplot(aes(colour=NYC_listings$neighbourhood_group_cleansed),outlier.colour = "red", outlier.shape = 4, show.legend = F) + geom_jitter(width = 0.2, colour = '#366dc4') + labs(title = "Neighbourhood group wise prices") + labs(x = "Neighbourhood Group") + labs(y = "Prices per night for listings(in $)")

