# Shareworks Interview Technical Assessment 

# Libraries 

```r
library(httr)
library(jsonlite)
library(RODBC)
library(gepaf)
library(anytime)
library(tidyverse)
```

# Functions 

```r
GetCurrentWinterRoadConditionsAB <- function(language="en")
{
  url <- "https://511.alberta.ca/api/v2/get/winterroads"
  # Only options available are french and english. If not fr, french or francais, the default will be english
  if (tolower(language) == "fr" | tolower(language) == "french" | tolower(language) == "francais")
  {
    paste0(url, "?lang=fr", collapse = '')
  }
  request <- GET(url)
  if (request$status_code == 200)
  {
    road.information.ls <- content(request, as="parsed")
    return(road.information.ls)
  }else
  {
    print(paste("Error Code:", request$status_code))
    return(NULL)
  }
}
PercentageOfLocationsWithGoodVisibility <- function(road.condition.obj)
{
  good.visibility.count  <- unlist(sapply(road.condition.obj, function(x) if (!is.null(x$Visibility)){ if (tolower(x$Visibility) == "good"){return(TRUE) } }))
  return(sum(good.visibility.count) / length(road.condition.obj) * 100)
}
AreasWithPoorVisibility <- function(road.condition.obj)
{
  poor.visibility.areas <- unlist(sapply(road.condition.obj, function(x) if (!is.null(x$Visibility)){ if (tolower(x$Visibility) == "poor") {return(x$AreaName)}}))
  return(unique(poor.visibility.areas))
}
AreasWithBadConditions <- function(road.condition.obj, good.condition.definitions)
{
  #https://511.alberta.ca/about/tutorial
  
  areas.with.bad.conditions <- c()
  for (i in road.condition.obj) 
  {
    if (sum(i$`Primary Condition` != good.condition.definitions) > 0)
    {
      areas.with.bad.conditions <- c(areas.with.bad.conditions, i$AreaName)
    }else
    {
      for (j in i$`Secondary Conditions`) 
      {
        if (sum(j != good.condition.definitions) > 0)
        {
          areas.with.bad.conditions <- c(areas.with.bad.conditions, i$AreaName)
        }
      }
    }
  }
  return(unique(areas.with.bad.conditions))
}
UpdateAllTables <- function(road.condition.obj, dbCon)
{
  roadInfo.dfs <- GetRoadTables(road.condition.obj)
  conditionInfo.dfs <- GetConditionTables(road.condition.obj)
  # Append existing table
  sqlSave(dbCon, conditionInfo.dfs[[1]], tablename = "ConditionInstances", rownames = FALSE, append = TRUE)
  sqlSave(dbCon, conditionInfo.dfs[[2]], tablename = "Conditions", rownames = FALSE, append = TRUE)
}
CreateDatabaseTables <- function(road.condition.obj, dbCon)
{
  roadInfo.dfs <- GetRoadTables(road.condition.obj)
  conditionInfo.dfs <- GetConditionTables(road.condition.obj)
  
  try(sqlDrop(dbCon, "ConditionInstances"))
  try(sqlDrop(dbCon, "Conditions"))
  try(sqlDrop(dbCon, "Coordinates"))
  try(sqlDrop(dbCon, "Roads"))
  # relationships must be manually added in access
  sqlSave(dbCon, conditionInfo.dfs[[1]], tablename = "ConditionInstances", rownames = FALSE)
  sqlSave(dbCon, conditionInfo.dfs[[2]], tablename = "Conditions", rownames = FALSE)
  sqlSave(dbCon, roadInfo.dfs[[1]], tablename = "Coordinates", rownames = FALSE)
  sqlSave(dbCon, roadInfo.dfs[[2]], tablename = "Roads", rownames = FALSE)
}
GetConditionTables <- function(road.condition.obj)
{
  #Variables declared outside of the array to speed up process
  condition.instances.df <- data.frame(stringsAsFactors = FALSE)
  condition.ids <- c()
  conditions <- c()
  condition.types <-c()
  visibility <- character()
  date.time <- character()
  date <- date()
  time <- character()
  condition.id <- numeric()
  roadId <- numeric()
  for (i in road.condition.obj) 
  {
    # ConditionInstance = id, conditionID, date, time, visibility
    visibility <- i[[5]]
    if (is.null(visibility))
    {
      visibility <- "NA"
    }
    date.time <- anytime(i[[9]])
    date <- format(as.Date(date.time), "%d/%m/%Y")
    time <- strsplit(as.character(date.time), " ")[[1]][2]
    # Condition = id, condition description, conditionType
    
    # add to data base if the condition does not exist
    if (sum(conditions == i[[3]]) == 0)
    {
      conditions <- c(conditions, i[[3]])
      condition.ids <- c(condition.ids, length(condition.ids))
      condition.types <- c(condition.types, "Primary")
    }
    for (j in i[[4]])
    {
      if (sum(conditions == j) == 0)
      {
        conditions <- c(conditions, j)
        condition.ids <- c(condition.ids, length(condition.ids))
        condition.types <- c(condition.types, "Secondary")
      }
    }
    for (j in c(i[[3]], unlist(i[[4]])))
    {
      condition.id <- which(conditions == j) - 1
      roadId <- i[[1]]
      condition.instances.df <- rbind(condition.instances.df,
                                      data.frame(roadId, condition.id, date, time, visibility, stringsAsFactors = FALSE))
    }
  }
  conditions.df <- data.frame(as.integer(condition.ids), conditions, condition.types, stringsAsFactors = FALSE)
  colnames(conditions.df) <- c("id", "conditionDesc", "conditionType")
  colnames(condition.instances.df) <- c("roadID", "conditionId", "date", "time", "visibility")
  return(list(condition.instances.df, conditions.df))
}
GetRoadTables <- function(road.condition.obj)
{
  id <- c()
  locationDescription <- c()
  areaName <- c()
  roadwayName <- c()
  
  roadID <- c()
  latitude <- c()
  longitude <- c()
  for (i in road.condition.obj) 
  {
    id <- c(id, i[[1]])
    locationDescription <- c(locationDescription, i[[2]])
    areaName <- c(areaName, i[[6]])
    roadwayName <- c(roadwayName, i[[7]])
    
    
    lat.lon.df <- decodePolyline(i[[8]])
    latitude <- c(latitude, as.numeric(lat.lon.df$lat))
    longitude <- c(longitude, as.numeric(lat.lon.df$lon))
    roadID <- c(roadID, ((((1:length(lat.lon.df$lat))*0)+1)*i[[1]]))
  }
  road.coordinates.df <- data.frame(roadID, latitude, longitude, stringsAsFactors = FALSE)
  roads.df <- data.frame(id, locationDescription, areaName, roadwayName, stringsAsFactors = FALSE)
  return(list(road.coordinates.df, roads.df))
}
```


# Code

```r
# Get data from API
road.condition.obj <- GetCurrentWinterRoadConditionsAB()

# Connect an access database (  For Fun :)  )
dbCon <- odbcConnectAccess2007("AlbertaWinterRoadConditions.accdb")

# Create write data access database
#CreateDatabaseTables(road.conditions.obj, dbCon)

#Responses to Shareworks Questions
PercentageOfLocationsWithGoodVisibility(road.condition.obj)
AreasWithPoorVisibility(road.condition.obj)
AreasWithBadConditions(road.condition.obj, c("No Report", "Bare wheel paths", "Bare Dry"))
# Visualization was done in PowerBI (Opted to use this over leaflet/ggplot, so I could quickly make a full dashboard)

# BONUS: What other interesting insights can you find? How can this be used by the government to improve people's welling?
  # Save and analyze historical data in addition to traffic data, and collision data to improve road safety.
  # Determine which roads are in need of safety improvements. 
  # Determine if the risk associated with each road is environment or driver based. Improve roads accordingly
  ```r



