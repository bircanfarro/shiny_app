library(shinydashboard)
library(shiny)
library(data.table)
library(dplyr)
library(dtplyr)
library(leaflet)
library(dygraphs)



#Refugees & Asylumm Seekers original dataset___Dataset(1)

  filename_refgs <- "unhcr_popstats_export_persons_of_concern_2018_04_29_194151.csv"
  refugee <- read.csv(filename_refgs, header = T, stringsAsFactors = F, skip = 2)
  dt_refugees <- data.table(refugee)
  dt_refugees[,Returned.Refugees := NULL]
  
  
  
  dt_refugees$Refugees <- as.numeric(dt_refugees$Refugees)
  dt_refugees$Asylum.Seekers <- as.numeric(dt_refugees$Asylum.Seekers)
  dt_refugees$Year <- as.numeric(dt_refugees$Year)
  dt_refugees = na.omit(dt_refugees)





#Countries longitude and latitude dataset for maps

  filename_coords <- "country_coords.csv"
  df_coords <- read.csv(filename_coords, header = T, stringsAsFactors = F)
  dt_coords <- data.table(df_coords)
  dt_coords[,country := NULL]





#Defining each group of people on the same dataset based on their Origin or Destination

  dt_ref_to_turkey <- dt_refugees[Destination == "Turkey" & Year > 1979]
  dt_ref_to_turkey <- dt_ref_to_turkey[dt_coords, on="Origin==name", nomatch=0]
  
  
  dt_ref_from_turkey <- dt_refugees[Origin == "Turkey" & Year > 1979]
  dt_ref_from_turkey <- dt_ref_from_turkey[dt_coords, on="Destination==name", nomatch=0]
  
  
  dt_asylee <- dt_refugees[Origin == "Turkey" & Year > 1999]
  dt_asylee <- dt_asylee[dt_coords, on="Destination==name", nomatch=0]





#Describing a function to determine the total value of refugees based on their origin and destination.
#Origin == Turkey means refugees who fled fromm Turkey
#Destination == Turkey means refugees who fled to Turkey
#For asylum seekers same function was used as their Origin == Turkey

  totalWithinYears <- function(dt, groupOnOrigin, startYear, endYear) {
    if (groupOnOrigin) {
      return(
        dt[Year >= startYear & Year <= endYear] %>%
          group_by(Origin) %>% summarise(Total = sum(Refugees, na.rm=TRUE))  
      )
    } else {
      return(
        dt[Year >= startYear & Year <= endYear] %>%
          group_by(Destination) %>% summarise(Total = sum(Refugees, na.rm=TRUE))  
      )
    }
  }

  totalAsyleesWithinYears <- function(dt, startYear, endYear) {
    return(
      dt[Year >= startYear & Year <= endYear] %>%
        group_by(Destination) %>% summarise(Total = sum(Asylum.Seekers, na.rm=TRUE))  
    )
  }





#Calculation and normalization for the circle markers on the interactive mao

  marker_radius <- 25
  marker_radius_min <- 5
  
  
  calcNorm <- function(vals) {
    vmin <- min(vals, na.rm=TRUE)
    vmax <- max(vals, na.rm=TRUE)
    return((vals-vmin)/(vmax-vmin))
  }
  
  
  calcRadius <- function(valeuVec) {
    return(calcNorm(valeuVec) * marker_radius + marker_radius_min)
  }





#Original refugees demographics dataset____Dataset(2)

  filename_demos <- "unhcr_popstats_export_demographics_2018_04_29_193429.csv"
  demographic <- read.csv(filename_demos, header = T, sep = ",", dec = ".", fill = TRUE, skip = 2)
  
  
  demographic$Female.0.4 = as.numeric(as.character(demographic$Female.0.4))
  demographic$Female.5.11 = as.numeric(as.character(demographic$Female.5.11))
  demographic$Female.12.17 = as.numeric(as.character(demographic$Female.12.17))
  demographic$Female.18.59 = as.numeric(as.character(demographic$Female.18.59))
  demographic$Female.60. = as.numeric(as.character(demographic$Female.60.))
  demographic = na.omit(demographic)
  
  
  demographic$Male.0.4 = as.numeric(as.character(demographic$Male.0.4))
  demographic$Male.5.11 = as.numeric(as.character(demographic$Male.5.11))
  demographic$Male.12.17 = as.numeric(as.character(demographic$Male.12.17))
  demographic$Male.18.59 = as.numeric(as.character(demographic$Male.18.59))
  demographic$Male.60.= as.numeric(as.character(demographic$Male.60.))
  demographic = na.omit(demographic)
  
  
  dt_demo <- data.table(demographic)
  dt_demo_turkey <- dt_demo[Destination == "Turkey"]
  dt_demo_turkey[,Destination := NULL]
  dt_demo_turkey[,Location := NULL]
  
