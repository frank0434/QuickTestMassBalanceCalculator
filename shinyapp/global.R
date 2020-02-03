
# Author: Jian (AKA Frank) Liu
# Aim: Loading essentials for the shiny app
# Date: 2019-09-09
Sys.setlocale("LC_ALL","English")
library(shiny)
library(shinyjs)
library(V8)
library(ggplot2)
library(dplyr)
library(DBI)
library(tidyr)
library(rmarkdown)
library(knitr)
library(openxlsx)
library(DT)

# selectInput -------------------------------------------------------------

## System

#connect to the sqlite file
conn <- dbConnect(RSQLite::SQLite(), "qtmb_data.sqlite3")

#read the amn tab in for the system information
systems <- dbReadTable(conn, "tab_AMN")
input_systems <- unique(systems$System)

## Crop
#read parameter tab in and unique the crop names
crop.para <- dbReadTable(conn, "tab_crop.para")
crops <- unique(crop.para$Crop_name_display)

#read the yield tab in
crop.yield <- dbReadTable(conn, "tab_crop")

#read the soil tab in and trim the conversion factor to 2 decimal
soil <- dbReadTable(conn, "tab_soil")%>%
  mutate(CF = round(CF, digits = 2))

#unique the texture and moisture for user selection
soil.texture <- unique(soil$Texture)
soil.moisture <- unique(soil$Moisture)

#disconnect the file
dbDisconnect(conn)


# customisation -------

width_box <- 400

# constants----
#two top layer options
layer.1 <- "0-30 cm"
layer.1.1 <- "0-15 cm"
#message to show in the UI
warning_report.tab  <- as.character("WARNING! Please fill in the soil tab first.")
warning_soil.tab <- as.character("Quick test result must be between 0 and 2147483647.")
warning_samplingdate <- as.character("WARNING! \r\nSampling date must be smaller than the next sampling date.")

# action button style text ----
btn_style <- ".btn {
              display:block;
              color: black;
              border: 2px solid grey;
              background-color:rgb(0, 158, 115);
              height: 40px;
              width: 240px;
              font-size: 15px;
              font-weight:bold;
              } "


# style = "border: 3px solid black; padding:14px; font-size:100%"

# plotting -----


