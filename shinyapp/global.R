
# Author: Jian (AKA Frank) Liu
# Aim: Loading essentials for the shiny app
# Date: 2019-09-09

library(shiny)
library(shinyjs)
library(V8)
library(ggplot2)
library(dplyr)
library(DBI)
library(tidyr)
library(rmarkdown)
library(knitr)


# selectInput -------------------------------------------------------------

## System
conn <- dbConnect(RSQLite::SQLite(), "qtmb_data.sqlite3")

systems <- dbReadTable(conn, "tab_AMN")
input_systems <- unique(systems$System)
## Crop

crop.para <- dbReadTable(conn, "tab_crop.para")
crops <- unique(crop.para$Crop_name_display)

crop.yield <- dbReadTable(conn, "tab_crop")

soil <- dbReadTable(conn, "tab_soil")%>%
  mutate(CF = round(CF, digits = 2))
soil.texture <- unique(soil$Texture)
soil.moisture <- unique(soil$Moisture)

amn <- dbReadTable(conn, "tab_AMN")
dbDisconnect(conn)


# customisation -------

width_box <- 400

# constants----

layer.1 <- "0-30 cm"
layer.1.1 <- "0-15 cm"
warning_report.tab  <- as.character("Please fill in the soil tab first.")


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
