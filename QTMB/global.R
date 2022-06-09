
# Author: Jian (AKA Frank) Liu
# Aim: Loading essentials for the shiny app
# Date: 2019-09-09

library(shiny)
library(shinyjs)
library(ggplot2)
library(data.table)
library(dplyr)
library(DBI)
library(tidyr)


# selectInput -------------------------------------------------------------

## System
conn <- dbConnect(RSQLite::SQLite(), "qtmb_data.sqlite3")

systems <- dbReadTable(conn, "tab_AMN")
input_systems <- unique(systems$System)
## Crop

crop.para <- dbReadTable(conn, "tab_crop.para")
crops <- unique(crop.para$Crop)

crop.yield <- dbReadTable(conn, "tab_crop")

soil <- dbReadTable(conn, "tab_soil")

amn <- dbReadTable(conn, "tab_AMN")
dbDisconnect(conn)
