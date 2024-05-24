options(stringsAsFactors=FALSE)

library(shinymaterial)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(shinythemes)
library(shinyjs)
library(dplyr)
#library(openxlsx)

#library(shinyauthr)

#library(imager)
#library(OpenImageR)

source("R/login_server.R")
source("R/card.R")
source("R/copied functions.R")

source("Daten/bsp_daten_erzeugen.R")

#filter_options <- create_filter_options()
#card_data <- create_bsp_card_data()

#card_data <- openxlsx::read.xlsx("Daten/Beispielbeitraege.xlsx")
#user_data <- openxlsx::read.xlsx("Daten/beispieluser.xlsx")
# card_data <- openxlsx::read.xlsx("Daten/Beispielbeitraege.xlsx")
# user_data <- openxlsx::read.xlsx("Daten/Beispielbeitraege.xlsx")

load("Daten/card_data.rdata")

load("Daten/user_data.rdata")

filter_options <- openxlsx::read.xlsx("Daten/Filter.xlsx")


my_id <- 1


