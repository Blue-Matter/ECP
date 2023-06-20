library(dplyr)
library(DT)
library(ggplot2)
library(ggrepel)
library(readxl)
library(shinyBS)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(shinyjs)
library(shinyalert)
library(cowplot)
library(tidyr)

#source('home.r')
#source('load.R')
#source('FPI.R')
#source('dynamics.R')
#source('results.R')

#for (fl in list.files("./Source")) source(file.path("./Source", fl))


# Shared variables

Current_Year<<-as.integer(substr(Sys.time(),start=1,stop=4))



