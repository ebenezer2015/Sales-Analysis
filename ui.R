####################################################
##### load the required packages for Shiny App #####
####################################################
library(shiny)
require(shinydashboard)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(plotly)

##############################################################
#### Dashboard header carrying the title of the dashboard ####
##############################################################
header <- dashboardHeader(title = "Sales Analysis for 2016/2017 Financial Year", 
                          titleWidth = 550) 

#########################################
#### Sidebar content of the dashboard ###
#########################################
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    selectInput(inputId = "SupplyChain",label= strong("Chain"),
                choices = unique(full_data$Chain),
                selected = "Bellings"),
    selectInput(inputId = "States",label= strong("List of States"),
                choices = unique(full_data$State),
                selected = "ACT"),
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Visit-us", icon = icon("send",lib='glyphicon'), 
             href = "https://www.seunebenzer.com")
  )
)

###########################################################################
frow1 <- fluidRow(
  valueBoxOutput("value1", width = 4.0),
  valueBoxOutput("value2", width = 4.0),
  valueBoxOutput("value3", width = 4.0),
  valueBoxOutput("value4", width = 4.0),
  valueBoxOutput("value5", width = 4.0),
  valueBoxOutput("value6", width = 4.0)
)
frow2 <- fluidRow(
  
  box(
    title = "Trend Analysis of Quarterly Revenue"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,background = "light-blue"
    ,plotlyOutput("Trend", height = "400px")),
  
  box(
    title = "Sales Analysis by Category"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE
    ,background = "light-blue"
    ,plotlyOutput("Category", height = "400px")),
  
  box(
    title = "Sales Analysis by Buyers"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,background = "light-blue"
    ,plotOutput("Buyers", height = "600px")),
  
  box(
    title = "Sales Analysis by Managers"
    ,status = "primary"
    ,solidHeader = TRUE 
    ,collapsible = TRUE 
    ,background = "light-blue"
    ,plotOutput("Managers", height = "600px"))
  
)

# combine the two fluid rows to make the body
#body <- dashboardBody(frow1, frow2)

body <- dashboardBody(frow1, frow2,
                      tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Times New Roman", serif;
        font-weight: bold;
        font-size: 24px;
      }
    ')))
)

ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='blue')
