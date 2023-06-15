library(shiny)
library(shinydashboard)
library(fresh)
library(shinyWidgets)
library(leaflet)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#004f9f",
    aqua = "#0088c2",
    green = "#019050",
    red = "#c3006b",
    yellow = "#ffcc00"
  ),
  adminlte_sidebar(
    #dark_bg = "#003063"
  ),
  adminlte_global(
    box_bg = "#cee6f5",
    content_bg = "white"
  )
)

modify_stop_propagation <- function(x) {
  x$children[[1]]$attribs$onclick = "event.stopPropagation()"
  x
}

sidebar <- dashboardSidebar(sidebarMenu(
  id = "tabs",
  modify_stop_propagation(menuItem(
    "Erklärungen",
    tabName = "explanations",
    startExpanded = TRUE,
    menuSubItem(
      "Einführung",
      tabName = "intro",
      icon = icon("angle-right")
    ),
    menuSubItem(
      "Vorhersagequalität",
      tabName = "performance",
      icon = icon("angle-right")
    )
  )),
  menuItem("Spiel", tabName = "game"),
  hr(),
  column(
    width = 11,
    uiOutput("gameProgress"),
    fixedPanel(
      left = "10px", bottom = "10px", width = "220px",
      div(HTML(
        "Weather data &copy; Deutscher Wetterdienst"
      ), style = "font-size: 10px;")
    )
  )
))

body <- dashboardBody(
    includeCSS("www/styles.css"),
    use_theme(mytheme),
    tabItems(
      tabItem(
        tabName = "explanations",
        "test123"
      ),
      tabItem(
        tabName = "intro",
        uiOutput("gameIntroduction")
      ),
      tabItem(
        tabName = "performance",
        uiOutput("performanceExplanation")
      ),
      tabItem(
        tabName = "game",
        uiOutput("scoreModal"),
        conditionalPanel(
          condition = 'output.gameState == "Forecasting"',
          fluidRow(
            column(
              width = 12,
              leafletOutput("gameMap", height = "calc(100vh - 80px)"),
              uiOutput("mapSlider"),
              uiOutput("stationPanel"),
              uiOutput("forecastPanel2")
            )
          )
        ),
        conditionalPanel(
          condition = 'output.gameState == "End"',
          fluidRow(
            column(
              width = 12,
              uiOutput("finalScore")
            )
          )
        )
      )
    )
  )

dashboardPage(
  dashboardHeader(title = "Vorhersagespiel"),
  sidebar,
  body
)
