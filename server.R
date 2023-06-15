library(shiny)
library(leaflet)
library(ggplot2)
library(data.table)

trackfiles <- dir("data", pattern = ".rds", full.names = TRUE)

svUpdateLeaflet <- parse("svUpdateLeaflet.R")

outputMain <- parse("outputMain.R")
outputIntroduction <- parse("outputIntroduction.R")
outputPerformance <- parse("outputPerformance.R")
outputForecasting <- parse("outputForecasting.R")
outputResults <- parse("outputResults.R")

myActionButton <- function(inputId, label, icon = NULL, ...) {
  value <- restoreInput(id = inputId, default = NULL)
  tags$button(
    id = inputId,
    type = "button",
    style = "width: 100%; height: 40px;",
    class = "small-box bg-light-blue action-button",
    `data-val` = value,
    list(shiny:::validateIcon(icon), label),
    ...
  )
}

function(input, output, session) {
  
  ## variables
  
  site <- reactiveValues(lang = "DE")
  
  n_rounds <- 10
  
  records <- reactiveValues(
    DRAWS = sample(trackfiles, n_rounds),
    MESS_DATUM = rep(NA, n_rounds),
    FORECAST = rep(NA, n_rounds), 
    NIEDERSCHLAGSHOEHE = rep(NA, n_rounds),
    SCORE = rep(NA, n_rounds)
  )
  
  initialDraw <- isolate(readRDS(records$DRAWS[1]))
  
  game <- reactiveValues(
    state = "Forecasting",
    status = "primary",
    mapGroup = "grp1", # variable for smoother map animation
    round = 1,
    LOCATION = initialDraw$LOCATION,
    LATITUDE = initialDraw$LATITUDE,
    LONGITUDE = initialDraw$LONGITUDE,
    today = initialDraw$today,
    tomorrow = initialDraw$tomorrow,
    precipitation = initialDraw$precipitation,
    obs = initialDraw$obs,
    events = initialDraw$precipitation[NIEDERSCHLAGSHOEHE > 0]
  )
  
  ## observer
  
  observeEvent({
    input$reset
  }, {
    game$state <- "Forecasting"
    records$DRAWS <- sample(trackfiles, n_rounds)
    records$MESS_DATUM <- rep(NA, n_rounds)
    records$FORECAST <- rep(NA, n_rounds)
    records$NIEDERSCHLAGSHOEHE <- rep(NA, n_rounds)
    records$SCORE <- rep(NA, n_rounds)
    
    drawn <- readRDS(records$DRAWS[1])
    game$round <- 1
    game$today <- drawn$today
    game$tomorrow <- drawn$tomorrow
    game$precipitation <- drawn$precipitation
    game$LOCATION <- drawn$LOCATION
    game$LATITUDE <- drawn$LATITUDE
    game$LONGITUDE <- drawn$LONGITUDE
    game$obs <- drawn$obs
    game$events <- drawn$precipitation[NIEDERSCHLAGSHOEHE > 0]
    
    updateSliderInput(session, "gameHour", value = 0)
    eval(svUpdateLeaflet)
  })
  observeEvent({
    input$reset2
  }, {
    game$state <- "Forecasting"
    records$DRAWS <- sample(trackfiles, n_rounds)
    records$MESS_DATUM <- rep(NA, n_rounds)
    records$FORECAST <- rep(NA, n_rounds)
    records$NIEDERSCHLAGSHOEHE <- rep(NA, n_rounds)
    records$SCORE <- rep(NA, n_rounds)
    
    drawn <- readRDS(records$DRAWS[1])
    game$round <- 1
    game$today <- drawn$today
    game$tomorrow <- drawn$tomorrow
    game$precipitation <- drawn$precipitation
    game$LOCATION <- drawn$LOCATION
    game$LATITUDE <- drawn$LATITUDE
    game$LONGITUDE <- drawn$LONGITUDE
    game$obs <- drawn$obs
    game$events <- drawn$precipitation[NIEDERSCHLAGSHOEHE > 0]
    
    updateSliderInput(session, "gameHour", value = 0)
    eval(svUpdateLeaflet)
  })
  
  observeEvent({
    input$submit
  }, {
    game$state <- "Evaluation"
    game$score <- 10000 - (game$forecast - 100 * (game$obs > 0)) ^ 2
    game$status <- ifelse(game$score >= 7500, "success", "danger")
    records$MESS_DATUM[game$round] <- game$tomorrow
    records$FORECAST[game$round] <- game$forecast
    records$NIEDERSCHLAGSHOEHE[game$round] <- game$obs
    records$SCORE[game$round] <- game$score
    
    updateSliderInput(session, "gameHour", value = 0)
    showModal(evalModal(game))
  })
               
  observeEvent({
    input$nextDate
  }, {
    removeModal()
    if (game$round < n_rounds) {
      game$state <- "Forecasting"
      game$round <- game$round + 1
      game$status <- "primary"
      drawn <- readRDS(records$DRAWS[game$round])
      game$today <- drawn$today
      game$tomorrow <- drawn$tomorrow
      game$precipitation <- drawn$precipitation
      game$LOCATION <- drawn$LOCATION
      game$LATITUDE <- drawn$LATITUDE
      game$LONGITUDE <- drawn$LONGITUDE
      game$obs <- drawn$obs
      game$events <- drawn$precipitation[NIEDERSCHLAGSHOEHE > 0]
      
      updateSliderInput(session, "gameHour", value = 0)
      eval(svUpdateLeaflet)
    } else {
      game$state <- "End"
    }
  })
  
  observeEvent({
    input$gameHour
  }, {
    eval(svUpdateLeaflet)
  })
  
  observeEvent({
    input$gameMap_zoom
  }, {
    eval(svUpdateLeaflet)
  })
  
  observeEvent({
    input$fcX
  }, {
    game$forecast <- input$fcX
  })
  
  # output
  
  eval(outputMain)
  eval(outputIntroduction)
  eval(outputPerformance)
  eval(outputForecasting)
  eval(outputResults)
  
  output$gameState <- renderText(game$state)
  outputOptions(output, "gameState", suspendWhenHidden = FALSE)
  outputOptions(output, "gameMap", suspendWhenHidden = FALSE)
}