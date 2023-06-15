output$gameMap <- renderLeaflet({
  leaflet(
    options = leafletOptions(
      minZoom = 5,
      maxZoom = 7,
      zoomControl = FALSE,
      dragging = TRUE,
      maxBounds = list(c(66.5, -33.75), c(32, 56.25))
    )
  ) %>%
    addTiles(
      urlTemplate = "map/Tiles/{z}/{x}/{y}.png",
      attribution = HTML('Map tiles by <a href="http://stamen.com">Stamen Design</a>, under <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>. Data by <a href="http://openstreetmap.org">OpenStreetMap</a>, under <a href="http://www.openstreetmap.org/copyright">ODbL</a>')
    ) %>%
    setView(11, 51.4, 6)
})

output$mapSlider <- renderUI({
  mapSlider("gameHour", textOutput("gameMapSliderLabel"))
})

output$stationPanel <- renderUI({
  stationPanel(game)
})

output$forecastPanel2 <- renderUI({
  forecastPanel(game)
})

mapSlider <- function(inputId, label) {
  div(
    style = "top: 12px; left: 25px; width: 50vw; min-width: 300px; height: 60px; position: absolute; background: white; box-shadow: 0px 0px 0px 2px #004f9f;",
    class = "well",
    fluidRow(
      div(
        class = "slider-animate-container",
        style = "width: 42px;",
        tags$a(
          href = "#",
          class = "slider-animate-button",
          'data-target-id' = "gameHour",
          'data-interval' = "125",
          'data-loop' = "FALSE",
          span(
            class = "play",
            div(
              style = "color: Black;",
              tags$i(
                'aria-label' = "play icon",
                class = "glyphicon glyphicon-play fa-2x",
                role="presentation"
              )
            )
          ),
          span(
            class = "pause",
            div(
              style = "color: Black;",
              tags$i(
                'aria-label' = "pause icon",
                class = "glyphicon glyphicon-pause fa-2x",
                role = "presentation"
              )
            )
          )
        )
      ),
      div(
        style = "position: relative; top: -45px; left: 50px; width: calc(100% - 70px);",
        sliderInput(
          inputId,
          label = NULL,
          min = 0,
          max = 23,
          value = 0,
          step = 1,
          ticks = FALSE,
          post = ":00",
          width = NULL,
        )
      )
    )
  )
}

stationPanel <- function(game) {
  location <- game$LOCATION
  Encoding(location) <- "latin1"
  season <- getSeason(game$tomorrow)
  div(style = "top: 79px; left: 25px; min-width: 110px; position: absolute; background: white; box-shadow: 0px 0px 0px 2px #004f9f;",
      class = "well",
      fluidRow(
        div(
          style = "margin-left: 5px;",
          tags$b("Station"), tags$br(), location),
        div(
          style = "margin-left: 5px;",
          tags$b("Jahreszeit"), tags$br(), HTML(season))
      )
  )
}
getSeason <- function(DATES, lang) {
  switch(strftime(DATES, format="%m"),
         '12' =,
         '01' =,
         '02' = "Winter",
         '03' =,
         '04' =,
         '05' = "Fr&uuml;hling",
         '06' =,
         '07' =,
         '08' = "Sommer",
         '09' =,
         '10' =,
         '11' = "Herbst")
}

forecastPanel <- function(game) {
  div(style = "top: 206px; left: 25px; width: 110px; height: 50vh; min-height: 300px; position: absolute; background: white; box-shadow: 0px 0px 0px 2px #004f9f;",
      class = "well",
      fluidRow(
        div(
          style = "margin-left: 5px;",
          tags$b("Vorhersage"),
          div(style = "font-size: 12px;", "(nächste 24 Stunden)")
        ),
        tags$br(),
        div(style = "text-align: center; height: calc(100% - 100px);",
            noUiSliderInput(
              inputId = "fcX",
              min = 0,
              max = 100,
              value = 50,
              step = 1,
              format = wNumbFormat(decimals = 0, suffix = " %"),
              orientation = "vertical",
              direction = "rtl",
              height = "calc(50vh - 180px)",
              color = "#004f9f"
            ),
            div(style = "margin-left: 5px; margin-right: 5px;",
                myActionButton("submit", "Abgeben"))
        )
      )
  )
}

evalModal <- function(game) {
  evalModalcol <- switch(
    game$status,
    primary = "aqua",
    success = "green",
    danger = "red"
  )
  evalModalvIcon1 <- switch(
    game$status,
    success = icon("thumbs-up"),
    danger = icon("thumbs-down")
  )
  location <- game$LOCATION
  Encoding(location) <- "latin1"
  modalDialog(
    fluidRow(
      valueBox(
        width = 12,
        icon = icon("bullseye", "fa-lg"),
        color = "light-blue",
        location,
        sprintf("(%s)", game$tomorrow)
      ),
      ###
      valueBox(
          width = 4,
          color = "light-blue",
          icon = icon("eye-open", lib = "glyphicon"),
          paste0(game$forecast, "%"),
          "Vorhersage"
      ),
      valueBox(
          width = 4,
          icon = icon("tint"),
          color = "light-blue",
          paste0(game$obs, " mm"),
          "Niederschlag"
      ),
      valueBox(
        width = 4,
        icon = evalModalvIcon1,
        color = evalModalcol,
        game$score,
        "Punkte"
      ),
      ###
      column(
        width = 12,
        tags$button(
          id = "nextDate",
          type = "button",
          style = "height: 40px;",
          class = "col-sm-12 small-box bg-light-blue action-button",
          `data-val` = restoreInput(id = "nextDate", default = NULL),
          list("Weiter")
        )
      )
    ),
    footer = NULL
  )
}
