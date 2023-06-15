output$finalScore <- renderUI({
  if (game$state == "End") {
    fluidRow(
      myValueBox(
        div(
          style = "margin-top: 15px; margin-bottom: 17px;",
          h3(sprintf("%i", sum(isolate(records$SCORE))))
        ),
        p("Gesamtpunktzahl"),
        color = "light-blue",
        width = 4
      ),
      myValueBox(
        div(
          style = "margin-left: 10px; margin-right: 10px;",
          h4(uiOutput("calibrationBar"))),
        p("Kalibration"),
        color = "light-blue",
        width = 4
      ),
      myValueBox(
        div(
          style = "margin-left: 10px; margin-right: 10px; margin-bottom: 36px;",
          h4(uiOutput("sharpnessBar"))),
        p("Schärfe"),
        color = "light-blue",
        width = 4
      ),
      box(
        title = "Spielverlauf",
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        plotOutput("endGraph", height = "300px")
      ),
      column(width = 12, myActionButton("reset2", "Neues Spiel"))
      
    )
  }
})

output$endGraph <- renderPlot({
  if (game$state %in% c("End")) {
    plotData <- data.frame(
      Round = seq_len(game$round),
      Forecast = records$FORECAST,
      Precipitation = records$NIEDERSCHLAGSHOEHE > 0
    )
      
    ggplot(plotData, aes(x = Round, y = Forecast)) +
      geom_tile(
        data = dplyr::filter(plotData, Precipitation),
        aes(y = 50, height = 100, width = .8),
        fill = "#004f9f") +
      geom_line() +
      geom_point(size = 2.5) +
      ylim(0, 100) +
      xlab("Runde") +
      ylab("Vorhersage (in %)") +
      scale_x_discrete(breaks = plotData$Round, labels = plotData$round) +
      theme_grey(base_size = 14) +
      theme(
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA)
      )
  }
},
  bg = "transparent"
)

output$calibrationBar <- renderUI({
  v1 <- mean(records$FORECAST)
  l1 <- sprintf("%i%%", round(v1, 0))
  v2 <- sum(records$NIEDERSCHLAGSHOEHE > 0)
  l2 <- sprintf("%i/%i", v2, n_rounds)
  endBars(v1, l1, v2 * round(100 / n_rounds), l2)
})

output$sharpnessBar <- renderUI({
  v1 <- mean(abs(records$FORECAST - 50))
  l1 <- round(v1, 0)
  endBars(2*v1, l1)
})

myValueBox <- function(value, subtitle, icon = NULL, color = "aqua", width = 4, href = NULL) {
  shinydashboard:::validateColor(color)
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  boxContent <- div(
    class = paste0("small-box bg-", color),
    style = "height: 120px;",
    div(
      class = "inner",
      value,
      subtitle
    ),
    if (!is.null(icon))
      div(class = "icon-large", icon)
  )
  if (!is.null(href))
    boxContent <- a(href = href, boxContent)
  div(class = if(!is.null(width)) paste0("col-sm-", width), boxContent)
}

endBars <- function(v1, l1, v2 = NULL, l2 = NULL) {
  label1 <- div(
    style = "text-align: center;",
    div(
      style = sprintf("position: relative; left: %f%%", v1 - 50),
      div(
        class = "label",
        l1
      )
    ),
    div(
      style = sprintf("height: 6px; position: relative; left: %f%%", v1 - 50),
      div(class = "above-tip")
    )
  )
  
  if (is.null(v2) | is.null(l2)) {
    return(
      tagList(
        label1,
        div(
          class = "progress progress-xxs",
          div(class = "progress-bar",
              style = sprintf("width: %f%%; background-color: white;", v1)),
          div(class = "progress-bar progress-bar-danger",
              style = sprintf("width: %f%%;", 100 - v1))
        )
      )
    )
  }
  
  label2 <- div(
    style = "text-align: center;",
    div(
      style = sprintf("height: 6px; position: relative; left: %f%%", v2 - 50),
      div(class = "below-tip")
    ),
    div(
      style = sprintf("position: relative; top: -3px; left: %f%%", v2 - 50),
      div(
        class = "label",
        l2
      )
    )
  )
  
  tagList(
    label1,
    div(
      class = "progress progress-xxs", style = "margin-bottom: 0;",
      div(
        class = "progress-bar",
        style = sprintf("width: %f%%; background-color: white;", min(v1, v2))
      ),
      div(
        class = "progress-bar progress-bar-danger",
        style = sprintf("width: %f%%;", max(v1, v2) - min(v1, v2))
      ),
      div(
        class = "progress-bar",
        style = sprintf("width: %f%%; background-color: white;", 100 - max(v1, v2))
      )
    ),
    label2
  )
}
