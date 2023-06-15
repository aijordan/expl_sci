# sidebar
output$gameProgress <- renderUI({
  if (game$round > 1) {
    bar1 <- "Runde"
    bar2 <- "Punkte (letzte Runde)"
    bar3 <- "Gesamtpunktzahl"
    scoreLatest <- records$SCORE[game$round - 1]
    scoreSum <- sum(records$SCORE, na.rm = TRUE)
    colorLatest <- ifelse(scoreLatest >= 7500, "success", "danger")
    colorAverage <- ifelse(scoreSum / (game$round - 1) >= 7500, "success", "danger")
    
    div(
      sidebarProgress(bar1, "primary", sprintf("%i/%i", game$round, n_rounds), 1),
      sidebarProgress(bar2, colorLatest, scoreLatest, 10000),
      sidebarProgress(bar3, colorAverage, sprintf("%i", scoreSum), 10000 * n_rounds),
      column(width = 12, myActionButton("reset", "Neustart"))
    )
  }
})

sidebarProgress <- function(title, status, value, max, type = "xxs") {
  div(
    h4(
      class = "control-sidebar-subheading",
      title,
      span(
        class = sprintf("label label-%s pull-right", status),
        value
      )
    ),
    div(
      class = paste0("progress progress-", type),
      div(
        class = sprintf("progress-bar progress-bar-%s", status),
        style = sprintf("width: %i%%", as.integer(eval(parse(text = value))/max * 100))
      )
    )
  )
}
