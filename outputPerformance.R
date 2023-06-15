output$performanceExplanation <- renderUI({
  fluidRow(
    box(
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      title = "Wie misst man die Vorhersagequalität?",
      h4("Je höher die Punktzahl, desto besser die Vorhersage!"),
      p("Bewertungsfunktionen messen, wie gut eine Vorhersage kalibriert ist, aber auch, wie scharf sie ist. Die Qualität wird am Ende mit einer Gesamtpunktzahl gemessen."),
      hr(),
      div(style = "padding-left: 10px;", h4("Regenwahrscheinlichkeit")),
      div(
        style = "position: relative; top: -10px; left: -10px; padding-left: 10px; padding-right: 10px; width: calc(100% + 10px);",
        noUiSliderInput(
          inputId = "scoreSlider",
          min = 0,
          max = 100,
          value = 50,
          step = 1,
          format = wNumbFormat(decimals = 0, suffix = " %"),
          width = "100%",
          color = "#004f9f"
        )
      ),
      hr(),
      div(
        style = "padding-left: 10px; padding-right: 10px;",
        uiOutput("scoreBars")
      )
    ),
    ###
    box(
      title = "Wann sind Vorhersagen gut?",
      width = 12,
      status = "primary",
      solidHeader = TRUE,
      h4("Von zwei kalibrierten Vorhersagen, ist die schärfere auch die bessere!"),
      hr(),
      fluidRow(
        column(
          width = 6,
          h4("Kalibration"),
          div(
            class = "shiny-plot-output html-fill-item",
            id = "calibrationplot",
            style = "width: 100%; height: 150px; max-width: 300px;"
          ),
          p("Kalibration ist die statistische Vereinbarkeit von Vorhersagen und Beobachtungen."),
          p("Versuche die durchschnittliche Vorhersagewahrscheinlichkeit an die Niederschlagshäufigkeit anzupassen.")
        ),
        column(
          width = 6,
          h4("Schärfe"),
          div(
            class = "shiny-plot-output html-fill-item",
            id = "sharpnessplot",
            style = "width: 100%; height: 150px; max-width: 300px;"
          ),
          p("Schärfe ist die Sicherheit darüber, ob es regnen wird oder nicht."),
          p("Versuche möglichst scharfe Vorhersagen abzugeben, ohne dabei übermütig zu werden.")
        )
      )
    ),
    ###
    box(
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      title = span(
        style = "padding-right: 10px;",
        'data-widget' = "collapse",
        "Wie erreiche ich im Durchschnitt die meisten Punkte?"
      ),
      p("Versuche deine Vorhersage so zu treffen, dass bei einer Vorhersage von zum Beispiel 33% Regenwahrscheinlichkeit auch im Mittel jedes dritte Mal Regen eintritt. Gibst du eine Vorhersage von 99% ab, so darf es im Mittel auch nur jedes hundertste Mal regnen."),
      p("Wetterdienste nutzen die gleiche Bewertungsregel wie in diesem Spiel. Damit können verschiedene Modelle verglichen werden und das zuverlässigste wird als das beste bewertet.")
    ),
    ###
    box(
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,
      title = span(
        style = "padding-right: 10px;",
        'data-widget' = "collapse",
        "Warum bekomme ich für eine 50% Vorhersage so viele Punkte?"
      ),
      p("Manchmal bist du dir vielleicht sehr sicher, dass es regnen wird, liegst schließlich aber daneben. In so einem Fall darf es nicht zu einfach sein trotzdem viele Punkte zu bekommen."),
      p("Überlege dir immer, was für Trockenheit oder Regen spricht. Es ist sehr schwierig vorherzusagen, ob es an einem ganz bestimmten Ort regnen wird. Denke daran, dass das nur selten mit absoluter Sicherheit geht.")
    )
  )
})

output$scoreBars <- renderUI({
  pop <- input$scoreSlider
  s0 <- 10000 - pop ^ 2
  c0 <- ifelse(s0 >= 7500, "success", "danger")
  s1 <- 10000 - (100 - pop) ^ 2
  c1 <- ifelse(s1 >= 7500, "success", "danger")
  
  div(
    sidebarProgress("Punkte (bei Trockenheit)", c0, s0, 10000, "xs"),
    sidebarProgress("Punkte (bei Regen)", c1, s1, 10000, "xs")
  )
})

output$sharpnessplot <- renderPlot({
  df <- data.frame(fc = c(0, 50, 100), s = c(50, 0, 50))
  ggplot(df, aes(fc, ymax = s, ymin = 0)) +
    geom_line(aes(y = s)) +
    theme_grey(base_size = 14) +
    theme(
      aspect.ratio = 0.5,
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA)
    ) +
    xlab("Vorhersage (in %)") +
    ylab("") +
    scale_y_continuous(
      breaks = c(0, 25, 50),
      sec.axis = sec_axis(
        trans = ~ .,
        breaks = c(0, 25, 50),
        labels = c(
          "unsicher",
          "eher\nsicher",
          "absolut\nsicher"
        )
      )) +
    scale_x_continuous(
      breaks = c(0, 25, 50, 75, 100)
    )
}, bg = "transparent")

output$calibrationplot <- renderPlot({
  width <- 5
  height <- 0.1
  df <- rbind(
    f(0, 3, width, height),
    f(25, 8, width, height),
    f(50, 6, width, height),
    f(75, 4, width, height),
    f(100, 2, width, height)
  )
  ggplot(df, aes(fc, obs)) +
    geom_point(size = 2) +
    theme_grey(base_size = 14) +
    theme(
      aspect.ratio = 0.5,
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.minor = element_blank()
    ) +
    ylab("") +
    xlab("Vorhersage (in %)") +
    scale_y_continuous(
      breaks = c(0, 1),
      label = c("Trockentage", "Regentage"),
      position = "right"
    ) +
    scale_x_continuous(
      breaks = c(0, 25, 50, 75, 100)
    )
}, bg = "transparent")

f <- function(fc, n, width = 1, height = 0.01) {
  n0 <- (100 - fc) * n / 100
  n1 <- fc * n / 100
  i0 <- which.min(n0 > c(1, 3, 6, 10, 15))
  i1 <- which.min(n1 > c(1, 3, 6, 10, 15))
  data.frame(
    fc = fc + width * c(fx(n0, i0), fx(n1, i1)),
    obs = c(height * 0.5 * sqrt(3) * fy(n0, i0, 0),
                1 - height * 0.5 * sqrt(3) * fy(n1, i1, 0))
  )
}
fx <- function(n, m) {
  if (!n) return(NULL)
  c(seq_len(m) - 0.5 * (m + 1), fx(n - m, m - 1))
}
fy <- function(n, m, lvl) {
  if (!n) return(NULL)
  c(rep(lvl, m), fy(n - m, m - 1, lvl + 1))
}
