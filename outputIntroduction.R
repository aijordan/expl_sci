output$gameIntroduction <- renderUI({
    fluidRow(
      box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = "Ziel des Spiels",
        h4("Sage voraus, ob es regnen wird, und sammle dabei möglichst viele Punkte!"),
      ),
      ###
      box(
        width = 12,
        solidHeader = TRUE,
        status = "primary",
        title = "So funktioniert das Spiel",
        tabsetPanel(
          tabPanel(
            "Schritt 1",
            tags$img(
              src = "Intro1.png", 
              style = "width: 100%; max-height: 448px; max-width: 544px;"
            ),
            div(
              h4("Finde die Station, für die eine Vorhersage gemacht werden soll.")
            )
            
          ),
          tabPanel(
            "Schritt 2",
            tags$img(
              src = "Intro2.png",
              style = "width: 100%; max-height: 448px; max-width: 544px;"
            ),
            div(
              h4("Schaue dir die letzten 24 Stunden an Daten an."),
              h4("An wievielen Orten hat es überhaupt geregnet? Und wie bewegt sich das Regenfeld?")
            )
          ),
          tabPanel(
            "Schritt 3",
            tags$img(
              src = "Intro3.png",
              style = "width: 100%; max-height: 448px; max-width: 544px;"
            ),
            div(
              h4("Gib eine Vorhersage darüber ab, wie hoch die Wahrscheinlichkeit ist, dass es an der Station in den nächsten 24 Stunden regnen wird.")
            )
          )
        )
      ),
      ###
      column(
        width = 12,
        tags$a(
          href = "#shiny-tab-game",
          `data-toggle` = "tab",
          `data-value` = "game",
          myActionButton("start", "Hier geht's los")
        )
      )
    )
})
