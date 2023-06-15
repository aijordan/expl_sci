time <- sprintf("%s%02d", format(game$today, "%Y%m%d"), input$gameHour)
eventData <- game$events[MESS_DATUM %in% as.integer(time)]

proxy <- leafletProxy("gameMap", data = eventData)

proxy %>%
  addCircleMarkers(
    lng = ~ LONGITUDE,
    lat = ~ LATITUDE,
    group = isolate(game$mapGroup),
    radius = ceiling(3 * 2 ^ input$gameMap_zoom / 2 ^ 6),
    weight = 1,
    color = "#FFFFFF",
    opacity = 0.5,
    fillOpacity = 1,
    fillColor = "#004f9f"
  )

isolate({
  if (game$mapGroup != "grp2")
    game$mapGroup <- "grp2"
  else
    game$mapGroup <- "grp1"
})

proxy %>%
  clearGroup(isolate(game$mapGroup)) %>%
  addCircleMarkers(
    lng = game$LONGITUDE,
    lat = game$LATITUDE,
    layerId = "stationmarker",
    radius = 4,
    weight = 3,
    color = "#ffcc00",
    opacity = 1,
    fillColor = "#c3006b",
    fillOpacity = 1
  )
