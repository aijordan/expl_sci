tiles_params <- list(
  z5 = list(z = 5, x = 13:20, y = 8:12),
  z6 = list(z = 6, x = 26:41, y = 16:25),
  z7 = list(z = 7, x = 52:83, y = 32:51)
)

urls_template <- "https://stamen-tiles.a.ssl.fastly.net/terrain-background/%i/%i/%i.png"
destdirs_template <- "shiny-app/www/map/Tiles/%i/%i/"
destfiles_template <- paste0(destdirs_template, "%i.png")

makePathList <- function(ll) {
  f <- \(y, x, z, template) sprintf(template, z, x, y)
  with(ll, cbind(
    src = outer(y, x, FUN = f, z = z, template = urls_template) |> as.vector(),
    dest = outer(y, x, FUN = f, z = z, template = destfiles_template) |> as.vector()
  ))
}

createdirs <- function(ll) {
  f <- \(x, z, template) sprintf(template, z, x)
  src <- with(ll, f(x, z, destdirs_template))
  lapply(src, dir.create, recursive = TRUE)
}

lapply(tiles_params, createdirs)
srcdest <- do.call(rbind, lapply(tiles_params, makePathList))
curl::multi_download(urls = srcdest[, "src"], destfiles = srcdest[, "dest"], resume = TRUE)
