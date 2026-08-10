#' @title Geometrische Grundstruktur des Quanten-Oktaeders
#' @description Berechnet die exakten 3D-Koordinaten der 6 Oktaeder-Scheitelpunkte im Raum.
#' @param radius Der Abstand der Ecken vom Nullpunkt (Skalierung des Quanten-Kerns).
#' @export
octa_vertices <- function(radius = 1.0) {
  vertices <- matrix(c(
    radius,  0.0,  0.0,
    -radius,  0.0,  0.0,
    0.0,  radius,  0.0,
    0.0, -radius,  0.0,
    0.0,  0.0,  radius,
    0.0,  0.0, -radius
  ), ncol = 3, byrow = TRUE)
  colnames(vertices) <- c("x", "y", "z")
  return(vertices)
}
#'
#' @title neuartige Oktaeder-Quantenwelle mit Raumdämpfung
#' @description Berechnet die Superposition von Wellen, die von den 6 Ecken eines Oktaeders ausgestrahlt werden, inklusive einer quantenmechanischen Amplitudendämpfung.
#' @param x,y,z Die Koordinaten des Punktes im Raum.
#' @param freq Die Schwingungsfrequenz der Wellenkomponenten.
#' @param radius Der Radius des zugrundeliegenden Oktaeders.
#' @param damping Der Dämpfungsfaktor (wie schnell die Welle im Raum abklingt).
#' @export
octa_wave_field <- function(x, y, z, freq = 2.0, radius = 1.5, damping = 0.2) {
  centers <- octa_vertices(radius)
  total_wave <- 0

  for(i in 1:nrow(centers)) {
    r <- sqrt((x - centers[i,"x"])^2 + (y - centers[i,"y"])^2 + (z - centers[i,"z"])^2)
    if(r < 0.01) r <- 0.01

    wave_component <- (sin(freq * r) / r) * exp(-damping * r)
    total_wave <- total_wave + wave_component
  }
  return(total_wave)
}
#'
#
#' @title Visualisierung der octawave Quantenwelle
#' @description Erstellt einen automatischen 2D-Schnitt durch das Oktaeder-Wellenfeld im klassischen MATLAB-Stil.
#' @param resolution Die Auflösung des Gitters (höhere Werte bedeuten feinere Bilder).
#' @param range Der sichtbare Bereich auf der X- und Y-Achse.
#' @export
plot_octawave <- function(resolution = 200, range = 8) {
  # Gitter aufbauen
  space_grid <- seq(-range, range, length.out = resolution)
  wave_matrix <- matlab::zeros(resolution, resolution)

  # Matrix mit meiner octa_wave_field Formel füllen
  for(i in 1:resolution) {
    for(j in 1:resolution) {
      wave_matrix[i, j] <- octa_wave_field(space_grid[i], space_grid[j], z = 0, freq = 2.5, radius = 2.0, damping = 0.15)
    }
  }

  # Den Plot im MATLAB-Stil ausgeben
  matlab::imagesc(wave_matrix, main = "octawave - Lokale Feld-Resonanz (z = 0)")
}

#' @title Interaktive 3D-Visualisierung der Oktaeder-Welle
#' @description Erstellt ein dreidimensionales, interaktives Volumenmodell des Quantenfeldes.
#' @param resolution Die Feinheit des 3D-Gitters (z.B. 30 oder 40).
#' @param range Der sichtbare Raumbereich (X, Y und Z).
#' @export
plot_octawave_3d <- function(resolution = 35, range = 5) {
  if(!requireNamespace("plotly", quietly = TRUE)) {
    stop("Bitte installiere plotly: install.packages('plotly')")
  }

  # 3D-Gitter aufspannen
  gitter_3d <- seq(-range, range, length.out = resolution)
  grid <- expand.grid(x = gitter_3d, y = gitter_3d, z = gitter_3d)

  # Wellenwerte für jeden Punkt im Raum berechnen
  grid$val <- mapply(octa_wave_field, grid$x, grid$y, grid$z,
                     MoreArgs = list(freq = 2.0, radius = 1.8, damping = 0.1))

  # Der interaktive 3D-Plot (ohne %>% Pfeil)
  p <- plotly::plot_ly(grid, x = ~x, y = ~y, z = ~z, value = ~val,
                       type = "volume", opacity = 0.15, surface = list(count = 6))
  p <- plotly::layout(p, title = "octawave - Interaktives 3D Quantenfeld")
  return(p)
}


