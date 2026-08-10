#' Geometrische Grundstruktur des Quanten-Oktaeders / Geometric Core Structure of the Quantum Octahedron
#'
#' @description
#' [DEUTSCH] Berechnet die exakten 3D-Koordinaten der 6 Oktaeder-Scheitelpunkte im Raum.
#'
#' [ENGLISH] Computes the exact 3D coordinates of the 6 octahedral vertices in space.
#'
#' @param radius Der Abstand der Ecken vom Nullpunkt (Skalierung des Quanten-Kerns) / The distance of the vertices from the origin (scaling of the quantum core).
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

#' Neuartige Oktaeder-Quantenwelle mit Raumdämpfung / Novel Octahedral Quantum Wave with Spatial Damping
#'
#' @description
#' [DEUTSCH] Berechnet die Superposition von Wellen, die von den 6 Ecken eines Oktaeders ausgestrahlt werden, inklusive einer quantenmechanischen Amplitudendämpfung.
#'
#' [ENGLISH] Computes the superposition of waves radiated from the 6 vertices of an octahedron, including quantum-mechanical amplitude damping.
#'
#' @param x,y,z Die Koordinaten des Punktes im Raum / The coordinates of the point in space.
#' @param freq Die Schwingungsfrequenz der Wellenkomponenten / The vibration frequency of the wave components.
#' @param radius Der Radius des zugrundeliegenden Oktaeders / The radius of the underlying octahedron.
#' @param damping Der Dämpfungsfaktor (wie schnell die Welle im Raum abklingt) / The damping factor (how fast the wave decays in space).
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

#' Visualisierung der octawave Quantenwelle / Visualization of the octawave Quantum Wave
#'
#' @description
#' [DEUTSCH] Erstellt einen automatischen 2D-Schnitt durch das Oktaeder-Wellenfeld im klassischen MATLAB-Stil.
#'
#' [ENGLISH] Generates an automated 2D cross-section through the octahedral wave field in classic MATLAB style.
#'
#' @param resolution Die Auflösung des Gitters (höhere Werte bedeuten feinere Bilder) / The grid resolution (higher values yield finer images).
#' @param range Der sichtbare Bereich auf der X- und Y-Achse / The visible range on the X and Y axes.
#' @export
plot_octawave <- function(resolution = 200, range = 8) {
  space_grid <- seq(-range, range, length.out = resolution)
  wave_matrix <- matlab::zeros(resolution, resolution)

  for(i in 1:resolution) {
    for(j in 1:resolution) {
      wave_matrix[i, j] <- octa_wave_field(space_grid[i], space_grid[j], z = 0, freq = 2.5, radius = 2.0, damping = 0.15)
    }
  }

  matlab::imagesc(wave_matrix, main = "octawave - Lokale Feld-Resonanz (z = 0)")
}

#' Interaktive 3D-Visualisierung der Oktaeder-Welle / Interactive 3D Visualization of the Octahedral Wave
#'
#' @description
#' [DEUTSCH] Erstellt ein dreidimensionales, interaktives Volumenmodell des Quantenfeldes.
#'
#' [ENGLISH] Generates a three-dimensional, interactive volume model of the quantum field.
#'
#' @param resolution Die Feinheit des 3D-Gitters (z.B. 30 oder 40) / The density of the 3D grid (e.g., 30 or 40).
#' @param range Der sichtbare Raumbereich (X, Y und Z) / The visible spatial range (X, Y, and Z).
#' @export
plot_octawave_3d <- function(resolution = 35, range = 5) {
  if(!requireNamespace("plotly", quietly = TRUE)) {
    stop("Bitte installiere plotly: install.packages('plotly')")
  }

  gitter_3d <- seq(-range, range, length.out = resolution)
  grid <- expand.grid(x = gitter_3d, y = gitter_3d, z = gitter_3d)

  grid$val <- mapply(octa_wave_field, grid$x, grid$y, grid$z,
                     MoreArgs = list(freq = 2.0, radius = 1.8, damping = 0.1))

  p <- plotly::plot_ly(grid, x = ~x, y = ~y, z = ~z, value = ~val,
                       type = "volume", opacity = 0.15, surface = list(count = 6))
  p <- plotly::layout(p, title = "octawave - Interaktives 3D Quantenfeld")
  return(p)
}
