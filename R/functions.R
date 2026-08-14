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


#' @title Time-dependent Octahedron Quantum Wave / Zeitabhängige Oktaeder-Quantenwelle
#' @description Calculates the wave field at an exact time 't' to simulate dynamic movement.
#' Berechnet das Wellenfeld zu einem exakten Zeitpunkt t, um Bewegung zu simulieren.
#' @param x,y,z Spatial coordinates / Raumkoordinaten.
#' @param t Current time parameter to control wave progression / Aktuelle Zeit steuert das Fortschreiten.
#' @param freq Wave frequency / Schwingungsfrequenz.
#' @param radius Radius of the core octahedron / Radius des Oktaeders.
#' @param damping Damping factor of the quantum wave / Dämpfungsfaktor.
#' @export
octa_wave_time <- function(x, y, z, t = 0, freq = 2.0, radius = 1.5, damping = 0.1) {
  centers <- matrix(c(
    radius,  0,       0,
    -radius,  0,       0,
    0,       radius,  0,
    0,      -radius,  0,
    0,       0,       radius,
    0,       0,      -radius
  ), ncol = 3, byrow = TRUE)

  total_wave <- 0
  for(i in 1:nrow(centers)) {
    r <- sqrt((x - centers[i,1])^2 + (y - centers[i,2])^2 + (z - centers[i,3])^2)
    r <- ifelse(r < 0.01, 0.01, r)
    wave_component <- (sin(freq * r - t) / r) * exp(-damping * r)
    total_wave <- total_wave + wave_component
  }
  return(total_wave)
}

#' @title Floating 3D Octawave Visualizer / Schwebende 3D-Oktaeder-Quantenwelle
#' @description Plots the quantum wave field at a fixed time step as a beautiful 3D landscape.
#' Zeichnet das Wellenfeld zu einem festen Zeitpunkt als wunderschöne 3D-Landschaft.
#' @param zeitpunkt Fixed time step for the snapshot / Gewählter Zeitpunkt für die Momentaufnahme.
#' @export
plot_octawave_3d <- function(zeitpunkt = 0) {
  resolution <- 60
  space_grid <- seq(-5, 5, length.out = resolution)

  wave_matrix <- matrix(0, nrow = resolution, ncol = resolution)
  for(i in 1:resolution) {
    for(j in 1:resolution) {
      wave_matrix[i, j] <- octa_wave_time(space_grid[i], space_grid[j], z = 0, t = zeitpunkt, freq = 2.5, radius = 2.0)
    }
  }

  # Classic MATLAB-style jet color palette / Klassische MATLAB-Farben
  matlab_colors <- colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))(100)

  # Render the 3D perspective / 3D-Perspektive zeichnen
  persp(space_grid, space_grid, wave_matrix,
        theta = 35, phi = 30,
        expand = 0.6, ltheta = 120,
        shade = 0.45, tcl = -0.2,
        col = matlab_colors[cut(wave_matrix, 100)],
        main = paste("octawave 3D - Time / Zeit t =", round(zeitpunkt, 2)),
        xlab = "X-Quantum", ylab = "Y-Quantum", zlab = "Amplitude",
        border = NA)
}



#' @title Export 3D Octawave Snapshot Series / 3D-Bilder-Serie exportieren
#' @description Saves a series of 3D snapshots as PNG files to visualize the wave progression.
#' Speichert eine Serie von 3D-Momentaufnahmen als PNG-Dateien, um das Fortschreiten zu zeigen.
#' @param count Number of images to export / Anzahl der Bilder, die gespeichert werden sollen.
#' @export
export_octawave_series <- function(count = 5) {
  # Zeitpunkte von 0 bis zu einer halben Periode verteilen
  zeit_punkte <- seq(0, pi, length.out = count)

  for(i in 1:count) {
    # Erstellt einen sauberen Dateinamen, z.B. octawave_snapshot_1.png
    file_name <- paste0("octawave_snapshot_", i, ".png")

    # Öffnet den Bild-Kanal für den Mac
    png(file_name, width = 800, height = 800, res = 120)

    # Ruft deine bestehende 3D-Funktion auf, um das Bild im Kanal zu zeichnen
    plot_octawave_3d(zeitpunkt = zeit_punkte[i])

    # Schließt den Kanal und speichert die Datei sicher ab
    dev.off()
  }
  message(paste("Erfolgreich", count, "Bilder direkt im Projektordner gespeichert!"))
}


#' @title Interactive Drop Zone / Interaktives Mausklick-Cockpit
#' @description Allows the user to click into the plot to trigger a new wave at that exact coordinate.
#' Erlaubt es, per Mausklick eine neue Welle an der gewählten Koordinate auszulösen.
#' @export
interactive_octawave <- function() {
  resolution <- 100
  space_grid <- seq(-5, 5, length.out = resolution)

  # 1. Erste Basis-Welle bei t = 0 zeichnen
  wave_matrix <- matrix(0, nrow = resolution, ncol = resolution)
  for(i in 1:resolution) {
    for(j in 1:resolution) {
      wave_matrix[i, j] <- octa_wave_time(space_grid[i], space_grid[j], z = 0, t = 0, freq = 2.5, radius = 2.0)
    }
  }

  matlab_colors <- colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))(100)

  # Plot anzeigen und Anleitung ausgeben
  image(space_grid, space_grid, wave_matrix, col = matlab_colors, asp = 1,
        main = "Klicke in die Grafik für einen neuen Einschlag!",
        xlab = "X-Raum", ylab = "Y-Raum")

  message("Bitte klicke jetzt einmal mit der Maus irgendwo in das 'Plots'-Fenster...")

  # 2. Auf den Mausklick der Userin warten
  klick <- locator(n = 1)

  if(!is.null(klick)) {
    new_x <- klick$x
    new_y <- klick$y

    message(paste("Einschlag registriert bei X =", round(new_x, 2), "und Y =", round(new_y, 2)))

    # 3. Neue Matrix berechnen, zentriert um den Klick-Punkt
    new_matrix <- matrix(0, nrow = resolution, ncol = resolution)
    for(i in 1:resolution) {
      for(j in 1:resolution) {
        # Wir verschieben die Koordinaten relativ zum Klickpunkt
        dx <- space_grid[i] - new_x
        dy <- space_grid[j] - new_y
        new_matrix[i, j] <- octa_wave_time(dx, dy, z = 0, t = 1.2, freq = 2.5, radius = 2.0)
      }
    }

    # 4. Das neue, veränderte Wellenfeld zeichnen
    image(space_grid, space_grid, new_matrix, col = matlab_colors, asp = 1,
          main = paste("Neue Quantenwelle bei X =", round(new_x, 1), "Y =", round(new_y, 1)),
          xlab = "X-Raum", ylab = "Y-Raum")
  }
}


#' @title Interactive Multi-Drop 3D Zone / Interaktives 3D-Mehrfach-Klick-Cockpit
#' @description Allows the user to click 3 times to create overlapping waves in a 3D landscape.
#' Erlaubt 3 Mausklicks, um sich überlagernde Wellen in einer 3D-Landschaft zu erzeugen.
#' @export
interactive_multi_3d <- function() {
  resolution <- 60
  space_grid <- seq(-5, 5, length.out = resolution)
  matlab_colors <- colorRampPalette(c("blue", "cyan", "green", "yellow", "red"))(100)

  # Zeige eine flache Start-Ebene für die Klicks
  image(space_grid, space_grid, matrix(0, resolution, resolution), col = matlab_colors, asp = 1,
        main = "Klicke nacheinander 3x in die Grafik!", xlab = "X", ylab = "Y")

  message("Bitte klicke jetzt nacheinander 3x in das 'Plots'-Fenster...")

  # Wir sammeln 3 Klicks
  klicks <- list(x = c(), y = c())
  for(k in 1:3) {
    pt <- locator(n = 1)
    if(!is.null(pt)) {
      klicks$x <- c(klicks$x, pt$x)
      klicks$y <- c(klicks$y, pt$y)
      points(pt$x, pt$y, col = "white", pch = 19, cex = 1.5) # Zeigt den Klick live an
      message(paste("Klick", k, "registriert!"))
    }
  }

  # Berechne das kombinierte 3D-Wellenfeld aller 3 Einschläge
  wave_matrix <- matrix(0, nrow = resolution, ncol = resolution)
  for(i in 1:resolution) {
    for(j in 1:resolution) {
      total_val <- 0
      for(k in 1:length(klicks$x)) {
        dx <- space_grid[i] - klicks$x[k]
        dy <- space_grid[j] - klicks$y[k]
        total_val <- total_val + octa_wave_time(dx, dy, z = 0, t = 1.0, freq = 2.5, radius = 2.0)
      }
      wave_matrix[i, j] <- total_val
    }
  }

  # Zeichne Katas vollendetes 3D-Meisterwerk
  persp(space_grid, space_grid, wave_matrix,
        theta = 35, phi = 30, expand = 0.6, ltheta = 120, shade = 0.45,
        col = matlab_colors[cut(wave_matrix, 100, labels = FALSE)],
        main = "3D-Quantenraum nach 3 Einschlägen", border = NA)
}



#' @title Fullerene Gitternetz MRT-Scan / Pentagon-Hexagon Wave Slice
#' @description Scans a complex 5- and 6-edged quantum grid layer by layer with bilingual support.
#' @param z_slice Die Höhe des MRT-Schnitts (von -2.5 bis +2.5)
#' @param zeitpunkt Der aktuelle Zeitschritt der Welle (t)
#' @param lang Sprache für den Plot: "de" für Deutsch, "en" für Englisch
#' @export
plot_mri_fullerene <- function(z_slice = 0, zeitpunkt = 1.0, lang = "de") {
  # Goldener Schnitt für die Fullerene-Geometrie (C60)
  phi <- (1 + sqrt(5)) / 2

  # Basis-Koordinaten der 60 Ecken im 3D-Raum
  coords <- matrix(c(
    0, 1, 3*phi,   0, 1, -3*phi,  0, -1, 3*phi,  0, -1, -3*phi,
    1, 3*phi, 0,   1, -3*phi, 0,  -1, 3*phi, 0,  -1, -3*phi, 0,
    3*phi, 0, 1,   3*phi, 0, -1,  -3*phi, 0, 1,  -3*phi, 0, -1
  ), ncol = 3, byrow = TRUE)

  # Skalieren für den Scan-Bereich
  vertices <- coords / max(coords) * 2.0
  colnames(vertices) <- c("x", "y", "z")

  # Das 2D-Gitter für das Plots-Fenster
  grid_size <- 150
  x_vec <- seq(-3, 3, length.out = grid_size)
  y_vec <- seq(-3, 3, length.out = grid_size)
  grid <- expand.grid(X = x_vec, Y = y_vec)

  wellen_matrix <- matrix(0, nrow = grid_size, ncol = grid_size)

  # Interferenzwellen aller 60 Punkte aufschwingen
  for(i in 1:nrow(vertices)) {
    dx <- grid$X - vertices[i, "x"]
    dy <- grid$Y - vertices[i, "y"]
    dz <- z_slice - vertices[i, "z"]

    r <- sqrt(dx^2 + dy^2 + dz^2)
    amplitude <- sin(5 * r - zeitpunkt) / (r + 0.5)
    wellen_matrix <- wellen_matrix + matrix(amplitude, nrow = grid_size, ncol = grid_size)
  }

  # Zweisprachige Beschriftung auswählen
  if (lang == "en") {
    titel <- paste("Fullerene MRI Scan | Slice Height z =", round(z_slice, 2))
    x_lbl <- "X-Axis (Quantum Space)"
    y_lbl <- "Y-Axis (Quantum Space)"
  } else {
    titel <- paste("Fullerene MRT-Scan | Schichthöhe z =", round(z_slice, 2))
    x_lbl <- "X-Achse (Quantenraum)"
    y_lbl <- "Y-Achse (Quantenraum)"
  }

  # Zeichnen der aktuellen mathematischen Schicht
  image(x_vec, y_vec, wellen_matrix,
        col = terrain.colors(100),
        main = titel,
        xlab = x_lbl, ylab = y_lbl,
        asp = 1, axes = TRUE)
}
