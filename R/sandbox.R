# Englische Version der Scan-Animation starten
cat("\U0001f680 Booting MRI scanner... Starting Katharinas Fullerene quantum protocol!\n")

for (height in seq(-2.2, 2.2, length.out = 80)) {
  # Hier uebergeben wir lang = "en" fuer die englischen Titel!
  plot_mri_fullerene(z_slice = height, zeitpunkt = 1.0, lang = "en")

  Sys.sleep(0.05)
}

cat("Scan completed successfully. Data sealed in memory.\n")








# Animation: Der klassische Octawave Oktaeder-Scan
cat("\U0001f680 Starte Katharinas klassischen Octawave MRT-Scan...Starting Katharinas octawave MRI scan.. Oktaeder-Protokoll aktiv!\n")

for (height in seq(-2.2, 2.2, length.out = 80)) {
  # Ruft die neue klassische Oktaeder-Funktion auf
  plot_mri_octawave(z_slice = height, zeitpunkt = 1.0, lang = "de")

  # Kurze Atempause fuer ein fluessiges Bild
  Sys.sleep(0.05)
}

cat("Oktaeder-Scan beendet. Daten im Speicher versiegelt.\n")
