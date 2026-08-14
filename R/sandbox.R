# Englische Version der Scan-Animation starten
cat("🚀 Booting MRI scanner... Starting Katharina’s Fullerene quantum protocol!\n")

for (hoehe in seq(-2.2, 2.2, length.out = 80)) {
  # Hier übergeben wir lang = "en" für die englischen Titel!
  plot_mri_fullerene(z_slice = hoehe, zeitpunkt = 1.0, lang = "en")

  Sys.sleep(0.05)
}

cat("🛡️ Scan completed successfully. Data sealed in memory.\n")
