

# Liste des années et périodes à télécharger
annees <- c("2024", "2023Q4", "2023Q3", "2023Q2", "2023Q1", 
            "2022Q4", "2022Q3", "2022Q2", "2022Q1", 
            "2021Q4", "2021Q3", "2021Q2", "2021Q1", 
            "2020Q4", "2020Q3", "2020Q2", "2020Q1", 
            "2019Q4", "2019Q3", "2019Q2", "2019Q1", 
            "2018H1", "2017H1", "2016H1", "2015H1")

# Dossier de destination
destination_dir <- "Donnees projet/Data/Telechargement/"

# URL de base (exemple, remplacez par la vraie URL)
base_url <- "https://aqicn.org/data-platform/covid19/verify/bc4b7349-9163-4e58-a7f4-df91df6feaf4/waqi-covid19-airqualitydata-"

# Boucle pour télécharger tous les fichiers
for (periode in annees) {
  # Construire l'URL complète
  file_url <- paste0(base_url, periode, ".csv")
  
  # Construire le chemin de sauvegarde
  destination <- paste0(destination_dir, "waqi-covid19-airqualitydata-", periode, ".csv")
  
  # Télécharger le fichier
  tryCatch({
    download.file(file_url, destfile = destination, mode = "wb")
    cat("Téléchargé : ", file_url, "\n")
  }, error = function(e) {
    cat("Erreur : ", file_url, "\n")
  })
}
