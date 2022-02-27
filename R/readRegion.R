

readRegion <- function(Region, Vergleichsdaten = "Deutschland", parameter = c("Fallzahl", "Inzidenz")){ 

  # Testingvariables  
    # Region = "Reinickendorf"
    # parameter = c("Fallzahl", "Inzidenz")
    # Vergleichsdaten = "Deutschland"
    
  regionFolder <- paste0("data/", Region, "/")
  vergleichFolder <- paste0("data/", Vergleichsdaten, "/")
  
  regionFallzahlFile <- paste0(regionFolder,list.files(regionFolder)[str_detect(list.files(regionFolder), parameter[1])])
  vergleichFallzahlFile <- paste0(vergleichFolder,list.files(vergleichFolder)[str_detect(list.files(vergleichFolder), parameter[1])])
  regionInzidenzFile <- paste0(regionFolder,list.files(regionFolder)[str_detect(list.files(regionFolder), parameter[2])])
  vergleichInzidenzFile <- paste0(vergleichFolder,list.files(vergleichFolder)[str_detect(list.files(vergleichFolder), parameter[2])])
  
  
  data <- bind_rows(readSurvstat(regionFallzahlFile), readSurvstat(vergleichFallzahlFile)) %>% 
    left_join(bind_rows(readSurvstat(regionInzidenzFile), readSurvstat(vergleichInzidenzFile)), 
              by = c("Krankheit", "Datum", "Quellenangabe", "Referenzdefinition", "Ort"))
  
  data
}