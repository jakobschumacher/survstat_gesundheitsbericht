
readSurvstat <- function(file){
  
  # Unzipping of the given file
  unzip(file)
  
  # Read in the Info.pdf that goes along with every Survstat-Zip-File
  metadata <- pdf_text("Info.pdf") %>% str_replace_all("\n", "")
  Datenstand <- str_match(metadata, "Abfragezeitpunkt (.*?) Sprache")[2]
  Datenstand <- str_trim(Datenstand)
  Spalte <- str_match(metadata, "Spaltenmerkmal (.*?) Anzeigeoptionen")[2]
  Spalte <- str_replace_all(Spalte, "\\s|-", "")
  Zeile <- str_match(metadata, "Zeilenmerkmal  (.*?) Spaltenmerkmal")[2]
  Zeile <- str_replace_all(Zeile, "\\s|-", "")
  Filter <- str_match(metadata, "Filtereinstellungen  (.*?) Zeilenmerkmal")[2]
  Filter <- str_split(Filter, "\\s{5}", simplify = TRUE) 
  Filter <- str_subset(Filter, pattern = "[:alnum:]")
  Filter <- str_trim(Filter)
  Referenzdefinition <- ifelse(str_detect(paste(Filter, collapse = ""), "Referenzdefinition"), Filter[(str_which(Filter, "Referenzdefinition") + 1)], "Nein")
  Ort <- ifelse(str_detect(paste(Filter, collapse = ""), "Bundesland"), Filter[(str_which(Filter, "Bundesland") + 1)], "Deutschland")
  Anzeigeoptionen <- str_match(metadata, "Anzeigeoptionen (.*?) Datenstand zur")[2]
  Anzeigeoptionen <- str_split(Anzeigeoptionen, "\\s{5}", simplify = TRUE)
  Anzeigeoptionen <- str_subset(Anzeigeoptionen, pattern = "[:alnum:]")
  Maßzahl <- Anzeigeoptionen[str_which(Anzeigeoptionen, "Inzidenz anstatt Anzahl anzeigen")]
  Maßzahl <- ifelse(str_trim(str_split(Maßzahl, "\\:", simplify = TRUE)[[2]]) == "Nein", "Anzahl", "Inzidenz")
  Quellenangabe <- str_split(metadata, "»|«", simplify = TRUE)[[2]]
  
  # Read in the data file 
  data <- read.delim("Data.csv", sep = "\t", skip = 1, fileEncoding = "UTF-16LE", blank.lines.skip = TRUE, stringsAsFactors = FALSE)
  
  # Remove empty line which gets introduced by reading the survstat format file
  data <- data[-c(1),] 
  
  # Tidy up
  data <- data %>% 
    gather(key = "column", value = "n", -X) %>% 
    mutate(n = ifelse(n == "", 0, n)) %>% 
    mutate(n = ifelse(is.na(n), 0, n)) %>% 
    mutate(n = str_replace(n, ",", ".")) %>%  
    mutate(n = as.numeric(n)) %>% 
    mutate(X = as.factor(X)) %>% 
    rename(!!Spalte := column) %>% 
    rename(!!Maßzahl := n) %>% 
    rename(!!Zeile := "X") %>% 
    mutate(Quellenangabe = Quellenangabe)
  
  
  # Change Meldejahr mit Woche to date
  if (str_detect(paste(names(data), collapse = ""), "Meldejahrmitwoche")) {
    data <- data %>% 
      mutate(Meldejahrmitwoche = str_replace(Meldejahrmitwoche, ".K", "-")) %>% 
      mutate(Meldejahrmitwoche = str_replace(Meldejahrmitwoche, "X", "")) %>% 
      mutate(Meldejahrmitwoche = paste0(Meldejahrmitwoche, "-", 3)) %>% 
      mutate(Meldejahrmitwoche = ISOweek::ISOweek2date(Meldejahrmitwoche)) 
  }
  
  # Attach the information from the metadata on filter
  data <- data %>% 
    mutate(Referenzdefinition =  Referenzdefinition) %>% 
    mutate(Ort =  Ort) 
  
  # Hand over the data object
  data
}