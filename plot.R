fallzahlPlot <- function(Krankheitsvar = "Acinetobacter-Infektion oder –Kolonisation", floor = "1 year"){
 
  # Make the graphs description
  Quellenangabe <- data$Quellenangabe[1]
  title <- paste("Fälle von", Krankheitsvar)
  subtitle <- paste(data$Ort[1], 
                    "\n vom", 
                    strftime(min(data$Meldejahrmitwoche), format = "%d.%m.%Y"),
                    "bis zum",
                    strftime(max(data$Meldejahrmitwoche), format = "%d.%m.%Y"))
  
  
  # Filter the data and floor to the floor
  df <- data %>% filter(Krankheit == Krankheitsvar) %>%
    mutate(Meldejahrmitwoche = floor_date(Meldejahrmitwoche, floor)) %>%
    group_by(Meldejahrmitwoche, Ort) %>% 
    summarize(Anzahl = sum(Anzahl))
  
  earliestDate <- min(df %>% filter(Anzahl != 0) %>% pull(Meldejahrmitwoche))
  latestDate <- max(df %>% filter(Anzahl != 0) %>% pull(Meldejahrmitwoche))
  
  df <- df %>% filter(Meldejahrmitwoche > earliestDate & Meldejahrmitwoche < latestDate)
  
    # Plot the graph
    print(
      ggplot(data = df, aes(y = Anzahl, x = Meldejahrmitwoche)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        facet_grid(Ort~., scales = "free") +
        labs(title = title,
             subtitle = subtitle,
             caption = paste("Datenquelle:", Quellenangabe)) +
        theme_classic() 
    )
}


inzidenzPlot <- function(Krankheitsvar = "Acinetobacter-Infektion oder –Kolonisation", floor = "1 year"){
  
  # Make the graphs description
  Quellenangabe <- data$Quellenangabe[1]
  title <- paste("Fälle von", Krankheitsvar)
  subtitle <- paste(data$Ort[1], 
                    "\n vom", 
                    strftime(min(data$Meldejahrmitwoche), format = "%d.%m.%Y"),
                    "bis zum",
                    strftime(max(data$Meldejahrmitwoche), format = "%d.%m.%Y"))
  
  
  # Filter the data and floor to the floor
  df <- data %>% filter(Krankheit == Krankheitsvar) %>%
    mutate(Meldejahrmitwoche = floor_date(Meldejahrmitwoche, floor)) %>%
    group_by(Meldejahrmitwoche, Ort) %>% 
    summarise(Inzidenz = mean(Inzidenz)) 
  
  earliestDate <- min(df %>% filter(Inzidenz != 0) %>% pull(Meldejahrmitwoche))
  latestDate <- max(df %>% filter(Inzidenz != 0) %>% pull(Meldejahrmitwoche))
  df <- df %>% filter(Meldejahrmitwoche > earliestDate & Meldejahrmitwoche < latestDate)
  
  print(
    ggplot(data = df, aes(y = Inzidenz, x = Meldejahrmitwoche, color = Ort)) +
      geom_line(stat = "identity") +
      geom_point() +
      labs(title = title,
           subtitle = subtitle,
           caption = paste("Datenquelle:", Quellenangabe)) +
      theme_classic() +
      expand_limits(y = 0) +
      scale_color_manual(values = c("#CC6666", "#9999CC")) +
      theme(legend.position="bottom")
  )
}
