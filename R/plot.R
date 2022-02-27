fallzahlPlot <- function(Krankheitsvar = "Campylobacter-Enteritis", floor = "3 months"){
 
  # Make the graphs description
  Quellenangabe <- data$Quellenangabe[1]
  title <- paste("Fälle von", Krankheitsvar)
  subtitle <- paste(data$Ort[1], 
                    "\n vom", 
                    strftime(min(data$Datum), format = "%d.%m.%Y"),
                    "bis zum",
                    strftime(max(data$Datum), format = "%d.%m.%Y"))
  
  
  # Filter the data and floor to the floor
  df <- data %>% filter(Krankheit == Krankheitsvar) %>%
    mutate(Datum = floor_date(Datum, floor)) %>%
    group_by(Datum, Ort) %>% 
    summarize(Anzahl = sum(Anzahl))
  
  earliestDate <- min(df %>% filter(Anzahl != 0) %>% pull(Datum))
  latestDate <- max(df %>% filter(Anzahl != 0) %>% pull(Datum))
  df <- df %>% filter(Datum > earliestDate & Datum < latestDate)
  
    # Plot the graph
  
      ggplot(data = df, aes(y = Anzahl, x = Datum)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        facet_grid(Ort~., scales = "free") +
        labs(title = title,
             subtitle = subtitle,
             caption = paste("Datenquelle:", Quellenangabe)) +
        theme_classic() 
   
}


inzidenzPlot <- function(Krankheitsvar = "Legionellose", floor = "1 year"){
  
  # Make the graphs description
  Quellenangabe <- data$Quellenangabe[1]
  title <- paste("Fälle von", Krankheitsvar)
  subtitle <- paste(data$Ort[1], 
                    "\n vom", 
                    strftime(min(data$Datum), format = "%d.%m.%Y"),
                    "bis zum",
                    strftime(max(data$Datum), format = "%d.%m.%Y"))
  
  
  # Filter the data and floor to the floor
  df <- data %>% filter(Krankheit == Krankheitsvar) %>%
    mutate(Datum = floor_date(Datum, floor)) %>%
    group_by(Datum, Ort) %>% 
    summarise(Inzidenz = mean(Inzidenz)) 
  
  earliestDate <- min(df %>% filter(Inzidenz != 0) %>% pull(Datum))
  latestDate <- max(df %>% filter(Inzidenz != 0) %>% pull(Datum))
  df <- df %>% filter(Datum > earliestDate & Datum < latestDate)
  
    ggplot(data = df, aes(y = Inzidenz, x = Datum, color = Ort)) +
      geom_line(stat = "identity") +
      geom_point() +
      labs(title = title,
           subtitle = subtitle,
           caption = paste("Datenquelle:", Quellenangabe)) +
      theme_classic() +
      expand_limits(y = 0) +
      scale_color_manual(values = c("#CC6666", "#9999CC")) +
      theme(legend.position = "bottom")
  
}
