library("rmarkdown")

render_report = function(Ortwert) {
  rmarkdown::render("Gesundheitsbericht.Rmd", 
                    params = list(Ort = Ortwert),
                    output_file = paste0("Gesundheitsbericht-", Ort, ".html")
  )
}

render_report("Reinickendorf")
