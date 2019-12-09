library("rmarkdown")

render_report = function(Ort, Datum = strftime(Sys.Date(), format = "%Y-%m-%d")) {
  rmarkdown::render("Gesundheitsbericht.Rmd", 
                    params = list(Ort = Ort),
                    output_file = paste0(Datum, "-Gesundheitsbericht-", Ort),
                    output_dir = "_posts"
  )
}


render_report("Reinickendorf")
