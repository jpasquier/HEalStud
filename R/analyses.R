for (z in c(0, 0.5)) {
  date <- "2020-11-25"
  output_file <- paste0("../results/analyses_20201125_T",
                        sub("0\\.5", "05", z), ".html")
  rmarkdown::render("analyses_20201125.Rmd",
                    params = list(temps = z, date = date),
                    output_file = output_file)
}
# https://stackoverflow.com/questions/39550732/
