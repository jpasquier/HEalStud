wd <- "~/Projects/LaSource/Guzman - Healthy Students"
date <- "2021-xx-xx"

z <- 0.5
dom <- NA

output_file <- "results/test.html"
output_file <- path.expand(file.path(wd, output_file))
rmd_file <- path.expand(file.path(wd, "R/analyses_202101xx_dev.Rmd"))
tmp_file_1 <- "/tmp/HS_tmp_5842364481.Rmd"
if (z == 0.5 | !is.na(dom)) {
  system(paste("sed '/<!-- BEGIN PSS10 -->/,/<!-- END PSS10 -->/d'",
               shQuote(rmd_file), ">", tmp_file_1))
} else {
  system(paste("cp", shQuote(rmd_file), tmp_file_1))
}
tmp_file_2 <- "/tmp/HS_tmp_3507772647.Rmd"
if (z != 0.5) {
  system(paste("sed '/<!-- BEGIN PTGI -->/,/<!-- END PTGI -->/d'",
               tmp_file_1, ">", tmp_file_2))
} else {
  system(paste("cp", tmp_file_1, tmp_file_2))
}
rmarkdown::render(tmp_file_2,
                  params = list(temps = z, date = date, dom = dom),
                  output_file = output_file)
file.remove(tmp_file_1, tmp_file_2)
