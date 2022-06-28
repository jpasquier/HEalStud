wd <- "~/Projects/LaSource/Guzman - Healthy Students"
date <- "2022-06-28"
load(file.path(wd, "/data/hs_original_gender_20220628.rda"))
hs <- hs_original_gender
s0 <- paste0("analyses_", gsub("-", "", date))
output_dir <- path.expand(file.path(wd, "results", s0))
if (!dir.exists(output_dir)) dir.create(output_dir)
L <- rbind(
  data.frame(
    z = 0,
    dom = c(NA, names(attr(hs$Domaine, "labels")), "Soins infirmiers",
            "Annee_1", "Annee_2", "Annee_Autre")
  ),
  data.frame(z = 0.5, dom = c(NA, "COVID19_4_Non", "COVID19_4_Oui")),
  data.frame(
    z = 1,
    dom = c(NA, names(attr(hs$Domaine, "labels")), "Soins infirmiers",
            "COVID19_4_Non", "COVID19_4_Oui", "Annee_2", "Annee_3")
  )
)
L <- data.frame(z = 0, dom = "Langue_FR")
for (k in 1:nrow(L)) {
  z <- L[k, "z"]
  dom <- L[k, "dom"]
  print(paste("Temps :", z, "/ Domaine :", dom))
  s1 <- sub("0\\.5", "05", z)
  s2 <- gsub(" ", "_", iconv(dom, to="ASCII//TRANSLIT"))
  if (is.na(s2)) s2 <- "" else s2 <- paste0("_", s2)
  output_file <- file.path(output_dir, paste0(s0, "_T", s1, s2, ".html"))
  rmd_file <- path.expand(file.path(wd, "R/analyses.Rmd"))
  tmp_file_1 <- paste0("/tmp/HS_tmp_", round(runif(1) * 10^10), ".Rmd")
  tmp_file_2 <- paste0("/tmp/HS_tmp_", round(runif(1) * 10^10), ".Rmd")
  if (z == 0.5 | !is.na(dom)) {
    system(paste("sed '/<!-- BEGIN PSS10 -->/,/<!-- END PSS10 -->/d'",
                 shQuote(rmd_file), ">", tmp_file_1))
  } else {
    system(paste("cp", shQuote(rmd_file), tmp_file_1))
  }
  if (z != 0.5) {
    system(paste("sed '/<!-- BEGIN PTGI -->/,/<!-- END PTGI -->/d'",
                 tmp_file_1, ">", tmp_file_2))
  } else {
    system(paste("cp", tmp_file_1, tmp_file_2))
  }
  rmarkdown::render(tmp_file_2,
                    params = list(data = hs, temps = z, date = date, dom = dom,
                                  sttl = paste('Domaine :', dom,
                                               '- Variable Sexe inchangée')),
                    output_file = output_file)
  file.remove(tmp_file_1, tmp_file_2)
}
# https://stackoverflow.com/questions/39550732/
