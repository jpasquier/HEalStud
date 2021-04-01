library(readxl)
library(labelled)

# ---------------------------- Working directory ---------------------------- #

setwd("~/Projects/LaSource/Guzman - Healthy Students")

# ----------------- Load data preprocessed on May 26, 2020 ------------------ #

load("data/hs_20200526.rda")

# -------------------------------- Codebook --------------------------------- #

cb <- as.data.frame(read_xlsx("data-raw/HEalstud_codebook_final.xlsx"))

# Delete a line of the codebook dataframe (see comment in the xlsx file)
i <- cb$variable == "Filiere" & cb$code == 46 &
  cb$value_fr == "Psychomotricité"
cb <- cb[!i, ]
rm(i)

# ---------------------------- Delete duplicates ---------------------------- #

# Delete observations 4, 2297, 2322 and 2326 as discussed in the email form
# Claudia Ortoleva dated 19 August 2020.
if (any(is.na(hs$`N°Obs`))) stop("missing N°Obs")
hs <- hs[!(hs$`N°Obs` %in% c(4, 2297, 2322, 2326)), ] 

# -------------------------- Remove empty columns --------------------------- #

j <- apply(is.na(hs), 2, all)
if (FALSE) sapply(hs[j], class)
hs <- hs[!j]
rm(j)

# Remove colums `Consentement` and `Enregistrer`
for (v in c("Consentement", "Enregister")) hs[[v]] <- NULL
rm(v)

# --------------------------- Recoding variables ---------------------------- #

hs$DATE_SAISIE <- as.POSIXct(hs$DATE_SAISIE, format = "%d/%m/%Y %H:%M:%OS")
hs$DATE_ENREG <- as.POSIXct(hs$DATE_ENREG, format = "%d/%m/%Y %H:%M:%OS")
hs$ORIGINE_SAISIE <- factor(hs$ORIGINE_SAISIE)
hs$LANG_SAISIE <- factor(hs$LANG_SAISIE, c("FR", "DE", "ANG"))
hs$APPAREIL_SAISIE <- factor(hs$APPAREIL_SAISIE)
hs$PROGRESSION <- factor(hs$PROGRESSION,
                         c("Terminé", "abgeschlossen", "Completed"))

V <- unique(cb$variable)
V <- V[V %in% names(hs)]
for (v in V) {
  if (grepl("^PSS10_[0-9]+$", v)) {
    lg <- "value_de"
  } else {
    lg <- "value_fr"
  }
  i <- cb$variable == v
  hs[[v]] <- labelled(hs[[v]], setNames(cb[i, "code"], cb[i, lg]))
}
rm(i, lg, V, v)

# --------------------------- Save processed data --------------------------- #

save(hs, file = "data/hs_20200916.rda", compress = "xz")
sink("data-raw/preprocessing_sessionInfo_20200916.txt")
print(sessionInfo(), locale = FALSE)
sink()
