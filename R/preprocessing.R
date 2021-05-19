# ------------------------------- Librairies -------------------------------- #

library(labelled)
library(readxl)
library(writexl)

# ---------------------------- Working directory ---------------------------- #

setwd("~/Projects/LaSource/Guzman - Healthy Students")

# -------------------------------- Codebook --------------------------------- #

.cb_path <- "data-raw/HEalstud_codebook_final.xlsx"
cb <- list(all = as.data.frame(read_xlsx(.cb_path)))
for (.lg in c("fr", "en", "de")) {
  cb[[.lg]] <- cb$all[c("variable", "code", paste0("value_", .lg))]
  names(cb[[.lg]])[3] <- "value"
  cb[[.lg]] <- cb[[.lg]][!is.na(cb[[.lg]]$value), ]
}
rm(.cb_path, .lg)

# ---------------------------------- Data ----------------------------------- #

hs <- list()
hs$T0 <- list(fr = 1:2377, en = 2378:2417, de = 2418:2537)
hs$T0 <- lapply(hs$T0, function(rows) {
  file_name <- "data-raw/Données questionnaires - ANONYME.XLSX"
  as.data.frame(read_xlsx(file_name, range = cell_rows(rows)))
})
hs$T05 <- list(fr = as.data.frame(read_xlsx(
  "data-raw/EtudeHealstud_Healstud_2_Résultats.xlsx"
)))
hs$T1 <- list(fr = "FR", en = "EN", de = "DE")
hs$T1 <- lapply(hs$T1, function(lg) {
  file_name = paste0("data-raw/T1-", lg, " - Résultats anonymisés.xlsx")
  as.data.frame(read_xlsx(file_name))
})
hs <- lapply(setNames(names(hs), names(hs)), function(Time) {
  lapply(hs[[Time]], function(tbl) {
    # removes empty columns
    for (v in grep("^\\.{3}[0-9]+$", names(tbl), value = TRUE)) {
      if (all(is.na(tbl[[v]]))) {
        tbl[[v]] <- NULL
      } else {
        stop("Check empty columns")
      }
    }
    # renames variables
    new_names <- sub("^[0-9]+\\. ", "", names(tbl))
    if (any(duplicated(new_names))) {
      stop("duplicated variable names")
    } else {
      names(tbl) <- new_names
    }
    # Add time variable
    tbl$Temps <- c(T0 = 0, T05 = 0.5, T1 = 1)[Time]
    return(tbl)
  })
})

# Number of observations
nobs <- do.call(rbind, lapply(names(hs), function(Time) {
  do.call(rbind, lapply(names(hs[[Time]]), function(lang) {
    data.frame(Temps = Time, Langue = lang, Nobs = nrow(hs[[Time]][[lang]]),
               stringsAsFactors = FALSE)
  }))
}))

# Removes multiple "spaces" in GSES variables
for (Time in c("T0", "T05", "T1")) {
  for (v in grep("^GSES", names(hs[[Time]]$fr), value = TRUE)) {
    hs[[Time]]$fr[[v]] <- gsub(" +", " ", hs[[Time]]$fr[[v]])
  }
}
rm(Time, v)

# Gender variable is filled with dates in the english questionnaire
hs$T0$en$Sexe <- NA

# Rename the variable CLE in T05 database as UID
names(hs$T05$fr)[names(hs$T05$fr) == "CLE"] <- "UID"

# Rename the variable Identifiant in T1 database as UID
hs$T1 <- lapply(hs$T1, function(tbl) {
  names(tbl)[names(tbl) == "Identifiant"] <- "UID"
  return(tbl)
})

# Participants to T05 are all of Dommaine Santé and Filière Soins infirmiers
# Guzman Villegas-Frei Myriam <m.guzmanvillegas-frei@ecolelasource.ch>
# 11.11.2020
hs$T05$fr$Domaine <- "Santé"
hs$T05$fr$Filiere <- "Soins infirmiers"

# We have a link for some students (but not for all) who participated to T0 and
# T05 (Arnould Yannick <yannick.arnould@heig-vd.ch> 05.11.2020)
cle <- read_xlsx("data-raw/Correspondance clés T0.5-T0.xlsx")
names(cle)[names(cle) == "CLE_enquête T0.5"] <- "UID"
names(cle)[names(cle) == "CLE_correspondante enquête T0"] <- "UID_T0"
hs$T05$fr <- merge(hs$T05$fr, cle, by = "UID", all.x = TRUE, sort = FALSE)
rm(cle)

# T1: Remove prefixes in PGI variables
hs$T1 <- lapply(hs$T1, function(tbl) {
  for (v in grep("^PGI", names(tbl), value = TRUE)) {
    tbl[[v]] <- sub("[0-5]( )?= ", "", tbl[[v]])
  }
  return(tbl)
})

# T1: Correct LANG_SAISIE in english questionnaire (set to FR)
hs$T1$en$LANG_SAISIE[hs$T1$en$LANG_SAISIE == "FR"] <- "ANG"

# Harmonization of codes between T0, T05 and T1
hs$T1$fr$Sexe[hs$T1$fr$Sexe == "je désire spécifier différemment"] <-
  "Je désire spécifier différemment"
for (v in paste0("WHOQOL_", 1:26)) {
  hs$T1$de[[v]][hs$T1$de[[v]] == "Mittelmässig"] <- "Mittel-mäßig"
  hs$T1$de[[v]][hs$T1$de[[v]] == "Äusserst"] <- "Äußerst"
}
hs$T0$de$WHOQOL_26[hs$T0$de$WHOQOL_26 == "Zeitweilig"] <- "Gelegentlich"
rm(v)

# -------------------------------- Recoding --------------------------------- #

hs <- lapply(setNames(names(hs), names(hs)), function(Time) {
  lapply(setNames(names(hs[[Time]]), names(hs[[Time]])), function(lg) {
    .hs <- hs[[Time]][[lg]]
    .cb <- cb[[lg]]
    recoded_variables <- c()
    for (v in names(.hs)) {
      if (v %in% .cb$variable) {
        dic <- .cb[.cb$variable == v, c("code", "value")]
        if (all(na.omit(.hs[[v]]) %in% dic$value)) {
          map <- setNames(dic$code, dic$value)
          .hs[[v]] <- unname(map[.hs[[v]]])
          recoded_variables <- c(recoded_variables, v)
        } else {
          warning(paste0(Time, "::", lg, "::", v,
                         " missing value(s) in the codebook"))
        }
      }
    }
    attr(.hs, "recoded_variables") <- recoded_variables
    return(.hs)
  })
})

rec <- lapply(setNames(names(hs), names(hs)), function(Time) {
  rec <- lapply(setNames(names(hs[[Time]]), names(hs[[Time]])), function(lg) {
    .hs <- hs[[Time]][[lg]]
    r <- attr(.hs, "recoded_variables")
    r1 <- data.frame(r, 1)
    r1 <- setNames(r1, c("variable", paste(Time, lg, sep = ".")))
    r0 <- data.frame(names(.hs)[!(names(.hs) %in% r)], 0)
    r0 <- setNames(r0, c("variable", paste(Time, lg, sep = ".")))
    rbind(r1, r0)
  })
  Reduce(function(x, y) merge(x, y, by = "variable", all = TRUE), rec)
})
rec <- Reduce(function(x, y) merge(x, y, by = "variable", all = TRUE), rec)
rec <- rec[order(rec$variable), ]

# ------------------------ Concatenation of the data ------------------------ #

# Adds missing variables to each dataframe
hs <- lapply(setNames(names(hs), names(hs)), function(Time) {
  lapply(setNames(names(hs[[Time]]), names(hs[[Time]])), function(lg) {
    .hs <- hs[[Time]][[lg]]
    for (v in rec[is.na(rec[[paste(Time, lg, sep = ".")]]), "variable"]) {
      .hs[[v]] <- NA
    }
    return(.hs)
  })
})

# Concatenantes dataframes
hs <- do.call(rbind, lapply(hs, function(z) do.call(rbind, z)))

# -------------------------------- Deletions -------------------------------- #

# Delete observations 4/6JDQ-PADT, 2297/QMCR-MYNS, 2322/XEFJ-B8Z7 and
# 2326/QAAN-GMH8 of T0 as discussed in the email form Claudia Ortoleva dated 19
# August 2020.
if (any(is.na(hs$Temps))) stop("missing Temps")
if (any(is.na(hs$`N°Obs`) & hs$Temps != 1)) stop("missing N°Obs")
if (any(is.na(hs$UID))) stop("missing UID")
dups <-
  hs$Temps == 0 & hs$`N°Obs` == 4    & hs$UID == "6JDQ-PADT" |
  hs$Temps == 0 & hs$`N°Obs` == 2297 & hs$UID == "QMCR-MYNS" |
  hs$Temps == 0 & hs$`N°Obs` == 2322 & hs$UID == "XEFJ-B8Z7" |
  hs$Temps == 0 & hs$`N°Obs` == 2326 & hs$UID == "QAAN-GMH8"
ndups <- aggregate(dups, list(hs$Temps, hs$LANG_SAISIE), sum)
names(ndups) <- c("Temps", "Langue", "Doublons")
ndups$Temps <- paste0("T", sub("\\.(0)?", "", ndups$Temps))
ndups$Langue <- sub("ang", "en", tolower(ndups$Langue))
nobs <- merge(nobs, ndups, by = c("Temps", "Langue"), all.x = TRUE)
hs <- hs[!dups, ]
if (any(duplicated(hs[c("UID", "Temps")]))) stop("Duplicated UID")
rm(dups, ndups)

# Deletion of persons who have withdrawn their consent
# yannick.arnould@heig-vd.ch, 11.03.2021
nobs$ConsentementRetire <- 0
for (uid in c("552Q-WQKT", "VWA3-66SR")) {
  Time <- paste0("T", sub("\\.(0)?", "", hs[hs$UID == uid, "Temps"]))
  lang <- sub("ang", "en", tolower(hs[hs$UID == uid, "LANG_SAISIE"]))
  nobs[nobs$Temps == Time & nobs$Langue == lang, "ConsentementRetire"] <-
    nobs[nobs$Temps == Time & nobs$Langue == lang, "ConsentementRetire"] + 1
  hs <- hs[hs$UID != uid, ]
}
rm(uid, Time, lang)

# Removal of persons of undetermined gender (according to discussion by Zoom
# of May 18, 2021)
nobs$GenreIndetermine <- 0
R <- which(hs$Sexe %in% 2:3)
for (r in R) {
  Time <- paste0("T", sub("\\.(0)?", "", hs[r, "Temps"]))
  lang <- sub("ang", "en", tolower(hs[r, "LANG_SAISIE"]))
  nobs[nobs$Temps == Time & nobs$Langue == lang, "GenreIndetermine"] <-
    nobs[nobs$Temps == Time & nobs$Langue == lang, "GenreIndetermine"] + 1
}
hs <- hs[!(hs$Sexe %in% 2:3), ]
rm(R, r, Time, lang)

# ----------- Remove empty columns and colums with a unique value ----------- #

# Empty columns
j <- apply(is.na(hs), 2, all)
if (FALSE) sapply(hs[j], class)
if (sum(j) == 1) {
  message(paste("Variable", names(hs)[j], "is removed because it contains",
                "only missing values"))
} else if (sum(j) > 1) {
  message(paste("Variables", paste(names(hs)[j], collapse = ", "), "are",
                "removed because they contain only missing values"))
}
if (sum(j) > 0) hs <- hs[!j]
rm(j)

# Remove colums with a unique value
j <- apply(hs, 2, function(x) all(!is.na(x)) & length(unique(x)) == 1)
message(paste("Variables", paste(names(hs)[j], collapse = ", "), "are",
                "removed because they contain a unique value"))
hs <- hs[!j]
rm(j)

# --------------------------- Recoding variables ---------------------------- #

# Dates and factors
hs$DATE_SAISIE <- as.POSIXct(hs$DATE_SAISIE, format = "%d/%m/%Y %H:%M:%OS")
hs$DATE_ENREG <- as.POSIXct(hs$DATE_ENREG, format = "%d/%m/%Y %H:%M:%OS")
#hs$ORIGINE_SAISIE <- factor(hs$ORIGINE_SAISIE)
hs$LANG_SAISIE <- factor(hs$LANG_SAISIE, c("FR", "DE", "ANG"))
hs$APPAREIL_SAISIE <- factor(hs$APPAREIL_SAISIE)
hs$PROGRESSION <- factor(hs$PROGRESSION,
                         c("Terminé", "abgeschlossen", "Completed"))
hs$Temps <- factor(hs$Temps, c(0, 0.5, 1))

# Modifiy the codebook
# Delete a line of the codebook dataframe (see comment in the xlsx file)
cb_bak <- cb
cb <- cb$all
i <- cb$variable == "Filiere" & cb$code == 46 &
  cb$value_fr == "Psychomotricité"
cb <- cb[!i, ]
rm(i)

# Add labels
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

# --------------------------------- WHOQOL ---------------------------------- #

# Checks if there are some missing values in the WHOQOL variables
if (sum(is.na(hs[grep("^WHOQOL_[0-9]+$", names(hs))])) > 0) {
  stop("WHOQOL: missing values must be processed")
}

# Reverse items
hs$WHOQOL_3r <- 6 - hs$WHOQOL_3
hs$WHOQOL_4r <- 6 - hs$WHOQOL_4
hs$WHOQOL_26r <- 6 - hs$WHOQOL_26

# Computes dimension scores
.v <- list(c("3r", "4r", 10, 15:18), c(5:7, 11, 19, "26r"),
           20:22, c(8:9, 12:14, 23:25))
.v <- lapply(.v, function(z) paste0("WHOQOL_", z))
for(.i in 1:4) {
  .x <- paste0("WHOQOL_d", .i, "_raw_score")
  hs[[.x]] <- apply(hs[.v[[.i]]], 1, sum)
}
rm(.i, .v, .x)

# Transforms scores
whoqol_table4 <- lapply(list(7:35, 6:30, 3:15, 8:40), function(z) {
  w <- cbind(z, round((z - min(z)) / (max(z) - min(z)) * 16) + 4, NA)
  w[, 3] <- round((w[, 2] - 4) / 16 * 100 + 0.001)
  w
})
hs$WHOQOL_d1_s20 <- round((hs$WHOQOL_d1_raw_score - 7) / 28 * 16) + 4
hs$WHOQOL_d2_s20 <- round((hs$WHOQOL_d2_raw_score - 6) / 24 * 16) + 4
hs$WHOQOL_d3_s20 <- round((hs$WHOQOL_d3_raw_score - 3) / 12 * 16) + 4
hs$WHOQOL_d4_s20 <- round((hs$WHOQOL_d4_raw_score - 8) / 32 * 16) + 4
hs$WHOQOL_d1_s100 <- round((hs$WHOQOL_d1_s20 - 4) / 16 * 100 + 0.001)
hs$WHOQOL_d2_s100 <- round((hs$WHOQOL_d2_s20 - 4) / 16 * 100 + 0.001)
hs$WHOQOL_d3_s100 <- round((hs$WHOQOL_d3_s20 - 4) / 16 * 100 + 0.001)
hs$WHOQOL_d4_s100 <- round((hs$WHOQOL_d4_s20 - 4) / 16 * 100 + 0.001)

# ----------------------------------- PSS ----------------------------------- #

# Checks that for each participant the PSS10 variables are all either missing
# or not missing
.X <- is.na(hs[grep("^PSS10_[0-9]+$", names(hs))])
if (!all(!apply(.X, 1, any) | apply(.X, 1, all))) {
  stop("PSS10: missing values must be processed")
}
rm(.X)

# Computes PSS10 score
hs$PSS10_tot <- apply(hs[paste0("PSS10_", c(1:3, 6, 9:10))], 1, sum) +
  apply(4 - hs[paste0("PSS10_", c(4:5, 7:8))], 1, sum)

# Checks that for each participant the PSS14 variables are all either missing
# or not missing
.X <- is.na(hs[grep("^PSS14_[0-9]+$", names(hs))])
if (!all(!apply(.X, 1, any) | apply(.X, 1, all))) {
  stop("PSS14: missing values must be processed")
}
rm(.X)

# Computes PSS14 score
hs$PSS14_tot <- apply(hs[paste0("PSS14_", c(1:3, 8, 11:12, 14))], 1, sum) +
  apply(4 - hs[paste0("PSS14_", c(4:7, 9:10, 13))], 1, sum)

# ---------------------------------- GSES ----------------------------------- #

# Checks if there are some missing values in the GSES variables
if (any(is.na(hs[grep("^GSES[0-9]+$", names(hs))]))) {
  stop("GSES: missing values must be processed")
}

# Computes GSES sum score
hs$GSES_tot <- apply(hs[grep("^GSES[0-9]+$", names(hs))], 1, sum)

# ---------------------------------- MAAS ----------------------------------- #

# Checks if there are some missing values in the MAAS variables
if (any(is.na(hs[grep("^MAAS[0-9]+$", names(hs))]))) {
  stop("MAAS: missing values must be processed")
}

# Computes MAAS sum score
hs$MAAS_tot <- apply(hs[grep("^MAAS[0-9]+$", names(hs))], 1, sum)

# --------------------------------- CD_RISC --------------------------------- #

# Checks if there are some missing values in the CD_RISC variables
if (any(is.na(hs[grep("^CD_RISC[0-9]+$", names(hs))]))) {
  stop("CD_RISC: missing values must be processed")
}

# Computes CD_RISC sum score
hs$CD_RISC_tot <- apply(hs[grep("^CD_RISC[0-9]+$", names(hs))], 1, sum)

# ---------------------------------- MSPSS ---------------------------------- #

# Checks that for each participant the MSPSS variables are all either missing
# or not missing
.X <- is.na(hs[grep("^MSPSS[0-9]+$", names(hs))])
if (!all(!apply(.X, 1, any) | apply(.X, 1, all))) {
  stop("MSPSS: missing values must be processed")
}
rm(.X)

# Computes MSPSS scores
hs$MSPSS_signif_other <- apply(hs[paste0("MSPSS", c(1:2, 5, 10))], 1, sum) / 4
hs$MSPSS_family <- apply(hs[paste0("MSPSS", c(3:4, 8, 11))], 1, sum) / 4
hs$MSPSS_friends <- apply(hs[paste0("MSPSS", c(6:7, 9, 12))], 1, sum) / 4
hs$MSPSS_tot <- apply(hs[paste0("MSPSS", 1:12)], 1, sum) / 12

# ----------------------------------- HFI ----------------------------------- #

# Checks that for each participant the HF+ variables are all either missing
# or not missing
.X <- is.na(hs[grep("^HFI[0-9]+$", names(hs))])
if (!all(!apply(.X, 1, any) | apply(.X, 1, all))) {
  stop("HFI: missing values must be processed")
}
rm(.X)

# Computes HFI scores
for (.i in 1:6) {
  hs[[paste0("HFI_d", .i)]] <-
    apply(hs[paste0("HFI", 2 * .i + c(-1, 0))], 1, mean)
}
hs$HFI_tot <- apply(hs[paste0("HFI", 1:10)], 1, mean)
hs$SHFI_tot <- apply(hs[paste0("HFI", 1:12)], 1, mean)
rm(.i)

# ----------------------------------- PGI ----------------------------------- #

# Checks if there is any missing values
if (any(is.na(hs[hs$Temps %in% c(0.5, 1), grep("^PGI", names(hs))]))) {
  stop("PGI: missing values must be processed")
}

# Computes PGI scores
dim_list <- list(
  PTGI_new_possibilities = c(3, 7, 11, 14, 17),
  PTGI_relating_to_others = c(6, 8:9, 15:16, 20:21),
  PTGI_personal_strength = c(4, 10, 12, 19),
  PTGI_appreciation_of_life = c(1:2, 13),
  PTGI_spiritual_change = c(5, 18),
  PTGI_tot = 1:21
)
for (v in names(dim_list)) {
  hs[[v]] <- apply(hs[paste0("PGI_", dim_list[[v]])], 1, sum)
}
rm(dim_list, v)

# --------------------------- Save processed data --------------------------- #

save(hs, file = "data/hs_20210519.rda", compress = "xz")
write_xlsx(list(data = hs, recoded_variables = rec, Nobs = nobs),
           "data/healstud_data_20210519.xlsx")
sink("data-raw/preprocessing_sessionInfo_20210519.txt")
print(sessionInfo(), locale = FALSE)
sink()

# --------------------------------------------------------------------------- #

nrow(hs) - sum(nobs$Nobs) + sum(nobs[c("Doublons", "ConsentementRetire",
                                       "GenreIndetermine")])
