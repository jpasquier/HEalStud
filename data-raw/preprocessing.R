# ------------------------------- Librairies -------------------------------- #

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

hs <- lapply(list(fr=1:2377, en=2378:2417, de=2418:2537), function(rows) {
  file_name <- "data-raw/Données questionnaires - ANONYME.XLSX"
  tbl <- as.data.frame(read_xlsx(file_name, range = cell_rows(rows)))
  # removes empty columns
  for (v in grep("^\\.{3}[0-9]+$", names(tbl), value = TRUE)) {
    if (all(is.na(tbl[[v]]))) {
      tbl[[v]] <- NULL
    }
  }
  # renames variables
  new_names <- sub("^[0-9]+\\. ", "", names(tbl))
  if (any(duplicated(new_names))) {
    stop("duplicated variable names")
  } else {
    names(tbl) <- new_names
  }
  return(tbl)
})

# Removes multiple "spaces" in GSES variables
for (v in grep("^GSES", names(hs$fr), value = TRUE)) {
  hs$fr[[v]] <- gsub(" +", " ", hs$fr[[v]])
}
rm(v)

# Gender variable is filled with dates in the english questionnaire
hs$en$Sexe <- NA

# -------------------------------- Recoding --------------------------------- #

hs <- lapply(setNames(names(hs), names(hs)), function(lg) {
  .hs <- hs[[lg]]
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
        warning(paste0(lg, "::", v, " missing value(s) in the codebook"))
      }
    }
  }
  attr(.hs, "recoded_variables") <- recoded_variables
  return(.hs)
})

rec <- lapply(setNames(names(hs), names(hs)), function(lg) {
  .hs <- hs[[lg]]
  r <- attr(.hs, "recoded_variables")
  r1 <- data.frame(r, 1)
  r1 <- setNames(r1, c("variable", lg))
  r0 <- data.frame(names(.hs)[!(names(.hs) %in% r)], 0)
  r0 <- setNames(r0, c("variable", lg))
  rbind(r1, r0)
})
rec <- Reduce(function(x, y) merge(x, y, by = "variable", all = TRUE), rec)
rec <- rec[order(rec$variable), ]

# ----------- Concatenation the data of the three questionnaires ------------ #

# Adds missing variables to each dataframe
hs <- lapply(setNames(names(hs), names(hs)), function(lg) {
  .hs <- hs[[lg]]
  for (v in rec[is.na(rec[[lg]]), "variable"]) {
    .hs[[v]] <- NA
  }
  return(.hs)
})

# Concatenantes dataframes
hs <- do.call(rbind, hs)

# ----------------------------- Duplicated UID ------------------------------ #

.v <- c("N°Obs", "LANG_SAISIE", "UID")
.v <- c(.v, names(hs)[!(names(hs) %in% .v)])
dup_uid <- hs[hs$UID %in% hs$UID[duplicated(hs$UID)], .v]
dup_uid <- dup_uid[order(dup_uid$UID), ]

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

# --------------------------- Save processed data --------------------------- #

save(hs, file = "data/hs_20200526.rda", compress = "xz")
write_xlsx(list(data = hs, dup_uid = dup_uid), "~/hs_data_20200526.xlsx")
sink("data-raw/preprocessing_sessionInfo_20200226.txt")
print(sessionInfo(), locale = FALSE)
sink()

