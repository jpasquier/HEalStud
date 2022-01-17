#!/usr/bin/env Rscript

argsList <- commandArgs(trailingOnly = TRUE)
if (length(argsList) == 0) {
  Temps <- 0
} else {
  Temps <- as.numeric(argsList[1])
}

# --------------------------------------------------------------------------- #

library(labelled)
library(writexl)

# ---------------------------- Working directory ---------------------------- #

setwd("~/Projects/LaSource/Guzman - Healthy Students")

# -------------------------------- Load data -------------------------------- #

dataFile <- "data/hs_20210519.rda"
load(dataFile)
message(paste("Data:", dataFile))

hs <- hs[hs$Temps == Temps, ]

# --------------------------- Construct validity ---------------------------- #

# Variables convergentes ou divergentes :
#
# WHOQOL-BREF degré de satisfaction par rapport à sa vie
# WHOQOL-BREF sens de la vie
# WHOQOL-BREF capacité de concentration, degré d’acceptation de soi
# WHOQOL-BREF degré de satisfaction de soi-même
# WHOQOL-BREF fréquence de sentiments négatifs
# Echelle de résilience Connor-Davidson (CD-RISC®)
# Echelle multidimensionnelle de soutien social perçu (MSPSS)
# Exercice d’une activité lucrative accessoire
#
# Variables discriminantes
# Etat civil
# Suivi des études
# Sexe

# Correlation coefficient
X1 <- c("WHOQOL_1", paste0("WHOQOL_d", 1:4, "_raw_score"),
        "CD_RISC_tot", "MSPSS_tot")
N <- unique(sapply(X1, function(x) nrow(na.omit(hs[c(x, "HFI_tot")]))))
R <- t(sapply(X1, function(x) {
  tmp <- na.omit(hs[c(x, "HFI_tot", "SHFI_tot")])
  c(FI = cor(tmp$HFI_tot, tmp[[x]]), SFI = cor(tmp$SHFI_tot, tmp[[x]]))
}))
R <- cbind(data.frame(rownames(R), R))
rownames(R) <- NULL
names(R)[1] <- paste0("Pearson (n=", N, ")")
R <- list(correlation_coefficients = R)
rm(X1, N)

# Anova
X2 <- c("Act_lucrative", "Etat_civil", "Annee_etude", "Sexe")
R <- c(R, lapply(X2, function(x) {
  tmp <- na.omit(hs[c(x, "HFI_tot", "SHFI_tot")])
  tmp[[x]] <- to_factor(tmp[[x]])
  Merge <- function(u, v) merge(u, v, by = x, sort = FALSE)
  M <- Reduce(Merge, lapply(1:2, function(k) {
    y <- c("HFI_tot", "SHFI_tot")[k]
    Y <- c("FI", "SFI")[k]
    fml <- as.formula(paste(y, "~", x))
    M <- aggregate(fml, tmp, mean)
    names(M)[2] <- Y
    pv <- data.frame("p.value anova", anova(lm(fml, tmp))$`Pr(>F)`[1])
    names(pv) <- c(x, Y)
    rbind(M, pv)
  }))
  names(M)[1] <- paste0(names(M)[1], " (n=", nrow(tmp), ")")
  M
}))
names(R)[-1] <- X2

# ----------------------------- Export results ------------------------------ #

outputFile <- paste0("results/construct_validity_FI_T", Temps, "_",
                     format(Sys.time(), "%Y%m%d"), ".xlsx")
write_xlsx(R, outputFile)

