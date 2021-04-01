# --------------------------------------------------------------------------- #

library(labelled)

# Set working directory
setwd("~/Projects/LaSource/Guzman - Healthy Students")

# Load and prepare data
load("data/hs_20201119.rda")
hs <- hs[hs$Temps == 0, ]
hs$Sexe <- to_factor(hs$Sexe)
hs$Annee_etude <- droplevels(to_factor(hs$Annee_etude))
hs$Annee_etude <- relevel(hs$Annee_etude, ref = "1ère année Bachelor")

# Regressions with one moderator variable
X <- c("GSES_tot", "CD_RISC_tot", "MAAS_tot", "MSPSS_tot",
       "MSPSS_signif_other", "MSPSS_family", "MSPSS_friends")
X <- setNames(X, X)
fits1 <- lapply(X, function(x) {
  fml <- paste0("WHOQOL_d2_raw_score ~ PSS14_tot * (", x, " + Annee_etude)")
  fml <- as.formula(fml)
  m0 <- do.call("lm", list(formula = fml, data = quote(hs)))
  fml <- update(fml, . ~ . + Sexe + Age)
  m1 <- do.call("lm", list(formula = fml, data = quote(hs)))
  return(list(unadjusted = m0, adjusted = m1))
})

# Regressions with several moderator variables
X <- c("GSES_tot", "CD_RISC_tot", "MAAS_tot", "MSPSS_tot", "Annee_etude")
fml <- paste0("WHOQOL_d2_raw_score  ~ PSS14_tot * (",
              paste(X, collapse = " + "), ")")
fml <- as.formula(fml)
complete_cases <- na.omit(hs[c("WHOQOL_d2_raw_score", "PSS14_tot", X)])
m0 <- do.call("lm", list(formula = fml, data = quote(complete_cases)))
s0 <- step(m0, trace = FALSE)
fml <- update(fml, . ~ . + Sexe + Age)
complete_cases <- na.omit(hs[c("WHOQOL_d2_raw_score", "PSS14_tot", X,
                               "Sexe", "Age")])
m1 <- do.call("lm", list(formula = fml, data = quote(complete_cases)))
s1 <- step(m1, trace = FALSE)
fits2 <- list(unadj = m0, unadj_s = s0, adj = m1, adj_s = s1)

# Save results in a temporary file to use them in a rmarkdown script
save(fits1, fits2, file = "/tmp/hs_fits.rda")

# rmarkdown file
rmd_file <- path.expand("/tmp/hs_rmd.rmd")

# rmarkdown: initial part
rmd_init <-
"---
title: 'HealStud : Effet de la variable Annee_etude à T0'
author: 'Jérôme Pasquier'
date:
output:
  html_document:
    toc: true
    toc_depth: 2
    toc_float: false
---\n
```{r, echo=FALSE, message=FALSE}
library(knitr)
library(xfun)
library(broom)
library(writexl)
options(knitr.kable.NA = '', width = 100)
opts_chunk$set(echo = FALSE)
load('/tmp/hs_fits.rda')
```\n
# Un seul moderateur\n
"
cat(rmd_init, file = rmd_file)

# rmarkdowm: regressions with one moderator variable
for (x in X) {
  rmd_txt <- paste0(
"## Variable ", x, "\n
### Sans ajustement pour l'âge et le sexe\n
```{r}
m <- fits1[['", x, "']]$unadjusted
```\n
Nombre d'observations : `r nrow(m$model)`\n
```{r}
summary(m)
f <- '/tmp/hs_reg_tbl.xlsx'
write_xlsx(tidy(m), f)
embed_file(f, text = 'Tableau régression (xlsx)')
f <- file.remove(f)
```\n
### Avec ajustement pour l'âge et le sexe\n
```{r}
m <- fits1[['", x, "']]$adjusted
```\n
Nombre d'observations : `r nrow(m$model)`\n
```{r}
summary(m)
f <- '/tmp/hs_reg_tbl.xlsx'
write_xlsx(tidy(m), f)
embed_file(f, text = 'Tableau régression (xlsx)')
f <- file.remove(f)
```\n\n"
  )
  cat(rmd_txt, file = rmd_file, append = TRUE)
}

# rmarkdowm: regressions with several moderator variables
list_ttl = list(
  unadj =   "## Sans ajustement pour l'âge et le sexe, sans sélection\n\n",
  unadj_s = "## Sans ajustement pour l'âge et le sexe, avec sélection\n\n",
  adj =     "## Avec ajustement pour l'âge et le sexe, sans sélection\n\n",
  adj_s =   "## Avec ajustement pour l'âge et le sexe, avec sélection\n\n"
)
cat("# Plusieurs modérateurs\n\n", file = rmd_file, append = TRUE)
for (fit in names(fits2)) {
  rmd_txt <- paste0(
list_ttl[[fit]],
"```{r}
m <- fits2[['", fit, "']]
```\n
Nombre d'observations : `r nrow(m$model)`\n
```{r}
summary(m)
f <- '/tmp/hs_reg_tbl.xlsx'
write_xlsx(tidy(m), f)
embed_file(f, text = 'Tableau régression (xlsx)')
f <- file.remove(f)
```\n\n"
  )
  cat(rmd_txt, file = rmd_file, append = TRUE)
}

# rmarkdown: render html
output_dir <- path.expand(file.path(getwd(), "results/analyses_20210401"))
output_file <- file.path(output_dir, "effet_annee_etude_t0.html")
rmarkdown::render(rmd_file, output_file = output_file)
file.remove("/tmp/hs_fits.rda", rmd_file)

