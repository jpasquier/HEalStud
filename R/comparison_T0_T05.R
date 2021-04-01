library(parallel)
library(labelled)
library(XLConnect)

# --------------------------------- Options --------------------------------- #

options(mc.cores = detectCores())

# ---------------------------- Working directory ---------------------------- #

setwd("~/Projects/LaSource/Guzman - Healthy Students")

# -------------------------------- Load data -------------------------------- #

load("data/hs_20201119.rda")
hs <- hs[hs$Filiere == 41, ]

# --------------- Descriptive statistics - Compare T0 and T05 --------------- #

X <- c("WHOQOL_(1|2|d[1-4]_.+)", "PSS14_tot", "GSES_tot", "CD_RISC_tot",
       "MAAS_tot", "^MSPSS_.+", "(S)?HFI_(d[1-6]|tot)", "Act_lucrative",
       "Annee_etude", "ENSA", "Etat_civil", "Follow_up",
       "HESSOGeneve", "Recrutement", "Sexe")
X <- paste0("^", X, "$")
X <- do.call(base::c, mclapply(X, grep, x = names(hs), value = TRUE))
tbl_descr <- do.call(rbind, lapply(X, function(x) {
  if (any(class(hs[[x]]) == "haven_labelled")) hs[[x]] <- to_factor(hs[[x]])
  type <- paste(class(hs[[x]]), collapse = " ")
  i <- !is.na(hs[[x]])
  z <- hs[i, x]
  w <- hs[i, "Temps"]
  if (any(class(hs[[x]]) %in% c("factor", "logical"))) {
    tab <- table(z, w)
    tab <- cbind(tab, prop.table(tab, 2))[, c(1, 3, 2, 4)]
    chisq.pval <- suppressWarnings(chisq.test(z, w)$p.value)
    fisher.pval <-
      fisher.test(z, w, simulate.p.value = (x == "Recrutement"))$p.value
    tab <- data.frame(rownames(tab), tab, chisq.pval, fisher.pval)
  } else if (class(hs[[x]]) %in% c("numeric", "integer")) {
    tab <- do.call(cbind, lapply(c(0, 0.5), function(s) {
      z <- z[w == s]
      data.frame(
        c(mean(z), median(z), min(z)),
        c(sd(z), IQR(z), max(z))
      )
    }))
    tab <- cbind(c("Mean/SD", "Median/IQR", "Min/Max"), tab,
                t.test(z ~ w)$p.value, wilcox.test(z ~ w)$p.value)
  } else {
    stop(paste0(x, ": unsupported type (", type, ")"))
  }
  tab <- cbind(x, type, sum(w == 0), sum(w == 0.5), tab)
  names(tab) <- 
    c("Variable", "Type", "N T0", "N T05", "Value", "T0 (N)", "T0 (Prop)",
      "T05 (N)", "T05 (Prop)", "Chisq/t-test", "Fisher/Wilcoxon")
  rownames(tab) <- NULL
  return(tab)
}))
rm(X)

# ------------------------------ Export table ------------------------------- #

f <- "results/comparison_T0_T05_20201125.xlsx"
if (file.exists(f)) file.remove(f)
wb <- loadWorkbook(f, create = TRUE)
## Init WB
r <- tbl_descr
s <- "comp_T0_T05"
mc_var <- list(r$Variable)
mc_list <- c("B", "C", "D", "J", "K")
createSheet(wb, name = s)
writeWorksheet(wb, r, sheet = s)
## Merged cells
mc <- aggregate(1:nrow(r), mc_var, function(x) {
  if(length(x) > 1) {
    x <- x + 1
    out <- paste0("A", min(x), ":A", max(x))
  } else {
    out <- NA
  }
  return(out)
})[, length(mc_var) + 1]
mc <- mc[!is.na(mc)]
mc <- c(mc, sapply(mc_list, function(z) gsub("A", z, mc)))
mergeCells(wb, sheet = s, reference = mc)
## Borders
cs <- createCellStyle(wb)
setBorder(cs, side = "top", type = XLC$"BORDER.THIN",
          color = XLC$"COLOR.BLACK")
for(i in aggregate(1:nrow(r), mc_var, min)[, length(mc_var) + 1] + 1) {
  setCellStyle(wb, sheet = s, row = i, col = 1:ncol(r), cellstyle = cs)
}
## Colors
cs <- createCellStyle(wb)
setFillPattern(cs, fill = XLC$"FILL.SOLID_FOREGROUND")
setFillForegroundColor(cs, color = XLC$"COLOR.YELLOW")
## Save WB
saveWorkbook(wb)
## Remove unused variables
rm(f, wb, r, s, mc_var, mc_list, mc, cs, i)
