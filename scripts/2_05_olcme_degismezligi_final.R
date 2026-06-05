# ============================================================
# 2_05_olcme_degismezligi.R  (FINAL)
# Olcme Degismezligi (MGCFA)
#
# ONEMLI METODOLOJIK NOT:
# PISA 2022'de CREATEFF maddeleri (ST334) MATRIX SAMPLING ile
# uygulanmistir: her ogrenci 5 maddeden ort. ~2.4'unu yanitlamistir.
# Bu nedenle CREATEFF madde duzeyi MGCFA MUMKUN DEGILDIR; OECD bu
# maddeleri IRT/WLE ile birlestirip hazir indeks olarak saglar.
# -> CREATEFF icin OECD'nin kalibrasyon/invariance dayanagina atif yapilir.
#
# CREATSCH maddeleri (ST335) tam uygulanmistir (8831 tam gozlem)
# -> CREATSCH icin TAM madde duzeyi MGCFA yapilir.
# ============================================================

library(haven)
library(lavaan)
library(dplyr)

raw_yol <- "E:/Doktora/Politika/Pisa2022/CY08MSP_STU_QQQ.sav"
creatsch_items <- c("ST335Q01JA","ST335Q02JA","ST335Q05JA","ST335Q06JA")

cat("Ham veri okunuyor...\n")
raw <- read_sav(raw_yol, col_select = all_of(c("CNT", creatsch_items)))

dat <- raw %>% filter(CNT %in% c("TUR","KOR","FIN","MEX")) %>% as.data.frame()
for (m in creatsch_items) {
  dat[[m]] <- as.numeric(dat[[m]])
  dat[[m]][dat[[m]] > 90] <- NA
}

# Tam gozlem
d <- dat[complete.cases(dat[, creatsch_items]), c("CNT", creatsch_items)]
cat(sprintf("\nCREATSCH tam gozlem: %d satir\n", nrow(d)))
print(table(d$CNT))

# -- MGCFA: CREATSCH --
model <- paste0("CREATSCH =~ ", paste(creatsch_items, collapse = " + "))

cat("\n========== CONFIGURAL ==========\n")
fit_c <- cfa(model, data = d, group = "CNT", ordered = creatsch_items,
             estimator = "WLSMV", parameterization = "theta")

cat("========== METRIC (loadings) ==========\n")
fit_m <- cfa(model, data = d, group = "CNT", ordered = creatsch_items,
             estimator = "WLSMV", parameterization = "theta",
             group.equal = "loadings")

cat("========== SCALAR (loadings + thresholds) ==========\n")
fit_s <- cfa(model, data = d, group = "CNT", ordered = creatsch_items,
             estimator = "WLSMV", parameterization = "theta",
             group.equal = c("loadings","thresholds"))

g <- function(f, idx) as.numeric(fitMeasures(f, idx))
ozet <- data.frame(
  Olcek = "CREATSCH",
  Model = c("Configural","Metric","Scalar"),
  CFI   = c(g(fit_c,"cfi.scaled"), g(fit_m,"cfi.scaled"), g(fit_s,"cfi.scaled")),
  TLI   = c(g(fit_c,"tli.scaled"), g(fit_m,"tli.scaled"), g(fit_s,"tli.scaled")),
  RMSEA = c(g(fit_c,"rmsea.scaled"), g(fit_m,"rmsea.scaled"), g(fit_s,"rmsea.scaled")),
  SRMR  = c(g(fit_c,"srmr"), g(fit_m,"srmr"), g(fit_s,"srmr"))
)
ozet[["dCFI"]]   <- c(NA, round(diff(ozet[["CFI"]]), 4))
ozet[["dRMSEA"]] <- c(NA, round(diff(ozet[["RMSEA"]]), 4))
ozet[, 3:6] <- round(ozet[, 3:6], 4)

cat("\n\n========== CREATSCH DEGISMEZLIK OZETI ==========\n")
print(ozet, row.names = FALSE)

write.csv(ozet, "output/tables/02_tablo12_olcme_degismezligi.csv", row.names = FALSE)
saveRDS(list(configural=fit_c, metric=fit_m, scalar=fit_s),
        "data/derived/02_invariance_creatsch.rds")

cat("\n=== YORUM KRITERI (Chen 2007; Cheung & Rensvold 2002) ===\n")
cat("  dCFI >= -0.010 VE dRMSEA <= 0.015  -> degismezlik korunmus\n")
cat("  Metric OK -> CREATSCH yol katsayilari ulkeler arasi karsilastirilabilir\n")
cat("  Scalar OK -> CREATSCH ortalamalari karsilastirilabilir\n")

cat("\n=== CREATEFF NOTU (makaleye) ===\n")
cat("CREATEFF maddeleri (ST334) matrix sampling ile uygulandi (ort. 2.4/5 madde\n")
cat("per ogrenci). Madde duzeyi MGCFA uygun degil. CREATEFF icin OECD'nin\n")
cat("IRT-kalibre WLE indeksi kullanildi; olcek-duzeyi invariance OECD 2024b\n")
cat("teknik raporunda belgelenmistir.\n")
