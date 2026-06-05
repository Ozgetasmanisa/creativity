# ============================================================
# 2_02_paralel_mediation.R
# Makale 2 (Q1) — Adim 2: Paralel Mediation Analizi
#
# Model: BULLIED -> BELONG   -> CREATEFF  (yol 1)
#        BULLIED -> FEELSAFE -> CREATEFF  (yol 2)
#
# Yaklasim: Design-based MLM + CWC + Monte Carlo CI
#   - ICC < .05 ama PISA cluster sampling -> MLM gerekli
#   - Random intercept (~ 1), no random slopes
#   - CWC ayristirmasi -> katsayilar within-school etkileri
#   - Monte Carlo CI (20,000 sim) dolayli etkiler icin
#   - BRR agirlikli (Fay = 0.5, 80 replika)
#
# Regresyon modelleri (her ulke icin ayri):
#   Model A1: BELONG   = a1*BULLIED_CWC + kovaryatlar + u0j + eij
#   Model A2: FEELSAFE = a2*BULLIED_CWC + kovaryatlar + u0j + eij
#   Model B:  CREATEFF = c'*BULLIED_CWC + b1*BELONG_CWC
#                         + b2*FEELSAFE_CWC + kovaryatlar + u0j + eij
#
# Kovaryatlar (L1): ESCS_CWC, ANXMAT_CWC, FAMSUP_CWC, ST004D01T
#
# Dolayli etkiler:
#   ind1         = a1 * b1  (BELONG yolu)
#   ind2         = a2 * b2  (FEELSAFE yolu)
#   total_ind    = ind1 + ind2
#   total_effect = c' + total_ind
#
# Ciktilar:
#   output/tables/02_tablo04_regresyon_katsayilari.csv
#   output/tables/02_tablo05_dolayli_etkiler.csv
#   output/tables/02_tablo06_model_ozet.csv
#   output/logs/02_02_mediation_log.txt
#   data/derived/02_modeller_mediation.rds
#
# Referanslar:
#   Preacher & Selig (2012) — Monte Carlo CI for mediation
#   Marsh et al. (2012) — CWC in PISA multilevel
#   Ludtke et al. (2009) — contextual effects decomposition
#   OECD (2024) — PISA 2022 Technical Report, BRR
#
# 2026-05-10
# ============================================================

library(here)
library(dplyr)
library(haven)
library(BIFIEsurvey)

cat("
=====================================================
  Makale 2 (Q1) — Adim 2: Paralel Mediation
  BULLIED -> BELONG/FEELSAFE -> CREATEFF
  Design-based MLM + CWC + Monte Carlo CI
  TUR / KOR / FIN / MEX
=====================================================
")

# -- 0. Cikti klasorleri -----------------------------------
for (p in c("output/tables", "output/logs", "data/derived")) {
  if (!dir.exists(here(p))) dir.create(here(p), recursive = TRUE)
}

log_con <- file(here("output", "logs", "02_02_mediation_log.txt"), open = "wt")
sink(log_con, type = "output", split = TRUE)

# -- 1. Veri ve BIFIE nesneleri yukle ----------------------
dat       <- readRDS(here("data", "derived", "dat_4ulke_Q1.rds"))
bdat_list <- readRDS(here("data", "derived", "02_bdat_4ulke.rds"))

ulkeler   <- c("TUR", "KOR", "FIN", "MEX")

# BRR parametreleri
WGT_MAIN   <- "W_FSTUWT"
WGT_REP    <- paste0("W_FSTURWT", 1:80)
FAY_FAC    <- 0.5
rep_mevcut <- WGT_REP[WGT_REP %in% colnames(dat)]

# ============================================================
# 2. Monte Carlo CI fonksiyonu (Preacher & Selig, 2012)
# ============================================================
monte_carlo_ci <- function(a_est, a_se, b_est, b_se,
                           nsim = 20000, ci_level = 0.95) {
  # a ve b yollarinin ortak dagilimindan orneklem al
  # (bagimsiz normal varsayimi — standart Monte Carlo)
  set.seed(2026)
  a_sim <- rnorm(nsim, mean = a_est, sd = a_se)
  b_sim <- rnorm(nsim, mean = b_est, sd = b_se)
  ab    <- a_sim * b_sim

  alpha <- 1 - ci_level
  ci    <- quantile(ab, probs = c(alpha / 2, 1 - alpha / 2))

  list(
    ab_est   = a_est * b_est,
    ab_se    = sd(ab),
    ci_lower = ci[1],
    ci_upper = ci[2],
    p_mc     = 2 * min(mean(ab > 0), mean(ab < 0)),
    sig      = ifelse(ci[1] > 0 | ci[2] < 0, "EVET", "HAYIR")
  )
}

# ============================================================
# 3. Katsayi cikarma yardimci fonksiyonu
# ============================================================
extract_coef <- function(model_obj, param_name) {
  ct <- model_obj$stat

  # BIFIE.twolevelreg parametre isimleri "beta_" onekli
  # ve SE sutunu buyuk harf
  beta_name <- paste0("beta_", param_name)

  idx <- which(ct$parameter == beta_name)
  if (length(idx) == 0) {
    # Onek olmadan dene
    idx <- which(ct$parameter == param_name)
  }
  if (length(idx) == 0) {
    # Kismi eslestirme
    idx <- grep(param_name, ct$parameter, fixed = TRUE)
  }

  if (length(idx) == 0) {
    warning(sprintf("Parametre bulunamadi: %s (veya beta_%s)", param_name, param_name))
    return(list(est = NA, se = NA, t = NA, p = NA))
  }

  list(
    est = ct$est[idx[1]],
    se  = ct$SE[idx[1]],
    t   = ct$t[idx[1]],
    p   = ct$p[idx[1]]
  )
}

# ============================================================
# 4. Kovaryat formulleri
# ============================================================
# L1 kovaryatlar (CWC ayristirilmis)
kov_L1 <- c("ESCS_CWC", "ANXMAT_CWC", "FAMSUP_CWC", "ST004D01T")
# Veride mevcut olanlari filtrele
kov_L1 <- intersect(kov_L1, colnames(dat))
cat(sprintf("\nKovaryatlar: %s\n", paste(kov_L1, collapse = ", ")))

# Formul olusturucu
make_formula <- function(predictors) {
  as.formula(paste("~", paste(predictors, collapse = " + ")))
}

# ============================================================
# 4b. BELONG_CWC ve FEELSAFE_CWC olustur (dongu oncesi)
# ============================================================
for (med_var in c("BELONG", "FEELSAFE")) {
  cwc_name <- paste0(med_var, "_CWC")
  sch_name <- paste0(med_var, "_SCH")

  if (!cwc_name %in% colnames(dat)) {
    cat(sprintf("\n  %s olusturuluyor...\n", cwc_name))

    if (!sch_name %in% colnames(dat)) {
      okul_ort <- dat %>%
        group_by(CNT, CNTSCHID) %>%
        summarise(!!sch_name := mean(.data[[med_var]], na.rm = TRUE),
                  .groups = "drop")
      dat <- dat %>% left_join(okul_ort, by = c("CNT", "CNTSCHID"))
    }
    dat[[cwc_name]] <- dat[[med_var]] - dat[[sch_name]]
    dat[[cwc_name]][is.nan(dat[[cwc_name]])] <- NA_real_
    dat[[sch_name]][is.nan(dat[[sch_name]])] <- NA_real_

    cat(sprintf("    %s M~0: %.4f | %s M: %.3f\n",
                cwc_name, mean(dat[[cwc_name]], na.rm = TRUE),
                sch_name, mean(dat[[sch_name]], na.rm = TRUE)))
  } else {
    cat(sprintf("\n  %s zaten veride mevcut.\n", cwc_name))
  }
}

# Guncellenmis veriyi kaydet
saveRDS(dat, here("data", "derived", "dat_4ulke_Q1.rds"))
cat("  >>> dat_4ulke_Q1.rds guncellendi (BELONG_CWC/FEELSAFE_CWC)\n")

# ============================================================
# 5. ANA ANALIZ DONGUSU
# ============================================================
cat("\n\n=== PARALEL MEDIATION ANALIZI ===\n")

# BIFIE icin gerekli tum sutunlar
bifie_cols <- unique(c(
  "BULLIED", "BELONG", "FEELSAFE", "CREATEFF",
  "BULLIED_CWC", "BELONG_CWC", "FEELSAFE_CWC", "CREATEFF_CWC",
  kov_L1,
  grep("_CWC$|_SCH$", colnames(dat), value = TRUE),
  "CNTSCHID", WGT_MAIN, rep_mevcut
))
bifie_cols <- intersect(bifie_cols, colnames(dat))
cat(sprintf("  BIFIE sutun sayisi: %d\n", length(bifie_cols)))

# Konteynerler
tablo_katsayi <- data.frame()
tablo_dolayli <- data.frame()
tablo_model   <- data.frame()
modeller_list <- list()

for (cnt in ulkeler) {
  cat(sprintf("\n\n========================================\n"))
  cat(sprintf("  %s — Paralel Mediation\n", cnt))
  cat(sprintf("========================================\n"))

  # --- BIFIE nesnesi (tum degiskenlerle) ---
  d_cnt <- dat %>%
    filter(CNT == cnt) %>%
    arrange(CNTSCHID) %>%
    select(all_of(bifie_cols)) %>%
    as.data.frame()

  # Numeric zorlama
  for (col in colnames(d_cnt)) {
    if (!is.numeric(d_cnt[[col]]) && !is.integer(d_cnt[[col]])) {
      d_cnt[[col]] <- suppressWarnings(as.numeric(as.character(d_cnt[[col]])))
    }
  }

  cat(sprintf("  N = %d ogrenci, %d okul\n",
              nrow(d_cnt), length(unique(d_cnt$CNTSCHID))))

  bdat <- BIFIEsurvey::BIFIE.data(
    data   = d_cnt,
    wgt    = WGT_MAIN,
    wgtrep = as.matrix(d_cnt[, rep_mevcut]),
    fayfac = FAY_FAC
  )

  # ──────────────────────────────────────────────────────────
  # MODEL A1: BELONG = a1 * BULLIED_CWC + kovaryatlar
  # ──────────────────────────────────────────────────────────
  cat("\n  --- Model A1: BELONG ~ BULLIED_CWC ---\n")
  ff_A1 <- make_formula(c("BULLIED_CWC", kov_L1))

  r2_A1 <- NA
  tryCatch({
    mod_A1 <- BIFIEsurvey::BIFIE.twolevelreg(
      BIFIEobj = bdat, dep = "BELONG",
      formula.fixed = ff_A1, formula.random = ~ 1,
      idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN
    )
    modeller_list[[paste0(cnt, "_A1")]] <- mod_A1

    a1 <- extract_coef(mod_A1, "BULLIED_CWC")
    cat(sprintf("    a1 = %.4f (SE = %.4f, p = %.4f)\n",
                a1$est, a1$se, a1$p))

    s_A1 <- mod_A1$stat
    s_A1$ulke <- cnt; s_A1$model <- "A1_BELONG"
    tablo_katsayi <- rbind(tablo_katsayi, s_A1)

    r2_idx <- which(s_A1$parameter == "Rsquared")
    if (length(r2_idx) > 0) r2_A1 <- s_A1$est[r2_idx[1]]
  }, error = function(e) {
    cat(sprintf("    HATA: %s\n", e$message))
    a1 <<- list(est = NA, se = NA, t = NA, p = NA)
  })

  # ──────────────────────────────────────────────────────────
  # MODEL A2: FEELSAFE = a2 * BULLIED_CWC + kovaryatlar
  # ──────────────────────────────────────────────────────────
  cat("\n  --- Model A2: FEELSAFE ~ BULLIED_CWC ---\n")
  ff_A2 <- make_formula(c("BULLIED_CWC", kov_L1))

  r2_A2 <- NA
  tryCatch({
    mod_A2 <- BIFIEsurvey::BIFIE.twolevelreg(
      BIFIEobj = bdat, dep = "FEELSAFE",
      formula.fixed = ff_A2, formula.random = ~ 1,
      idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN
    )
    modeller_list[[paste0(cnt, "_A2")]] <- mod_A2

    a2 <- extract_coef(mod_A2, "BULLIED_CWC")
    cat(sprintf("    a2 = %.4f (SE = %.4f, p = %.4f)\n",
                a2$est, a2$se, a2$p))

    s_A2 <- mod_A2$stat
    s_A2$ulke <- cnt; s_A2$model <- "A2_FEELSAFE"
    tablo_katsayi <- rbind(tablo_katsayi, s_A2)

    r2_idx <- which(s_A2$parameter == "Rsquared")
    if (length(r2_idx) > 0) r2_A2 <- s_A2$est[r2_idx[1]]
  }, error = function(e) {
    cat(sprintf("    HATA: %s\n", e$message))
    a2 <<- list(est = NA, se = NA, t = NA, p = NA)
  })

  # ──────────────────────────────────────────────────────────
  # MODEL B: CREATEFF = c'*BULLIED_CWC + b1*BELONG_CWC
  #                      + b2*FEELSAFE_CWC + kovaryatlar
  # ──────────────────────────────────────────────────────────
  cat("\n  --- Model B: CREATEFF ~ BULLIED_CWC + BELONG_CWC + FEELSAFE_CWC ---\n")
  ff_B <- make_formula(c("BULLIED_CWC", "BELONG_CWC", "FEELSAFE_CWC", kov_L1))

  r2_B <- NA
  tryCatch({
    mod_B <- BIFIEsurvey::BIFIE.twolevelreg(
      BIFIEobj = bdat, dep = "CREATEFF",
      formula.fixed = ff_B, formula.random = ~ 1,
      idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN
    )
    modeller_list[[paste0(cnt, "_B")]] <- mod_B

    b1   <- extract_coef(mod_B, "BELONG_CWC")
    b2   <- extract_coef(mod_B, "FEELSAFE_CWC")
    c_pr <- extract_coef(mod_B, "BULLIED_CWC")

    cat(sprintf("    c' (dogrudan)     = %.4f (SE = %.4f, p = %.4f)\n",
                c_pr$est, c_pr$se, c_pr$p))
    cat(sprintf("    b1 (BELONG)       = %.4f (SE = %.4f, p = %.4f)\n",
                b1$est, b1$se, b1$p))
    cat(sprintf("    b2 (FEELSAFE)     = %.4f (SE = %.4f, p = %.4f)\n",
                b2$est, b2$se, b2$p))

    s_B <- mod_B$stat
    s_B$ulke <- cnt; s_B$model <- "B_CREATEFF"
    tablo_katsayi <- rbind(tablo_katsayi, s_B)

    r2_idx <- which(s_B$parameter == "Rsquared")
    if (length(r2_idx) > 0) r2_B <- s_B$est[r2_idx[1]]
  }, error = function(e) {
    cat(sprintf("    HATA: %s\n", e$message))
    b1   <<- list(est = NA, se = NA, t = NA, p = NA)
    b2   <<- list(est = NA, se = NA, t = NA, p = NA)
    c_pr <<- list(est = NA, se = NA, t = NA, p = NA)
  })

  # ──────────────────────────────────────────────────────────
  # DOLAYLI ETKILER — Monte Carlo CI
  # ──────────────────────────────────────────────────────────
  cat("\n  --- Dolayli Etkiler (Monte Carlo CI, 20K sim) ---\n")

  # Yol 1: BULLIED -> BELONG -> CREATEFF
  if (!is.na(a1$est) & !is.na(b1$est)) {
    mc1 <- monte_carlo_ci(a1$est, a1$se, b1$est, b1$se)
    cat(sprintf("    ind1 (BELONG)   = %.4f [%.4f, %.4f] %s\n",
                mc1$ab_est, mc1$ci_lower, mc1$ci_upper, mc1$sig))
  } else {
    mc1 <- list(ab_est=NA, ab_se=NA, ci_lower=NA, ci_upper=NA, p_mc=NA, sig="HATA")
  }

  # Yol 2: BULLIED -> FEELSAFE -> CREATEFF
  if (!is.na(a2$est) & !is.na(b2$est)) {
    mc2 <- monte_carlo_ci(a2$est, a2$se, b2$est, b2$se)
    cat(sprintf("    ind2 (FEELSAFE) = %.4f [%.4f, %.4f] %s\n",
                mc2$ab_est, mc2$ci_lower, mc2$ci_upper, mc2$sig))
  } else {
    mc2 <- list(ab_est=NA, ab_se=NA, ci_lower=NA, ci_upper=NA, p_mc=NA, sig="HATA")
  }

  # Toplam dolayli etki
  total_ind <- mc1$ab_est + mc2$ab_est
  total_eff <- c_pr$est + total_ind
  pct_med   <- ifelse(!is.na(total_eff) & abs(total_eff) > 0.001,
                      100 * total_ind / total_eff, NA)

  cat(sprintf("    Toplam dolayli  = %.4f\n", total_ind))
  cat(sprintf("    c' (dogrudan)   = %.4f\n", c_pr$est))
  cat(sprintf("    Toplam etki     = %.4f\n", total_eff))
  cat(sprintf("    Mediation %%     = %.1f%%\n", pct_med))

  # Sonuclari tabloya ekle
  tablo_dolayli <- rbind(tablo_dolayli, data.frame(
    ulke          = cnt,
    yol           = c("ind1_BELONG", "ind2_FEELSAFE",
                      "total_indirect", "c_prime", "total_effect"),
    a_est         = c(a1$est, a2$est, NA, NA, NA),
    a_se          = c(a1$se,  a2$se,  NA, NA, NA),
    b_est         = c(b1$est, b2$est, NA, NA, NA),
    b_se          = c(b1$se,  b2$se,  NA, NA, NA),
    ab_est        = c(mc1$ab_est, mc2$ab_est, total_ind, c_pr$est, total_eff),
    ab_se         = c(mc1$ab_se,  mc2$ab_se,  NA, c_pr$se, NA),
    ci_lower_95   = c(mc1$ci_lower, mc2$ci_lower, NA, NA, NA),
    ci_upper_95   = c(mc1$ci_upper, mc2$ci_upper, NA, NA, NA),
    anlamli       = c(mc1$sig, mc2$sig,
                      ifelse(mc1$sig == "EVET" | mc2$sig == "EVET",
                             "EN AZ BIR", "HAYIR"),
                      ifelse(!is.na(c_pr$p),
                             ifelse(c_pr$p < .05, "EVET", "HAYIR"), "HATA"),
                      NA),
    pct_mediation = c(NA, NA, pct_med, NA, NA)
  ))

  tablo_model <- rbind(tablo_model, data.frame(
    ulke  = cnt,
    model = c("A1_BELONG", "A2_FEELSAFE", "B_CREATEFF"),
    R2    = c(r2_A1, r2_A2, r2_B)
  ))
}

# ============================================================
# 6. Tablolari kaydet
# ============================================================

# Katsayi tablosu
write.csv(tablo_katsayi,
          here("output", "tables", "02_tablo04_regresyon_katsayilari.csv"),
          row.names = FALSE)

# Dolayli etki tablosu
write.csv(tablo_dolayli,
          here("output", "tables", "02_tablo05_dolayli_etkiler.csv"),
          row.names = FALSE)

# Model ozet
write.csv(tablo_model,
          here("output", "tables", "02_tablo06_model_ozet.csv"),
          row.names = FALSE)

# Model nesneleri
saveRDS(modeller_list,
        here("data", "derived", "02_modeller_mediation.rds"))

# ============================================================
# 7. OZET TABLO — Konsol ciktisi
# ============================================================
cat("\n\n=====================================================\n")
cat("     PARALEL MEDIATION OZET — Makale 2 (Q1)\n")
cat("=====================================================\n\n")

ozet <- tablo_dolayli %>%
  filter(yol %in% c("ind1_BELONG", "ind2_FEELSAFE", "c_prime")) %>%
  select(ulke, yol, ab_est, ci_lower_95, ci_upper_95, anlamli)

cat("  Dolayli Etkiler:\n")
print(ozet, row.names = FALSE)

cat("\n\n  Mediation Oranlari:\n")
med_oran <- tablo_dolayli %>%
  filter(yol == "total_indirect") %>%
  select(ulke, ab_est, pct_mediation)
print(med_oran, row.names = FALSE)

cat("\n\n=====================================================\n")
cat("  Ciktilar:\n")
cat("    02_tablo04_regresyon_katsayilari.csv\n")
cat("    02_tablo05_dolayli_etkiler.csv\n")
cat("    02_tablo06_model_ozet.csv\n")
cat("    02_modeller_mediation.rds\n")
cat("\n")
cat("  Sonraki adim: 2_03_ek_analizler.R\n")
cat("    -> Duyarlilik: kontrol degisken varyasyonlari\n")
cat("    -> Contrast: ind1 vs ind2 (BELONG vs FEELSAFE)\n")
cat("    -> Cinsiyet moderasyonu (opsiyonel)\n")
cat("=====================================================\n")

# Log kapat
sink()
close(log_con)

cat("\n>>> Log: output/logs/02_02_mediation_log.txt\n")
cat("\n=== SCRIPT SONU ===\n")
