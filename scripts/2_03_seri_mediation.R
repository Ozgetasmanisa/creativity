# ============================================================
# 2_03_seri_mediation.R
# Makale 2 (Q1) — Adim 3: Seri Mediation
#
# Model: BULLIED -> BELONG/FEELSAFE -> CREATSCH -> CREATEFF
#
# Teorik zincir:
#   Zorbalik -> aidiyet/guvenlik azalir -> yaratici okul
#   ortamiyla etkilesim azalir -> yaratici oz-yeterlik gelismez
#
# 4 Regresyon (her ulke icin):
#   M1: BELONG   = a1*BULLIED_CWC + kov
#   M2: FEELSAFE = a2*BULLIED_CWC + kov
#   M3: CREATSCH = d1*BELONG_CWC + d2*FEELSAFE_CWC
#                  + a3*BULLIED_CWC + kov
#   M4: CREATEFF = b1*BELONG_CWC + b2*FEELSAFE_CWC
#                  + b3*CREATSCH_CWC + c'*BULLIED_CWC + kov
#
# Dolayli etkiler:
#   serial1  = a1 * d1 * b3  (BULLIED->BELONG->CREATSCH->CREATEFF)
#   serial2  = a2 * d2 * b3  (BULLIED->FEELSAFE->CREATSCH->CREATEFF)
#   simple1  = a1 * b1        (BULLIED->BELONG->CREATEFF)
#   simple2  = a2 * b2        (BULLIED->FEELSAFE->CREATEFF)
#   simple3  = a3 * b3        (BULLIED->CREATSCH->CREATEFF, opsiyonel)
#   total_indirect = serial1 + serial2 + simple1 + simple2 + simple3
#
# Monte Carlo CI (20,000 sim) — seri etkiler icin 3-yol carpim
#
# Ciktilar:
#   output/tables/02_tablo07_seri_katsayilar.csv
#   output/tables/02_tablo08_seri_dolayli.csv
#   output/logs/02_03_seri_mediation_log.txt
#   data/derived/02_modeller_seri.rds
#
# 2026-05-10
# ============================================================

library(here)
library(dplyr)
library(haven)
library(BIFIEsurvey)

cat("
=====================================================
  Makale 2 (Q1) — Adim 3: Seri Mediation
  BULLIED -> BELONG/FEELSAFE -> CREATSCH -> CREATEFF
  TUR / KOR / FIN / MEX
=====================================================
")

# -- 0. Cikti klasorleri -----------------------------------
for (p in c("output/tables", "output/logs")) {
  if (!dir.exists(here(p))) dir.create(here(p), recursive = TRUE)
}

log_con <- file(here("output", "logs", "02_03_seri_mediation_log.txt"), open = "wt")
sink(log_con, type = "output", split = TRUE)

# -- 1. Veri yukle -----------------------------------------
dat     <- readRDS(here("data", "derived", "dat_4ulke_Q1.rds"))
ulkeler <- c("TUR", "KOR", "FIN", "MEX")

WGT_MAIN   <- "W_FSTUWT"
WGT_REP    <- paste0("W_FSTURWT", 1:80)
FAY_FAC    <- 0.5
rep_mevcut <- WGT_REP[WGT_REP %in% colnames(dat)]

# ============================================================
# 2. CWC/SCH kontrol — eksik olanlari olustur
# ============================================================
cat("\n=== CWC/SCH KONTROL ===\n")

cwc_gerekli <- c("BULLIED", "BELONG", "FEELSAFE", "CREATEFF",
                  "CREATSCH", "ESCS", "ANXMAT", "FAMSUP")

for (v in cwc_gerekli) {
  cwc_name <- paste0(v, "_CWC")
  sch_name <- paste0(v, "_SCH")

  if (!cwc_name %in% colnames(dat) & v %in% colnames(dat)) {
    cat(sprintf("  %s olusturuluyor...\n", cwc_name))

    if (!sch_name %in% colnames(dat)) {
      okul_ort <- dat %>%
        group_by(CNT, CNTSCHID) %>%
        summarise(!!sch_name := mean(.data[[v]], na.rm = TRUE),
                  .groups = "drop")
      dat <- dat %>% left_join(okul_ort, by = c("CNT", "CNTSCHID"))
    }
    dat[[cwc_name]] <- dat[[v]] - dat[[sch_name]]
    dat[[cwc_name]][is.nan(dat[[cwc_name]])] <- NA_real_
    dat[[sch_name]][is.nan(dat[[sch_name]])] <- NA_real_

    cat(sprintf("    %s M~0: %.4f\n", cwc_name,
                mean(dat[[cwc_name]], na.rm = TRUE)))
  } else if (cwc_name %in% colnames(dat)) {
    cat(sprintf("  %s zaten mevcut.\n", cwc_name))
  }
}

# Guncelle
saveRDS(dat, here("data", "derived", "dat_4ulke_Q1.rds"))

# ============================================================
# 3. Yardimci fonksiyonlar
# ============================================================

# Katsayi cikarma (BIFIE.twolevelreg $stat: SE buyuk harf, beta_ oneki)
extract_coef <- function(model_obj, param_name) {
  ct <- model_obj$stat
  beta_name <- paste0("beta_", param_name)

  idx <- which(ct$parameter == beta_name)
  if (length(idx) == 0) idx <- which(ct$parameter == param_name)
  if (length(idx) == 0) idx <- grep(param_name, ct$parameter, fixed = TRUE)

  if (length(idx) == 0) {
    warning(sprintf("Parametre bulunamadi: %s", param_name))
    return(list(est = NA_real_, se = NA_real_, t = NA_real_, p = NA_real_))
  }
  list(est = ct$est[idx[1]], se = ct$SE[idx[1]],
       t = ct$t[idx[1]], p = ct$p[idx[1]])
}

# Monte Carlo CI — 2 yol carpimi (basit dolayli etki)
mc_ci_2way <- function(a_est, a_se, b_est, b_se, nsim = 20000) {
  set.seed(2026)
  a_sim <- rnorm(nsim, a_est, a_se)
  b_sim <- rnorm(nsim, b_est, b_se)
  ab    <- a_sim * b_sim
  ci    <- quantile(ab, c(.025, .975))
  list(est = a_est * b_est, se = sd(ab),
       ci_lo = ci[1], ci_hi = ci[2],
       sig = ifelse(ci[1] > 0 | ci[2] < 0, "EVET", "HAYIR"))
}

# Monte Carlo CI — 3 yol carpimi (seri dolayli etki)
mc_ci_3way <- function(a_est, a_se, d_est, d_se, b_est, b_se, nsim = 20000) {
  set.seed(2026)
  a_sim <- rnorm(nsim, a_est, a_se)
  d_sim <- rnorm(nsim, d_est, d_se)
  b_sim <- rnorm(nsim, b_est, b_se)
  adb   <- a_sim * d_sim * b_sim
  ci    <- quantile(adb, c(.025, .975))
  list(est = a_est * d_est * b_est, se = sd(adb),
       ci_lo = ci[1], ci_hi = ci[2],
       sig = ifelse(ci[1] > 0 | ci[2] < 0, "EVET", "HAYIR"))
}

make_formula <- function(preds) {
  as.formula(paste("~", paste(preds, collapse = " + ")))
}

# ============================================================
# 4. Kovaryatlar
# ============================================================
kov_L1 <- intersect(c("ESCS_CWC", "ANXMAT_CWC", "FAMSUP_CWC", "ST004D01T"),
                    colnames(dat))
cat(sprintf("\nKovaryatlar: %s\n", paste(kov_L1, collapse = ", ")))

# ============================================================
# 5. BIFIE icin gerekli sutunlar
# ============================================================
bifie_cols <- unique(c(
  "BULLIED", "BELONG", "FEELSAFE", "CREATEFF", "CREATSCH",
  "BULLIED_CWC", "BELONG_CWC", "FEELSAFE_CWC",
  "CREATEFF_CWC", "CREATSCH_CWC",
  kov_L1,
  grep("_CWC$|_SCH$", colnames(dat), value = TRUE),
  "CNTSCHID", WGT_MAIN, rep_mevcut
))
bifie_cols <- intersect(bifie_cols, colnames(dat))
cat(sprintf("BIFIE sutun sayisi: %d\n", length(bifie_cols)))

# ============================================================
# 6. ANA ANALIZ DONGUSU
# ============================================================
cat("\n\n=== SERI MEDIATION ANALIZI ===\n")

tablo_katsayi <- data.frame()
tablo_dolayli <- data.frame()
modeller_list <- list()

for (cnt in ulkeler) {
  cat(sprintf("\n\n========================================\n"))
  cat(sprintf("  %s — Seri Mediation\n", cnt))
  cat(sprintf("========================================\n"))

  # --- BIFIE nesnesi ---
  d_cnt <- dat %>%
    filter(CNT == cnt) %>%
    arrange(CNTSCHID) %>%
    select(all_of(bifie_cols)) %>%
    as.data.frame()

  for (col in colnames(d_cnt)) {
    if (!is.numeric(d_cnt[[col]]) && !is.integer(d_cnt[[col]])) {
      d_cnt[[col]] <- suppressWarnings(as.numeric(as.character(d_cnt[[col]])))
    }
  }

  cat(sprintf("  N = %d ogrenci, %d okul\n",
              nrow(d_cnt), length(unique(d_cnt$CNTSCHID))))

  bdat <- BIFIEsurvey::BIFIE.data(
    data = d_cnt, wgt = WGT_MAIN,
    wgtrep = as.matrix(d_cnt[, rep_mevcut]),
    fayfac = FAY_FAC
  )

  # ────────────────────────────────────────
  # M1: BELONG = a1*BULLIED_CWC + kov
  # ────────────────────────────────────────
  cat("\n  --- M1: BELONG ~ BULLIED_CWC ---\n")
  tryCatch({
    mod_M1 <- BIFIEsurvey::BIFIE.twolevelreg(
      BIFIEobj = bdat, dep = "BELONG",
      formula.fixed = make_formula(c("BULLIED_CWC", kov_L1)),
      formula.random = ~ 1,
      idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN
    )
    modeller_list[[paste0(cnt, "_M1")]] <- mod_M1
    a1 <- extract_coef(mod_M1, "BULLIED_CWC")
    cat(sprintf("    a1 = %.4f (SE=%.4f, p=%.4f)\n", a1$est, a1$se, a1$p))
    s <- mod_M1$stat; s$ulke <- cnt; s$model <- "M1_BELONG"
    tablo_katsayi <- rbind(tablo_katsayi, s)
  }, error = function(e) {
    cat(sprintf("    HATA: %s\n", e$message))
    a1 <<- list(est=NA_real_, se=NA_real_, t=NA_real_, p=NA_real_)
  })

  # ────────────────────────────────────────
  # M2: FEELSAFE = a2*BULLIED_CWC + kov
  # ────────────────────────────────────────
  cat("\n  --- M2: FEELSAFE ~ BULLIED_CWC ---\n")
  tryCatch({
    mod_M2 <- BIFIEsurvey::BIFIE.twolevelreg(
      BIFIEobj = bdat, dep = "FEELSAFE",
      formula.fixed = make_formula(c("BULLIED_CWC", kov_L1)),
      formula.random = ~ 1,
      idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN
    )
    modeller_list[[paste0(cnt, "_M2")]] <- mod_M2
    a2 <- extract_coef(mod_M2, "BULLIED_CWC")
    cat(sprintf("    a2 = %.4f (SE=%.4f, p=%.4f)\n", a2$est, a2$se, a2$p))
    s <- mod_M2$stat; s$ulke <- cnt; s$model <- "M2_FEELSAFE"
    tablo_katsayi <- rbind(tablo_katsayi, s)
  }, error = function(e) {
    cat(sprintf("    HATA: %s\n", e$message))
    a2 <<- list(est=NA_real_, se=NA_real_, t=NA_real_, p=NA_real_)
  })

  # ────────────────────────────────────────
  # M3: CREATSCH = d1*BELONG_CWC + d2*FEELSAFE_CWC
  #               + a3*BULLIED_CWC + kov
  # ────────────────────────────────────────
  cat("\n  --- M3: CREATSCH ~ BELONG_CWC + FEELSAFE_CWC + BULLIED_CWC ---\n")
  tryCatch({
    mod_M3 <- BIFIEsurvey::BIFIE.twolevelreg(
      BIFIEobj = bdat, dep = "CREATSCH",
      formula.fixed = make_formula(c("BELONG_CWC", "FEELSAFE_CWC",
                                     "BULLIED_CWC", kov_L1)),
      formula.random = ~ 1,
      idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN
    )
    modeller_list[[paste0(cnt, "_M3")]] <- mod_M3
    d1 <- extract_coef(mod_M3, "BELONG_CWC")
    d2 <- extract_coef(mod_M3, "FEELSAFE_CWC")
    a3 <- extract_coef(mod_M3, "BULLIED_CWC")
    cat(sprintf("    d1 (BELONG)   = %.4f (SE=%.4f, p=%.4f)\n", d1$est, d1$se, d1$p))
    cat(sprintf("    d2 (FEELSAFE) = %.4f (SE=%.4f, p=%.4f)\n", d2$est, d2$se, d2$p))
    cat(sprintf("    a3 (BULLIED)  = %.4f (SE=%.4f, p=%.4f)\n", a3$est, a3$se, a3$p))
    s <- mod_M3$stat; s$ulke <- cnt; s$model <- "M3_CREATSCH"
    tablo_katsayi <- rbind(tablo_katsayi, s)
  }, error = function(e) {
    cat(sprintf("    HATA: %s\n", e$message))
    d1 <<- list(est=NA_real_, se=NA_real_, t=NA_real_, p=NA_real_)
    d2 <<- list(est=NA_real_, se=NA_real_, t=NA_real_, p=NA_real_)
    a3 <<- list(est=NA_real_, se=NA_real_, t=NA_real_, p=NA_real_)
  })

  # ────────────────────────────────────────
  # M4: CREATEFF = b1*BELONG_CWC + b2*FEELSAFE_CWC
  #               + b3*CREATSCH_CWC + c'*BULLIED_CWC + kov
  # ────────────────────────────────────────
  cat("\n  --- M4: CREATEFF ~ BELONG_CWC + FEELSAFE_CWC + CREATSCH_CWC + BULLIED_CWC ---\n")
  tryCatch({
    mod_M4 <- BIFIEsurvey::BIFIE.twolevelreg(
      BIFIEobj = bdat, dep = "CREATEFF",
      formula.fixed = make_formula(c("BULLIED_CWC", "BELONG_CWC",
                                     "FEELSAFE_CWC", "CREATSCH_CWC",
                                     kov_L1)),
      formula.random = ~ 1,
      idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN
    )
    modeller_list[[paste0(cnt, "_M4")]] <- mod_M4
    b1   <- extract_coef(mod_M4, "BELONG_CWC")
    b2   <- extract_coef(mod_M4, "FEELSAFE_CWC")
    b3   <- extract_coef(mod_M4, "CREATSCH_CWC")
    c_pr <- extract_coef(mod_M4, "BULLIED_CWC")

    cat(sprintf("    c' (dogrudan)    = %.4f (SE=%.4f, p=%.4f)\n", c_pr$est, c_pr$se, c_pr$p))
    cat(sprintf("    b1 (BELONG)      = %.4f (SE=%.4f, p=%.4f)\n", b1$est, b1$se, b1$p))
    cat(sprintf("    b2 (FEELSAFE)    = %.4f (SE=%.4f, p=%.4f)\n", b2$est, b2$se, b2$p))
    cat(sprintf("    b3 (CREATSCH)    = %.4f (SE=%.4f, p=%.4f)\n", b3$est, b3$se, b3$p))

    s <- mod_M4$stat; s$ulke <- cnt; s$model <- "M4_CREATEFF"
    tablo_katsayi <- rbind(tablo_katsayi, s)
  }, error = function(e) {
    cat(sprintf("    HATA: %s\n", e$message))
    b1   <<- list(est=NA_real_, se=NA_real_, t=NA_real_, p=NA_real_)
    b2   <<- list(est=NA_real_, se=NA_real_, t=NA_real_, p=NA_real_)
    b3   <<- list(est=NA_real_, se=NA_real_, t=NA_real_, p=NA_real_)
    c_pr <<- list(est=NA_real_, se=NA_real_, t=NA_real_, p=NA_real_)
  })

  # ────────────────────────────────────────
  # DOLAYLI ETKILER — Monte Carlo CI
  # ────────────────────────────────────────
  cat("\n  --- Dolayli Etkiler (Monte Carlo CI, 20K sim) ---\n")

  # Seri 1: BULLIED -> BELONG -> CREATSCH -> CREATEFF (a1 * d1 * b3)
  if (!anyNA(c(a1$est, d1$est, b3$est))) {
    mc_s1 <- mc_ci_3way(a1$est, a1$se, d1$est, d1$se, b3$est, b3$se)
    cat(sprintf("    serial1 (BELONG->CREATSCH)   = %.5f [%.5f, %.5f] %s\n",
                mc_s1$est, mc_s1$ci_lo, mc_s1$ci_hi, mc_s1$sig))
  } else {
    mc_s1 <- list(est=NA, se=NA, ci_lo=NA, ci_hi=NA, sig="HATA")
  }

  # Seri 2: BULLIED -> FEELSAFE -> CREATSCH -> CREATEFF (a2 * d2 * b3)
  if (!anyNA(c(a2$est, d2$est, b3$est))) {
    mc_s2 <- mc_ci_3way(a2$est, a2$se, d2$est, d2$se, b3$est, b3$se)
    cat(sprintf("    serial2 (FEELSAFE->CREATSCH) = %.5f [%.5f, %.5f] %s\n",
                mc_s2$est, mc_s2$ci_lo, mc_s2$ci_hi, mc_s2$sig))
  } else {
    mc_s2 <- list(est=NA, se=NA, ci_lo=NA, ci_hi=NA, sig="HATA")
  }

  # Basit 1: BULLIED -> BELONG -> CREATEFF (a1 * b1)
  if (!anyNA(c(a1$est, b1$est))) {
    mc_p1 <- mc_ci_2way(a1$est, a1$se, b1$est, b1$se)
    cat(sprintf("    simple1 (BELONG direct)      = %.5f [%.5f, %.5f] %s\n",
                mc_p1$est, mc_p1$ci_lo, mc_p1$ci_hi, mc_p1$sig))
  } else {
    mc_p1 <- list(est=NA, se=NA, ci_lo=NA, ci_hi=NA, sig="HATA")
  }

  # Basit 2: BULLIED -> FEELSAFE -> CREATEFF (a2 * b2)
  if (!anyNA(c(a2$est, b2$est))) {
    mc_p2 <- mc_ci_2way(a2$est, a2$se, b2$est, b2$se)
    cat(sprintf("    simple2 (FEELSAFE direct)    = %.5f [%.5f, %.5f] %s\n",
                mc_p2$est, mc_p2$ci_lo, mc_p2$ci_hi, mc_p2$sig))
  } else {
    mc_p2 <- list(est=NA, se=NA, ci_lo=NA, ci_hi=NA, sig="HATA")
  }

  # Basit 3: BULLIED -> CREATSCH -> CREATEFF (a3 * b3)
  if (!anyNA(c(a3$est, b3$est))) {
    mc_p3 <- mc_ci_2way(a3$est, a3$se, b3$est, b3$se)
    cat(sprintf("    simple3 (CREATSCH direct)    = %.5f [%.5f, %.5f] %s\n",
                mc_p3$est, mc_p3$ci_lo, mc_p3$ci_hi, mc_p3$sig))
  } else {
    mc_p3 <- list(est=NA, se=NA, ci_lo=NA, ci_hi=NA, sig="HATA")
  }

  # Toplamlar
  total_ind <- sum(c(mc_s1$est, mc_s2$est, mc_p1$est, mc_p2$est, mc_p3$est),
                   na.rm = TRUE)
  total_eff <- c_pr$est + total_ind
  pct_med   <- ifelse(abs(total_eff) > 0.001, 100 * total_ind / total_eff, NA)

  cat(sprintf("\n    Toplam dolayli  = %.4f\n", total_ind))
  cat(sprintf("    c' (dogrudan)   = %.4f\n", c_pr$est))
  cat(sprintf("    Toplam etki     = %.4f\n", total_eff))
  cat(sprintf("    Mediation %%     = %.1f%%\n", pct_med))

  # Tabloya ekle
  tablo_dolayli <- rbind(tablo_dolayli, data.frame(
    ulke = cnt,
    yol  = c("serial1_BELONG_CREATSCH", "serial2_FEELSAFE_CREATSCH",
             "simple1_BELONG", "simple2_FEELSAFE", "simple3_CREATSCH",
             "total_indirect", "c_prime", "total_effect"),
    est  = c(mc_s1$est, mc_s2$est, mc_p1$est, mc_p2$est, mc_p3$est,
             total_ind, c_pr$est, total_eff),
    se   = c(mc_s1$se, mc_s2$se, mc_p1$se, mc_p2$se, mc_p3$se,
             NA, c_pr$se, NA),
    ci_lo = c(mc_s1$ci_lo, mc_s2$ci_lo, mc_p1$ci_lo, mc_p2$ci_lo, mc_p3$ci_lo,
              NA, NA, NA),
    ci_hi = c(mc_s1$ci_hi, mc_s2$ci_hi, mc_p1$ci_hi, mc_p2$ci_hi, mc_p3$ci_hi,
              NA, NA, NA),
    sig   = c(mc_s1$sig, mc_s2$sig, mc_p1$sig, mc_p2$sig, mc_p3$sig,
              NA, ifelse(!is.na(c_pr$p), ifelse(c_pr$p < .05, "EVET", "HAYIR"), NA),
              NA),
    pct   = c(NA, NA, NA, NA, NA, pct_med, NA, NA)
  ))
}

# ============================================================
# 7. Tablolari kaydet
# ============================================================
write.csv(tablo_katsayi,
          here("output", "tables", "02_tablo07_seri_katsayilar.csv"),
          row.names = FALSE)

write.csv(tablo_dolayli,
          here("output", "tables", "02_tablo08_seri_dolayli.csv"),
          row.names = FALSE)

saveRDS(modeller_list,
        here("data", "derived", "02_modeller_seri.rds"))

# ============================================================
# 8. OZET TABLO
# ============================================================
cat("\n\n=====================================================\n")
cat("     SERI MEDIATION OZET — Makale 2 (Q1)\n")
cat("=====================================================\n\n")

cat("  Seri dolayli etkiler (a * d * b3):\n")
ozet_seri <- tablo_dolayli %>%
  filter(grepl("serial", yol)) %>%
  select(ulke, yol, est, ci_lo, ci_hi, sig)
print(ozet_seri, row.names = FALSE)

cat("\n\n  Basit dolayli etkiler (a * b):\n")
ozet_basit <- tablo_dolayli %>%
  filter(grepl("simple", yol)) %>%
  select(ulke, yol, est, ci_lo, ci_hi, sig)
print(ozet_basit, row.names = FALSE)

cat("\n\n  Toplam etki:\n")
ozet_toplam <- tablo_dolayli %>%
  filter(yol %in% c("total_indirect", "c_prime", "total_effect")) %>%
  select(ulke, yol, est, sig, pct)
print(ozet_toplam, row.names = FALSE)

cat("\n\n=====================================================\n")
cat("  Ciktilar:\n")
cat("    02_tablo07_seri_katsayilar.csv\n")
cat("    02_tablo08_seri_dolayli.csv\n")
cat("    02_modeller_seri.rds\n")
cat("=====================================================\n")

# Log kapat
sink()
close(log_con)

cat("\n>>> Log: output/logs/02_03_seri_mediation_log.txt\n")
cat("\n=== SCRIPT SONU ===\n")
