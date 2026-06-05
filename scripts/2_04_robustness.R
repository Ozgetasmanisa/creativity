# ============================================================
# 2_04_robustness.R
# Makale 2 (Q1) — Adim 4: Duyarlilik / Robustness
#
# 3 analiz:
#   A) Kovaryatsiz model — kontrol degiskenleri cikarildiginda
#      seri dolayli etkiler degisiyor mu?
#   B) Contrast testi — serial1 (BELONG yolu) vs serial2
#      (FEELSAFE yolu) farki anlamli mi?
#   C) Cinsiyet moderasyonu — kiz/erkek alt gruplarda
#      seri dolayli etkiler farklilasıyor mu?
#
# Ciktilar:
#   output/tables/02_tablo09_robustness_kovaryatsiz.csv
#   output/tables/02_tablo10_contrast.csv
#   output/tables/02_tablo11_cinsiyet.csv
#   output/logs/02_04_robustness_log.txt
#
# 2026-05-11
# ============================================================

library(here)
library(dplyr)
library(BIFIEsurvey)

cat("
=====================================================
  Makale 2 (Q1) — Adim 4: Robustness / Duyarlilik
  A) Kovaryatsiz  B) Contrast  C) Cinsiyet
=====================================================
")

for (p in c("output/tables", "output/logs")) {
  if (!dir.exists(here(p))) dir.create(here(p), recursive = TRUE)
}

log_con <- file(here("output", "logs", "02_04_robustness_log.txt"), open = "wt")
sink(log_con, type = "output", split = TRUE)

# -- Veri yukle --------------------------------------------
dat     <- readRDS(here("data", "derived", "dat_4ulke_Q1.rds"))
ulkeler <- c("TUR", "KOR", "FIN", "MEX")

WGT_MAIN   <- "W_FSTUWT"
WGT_REP    <- paste0("W_FSTURWT", 1:80)
FAY_FAC    <- 0.5
rep_mevcut <- WGT_REP[WGT_REP %in% colnames(dat)]

# -- Yardimci fonksiyonlar ---------------------------------
extract_coef <- function(mod, param) {
  ct <- mod$stat
  nm <- paste0("beta_", param)
  idx <- which(ct$parameter == nm)
  if (length(idx) == 0) idx <- grep(param, ct$parameter, fixed = TRUE)
  if (length(idx) == 0) return(list(est=NA_real_, se=NA_real_, p=NA_real_))
  list(est = ct$est[idx[1]], se = ct$SE[idx[1]], p = ct$p[idx[1]])
}

mc_3way <- function(a, a_se, d, d_se, b, b_se, nsim = 20000) {
  set.seed(2026)
  ab <- rnorm(nsim, a, a_se) * rnorm(nsim, d, d_se) * rnorm(nsim, b, b_se)
  ci <- quantile(ab, c(.025, .975))
  list(est = a*d*b, ci_lo = ci[1], ci_hi = ci[2],
       sig = ifelse(ci[1] > 0 | ci[2] < 0, "EVET", "HAYIR"))
}

mc_2way <- function(a, a_se, b, b_se, nsim = 20000) {
  set.seed(2026)
  ab <- rnorm(nsim, a, a_se) * rnorm(nsim, b, b_se)
  ci <- quantile(ab, c(.025, .975))
  list(est = a*b, ci_lo = ci[1], ci_hi = ci[2],
       sig = ifelse(ci[1] > 0 | ci[2] < 0, "EVET", "HAYIR"))
}

ff <- function(preds) as.formula(paste("~", paste(preds, collapse = " + ")))

# BIFIE nesnesi olusturucu
bifie_cols <- unique(c(
  "BULLIED", "BELONG", "FEELSAFE", "CREATEFF", "CREATSCH",
  "BULLIED_CWC", "BELONG_CWC", "FEELSAFE_CWC",
  "CREATEFF_CWC", "CREATSCH_CWC",
  "ESCS_CWC", "ANXMAT_CWC", "FAMSUP_CWC", "ST004D01T",
  grep("_CWC$|_SCH$", colnames(dat), value = TRUE),
  "CNTSCHID", WGT_MAIN, rep_mevcut
))
bifie_cols <- intersect(bifie_cols, colnames(dat))

make_bdat <- function(data_sub) {
  d <- data_sub %>%
    arrange(CNTSCHID) %>%
    select(all_of(bifie_cols)) %>%
    as.data.frame()
  for (col in colnames(d)) {
    if (!is.numeric(d[[col]]) && !is.integer(d[[col]]))
      d[[col]] <- suppressWarnings(as.numeric(as.character(d[[col]])))
  }
  BIFIEsurvey::BIFIE.data(
    data = d, wgt = WGT_MAIN,
    wgtrep = as.matrix(d[, rep_mevcut]),
    fayfac = FAY_FAC
  )
}

# Seri mediation calistirici (4 model, 2 seri dolayli etki)
run_serial <- function(bdat, kov) {
  # M1
  m1 <- BIFIEsurvey::BIFIE.twolevelreg(
    BIFIEobj = bdat, dep = "BELONG",
    formula.fixed = ff(c("BULLIED_CWC", kov)),
    formula.random = ~ 1, idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN)
  a1 <- extract_coef(m1, "BULLIED_CWC")

  # M2
  m2 <- BIFIEsurvey::BIFIE.twolevelreg(
    BIFIEobj = bdat, dep = "FEELSAFE",
    formula.fixed = ff(c("BULLIED_CWC", kov)),
    formula.random = ~ 1, idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN)
  a2 <- extract_coef(m2, "BULLIED_CWC")

  # M3
  m3 <- BIFIEsurvey::BIFIE.twolevelreg(
    BIFIEobj = bdat, dep = "CREATSCH",
    formula.fixed = ff(c("BELONG_CWC", "FEELSAFE_CWC", "BULLIED_CWC", kov)),
    formula.random = ~ 1, idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN)
  d1 <- extract_coef(m3, "BELONG_CWC")
  d2 <- extract_coef(m3, "FEELSAFE_CWC")

  # M4
  m4 <- BIFIEsurvey::BIFIE.twolevelreg(
    BIFIEobj = bdat, dep = "CREATEFF",
    formula.fixed = ff(c("BULLIED_CWC", "BELONG_CWC", "FEELSAFE_CWC",
                         "CREATSCH_CWC", kov)),
    formula.random = ~ 1, idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN)
  b3   <- extract_coef(m4, "CREATSCH_CWC")
  c_pr <- extract_coef(m4, "BULLIED_CWC")

  # Seri dolayli etkiler
  s1 <- mc_3way(a1$est, a1$se, d1$est, d1$se, b3$est, b3$se)
  s2 <- mc_3way(a2$est, a2$se, d2$est, d2$se, b3$est, b3$se)

  list(a1=a1, a2=a2, d1=d1, d2=d2, b3=b3, c_pr=c_pr,
       serial1=s1, serial2=s2)
}

# ============================================================
# A) KOVARYATSIZ MODEL
# ============================================================
cat("\n\n============================================\n")
cat("  A) KOVARYATSIZ MODEL\n")
cat("============================================\n")

tablo_kov <- data.frame()

for (cnt in ulkeler) {
  cat(sprintf("\n--- %s ---\n", cnt))
  d_sub <- dat %>% filter(CNT == cnt)
  bdat  <- make_bdat(d_sub)

  tryCatch({
    res <- run_serial(bdat, kov = character(0))  # bos kovaryat

    cat(sprintf("  serial1 = %.5f [%.5f, %.5f] %s\n",
                res$serial1$est, res$serial1$ci_lo, res$serial1$ci_hi, res$serial1$sig))
    cat(sprintf("  serial2 = %.5f [%.5f, %.5f] %s\n",
                res$serial2$est, res$serial2$ci_lo, res$serial2$ci_hi, res$serial2$sig))

    tablo_kov <- rbind(tablo_kov, data.frame(
      ulke = cnt,
      yol = c("serial1_nokov", "serial2_nokov"),
      est = c(res$serial1$est, res$serial2$est),
      ci_lo = c(res$serial1$ci_lo, res$serial2$ci_lo),
      ci_hi = c(res$serial1$ci_hi, res$serial2$ci_hi),
      sig = c(res$serial1$sig, res$serial2$sig),
      b3 = c(res$b3$est, res$b3$est)
    ))
  }, error = function(e) {
    cat(sprintf("  HATA: %s\n", e$message))
  })
}

write.csv(tablo_kov,
          here("output", "tables", "02_tablo09_robustness_kovaryatsiz.csv"),
          row.names = FALSE)
cat("\n>>> 02_tablo09_robustness_kovaryatsiz.csv\n")

# ============================================================
# B) CONTRAST TESTI: serial1 vs serial2
#    Monte Carlo fark dagilimi
# ============================================================
cat("\n\n============================================\n")
cat("  B) CONTRAST: serial1 vs serial2\n")
cat("============================================\n")

tablo_contrast <- data.frame()
kov_L1 <- intersect(c("ESCS_CWC", "ANXMAT_CWC", "FAMSUP_CWC", "ST004D01T"),
                    colnames(dat))

for (cnt in ulkeler) {
  cat(sprintf("\n--- %s ---\n", cnt))
  d_sub <- dat %>% filter(CNT == cnt)
  bdat  <- make_bdat(d_sub)

  tryCatch({
    # Katsayilari cek (ana model, kovaryatli)
    res <- run_serial(bdat, kov = kov_L1)

    # Contrast: serial1 - serial2
    set.seed(2026)
    nsim <- 20000
    a1_sim <- rnorm(nsim, res$a1$est, res$a1$se)
    d1_sim <- rnorm(nsim, res$d1$est, res$d1$se)
    a2_sim <- rnorm(nsim, res$a2$est, res$a2$se)
    d2_sim <- rnorm(nsim, res$d2$est, res$d2$se)
    b3_sim <- rnorm(nsim, res$b3$est, res$b3$se)

    s1_sim <- a1_sim * d1_sim * b3_sim
    s2_sim <- a2_sim * d2_sim * b3_sim
    diff   <- s1_sim - s2_sim

    ci_diff <- quantile(diff, c(.025, .975))
    sig_diff <- ifelse(ci_diff[1] > 0 | ci_diff[2] < 0, "EVET", "HAYIR")

    cat(sprintf("  serial1 = %.5f\n", res$serial1$est))
    cat(sprintf("  serial2 = %.5f\n", res$serial2$est))
    cat(sprintf("  fark    = %.5f [%.5f, %.5f] %s\n",
                mean(diff), ci_diff[1], ci_diff[2], sig_diff))

    tablo_contrast <- rbind(tablo_contrast, data.frame(
      ulke = cnt,
      serial1 = res$serial1$est,
      serial2 = res$serial2$est,
      diff = mean(diff),
      ci_lo = ci_diff[1],
      ci_hi = ci_diff[2],
      sig = sig_diff
    ))
  }, error = function(e) {
    cat(sprintf("  HATA: %s\n", e$message))
  })
}

write.csv(tablo_contrast,
          here("output", "tables", "02_tablo10_contrast.csv"),
          row.names = FALSE)
cat("\n>>> 02_tablo10_contrast.csv\n")

# ============================================================
# C) CINSIYET MODERASYONU
#    Kiz (ST004D01T == 1) vs Erkek (ST004D01T == 2) alt grup
# ============================================================
cat("\n\n============================================\n")
cat("  C) CINSIYET MODERASYONU\n")
cat("============================================\n")

tablo_cinsiyet <- data.frame()

for (cnt in ulkeler) {
  cat(sprintf("\n--- %s ---\n", cnt))

  for (gender in c(1, 2)) {
    g_label <- ifelse(gender == 1, "Kiz", "Erkek")
    cat(sprintf("  %s:\n", g_label))

    d_sub <- dat %>% filter(CNT == cnt, ST004D01T == gender)
    cat(sprintf("    N = %d\n", nrow(d_sub)))

    if (nrow(d_sub) < 200) {
      cat("    Yetersiz N, atlaniyor.\n")
      next
    }

    tryCatch({
      bdat <- make_bdat(d_sub)
      # Cinsiyet zaten filtrelenmis, kovaryatlardan cikar
      kov_no_gender <- setdiff(kov_L1, "ST004D01T")
      res <- run_serial(bdat, kov = kov_no_gender)

      cat(sprintf("    serial1 = %.5f [%.5f, %.5f] %s\n",
                  res$serial1$est, res$serial1$ci_lo, res$serial1$ci_hi,
                  res$serial1$sig))
      cat(sprintf("    serial2 = %.5f [%.5f, %.5f] %s\n",
                  res$serial2$est, res$serial2$ci_lo, res$serial2$ci_hi,
                  res$serial2$sig))

      tablo_cinsiyet <- rbind(tablo_cinsiyet, data.frame(
        ulke = cnt, cinsiyet = g_label, N = nrow(d_sub),
        yol = c("serial1", "serial2"),
        est = c(res$serial1$est, res$serial2$est),
        ci_lo = c(res$serial1$ci_lo, res$serial2$ci_lo),
        ci_hi = c(res$serial1$ci_hi, res$serial2$ci_hi),
        sig = c(res$serial1$sig, res$serial2$sig),
        b3 = c(res$b3$est, res$b3$est),
        a1 = c(res$a1$est, NA),
        a2 = c(NA, res$a2$est)
      ))
    }, error = function(e) {
      cat(sprintf("    HATA: %s\n", e$message))
    })
  }
}

write.csv(tablo_cinsiyet,
          here("output", "tables", "02_tablo11_cinsiyet.csv"),
          row.names = FALSE)
cat("\n>>> 02_tablo11_cinsiyet.csv\n")

# ============================================================
# OZET
# ============================================================
cat("\n\n=====================================================\n")
cat("     ROBUSTNESS OZET — Makale 2 (Q1)\n")
cat("=====================================================\n\n")

cat("  A) Kovaryatsiz model:\n")
print(tablo_kov %>% select(ulke, yol, est, ci_lo, ci_hi, sig),
      row.names = FALSE)

cat("\n\n  B) Contrast (serial1 - serial2):\n")
print(tablo_contrast, row.names = FALSE)

cat("\n\n  C) Cinsiyet:\n")
print(tablo_cinsiyet %>% select(ulke, cinsiyet, yol, est, ci_lo, ci_hi, sig),
      row.names = FALSE)

cat("\n\n=====================================================\n")
cat("  Ciktilar:\n")
cat("    02_tablo09_robustness_kovaryatsiz.csv\n")
cat("    02_tablo10_contrast.csv\n")
cat("    02_tablo11_cinsiyet.csv\n")
cat("=====================================================\n")

sink()
close(log_con)

cat("\n>>> Log: output/logs/02_04_robustness_log.txt\n")
cat("\n=== SCRIPT SONU ===\n")
