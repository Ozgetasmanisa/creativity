# ============================================================
# 2_01_veri_dogrulama_ICC.R  (v4)
# Makale 2 (Q1) — Adim 1: Veri Dogrulama + Betimsel + ICC
#
# Model : BULLIED -> BELONG / FEELSAFE -> CREATEFF
# Ulkeler: TUR, KOR, FIN, MEX
# Veri  : dat_4ulke_Q1.rds (26,993 ogrenci, 891 okul)
#
# Duzeltme (v4):
#   BIFIE.data'ya sadece analiz sutunlari verilecek (117 sutunun
#   tamami degil). Character/factor sutunlar BIFIE icerisinde
#   "non-numeric argument" hatasina neden oluyordu.
#
# Ciktilar:
#   output/tables/02_tablo01_betimsel.csv
#   output/tables/02_tablo02_korelasyon.csv
#   output/tables/02_tablo03_ICC.csv
#   output/logs/02_01_veri_dogrulama_log.txt
#   data/derived/02_bdat_4ulke.rds
#   data/derived/dat_4ulke_Q1.rds  (CREATEFF eklenmisse guncellenir)
#
# 2026-05-10
# ============================================================

library(here)
library(dplyr)
library(tidyr)
library(haven)
library(BIFIEsurvey)

cat("
=====================================================
  Makale 2 (Q1) — Adim 1 (v4)
  BULLIED -> BELONG/FEELSAFE -> CREATEFF
  TUR / KOR / FIN / MEX
=====================================================
")

# -- 0. Cikti klasorleri -----------------------------------
for (p in c("output/tables", "output/logs", "data/derived")) {
  if (!dir.exists(here(p))) dir.create(here(p), recursive = TRUE)
}

log_con <- file(here("output", "logs", "02_01_veri_dogrulama_log.txt"), open = "wt")
sink(log_con, type = "output", split = TRUE)

# -- 1. Veri yukle -----------------------------------------
dat <- readRDS(here("data", "derived", "dat_4ulke_Q1.rds"))
ulkeler <- c("TUR", "KOR", "FIN", "MEX")

cat(sprintf("\n=== VERI BOYUTU ===\n"))
cat(sprintf("  Toplam satir : %d\n", nrow(dat)))
cat(sprintf("  Toplam sutun : %d\n", ncol(dat)))
cat(sprintf("  Toplam okul  : %d\n", length(unique(dat$CNTSCHID))))

cat("\n=== ULKE DAGILIMI ===\n")
for (cnt in ulkeler) {
  d_cnt <- dat %>% filter(CNT == cnt)
  cat(sprintf("  %s : %5d ogrenci, %3d okul\n",
              cnt, nrow(d_cnt), length(unique(d_cnt$CNTSCHID))))
}
stopifnot(all(dat$CNT %in% ulkeler))

# ============================================================
# 1b. CREATEFF / CREATSCH — veride yoksa raw SAV'dan cek
# ============================================================
yaraticilik_vars <- c("CREATEFF", "CREATSCH")
eksik_yarat      <- yaraticilik_vars[!yaraticilik_vars %in% colnames(dat)]

if (length(eksik_yarat) > 0) {
  cat(sprintf("\n!!! EKSIK DEGISKENLER: %s\n", paste(eksik_yarat, collapse = ", ")))

  # --- Dosya yolu: once proje icinde, yoksa ust dizinde ara ---
  sav_candidates <- c(
    "E:/Doktora/Politika/Pisa2022/CY08MSP_STU_QQQ.sav",
    here("data", "raw", "CY08MSP_STU_QQQ.sav"),
    here("..", "data", "raw", "CY08MSP_STU_QQQ.sav")
  )
  stu_path <- NA
  for (sp in sav_candidates) {
    if (file.exists(sp)) { stu_path <- sp; break }
  }

  if (!is.na(stu_path)) {
    cat(sprintf("  Raw dosya bulundu: %s\n", stu_path))

    stu_yarat <- read_sav(
      stu_path,
      col_select = c("CNT", "CNTSTUID", "CNTSCHID", all_of(eksik_yarat))
    ) %>%
      filter(CNT %in% ulkeler) %>%
      mutate(across(all_of(eksik_yarat),
                    ~as.numeric(haven::zap_labels(.)))) %>%
      mutate(CNTSCHID = as.character(CNTSCHID),
             CNTSTUID = as.character(CNTSTUID))

    cat(sprintf("  Raw'dan %d satir okundu\n", nrow(stu_yarat)))
    for (v in eksik_yarat) {
      cat(sprintf("  %-12s NA: %.1f%%\n", v,
                  100 * mean(is.na(stu_yarat[[v]]))))
    }

    # Merge
    if ("CNTSTUID" %in% colnames(dat)) {
      dat <- dat %>%
        mutate(CNTSTUID = as.character(CNTSTUID)) %>%
        left_join(
          stu_yarat %>% select(CNT, CNTSTUID, all_of(eksik_yarat)),
          by = c("CNT", "CNTSTUID")
        )
    } else {
      cat("  UYARI: CNTSTUID yok, CNTSCHID+sira ile merge\n")
      dat <- dat %>% mutate(CNTSCHID_chr = as.character(CNTSCHID))
      stu_yarat <- stu_yarat %>%
        group_by(CNT, CNTSCHID) %>% mutate(.sira = row_number()) %>% ungroup()
      dat <- dat %>%
        group_by(CNT, CNTSCHID_chr) %>% mutate(.sira = row_number()) %>% ungroup() %>%
        left_join(stu_yarat %>% select(CNT, CNTSCHID, .sira, all_of(eksik_yarat)),
                  by = c("CNT", "CNTSCHID_chr" = "CNTSCHID", ".sira")) %>%
        select(-.sira, -CNTSCHID_chr)
    }

    for (v in eksik_yarat) {
      if (v %in% colnames(dat)) {
        cat(sprintf("  Merge sonrasi %-12s NA: %.1f%%\n", v,
                    100 * mean(is.na(dat[[v]]))))
      }
    }
    rm(stu_yarat); gc()

    # CWC + SCH ayristirmasi
    cat("\n  CWC/SCH ayristirmasi...\n")
    for (v in eksik_yarat) {
      if (!v %in% colnames(dat)) next
      cwc_name <- paste0(v, "_CWC")
      sch_name <- paste0(v, "_SCH")

      okul_ort <- dat %>%
        group_by(CNT, CNTSCHID) %>%
        summarise(!!sch_name := mean(.data[[v]], na.rm = TRUE), .groups = "drop")

      dat <- dat %>%
        left_join(okul_ort, by = c("CNT", "CNTSCHID")) %>%
        mutate(!!cwc_name := .data[[v]] - .data[[sch_name]])

      dat[[sch_name]][is.nan(dat[[sch_name]])] <- NA_real_
      dat[[cwc_name]][is.nan(dat[[cwc_name]])] <- NA_real_

      cat(sprintf("    %s -> %s (M=%.3f) + %s (M~0: %.4f)\n",
                  v, sch_name, mean(dat[[sch_name]], na.rm = TRUE),
                  cwc_name, mean(dat[[cwc_name]], na.rm = TRUE)))
    }

    saveRDS(dat, here("data", "derived", "dat_4ulke_Q1.rds"))
    cat("\n  >>> dat_4ulke_Q1.rds guncellendi\n")

  } else {
    cat("  HATA: Raw .SAV dosyasi bulunamadi!\n")
    cat("  Aranan yollar:\n")
    for (sp in sav_candidates) cat(sprintf("    %s\n", sp))
    cat("  Script CREATEFF/CREATSCH olmadan devam edecek.\n")
  }
} else {
  cat("\n  CREATEFF ve CREATSCH veride mevcut.\n")
}

# ============================================================
# 2. Degisken listeleri (guncel sutunlardan)
# ============================================================
all_cols <- colnames(dat)

ana_vars     <- intersect(c("BULLIED", "BELONG", "FEELSAFE", "CREATEFF"), all_cols)
kontrol_vars <- intersect(c("ESCS", "ANXMAT", "FAMSUP", "CREATSCH", "ST004D01T"), all_cols)
cwc_mevcut   <- grep("_CWC$", all_cols, value = TRUE)
sch_mevcut   <- grep("_SCH$", all_cols, value = TRUE)

WGT_MAIN   <- "W_FSTUWT"
WGT_REP    <- paste0("W_FSTURWT", 1:80)
FAY_FAC    <- 0.5
rep_mevcut <- WGT_REP[WGT_REP %in% all_cols]

cat("\n=== DEGISKEN KONTROLU ===\n")
cat(sprintf("  Ana vars   : %s\n", paste(ana_vars, collapse = ", ")))
cat(sprintf("  Kontrol    : %s\n", paste(kontrol_vars, collapse = ", ")))
cat(sprintf("  CWC vars   : %s\n", paste(cwc_mevcut, collapse = ", ")))
cat(sprintf("  SCH vars   : %s\n", paste(sch_mevcut, collapse = ", ")))
cat(sprintf("  BRR rep    : %d / 80\n", length(rep_mevcut)))
stopifnot(length(rep_mevcut) == 80)

# ============================================================
# 3. Veri tipi duzeltme
# ============================================================
cat("\n=== VERI TIPI DUZELTME ===\n")

# Haven etiketleri — CNT, CNTSTUID gibi ID sutunlarini HARIC TUT
id_cols    <- c("CNT", "CNTSTUID", "CNT_BILGI", "STRATUM", "OECD")
label_cols <- all_cols[sapply(dat, haven::is.labelled)]
label_cols_num <- setdiff(label_cols, id_cols)  # sadece numeric olmasi gerekenler

if (length(label_cols) > 0) {
  cat(sprintf("  Haven etiketli sutun: %d (numeric'e cevirilecek: %d)\n",
              length(label_cols), length(label_cols_num)))
  cat(sprintf("  Haric tutulan (ID)  : %s\n",
              paste(intersect(label_cols, id_cols), collapse = ", ")))
  
  # ID sutunlarini zap_labels ile character'a cevir (numeric degil!)
  id_label_cols <- intersect(label_cols, id_cols)
  if (length(id_label_cols) > 0) {
    dat <- dat %>%
      mutate(across(all_of(id_label_cols),
                    ~as.character(haven::zap_labels(.))))
  }
  # Geri kalanlari numeric'e cevir
  if (length(label_cols_num) > 0) {
    dat <- dat %>%
      mutate(across(all_of(label_cols_num),
                    ~as.numeric(haven::zap_labels(.))))
  }
}

# Zorla numeric
force_cols <- intersect(
  c(ana_vars, kontrol_vars, cwc_mevcut, sch_mevcut, WGT_MAIN, rep_mevcut),
  colnames(dat)
)
n_conv <- 0
for (col in force_cols) {
  if (!is.numeric(dat[[col]])) {
    dat[[col]] <- suppressWarnings(as.numeric(as.character(dat[[col]])))
    n_conv <- n_conv + 1
  }
}
cat(sprintf("  Numeric zorlamasi: %d sutun donusturuldu\n", n_conv))

dat <- dat %>%
  mutate(
    CNTSCHID  = as.integer(as.numeric(as.character(CNTSCHID))),
    ST004D01T = as.integer(ST004D01T)
  )

# ============================================================
# 4. Eksik veri raporu
# ============================================================
cat("\n=== EKSIK VERI RAPORU ===\n")
rapor_vars <- intersect(c(ana_vars, kontrol_vars, cwc_mevcut, sch_mevcut), colnames(dat))
for (v in rapor_vars) {
  na_n <- sum(is.na(dat[[v]]))
  cat(sprintf("  %-20s NA: %5d (%5.1f%%)\n", v, na_n, 100 * mean(is.na(dat[[v]]))))
}

cat("\n=== EKSIK VERI — ULKE BAZLI ===\n")
for (cnt in ulkeler) {
  cat(sprintf("\n  --- %s ---\n", cnt))
  d_cnt <- dat %>% filter(CNT == cnt)
  for (v in ana_vars) {
    cat(sprintf("    %-18s %5.1f%%\n", v, 100 * mean(is.na(d_cnt[[v]]))))
  }
}

# ============================================================
# 5. BIFIE.data — SADECE GEREKLI SUTUNLARI VER
#    (117 sutunun tamami degil — character/factor sutunlar
#     BIFIE icerisinde "non-numeric" hatasina neden oluyordu)
# ============================================================
cat("\n\n=== BIFIE NESNELERI OLUSTURULUYOR ===\n")

# BIFIE'ye girecek sutunlari belirle
bifie_analiz_cols <- unique(c(
  ana_vars, kontrol_vars, cwc_mevcut, sch_mevcut,
  "CNTSCHID", "CNT",
  WGT_MAIN, rep_mevcut
))
bifie_analiz_cols <- intersect(bifie_analiz_cols, colnames(dat))

cat(sprintf("  BIFIE'ye verilecek sutun sayisi: %d (toplam: %d)\n",
            length(bifie_analiz_cols), ncol(dat)))

betimsel_vars <- intersect(
  c(ana_vars, "ESCS", "ANXMAT", "FAMSUP", "CREATSCH"),
  colnames(dat)
)

bdat_list      <- list()
betimsel_sonuc <- data.frame()

for (cnt in ulkeler) {
  cat(sprintf("\n============ %s ============\n", cnt))

  # Sadece gerekli sutunlari sec + cluster sirala
  d <- dat %>%
    filter(CNT == cnt) %>%
    arrange(CNTSCHID) %>%
    select(all_of(bifie_analiz_cols)) %>%
    as.data.frame()

  # Tum sutunlarin numeric oldugunu dogrula (paranoyak kontrol)
  for (col in setdiff(colnames(d), c("CNT"))) {
    if (!is.numeric(d[[col]]) && !is.integer(d[[col]])) {
      d[[col]] <- suppressWarnings(as.numeric(as.character(d[[col]])))
    }
  }
  # CNT'yi cikar — BIFIE sadece numeric istiyor
  d_bifie <- d %>% select(-CNT)

  bdat <- BIFIEsurvey::BIFIE.data(
    data   = d_bifie,
    wgt    = WGT_MAIN,
    wgtrep = as.matrix(d_bifie[, rep_mevcut]),
    fayfac = FAY_FAC
  )
  bdat_list[[cnt]] <- bdat

  # --- Betimsel istatistikler (Manuel BRR) ---
  # BIFIE.univar 3.8'de "non-numeric" hatasi veriyor,
  # BIFIE.correl ve twolevelreg calisiyor — dolayisiyla
  # betimsel icin manuel BRR hesaplama kullaniyoruz.
  for (v in betimsel_vars) {
    x <- d_bifie[[v]]
    w <- d_bifie[[WGT_MAIN]]
    valid <- !is.na(x) & !is.na(w)
    if (sum(valid) == 0) next

    x_v <- x[valid]
    w_v <- w[valid]

    # Final agirlikli ortalama ve SD
    N_wt   <- sum(w_v)
    M_est  <- weighted.mean(x_v, w_v)
    SD_est <- sqrt(sum(w_v * (x_v - M_est)^2) / N_wt)

    # BRR SE hesapla (Fay = 0.5)
    M_rep  <- numeric(80)
    SD_rep <- numeric(80)
    for (r in seq_along(rep_mevcut)) {
      wr <- d_bifie[[rep_mevcut[r]]][valid]
      M_rep[r]  <- weighted.mean(x_v, wr)
      SD_rep[r] <- sqrt(sum(wr * (x_v - M_rep[r])^2) / sum(wr))
    }
    SE_M  <- sqrt(sum((M_rep  - M_est)^2)  / (80 * (1 - FAY_FAC)^2))
    SE_SD <- sqrt(sum((SD_rep - SD_est)^2) / (80 * (1 - FAY_FAC)^2))

    betimsel_sonuc <- rbind(betimsel_sonuc, data.frame(
      ulke     = cnt,
      degisken = v,
      N        = round(N_wt, 0),
      M        = round(M_est, 3),
      SE_M     = round(SE_M, 3),
      SD       = round(SD_est, 3),
      SE_SD    = round(SE_SD, 3)
    ))
    cat(sprintf("  %-12s  M = %7.3f (SE = %.3f)  SD = %.3f\n",
                v, M_est, SE_M, SD_est))
  }
}

write.csv(betimsel_sonuc,
          here("output", "tables", "02_tablo01_betimsel.csv"),
          row.names = FALSE)
cat("\n>>> Betimsel: output/tables/02_tablo01_betimsel.csv\n")

# ============================================================
# 6. Korelasyon matrisi (BRR-agirlikli)
# ============================================================
cat("\n\n=== KORELASYON MATRISLERI ===\n")

kor_vars <- intersect(
  c("BULLIED", "BELONG", "FEELSAFE", "CREATEFF",
    "ESCS", "ANXMAT", "FAMSUP"),
  colnames(dat)
)

kor_sonuc_tum <- data.frame()

for (cnt in ulkeler) {
  cat(sprintf("\n--- %s ---\n", cnt))
  bdat <- bdat_list[[cnt]]

  tryCatch({
    corr <- BIFIEsurvey::BIFIE.correl(bdat, vars = kor_vars)
    cr <- corr$stat.cor
    cr$ulke <- cnt
    kor_sonuc_tum <- rbind(kor_sonuc_tum, cr)

    ana_ciftler <- list(
      c("BULLIED", "BELONG"), c("BULLIED", "FEELSAFE"),
      c("BULLIED", "CREATEFF"), c("BELONG", "CREATEFF"),
      c("FEELSAFE", "CREATEFF")
    )
    for (pr in ana_ciftler) {
      if (!all(pr %in% kor_vars)) next
      row <- cr %>% filter((var1==pr[1] & var2==pr[2])|(var1==pr[2] & var2==pr[1]))
      if (nrow(row) > 0)
        cat(sprintf("  r(%s, %s) = %.3f (SE = %.3f)\n",
                    pr[1], pr[2], row$cor[1], row$cor_SE[1]))
    }
  }, error = function(e) {
    cat(sprintf("  Korelasyon HATA: %s\n", e$message))
  })
}

write.csv(kor_sonuc_tum,
          here("output", "tables", "02_tablo02_korelasyon.csv"),
          row.names = FALSE)
cat("\n>>> Korelasyon: output/tables/02_tablo02_korelasyon.csv\n")

# ============================================================
# 7. ICC — Bos model (BIFIE.twolevelreg)
# ============================================================
cat("\n\n=== ICC ANALIZI (Bos Model) ===\n")
cat("  Kriter: ICC > .05 -> MLM gerekli (Heck & Thomas, 2015)\n\n")

icc_vars <- intersect(c("BULLIED", "BELONG", "FEELSAFE", "CREATEFF"), colnames(dat))
icc_sonuclar <- data.frame()

for (cnt in ulkeler) {
  cat(sprintf("\n============ %s ============\n", cnt))
  bdat <- bdat_list[[cnt]]

  for (dv in icc_vars) {
    cat(sprintf("  ICC: %s ...\n", dv))
    tryCatch({
      null_mod <- BIFIEsurvey::BIFIE.twolevelreg(
        BIFIEobj = bdat, dep = dv,
        formula.fixed = ~ 1, formula.random = ~ 1,
        idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN
      )
      s <- null_mod$stat
      icc_val  <- s$est[s$parameter == "ICC_Uncond"]
      tau2_val <- s$est[s$parameter == "Var_(Intercept)"]
      sig2_val <- s$est[s$parameter == "ResidVar"]
      mlm_ok   <- ifelse(icc_val > 0.05, "EVET", "HAYIR")

      cat(sprintf("    tau2=%.4f | sigma2=%.4f | ICC=%.4f | MLM: %s\n",
                  tau2_val, sig2_val, icc_val, mlm_ok))

      icc_sonuclar <- rbind(icc_sonuclar, data.frame(
        ulke = cnt, degisken = dv,
        tau2 = round(tau2_val, 4), sigma2 = round(sig2_val, 4),
        ICC = round(icc_val, 4), mlm_gerekli = mlm_ok
      ))
    }, error = function(e) {
      cat(sprintf("    HATA: %s\n", e$message))
      icc_sonuclar <<- rbind(icc_sonuclar, data.frame(
        ulke = cnt, degisken = dv,
        tau2 = NA, sigma2 = NA, ICC = NA, mlm_gerekli = "HATA"
      ))
    })
  }
}

cat("\n\n=====================================================\n")
cat("       ICC OZET TABLOSU — Makale 2 (Q1)\n")
cat("=====================================================\n")
print(icc_sonuclar, row.names = FALSE)
cat("=====================================================\n")

write.csv(icc_sonuclar,
          here("output", "tables", "02_tablo03_ICC.csv"),
          row.names = FALSE)
cat("\n>>> ICC: output/tables/02_tablo03_ICC.csv\n")

# ============================================================
# 8. Kaydet
# ============================================================
saveRDS(bdat_list, here("data", "derived", "02_bdat_4ulke.rds"))
cat(">>> BIFIE nesneleri: data/derived/02_bdat_4ulke.rds\n")

# ============================================================
# 9. Ozet
# ============================================================
cat("\n\n=====================================================\n")
cat("       ADIM 1 TAMAMLANDI — Makale 2 (Q1)\n")
cat("=====================================================\n")
cat("  Ciktilar:\n")
cat("    02_tablo01_betimsel.csv\n")
cat("    02_tablo02_korelasyon.csv\n")
cat("    02_tablo03_ICC.csv\n")
cat("    02_bdat_4ulke.rds\n\n")
cat("  Sonraki adim: 2_02_dogrudan_etkiler.R\n")
cat("=====================================================\n")

sink()
close(log_con)
cat("\n>>> Log: output/logs/02_01_veri_dogrulama_log.txt\n")
cat("\n=== SCRIPT SONU ===\n")
