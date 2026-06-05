# ============================================================
# 2_00_dosya_tasima.R
# Makale 2 (Q1) — Tüm dosyaları düzenli klasör yapısına taşı
#
# KAYNAK: E:/Doktora/Politika/Makale Son Son/R_Q1/
# HEDEF:  E:/Doktora/Politika/Makale Son Son/Makale2_JCB/
#
# Çalıştırmadan önce kontrol et ve hedef klasörü değiştir!
# ============================================================

# ── AYARLAR ──────────────────────────────────────────────────
kaynak <- "E:/Doktora/Politika/Makale Son Son/R_Q1"
hedef  <- "E:/Doktora/Politika/Makale Son Son/Makale2_JCB"

cat("
=====================================================
  Makale 2 (Q1) — Dosya Taşıma / Organizasyon
  Kaynak: ", kaynak, "
  Hedef:  ", hedef, "
=====================================================
")

# ── HEDEF KLASÖR YAPISI ──────────────────────────────────────
klasorler <- c(
  "scripts",
  "data/derived",
  "data/raw",
  "output/tables",
  "output/figures",
  "output/logs",
  "manuscript",
  "docs"
)

for (k in klasorler) {
  dir.create(file.path(hedef, k), recursive = TRUE, showWarnings = FALSE)
}
cat("\nKlasorler olusturuldu.\n")

# ── DOSYA HARITALAMASI ───────────────────────────────────────
# Format: list(kaynak_yol, hedef_yol)
# Kaynak yolları R_Q1 içinden, hedef yolları Makale2_JCB içinden

dosyalar <- list(

  # ════ SCRIPTS ════
  list("2_01_veri_dogrulama_ICC.R",          "scripts/2_01_veri_dogrulama_ICC.R"),
  list("2_02_paralel_mediation.R",           "scripts/2_02_paralel_mediation.R"),
  list("2_03_seri_mediation.R",              "scripts/2_03_seri_mediation.R"),
  list("2_04_robustness.R",                  "scripts/2_04_robustness.R"),

  # ════ DATA ════
  list("data/derived/dat_4ulke_Q1.rds",      "data/derived/dat_4ulke_Q1.rds"),
  list("data/derived/02_bdat_4ulke.rds",     "data/derived/02_bdat_4ulke.rds"),
  list("data/derived/02_modeller_mediation.rds", "data/derived/02_modeller_mediation.rds"),
  list("data/derived/02_modeller_seri.rds",  "data/derived/02_modeller_seri.rds"),

  # ════ OUTPUT: TABLES ════
  list("output/tables/02_tablo01_betimsel.csv",              "output/tables/02_tablo01_betimsel.csv"),
  list("output/tables/02_tablo02_korelasyon.csv",            "output/tables/02_tablo02_korelasyon.csv"),
  list("output/tables/02_tablo03_ICC.csv",                   "output/tables/02_tablo03_ICC.csv"),
  list("output/tables/02_tablo04_regresyon_katsayilari.csv", "output/tables/02_tablo04_regresyon_katsayilari.csv"),
  list("output/tables/02_tablo05_dolayli_etkiler.csv",       "output/tables/02_tablo05_dolayli_etkiler.csv"),
  list("output/tables/02_tablo06_model_ozet.csv",            "output/tables/02_tablo06_model_ozet.csv"),
  list("output/tables/02_tablo07_seri_katsayilar.csv",       "output/tables/02_tablo07_seri_katsayilar.csv"),
  list("output/tables/02_tablo08_seri_dolayli.csv",          "output/tables/02_tablo08_seri_dolayli.csv"),
  list("output/tables/02_tablo09_robustness_kovaryatsiz.csv","output/tables/02_tablo09_robustness_kovaryatsiz.csv"),
  list("output/tables/02_tablo10_contrast.csv",              "output/tables/02_tablo10_contrast.csv"),
  list("output/tables/02_tablo11_cinsiyet.csv",              "output/tables/02_tablo11_cinsiyet.csv"),

  # ════ OUTPUT: LOGS ════
  list("output/logs/02_01_veri_dogrulama_log.txt",           "output/logs/02_01_veri_dogrulama_log.txt"),
  list("output/logs/02_02_mediation_log.txt",                "output/logs/02_02_mediation_log.txt"),
  list("output/logs/02_03_seri_mediation_log.txt",           "output/logs/02_03_seri_mediation_log.txt"),
  list("output/logs/02_04_robustness_log.txt",               "output/logs/02_04_robustness_log.txt")
)

# ── CLAUDE'DAN İNDİRİLEN DOSYALAR ───────────────────────────
# Bu dosyaları Claude'dan indirip aşağıdaki konumlara koy:
claude_dosyalar <- list(
  # Figürler -> output/figures/
  # "02_Figure1_path_diagram.png"   -> output/figures/
  # "02_Figure1_path_diagram.tiff"  -> output/figures/
  # "02_Figure2_forest_plot.png"    -> output/figures/
  # "02_Figure2_forest_plot.tiff"   -> output/figures/


  # Makale -> manuscript/
  # "Q1_Manuscript_JCB_COMPLETE.docx" -> manuscript/
  # "Q1_Article_Tables_JCB.docx"      -> manuscript/

  # Dökümantasyon -> docs/
  # "SONUCLAR_LOG_Q1.md"              -> docs/
  # "PISA2022_4Ulke_Degisken_Envanteri.docx" -> docs/
)

# ── KOPYALAMA ────────────────────────────────────────────────
cat("\n=== DOSYA KOPYALAMA ===\n")

basarili <- 0
hatali   <- 0

for (d in dosyalar) {
  src <- file.path(kaynak, d[[1]])
  dst <- file.path(hedef, d[[2]])

  if (file.exists(src)) {
    file.copy(src, dst, overwrite = TRUE)
    cat(sprintf("  OK  %s\n", d[[2]]))
    basarili <- basarili + 1
  } else {
    cat(sprintf("  !!  BULUNAMADI: %s\n", d[[1]]))
    hatali <- hatali + 1
  }
}

cat(sprintf("\n  Kopyalanan: %d / %d\n", basarili, basarili + hatali))
if (hatali > 0) cat(sprintf("  Bulunamayan: %d\n", hatali))

# ── CLAUDE'DAN İNDİRİLECEKLER LİSTESİ ───────────────────────
cat("\n
=====================================================
  CLAUDE'DAN İNDİRİP ELLE KOYACAGIN DOSYALAR:
=====================================================

  1. manuscript/Q1_Manuscript_JCB_COMPLETE.docx
     (Ana makale — metin + tablolar + figürler)

  2. manuscript/Q1_Article_Tables_JCB.docx
     (Tablolar ayrı dosya — gerekirse)

  3. output/figures/02_Figure1_path_diagram.png
  4. output/figures/02_Figure1_path_diagram.tiff
  5. output/figures/02_Figure2_forest_plot.png
  6. output/figures/02_Figure2_forest_plot.tiff

  7. docs/SONUCLAR_LOG_Q1.md

  8. docs/PISA2022_4Ulke_Degisken_Envanteri.docx
     (Zaten elinde var, docs/ altına kopyala)

=====================================================
")

# ── DOĞRULAMA ────────────────────────────────────────────────
cat("\n=== HEDEF KLASÖR YAPISI ===\n")
hedef_dosyalar <- list.files(hedef, recursive = TRUE, full.names = FALSE)
for (f in sort(hedef_dosyalar)) {
  cat(sprintf("  %s\n", f))
}

cat(sprintf("\n  Toplam dosya: %d\n", length(hedef_dosyalar)))

cat("\n
=====================================================
  SONRAKI ADIM:
  Claude'dan indirdigin dosyalari manuscript/ ve
  output/figures/ altina koy, sonra bu scripti
  tekrar calistirarak dogrulama yap.
=====================================================
")
