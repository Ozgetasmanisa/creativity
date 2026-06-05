# ============================================================
# 2_06_ulke_farklari_etkilesim.R  (DUZELTILMIS)
# Ulke Farklarinin Formel Testi (Hakem Raporu Madde 7)
#
# YOL A: Havuzlanmis model + ulke kukla x yordayici etkilesimi
# CNT -> sayisal kukla (KOR, FIN, MEX; TUR = referans)
# Etkilesimler elle sayisal sutun olarak olusturulur (BIFIE sayisal ister)
#
# Calisan kalip: 2_03_seri_mediation.R ile ayni
#   BIFIE.data(data=..., wgt=..., wgtrep=as.matrix(...), fayfac=0.5)
# ============================================================

library(here)
library(dplyr)
library(BIFIEsurvey)

dat <- readRDS(here("data","derived","dat_4ulke_Q1.rds"))

WGT_MAIN   <- "W_FSTUWT"
rep_mevcut <- paste0("W_FSTURWT", 1:80)
rep_mevcut <- rep_mevcut[rep_mevcut %in% colnames(dat)]
FAY_FAC    <- 0.5
kov_L1     <- intersect(c("ESCS_CWC","ANXMAT_CWC","FAMSUP_CWC","ST004D01T"), colnames(dat))

# ── ULKE KUKLA DEGISKENLERI (TUR = referans) ─────────────────
dat$d_KOR <- as.numeric(dat$CNT == "KOR")
dat$d_FIN <- as.numeric(dat$CNT == "FIN")
dat$d_MEX <- as.numeric(dat$CNT == "MEX")

# ── ETKILESIM TERIMLERI (yordayici x ulke kukla) ─────────────
# b3 yolu: CREATSCH_CWC x ulke
dat$CREATSCH_KOR <- dat$CREATSCH_CWC * dat$d_KOR
dat$CREATSCH_FIN <- dat$CREATSCH_CWC * dat$d_FIN
dat$CREATSCH_MEX <- dat$CREATSCH_CWC * dat$d_MEX
# a1 yolu: BULLIED_CWC x ulke
dat$BULLIED_KOR <- dat$BULLIED_CWC * dat$d_KOR
dat$BULLIED_FIN <- dat$BULLIED_CWC * dat$d_FIN
dat$BULLIED_MEX <- dat$BULLIED_CWC * dat$d_MEX
# d1 yolu: BELONG_CWC x ulke
dat$BELONG_KOR <- dat$BELONG_CWC * dat$d_KOR
dat$BELONG_FIN <- dat$BELONG_CWC * dat$d_FIN
dat$BELONG_MEX <- dat$BELONG_CWC * dat$d_MEX

make_formula <- function(preds) as.formula(paste("~", paste(preds, collapse = " + ")))

extract_rows <- function(model_obj, pattern) {
  ct <- model_obj$stat
  idx <- grep(pattern, ct$parameter)
  ct[idx, c("parameter","est","SE","t","p")]
}

# ── BIFIE icin sayisal sutunlar ──────────────────────────────
bifie_cols <- unique(c(
  "CREATEFF","BELONG","CREATSCH",
  "BULLIED_CWC","BELONG_CWC","FEELSAFE_CWC","CREATSCH_CWC",
  kov_L1,
  "d_KOR","d_FIN","d_MEX",
  "CREATSCH_KOR","CREATSCH_FIN","CREATSCH_MEX",
  "BULLIED_KOR","BULLIED_FIN","BULLIED_MEX",
  "BELONG_KOR","BELONG_FIN","BELONG_MEX",
  grep("_CWC$|_SCH$", colnames(dat), value = TRUE),
  "CNTSCHID", WGT_MAIN, rep_mevcut
))
bifie_cols <- intersect(bifie_cols, colnames(dat))

d_all <- dat %>%
  arrange(CNTSCHID) %>%
  select(all_of(bifie_cols)) %>%
  as.data.frame()

for (col in colnames(d_all)) {
  if (!is.numeric(d_all[[col]])) d_all[[col]] <- as.numeric(as.character(d_all[[col]]))
}

cat(sprintf("Havuzlanmis N = %d, okul = %d\n",
            nrow(d_all), length(unique(d_all$CNTSCHID))))

bdat <- BIFIEsurvey::BIFIE.data(
  data = d_all, wgt = WGT_MAIN,
  wgtrep = as.matrix(d_all[, rep_mevcut]),
  fayfac = FAY_FAC
)

# ============================================================
# TEST 1: b3 (CREATSCH -> CREATEFF) ulke etkilesimi
# ============================================================
cat("\n\n========== TEST 1: b3 ULKE ETKILESIMI ==========\n")
preds_b3 <- c("CREATSCH_CWC","d_KOR","d_FIN","d_MEX",
              "CREATSCH_KOR","CREATSCH_FIN","CREATSCH_MEX",
              "BELONG_CWC","FEELSAFE_CWC","BULLIED_CWC", kov_L1)
mod_b3 <- BIFIE.twolevelreg(BIFIEobj = bdat, dep = "CREATEFF",
            formula.fixed = make_formula(preds_b3),
            formula.random = ~ 1, idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN)
etk_b3 <- extract_rows(mod_b3, "CREATSCH_KOR|CREATSCH_FIN|CREATSCH_MEX")
print(etk_b3, row.names = FALSE)

# ============================================================
# TEST 2: a1 (BULLIED -> BELONG) ulke etkilesimi
# ============================================================
cat("\n\n========== TEST 2: a1 ULKE ETKILESIMI ==========\n")
preds_a1 <- c("BULLIED_CWC","d_KOR","d_FIN","d_MEX",
              "BULLIED_KOR","BULLIED_FIN","BULLIED_MEX", kov_L1)
mod_a1 <- BIFIE.twolevelreg(BIFIEobj = bdat, dep = "BELONG",
            formula.fixed = make_formula(preds_a1),
            formula.random = ~ 1, idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN)
etk_a1 <- extract_rows(mod_a1, "BULLIED_KOR|BULLIED_FIN|BULLIED_MEX")
print(etk_a1, row.names = FALSE)

# ============================================================
# TEST 3: d1 (BELONG -> CREATSCH) ulke etkilesimi
# ============================================================
cat("\n\n========== TEST 3: d1 ULKE ETKILESIMI ==========\n")
preds_d1 <- c("BELONG_CWC","d_KOR","d_FIN","d_MEX",
              "BELONG_KOR","BELONG_FIN","BELONG_MEX",
              "FEELSAFE_CWC","BULLIED_CWC", kov_L1)
mod_d1 <- BIFIE.twolevelreg(BIFIEobj = bdat, dep = "CREATSCH",
            formula.fixed = make_formula(preds_d1),
            formula.random = ~ 1, idcluster = "CNTSCHID", wgtlevel2 = WGT_MAIN)
etk_d1 <- extract_rows(mod_d1, "BELONG_KOR|BELONG_FIN|BELONG_MEX")
print(etk_d1, row.names = FALSE)

# ── OZET ─────────────────────────────────────────────────────
cat("\n\n========== OZET: ULKE FARKLARI (TUR = referans) ==========\n")
ozet <- rbind(
  data.frame(Yol="b3 (CREATSCH->CREATEFF)", etk_b3),
  data.frame(Yol="a1 (BULLIED->BELONG)",    etk_a1),
  data.frame(Yol="d1 (BELONG->CREATSCH)",   etk_d1)
)
ozet[,c("est","SE","t","p")] <- round(ozet[,c("est","SE","t","p")], 4)
ozet$Anlamli <- ifelse(ozet$p < .05, "EVET*", ifelse(ozet$p < .10, "marjinal", "Hayir"))
print(ozet, row.names = FALSE)

write.csv(ozet, here("output","tables","02_tablo13_ulke_etkilesim.csv"), row.names = FALSE)

cat("\n=== YORUM ===\n")
cat("Her terim, ilgili ulkenin yolunu TUR ile karsilastirir.\n")
cat("Anlamli (p<.05) -> o ulkede yol TUR'dan FARKLI.\n")
cat("Hicbiri anlamli degilse -> betimsel ulke farklari formel destekli DEGIL.\n")
cat("\nKaydedildi: output/tables/02_tablo13_ulke_etkilesim.csv\n")
