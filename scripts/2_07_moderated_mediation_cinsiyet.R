# ============================================================
# 2_07_moderated_mediation_cinsiyet.R
# Index of Moderated Mediation - Cinsiyet (Hakem Raporu Madde 8)
#
# SORUN: "Kizlarda anlamli, erkeklerde degil" -> moderasyon KANITI DEGIL.
# DOGRU TEST: Kiz ve erkek seri dolayli etkilerinin FARKI anlamli mi?
#   index of moderated mediation = serial_kiz - serial_erkek
#   Bu farkin %95 Monte Carlo CI'si sifiri disliyorsa -> moderasyon VAR.
#
# Yontem: Her ulke icin kiz/erkek alt-orneklemde 4 denklem ayri
#   kosulur, seri dolayli etki (a1*d1*b3) hesaplanir, sonra
#   Monte Carlo ile FARKIN dagilimi simule edilir.
#
# Calisan kalip: 2_03_seri_mediation.R ile ayni (BIFIE.data data=...)
# ============================================================

library(here)
library(dplyr)
library(BIFIEsurvey)

dat <- readRDS(here("data","derived","dat_4ulke_Q1.rds"))

WGT_MAIN   <- "W_FSTUWT"
rep_mevcut <- paste0("W_FSTURWT", 1:80)
rep_mevcut <- rep_mevcut[rep_mevcut %in% colnames(dat)]
FAY_FAC    <- 0.5
# Cinsiyet kovaryattan cikar (gruplama degiskeni oldu)
kov_L1 <- intersect(c("ESCS_CWC","ANXMAT_CWC","FAMSUP_CWC"), colnames(dat))
ulkeler <- c("TUR","KOR","FIN","MEX")

# ST004D01T: 1 = kiz, 2 = erkek
cat("Cinsiyet dagilimi (1=kiz, 2=erkek):\n")
print(table(dat$ST004D01T, useNA="always"))

make_formula <- function(preds) as.formula(paste("~", paste(preds, collapse=" + ")))
extract_coef <- function(m, par) {
  ct <- m$stat; idx <- which(ct$parameter == paste0("beta_", par))
  if (length(idx)==0) return(list(est=NA,se=NA))
  list(est=ct$est[idx[1]], se=ct$SE[idx[1]])
}

bifie_cols <- unique(c(
  "BELONG","CREATSCH","CREATEFF",
  "BULLIED_CWC","BELONG_CWC","FEELSAFE_CWC","CREATSCH_CWC",
  kov_L1, "ST004D01T",
  grep("_CWC$|_SCH$", colnames(dat), value=TRUE),
  "CNTSCHID", WGT_MAIN, rep_mevcut
))
bifie_cols <- intersect(bifie_cols, colnames(dat))

# Seri dolayli etki + SE hesaplama (bir alt-orneklem icin)
serial_indirect <- function(d_sub) {
  for (col in colnames(d_sub))
    if (!is.numeric(d_sub[[col]])) d_sub[[col]] <- as.numeric(as.character(d_sub[[col]]))

  bd <- BIFIE.data(data=d_sub, wgt=WGT_MAIN,
                   wgtrep=as.matrix(d_sub[, rep_mevcut]), fayfac=FAY_FAC)

  m1 <- BIFIE.twolevelreg(bd, dep="BELONG",
        formula.fixed=make_formula(c("BULLIED_CWC",kov_L1)),
        formula.random=~1, idcluster="CNTSCHID", wgtlevel2=WGT_MAIN)
  m3 <- BIFIE.twolevelreg(bd, dep="CREATSCH",
        formula.fixed=make_formula(c("BELONG_CWC","FEELSAFE_CWC","BULLIED_CWC",kov_L1)),
        formula.random=~1, idcluster="CNTSCHID", wgtlevel2=WGT_MAIN)
  m4 <- BIFIE.twolevelreg(bd, dep="CREATEFF",
        formula.fixed=make_formula(c("BULLIED_CWC","BELONG_CWC","FEELSAFE_CWC","CREATSCH_CWC",kov_L1)),
        formula.random=~1, idcluster="CNTSCHID", wgtlevel2=WGT_MAIN)

  a1 <- extract_coef(m1,"BULLIED_CWC")
  d1 <- extract_coef(m3,"BELONG_CWC")
  b3 <- extract_coef(m4,"CREATSCH_CWC")
  list(a1=a1, d1=d1, b3=b3)
}

# Index of moderated mediation: kiz - erkek seri dolayli farki
set.seed(2026)
NSIM <- 20000

sonuc <- data.frame()

for (cnt in ulkeler) {
  cat(sprintf("\n\n======== %s ========\n", cnt))

  d_kiz <- dat %>% filter(CNT==cnt, ST004D01T==1) %>% arrange(CNTSCHID) %>%
           select(all_of(bifie_cols)) %>% as.data.frame()
  d_erk <- dat %>% filter(CNT==cnt, ST004D01T==2) %>% arrange(CNTSCHID) %>%
           select(all_of(bifie_cols)) %>% as.data.frame()

  cat(sprintf("  Kiz N=%d, Erkek N=%d\n", nrow(d_kiz), nrow(d_erk)))

  ck <- tryCatch(serial_indirect(d_kiz), error=function(e){cat("KIZ HATA:",e$message,"\n"); NULL})
  ce <- tryCatch(serial_indirect(d_erk), error=function(e){cat("ERKEK HATA:",e$message,"\n"); NULL})
  if (is.null(ck) || is.null(ce)) next

  # Monte Carlo: her grup icin seri dolayli dagilimi
  sk <- rnorm(NSIM, ck$a1$est, ck$a1$se) * rnorm(NSIM, ck$d1$est, ck$d1$se) *
        rnorm(NSIM, ck$b3$est, ck$b3$se)
  se <- rnorm(NSIM, ce$a1$est, ce$a1$se) * rnorm(NSIM, ce$d1$est, ce$d1$se) *
        rnorm(NSIM, ce$b3$est, ce$b3$se)

  # Index of moderated mediation = fark
  imm <- sk - se
  ci_imm <- quantile(imm, c(.025,.975))
  ci_k   <- quantile(sk, c(.025,.975))
  ci_e   <- quantile(se, c(.025,.975))

  cat(sprintf("  Kiz   serial1 = %.5f  [%.5f, %.5f]\n", mean(sk), ci_k[1], ci_k[2]))
  cat(sprintf("  Erkek serial1 = %.5f  [%.5f, %.5f]\n", mean(se), ci_e[1], ci_e[2]))
  cat(sprintf("  INDEX (kiz-erkek) = %.5f  [%.5f, %.5f]  %s\n",
      mean(imm), ci_imm[1], ci_imm[2],
      ifelse(ci_imm[1]>0 | ci_imm[2]<0, "MODERASYON VAR*", "moderasyon yok")))

  sonuc <- rbind(sonuc, data.frame(
    Ulke=cnt,
    serial_kiz=round(mean(sk),5), kiz_lo=round(ci_k[1],5), kiz_hi=round(ci_k[2],5),
    serial_erkek=round(mean(se),5), erk_lo=round(ci_e[1],5), erk_hi=round(ci_e[2],5),
    index_MM=round(mean(imm),5), imm_lo=round(ci_imm[1],5), imm_hi=round(ci_imm[2],5),
    moderasyon=ifelse(ci_imm[1]>0 | ci_imm[2]<0, "VAR*", "yok")
  ))
}

cat("\n\n========== INDEX OF MODERATED MEDIATION OZETI ==========\n")
print(sonuc, row.names=FALSE)

write.csv(sonuc, here("output","tables","02_tablo14_moderated_mediation.csv"), row.names=FALSE)

cat("\n=== YORUM ===\n")
cat("index_MM = kiz serial1 - erkek serial1 (BULLIED->BELONG->CREATSCH->CREATEFF)\n")
cat("CI sifiri dislarsa (VAR*) -> cinsiyet GERCEK moderator.\n")
cat("CI sifiri icerirse -> 'sadece kizlarda anlamli' ifadesi moderasyon kaniti DEGIL;\n")
cat("  iki grup arasinda istatistiksel fark yok demektir.\n")
cat("\nKaydedildi: output/tables/02_tablo14_moderated_mediation.csv\n")
