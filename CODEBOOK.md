# CODEBOOK — Creative School Climate, Bullying Victimization, and Creative Self-Efficacy (PISA 2022, Four Countries)

Documentation of all variables, codings, and analytic decisions so that the
shared scripts can be reproduced by an independent researcher.

---

## 1. Repository Information

| Field | Value |
|-------|-------|
| Study title | Creative School Climate as a Link Between Bullying Victimization, School Belonging, and Creative Self-Efficacy: Evidence From PISA 2022 Across Four Countries |
| Authors / ORCID | Özge Yiğit (first author), Pamukkale University, ORCID 0009-0000-3534-3474; Kazım Çelik (second author, corresponding), Pamukkale University, ORCID 0000-0001-7319-6567 |
| Related publication (DOI) | To be assigned upon journal acceptance |
| Repository platform | GitHub (archived to Zenodo for permanent DOI) |
| Repository URL | https://github.com/Ozgetasmanisa/creativity |
| Zenodo DOI | 10.5281/zenodo.20562727 (https://doi.org/10.5281/zenodo.20562727) |
| Anonymous review link | [view-only/anonymized link if required] |
| License | Code & derived materials: CC BY 4.0 |
| Version / date | v1.1 / 2026 |
| Contact | Kazım Çelik, kcelik@pau.edu.tr |

## 2. License and Citation

License: **CC BY 4.0** for all code and derived materials. The PISA 2022 raw
data are subject to OECD terms and are **not** redistributed here; only the
access path and derivation scripts are shared.

Suggested citation:
> Yiğit, Ö., & Çelik, K. (2026). *Creative school climate as a link between bullying victimization, school belonging, and creative self-efficacy: Evidence from PISA 2022 across four countries*
> [Analysis code and codebook]. Zenodo. https://doi.org/10.5281/zenodo.20562727

## 3. Data Source and Access

- Source: PISA 2022 student questionnaire and context indices (OECD).
- Access: OECD PISA database, https://www.oecd.org/pisa/data/ (public).
- File used: `CY08MSP_STU_QQQ.sav` (student questionnaire data file).
- Countries: Turkey (TUR), Korea (KOR), Finland (FIN), Mexico (MEX).
- Analytic sample: 26,993 students / 891 schools
  (TUR 6,999/196; KOR 6,288/185; FIN 8,272/240; MEX 5,434/270).

## 4. File Manifest

| File | Description |
|------|-------------|
| README.md | Repository overview, run order, software, license |
| CODEBOOK.md | This document: variable dictionary and analytic decisions |
| scripts/2_00_dosya_tasima.R | Path/file setup |
| scripts/2_01_veri_dogrulama_ICC.R | Data validation, ICC computation |
| scripts/2_02_paralel_mediation.R | Parallel mediation models |
| scripts/2_03_seri_mediation.R | Serial mediation (4 equations/country) |
| scripts/2_04_robustness.R | Robustness / sensitivity checks |
| scripts/2_05_olcme_degismezligi_final.R | Measurement invariance (MGCFA, CREATSCH) |
| scripts/2_06_ulke_farklari_etkilesim_v2.R | Formal cross-national difference tests |
| scripts/2_07_moderated_mediation_cinsiyet.R | Index of moderated mediation (gender) |
| output/tables/ | Generated result tables (CSV) |
| output/figures/ | Path diagram, forest plot (PNG/TIFF) |

## 5. Sample Derivation and Filters

From the full PISA 2022 student file:
```r
# Country selection
dat <- subset(raw, CNT %in% c("TUR","KOR","FIN","MEX"))
# Focal variables
focal <- c("BULLIED","BELONG","FEELSAFE","CREATSCH","CREATEFF")
```
- Analytic n derived by retaining the four countries and valid design
  information (weights, school IDs).
- Note: PISA's rotated test-form design means the creativity self-efficacy
  items (ST334) were administered to only a subset of students per form
  (~2.4 of 5 items per student); the OECD WLE composite index (CREATEFF) is
  used for the main models, which is defined for the full analytic sample.

## 6. Variable Dictionary

### 6.1 Focal variables

| Analysis name | PISA source | Label | Coding / scale | Role |
|---------------|-------------|-------|----------------|------|
| BULLIED | BULLIED | Exposure to bullying | WLE composite index; OECD M=0, SD=1; higher = more | Predictor (X) |
| BELONG | BELONG | Sense of school belonging | WLE composite index; std (0,1) | Mediator (M1) |
| FEELSAFE | FEELSAFE | Feeling safe at school | WLE composite index; std (0,1) | Mediator (M2) |
| CREATSCH | CREATSCH | Creative school and class environment (new in 2022) | WLE composite index; std (0,1) | Mediator (M3) |
| CREATEFF | CREATEFF | Creative self-efficacy (CSE) | WLE composite index; std (0,1) | Outcome (Y) |

CREATSCH item-level source (used in invariance test, ST335):
ST335Q01 (teachers give time for creative solutions), ST335Q02 (teachers value
creativity), ST335Q05 (assignments require different solutions), ST335Q06
(teachers encourage original answers).

CREATEFF item-level source (matrix-sampled, ST334): ST334Q01–Q05 (confidence in
coming up with creative ideas, being creative, telling creative stories,
expressing ideas creatively, making creative drawings).

### 6.2 Covariates and design variables

| Analysis name | PISA source | Label | Coding | Role |
|---------------|-------------|-------|--------|------|
| ESCS | ESCS | Economic, social & cultural status | Continuous; std | Covariate |
| ANXMAT | ANXMAT | Mathematics anxiety | WLE composite; std | Covariate |
| FAMSUP | FAMSUP | Family support | WLE composite; std | Covariate |
| gender | ST004D01T | Gender | 1 = girl, 2 = boy | Covariate / moderator |
| country | CNT | Country | TUR, KOR, FIN, MEX | Grouping / stratification |
| school_id | CNTSCHID | School identifier | Cluster (Level-2) | Clustering |
| student_id | CNTSTUID | Student identifier | Unit (Level-1) | Identifier |
| w_final | W_FSTUWT | Final student weight | Continuous | Sampling weight |
| w_rep1–80 | W_FSTURWT1–80 | BRR replicate weights | 80 replicates | Variance (BRR, Fay=0.5) |

## 7. Derived Variables (Centering Within Cluster, CWC)

All continuous predictors were decomposed via centering within cluster
(Enders & Tofighi, 2007), separating within-school and between-school variance:
```r
library(dplyr)
dat <- dat %>% group_by(CNTSCHID) %>%
  mutate(BULLIED_CWC = BULLIED - mean(BULLIED, na.rm = TRUE)) %>%
  ungroup()
# Repeated for BELONG, FEELSAFE, CREATSCH, ESCS, ANXMAT, FAMSUP
```
Produced: BULLIED_CWC, BELONG_CWC, FEELSAFE_CWC, CREATSCH_CWC, ESCS_CWC,
ANXMAT_CWC, FAMSUP_CWC (and corresponding _SCH school-mean variables).

## 8. Missing Data Handling

- Main models: listwise within each design-based regression (BIFIE handles
  weights and replicate weights on available cases).
- PISA missing codes (e.g., 95–99 / system missing) recoded to NA before
  analysis; for invariance items, values > 90 set to NA.
- Item missingness for focal scales ranged from 3% to 19% (CREATSCH) up to
  ~55% for individual CREATEFF items due to the rotated form design.

## 9. Weighting and Variance (BRR)

- Final weight: W_FSTUWT. Replicate weights: W_FSTURWT1–W_FSTURWT80.
- Variance: balanced repeated replication (BRR), Fay factor = 0.5 (OECD, 2024b).
- Multilevel structure: two-level random-intercept model; justification is the
  two-stage cluster sampling design (students within schools).
- Estimator: `BIFIE.twolevelreg` (BIFIEsurvey package).

## 10. Software and Package Versions

- R version: 4.5.3 (2026-03-11 ucrt), platform x86_64-w64-mingw32/x64, Windows 10 x64
- BIFIEsurvey: 3.8 (Robitzsch & Oberwimmer, 2022)
- lavaan: used for MGCFA measurement invariance (record exact version from sessionInfo() after running scripts)
- Other: dplyr, haven, here
- Full session details should be saved to `sessionInfo.txt`.

## 11. Reproduction Order

1. Download the raw PISA file from OECD into `data/raw/` (not in repo).
2. `2_01_veri_dogrulama_ICC.R` — validation, ICC.
3. `2_02_paralel_mediation.R` — parallel mediation.
4. `2_03_seri_mediation.R` — serial mediation (Tables 7–8).
5. `2_04_robustness.R` — sensitivity checks.
6. `2_05_olcme_degismezligi_final.R` — measurement invariance (CREATSCH).
7. `2_06_ulke_farklari_etkilesim_v2.R` — cross-national difference tests.
8. `2_07_moderated_mediation_cinsiyet.R` — gender index of moderated mediation.
```r
set.seed(2026)   # required for Monte Carlo reproducibility (20,000 draws)
```

## 12. Analytic Decision Log

Key decisions, documented to preempt reviewer questions:

- **Outcome choice:** Creative self-efficacy (not objective creative-thinking
  performance) was the terminal outcome, because (a) creative self-beliefs are a
  consequential outcome in their own right under the creative-agency framework,
  and (b) some countries (incl. Turkey) did not take the PISA cognitive
  creative-thinking test, whereas all four provided the perception indices.
- **Standardized indirect effects:** Indirect effects are products of
  unstandardized path coefficients; Monte Carlo CIs (20,000 draws) were used
  (Preacher & Selig, 2012).
- **Measurement invariance:** CREATSCH tested at item level via MGCFA (WLSMV,
  theta parameterization); full scalar invariance held (ΔCFI ≤ .010,
  ΔRMSEA ≤ .015; Chen, 2007; Cheung & Rensvold, 2002). CREATEFF item-level
  invariance not feasible (matrix sampling); OECD WLE index used.
- **Cross-national differences:** Tested formally via a pooled model with
  country × predictor interactions (Turkey as reference), not by comparing
  separate-country significance. Only the a1 path (bullying → belonging)
  differed significantly (Finland vs Turkey, p = .004); b3 and d1 did not differ.
- **Gender moderation:** Tested via index of moderated mediation (girls' minus
  boys' serial indirect effect) with Monte Carlo CIs. No index reached
  significance in any country; subgroup-only significance was not interpreted as
  moderation.
- **p < .10 results:** labeled "marginal" and not treated as confirmatory.

## 13. Version History

| Version | Date | Change |
|---------|------|--------|
| v1.0 | 2026 | Initial submission version |
| v1.1 | 2026 | Documentation update |

---

*Note. This codebook documents secondary analysis of publicly available PISA
2022 data. No ethics approval was required (secondary analysis of de-identified,
publicly released data); no funding was received; the authors declare no
conflicts of interest. PISA variable names should be confirmed against the
official PISA 2022 codebook; raw data are not redistributed here.*
