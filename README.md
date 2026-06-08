# Creative School Climate as a Link Between Bullying Victimization, School Belonging, and Creative Self-Efficacy: Evidence From PISA 2022 Across Four Countries

Analysis code and documentation for the study examining how creative school
climate mediates the serial pathway from bullying victimization through school
belonging to creative self-efficacy, using PISA 2022 data from four countries
(Turkey, Korea, Finland, Mexico).

## Authors

- **Özge Yiğit** (First author) — Doctoral Candidate, Department of Educational Administration, Faculty of Education, Pamukkale University, Denizli, Türkiye. ORCID: [0009-0000-3534-3474](https://orcid.org/0009-0000-3534-3474)
- **Kazım Çelik** (Second author, corresponding author) — Prof. Dr., Department of Educational Administration, Faculty of Education, Pamukkale University, Denizli, Türkiye. ORCID: [0000-0001-7319-6567](https://orcid.org/0000-0001-7319-6567)

## Citation

> Yiğit, Ö., & Çelik, K. (2026). *Creative school climate as a link between bullying victimization, school belonging, and creative self-efficacy: Evidence from PISA 2022 across four countries*
> [Analysis code and codebook]. GitHub. https://github.com/Ozgetasmanisa/creativity

## License

Code and documentation: **CC BY 4.0** (see `LICENSE`).
PISA 2022 raw data are **not** included (OECD property); see Data Access below.

## Data Access

This repository does **not** contain the PISA 2022 raw data. To reproduce the
analyses, download the student questionnaire data file from the OECD PISA 2022
database (publicly available):

- Source: https://www.oecd.org/pisa/data/
- File used: `CY08MSP_STU_QQQ.sav` (student questionnaire)
- Place it in a local `data/raw/` folder (this folder is git-ignored).

The four analytic countries are Turkey (TUR), Korea (KOR), Finland (FIN), and
Mexico (MEX); the analytic sample is 26,993 students nested in 891 schools.

## Software

- R (version 4.x or later)
- Key packages:
  - `BIFIEsurvey` (3.8) — design-based two-level regression with BRR weights
  - `lavaan` — multigroup confirmatory factor analysis (measurement invariance)
  - `dplyr`, `haven`, `here`

Install:
```r
install.packages(c("BIFIEsurvey", "lavaan", "dplyr", "haven", "here"))
```

## Reproduction Steps

Run the scripts in `scripts/` in the following order:

| Script | Purpose | Output |
|--------|---------|--------|
| `2_00_dosya_tasima.R` | File/path setup | — |
| `2_01_veri_dogrulama_ICC.R` | Data validation, ICC | Table 3 (ICC) |
| `2_02_paralel_mediation.R` | Parallel mediation models | Tables 5–6 |
| `2_03_seri_mediation.R` | Serial mediation (4 equations/country) | Tables 7–8, `02_modeller_seri.rds` |
| `2_04_robustness.R` | Robustness / sensitivity checks | Tables 9–10 |
| `2_05_olcme_degismezligi_final.R` | Measurement invariance (MGCFA, CREATSCH) | Table 12 |
| `2_06_ulke_farklari_etkilesim_v2.R` | Formal cross-national difference tests | Table 13 |
| `2_07_moderated_mediation_cinsiyet.R` | Index of moderated mediation (gender) | Table 14 |

All models use:
- Final student weight `W_FSTUWT` and 80 BRR replicate weights (`W_FSTURWT1`–`80`)
- Fay factor = 0.5
- Centering within cluster (CWC) for continuous predictors (Enders & Tofighi, 2007)
- Random-intercept two-level structure (students within schools)

A fixed seed (`set.seed(2026)`) is used for all Monte Carlo confidence
intervals (20,000 draws) to ensure reproducibility.

## Repository Structure

```
.
├── README.md
├── LICENSE
├── CODEBOOK.md            # Variable dictionary and analytic decisions
├── scripts/               # All R analysis scripts (2_00 – 2_07)
└── output/
    ├── tables/            # Generated result tables (CSV)
    └── figures/           # Path diagram and forest plot (PNG/TIFF)
```

## Notes on Measurement

- **CREATSCH** (creative school climate, 4 items: ST335Q01/Q02/Q05/Q06) was
  tested for measurement invariance at the item level via MGCFA (WLSMV);
  full scalar invariance held across the four countries.
- **CREATEFF** (creative self-efficacy, items ST334) was administered under a
  rotated (matrix-sampling) design (~2.4 of 5 items per student), so item-level
  MGCFA was not feasible; the OECD-calibrated WLE index was used (OECD, 2024b).
