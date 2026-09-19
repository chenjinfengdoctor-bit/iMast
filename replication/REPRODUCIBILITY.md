# Reproducibility Information for the iMast JSS Paper

## Overview

This directory contains all materials needed to reproduce the numerical
results and figures presented in the JSS paper:

> Sun, Z. *iMast: A jamovi-Based Graphical Statistical Software for In
> Vitro Diagnostic Evaluation.* Journal of Statistical Software.

## Prerequisites

- **R** >= 4.0.0 (tested on R 4.3.1)
- **R packages**: `outliers`, `mcr`
- **Operating system**: Any OS supporting R (tested on Windows 11)

## Directory Structure

```
03_可复现性材料/
├── imast_replication.R      # Main replication script
├── REPRODUCIBILITY.md       # This file
├── data/                    # Data files used in case studies
│   ├── ivd_precisionevaluation.csv   # Case 1: EP05 data
│   ├── ivd_commutabilityep14a3.csv   # Case 2: EP09/EP14-A3 data
│   ├── ivd_ubb.csv                   # Case 3: u_bb homogeneity data
│   ├── ivd_us.csv                    # Case 3: u_ts stability data
│   └── ivd_uchar.csv                # Case 3: u_char characterization data
├── figures/                 # Generated PDF figures (created on run)
│   ├── fig3_precision_data.pdf
│   ├── fig4_passing_bablok.pdf
│   ├── fig5_bland_altman.pdf
│   ├── fig6_uncertainty_budget.pdf
│   └── fig7_monte_carlo.pdf
├── case1_ep05.R             # Individual case study scripts
├── case2_ep09.R
└── case3_gum.R
```

## Running the Replication

```r
# Set working directory to this folder
setwd("path/to/03_可复现性材料")

# Run the full replication script
source("imast_replication.R")
```

The script will:
1. Print R version, platform, and run metadata
2. Install missing packages automatically
3. Execute all three case studies in sequence
4. Generate all numerical results matching the paper
5. Save 5 PDF figures to the `figures/` subdirectory

## Expected Results

### Case 1: EP05 Precision Evaluation
- Grubbs outlier detection at alpha = 0.01 (no outliers expected)
- Level-specific SD and CV% for High-X, High-Y, Low-X, Low-Y
- Pooled SD: s_c ≈ sqrt((s_low² + s_high²) / 2)

### Case 2: EP09 Method Comparison (EP14-A3)
- OLS regression with prediction intervals for commutability assessment
- Deming regression (via mcr package)
- Passing-Bablok regression (via mcr package)
- Bland-Altman analysis with 95% limits of agreement

### Case 3: GUM Measurement Uncertainty
- u_bb: Between-bottle homogeneity (one-way ANOVA + conservative estimate)
- u_ts: Stability uncertainty from regression slope
- u_char: Characterization uncertainty (calibration + replicate + other)
- u_c: Combined standard uncertainty (root-sum-square, u_max/3 rule)
- U = 2 × u_c: Expanded uncertainty (k = 2, ~95% coverage)

## Random Seed

All analyses use deterministic computations (no random number generation
in the statistical methods themselves). The seed `set.seed(12345)` is set
at the top of the script for full reproducibility of any auxiliary random
processes.

## Correspondence to Paper Sections

| Paper Section | Script Section | Output |
|---|---|---|
| Section 5 (Case 1) | Case Study 1 in script | Table 3 + Figure 3 |
| Section 6 (Case 2) | Case Study 2 in script | Table 4 + Figures 4-5 |
| Section 7 (Case 3) | Case Study 3 in script | Table 5 + Figures 6-7 |

## Session Info

The script prints `sessionInfo()`-equivalent metadata at the top of its
output, including R version, platform, OS, run date/time, and seed value.

## Contact

Zhongjie Sun
School of Biological Science and Medical Engineering
Beihang University, Beijing, China
E-mail: sun_zj@buaa.edu.cn
