###############################################################################
## iMast JSS Paper -- Independent R Replication Script
##
## This script reproduces all numerical results and figures presented in the
## JSS paper "iMast: A jamovi-Based Graphical Statistical Software for
## In Vitro Diagnostic Evaluation" using standard R packages only.
##
## Requirements: R >= 4.0.0, packages: outliers, mcr
## Data files: ./data/*.csv (shipped with this script)
##
## Usage:
##   setwd("path/to/03_可复现性材料")
##   source("imast_replication.R")
##
## Author: Zhongjie Sun (sun_zj@buaa.edu.cn)
###############################################################################

## --- Reproducibility metadata ---
set.seed(12345)
cat("=============================================================\n")
cat("iMast JSS Paper Replication Script\n")
cat("=============================================================\n")
cat(sprintf("R version:   %s\n", R.version.string))
cat(sprintf("Platform:    %s\n", R.version$platform))
cat(sprintf("OS:          %s\n", Sys.info()["sysname"]))
cat(sprintf("Run date:    %s\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
cat(sprintf("Seed:        12345\n"))
cat("-------------------------------------------------------------\n\n")

## --- Package checks --------------------------------------------------------
required_pkgs <- c("outliers", "mcr")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("Installing missing package: %s\n", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org/")
  }
}
suppressPackageStartupMessages({
  library(outliers)
  library(mcr)
})
cat("Packages loaded: outliers, mcr\n\n")

## --- Data path -------------------------------------------------------------
## When run via source(), use the script directory; otherwise use current dir
script_dir <- tryCatch({
  dirname(normalizePath(sys.frame(1)$ofile))
}, error = function(e) {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("--file=", "", grep("--file=", args, value = TRUE))
  if (length(file_arg) > 0 && nzchar(file_arg)) dirname(normalizePath(file_arg))
  else "."
})
data_dir <- file.path(script_dir, "data")
if (!dir.exists(data_dir)) data_dir <- "./data"
cat(sprintf("Data directory: %s\n\n", normalizePath(data_dir)))

## --- Output directory for figures -----------------------------------------
fig_dir <- file.path(script_dir, "figures")
if (!dir.exists(fig_dir)) {
  fig_dir <- "./figures"
  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
}
cat(sprintf("Figure directory: %s\n\n", normalizePath(fig_dir)))

###############################################################################
## Case Study 1: EP05 Precision Evaluation
###############################################################################
cat("=============================================================\n")
cat("Case Study 1: EP05 Precision Evaluation\n")
cat("=============================================================\n\n")

df1 <- read.csv(file.path(data_dir, "ivd_precisionevaluation.csv"))
cat(sprintf("Data: %d rows x %d cols\n", nrow(df1), ncol(df1)))
cat(sprintf("Columns: %s\n\n", paste(names(df1), collapse = ", ")))

## --- Grubbs outlier test (iterative) ---
grubbs_check <- function(x, alpha = 0.01) {
  n <- length(x)
  while (n > 3) {
    g <- max(abs(x - mean(x))) / sd(x)
    pval <- pgrubbs(g, n, type = 10)
    if (pval < alpha) {
      idx <- which.max(abs(x - mean(x)))
      cat(sprintf("  Outlier removed: x=%.4f, G=%.4f, p=%.4f\n", x[idx], g, pval))
      x <- x[-idx]
      n <- n - 1
    } else {
      cat(sprintf("  No outlier (G=%.4f, p=%.4f). Remaining n=%d\n", g, pval, n))
      break
    }
  }
  x
}

cat("=== Grubbs Outlier Detection (alpha=0.01) ===\n")
cat("High level X (hcx):\n")
hcx_clean <- grubbs_check(df1$hcx)
cat("High level Y (hcy):\n")
hcy_clean <- grubbs_check(df1$hcy)
cat("Low level X (lcx):\n")
lcx_clean <- grubbs_check(df1$lcx)
cat("Low level Y (lcy):\n")
lcy_clean <- grubbs_check(df1$lcy)
cat("\n")

## --- Level-specific statistics ---
cat("=== Precision Statistics ===\n")
cat(sprintf("%-12s %8s %8s %8s %8s\n", "Level", "n", "Mean", "SD", "CV(%)"))
cat(sprintf("%-12s %8d %8.4f %8.4f %8.2f\n", "High-X", length(hcx_clean),
    mean(hcx_clean), sd(hcx_clean), 100*sd(hcx_clean)/mean(hcx_clean)))
cat(sprintf("%-12s %8d %8.4f %8.4f %8.2f\n", "High-Y", length(hcy_clean),
    mean(hcy_clean), sd(hcy_clean), 100*sd(hcy_clean)/mean(hcy_clean)))
cat(sprintf("%-12s %8d %8.4f %8.4f %8.2f\n", "Low-X",  length(lcx_clean),
    mean(lcx_clean), sd(lcx_clean), 100*sd(lcx_clean)/mean(lcx_clean)))
cat(sprintf("%-12s %8d %8.4f %8.4f %8.2f\n", "Low-Y",  length(lcy_clean),
    mean(lcy_clean), sd(lcy_clean), 100*sd(lcy_clean)/mean(lcy_clean)))

## --- Pooled SD ---
s_low  <- mean(c(sd(lcx_clean), sd(lcy_clean)))
s_high <- mean(c(sd(hcx_clean), sd(hcy_clean)))
s_c <- sqrt((s_low^2 + s_high^2) / 2)
cat(sprintf("\nPooled SD: s_c = sqrt((s_low^2 + s_high^2) / 2) = %.4f\n", s_c))
cat(sprintf("  s_low = %.4f, s_high = %.4f\n\n", s_low, s_high))

## --- Figure: precision dot plot ---
pdf(file.path(fig_dir, "fig3_precision_data.pdf"), width = 8, height = 5)
par(mfrow = c(1, 1), mar = c(4, 4, 3, 1))
boxplot(df1, col = c("#4e9eff", "#4e9eff", "#3ddc84", "#3ddc84"),
        names = c("High-X", "High-Y", "Low-X", "Low-Y"),
        ylab = "Measurement Value", main = "EP05 Precision: Replicate Measurements")
abline(h = c(100, 10), lty = 2, col = "gray50")
dev.off()
cat(sprintf("Figure saved: %s\n\n", file.path(fig_dir, "fig3_precision_data.pdf")))

###############################################################################
## Case Study 2: EP09 Method Comparison and Commutability (EP14-A3)
###############################################################################
cat("=============================================================\n")
cat("Case Study 2: EP09 Method Comparison and Commutability\n")
cat("=============================================================\n\n")

df2 <- read.csv(file.path(data_dir, "ivd_commutabilityep14a3.csv"),
  stringsAsFactors = FALSE)
cs_data <- df2[df2$csid != "" & !is.na(df2$csid), ]
rm_data <- df2[df2$rmid != "" & !is.na(df2$rmid), ]
cat(sprintf("Clinical samples: %d\n", nrow(cs_data)))
cat(sprintf("Reference materials: %d\n\n", nrow(rm_data)))

## --- Prepare CS mean values ---
cs_x <- rowMeans(cs_data[, c("csx1", "csx2")])
cs_y <- rowMeans(cs_data[, c("csy1", "csy2")])
rm_x <- rowMeans(rm_data[, c("rmx1", "rmx2")])
rm_y <- rowMeans(rm_data[, c("rmy1", "rmy2")])

cat("=== Reference Material Mean Values ===\n")
cat(sprintf("  %-6s %10s %10s\n", "RM_ID", "X_mean", "Y_mean"))
for (i in seq_along(rm_x)) {
  cat(sprintf("  %-6s %10.4f %10.4f\n", rm_data$rmid[i], rm_x[i], rm_y[i]))
}
cat("\n")

## --- OLS Regression ---
cat("=== OLS Regression ===\n")
ols <- lm(cs_y ~ cs_x)
b_ols <- coef(ols)[2]
a_ols <- coef(ols)[1]
S_YX <- summary(ols)$sigma
n <- length(cs_x)
cat(sprintf("  beta = %.6f, alpha = %.6f\n", b_ols, a_ols))
cat(sprintf("  S_Y.X = %.6f, n = %d, R^2 = %.4f\n\n", S_YX, n, summary(ols)$r.squared))

cat("  OLS Prediction Intervals at RM concentrations:\n")
cat(sprintf("  %-6s %10s %10s %12s %12s %10s  %s\n",
    "RM_ID", "X_mean", "Y_hat", "PI_lower", "PI_upper", "Y_mean", "Decision"))
for (i in seq_along(rm_x)) {
  xc <- rm_x[i]
  yhat <- a_ols + b_ols * xc
  se_pred <- S_YX * sqrt(1 + 1/n + (xc - mean(cs_x))^2 / sum((cs_x - mean(cs_x))^2))
  t_val <- qt(0.975, n - 2)
  pi_lower <- yhat - t_val * se_pred
  pi_upper <- yhat + t_val * se_pred
  y_rm <- rm_y[i]
  comm <- ifelse(y_rm >= pi_lower & y_rm <= pi_upper, "Commutable", "Non-commutable")
  cat(sprintf("  %-6s %10.4f %10.4f %12.4f %12.4f %10.4f  %s\n",
      rm_data$rmid[i], xc, yhat, pi_lower, pi_upper, y_rm, comm))
}

## --- Deming Regression ---
cat("\n=== Deming Regression ===\n")
dem <- mcreg(cs_x, cs_y, method.reg = "Deming")
dem_coef <- coef(dem)
cat(sprintf("  beta = %.6f, alpha = %.6f\n",
    dem_coef["Slope", "EST"], dem_coef["Intercept", "EST"]))
cat(sprintf("  95%% CI beta:  [%.6f, %.6f]\n",
    dem_coef["Slope", "LCI"], dem_coef["Slope", "UCI"]))

## --- Passing-Bablok Regression ---
cat("\n=== Passing-Bablok Regression ===\n")
pb <- mcreg(cs_x, cs_y, method.reg = "PaBa")
pb_coef <- coef(pb)
cat(sprintf("  beta = %.6f, alpha = %.6f\n",
    pb_coef["Slope", "EST"], pb_coef["Intercept", "EST"]))
cat(sprintf("  95%% CI beta:  [%.6f, %.6f]\n",
    pb_coef["Slope", "LCI"], pb_coef["Slope", "UCI"]))
cat(sprintf("  95%% CI alpha: [%.6f, %.6f]\n\n",
    pb_coef["Intercept", "LCI"], pb_coef["Intercept", "UCI"]))

## --- Bland-Altman analysis ---
cat("=== Bland-Altman Analysis ===\n")
ba_mean <- (cs_x + cs_y) / 2
ba_diff <- cs_y - cs_x
bias <- mean(ba_diff)
sd_diff <- sd(ba_diff)
cat(sprintf("  Mean bias: %.4f\n", bias))
cat(sprintf("  SD of differences: %.4f\n", sd_diff))
cat(sprintf("  95%% LoA: [%.4f, %.4f]\n\n",
    bias - 1.96 * sd_diff, bias + 1.96 * sd_diff))

## --- Figure: Passing-Bablok regression plot ---
pdf(file.path(fig_dir, "fig4_passing_bablok.pdf"), width = 8, height = 6)
par(mar = c(4, 4, 3, 1))
plot(cs_x, cs_y, pch = 19, col = "#4e9eff",
     xlab = "Method X (mean of duplicates)",
     ylab = "Method Y (mean of duplicates)",
     main = "Passing-Bablok Regression")
abline(a = pb_coef["Intercept", "EST"], b = pb_coef["Slope", "EST"],
       col = "#ff5f56", lwd = 2)
abline(a = 0, b = 1, lty = 2, col = "gray50")
legend("topleft",
       legend = c(sprintf("PB: y = %.4f + %.4f*x",
                          pb_coef["Intercept", "EST"],
                          pb_coef["Slope", "EST"]),
                  "Identity (y = x)"),
       lty = c(1, 2), col = c("#ff5f56", "gray50"), lwd = c(2, 1))
dev.off()
cat(sprintf("Figure saved: %s\n", file.path(fig_dir, "fig4_passing_bablok.pdf")))

## --- Figure: Bland-Altman plot ---
pdf(file.path(fig_dir, "fig5_bland_altman.pdf"), width = 8, height = 6)
par(mar = c(4, 4, 3, 1))
plot(ba_mean, ba_diff, pch = 19, col = "#4e9eff",
     xlab = "Mean of X and Y",
     ylab = "Difference (Y - X)",
     main = "Bland-Altman Plot")
abline(h = bias, col = "#ff5f56", lwd = 2)
abline(h = bias - 1.96 * sd_diff, col = "#ff5f56", lty = 2, lwd = 1)
abline(h = bias + 1.96 * sd_diff, col = "#ff5f56", lty = 2, lwd = 1)
legend("topleft",
       legend = c(sprintf("Bias = %.4f", bias),
                  sprintf("95%% LoA: [%.4f, %.4f]",
                          bias - 1.96 * sd_diff,
                          bias + 1.96 * sd_diff)),
       lty = c(1, 2), col = c("#ff5f56", "#ff5f56"), lwd = c(2, 1))
dev.off()
cat(sprintf("Figure saved: %s\n\n", file.path(fig_dir, "fig5_bland_altman.pdf")))

###############################################################################
## Case Study 3: GUM Measurement Uncertainty
###############################################################################
cat("=============================================================\n")
cat("Case Study 3: GUM Measurement Uncertainty\n")
cat("=============================================================\n\n")

## --- u_bb: Between-bottle homogeneity ---
cat("=== u_bb: Between-Bottle Homogeneity ===\n")
df_ubb <- read.csv(file.path(data_dir, "ivd_ubb.csv"))
cat(sprintf("Data: %d bottles x %d replicates\n", nrow(df_ubb), ncol(df_ubb)))
X <- as.matrix(df_ubb)
m <- nrow(X)
n_i <- ncol(X)
N <- m * n_i
grand_mean <- mean(X)
bottle_means <- rowMeans(X)
SS_bb <- sum(n_i * (bottle_means - grand_mean)^2)
v_bb <- m - 1
SS_wb <- sum((X - bottle_means)^2)
v_wb <- N - m
MS_bb <- SS_bb / v_bb
MS_wb <- SS_wb / v_wb
F_stat <- MS_bb / MS_wb
F_crit <- qf(0.95, v_bb, v_wb)
cat(sprintf("  MS_bb = %.6f, MS_wb = %.6f\n", MS_bb, MS_wb))
cat(sprintf("  F = %.4f, F_crit(0.05, %d, %d) = %.4f\n", F_stat, v_bb, v_wb, F_crit))
if (F_stat < F_crit) {
  u_bb <- sqrt(MS_wb / N) * (2 / v_wb)^(1/4)
  cat(sprintf("  F < F_crit -> Conservative estimate: u_bb = %.6f\n", u_bb))
} else {
  u_bb <- sqrt((MS_bb - MS_wb) / n_i)
  cat(sprintf("  F >= F_crit -> Direct estimate: u_bb = %.6f\n", u_bb))
}
cat("\n")

## --- u_ts: Stability uncertainty ---
cat("=== u_ts: Stability Uncertainty ===\n")
df_us <- read.csv(file.path(data_dir, "ivd_us.csv"))
cat(sprintf("Data: %d time points\n", nrow(df_us)))
time_pts <- df_us$time
values <- df_us[, 2]
fit <- lm(values ~ time_pts)
b1 <- coef(fit)[2]
s <- summary(fit)$sigma
s_b1 <- s / sqrt(sum((time_pts - mean(time_pts))^2))
t_span <- max(time_pts)
u_ts <- t_span * s_b1
t_stat <- abs(b1) / s_b1
t_crit <- qt(0.975, length(time_pts) - 2)
cat(sprintf("  Slope b1 = %.6f, s(b1) = %.6f\n", b1, s_b1))
cat(sprintf("  t = %.4f, t_crit = %.4f -> %s\n", t_stat, t_crit,
    ifelse(t_stat < t_crit, "No significant trend", "Significant trend")))
cat(sprintf("  t_span = %.1f, u_ts = %.6f\n\n", t_span, u_ts))

## --- u_char: Characterization uncertainty ---
cat("=== u_char: Characterization Uncertainty ===\n")
df_char <- read.csv(file.path(data_dir, "ivd_uchar.csv"))
cat(sprintf("Data: %d groups x %d replicates\n", nrow(df_char), ncol(df_char)))
X_char <- as.matrix(df_char)
C <- mean(X_char)
u_wcal_rel <- 0.5 / (2 * 100)
u_rep_rel <- sqrt(sum((X_char - mean(X_char))^2) /
  (length(X_char) * (length(X_char) - 1))) / C
u_or_rel <- 0.001
u_char_rel <- sqrt(u_wcal_rel^2 + u_rep_rel^2 + u_or_rel^2)
u_char <- u_char_rel * C
cat(sprintf("  u_wcal,rel = %.6f\n", u_wcal_rel))
cat(sprintf("  u_rep,rel = %.6f\n", u_rep_rel))
cat(sprintf("  u_char,rel = %.6f\n", u_char_rel))
cat(sprintf("  C (certified value) = %.4f\n", C))
cat(sprintf("  u_char = %.6f\n\n", u_char))

## --- CSU: Combined and expanded uncertainty ---
cat("=== CSU: Combined Standard Uncertainty ===\n")
components <- c(u_char = u_char, u_bb = u_bb, u_ts = u_ts)
u_max <- max(components)
cat("  Components:\n")
for (nm in names(components)) {
  included <- ifelse(components[nm] >= u_max / 3, "Included", "Omitted (<u_max/3)")
  cat(sprintf("    %s = %.6f  %s\n", nm, components[nm], included))
}
u_c <- sqrt(sum(components[components >= u_max / 3]^2))
U_exp <- 2 * u_c
cat(sprintf("\n  u_max = %.6f, threshold = u_max/3 = %.6f\n", u_max, u_max / 3))
cat(sprintf("  u_c = sqrt(sum of included) = %.6f\n", u_c))
cat(sprintf("  U = k * u_c = 2 * %.6f = %.6f\n\n", u_c, U_exp))

## --- Figure: Uncertainty budget bar chart ---
pdf(file.path(fig_dir, "fig6_uncertainty_budget.pdf"), width = 8, height = 5)
par(mar = c(6, 4, 3, 1))
barplot(components, col = c("#4e9eff", "#3ddc84", "#ff5f56"),
        names.arg = expression(u[char], u[bb], u[ts]),
        ylab = "Uncertainty Value",
        main = "Measurement Uncertainty Budget",
        las = 2)
abline(h = u_max / 3, lty = 2, col = "gray50")
text(3.5, u_max / 3 + 0.02, "u_max/3 threshold", cex = 0.8, adj = 1)
dev.off()
cat(sprintf("Figure saved: %s\n", file.path(fig_dir, "fig6_uncertainty_budget.pdf")))

## --- Figure: Monte Carlo simulation (stability regression) ---
pdf(file.path(fig_dir, "fig7_monte_carlo.pdf"), width = 8, height = 5)
par(mar = c(4, 4, 3, 1))
plot(time_pts, values, pch = 19, col = "#4e9eff",
     xlab = "Time Point", ylab = "Measurement Value",
     main = "Stability Study: Linear Regression")
abline(fit, col = "#ff5f56", lwd = 2)
conf_interval <- predict(fit, interval = "confidence", level = 0.95)
matlines(time_pts, conf_interval, col = "#ff5f56", lty = 2, lwd = 1)
legend("topleft",
       legend = c(sprintf("Slope = %.4f (p=%.4f)", b1,
                          summary(fit)$coefficients[2, 4]),
                  "95% CI"),
       lty = c(1, 2), col = c("#ff5f56", "#ff5f56"), lwd = c(2, 1))
dev.off()
cat(sprintf("Figure saved: %s\n\n", file.path(fig_dir, "fig7_monte_carlo.pdf")))

###############################################################################
## Summary
###############################################################################
cat("=============================================================\n")
cat("Replication Complete\n")
cat("=============================================================\n")
cat(sprintf("Case 1 (EP05):  s_c = %.4f\n", s_c))
cat(sprintf("Case 2 (EP09):  PB slope = %.6f, intercept = %.6f\n",
    pb_coef["Slope", "EST"], pb_coef["Intercept", "EST"]))
cat(sprintf("Case 3 (GUM):   u_c = %.4f, U = %.4f\n", u_c, U_exp))
cat("All figures saved to: figures/\n")
cat("=============================================================\n")
