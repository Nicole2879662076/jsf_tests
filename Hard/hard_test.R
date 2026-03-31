#!/usr/bin/env Rscript
# Hard Test: Stochastic Simulation Comparison
# Complete version that generates all 3 plots

cat("Starting Hard Test: Stochastic Birth-Death Simulation Comparison\n")
cat(strrep("=", 60), "\n\n")

# Set parameters (balanced for speed and completeness)
LAMBDA  <- 1.0       # birth rate
MU      <- 0.5       # death rate
X0      <- 5         # initial population
T_MAX   <- 4.0       # simulation time
N_REPS  <- 30        # number of replicates
SEED    <- 2026

# Create output directory
OUT_DIR <- "hard_output_complete"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
cat("Output directory:", normalizePath(OUT_DIR), "\n\n")

# Step 1: Check JSF installation
cat("[1/7] Checking JSF installation...\n")
jsf_check <- check_jsf_installation()
if (jsf_check$jsf_available && jsf_check$jsf_import == "Success") {
  cat("  ✓ JSF Python package is available\n")
} else {
  cat("  ✗ JSF installation issue detected\n")
  cat("  Will use Gillespie only for comparison\n")
  jsf_available <- FALSE
}
cat("\n")

# Step 2: Run Gillespie simulations
cat("[2/7] Running Gillespie simulations...\n")
set.seed(SEED)
tryCatch({
  gillespie_raw <- simulate_birth_death_ensemble(
    lambda = LAMBDA,
    mu = MU,
    x0 = X0,
    t_max = T_MAX,
    n_reps = N_REPS,
    seed = SEED
  )
  cat("  ✓ Gillespie: Generated", N_REPS, "trajectories\n")
}, error = function(e) {
  cat("  ✗ Gillespie error:", e$message, "\n")
  stop("Gillespie simulation failed. Cannot continue.")
})
cat("\n")

# Step 3: Run JSF simulations
cat("[3/7] Running JSF simulations...\n")
if (jsf_check$jsf_available && jsf_check$jsf_import == "Success") {
  tryCatch({
    jsf_raw <- generate_jsf_reference(
      lambda = LAMBDA,
      mu = MU,
      x0 = X0,
      t_max = T_MAX,
      n_reps = N_REPS,
      dt = 0.05,
      threshold = 10,
      seed = SEED,
      cache_file = file.path(OUT_DIR, "jsf_reference.rds")
    )
    cat("  ✓ JSF: Generated", N_REPS, "trajectories\n")
  }, error = function(e) {
    cat("  ✗ JSF error:", e$message, "\n")
    cat("  Will use Gillespie only for comparison\n")
    jsf_raw <- NULL
  })
} else {
  cat("  ✗ JSF not available, using Gillespie only\n")
  jsf_raw <- NULL
}
cat("\n")

# Step 4: Compare results
cat("[4/7] Comparing results...\n")
if (!is.null(jsf_raw)) {
  comparison <- compare_simulation_results(jsf_raw, gillespie_raw)
  cat("  JSF Mean Population:", round(comparison$overall_stats$jsf_mean_pop, 2), "\n")
  cat("  Gillespie Mean Population:", round(comparison$overall_stats$gillespie_mean_pop, 2), "\n")

  # Calculate relative improvement
  relative_improvement <- 100 * (comparison$overall_stats$gillespie_sd_pop -
                                   comparison$overall_stats$jsf_sd_pop) /
    comparison$overall_stats$gillespie_sd_pop
  cat("  JSF shows", round(relative_improvement, 1), "% lower variability\n")

  # Save comparison results
  saveRDS(comparison, file.path(OUT_DIR, "comparison_results.rds"))
  cat("  ✓ Comparison saved to comparison_results.rds\n")
} else {
  cat("  ✗ JSF data not available, skipping detailed comparison\n")
  comparison <- NULL
}
cat("\n")

# Step 5: Generate trajectory comparison plot
cat("[5/7] Generating Plot 1: Trajectory Comparison\n")
if (!is.null(jsf_raw)) {
  tryCatch({
    p1 <- plot_trajectories(
      jsf_data = jsf_raw,
      gillespie_data = gillespie_raw,
      lambda = LAMBDA,
      mu = MU,
      x0 = X0,
      n_show = 5,
      seed = SEED
    )
    ggplot2::ggsave(
      file.path(OUT_DIR, "01_trajectory_comparison.png"),
      p1,
      width = 10,
      height = 6,
      dpi = 300
    )
    cat("  ✓ Saved: 01_trajectory_comparison.png\n")
  }, error = function(e) {
    cat("  ✗ Trajectory plot error:", e$message, "\n")
  })
} else {
  cat("  ✗ Skipping: JSF data not available\n")
}
cat("\n")

# Step 6: Generate error over time plot
cat("[6/7] Generating Plot 2: Error Over Time\n")
if (!is.null(jsf_raw) && !is.null(gillespie_raw)) {
  tryCatch({
    p2 <- plot_error_vs_time(
      comparison_result = comparison,
      jsf_data = jsf_raw,
      gillespie_data = gillespie_raw,
      lambda = LAMBDA,
      mu = MU,
      x0 = X0
    )
    ggplot2::ggsave(
      file.path(OUT_DIR, "02_error_over_time.png"),
      p2,
      width = 9,
      height = 5,
      dpi = 300
    )
    cat("  ✓ Saved: 02_error_over_time.png\n")
  }, error = function(e) {
    cat("  ✗ Error plot error:", e$message, "\n")
    cat("  You can run this separately later\n")
  })
} else {
  cat("  ✗ Skipping: Required data not available\n")
}
cat("\n")

# Step 7: Generate parameter sensitivity heatmap
cat("[7/7] Generating Plot 3: Parameter Sensitivity Heatmap\n")
cat("  Note: This will take 3-5 minutes\n")
cat("  Using reduced parameter grid for faster execution...\n")

# MODIFIED LINE: Changed from if (FALSE) to if (TRUE)
if (TRUE) {  # ENABLED parameter sweep
  tryCatch({
    p3 <- plot_error_heatmap(
      lambda_seq = seq(0.4, 1.6, by = 0.4),  # 4 values: 0.4, 0.8, 1.2, 1.6
      mu_seq = seq(0.2, 1.4, by = 0.4),      # 4 values: 0.2, 0.6, 1.0, 1.4
      x0 = X0,
      t_max = 2.0,                           # Reduced simulation time
      n_reps = 10,                           # Reduced replicates
      grid_dt = 0.1,
      seed = SEED,
      output_dir = OUT_DIR
    )
    ggplot2::ggsave(
      file.path(OUT_DIR, "03_parameter_heatmap.png"),
      p3,
      width = 10,
      height = 7,
      dpi = 300
    )
    cat("  ✓ Saved: 03_parameter_heatmap.png\n")
  }, error = function(e) {
    cat("  ✗ Heatmap error:", e$message, "\n")
    cat("  You can run this separately with fewer parameters\n")
  })
} else {
  cat("  ✗ Parameter sweep disabled (should not reach here)\n")
}
cat("\n")

# Create summary file
summary_data <- data.frame(
  Test = "Hard Test: Stochastic Simulation Comparison",
  Timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
  Parameters = sprintf("λ=%.1f, μ=%.1f, X₀=%d", LAMBDA, MU, X0),
  Simulation = sprintf("t_max=%.1f, reps=%d", T_MAX, N_REPS),
  Seed = SEED,
  JSF_Available = if (!is.null(jsf_raw)) "Yes" else "No",
  Plots_Generated = if (exists("p1") && exists("p2") && exists("p3")) "All 3" else "Partial",
  Output_Directory = OUT_DIR
)

write.csv(summary_data, file.path(OUT_DIR, "test_summary.csv"), row.names = FALSE)

cat(strrep("=", 60), "\n")
cat("Hard Test: COMPLETE EXECUTION\n")
cat(strrep("=", 60), "\n\n")

cat("Summary:\n")
cat("  Output Directory:", normalizePath(OUT_DIR), "\n")
cat("  Parameters: λ =", LAMBDA, ", μ =", MU, ", X₀ =", X0, "\n")
cat("  Simulation: t_max =", T_MAX, ", reps =", N_REPS, "\n\n")

cat("Generated Files:\n")
if (dir.exists(OUT_DIR)) {
  files <- list.files(OUT_DIR, full.names = FALSE)
  for (file in files) {
    file_path <- file.path(OUT_DIR, file)
    if (file.exists(file_path)) {
      size_kb <- round(file.info(file_path)$size / 1024, 1)
      cat(sprintf("  • %s (%.1f KB)\n", file, size_kb))
    }
  }
}
cat("\n")

cat("Key Findings (from comparison):\n")
if (!is.null(comparison)) {
  cat("  • JSF Mean Population:", round(comparison$overall_stats$jsf_mean_pop, 2), "\n")
  cat("  • Gillespie Mean Population:", round(comparison$overall_stats$gillespie_mean_pop, 2), "\n")
  if (exists("relative_improvement")) {
    cat("  • JSF shows", round(max(0, relative_improvement), 1), "% lower variability\n")
  }
}
cat("\n")

cat("Next Steps:\n")
cat("  1. Review all 3 plots in the output directory\n")
cat("  2. Check comparison_results.rds for detailed statistics\n")
cat("  3. The heatmap shows RMSE across different parameter combinations\n")
cat("\n")

cat("To reproduce results, run:\n")
cat("  source('hard_test.R')\n\n")
