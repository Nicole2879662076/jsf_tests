#' Visualize Trajectory Comparison
#'
#' Plots comparison of Gillespie and JSF trajectories
#'
#' @param jsf_data JSF reference data from generate_jsf_reference
#' @param gillespie_data Gillespie data from simulate_birth_death_ensemble
#' @param lambda Birth rate for plot title
#' @param mu Death rate for plot title
#' @param x0 Initial population for plot title
#' @param n_show Number of random trajectories to show (default: 5)
#' @param seed Random seed for selecting trajectories
#' @return ggplot2 object
#' @export
plot_trajectories <- function(
    jsf_data,
    gillespie_data,
    lambda = 1.0,
    mu = 0.5,
    x0 = 5,
    n_show = 5,
    seed = 42
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required for plotting. Please install with: install.packages('ggplot2')")
  }

  # Set seed for reproducible trajectory selection
  set.seed(seed)

  # Randomly select trajectories to show
  jsf_sims <- unique(jsf_data$simulation)
  gillespie_sims <- unique(gillespie_data$simulation)

  if (length(jsf_sims) < n_show) n_show <- length(jsf_sims)
  if (length(gillespie_sims) < n_show) n_show <- length(gillespie_sims)

  jsf_show <- sample(jsf_sims, n_show)
  gillespie_show <- sample(gillespie_sims, n_show)

  # Filter data
  jsf_show_data <- jsf_data[jsf_data$simulation %in% jsf_show, ]
  gillespie_show_data <- gillespie_data[gillespie_data$simulation %in% gillespie_show, ]

  # Calculate mean trajectories
  jsf_mean <- aggregate(population ~ time, data = jsf_data, FUN = mean)
  gillespie_mean <- aggregate(population ~ time, data = gillespie_data, FUN = mean)

  # Calculate mean-field solution
  t_grid <- seq(0, max(jsf_data$time), length.out = 100)
  mean_field <- data.frame(
    time = t_grid,
    population = x0 * exp((lambda - mu) * t_grid)
  )

  # Create plot
  p <- ggplot2::ggplot() +
    # Individual JSF trajectories (thin lines)
    ggplot2::geom_line(
      data = jsf_show_data,
      ggplot2::aes(x = time, y = population, group = simulation, color = "JSF"),
      alpha = 0.3,
      linewidth = 0.3
    ) +
    # Individual Gillespie trajectories (thin lines)
    ggplot2::geom_line(
      data = gillespie_show_data,
      ggplot2::aes(x = time, y = population, group = simulation, color = "Gillespie SSA"),
      alpha = 0.3,
      linewidth = 0.3
    ) +
    # Mean trajectories (thick lines)
    ggplot2::geom_line(
      data = jsf_mean,
      ggplot2::aes(x = time, y = population, color = "JSF Mean"),
      linewidth = 1.2
    ) +
    ggplot2::geom_line(
      data = gillespie_mean,
      ggplot2::aes(x = time, y = population, color = "Gillespie Mean"),
      linewidth = 1.2
    ) +
    # Mean-field solution
    ggplot2::geom_line(
      data = mean_field,
      ggplot2::aes(x = time, y = population, color = "Mean-Field"),
      linewidth = 1.2,
      linetype = "dashed"
    ) +
    # Color scheme
    ggplot2::scale_color_manual(
      name = "Method",
      values = c(
        "JSF" = "#2A9D8F",
        "Gillespie SSA" = "#E63946",
        "JSF Mean" = "#1D7373",
        "Gillespie Mean" = "#A61C1C",
        "Mean-Field" = "black"
      ),
      breaks = c("JSF Mean", "Gillespie Mean", "Mean-Field", "JSF", "Gillespie SSA")
    ) +
    # Labels and title
    ggplot2::labs(
      title = "Birth-Death Process: Stochastic Simulation Comparison",
      subtitle = sprintf("λ = %.1f, μ = %.1f, X₀ = %d", lambda, mu, x0),
      x = "Time",
      y = "Population",
      caption = sprintf("Showing %d random trajectories from each method", n_show)
    ) +
    # Theme
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      legend.margin = ggplot2::margin(t = 5, r = 0, b = 0, l = 0),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40")
    )

  return(p)
}

#' Plot Error Evolution Over Time (Fixed Version)
#'
#' Plots absolute error of both methods compared to mean-field over time
#' FIXED: Handles different time point distributions between JSF and Gillespie
#'
#' @param comparison_result Comparison result from compare_simulation_results
#' @param jsf_data JSF reference data
#' @param gillespie_data Gillespie data
#' @param lambda Birth rate
#' @param mu Death rate
#' @param x0 Initial population
#' @param n_grid_points Number of points in common time grid (default: 200)
#' @return ggplot2 object
#' @export
plot_error_vs_time <- function(
    comparison_result = NULL,
    jsf_data,
    gillespie_data,
    lambda = 1.0,
    mu = 0.5,
    x0 = 5,
    n_grid_points = 200
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required. Please install with: install.packages('dplyr')")
  }

  # Diagnostic message
  message("FIXED: Processing error plot with time alignment...")
  message("  JSF data dimensions: ", nrow(jsf_data), " rows, ",
          length(unique(jsf_data$time)), " unique time points")
  message("  Gillespie data dimensions: ", nrow(gillespie_data), " rows, ",
          length(unique(gillespie_data$time)), " unique time points")

  # 1. Calculate mean trajectories
  jsf_mean <- aggregate(population ~ time, data = jsf_data, FUN = mean)
  gillespie_mean <- aggregate(population ~ time, data = gillespie_data, FUN = mean)

  message("  JSF mean trajectory: ", nrow(jsf_mean), " points")
  message("  Gillespie mean trajectory: ", nrow(gillespie_mean), " points")

  # 2. Create a common time grid that covers both time ranges
  time_min <- min(min(jsf_mean$time), min(gillespie_mean$time))
  time_max <- max(max(jsf_mean$time), max(gillespie_mean$time))

  message("  Time range: [", round(time_min, 3), ", ", round(time_max, 3), "]")
  message("  Creating common grid with ", n_grid_points, " points")

  common_time_grid <- seq(time_min, time_max, length.out = n_grid_points)

  # 3. Interpolate both mean trajectories to the common grid
  jsf_interp <- approx(jsf_mean$time, jsf_mean$population,
                       xout = common_time_grid,
                       method = "linear",
                       rule = 2)$y

  gillespie_interp <- approx(gillespie_mean$time, gillespie_mean$population,
                             xout = common_time_grid,
                             method = "linear",
                             rule = 2)$y

  # Handle NA values (just in case)
  jsf_interp[is.na(jsf_interp)] <- 0
  gillespie_interp[is.na(gillespie_interp)] <- 0

  message("  Interpolation complete")
  message("  JSF interpolated: ", sum(!is.na(jsf_interp)), " valid points")
  message("  Gillespie interpolated: ", sum(!is.na(gillespie_interp)), " valid points")

  # 4. Calculate mean-field solution on the common grid
  mean_field <- x0 * exp((lambda - mu) * common_time_grid)

  # 5. Calculate absolute errors
  error_data <- data.frame(
    time = common_time_grid,
    jsf_error = abs(jsf_interp - mean_field),
    gillespie_error = abs(gillespie_interp - mean_field)
  )

  # Calculate summary statistics
  jsf_mean_error <- mean(error_data$jsf_error, na.rm = TRUE)
  gillespie_mean_error <- mean(error_data$gillespie_error, na.rm = TRUE)
  jsf_rmse <- sqrt(mean(error_data$jsf_error^2, na.rm = TRUE))
  gillespie_rmse <- sqrt(mean(error_data$gillespie_error^2, na.rm = TRUE))

  message("  Error statistics:")
  message("    JSF - Mean Error: ", round(jsf_mean_error, 3), ", RMSE: ", round(jsf_rmse, 3))
  message("    Gillespie - Mean Error: ", round(gillespie_mean_error, 3), ", RMSE: ", round(gillespie_rmse, 3))

  # 6. Reshape for ggplot
  error_long <- reshape(
    error_data,
    direction = "long",
    varying = c("jsf_error", "gillespie_error"),
    timevar = "method",
    times = c("JSF", "Gillespie SSA"),
    v.names = "error"
  )

  # 7. Create plot
  p <- ggplot2::ggplot(error_long, ggplot2::aes(x = time, y = error, color = method)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::scale_color_manual(
      values = c("JSF" = "#2A9D8F", "Gillespie SSA" = "#E63946")
    ) +
    ggplot2::labs(
      title = "Absolute Error vs Mean-Field Solution Over Time",
      subtitle = sprintf(
        "λ = %.1f, μ = %.1f, X₀ = %d | JSF MAE: %.2f (RMSE: %.2f), Gillespie MAE: %.2f (RMSE: %.2f)",
        lambda, mu, x0, jsf_mean_error, jsf_rmse, gillespie_mean_error, gillespie_rmse
      ),
      x = "Time",
      y = "Absolute Error | Simulated - Mean-Field |",
      color = "Method"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40", size = 10)
    )

  message("FIXED: Error plot created successfully")
  return(p)
}

#' Plot Parameter Sweep Heatmap
#'
#' Creates a heatmap comparing RMSE of JSF and Gillespie across parameter space
#'
#' @param lambda_seq Sequence of birth rates to test
#' @param mu_seq Sequence of death rates to test
#' @param x0 Initial population
#' @param t_max Simulation time
#' @param n_reps Number of replicates per parameter combination
#' @param grid_dt Time grid spacing for interpolation
#' @param seed Random seed
#' @param output_dir Directory to save intermediate results
#' @return ggplot2 object
#' @export
plot_error_heatmap <- function(
    lambda_seq = seq(0.4, 2.0, by = 0.4),
    mu_seq = seq(0.2, 1.8, by = 0.4),
    x0 = 5,
    t_max = 4.0,
    n_reps = 30,
    grid_dt = 0.1,
    seed = 2026,
    output_dir = "hard_output"
) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package is required")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package is required")
  }

  # Create output directory
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  set.seed(seed)
  results <- data.frame()

  message("Running parameter sweep for heatmap...")
  total_combinations <- length(lambda_seq) * length(mu_seq)
  current <- 0

  for (lambda in lambda_seq) {
    for (mu in mu_seq) {
      current <- current + 1
      message(sprintf("  [%d/%d] λ = %.1f, μ = %.1f",
                      current, total_combinations, lambda, mu))

      tryCatch({
        # Run Gillespie simulations
        gillespie_raw <- simulate_birth_death_ensemble(
          lambda = lambda,
          mu = mu,
          x0 = x0,
          t_max = t_max,
          n_reps = n_reps,
          seed = seed
        )

        # Run JSF simulations
        jsf_raw <- generate_jsf_reference(
          lambda = lambda,
          mu = mu,
          x0 = x0,
          t_max = t_max,
          n_reps = n_reps,
          dt = 0.05,
          threshold = 10,
          seed = seed,
          cache_file = NULL
        )

        # Calculate mean trajectories
        jsf_mean <- aggregate(population ~ time, data = jsf_raw, FUN = mean)
        gillespie_mean <- aggregate(population ~ time, data = gillespie_raw, FUN = mean)

        # Create common time grid for error calculation
        time_min <- min(min(jsf_mean$time), min(gillespie_mean$time))
        time_max <- max(max(jsf_mean$time), max(gillespie_mean$time))
        common_grid <- seq(time_min, time_max, length.out = 200)

        # Interpolate to common grid
        jsf_interp <- approx(jsf_mean$time, jsf_mean$population,
                             xout = common_grid, method = "linear", rule = 2)$y
        gillespie_interp <- approx(gillespie_mean$time, gillespie_mean$population,
                                   xout = common_grid, method = "linear", rule = 2)$y

        # Calculate mean-field solution
        mean_field <- x0 * exp((lambda - mu) * common_grid)

        # Calculate RMSE on common grid
        jsf_rmse <- sqrt(mean((jsf_interp - mean_field)^2, na.rm = TRUE))
        gillespie_rmse <- sqrt(mean((gillespie_interp - mean_field)^2, na.rm = TRUE))

        # Store results
        results <- rbind(results, data.frame(
          lambda = lambda,
          mu = mu,
          jsf_rmse = jsf_rmse,
          gillespie_rmse = gillespie_rmse
        ))

      }, error = function(e) {
        message(sprintf("    Error for λ=%.1f, μ=%.1f: %s", lambda, mu, e$message))
      })
    }
  }

  # Reshape for faceting
  results_long <- reshape(
    results,
    direction = "long",
    varying = c("jsf_rmse", "gillespie_rmse"),
    timevar = "method",
    times = c("JSF", "Gillespie SSA"),
    v.names = "rmse"
  )

  # Create heatmap
  p <- ggplot2::ggplot(results_long, ggplot2::aes(x = factor(lambda), y = factor(mu), fill = rmse)) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(ggplot2::aes(label = round(rmse, 1)), size = 3, color = "white") +
    ggplot2::scale_fill_gradient(
      low = "#2A9D8F",
      high = "#E63946",
      na.value = "grey50"
    ) +
    ggplot2::facet_wrap(~ method) +
    ggplot2::labs(
      title = "Parameter Sensitivity: RMSE vs Mean-Field",
      subtitle = sprintf("X₀ = %d, t_max = %.1f, n_reps = %d", x0, t_max, n_reps),
      x = "Birth Rate (λ)",
      y = "Death Rate (μ)",
      fill = "RMSE"
    ) +
    ggplot2::theme_bw(base_size = 12) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      strip.background = ggplot2::element_rect(fill = "white", color = "black"),
      plot.title = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, color = "gray40")
    )

  # Save results
  write.csv(results, file.path(output_dir, "parameter_sweep_results.csv"), row.names = FALSE)

  return(p)
}
